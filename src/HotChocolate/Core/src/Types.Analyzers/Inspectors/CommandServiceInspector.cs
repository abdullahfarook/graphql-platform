using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using HotChocolate.Types.Analyzers.Filters;
using HotChocolate.Types.Analyzers.Models;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace HotChocolate.Types.Analyzers.Inspectors;

public sealed class CommandServiceInspector : ISyntaxInspector
{
    public ImmutableArray<ISyntaxFilter> Filters { get; } =
        [MethodWithAttribute.Instance]; // Use existing filter from HotChocolate

    public IImmutableSet<SyntaxKind> SupportedKinds { get; } =
        [SyntaxKind.ClassDeclaration];

    public bool TryHandle(
        GeneratorSyntaxContext context,
        [NotNullWhen(true)] out SyntaxInfo? syntaxInfo)
    {
        syntaxInfo = null;

        if (context.Node is not ClassDeclarationSyntax { AttributeLists.Count: > 0 } classSyntax)
        {
            return false;
        }

        // Get the class symbol
        var classSymbol = context.SemanticModel.GetDeclaredSymbol(classSyntax);
        if (classSymbol == null)
        {
            return false;
        }

        AttributeSyntax? matchingAttributeSyntax = null;
        IMethodSymbol? matchingAttributeSymbol = null;
        AttributeData? matchingAttributeData = null;

        // Look for [CommandService] attribute
        foreach (var attributeList in classSyntax.AttributeLists)
        {
            foreach (var attributeSyntax in attributeList.Attributes)
            {
                var symbol = context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol;

                if (symbol is IMethodSymbol attributeSymbol)
                {
                    var attributeContainingType = attributeSymbol.ContainingType;
                    var attributeName = attributeContainingType.ToDisplayString();

                    // Check if it's our CommandService attribute
                    if (IsCommandServiceAttribute(attributeName))
                    {
                        // Get the attribute data from the class symbol
                        var attributeData = GetCommandServiceAttributeData(classSymbol);
                        if (attributeData == null)
                        {
                            continue;
                        }

                        matchingAttributeSyntax = attributeSyntax;
                        matchingAttributeSymbol = attributeSymbol;
                        matchingAttributeData = attributeData;
                        break;
                    }
                }
            }

            if (matchingAttributeSyntax != null)
            {
                break;
            }
        }

        if (matchingAttributeSyntax == null
            || matchingAttributeSymbol == null
            || matchingAttributeData == null)
        {
            return false;
        }

        // Find all Handle methods in this class
        var methods = FindHandleMethods(classSymbol, context.SemanticModel);

        if (methods.Length == 0)
        {
            // We still create the info but with empty methods - validation will handle error
            methods = [];
        }

        // Create the CommandServiceInfo
        syntaxInfo = new CommandServiceInfo(
            matchingAttributeSyntax,
            matchingAttributeSymbol,
            matchingAttributeData,
            classSymbol,
            methods,
            classSyntax);

        return true;
    }

    private static ImmutableArray<CommandMethodInfo> FindHandleMethods(
        INamedTypeSymbol classSymbol,
        SemanticModel semanticModel)
    {
        var methods = new List<CommandMethodInfo>();

        foreach (var member in classSymbol.GetMembers())
        {
            if (member is not IMethodSymbol methodSymbol
                || methodSymbol.Name != "Handle"
                || methodSymbol.MethodKind != MethodKind.Ordinary)
            {
                continue;
            }

            // Validate it's a proper Handle method
            if (!IsValidHandleMethod(methodSymbol))
            {
                continue;
            }

            // Get the method syntax node
            var methodSyntax = GetMethodSyntax(methodSymbol, semanticModel);
            if (methodSyntax == null)
            {
                continue;
            }

            methods.Add(new CommandMethodInfo(methodSymbol, methodSyntax));
        }

        return methods.ToImmutableArray();
    }

    private static MethodDeclarationSyntax? GetMethodSyntax(
        IMethodSymbol methodSymbol,
        SemanticModel semanticModel)
    {
        foreach (var location in methodSymbol.Locations)
        {
            if (location.SourceTree?.GetRoot() is CompilationUnitSyntax root)
            {
                var node = root.FindNode(location.SourceSpan);
                if (node is MethodDeclarationSyntax methodSyntax)
                {
                    return methodSyntax;
                }
            }
        }
        return null;
    }

    private static AttributeData? GetCommandServiceAttributeData(INamedTypeSymbol classSymbol)
    {
        foreach (var attribute in classSymbol.GetAttributes())
        {
            var attributeClass = attribute.AttributeClass;
            if (attributeClass == null)
            {
                continue;
            }

            var attributeName = attributeClass.ToDisplayString();
            if (IsCommandServiceAttribute(attributeName))
            {
                return attribute;
            }
        }
        return null;
    }

    private static bool IsCommandServiceAttribute(string attributeName)
    {
        return attributeName.EndsWith("CommandServiceAttribute")
            || attributeName == "YourProject.Attributes.CommandServiceAttribute"
            || attributeName == "CommandServiceAttribute";
    }

    private static bool IsValidHandleMethod(IMethodSymbol method)
    {
        // Must have exactly 2 parameters
        if (method.Parameters.Length != 2)
        {
            return false;
        }

        // Second parameter must be CancellationToken
        var secondParam = method.Parameters[1];
        if (!IsCancellationToken(secondParam))
        {
            return false;
        }

        // Must return Task<Result> or Task<Result<T>>
        return ReturnsTaskOfResult(method);
    }

    private static bool IsCancellationToken(IParameterSymbol parameter)
    {
        var typeName = parameter.Type.ToDisplayString();
        return string.Equals(typeName, "CancellationToken", StringComparison.Ordinal)
            || string.Equals(typeName, "System.Threading.CancellationToken", StringComparison.Ordinal);
    }

    private static bool ReturnsTaskOfResult(IMethodSymbol method)
    {
        if (method.ReturnType is not INamedTypeSymbol returnType
            || returnType.Name != "Task"
            || !returnType.IsGenericType)
        {
            return false;
        }

        if (returnType.TypeArguments.Length != 1)
        {
            return false;
        }

        var resultType = returnType.TypeArguments[0];
        if (resultType is not INamedTypeSymbol resultNamedType)
        {
            return false;
        }

        // Check for Result or IResult
        var resultTypeName = resultNamedType.Name;
        if (resultTypeName == "Result" || resultTypeName == "IResult")
        {
            // Can be generic (Result<T>) or non-generic (Result)
            return resultNamedType.TypeArguments.Length == 0
                || resultNamedType.TypeArguments.Length == 1;
        }

        return false;
    }
}
