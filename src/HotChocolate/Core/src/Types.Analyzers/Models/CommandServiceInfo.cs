using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace HotChocolate.Types.Analyzers.Models;

public sealed class CommandServiceInfo : SyntaxInfo
{
    private readonly string[] _handlerNames;

    public CommandServiceInfo(
        AttributeSyntax attributeSyntax,
        IMethodSymbol attributeSymbol,
        AttributeData attributeData,
        INamedTypeSymbol serviceType,
        ImmutableArray<CommandMethodInfo> methods,
        ClassDeclarationSyntax classSyntax)
    {
        Validate(serviceType, classSyntax, methods);

        AttributeSyntax = attributeSyntax;
        AttributeSymbol = attributeSymbol;
        AttributeData = attributeData;
        ServiceType = serviceType;
        Methods = methods;
        ClassSyntax = classSyntax;

        _handlerNames = attributeData.GetHandlerNames();

        ServiceName = serviceType.Name;
        Namespace = serviceType.ContainingNamespace.ToDisplayString();
        if (Namespace == "<global namespace>")
        {
            Namespace = serviceType.ContainingModule.ToDisplayString().Replace(".dll", null);
        }
        ServiceFullName = $"{Namespace}.{ServiceName}";

        // Configuration from attribute
        GenerateInterfaces = attributeData.GetGenerateInterfaces();
        HandlerSuffix = attributeData.GetHandlerSuffix();
        IsPublic = attributeData.IsPublic();
        UsePrimaryConstructor = attributeData.UsePrimaryConstructor();
        GenerateRegistrationExtensions = attributeData.GenerateRegistrationExtensions();
        BaseHandlerType = attributeData.GetBaseHandlerType();
    }

    public string ServiceName { get; }
    public string ServiceFullName { get; }
    public string Namespace { get; }
    public string HandlerSuffix { get; }
    public string? BaseHandlerType { get; }
    public bool GenerateInterfaces { get; }
    public bool GenerateRegistrationExtensions { get; }
    public bool IsPublic { get; }
    public bool UsePrimaryConstructor { get; }

    public AttributeSyntax AttributeSyntax { get; }
    public IMethodSymbol AttributeSymbol { get; }
    public AttributeData AttributeData { get; }
    public INamedTypeSymbol ServiceType { get; }
    public ClassDeclarationSyntax ClassSyntax { get; }
    public ImmutableArray<CommandMethodInfo> Methods { get; }

    public override string OrderByKey => ServiceFullName;

    public ImmutableDictionary<string, string> GetHandlerNameOverrides()
    {
        if (_handlerNames.Length == 0)
        {
            return ImmutableDictionary<string, string>.Empty;
        }

        var builder = ImmutableDictionary.CreateBuilder<string, string>();

        foreach (var handlerName in _handlerNames)
        {
            var parts = handlerName.Split('=');
            if (parts.Length == 2)
            {
                builder[parts[0].Trim()] = parts[1].Trim();
            }
        }

        return builder.ToImmutable();
    }

    public string GetHandlerName(string commandName)
    {
        var overrides = GetHandlerNameOverrides();
        if (overrides.TryGetValue(commandName, out var customName))
        {
            return customName;
        }

        if (HandlerSuffix.Equals("Handler", StringComparison.OrdinalIgnoreCase))
        {
            return $"{commandName}Handler";
        }

        return commandName + HandlerSuffix;
    }

    public string GetInterfaceName(string commandName)
    {
        return GenerateInterfaces ? $"I{GetHandlerName(commandName)}" : GetHandlerName(commandName);
    }

    public ImmutableArray<ServiceParameterInfo> GetRequiredServices()
    {
        var builder = ImmutableArray.CreateBuilder<ServiceParameterInfo>();
        var seenTypes = new HashSet<string>();

        // Add the command service itself
        builder.Add(new ServiceParameterInfo(
            GetParameterName(ServiceType),
            ServiceType,
            ServiceParameterKind.Service));

        // Collect services from all methods
        foreach (var method in Methods)
        {
            foreach (var param in method.Parameters)
            {
                if (IsCancellationToken(param))
                {
                    continue;
                }

                if (IsLogger(param.Type))
                {
                    var paramName = GetParameterName(param.Type);
                    if (!seenTypes.Contains(paramName))
                    {
                        builder.Add(new ServiceParameterInfo(
                            paramName,
                            param.Type,
                            ServiceParameterKind.Logger));
                        seenTypes.Add(paramName);
                    }
                    continue;
                }

                if (IsHttpContext(param.Type))
                {
                    var paramName = GetParameterName(param.Type);
                    if (!seenTypes.Contains(paramName))
                    {
                        builder.Add(new ServiceParameterInfo(
                            paramName,
                            param.Type,
                            ServiceParameterKind.HttpContext));
                        seenTypes.Add(paramName);
                    }
                    continue;
                }

                if (IsMediator(param.Type))
                {
                    var paramName = GetParameterName(param.Type);
                    if (!seenTypes.Contains(paramName))
                    {
                        builder.Add(new ServiceParameterInfo(
                            paramName,
                            param.Type,
                            ServiceParameterKind.Mediator));
                        seenTypes.Add(paramName);
                    }
                    continue;
                }

                // Default to regular service
                var serviceName = GetParameterName(param.Type);
                if (!seenTypes.Contains(serviceName))
                {
                    builder.Add(new ServiceParameterInfo(
                        serviceName,
                        param.Type,
                        ServiceParameterKind.Service));
                    seenTypes.Add(serviceName);
                }
            }
        }

        return builder.ToImmutable();
    }

    private void Validate(
        INamedTypeSymbol serviceType,
        ClassDeclarationSyntax classSyntax,
        ImmutableArray<CommandMethodInfo> methods)
    {
        if (methods.Length == 0)
        {
            AddDiagnostic(
                Diagnostic.Create(
                    CommandServiceErrors.NoHandleMethodsFound,
                    Location.Create(
                        classSyntax.SyntaxTree,
                        classSyntax.Identifier.Span)));
        }

        if (!serviceType.DeclaredAccessibility.IsAccessible())
        {
            AddDiagnostic(
                Diagnostic.Create(
                    CommandServiceErrors.ServiceAccessModifierInvalid,
                    Location.Create(
                        classSyntax.SyntaxTree,
                        classSyntax.Modifiers.Span)));
        }

        // Check for duplicate command types
        var seenCommands = new HashSet<string>();
        foreach (var method in methods)
        {
            var commandName = method.CommandType.ToDisplayString();
            if (!seenCommands.Add(commandName))
            {
                AddDiagnostic(
                    Diagnostic.Create(
                        CommandServiceErrors.DuplicateCommandType,
                        Location.Create(
                            method.MethodSyntax.SyntaxTree,
                            method.MethodSyntax.ParameterList.Parameters[0].Span),
                        commandName));
            }

            // Validate method signatures
            ValidateMethod(method);
        }
    }

    private void ValidateMethod(CommandMethodInfo method)
    {
        var methodSyntax = method.MethodSyntax;

        if (method.MethodSymbol.Parameters.Length != 2)
        {
            AddDiagnostic(
                Diagnostic.Create(
                    CommandServiceErrors.InvalidHandleMethodSignature,
                    Location.Create(
                        methodSyntax.SyntaxTree,
                        methodSyntax.ParameterList.Span),
                    method.MethodSymbol.Name));
            return;
        }

        var secondParam = method.MethodSymbol.Parameters[1];
        if (!IsCancellationToken(secondParam))
        {
            AddDiagnostic(
                Diagnostic.Create(
                    CommandServiceErrors.SecondParameterMustBeCancellationToken,
                    Location.Create(
                        methodSyntax.SyntaxTree,
                        methodSyntax.ParameterList.Parameters[1].Span)));
        }

        if (!method.MethodSymbol.ReturnsTaskOfResult())
        {
            AddDiagnostic(
                Diagnostic.Create(
                    CommandServiceErrors.InvalidReturnType,
                    Location.Create(
                        methodSyntax.SyntaxTree,
                        methodSyntax.ReturnType.Span)));
        }
    }

    private static bool IsCancellationToken(IParameterSymbol parameter)
    {
        var typeName = parameter.Type.ToDisplayString();
        return string.Equals(typeName, WellKnownTypes.CancellationToken, StringComparison.Ordinal);
    }

    private static bool IsLogger(ITypeSymbol type)
    {
        var typeName = type.ToDisplayString();
        return typeName.StartsWith(WellKnownTypes.ILogger, StringComparison.Ordinal)
            || typeName.StartsWith(WellKnownTypes.ILoggerGeneric, StringComparison.Ordinal);
    }

    private static bool IsHttpContext(ITypeSymbol type)
    {
        var typeName = type.ToDisplayString();
        return string.Equals(typeName, WellKnownTypes.HttpContext, StringComparison.Ordinal);
    }

    private static bool IsMediator(ITypeSymbol type)
    {
        var typeName = type.ToDisplayString();
        return string.Equals(typeName, WellKnownTypes.IMediator, StringComparison.Ordinal)
            || typeName.StartsWith(WellKnownTypes.ISender, StringComparison.Ordinal);
    }

    private static string GetParameterName(ITypeSymbol type)
    {
        // Handle generic types
        if (type is INamedTypeSymbol namedType && namedType.IsGenericType)
        {
            var baseName = namedType.Name;
            if (baseName.StartsWith("I"))
            {
                baseName = baseName.Substring(1);
            }

            // Remove "Logger" suffix for ILogger<T>
            if (baseName == "ILogger")
            {
                return "logger";
            }

            return char.ToLowerInvariant(baseName[0]) + baseName.Substring(1);
        }

        var name = type.Name;
        if (name.StartsWith("I"))
        {
            name = name.Substring(1);
        }

        return char.ToLowerInvariant(name[0]) + name.Substring(1);
    }

    public override bool Equals(object? obj)
        => obj is CommandServiceInfo other && Equals(other);

    public override bool Equals(SyntaxInfo? obj)
        => obj is CommandServiceInfo other && Equals(other);

    private bool Equals(CommandServiceInfo? other)
    {
        if (other is null)
        {
            return false;
        }

        if (ReferenceEquals(this, other))
        {
            return true;
        }

        return OrderByKey.Equals(other.OrderByKey, StringComparison.Ordinal)
            && AttributeSyntax.IsEquivalentTo(other.AttributeSyntax)
            && ClassSyntax.IsEquivalentTo(other.ClassSyntax);
    }

    public override int GetHashCode()
        => HashCode.Combine(OrderByKey, AttributeSyntax, ClassSyntax);
}

// Supporting classes
public sealed class CommandMethodInfo
{
    public CommandMethodInfo(
        IMethodSymbol methodSymbol,
        MethodDeclarationSyntax methodSyntax)
    {
        MethodSymbol = methodSymbol;
        MethodSyntax = methodSyntax;
        MethodName = methodSymbol.Name;

        // Extract command type (first parameter)
        CommandType = methodSymbol.Parameters[0].Type;
        CommandTypeName = CommandType.ToDisplayString();

        // Extract result type
        ExtractResultType(methodSymbol.ReturnType);

        // Collect parameters (excluding command and cancellation token)
        Parameters = CollectParameters(methodSymbol);
    }

    public IMethodSymbol MethodSymbol { get; }
    public MethodDeclarationSyntax MethodSyntax { get; }
    public string MethodName { get; }
    public ITypeSymbol CommandType { get; }
    public string CommandTypeName { get; }
    public ITypeSymbol? ResultType { get; private set; }
    public bool HasGenericResult { get; private set; }
    public string? ResultTypeName { get; private set; }
    public ImmutableArray<IParameterSymbol> Parameters { get; }

    private void ExtractResultType(ITypeSymbol returnType)
    {
        if (returnType is INamedTypeSymbol namedReturn
            && namedReturn.IsGenericType
            && namedReturn.Name == "Task")
        {
            var taskResult = namedReturn.TypeArguments[0];

            if (taskResult is INamedTypeSymbol resultType)
            {
                // Handle Result or Result<T>
                if (resultType.Name == "Result")
                {
                    if (resultType.TypeArguments.Length == 1)
                    {
                        ResultType = resultType.TypeArguments[0];
                        HasGenericResult = true;
                        ResultTypeName = ResultType.ToDisplayString();
                    }
                    else
                    {
                        HasGenericResult = false;
                    }
                }
                // Handle IResult or IResult<T>
                else if (resultType.Name == "IResult")
                {
                    if (resultType.TypeArguments.Length == 1)
                    {
                        ResultType = resultType.TypeArguments[0];
                        HasGenericResult = true;
                        ResultTypeName = ResultType.ToDisplayString();
                    }
                    else
                    {
                        HasGenericResult = false;
                    }
                }
            }
        }
    }

    private static ImmutableArray<IParameterSymbol> CollectParameters(IMethodSymbol method)
    {
        // Skip first parameter (command) and last parameter (cancellation token)
        var builder = ImmutableArray.CreateBuilder<IParameterSymbol>();

        for (var i = 1; i < method.Parameters.Length - 1; i++)
        {
            builder.Add(method.Parameters[i]);
        }

        return builder.ToImmutable();
    }
}

public sealed class ServiceParameterInfo
{
    public ServiceParameterInfo(
        string parameterName,
        ITypeSymbol type,
        ServiceParameterKind kind)
    {
        ParameterName = parameterName;
        Type = type;
        Kind = kind;
        TypeName = type.ToDisplayString();
    }

    public string ParameterName { get; }
    public ITypeSymbol Type { get; }
    public string TypeName { get; }
    public ServiceParameterKind Kind { get; }
}

public enum ServiceParameterKind
{
    Service,
    Logger,
    HttpContext,
    Mediator,
    CancellationToken
}

public static class CommandServiceErrors
{
    public static readonly DiagnosticDescriptor NoHandleMethodsFound = new(
        id: "CSG001",
        title: "No valid Handle methods found",
        messageFormat: "CommandService must have at least one Handle method",
        category: "CommandServiceGenerator",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static readonly DiagnosticDescriptor ServiceAccessModifierInvalid = new(
        id: "CSG002",
        title: "Invalid access modifier",
        messageFormat: "CommandService class must be public, internal, or protected internal",
        category: "CommandServiceGenerator",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static readonly DiagnosticDescriptor InvalidHandleMethodSignature = new(
        id: "CSG003",
        title: "Invalid Handle method signature",
        messageFormat: "Handle method '{0}' must have exactly 2 parameters: (TCommand command, CancellationToken cancellationToken)",
        category: "CommandServiceGenerator",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static readonly DiagnosticDescriptor SecondParameterMustBeCancellationToken = new(
        id: "CSG004",
        title: "Second parameter must be CancellationToken",
        messageFormat: "The second parameter of Handle method must be CancellationToken",
        category: "CommandServiceGenerator",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static readonly DiagnosticDescriptor InvalidReturnType = new(
        id: "CSG005",
        title: "Invalid return type",
        messageFormat: "Handle method must return Task<Result> or Task<Result<T>>",
        category: "CommandServiceGenerator",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static readonly DiagnosticDescriptor DuplicateCommandType = new(
        id: "CSG006",
        title: "Duplicate command type",
        messageFormat: "Multiple Handle methods for command type '{0}' found",
        category: "CommandServiceGenerator",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);
}

//public static class WellKnownTypes
//{
//    public const string CancellationToken = "CancellationToken";
//    public const string CancellationTokenFull = "System.Threading.CancellationToken";
//    public const string ILogger = "Microsoft.Extensions.Logging.ILogger";
//    public const string ILoggerGeneric = "Microsoft.Extensions.Logging.ILogger<";
//    public const string HttpContext = "Microsoft.AspNetCore.Http.HttpContext";
//    public const string IMediator = "MediatR.IMediator";
//    public const string ISender = "MediatR.ISender";
//}

public static class AttributeDataExtensions
{
    public static bool GetGenerateInterfaces(this AttributeData attribute)
        => attribute.GetNamedArgument("GenerateInterfaces", true);

    public static string GetHandlerSuffix(this AttributeData attribute)
        => attribute.GetNamedArgument("HandlerSuffix", "Handler");

    public static bool IsPublic(this AttributeData attribute)
        => attribute.GetNamedArgument("IsPublic", true);

    public static bool UsePrimaryConstructor(this AttributeData attribute)
        => attribute.GetNamedArgument("UsePrimaryConstructor", true);

    public static bool GenerateRegistrationExtensions(this AttributeData attribute)
        => attribute.GetNamedArgument("GenerateRegistrationExtensions", true);

    public static string? GetBaseHandlerType(this AttributeData attribute)
        => attribute.GetNamedArgument("BaseHandlerType", string.Empty);

    public static string[] GetHandlerNames(this AttributeData attribute)
        => attribute.GetNamedArgument("HandlerNames", Array.Empty<string>());

    private static T GetNamedArgument<T>(this AttributeData attribute, string name, T defaultValue)
    {
        foreach (var arg in attribute.NamedArguments)
        {
            if (arg.Key == name && arg.Value.Value is T value)
            {
                return value;
            }
        }
        return defaultValue;
    }
}

public static class AccessibilityExtensions
{
    public static bool IsAccessible(this Accessibility accessibility)
        => accessibility is Accessibility.Public or
           Accessibility.Internal or
           Accessibility.ProtectedAndInternal;
}

public static class MethodSymbolExtensions
{
    public static bool ReturnsTaskOfResult(this IMethodSymbol method)
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
        if (resultType is INamedTypeSymbol resultNamedType)
        {
            return resultNamedType.Name == "Result"
                || resultNamedType.Name == "IResult";
        }

        return false;
    }
}
