using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using StrawberryShake.CodeGeneration.CSharp.Extensions;
using StrawberryShake.CodeGeneration.Descriptors.TypeDescriptors;
using StrawberryShake.CodeGeneration.Extensions;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static StrawberryShake.CodeGeneration.Descriptors.NamingConventions;

namespace StrawberryShake.CodeGeneration.CSharp.Generators;

public class InputTypeGenerator : CSharpSyntaxGenerator<InputObjectTypeDescriptor>
{
    protected override CSharpSyntaxGeneratorResult Generate(
        InputObjectTypeDescriptor descriptor,
        CSharpSyntaxGeneratorSettings settings)
    {
        var stateNamespace = $"{descriptor.RuntimeType.Namespace}.{State}";
        var infoInterfaceType = $"{stateNamespace}.{CreateInputValueInfo(descriptor.Name)}";
        var inputInterfaceType = $"{stateNamespace}.{CreateInputValue(descriptor.Name)}";

        var modifier = settings.AccessModifier == AccessModifier.Public
            ? SyntaxKind.PublicKeyword
            : SyntaxKind.InternalKeyword;

        return new(
            descriptor.Name,
            null,
            descriptor.RuntimeType.NamespaceWithoutGlobal,
            settings.InputRecords
                ? GenerateRecord(descriptor, modifier,inputInterfaceType, infoInterfaceType)
                : GenerateClass(descriptor, modifier, inputInterfaceType, infoInterfaceType));
    }

    private BaseTypeDeclarationSyntax GenerateRecord(
        InputObjectTypeDescriptor descriptor,
        SyntaxKind accessModifier,
        string interfaceType,
        string infoInterfaceType)
    {
        var recordDeclaration =
            RecordDeclaration(Token(SyntaxKind.RecordKeyword), descriptor.Name.ToEscapedName())
                .AddImplements(infoInterfaceType)
                .AddModifiers(
                    Token(accessModifier),
                    Token(SyntaxKind.PartialKeyword))
                .AddGeneratedAttribute()
                .AddEquality(descriptor.Name.ToEscapedName(), descriptor.Properties, true)
                .AddSummary(descriptor.Documentation)
                .WithOpenBraceToken(Token(SyntaxKind.OpenBraceToken));

        recordDeclaration = GenerateProperties(
            recordDeclaration,
            SyntaxKind.InitAccessorDeclaration,
            interfaceType,
            infoInterfaceType,
            descriptor);

        recordDeclaration = recordDeclaration.WithCloseBraceToken(
            Token(SyntaxKind.CloseBraceToken));

        return recordDeclaration;
    }

    private BaseTypeDeclarationSyntax GenerateClass(InputObjectTypeDescriptor descriptor,
        SyntaxKind accessModifier,
        string interfaceType,
        string infoInterfaceType)
    {
        var interfaces = descriptor.Name.EndsWith("FilterInput") == false
            ? new[] { infoInterfaceType, interfaceType }
            : new[] { infoInterfaceType };
        var classDeclaration =
            ClassDeclaration(descriptor.Name.ToEscapedName())
                .AddImplements(interfaces)
                .AddModifiers(
                    Token(accessModifier),
                    Token(SyntaxKind.PartialKeyword))
                .AddGeneratedAttribute()
                .AddEquality(descriptor.Name.ToEscapedName(), descriptor.Properties)
                .AddSummary(descriptor.Documentation);

        classDeclaration = GenerateProperties(
            classDeclaration,
            SyntaxKind.SetAccessorDeclaration,
            interfaceType,
            infoInterfaceType,
            descriptor);

        return classDeclaration;
    }

    private T GenerateProperties<T>(
        T typeDeclarationSyntax,
        SyntaxKind setAccessorKind,
        string interfaceType,
        string infoInterfaceType,
        InputObjectTypeDescriptor descriptor)
        where T : TypeDeclarationSyntax
    {
        TypeDeclarationSyntax current = typeDeclarationSyntax;

        foreach (var prop in descriptor.Properties)
        {
            var variable =
                VariableDeclarator(
                    Identifier(CreateInputValueField(prop.Name)));

            if (prop.Type.IsNonNull() && !prop.Type.GetRuntimeType().IsValueType)
            {
                variable = variable.WithSuppressNullableWarningExpression();
            }

            current = current.AddMembers(
                FieldDeclaration(
                        VariableDeclaration(
                            prop.Type.ToTypeSyntax(),
                            SingletonSeparatedList(variable)))
                    .AddModifiers(Token(SyntaxKind.PrivateKeyword)));

            current = current.AddMembers(
                FieldDeclaration(
                        VariableDeclaration(
                            ParseTypeName(TypeNames.Boolean),
                            SingletonSeparatedList(
                                VariableDeclarator(
                                    Identifier(CreateIsSetField(prop.Name))))))
                    .AddModifiers(Token(SyntaxKind.PrivateKeyword)));
        }

        foreach (var prop in descriptor.Properties)
        {
            var property =
                PropertyDeclaration(prop.Type.ToTypeSyntax(), prop.Name)
                    .AddModifiers(Token(SyntaxKind.PublicKeyword))
                    .AddSummary(prop.Description)
                    .AddAccessorListAccessors(
                        AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                            .WithExpressionBody(
                                ArrowExpressionClause(
                                    IdentifierName(CreateInputValueField(prop.Name))))
                            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken)),
                        AccessorDeclaration(setAccessorKind)
                            .WithBody(
                                Block(
                                    ExpressionStatement(
                                        AssignmentExpression(
                                            SyntaxKind.SimpleAssignmentExpression,
                                            IdentifierName(CreateIsSetField(prop.Name)),
                                            LiteralExpression(
                                                SyntaxKind.TrueLiteralExpression))),
                                    ExpressionStatement(
                                        AssignmentExpression(
                                            SyntaxKind.SimpleAssignmentExpression,
                                            IdentifierName(CreateInputValueField(prop.Name)),
                                            IdentifierName("value"))))));

            current = current.AddMembers(property);

            current = current.AddMembers(
                PropertyDeclaration(
                        ParseTypeName(TypeNames.Boolean),
                        CreateIsSetProperty(prop.Name))
                    .WithExplicitInterfaceSpecifier(
                        ExplicitInterfaceSpecifier(
                            IdentifierName(infoInterfaceType)))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            IdentifierName(CreateIsSetField(prop.Name))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)));
            // if prop is input and not filterInput then generate following
            // StateNamespace.PropInterface inputInterfaceType.PropertyName { get => this.PropertyName; }
           // i.e State.INomineeDtoInput State.ICreateIndividualInput.Nominee => Nominee;
            if (prop.Type.IsInput() && prop.Type.IsFilterInput() == false)
            {
                var stateNamespace = $"{descriptor.RuntimeType.Namespace}.{State}";
                var returnType = ParseTypeName($"{stateNamespace}.{CreateInputValue(prop.Type.Name)}");
                var propertyName = $"{interfaceType}.{prop.Name}";
                current = current.AddMembers(
                    PropertyDeclaration(
                            returnType,
                            propertyName)
                        .WithExpressionBody(
                            ArrowExpressionClause(
                                IdentifierName(prop.Name)))
                        .WithSemicolonToken(
                            Token(SyntaxKind.SemicolonToken)));
            }

        }

        return (T)current;
    }
}
