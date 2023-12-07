using HotChocolate.Types.Descriptors;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using StrawberryShake.CodeGeneration.CSharp.Extensions;
using StrawberryShake.CodeGeneration.Descriptors;
using StrawberryShake.CodeGeneration.Descriptors.TypeDescriptors;
using StrawberryShake.CodeGeneration.Extensions;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static StrawberryShake.CodeGeneration.Descriptors.NamingConventions;
using InputObjectTypeDescriptor = StrawberryShake.CodeGeneration.Descriptors.TypeDescriptors.InputObjectTypeDescriptor;

namespace StrawberryShake.CodeGeneration.CSharp.Generators;

public class InputTypeInterfaceGenerator : CSharpSyntaxGenerator<InputObjectTypeDescriptor>
{
    protected override CSharpSyntaxGeneratorResult Generate(
        InputObjectTypeDescriptor descriptor,
        CSharpSyntaxGeneratorSettings settings)
    {
        var stateNamespace = $"{descriptor.RuntimeType.Namespace}.{State}";
        var name = CreateInputValue(descriptor.Name);


        var interfaceDeclaration =
            SyntaxFactory.InterfaceDeclaration(name)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                .AddGeneratedAttribute();

        foreach (var prop in descriptor.Properties)
        {
            interfaceDeclaration = prop.Type.IsInput()?
                GenerateInputType(prop, interfaceDeclaration, stateNamespace) :
                GeneratePrimitiveType(prop, interfaceDeclaration);
        }

        return new(
            name,
            State,
            $"{descriptor.RuntimeType.NamespaceWithoutGlobal}.{State}",
            interfaceDeclaration);
    }

    protected override bool CanHandle(InputObjectTypeDescriptor descriptor, CSharpSyntaxGeneratorSettings settings)
    {
        return descriptor.Name.EndsWith("FilterInput") == false;
    }

    private InterfaceDeclarationSyntax GenerateInputType(
        PropertyDescriptor prop,
        InterfaceDeclarationSyntax interfaceDeclaration,
        string stateNamespace)
    {
        var type = ParseTypeName($"{stateNamespace}.{CreateInputValue(prop.Type.Name)}");
        return interfaceDeclaration.AddMembers(
            PropertyDeclaration(
                    type,
                    prop.Name)
                .WithGetter());
    }
    private InterfaceDeclarationSyntax GeneratePrimitiveType(
        PropertyDescriptor prop,
        InterfaceDeclarationSyntax interfaceDeclaration)
    {
        return interfaceDeclaration.AddMembers(
            PropertyDeclaration(
                    prop.Type.ToTypeSyntax(),
                    prop.Name)
                .WithGetterAndSetter());
    }
}
