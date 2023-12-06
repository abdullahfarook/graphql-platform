using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using StrawberryShake.CodeGeneration.CSharp.Extensions;
using StrawberryShake.CodeGeneration.Descriptors;
using StrawberryShake.CodeGeneration.Descriptors.TypeDescriptors;

namespace StrawberryShake.CodeGeneration.CSharp.Generators;

public class InputTypeInterfaceGenerator : CSharpSyntaxGenerator<InputObjectTypeDescriptor>
{
    protected override CSharpSyntaxGeneratorResult Generate(
        InputObjectTypeDescriptor descriptor,
        CSharpSyntaxGeneratorSettings settings)
    {
        var name = NamingConventions.CreateInputValue(descriptor.Name);

        var interfaceDeclaration =
            SyntaxFactory.InterfaceDeclaration(name)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.InternalKeyword))
                .AddGeneratedAttribute();

        foreach (var prop in descriptor.Properties)
        {
            interfaceDeclaration = interfaceDeclaration.AddMembers(
                SyntaxFactory.PropertyDeclaration(
                        prop.Type.ToTypeSyntax(),
                        prop.Name)
                    .WithGetterAndSetter());
        }

        return new(
            name,
            State,
            $"{descriptor.RuntimeType.NamespaceWithoutGlobal}.{State}",
            interfaceDeclaration);
    }
}
