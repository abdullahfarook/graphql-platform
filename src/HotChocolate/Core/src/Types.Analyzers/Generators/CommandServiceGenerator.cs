using System.Collections.Immutable;
using HotChocolate.Types.Analyzers.FileBuilders;
using HotChocolate.Types.Analyzers.Models;
using Microsoft.CodeAnalysis;

namespace HotChocolate.Types.Analyzers.Generators;

public sealed class CommandServiceGenerator : ISyntaxGenerator
{
    public void Generate(
        SourceProductionContext context,
        string assemblyName,
        ImmutableArray<SyntaxInfo> syntaxInfos,
        Action<string, string> addSource)
    {
        var commandServices = syntaxInfos
            .OfType<CommandServiceInfo>()
            .Where(cs => cs.Diagnostics.Length == 0)
            .ToList();

        if (commandServices.Count == 0)
        {
            return;
        }

        // Group by namespace
        var servicesByNamespace = commandServices
            .GroupBy(cs => cs.Namespace)
            .ToDictionary(g => g.Key, g => g.ToList());

        foreach (var item in servicesByNamespace)
        {
            var namespaceName = item.Key;
            var servicesInNamespace = item.Value;
            using var fileBuilder = new CommandServiceFileBuilder();
            var hasContent = false;

            fileBuilder.WriteHeader();

            if (!string.IsNullOrEmpty(namespaceName))
            {
                fileBuilder.WriteBeginNamespace(namespaceName);
            }

            foreach (var commandService in servicesInNamespace.OrderBy(cs => cs.ServiceName))
            {
                hasContent |= GenerateHandlersForService(fileBuilder, commandService);
            }

            if (!string.IsNullOrEmpty(namespaceName))
            {
                fileBuilder.WriteEndNamespace();
            }

            if (hasContent)
            {
                var fileName = GetFileName(namespaceName);
                addSource(fileName, fileBuilder.ToString());
            }
        }
    }

    private static bool GenerateHandlersForService(
        CommandServiceFileBuilder fileBuilder,
        CommandServiceInfo commandService)
    {
        var hasHandlers = false;
        var serviceName = commandService.ServiceName;
        var serviceTypeName = commandService.ServiceFullName;

        foreach (var method in commandService.Methods)
        {
            var commandType = method.CommandType;
            var commandTypeName = commandType.ToDisplayString();
            var commandSimpleName = commandType.Name;

            var handlerName = commandService.GetHandlerName(commandSimpleName);

            // Determine result type
            string resultType;
            if (method.HasGenericResult && method.ResultTypeName != null)
            {
                resultType = $"global::CSharpFunctionalExtensions.Result<{method.ResultTypeName}>";
            }
            else
            {
                resultType = "global::CSharpFunctionalExtensions.Result";
            }

            // Write handler class
            fileBuilder.WriteHandlerClass(
                handlerName,
                commandTypeName,
                resultType,
                serviceName,
                serviceTypeName,
                method.MethodName,
                commandService.IsPublic);

            fileBuilder.WriteLine();
            hasHandlers = true;
        }

        return hasHandlers;
    }

    private static string GetFileName(string namespaceName)
    {
        if (string.IsNullOrEmpty(namespaceName))
        {
            return "Global.CommandHandlers.g.cs";
        }

        var sanitized = namespaceName
            .Replace('.', '_')
            .Replace('<', '_')
            .Replace('>', '_')
            .Replace(' ', '_');

        return $"{sanitized}.CommandHandlers.g.cs";
    }
}
