namespace SourceGeneratorTest.Types;

[QueryType]
public static class Query
{
    public static Book GetBook()
        => new Book("C# in depth.", new Author("Jon Skeet"));

    public static Author GetAuthor(string name)
        => new Author(name);

    [DataLoader]
    public static async Task<Dictionary<string, Book>> GetBookByIdAsync(
        IReadOnlyList<string> bookIds,
        Author author,
        CancellationToken cancellationToken)
        => await Task.FromResult(bookIds.ToDictionary(id => id, id => new Book($"Book {id}", new Author($"Author {id}"))));

}


[MutationType]
public static class Mutation
{

    public static Author AddAuthor(AddAuthor author)
        => new Author(author.Name);
}

public record AddAuthor(string Name);
