using System.Reflection;
using CSharpFunctionalExtensions;
using MediatR;
using Result = CSharpFunctionalExtensions.Result;
var builder = WebApplication.CreateBuilder(args);
//var service = new NotificationCommand();

builder.Services.AddScoped<NotificationCommand>();
//builder.Services.AddScoped<CreateNotificationHandler>();
builder.Services.AddMediatR(cfg => cfg.RegisterServicesFromAssembly(Assembly.GetExecutingAssembly()));
builder.AddGraphQL().AddTypes();

var app = builder.Build();
using (var scope = app.Services.CreateScope())
{
    var mediator = scope.ServiceProvider.GetRequiredService<ISender>();
    var res = mediator.Send(new CreateNotification { Title = "Hello" }).Result;
}
app.MapGraphQL();

app.RunWithGraphQLCommands(args);

//public class CreateNotificationHandler(NotificationCommand service) : IRequestHandler<CreateNotification, CSharpFunctionalExtensions.Result<Guid>>
//{
//    public async Task<CSharpFunctionalExtensions.Result<Guid>> Handle(CreateNotification request, CancellationToken cancellationToken)
//    {
//        return await service.Handle(request, cancellationToken);
//    }
//}
//public class DeleteNotificationHandler(NotificationCommand service) : IRequestHandler<DeleteNotification, CSharpFunctionalExtensions.Result>
//{
//    public async Task<CSharpFunctionalExtensions.Result> Handle(DeleteNotification request, CancellationToken cancellationToken)
//    {
//        return await service.Handle(request, cancellationToken);
//    }
//}

public class CommandServiceAttribute : Attribute
{

}
public interface IResult : IRequest<Result>
{
}
public interface IResult<T> : IRequest<CSharpFunctionalExtensions.Result<T>>
{

}



public class CreateNotification: IResult<Guid>
{
    public string Title { get; set; }
}
public class DeleteNotification: IResult
{
    public Guid Id { get; set; }
}

[CommandService]
public class NotificationCommand
{
    public Task<CSharpFunctionalExtensions.Result<Guid>> Handle(CreateNotification request, CancellationToken cancellationToken)
    {
        return Task.FromResult(CSharpFunctionalExtensions.Result.Success(Guid.NewGuid()));
    }

    public Task<CSharpFunctionalExtensions.Result> Handle(DeleteNotification request, CancellationToken cancellationToken)
    {
        return Task.FromResult(CSharpFunctionalExtensions.Result.Success());
    }

}
