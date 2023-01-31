* 8-7-2022

I thought the way to run asyncio tasks concurrently was as follows:
>   import asyncio
>
>   async def f(): ...
>   async def g(): ...
>
>   task_1 = asyncio.create_task(f())
>   task_2 = asyncio.create_task(g())
>   await asyncio.gather(task_1, task_2)
>   result_1 = task_1.result()
>   result_2 = task_2.result()
Today I found out that a nicer way is
>   import asyncio
>
>   async def f(): ...
>   async def g(): ...
>
>   result_1, result_2 = await asyncio.gather(f(), g())
Moreover, `mypy` is able to infer the types of `result_1` and `result_2` in the snippet above.
