# Haskell-Server-Twitter
Проект по курсу "Промышленное программирование на Haskell", представляющий собой сервер а-ля Twitter на Haskell.

_Автор: Дмитрий Д. Никитин, ВШЭ, КНАД_

## Запуск
```
stack build
```
```
stack exec Haskell-Server-Twitter-exe
```

## Работа
Создание пользователя:
```
curl -v -X POST http://localhost:8080/auth/register \
  -H "Content-Type: application/json" \
  -d '["Stierlitz", "MullerLoh"]'
```

Логин:
```
curl -v -X POST http://localhost:8080/auth/login \
  -H "Content-Type: application/json" \
  -d '["Stierlitz", "MullerLoh"]'
```

Создание твита:
```
curl -v -X POST http://localhost:8080/send \
  -H "Authorization: YOUR_TOKEN" \
  -H "Content-Type: text/plain; charset=utf-8" \
  -d "Катю поймали! #Isaev"
```

Поиск твита:
```
http://localhost:8080/search?from=Stierlitz&tags=Isaev
```

Изменение твита:
```
curl -v -X PUT http://localhost:8080/tweets/1 \
  -H "Authorization: YOUR_TOKEN" \
  -H "Content-Type: text/plain; charset=utf-8" \
  -d "Пастор Шлаг в Швейцарии! #Isaev"
```

Удаление твита:
```
curl -v -X DELETE http://localhost:8080/tweets/1 \
  -H "Authorization: YOUR_TOKEN"
```
