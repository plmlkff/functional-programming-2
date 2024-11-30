# Лабораторная работа №2

## Вариант avl-set
Работу выполнил: Мальков Павел Александрович

Группа: P3310

Преподаватель: Новоселов Борис Сергеевич

## Задание
Цель: освоиться с построением пользовательских типов данных, полиморфизмом,
рекурсивными алгоритмами и средствами тестирования (unit testing, propertybased testing).
В рамках лабораторной работы вам предлагается реализовать одну из
предложенных классических структур данных (список, дерево, бинарное дерево,
hashmap, граф...).
Требования:
1. Функции:
o добавление и удаление элементов;
o фильтрация;
o отображение (map);
o свертки (левая и правая);
o структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based
тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль
программирования. Примечание: некоторые языки позволяют получить
большую часть API через реализацию небольшого интерфейса. Так как
лабораторная работа про ФП, а не про экосистему языка -- необходимо
реализовать их вручную и по возможности -- обеспечить совместимость.

## Исходный код программы
Доступен по [ссылке](https://github.com/plmlkff/functional-programming-2/blob/main/lab2/src/AVLSet.hs)

Структура дерева выглядит следующим образом:
```haskell
data AVLTree t = Empty
              | Node t (AVLTree t) (AVLTree t) deriving (Show, Eq, Generic)
```

Реализованный набор [функций](https://github.com/plmlkff/functional-programming-2/blob/main/lab2/src/AVLSet.hs):

```haskell
module AVLSet
(
    AVLTree(Empty),
    insert,
    delete,
    map',
    filter',
    foldr'',
    foldl'',
    toList,
    getMin,
    height
)
```

Реализованные unit и property-based [тесты](https://github.com/plmlkff/functional-programming-2/blob/main/lab2/test/Spec.hs):

```haskell
  describe "AVLTree" $ do
    -- Validity property tests
    it "ensures that empty AVLTree is valid" $
      shouldBeValid (Empty :: AVLTree Char)

    it "ensures that insert maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(word :: Int) ->
        forAllValid $ \tree ->
          shouldBeValid (insert word tree)

    it "ensures that remove maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(word :: Char) ->
        forAllValid $ \tree ->
          shouldBeValid (delete tree word)

    it "ensures that filter maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(tree :: AVLTree Int) ->
        shouldBeValid (filter' ( > 2) tree)

    it "ensures that toList maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(tree :: AVLTree Char) ->
        shouldBeValid (toList tree)

    it "ensures that order of insertions does not matter" $ do
        let tree1 = insert 4 (insert 6 (insert 5 (insert 3 (insert 7 (insert 1 Empty)))))
        let tree2 = insert 6 (insert 4 (insert 5 (insert 7 (insert 3 (insert 1 Empty)))))
        toList tree1 `shouldBe` toList tree2

        let tree1Deleted = delete (delete tree1 5) 4
        let tree2Deleted = delete (delete tree1 4) 5
        toList tree1 `shouldBe` toList tree2


    it "unit testing tree, polymorphic test" $ do
        let tree1 = insert 4 (insert 6 (insert 5 (insert 3 (insert 7 (insert 1 Empty)))))
        show tree1 `shouldBe` "Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 6 Empty (Node 7 Empty Empty))"

        let tree2 = delete tree1 1
        toList tree2 `shouldBe` [3, 4, 5, 6, 7]

        let stringTree = map' show tree2
        toList stringTree `shouldBe` ["3", "4", "5", "6", "7"]

        let filteredTree = filter' (>"3") stringTree
        toList filteredTree `shouldBe` ["4", "5", "6", "7"]

        let foldRight = foldr'' (+) 0 tree1
        foldRight `shouldBe` 26

        let foldLeft = foldl'' (+) 0 tree1
        foldLeft `shouldBe` 26
```

CI [файл](https://github.com/plmlkff/functional-programming-2/blob/main/.github/workflows/haskell.yml) для GitHub Actions:
```bash
name: Haskell Updated CI

on:
  push:
    branches: [ "main" ]
  pull_request:
    branches: [ "main" ]

permissions:
  contents: read
  
jobs:
  hlint:
    name: Run lint
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4

    - name: 'Set up HLint'
      uses: haskell-actions/hlint-setup@v2

    - name: 'Run HLint'
      uses: haskell-actions/hlint-run@v2
      with:
        path: lab2/src/
        fail-on: warning
  build-test:
    name: Build & Test
    runs-on: ubuntu-latest
    needs: hlint
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
      - run: cd lab2 && stack --no-terminal test --fast
```

## Вывод
Во время выполнения данной лабораторной работы я ознакомился с построением
собственных типов данных в Haskell и реализовал один из самых сложных алгоритмов бинарных деревьев - AVL, также получил опыт пользования удобным набором для Validity-based тестирования, позволяющим автоматически генерировать тестовые выборки для реализованных типов данных. Также, познакомился с понятием "монада" и ее главными принципами. Помимо этого, получил опыт оптимизирования работы GitHub Actions.