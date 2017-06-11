# Motion Blur - Summer internship exercise

## O problema

Deverá ser implementado um mecanismo de calculo de `MotionBlur` que deverá funcionar de forma paralela.

Tendo uma matriz com os pixeis, por exemplo:

```
M1 = 1 2 3 4 5
     2 3 4 5 6
     3 4 5 6 7
     4 5 6 7 8
```

o calculo do motion blur deverá ser:
```
resultado(x, y) = (M1(x, y)+M1(x-1, y)+M1(x, y-1)+M1(x, y+1)) / 4
```

o que significa que deve fazer a média entre o proprio pixel e os pixeis de cima, baixo e da esquerda.
Caso estes pixeis não estejam definidos, não devem entrar na média, por exemplo:

```
resultado(0, 0) = (M1(0, 0)+M1(0, 1)) / 2
```

O código deve ser implementado na classe MatrixMotionBlur e os testes em MatrixMotionBlurTest

## O que é preciso fazer

* Implementar a versão single thread em `MotionBlurSingleThread`.
* Implementar a versão que corre de forma paralela `MotionBlurMultiThread`.
* Implementar mais testes em `MotionBlurTest`

É visto com bons olhos se as versões seguirem uma abordagem mais funcional e menos imperativa.

## Como correr os testes

* Instalar o sbt [http://www.scala-lang.org/download/]
* Correr `sbt test`

## Dúvidas

Quaisquer dúvidas que tenhas, cria uma issue aqui no github :)

