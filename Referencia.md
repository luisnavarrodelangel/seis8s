# Referencia


La premisa detrás de seis8s es el poder interactuar con colecciones de conocimiento musical representado a través de bloques interlazados de código. En seis8s una coleccion de conocimiento se representa a través de un estilo musical. E.g. cumbia.

Dicho estilo necesita de un "cuerpo" para volverse tangible/visible (i.e. audible en este caso). Por ejemplo un piano.

```
cumbia piano
```

Otra idea fundamental en seis8s es la idea de "capa". En seis8s una capa se refiere a la adición de cuerpos (sonoros) con el fin de enriquecer la textura.

```
cumbia piano;
cumbia bajo;
```
> Aqui, el conocimiento musical embebido en el estilo de música cumbia es aplicado a cada uno de los instrumentos individualmente, pero con el fin de que tengan un significado audible en lo colectivo.

Otras capas que se pueden agregar son capas de instrumentos percusivos.

```
cumbia piano;
cumbia bajo;
cumbia guira;
cumbia contratiempos;
```

A grandes rasgos, el estilo proporciona información sobre los tonos y el ritmo que cada instrumento debe tomar, respectivamente. Esto parámetros se pueden reasignar de la siguiente forma.

```
nota 60 $ ritmo 0.25 $ cumbia piano
```

Sin embargo, el ejemplo de arriba no tiene mucho sentido, ya que nota y ritmo sobreescriben casi completamente la relación original de cumbia con el piano. E incluso el ejemplo de arriba seria equivalente a:

```
nota 60 $ ritmo 0.25 $ piano

```

> Donde el estilo se ha sobreescrito completamente.

Para evitar lo anterior y lograr cambios significativos a nuestro código, en seis8s las funciones o comandos también están conectadas con el conocimiento individual de los estilos. Por ejemplo, en la cumbia, una variación común del bajo, es tocar la tónica y la quinta del acorde. Esto se puede conseguir con la siguiente funcion:

```
tonicayquinta $ cumbia bajo
```

> En este ejemplo, escucharás dos notas distintas, en vez de las tres originales que se escuchan en "cumbia bajo".

En seis8s podemos explorar la tónica y la quinta individualmente con el siguiente comando:

```
intervalo "tónica" $ cumbia quinta
```
> ahora intentalo con intervalo "5a"

Otras funciones del bajo son dentro del estilo de cumbia son:

```
tonicayquinta2 $ cumbia bajo
```
>  Aqui, escucharás tres notas distinas, la tónica, la quinta y la quinta una octava abajo (i.e. más grave).

```
tonicaQtonica $ cumbia bajo
```
> Arriba, el bajo toca la tónica, la quinta y la octava alta de la tónica.

<!-- ```tucanes $ cumbia bajo
``` -->

```
tonicaQtercera  $ cumbia bajo
```
> En este ejemplo, el bajo toca la tónica, la quinta y la tercer del acorde.

<!-- ```saborcolombia $ cumbia bajo
``` -->


Otra característica importante en seis8s es la de poder combinar funciones.

```
cada 2 (tonicayquinta) $ cumbia bajo
```
> En el ejemplo arriba, estamos alternando entre el patrón de tres notas y el de tónica y quinta, que contiene dos notas.

Por lo tanto, el siguiente ejemplo también sería posible:

```
cada 2 (tónicayquinta2) $ tonicayquinta $ cumbia bajo
```


> En el ejemplo arriba, alternamos entre tonicaYquinta y tonicayquinta2



<!-- adorno tonicayquinta $ cumbia bajo -->

<!-- ```tumbao $ cumbia quinta; -->
<!-- ``` -->
