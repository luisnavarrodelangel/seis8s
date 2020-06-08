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

El ejemplo de arriba no tiene mucho sentido, ya que nota y ritmo sobreescriben casi completamente la relación original de cumbia. Por lo tanto el ejemplo de arriba seria equivalente a:

```
nota 60 $ ritmo 0.25 $ piano

```

> Donde el estilo se ha sobreescrito completamente.

Para evitar lo anterior y lograr cambios significativos a nuestro código, en seis8s las funciones o comandos también están conectadas con el conocimiento individual de los estilos. Por ejemplo, en la cumbia, una variación común del bajo, es tocar la tónica y la quinta del acorde. Esto se puede conseguir con la siguiente funcion:

```
tonicayquinta $ cumbia bajo

```

> En este ejemplo, escucharás dos notas distintas, en vez de las tres originales que se escuchan en "cumbia bajo".
