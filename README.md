# seis8s

<p align="center">
<img style src="fotos/portada.png" title="Seis8s" alt="Seis8s">
</p>

seis8s (pronunciado "seis octavos") es un lenguaje de programación que permite la interacción en tiempo real con audio digital y conocimiento musical localizado, particularmente de músicas de Latinoamérica.

seis8s es un proyecto reciente que pretende ser colaborativo, el cual pretene nutrirse con conocimiento musical consensuado y desde las diferentes fronteras personales y colectivas que existen en Latinoamérica.

---

## Índice

- [Editor_de_texto](#editor_de_texto)
- [Estilos_musicales](#estilos_musicales)
- [Referencia_rápida](#referencia_rápida)
- [Colaboradorxs](#colaboradorxs)
- [Pregutas_frecuentes](#preguntas_frecuentes)
- [Ayuda](#ayuda)
- [Licencia](#licencia)

---
## Editor_de_texto

sies8s se puede jugar de manera [individual](#individual), así como de manera [colectiva](#colectiva) a través de la plataforma Estuary (para ver más info sobre estuary haz click aqui: https://github.com/dktr0/estuary).

### individual
1_ Accesa a https://estuary.mcmaster.ca y da click en "solo mode".
<p align="center">
<img style src="fotos/estuary/homepage.png" width="80%" title="home de estuary" alt="home de estuary" />
</p>

2_ Una vez en "solo mode" veras muchas cajas de texto donde puedes escibir código. Para escribir más cómodamente en una sola caja de texto escribe el siguiente código en la parte de abajo donde dice "Terminal/Chat" y da enter en tu teclado: !presetview fulltexteditor. De esta forma ahora debes ver en la pantalla una sola caja de texto de tamaño grande.

<p align="center">
<img src="fotos/estuary/terminal.png" width="80%"/>
<img src="fotos/estuary/fulltexteditor.png" width="80%"/>
</p>

3_Selecciona en el menu el lenguaje seis8s.
<p align="center">
<img  src="fotos/estuary/menu1.png" width="80%" title="menu de estuary" alt="menu de estuary" />
</p>

4_ Prueba seis8s escribiendo el siguiente código y hazlo sonar presionando el boton ▶ o presionando las teclas shift + enter. Una vez que lo ejecutes debes escuchar el sonido de un bajo con el ritmo base de la cumbia.

```haskell

cumbia bajo;
```
<p align="center">
<img src="fotos/estuary/helloWorld.png" width="80%"/>
</p>

5_ Continua a la sección [estilos_musicales](#estilos_musicales) o a  [referencia_rápida](#referencia_rápida) para continuar jugando con seis8s.

## Referencia_rápida

- [Instrumentos](#instrumentos)
- [Sintáxis_básica](#sintáxis_básica)
- [Funciones_del_bajo](#funciones_del_bajo)
- [Funciones_del_teclado](#funciones_del_teclado)
- [Funciones_de_las_congas](#funciones_de__las_congas)
- [Funciones_de_la_guira](#funciones_de_la_guira)
- [Funciones_del_jamblock](#funciones_del_jamblock)

Esta es la referencia rápida del estilo de cumbia en seis8s.

### instrumentos

```
teclado; congas; jamblock; guira; bajo;
```
Para silenciar los sonidos puedes utilizar el comando

```
silencio
```

### Síntaxis_básica

La función de estilo se coloca a la izquierda del instrumento.

```
cumbia teclado;
```
Todas las funciones que modifican el estilo se agregan a la izquierda del instrumento seguidas de un paréntesis o un signo de peso "$".

```
acompañamiento 2 (cumbia teclado);
```
El signo $ es equivalente a los paréntesis.

```
acompañamiento 2 $ cumbia teclado;
```

Para silenciar los sonidos puedes utilizar el comando

```
silencio
```

###Funciones_del_bajo


## Estilos_musicales

<a href="https://github.com/luisnavarrodelangel/seis8s/blob/master/Referencia.md"> ¿Cómo usar seis8s? </a>
