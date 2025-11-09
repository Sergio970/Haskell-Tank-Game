# Haskell Tank Game / Space War

Simulador de combate entre dos equipos de tanques autónomos con:
- Bots con diferentes estrategias según el tipo de carro (Ligero, Pesado, Cazacarros).
- Meteoritos dinámicos con estelas dañinas.
- Bombas activables (cuenta atrás y daño radial).
- Obstáculos estáticos (planetas) con colisión.
- Sistema de torneos con múltiples rondas y límite de tiempo por partida.
- Configuración externa vía `config.txt`.

---

## Requisitos

- GHC 9.x o Stack / Cabal.
- Gloss (renderizado), gloss-juicy para PNG.
- Paquetes base incluidos: containers, random.

Instala dependencias (ejemplo Stack):

```bash
stack build
stack run
```

O con GHC directamente:

```bash
ghc -threaded -O2 Modulos/Main.hs -output haskell-tank-game
./haskell-tank-game
```

---

## Ejecución

Al iniciar se carga el mundo desde `Modulos/config.txt` y se ejecutan automáticamente las rondas configuradas (no se pide input por consola).  
Cada torneo termina cuando:
1. Solo queda un equipo vivo, o
2. Se alcanza el límite temporal (`tiempoMax`), decidiendo ganador por mayor número de tanques vivos (empate = 0).

---

## Archivo de Configuración (`Modulos/config.txt`)

Claves soportadas:

| Clave        | Tipo    | Descripción |
|--------------|---------|-------------|
| `tamX` / `tamY` | Float | Tamaño del mundo (ancho / alto). |
| `equipo1`, `equipo2` | Lista | Lista de tipos: `Ligero`, `Pesado`, `Cazacarros` (alias: `L`, `P`, `C`, `Caza`). |
| `bombs`      | Int     | Número de bombas iniciales. |
| `obstacles`  | Int     | Número de obstáculos estáticos. |
| `rondas` / `rounds` | Int | Número de torneos consecutivos. |
| `tiempoMax` / `timeLimit` / `duracion` | Float | Límite de tiempo por torneo (segundos). |

Ejemplo incluido:
```txt
tamX=700
tamY=800
equipo1=L,L,P,C
equipo2=P,P,L,C
bombs=6
obstacles=8
rondas=5
tiempoMax=120
```

Si faltan listas de equipos se generan por defecto (4 tanques cada lado).

---

## Controles

| Tecla | Acción |
|-------|--------|
| ENTER | Iniciar desde menú (si se usa modo menú). |
| P     | Pausa / volver a menú. |
| R     | Reinicia el torneo actual (nueva ronda). |
| F     | Cambiar fondo (en menú). |
| ESC   | Salir. |

(No hay control manual de tanques: todo es IA).

---

## Tipos de Carros

| Tipo        | Rasgos |
|-------------|--------|
| Ligero      | Alta visión / movilidad, bajo blindaje. |
| Pesado      | Mucho blindaje, lento, buena presión frontal. |
| Cazacarros  | Precisión alta, alcance medio, movilidad media. |

Efectos de tripulación: muerte de roles reduce velocidad, cadencia, precisión, radio.

---

## Bots (Estrategias)

- Ligero: flanqueo, reposicionamiento lateral, retirada si baja vida.
- Pesado: empuje, presión frontal, intento de usar bombas (alineación empuje → bomba).
- Cazacarros: mantiene distancia óptima, reposicionamiento lateral (strafe).
- Oportunidades especiales:
  - Disparar meteoritos cercanos a enemigos (si vida del meteorito es baja).
  - Evitar meteoritos en trayectorias peligrosas (vector de escape).
- Activación de bombas por contacto (enemigos o meteoritos).

---

## Sistema de Torneos

Flujo:
1. Se inicializa `GameState` con mundo de config.
2. Al terminar una ronda (victoria o tiempo agotado) se entra en modo `Victoria`.
3. Tras la cuenta atrás se crea nuevo mundo (misma configuración) hasta agotar `rondas`.
4. Estado final: `FinTorneos`.

---

## Arquitectura (Principales Módulos)

| Módulo        | Rol |
|---------------|-----|
| `Main.hs`     | Arranque y bucle Gloss. |
| `Torneos.hs`  | Lógica de rondas, parsing de config, límite de tiempo. |
| `Unidad.hs`   | Modelos de dominio (carros, mundo, bombas, meteoritos, tripulación). |
| `Bot.hs`      | IA táctica y estratégica. |
| `Collisions.hs` | Detección de colisiones (SAT + círculos). |
| `Physics.hs`  | Utilidades físicas y vectores. |
| `Rendering.hs`| Dibujado de sprites, HUD, explosiones, efectos. |
| `GameTypes.hs`| Tipos de modo de juego y GameState. |
| `Types.hs`    | Tipos básicos y enums. |

---

## Evento de Fin por Tiempo

Cuando `tiempo >= tiempoMax`:
- Se computan tanques vivos por equipo.
- Ganador = equipo con más supervivientes (empate = 0).
- Se dispara transición a `Victoria`.

---

## Posibles Mejoras Futuras

- Sonido (explosiones / disparos).
- Power-ups temporales.
- Detección de línea de visión con obstáculos sólidos.
- Configuración JSON / YAML más rica.
- Modo espectador con cámara orbital.

---

## Compilación Rápida (sin Stack)

```bash
ghc -O2 Modulos/Main.hs -output tankgame
./tankgame
```


---

## Créditos

Proyecto educativo en Haskell usando Gloss.  
Sprites fallback básicos integrados; reemplazables en `Assets/`.

---
¡Disfruta analizando o extendiendo la IA de combate!
