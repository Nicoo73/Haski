# # Space Survivors (Haskell + Gloss)

## Descripción del Juego
**Space Survivors** es un juego tipo *Vampire Survivors* y *The Binding of Isaac* desarrollado completamente en **Haskell**, utilizando la librería **Gloss** para gráficos 2D.

El jugador controla una nave que se mueve libremente en un plano 2D y debe sobrevivir a múltiples **oleadas de enemigos**. Puede realizar un **ataque manual** mediante una tecla.  
Al final del juego existe una **ronda final** donde aparece un **Jefe (Boss)** con más vida y mecánicas diferentes.

El juego implementa **mónadas** (`State`, `IO`) para manejar el estado del juego, actualizar la lógica y gestionar entradas del usuario.

---

## Características Principales
- Movimiento libre en 2D (no limitado a izquierda y derecha).
- Enemigos que aparecen en **oleadas** con dificultad creciente.
- Sistema de **disparo manual** mediante una tecla.
- Colisiones entre enemigos, balas y jugador.
- Sistema de vida y puntaje.
- **Jefe final** con comportamiento especial.
- Gráficos renderizados mediante **Gloss**.
- Uso de **mónadas** para el estado y lógica del juego.

---

## Tecnologías Utilizadas
- **Haskell (GHC)**
- **Gloss**
- **Mónadas:**
  - `State` para el estado del juego.
  - `IO` para carga de recursos gráficos.
- Stack o Cabal.

---

## Controles
| Acción | Tecla |
|--------|-------|
| Mover arriba | W |
| Mover abajo | S |
| Mover izquierda | A |
| Mover derecha | D |
| Disparar | Espacio |
| Salir del juego | Esc |

---

## Instalación

### 1. Clonar el repositorio
```bash
git clone <URL_DEL_REPOSITORIO>
cd space-survivors
````

### 2. Instalar dependencias

Con Stack:

```bash
stack setup
stack build
```

Con Cabal:

```bash
cabal update
cabal build
```

---

## Ejecución

Con Stack:

```bash
stack run
```

Con Cabal:

```bash
cabal run
```

---

## Estructura del Proyecto

```
space-survivors/
│
├── src/
│   ├── Main.hs         -- Punto de entrada, ciclo principal de Gloss.
│   ├── Types.hs        -- Tipos del juego: GameState, Player, Enemy, Bullet...
│   ├── Logic.hs        -- Actualización del mundo, colisiones, oleadas.
│   ├── Graphics.hs     -- Renderizado con Gloss.
│   ├── Input.hs        -- Manejo de controles.
│   └── Waves.hs        -- Generación de oleadas y jefe.
│
├── assets/
│   ├── player.png
│   ├── enemy.png
│   └── boss.png
│
└── README.md
```

---

## Uso de Mónadas

### `State GameState`

Utilizada para manejar las partes mutables del juego:

* Posición del jugador
* Balas
* Enemigos
* Oleada actual
* Vida del jugador
* Estado del jefe

Ejemplo conceptual:

```haskell
updateWorld :: Float -> State GameState ()
```

### `IO`

Utilizado en:

* Carga de imágenes (`loadBMP`, `loadJuicy`)
* Inicialización del entorno
* Renderizado mediante Gloss

### Integración con Gloss

```haskell
play window bgColor fps initialState render handleInput update
```

---

## Oleadas y Jefe Final

* Cada oleada aumenta la cantidad y velocidad de los enemigos.
* Tras varias oleadas aparece un **Boss** con:

  * Más vida
  * Movimientos especiales
  * Tamaño mayor

---

## Gráficos

Los gráficos se integran mediante Gloss. Ejemplo:

```haskell
playerSprite <- loadBMP "assets/player.bmp"
enemySprite  <- loadBMP "assets/enemy.bmp"
bossSprite   <- loadBMP "assets/boss.bmp"
```

Los dibujos se representan mediante `Picture` y `Pictures`.

---

## Estado del Proyecto

* [x] Diseño conceptual
* [ ] Definición de tipos base
* [ ] Movimiento del jugador
* [ ] Ataque manual
* [ ] Sistema básico de oleadas
* [ ] Colisiones
* [ ] Jefe final
* [ ] Implementación gráfica
* [ ] Menú inicial y Game Over

---
