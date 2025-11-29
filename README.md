# Space War (Haskell + Gloss)

## 📖 Informe del Proyecto

Este proyecto corresponde a la **Tarea 1 del curso INFO188 (2025)**, donde se debía implementar un videojuego estilo *Action RPG* en Haskell, haciendo uso de la **Mónada State** y cumpliendo con los requisitos mínimos establecidos.

---
## 👥 Integrantes del Grupo
Ian Cuevas

Marcelo Lara

Nicolás Molina

---

## 🎮 Descripción del Juego

**Space War** es un videojuego 2D desarrollado completamente en **Haskell**, utilizando la librería **Gloss** para gráficos.  
El jugador controla una nave espacial que debe **sobrevivir a oleadas de enemigos** lo suficiente como para enfrentar al **jefe final (Boss)**.  
Al derrotar al boss, el juego se considera ganado.

El juego incluye un **menú de instrucciones** que explica los controles y describe los distintos tipos de enemigos.

---

## ✅ Requisitos cumplidos

1. **Uso de la Mónada State**  
   - El estado del juego (`GameState`) se maneja con la Mónada State, permitiendo actualizar vida, enemigos, ítems y lógica de combate de forma funcional.

2. **Juego en terminal y 2D**  
   - El juego corre en entorno gráfico 2D usando Gloss, cumpliendo con el requisito de visualización simple.

3. **Control con teclado**  
   - El jugador puede moverse y disparar usando teclas específicas (WASD + Espacio).

4. **Items que afectan atributos**  
   - Hay ítems que modifican atributos como vida, daño y velocidad del jugador.

5. **Objetivo del juego**  
   - El objetivo es sobrevivir a las oleadas y vencer al jefe final.

6. **Obstáculos y enemigos**  
   - Enemigos con distintos patrones de ataque aparecen en oleadas. Algunos tienen mecánicas especiales como explosión kamikaze.

7. **Originalidad**  
   - Se implementó un sistema de oleadas progresivas, un jefe final con mecánicas distintas, y un menú de instrucciones que informa al jugador.

---

## ✨ Características Principales

- Movimiento libre en 2D.  
- Oleadas de enemigos con dificultad creciente.  
- Sistema de disparo manual.  
- Colisiones entre balas, enemigos y jugador.  
- Sistema de vida y daño.  
- Jefe final con comportamiento especial.  
- Menú de instrucciones con descripción de enemigos y controles.  
- Renderizado gráfico con Gloss.  
- Uso de mónadas para lógica y estado del juego.

---

## 🛠️ Tecnologías Utilizadas

- **Haskell (GHC)**  
- **Gloss**  
- **JuicyPixels** para manejo de imágenes  
- **Mónadas:**  
  - `State` para el estado del juego  
  - `IO` para carga de recursos gráficos  
- **Cabal** como sistema de construcción

---

## 🎮 Controles

| Acción           | Tecla   |
|------------------|---------|
| Mover arriba     | W       |
| Mover abajo      | S       |
| Mover izquierda  | A       |
| Mover derecha    | D       |
| Disparar         | Espacio |
| Salir del juego  | Esc     |

---

## Instalación

### 1. Clonar el repositorio
```bash
git clone <URL_DEL_REPOSITORIO>
cd space-war
````

### 2. Instalar dependencias

Con Cabal:

```bash
cabal update
cabal build
```

### 3. Ejecución
Puedes compilar y ejecutar el juego directamente con:

```bash
make run
```
Este comando compila el proyecto usando Cabal y lanza el juego automáticamente.

---

## Estructura del Proyecto

```
space-war/
│
├── assets/
│   ├── attacks/
│   ├── background/
│   ├── enemies/
│   ├── items/
│   ├── player/
│   └── stats/
│
├── space-game/
│   ├── dist-newstyle/
│   └── src/
│       ├── Assets.hs
│       ├── Boss.hs
│       ├── Enemy.hs
│       ├── GameState.hs
│       ├── Input.hs
│       ├── Item.hs
│       ├── Main.hs
│       ├── Render.hs
│       ├── Update.hs
│       └── Wave.hs
│
├── cabal.project
├── Makefile
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
## 🚀 Oleadas y Jefe Final

El sistema de oleadas es el núcleo del desafío en Space War. El jugador debe sobrevivir a múltiples rondas de enemigos que se vuelven progresivamente más difíciles.

Cada oleada incrementa la cantidad, velocidad y agresividad de las naves enemigas.

El jugador debe resistir lo suficiente para alcanzar la ronda final, donde aparece el Boss.

---

## 🧠 Mecánicas del Boss
Posee más vida que los enemigos normales.

Tiene comportamientos especiales y patrones de ataque distintos.

Su aparición marca el objetivo final del juego: Al derrotarlo, el juego se considera ganado.

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

## 🎯 Conclusión

Space War es un videojuego funcional en Haskell que cumple con los requisitos de la tarea INFO188. Se implementaron mecánicas de oleadas, combate, jefe final, ítems interactivos y un menú de instrucciones, todo gestionado mediante la Mónada State. El proyecto demuestra una estructura modular, uso correcto de efectos IO, y una experiencia de juego clara y progresiva.

---

