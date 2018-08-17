# Design

## feedbacks of v0.1.1
* overlapping plotly button 
* empty space in lower part
* splitting p2 boxplot 

## plotly vs ggplot+plotly

### plotly

* pros
  - unlikely to have library dependency problems
  - easy download
    - only if there is controller for height and width
    - fast: plot_ly -> rendering
* cons
  - lots of work to do, change from scratch
* problem solving
  - overlapping plotly button: adjust top margin
  - empty space in lower part: need test
  - splitting p2 boxplot: just do it
  
### ggplot + plotly

* pros
  - few work, not much changes
* cons
  - dependency problem
  - slow: ggplot object -> ggplotly --> rendering
* problem solving
  - overlapping plotly button: adjust plotly top margin
  - empty space in lower part: need test
  - splitting p2 boxplot: just do it

## future feature
* Update Documents
* Cache 기능
  * `hashmap` 라이브러리 이용
  * key는 string concatanation
  * value는 ggplot2 object
* Async
* Server Side preprocessing in server boot time

## version 2
* dashboard (usnig `shinydashboard`)

