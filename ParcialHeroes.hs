import Text.Show.Functions

data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
} deriving (Show)


messi :: Heroe
messi = Heroe "El Mejor De La Historia" 500 [] []

modificaEpiteto :: String -> Tarea
modificaEpiteto nuevoEpiteto heroe = heroe {epiteto = nuevoEpiteto}

modificaTarea :: Tarea -> Tarea
modificaTarea tarea heroe = heroe {tareas = tarea : tareas heroe}

modificarArtefactos funcion heroe = heroe {artefactos = funcion (artefactos heroe)}

agregarArtefactos :: Artefacto -> Tarea
agregarArtefactos nuevoArtefacto = modificarArtefactos (nuevoArtefacto :)

modificaReconocimiento :: Int -> Tarea
modificaReconocimiento numero heroe = heroe {reconocimiento = numero}

modificaEpitetoYArtefacto :: String -> Artefacto -> Tarea
modificaEpitetoYArtefacto nuevoEpiteto nuevoArtefacto = agregarArtefactos nuevoArtefacto.modificaEpiteto nuevoEpiteto

modificaRarezaArtefacto :: Int -> Artefacto -> Artefacto
modificaRarezaArtefacto numero artefacto = artefacto {rareza = numero}

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza Del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

elRelampagoDeZeus :: Artefacto
elRelampagoDeZeus = Artefacto "El Relampago de Zeus" 500

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
} deriving (Show)

type Tarea = Heroe -> Heroe

pasaALaHisotria :: Tarea
pasaALaHisotria heroe
    | reconocimiento heroe > 1000 = modificaEpiteto "El Mitico" heroe
    | reconocimiento heroe >= 500 = modificaEpitetoYArtefacto "El Magnifico" lanzaDelOlimpo heroe
    | reconocimiento heroe > 100  = modificaEpitetoYArtefacto "Hoplita" xiphos heroe
    | otherwise                   = heroe

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto artefacto heroe = agregarArtefactos artefacto. modificaReconocimiento (reconocimiento heroe + rareza artefacto) $ heroe

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza artefacto = artefacto {rareza = 3* rareza artefacto}

triplicarRarezaArtefactos :: Tarea
triplicarRarezaArtefactos = modificarArtefactos (map triplicarRareza)

noEsComun :: Artefacto -> Bool
noEsComun artefacto = rareza artefacto > 1000

deshecharArtefactos :: Tarea
deshecharArtefactos = modificarArtefactos (filter noEsComun)

escalarElOlimpo :: Tarea
escalarElOlimpo heroe = agregarArtefactos elRelampagoDeZeus . modificaReconocimiento (reconocimiento heroe + 500) . agregarArtefactos elRelampagoDeZeus .  deshecharArtefactos . triplicarRarezaArtefactos $  heroe

ayudarACruzarCalle :: Int -> Tarea
ayudarACruzarCalle numero = modificaEpiteto ("Groso" ++ replicate (numero - 1) 'o')

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

matarBestia :: Bestia -> Tarea
matarBestia bestia heroe
    | debilidad bestia heroe = modificaEpiteto ("El asesino de " ++ nombreBestia bestia) heroe
    | otherwise              = modificarArtefactos tail . modificaEpiteto "El cobarde" $ heroe

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

heracles :: Heroe
heracles = Heroe "Guardian del Olimpo" 700 [pistola, elRelampagoDeZeus] [matarAlLeonDeNemea]

epitetoMayorA20 :: Debilidad
epitetoMayorA20 heroe = length (epiteto heroe) > 20

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" epitetoMayorA20

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarBestia leonDeNemea

hacerTarea :: Tarea -> Tarea
hacerTarea tarea heroe = modificaTarea tarea (tarea heroe)

hacerTareas :: [Tarea] -> Tarea
hacerTareas tarea heroe = foldl (flip hacerTarea) heroe tarea

sumatoriaDeRarezas :: Heroe -> Int
sumatoriaDeRarezas = sum . map rareza . artefactos

realizarTareasDe :: Heroe -> Heroe -> Heroe
realizarTareasDe heroe1 = hacerTareas (tareas heroe1)

presumen :: Heroe -> Heroe -> (Heroe, Heroe)
presumen heroe1 heroe2
    | reconocimiento heroe1 < reconocimiento heroe2         = (heroe2, heroe1)
    | reconocimiento heroe1 > reconocimiento heroe2         = (heroe1, heroe2)
    | sumatoriaDeRarezas heroe1 < sumatoriaDeRarezas heroe2 = (heroe2, heroe1)
    | sumatoriaDeRarezas heroe1 > sumatoriaDeRarezas heroe2 = (heroe1, heroe2)
    | otherwise                                             = presumen (realizarTareasDe heroe1 heroe2) (realizarTareasDe heroe2 heroe1)
    