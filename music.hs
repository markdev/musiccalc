-- Let's start with the basics: get the diatonic of a given note
-- diatonic C Maj2 = D

data Note = C | CsDb | D | DsEb | E | F | FsGb | G | GsAb | A | AsBb | B deriving (Read, Eq, Ord, Enum, Bounded)
instance Show Note where
    show C = "C"
    show CsDb = "C#/Db"
    show D = "D"
    show DsEb = "D#/Eb"
    show E = "E"
    show F = "F"
    show FsGb = "F#/Gb"
    show G = "G"
    show GsAb = "G#/Ab"
    show A = "A"
    show AsBb = "A#/Bb"
    show B = "B"
data Diatonic = Unison | Min2 | Maj2 | Min3 | Maj3 | Per4 | Tritone | Per5 | Min6 | Maj6 | Min7 | Maj7 | Octave deriving (Read, Show, Eq, Ord, Enum, Bounded)

getNotes :: Note -> [Note]
getNotes root = take 13 $ dropWhile (/=root) $ cycle noteList
    where noteList :: [Note]
          noteList = [minBound..maxBound]

getDiatonics :: [Diatonic]
getDiatonics = [minBound..maxBound]

mapDiatonics :: Note -> [(Diatonic, Note)]
mapDiatonics root = zip getDiatonics $ getNotes root


--diatonic :: Note -> Diatonic -> Note
--diatonic note diat = 