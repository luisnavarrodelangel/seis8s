module Sound.Seis8s.Rhythm where

type Metre = Rational
type Attack = Rational
type Start = Rational
type End = Rational 
type RhythmicPosition = (Metre, Attack)
type ChordPosition = (Start, End)
type RhythmicPattern = [RhythmicPosition]
