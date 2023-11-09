module Shared.Types exposing (..)

type alias MarkdownString = String

type alias ImageFilePath = String

type TopicRowType = 
    NoImage
    | ImageOnLeft ImageFilePath
    | ImageOnRight ImageFilePath