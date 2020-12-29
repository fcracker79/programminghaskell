module BookOfMonads.Chapter11.ChristmasTree where


newChristmasTree :: Int -> Maybe String
newChristmasTree 0 = Just ""
newChristmasTree 1 = Just "|"
newChristmasTree n
    | n < 0 = Nothing
    | otherwise = (++) <$>
        pure (
            mconcat (
                fmap (\h -> replicate (n - h - 2) ' ' ++ replicate (h * 2 + 1) '*' ++ "\n") 
                [0.. n - 2]
            )
            ++
            (replicate (n - 2) ' ')
        )
        <*> 
        newChristmasTree 1

