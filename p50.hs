-- Problem 50

data HuffmanTree a = HuffmanTree Integer (HuffmanTree a) (HuffmanTree a) | HuffmanLeaf Integer a deriving Show

huffmanFreqAtNode (HuffmanLeaf freq _  ) = freq
huffmanFreqAtNode (HuffmanTree freq _ _) = freq

-- assumes initial list is already sorted by frequency
constructHuffmanTree :: [HuffmanTree a] -> HuffmanTree a
constructHuffmanTree []       = error "Can not construct from empty list"
constructHuffmanTree [t]      = t
constructHuffmanTree (f:g:fs) =
  let newSubTree = HuffmanTree (huffmanFreqAtNode f + huffmanFreqAtNode g) f g
  in
    constructHuffmanTree $ insertBy (comparing huffmanFreqAtNode) newSubTree fs

buildHuffmanTree :: [(Char, Integer)] -> HuffmanTree Char
buildHuffmanTree xs = constructHuffmanTree $ map (uncurry HuffmanLeaf . swap) $ sortBy (comparing snd) xs

buildHuffmanCodes tree currentSearchPath =
  case tree of
    HuffmanLeaf _ chr        -> (chr, currentSearchPath) : []
    HuffmanTree _ left right ->
      let leftCodes  = buildHuffmanCodes left  ('0':currentSearchPath)
          rightCodes = buildHuffmanCodes right ('1':currentSearchPath)
      in
        leftCodes ++ rightCodes

huffman :: [(Char, Integer)] -> [(Char, String)]
huffman xs =
  let tree = buildHuffmanTree xs
  in
    buildHuffmanCodes tree ""