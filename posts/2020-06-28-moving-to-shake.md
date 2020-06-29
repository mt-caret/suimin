---
title: Writing a Static Site Generator in Shake
date: 2020-06-28
---

Today I'm working on porting [nippo](https://github.com/mt-caret/nippo/) to
[Shake](https://shakebuild.com/).
Track my progress here: [mt-caret/suimin](https://github.com/mt-caret/suimin)

Pandoc's template language is simple enough that it's relatively
straightforward to implement what you want. I like it.

Took me a while to find `stringify :: [Inline] -> String`, but after that,
adding an atom feed was a breeze: <https://github.com/mt-caret/suimin/blob/e4ce8b41273bbcf5d5d7c82b637b3627e988ee82/Shakefile.hs#L140-L149>

<!--
- lsp for markdown?
-->
