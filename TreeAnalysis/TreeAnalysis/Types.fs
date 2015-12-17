namespace TreeAnalysis

module Types =

    type Label = string
    
    type Tree<'a> = { label    : 'a; 
                      children : Tree<'a> list }

    type LabeledTree = Tree<Label>
    