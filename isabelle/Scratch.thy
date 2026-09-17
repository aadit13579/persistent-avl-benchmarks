theory Scratch
  imports Main

begin

datatype 'a tree=  Leaf | Node "'a tree"'a "'a tree"

fun size_tree :: "'a tree \<Rightarrow> nat" where
"size_tree Leaf=0" |
"size_tree (Node l x r) =size_tree l + size_tree r +1"

fun leaf_count :: "'a tree \<Rightarrow> nat" where
"leaf_count Leaf = 1" |
"leaf_count (Node l x r) = leaf_count l + leaf_count r"

lemma leaf_count_eq: "leaf_count t = size_tree t + 1"
  apply (induction t)
  apply auto
  done

end