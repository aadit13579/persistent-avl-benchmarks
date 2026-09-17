theory bst
  imports Main

begin
datatype 'a tree=  Leaf | Node "'a tree"'a "'a tree"

fun set_tree :: "'a tree \<Rightarrow> 'a set" where
"set_tree Leaf ={}"|
"set_tree (Node l x r) =set_tree l \<union> {x} \<union> set_tree r"

fun bst_naive :: "'a::linorder tree \<Rightarrow> bool" where
"bst_naive Leaf = True" |
"bst_naive (Node l x r) = 
   ((\<forall>y. y \<in> set_tree l \<longrightarrow> y < x) \<and> 
    (\<forall>y. y \<in> set_tree r \<longrightarrow> y > x) \<and> 
    bst_naive l \<and> bst_naive r)"

fun bst_bounded :: "'a::linorder tree \<Rightarrow> 'a option \<Rightarrow> 'a option \<Rightarrow> bool" where
"bst_bounded Leaf lo hi = True" |
"bst_bounded (Node l x r) lo hi =
   ((case lo of None \<Rightarrow> True | Some low \<Rightarrow> low < x) \<and>
    (case hi of None \<Rightarrow> True | Some high \<Rightarrow> x < high) \<and>
    bst_bounded l lo (Some x) \<and>
    bst_bounded r (Some x) hi)"

definition bst :: "'a::linorder tree \<Rightarrow> bool" where
"bst t = bst_bounded t None None"

fun insert :: "'a::linorder \<Rightarrow> 'a tree \<Rightarrow> 'a tree" where
"insert x Leaf = Node Leaf x Leaf" |
"insert x (Node l y r) =
   (if x < y then Node (insert x l) y r
    else if x > y then Node l y (insert x r)
    else Node l y r)"

end