ggplot(mpg, aes(x=class, y=hwy)) + 
    geom_boxplot(color="red", fill="orange", alpha=0.2)

ggplot(data7, aes(x=star, y=sentiment, fill=factor(star))) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none") +
    scale_fill_brewer(palette="Dark2")

ggplot(data7, aes(x=star, y=sentiment, fill=factor(star))) + 
    geom_boxplot(alpha=0.3)  +
labs(title = "LEXICON:JOCKER'S")+
 theme(plot.title = element_text(color = "#999999", size = 12, face = "bold",hjust=0.5)) +
    scale_fill_brewer(palette="BuPu")

ggplot(data7, aes(x=star, y=sentiment, fill=factor(star))) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none") +
    scale_fill_manual(values=wes_palette(n=5, name="FantasticFox1"))

ggplot(data7, aes(x=star, y=sentiment, fill=factor(star))) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none") + 
scale_fill_brewer(palette="Dark2")


BottleRocket1, BottleRocket2, Rushmore1, Royal1, Royal2, Zissou1, Darjeeling1, 
Darjeeling2, Chevalier1 , FantasticFox1 , Moonrise1, Moonrise2, 
Moonrise3, Cavalcanti1, 
GrandBudapest1, GrandBudapest2, IsleofDogs1, IsleofDogs2


ggplot(data7, aes(x=star, y=sentiment, fill=factor(star))) + 
    geom_boxplot(alpha=0.3) +
labs(title = "LEXICON:AFINN")+
 theme(plot.title = element_text(color = "#999999", size = 12, face = "bold",hjust=0.5))+
 scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#3300FF","#99CCCC"))

r=ggplot(data7, aes(x=star, y=sentiment, fill=factor(star))) + 
    geom_boxplot(alpha=0.3) +
labs(title = "LEXICON:JOKER'S")+
 theme(plot.title = element_text(color = "#999999", size = 12, face = "bold",hjust=0.5)) + 
scale_fill_brewer(palette="OrRd")
r

