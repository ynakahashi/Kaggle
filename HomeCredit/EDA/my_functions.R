my_plot <- function(dat, y, x, skip_length = 20) {
   library(ggplot2)
   library(dplyr)
   
   tmp <- data.frame(
      y = dat[[y]],
      x = dat[[x]]
   )

   ## Skip
   if(seq_along(unique(tmp$x)) == 1) next
   if(is.character(tmp$x) && seq_along(unique(tmp$x)) > skip_length) next
   
   if(is.integer(tmp$x) && seq_along(unique(tmp$x)) <= 20) {
      tmp %>% 
         group_by(x) %>% 
         summarise(Num = n(),
                   EventNum = sum(y)) %>% 
         ungroup() %>% 
         mutate(NumPer = Num / sum(Num),
                EventRate = EventNum / Num) %>% 
         ggplot(.) +
         scale_y_continuous(breaks = seq(0, 1.1, 0.1)) + 
         ylab("Percent") +
         geom_bar(aes(x = factor(x), y = NumPer), stat ="Identity", alpha = 0.7) +
         geom_line(aes(x = factor(x), y = EventRate, group = 1), stat ="Identity", 
                   position = "Identity", color = "blue") +
         theme_classic(base_family = "HiraKakuProN-W3") +
         # xlab("重大事故における説明変数による発生率の差") +
         NULL
   } else if(is.character(tmp$x) && seq_along(unique(tmp$x)) <= 20) {
      tmp %>% 
         group_by(x) %>% 
         summarise(Num = n(),
                   EventNum = sum(y)) %>% 
         ungroup() %>% 
         mutate(NumPer = Num / sum(Num),
                EventRate = EventNum / Num) %>%
         arrange(desc(Num)) %>% 
         mutate(Group = factor(x, levels = .$x)) %>% 
         ggplot(.) +
         scale_y_continuous(breaks = seq(0, 1.1, 0.1)) + 
         ylab("Percent") +
         geom_bar(aes(x = Group, y = NumPer), stat ="Identity", alpha = 0.7) +
         geom_line(aes(x = Group, y = EventRate, group = 1), stat ="Identity", 
                   position = "Identity", color = "blue") +
         theme_classic(base_family = "HiraKakuProN-W3") +
         # xlab("重大事故における説明変数による発生率の差") +
         NULL
   } else {
      
   }
}


