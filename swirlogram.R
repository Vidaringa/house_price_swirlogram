
library(tidyverse)
library(httr)
library(lubridate)


# Get data ----------------------------------------------------------------

url <- "https://www.skra.is/library/Skrar/191119-N%C3%BD%C3%BAtg%C3%A1fur%C3%B6%C3%B0.xlsx"
GET(url, write_disk("skra.xlsx", overwrite = TRUE))

df <- readxl::read_excel("skra.xlsx")
max_ar <- max(df$...1, na.rm = TRUE)
max_man <- head(na.omit(df$...2), 1)
manudir_heiti <- c("janúar", "febrúar", "mars", "apríl", "maí","júní",
                   "júlí", "ágúst", "september", "október", "nóvember", "desember")

max_man_no = which(manudir_heiti == max_man)

max_dags <- make_date(year = max_ar, month = max_man_no, 1)

df <- df[,3]
df$index <- 1:nrow(df)
df <- df %>%
        arrange(desc(index)) %>%
        select(-index)

colnames(df) <- c("ibudaverd_hbs")
df$ibudaverd_hbs <- as.numeric(df$ibudaverd_hbs)
df <- df %>% na.omit()

df$date <- seq.Date(from = as.Date("1994-01-01"),
                    to = as.Date(max_dags),
                    "months")
df <- df %>%
        arrange(date)

# Vísitala neysluverðs

vnv <- read_csv2("https://px.hagstofa.is:443/pxis/sq/07c2b461-e9e8-44fe-a348-ae22794fa965",
                 locale = locale(encoding = "latin1"))

vnv_min_ar <- min(vnv$Ár)
vnv_max_ar <- max(vnv$Ár)

vnv_min_date <- make_date(vnv_min_ar, month = 1, day = 1)
vnv_max_date <- make_date(vnv_max_ar, month = 12, day = 1)

vnv_date <- seq.Date(vnv_min_date, vnv_max_date, "months")

vnv$date <- vnv_date
vnv <- vnv[,c(6, 5)]
colnames(vnv)[2] <- "vnv"

vnv <- vnv %>%
        filter(!(vnv %in% c(".", "..")))

vnv$vnv <- as.numeric(vnv$vnv)

# Útreikningar ------------------------------------------------------------

df <- df %>%
        left_join(vnv) %>%
        mutate(sma = TTR::SMA(ibudaverd_hbs, 12),
               growth = sma/lag(sma, 12) - 1,
               acceleration = (growth - lag(growth, 12))/12,
               year = year(date),
               label_year = case_when(month(date) == 1 ~ year,
                                      TRUE ~ NA_real_)) %>%
        mutate(raunverd = ibudaverd_hbs/vnv,
               sma_real = TTR::SMA(raunverd, 12),
               real_gr = sma_real/lag(sma_real, 12) - 1,
               real_acc = (real_gr - lag(real_gr, 12))/12) %>%
        filter(year >= 1997)


ggplot(filter(df, year >= 2005),
       aes(x = growth,
           y = acceleration,
           col = year)) +
        geom_path(lwd = 1) +
        geom_point() +
        scale_color_gradient(low = "#ff5959",  high = "#49beb7") +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_label(aes(label = label_year)) +
        annotate("text", label = "Þensla", x = 0.35, y = 0.02, col = "darkblue", size = 5, hjust = 1) +
        annotate("text", label = "Hægagangur", x = 0.35, y = -0.02, col = "darkblue", size = 5, hjust = 1) +
        annotate("text", label = "Niðursveifla", x = -0.1, y = -0.02, col = "darkblue", size = 5, hjust = 0) +
        annotate("text", label = "Bati", x = -0.1, y = 0.02, col = "darkblue", size = 5, hjust = 0) +
        labs(x = "12 mánaða breyting fasteignaverðs (smooth röð)",
             y = "Hröðun (breyting á 12 mánaða breytingu)",
             title = "Swirlogram fyrir fasteignamarkaðinn á höfuðborgarsvæðinu 2005 - 2019") +
         scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(labels = scales::percent) +
        theme(legend.title = element_blank())

ggsave("swirlogram.png")



# Raunverð ----------------------------------------------------------------


ggplot(filter(df, year >= 2005),
       aes(x = real_gr,
           y = real_acc,
           col = year)) +
        geom_path(lwd = 1) +
        geom_point() +
        scale_color_gradient(low = "#ff5959",  high = "#49beb7") +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_label(aes(label = label_year)) +
        annotate("text", label = "Þensla", x = 0.35, y = 0.02, col = "darkblue", size = 5, hjust = 1) +
        annotate("text", label = "Hægagangur", x = 0.35, y = -0.02, col = "darkblue", size = 5, hjust = 1) +
        annotate("text", label = "Niðursveifla", x = -0.2, y = -0.02, col = "darkblue", size = 5, hjust = 0) +
        annotate("text", label = "Bati", x = -0.2, y = 0.02, col = "darkblue", size = 5, hjust = 0) +
        labs(x = "12 mánaða breyting fasteignaverðs (smooth röð)",
             y = "Hröðun (breyting á 12 mánaða breytingu)",
             title = "Swirlogram fyrir fasteignamarkaðinn á höfuðborgarsvæðinu 2005 - 2019",
             subtitle = "Raunverð") +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(labels = scales::percent) +
        theme(legend.title = element_blank())

ggsave("swirlogram_real.png")