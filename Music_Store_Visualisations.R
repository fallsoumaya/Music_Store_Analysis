# Installer les packages si tu ne les as pas déjà
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("ggrepel")
install.packages("tidyr")


# Charger les packages
library(dplyr)
library(ggplot2)

# Spécifier le chemin du dossier contenant les fichiers CSV
path <- "C:/Users/HP/OneDrive/Desktop/Music Playlist- SQL Project"

# Lister tous les fichiers CSV dans le dossier
files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

# Importer tous les fichiers CSV dans des data frames distincts
album <- read.csv(files[1])
artist <- read.csv(files[2])
customer <- read.csv(files[3])
employee <- read.csv(files[4])
genre <- read.csv(files[5])
invoice <- read.csv(files[6])
invoice_line <- read.csv(files[7])
media_type <- read.csv(files[8])
playlist <- read.csv(files[9])
playlist_track <- read.csv(files[10])
track <- read.csv(files[11])

# Afficher un aperçu de chaque dataframe pour s'assurer qu'ils sont chargés correctement
list(album = head(album), artist = head(artist), customer = head(customer), 
     employee = head(employee), genre = head(genre), invoice = head(invoice), 
     invoice_line = head(invoice_line), media_type = head(media_type), 
     playlist = head(playlist), playlist_track = head(playlist_track), 
     track = head(track))

# Vérifier les noms de colonnes de chaque data frame
names(invoice)
names(invoice_line)
names(track)
names(genre)

library(dplyr)

# Joindre les données sans sélectionner pour vérifier les colonnes
ventes_par_genre <- invoice %>%
  inner_join(invoice_line, by = "invoice_id") %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id")

# Vérifier les noms de colonnes
names(ventes_par_genre)

# Joindre les données et sélectionner les colonnes correctes
ventes_par_genre <- invoice %>%
  inner_join(invoice_line, by = "invoice_id") %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id") %>%
  select(genre_name = name.y, total, quantity)  # Utiliser 'name.y' pour le genre

# Calculer le total des ventes par genre
ventes_par_genre_totales <- ventes_par_genre %>%
  group_by(genre_name) %>%
  summarise(total_ventes = sum(total, na.rm = TRUE), .groups = "drop")

# Afficher les résultats
print(ventes_par_genre_totales)

# Créer un graphique à barres
ggplot(ventes_par_genre_totales, aes(x = reorder(genre_name, total_ventes), y = total_ventes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  labs(title = "Total des ventes par genre",
       x = "Genre",
       y = "Total des ventes") +
  theme_minimal()



# Calculer le total dépensé par chaque client
depenses_par_client <- invoice %>%
  group_by(customer_id) %>%
  summarise(total_depenses = sum(total, na.rm = TRUE), .groups = "drop")
# Créer l'histogramme
ggplot(depenses_par_client, aes(x = total_depenses)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", boundary = 0) +
  labs(title = "Distribution des montants dépensés par les clients",
       x = "Montant dépensé",
       y = "Nombre de clients") +
  theme_minimal()



library(lubridate)

# Convertir invoice_date en format date
invoice <- invoice %>%
  mutate(invoice_date = as.Date(invoice_date, format = "%Y-%m-%d"))  # Ajuste le format si nécessaire


# Extraire le mois et l'année de la date de la facture
ventes_mensuelles <- invoice %>%
  mutate(mois = floor_date(invoice_date, "month")) %>%
  group_by(mois) %>%
  summarise(total_ventes = sum(total, na.rm = TRUE), .groups = "drop")

# Graphique linéaire des ventes mensuelles
ggplot(ventes_mensuelles, aes(x = mois, y = total_ventes)) +
  geom_line(color = "blue", linewidth = 1) +  # Remplacer size par linewidth
  labs(title = "Évolution des ventes mensuelles",
       x = "Mois",
       y = "Total des ventes") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")

# Extraire le trimestre et l'année de la date de la facture
ventes_trimestrielles <- invoice %>%
  mutate(trimestre = quarter(invoice_date, with_year = TRUE)) %>%
  group_by(trimestre) %>%
  summarise(total_ventes = sum(total, na.rm = TRUE), .groups = "drop")

# Graphique linéaire des ventes trimestrielles
ggplot(ventes_trimestrielles, aes(x = trimestre, y = total_ventes)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Évolution des ventes trimestrielles",
       x = "Trimestre",
       y = "Total des ventes") +
  theme_minimal()


# Joindre les données sans résumer pour vérifier les colonnes
revenus_data <- invoice_line %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id")

# Vérifier les noms des colonnes
names(revenus_data)

# Joindre les données nécessaires et calculer les revenus par artiste
revenus_par_artiste <- invoice_line %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id") %>%
  group_by(composer) %>%
  summarise(total_revenus = sum(`unit_price.x` * quantity, na.rm = TRUE), .groups = "drop") %>%  # Utilise le bon nom de colonne
  arrange(desc(total_revenus))  # Trier par revenus décroissants

# Afficher les artistes les plus rentables
head(revenus_par_artiste)

library(ggrepel)  # Assurez-vous d'avoir cette bibliothèque installée

# Limiter aux 15 artistes les plus rentables
top_artistes <- revenus_par_artiste %>%
  top_n(15, total_revenus)

# Graphique de revenus par artiste avec ggrepel
ggplot(top_artistes, aes(x = reorder(composer, total_revenus), y = total_revenus)) +
  geom_point(color = "blue", size = 3, show.legend = FALSE) +  # Supprimer la légende des points
  geom_text_repel(aes(label = composer), size = 3) +  # Utiliser geom_text_repel pour éviter le chevauchement
  labs(title = "Top Artistes les Plus Rentables",
       x = "Artiste",
       y = "Total des Revenus") +
  coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Ajuster la taille du texte des axes



# Compter le nombre de clients par pays
clients_par_pays <- customer %>%
  group_by(country) %>%
  summarise(nombre_clients = n()) %>%
  arrange(desc(nombre_clients))  # Trier par nombre de clients décroissant
# Créer un graphique à secteurs
ggplot(clients_par_pays, aes(x = "", y = nombre_clients, fill = country)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +  # Transformer en graphique à secteurs
  labs(title = "Répartition des Clients par Pays",
       fill = "Pays") +
  theme_void() +  # Supprimer le fond et les axes
  theme(legend.position = "right")  # Position de la légende



# Identifier les pistes ayant la durée la plus longue
pistes_longues <- track %>%
  arrange(desc(milliseconds)) %>%  # Utiliser 'milliseconds' pour la durée
  select(track_id, name, milliseconds) %>%  # Sélectionner les colonnes souhaitées sans 'popularity'
  head(10)  # Obtenir les 10 pistes les plus longues

# Afficher les pistes longues
print(pistes_longues)

# Histogramme de la distribution des longueurs de pistes
ggplot(track, aes(x = milliseconds)) +
  geom_histogram(binwidth = 10000, fill = "dodgerblue", color = "black") +  # Ajuste le binwidth selon tes préférences
  labs(title = "Distribution des Longueurs des Pistes",
       x = "Durée (millisecondes)",
       y = "Nombre de Pistes") +
  theme_minimal()



# Calculer le total des factures par ville
total_factures_par_ville <- invoice %>%
  group_by(billing_city) %>%
  summarize(total_factures = sum(total, na.rm = TRUE)) %>%
  arrange(desc(total_factures))  # Optionnel : trier par total décroissant

# Afficher le résultat
print(total_factures_par_ville)

# Créer un graphique à barres
ggplot(total_factures_par_ville, aes(x = reorder(billing_city, total_factures), y = total_factures)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total des Factures par Ville",
       x = "Ville",
       y = "Total des Factures") +
  theme_minimal() +
  coord_flip()  # Optionnel : inverse les axes pour une meilleure lisibilité



# Fusionner les DataFrames
total_spent_by_genre <- invoice %>%
  inner_join(invoice_line, by = "invoice_id") %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id") %>%
  group_by(name.y) %>% # group by genre name
  summarise(total_spent = sum(total)) # Calculate total spent per genre

# Afficher les résultats
print(total_spent_by_genre)

# Fusionner les DataFrames pour le box plot
box_plot_data <- invoice %>%
  inner_join(invoice_line, by = "invoice_id") %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id")

# Créer le box plot
ggplot(box_plot_data, aes(x = name.y, y = total)) +
  geom_boxplot() +
  labs(title = "Comparaison des Dépenses entre Genres",
       x = "Genres",
       y = "Montant Dépensé") +
  theme_minimal()



# Fusionner les DataFrames pour calculer le total des achats par client
total_purchases_by_customer <- invoice %>%
  inner_join(invoice_line, by = "invoice_id") %>%
  inner_join(customer, by = "customer_id") %>%
  group_by(customer_id, first_name, last_name) %>%
  summarise(total_spent = sum(total), .groups = 'drop') %>%  # Calculate total spent by customer
  arrange(desc(total_spent))  # Order by total spent in descending order

# Afficher les clients les plus dépensiers
print(total_purchases_by_customer)

# Limiter les données aux 10 clients les plus dépensiers pour la visualisation
top_customers <- total_purchases_by_customer %>%
  top_n(10, total_spent)

# Créer le graphique à barres
ggplot(top_customers, aes(x = reorder(paste(first_name, last_name), total_spent), y = total_spent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Clients par Montant Dépensé",
       x = "Clients",
       y = "Montant Dépensé") +
  theme_minimal() +
  coord_flip()  # Inverser les axes pour une meilleure lisibilité



library(tidyr)

# Fusionner les DataFrames pour obtenir le total des achats par pays et par genre
sales_by_country_genre <- invoice %>%
  inner_join(invoice_line, by = "invoice_id") %>%
  inner_join(customer, by = "customer_id") %>%
  inner_join(track, by = "track_id") %>%
  inner_join(genre, by = "genre_id") %>%
  group_by(country, name.y) %>%
  summarise(total_sales = sum(total), .groups = 'drop')  # Calculer le total des ventes

# Transformer les données en format matrice
sales_matrix <- sales_by_country_genre %>%
  pivot_wider(names_from = name.y, values_from = total_sales, values_fill = list(total_sales = 0))  # Remplir les NA par 0

# Afficher la matrice des ventes
print(sales_matrix)

# Créer la heatmap
ggplot(sales_by_country_genre, aes(x = name.y, y = country, fill = total_sales)) +
  geom_tile(color = "white") +  # Ajouter une bordure blanche autour des tuiles
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Définir le dégradé de couleur
  labs(title = "Heatmap des Achats par Pays et Genre",
       x = "Genre",
       y = "Pays",
       fill = "Total des Ventes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les labels de l'axe x pour une meilleure lisibilité
