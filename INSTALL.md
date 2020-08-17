# Steps to regenerate the renv recorded packages

```
docker-compose exec -u shiny -w /srv/shiny-server/dorothea shiny rm -rf renv renv.lock .Rprofile
docker-compose exec -u shiny -w /srv/shiny-server/dorothea shiny R -f "regen_bootstrap.R"
```

# Step to run to properly deploy dorothea inside Shiny docker-compose

```
docker-compose exec shiny /srv/shiny-server/dorothea/install_deps.sh
```

# (Superseded) Step to run to materialize dependences

docker-compose exec -u shiny -w /srv/shiny-server/dorothea shiny R -f "bootstrap.R"
