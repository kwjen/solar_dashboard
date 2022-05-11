# Calgary Solar Dashboard

## Description

* Simple, experimental app to test out various technologies
* Includes R Shiny, Python in R, and Docker
* Dockerfile needs work

## How To Use

```bash
# Clone this repository
$ git clone https://github.com/kwjen/clustering_dashboard.git

# Go into the repository
$ cd clustering_dashboard

# Build Docker image
$ docker build -t clustering_dashboard .

# Run the app
$ docker run --rm -p 3838:3838 clustering_dashboard
```

## License

Contains information licensed under the Open Government Licence – City of Calgary.


