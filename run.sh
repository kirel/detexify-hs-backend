docker build -t kirel/detexify-hs-backend .
docker run --rm --name detexify -p 3000:3000 -t kirel/detexify-hs-backend
