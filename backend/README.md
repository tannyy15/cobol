# COBOL Code Complexity Analyzer Backend

## Setup

1. Create a `.env` file based on `.env.example` and add your Gemini API key
2. Install dependencies: `pip install -r requirements.txt`
3. Run the server: `uvicorn app.main:app --reload`

## API Endpoints

- `GET /`: Health check endpoint
- `POST /analyze`: Upload and analyze COBOL code
  - Accepts multipart/form-data with a file field
  - Returns analysis and explanation data

## Docker

Build the Docker image:
```
docker build -t cobol-analyzer-backend .
```

Run the container:
```
docker run -p 8000:8000 -e GEMINI_API_KEY=your_key_here cobol-analyzer-backend
```
