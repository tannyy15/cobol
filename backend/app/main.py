from fastapi import FastAPI, UploadFile, File, HTTPException, Depends, Form, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import os
import google.generativeai as genai
from dotenv import load_dotenv
import tempfile
import shutil
import json

# Load environment variables
load_dotenv()

# Configure Gemini API
GEMINI_API_KEY = os.getenv("GEMINI_API_KEY")
if not GEMINI_API_KEY:
    raise ValueError("GEMINI_API_KEY environment variable not set")

genai.configure(api_key=GEMINI_API_KEY)
model = genai.GenerativeModel('gemini-1.5-pro')

app = FastAPI(title="COBOL Code Complexity Analyzer API")

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # In production, replace with specific origins
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.get("/")
async def read_root():
    return {"message": "COBOL Code Complexity Analyzer API"}

@app.post("/analyze")
async def analyze_code(file: UploadFile = File(...)):
    try:
        # Create a temporary file to store the uploaded content
        with tempfile.NamedTemporaryFile(delete=False) as temp_file:
            shutil.copyfileobj(file.file, temp_file)
            temp_file_path = temp_file.name
        
        # Read the file content
        with open(temp_file_path, 'r', errors='ignore') as f:
            code_content = f.read()
        
        # Clean up the temporary file
        os.unlink(temp_file_path)
        
        # Analyze the code using Gemini
        analysis_prompt = f"""
        You are a COBOL code complexity analyzer. Analyze the following COBOL code and provide:
        1. Overall complexity score (1-10).
        2. Detailed metrics:
           - Cyclomatic complexity (numeric value)
           - Nesting depth (numeric value)
           - Statement count (numeric value)
        3. Identify complex sections with their names and complexity scores.
        4. List any issues found in each section.
        
        Format your response as a JSON object with the following structure:
        {{"overallComplexity": number,
          "metrics": [
            {{"name": "Cyclomatic Complexity", "value": number, "maxValue": 30, "severity": "low|medium|high", "description": "string"}},
            {{"name": "Nesting Depth", "value": number, "maxValue": 10, "severity": "low|medium|high", "description": "string"}},
            {{"name": "Statement Count", "value": number, "maxValue": 500, "severity": "low|medium|high", "description": "string"}}
          ],
          "codeSections": [
            {{"id": "string", "name": "string", "code": "string", "complexity": number, "issues": ["string"]}}
          ]
        }}
        
        Here's the COBOL code to analyze:
        
        ```
        {code_content}
        ```
        
        Only respond with the JSON object, nothing else.
        """
        
        analysis_response = model.generate_content(analysis_prompt)
        analysis_text = analysis_response.text
        
        # Extract JSON from the response
        try:
            # Try to parse the response as JSON directly
            analysis_data = json.loads(analysis_text)
        except json.JSONDecodeError:
            # If direct parsing fails, try to extract JSON from markdown code blocks
            import re
            json_match = re.search(r'```(?:json)?\s*({.*?})\s*```', analysis_text, re.DOTALL)
            if json_match:
                analysis_data = json.loads(json_match.group(1))
            else:
                raise ValueError("Could not extract valid JSON from the model response")
        
        # Generate explanation using Gemini
        explanation_prompt = f"""
        You are a COBOL code explainer. Analyze the following COBOL code and provide:
        1. A detailed explanation of the code structure.
        2. Key COBOL constructs used in the code with descriptions.
        3. Modernization tips for improving the code.
        4. A summary of the code's purpose and functionality.
        
        Format your response as a JSON object with the following structure:
        {{"structure": "string",
          "keyConstructs": [
            {{"name": "string", "description": "string"}}
          ],
          "modernizationTips": [
            {{"title": "string", "description": "string"}}
          ],
          "summary": "string"
        }}
        
        Here's the COBOL code to explain:
        
        ```
        {code_content}
        ```
        
        Only respond with the JSON object, nothing else.
        """
        
        explanation_response = model.generate_content(explanation_prompt)
        explanation_text = explanation_response.text
        
        # Extract JSON from the response
        try:
            # Try to parse the response as JSON directly
            explanation_data = json.loads(explanation_text)
        except json.JSONDecodeError:
            # If direct parsing fails, try to extract JSON from markdown code blocks
            import re
            json_match = re.search(r'```(?:json)?\s*({.*?})\s*```', explanation_text, re.DOTALL)
            if json_match:
                explanation_data = json.loads(json_match.group(1))
            else:
                raise ValueError("Could not extract valid JSON from the model response")
        
        # Combine analysis and explanation data
        result = {
            "fileName": file.filename,
            "analysis": analysis_data,
            "explanation": explanation_data
        }
        
        return JSONResponse(content=result)
    
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error analyzing code: {str(e)}")
    finally:
        file.file.close()

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app.main:app", host="0.0.0.0", port=8000, reload=True)
