import React, { useState } from "react";
import { MoonIcon, SunIcon } from "lucide-react";
import { Button } from "./ui/button";
import { Card, CardContent } from "./ui/card";
import { Separator } from "./ui/separator";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "./ui/tabs";
import FileUploader from "./FileUploader";
import ComplexityDashboard from "./ComplexityDashboard";
import CodeExplainer from "./CodeExplainer";

interface AnalysisResult {
  fileName: string;
  complexity: {
    score: number;
    level: "Low" | "Medium" | "High";
    cyclomaticComplexity: number;
    nestingDepth: number;
    statementCount: number;
  };
  code: string;
  explanation: string;
}

export default function Home() {
  const [theme, setTheme] = useState<"light" | "dark">("light");
  const [files, setFiles] = useState<File[]>([]);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [analysisResults, setAnalysisResults] = useState<AnalysisResult[]>([]);
  const [selectedFileIndex, setSelectedFileIndex] = useState<number | null>(
    null,
  );
  const [activeTab, setActiveTab] = useState("upload");

  const toggleTheme = () => {
    setTheme(theme === "light" ? "dark" : "light");
    // In a real implementation, you would apply the theme to the document
    // document.documentElement.classList.toggle('dark');
  };

  const handleFilesUploaded = (
    uploadedFiles: File[],
    backendResults?: any[],
  ) => {
    setFiles(uploadedFiles);
    setIsAnalyzing(true);

    if (backendResults && backendResults.length > 0) {
      // Use the results from the backend
      setAnalysisResults(backendResults);
      setIsAnalyzing(false);
      setActiveTab("results");
      setSelectedFileIndex(0);
    } else {
      // Fallback to mock data if backend results are not available
      setTimeout(() => {
        const mockResults: AnalysisResult[] = uploadedFiles.map((file) => ({
          fileName: file.name,
          complexity: {
            score: Math.floor(Math.random() * 10) + 1,
            level: ["Low", "Medium", "High"][Math.floor(Math.random() * 3)] as
              | "Low"
              | "Medium"
              | "High",
            cyclomaticComplexity: Math.floor(Math.random() * 20) + 1,
            nestingDepth: Math.floor(Math.random() * 5) + 1,
            statementCount: Math.floor(Math.random() * 500) + 50,
          },
          code: `      * Sample COBOL code snippet
         IDENTIFICATION DIVISION.
         PROGRAM-ID. SAMPLE-PROGRAM.
         ENVIRONMENT DIVISION.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         01 WS-COUNTER PIC 9(3) VALUE ZERO.
         PROCEDURE DIVISION.
         MAIN-PARA.
             PERFORM VARYING WS-COUNTER FROM 1 BY 1
                 UNTIL WS-COUNTER > 10
                 DISPLAY "Counter: " WS-COUNTER
             END-PERFORM.
             STOP RUN.`,
          explanation:
            "This is a simple COBOL program that displays numbers from 1 to 10 using a PERFORM VARYING loop construct. The program has low complexity with minimal nesting and straightforward control flow.",
        }));

        setAnalysisResults(mockResults);
        setIsAnalyzing(false);
        setActiveTab("results");
        if (mockResults.length > 0) {
          setSelectedFileIndex(0);
        }
      }, 2000);
    }
  };

  const handleFileSelect = (index: number) => {
    setSelectedFileIndex(index);
  };

  return (
    <div className="min-h-screen bg-background flex flex-col">
      {/* Header */}
      <header className="border-b p-4 flex justify-between items-center">
        <h1 className="text-2xl font-bold">COBOL Code Complexity Analyzer</h1>
        <Button variant="ghost" size="icon" onClick={toggleTheme}>
          {theme === "light" ? (
            <MoonIcon className="h-5 w-5" />
          ) : (
            <SunIcon className="h-5 w-5" />
          )}
        </Button>
      </header>

      {/* Main Content */}
      <div className="flex flex-1 overflow-hidden">
        {/* Main Panel */}
        <div className="flex-1 p-6 overflow-auto">
          <Tabs
            value={activeTab}
            onValueChange={setActiveTab}
            className="w-full"
          >
            <TabsList className="mb-4">
              <TabsTrigger value="upload">Upload Files</TabsTrigger>
              <TabsTrigger
                value="results"
                disabled={analysisResults.length === 0}
              >
                Analysis Results
              </TabsTrigger>
            </TabsList>

            <TabsContent value="upload" className="mt-2">
              <Card>
                <CardContent className="pt-6">
                  <FileUploader
                    onFilesUploaded={handleFilesUploaded}
                    isUploading={isAnalyzing}
                  />
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="results" className="mt-2">
              {analysisResults.length > 0 && selectedFileIndex !== null && (
                <ComplexityDashboard
                  results={analysisResults}
                  selectedFileIndex={selectedFileIndex}
                  onFileSelect={handleFileSelect}
                />
              )}
            </TabsContent>
          </Tabs>
        </div>

        {/* Sidebar */}
        <div className="w-[350px] border-l bg-muted/20 overflow-auto">
          <div className="p-4">
            <h2 className="text-lg font-semibold mb-2">Code Explanation</h2>
            <Separator className="mb-4" />
            {selectedFileIndex !== null && analysisResults.length > 0 ? (
              <CodeExplainer
                results={analysisResults}
                selectedFileIndex={selectedFileIndex}
              />
            ) : (
              <p className="text-muted-foreground text-sm">
                Upload and analyze COBOL code to see AI-generated explanations
                here.
              </p>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
