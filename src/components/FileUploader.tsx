import React, { useState, useRef, useCallback } from "react";
import { Upload, X, FileText, AlertCircle, CheckCircle2 } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { Progress } from "@/components/ui/progress";
import { Alert, AlertDescription } from "@/components/ui/alert";

interface FileUploaderProps {
  onFilesUploaded?: (files: File[], analysisResults?: any[]) => void;
  maxFileSize?: number; // in MB
  maxFiles?: number;
  className?: string;
  isUploading?: boolean;
}

type FileWithPreview = {
  file: File;
  id: string;
  progress: number;
  error?: string;
  status: "uploading" | "error" | "success";
};

const FileUploader = ({
  onFilesUploaded = () => {},
  maxFileSize = 10, // Default 10MB
  maxFiles = 5, // Default 5 files
  className = "",
}: FileUploaderProps) => {
  const [isDragging, setIsDragging] = useState(false);
  const [files, setFiles] = useState<FileWithPreview[]>([]);
  const fileInputRef = useRef<HTMLInputElement>(null);

  // Accepted file types for COBOL files and documents
  const acceptedFileTypes = [
    ".cbl",
    ".cob",
    ".docx",
    ".pdf",
    ".txt",
    "application/pdf",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "text/plain",
  ];

  const handleDragEnter = useCallback((e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragging(true);
  }, []);

  const handleDragLeave = useCallback((e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragging(false);
  }, []);

  const handleDragOver = useCallback(
    (e: React.DragEvent<HTMLDivElement>) => {
      e.preventDefault();
      e.stopPropagation();
      if (!isDragging) setIsDragging(true);
    },
    [isDragging],
  );

  const validateFile = (file: File): string | null => {
    // Check file size
    if (file.size > maxFileSize * 1024 * 1024) {
      return `File size exceeds ${maxFileSize}MB limit`;
    }

    // Check file type
    const fileExtension = `.${file.name.split(".").pop()?.toLowerCase()}`;
    const fileType = file.type;

    if (
      !acceptedFileTypes.includes(fileExtension) &&
      !acceptedFileTypes.includes(fileType)
    ) {
      return "File type not supported. Please upload .cbl, .cob, .docx, .pdf, or .txt files";
    }

    return null;
  };

  const processFiles = useCallback(
    (fileList: FileList | null) => {
      if (!fileList) return;

      // Check if adding these files would exceed the max files limit
      if (files.length + fileList.length > maxFiles) {
        alert(`You can only upload a maximum of ${maxFiles} files`);
        return;
      }

      const newFiles: FileWithPreview[] = [];
      const validFiles: File[] = [];

      Array.from(fileList).forEach((file) => {
        const error = validateFile(file);
        const fileWithPreview: FileWithPreview = {
          file,
          id: `${file.name}-${Date.now()}`,
          progress: 0,
          status: error ? "error" : "uploading",
          error,
        };

        newFiles.push(fileWithPreview);
        if (!error) validFiles.push(file);
      });

      setFiles((prev) => [...prev, ...newFiles]);

      // Simulate upload progress for valid files
      newFiles.forEach((fileWithPreview) => {
        if (fileWithPreview.status !== "error") {
          simulateUploadProgress(fileWithPreview.id);
        }
      });

      if (validFiles.length > 0) {
        // Process each valid file through the backend
        const processFiles = async () => {
          try {
            const results = await Promise.all(
              validFiles.map(async (file) => {
                try {
                  return await uploadFileToBackend(file);
                } catch (error) {
                  console.error(`Error processing file ${file.name}:`, error);
                  // Update file status to error
                  setFiles((prev) =>
                    prev.map((f) =>
                      f.file.name === file.name
                        ? {
                            ...f,
                            status: "error",
                            error: "Failed to analyze file",
                          }
                        : f,
                    ),
                  );
                  return null;
                }
              }),
            );

            // Filter out null results (failed uploads)
            const successfulResults = results.filter(Boolean);
            if (successfulResults.length > 0) {
              onFilesUploaded(validFiles, successfulResults);
            }
          } catch (error) {
            console.error("Error processing files:", error);
          }
        };

        processFiles();
      }
    },
    [files.length, maxFiles, onFilesUploaded],
  );

  const handleDrop = useCallback(
    (e: React.DragEvent<HTMLDivElement>) => {
      e.preventDefault();
      e.stopPropagation();
      setIsDragging(false);

      const { files } = e.dataTransfer;
      processFiles(files);
    },
    [processFiles],
  );

  const handleFileInputChange = useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      processFiles(e.target.files);
      // Reset the input value so the same file can be uploaded again if needed
      if (fileInputRef.current) fileInputRef.current.value = "";
    },
    [processFiles],
  );

  const handleButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };

  const simulateUploadProgress = (fileId: string) => {
    let progress = 0;
    const interval = setInterval(() => {
      progress += Math.random() * 10;
      if (progress >= 100) {
        progress = 100;
        clearInterval(interval);
        setFiles((prev) =>
          prev.map((f) =>
            f.id === fileId ? { ...f, progress: 100, status: "success" } : f,
          ),
        );
      } else {
        setFiles((prev) =>
          prev.map((f) => (f.id === fileId ? { ...f, progress } : f)),
        );
      }
    }, 200);
  };

  const uploadFileToBackend = async (file: File): Promise<any> => {
    try {
      const formData = new FormData();
      formData.append("file", file);

      const response = await fetch("http://localhost:8000/analyze", {
        method: "POST",
        body: formData,
      });

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      return await response.json();
    } catch (error) {
      console.error("Error uploading file:", error);
      throw error;
    }
  };

  const removeFile = (fileId: string) => {
    setFiles((prev) => prev.filter((f) => f.id !== fileId));
  };

  return (
    <div className={`w-full bg-background ${className}`}>
      <div
        className={`border-2 border-dashed rounded-lg p-8 transition-colors ${isDragging ? "border-primary bg-primary/5" : "border-muted-foreground/20"} flex flex-col items-center justify-center cursor-pointer`}
        onDragEnter={handleDragEnter}
        onDragOver={handleDragOver}
        onDragLeave={handleDragLeave}
        onDrop={handleDrop}
        onClick={handleButtonClick}
      >
        <input
          type="file"
          ref={fileInputRef}
          className="hidden"
          multiple
          accept={acceptedFileTypes.join(",")}
          onChange={handleFileInputChange}
        />
        <div className="flex flex-col items-center justify-center text-center">
          <div className="p-4 rounded-full bg-primary/10 mb-4">
            <Upload className="h-10 w-10 text-primary" />
          </div>
          <h3 className="text-lg font-semibold mb-2">
            Drag & Drop COBOL Files
          </h3>
          <p className="text-sm text-muted-foreground mb-4">
            Upload .cbl, .cob, .docx, .pdf, or .txt files (max {maxFileSize}MB
            each)
          </p>
          <Button variant="outline" type="button">
            Select Files
          </Button>
          <p className="text-xs text-muted-foreground mt-4">
            Maximum {maxFiles} files can be uploaded at once
          </p>
        </div>
      </div>

      {files.length > 0 && (
        <div className="mt-6 space-y-4">
          <h4 className="text-sm font-medium">
            Uploaded Files ({files.length}/{maxFiles})
          </h4>
          <div className="space-y-3">
            {files.map((file) => (
              <Card key={file.id} className="overflow-hidden">
                <CardContent className="p-3">
                  <div className="flex items-center justify-between">
                    <div className="flex items-center space-x-3">
                      <div className="p-2 rounded-md bg-muted">
                        <FileText className="h-5 w-5" />
                      </div>
                      <div className="space-y-1">
                        <p className="text-sm font-medium truncate max-w-[200px] sm:max-w-[300px]">
                          {file.file.name}
                        </p>
                        <p className="text-xs text-muted-foreground">
                          {(file.file.size / 1024 / 1024).toFixed(2)} MB
                        </p>
                      </div>
                    </div>
                    <div className="flex items-center space-x-2">
                      {file.status === "error" ? (
                        <AlertCircle className="h-5 w-5 text-destructive" />
                      ) : file.status === "success" ? (
                        <CheckCircle2 className="h-5 w-5 text-green-500" />
                      ) : null}
                      <Button
                        variant="ghost"
                        size="icon"
                        onClick={(e) => {
                          e.stopPropagation();
                          removeFile(file.id);
                        }}
                      >
                        <X className="h-4 w-4" />
                      </Button>
                    </div>
                  </div>
                  {file.status === "error" ? (
                    <Alert variant="destructive" className="mt-2 py-2">
                      <AlertDescription>{file.error}</AlertDescription>
                    </Alert>
                  ) : (
                    <Progress value={file.progress} className="h-1 mt-2" />
                  )}
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      )}
    </div>
  );
};

export default FileUploader;
