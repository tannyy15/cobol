import React from "react";
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Progress } from "@/components/ui/progress";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import { Badge } from "@/components/ui/badge";
import { Download, FileCode, Info, AlertCircle } from "lucide-react";
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";

interface ComplexityMetric {
  name: string;
  value: number;
  maxValue: number;
  severity: "low" | "medium" | "high";
  description: string;
}

interface CodeSection {
  id: string;
  name: string;
  code: string;
  complexity: number;
  issues: string[];
}

interface ComplexityDashboardProps {
  fileName?: string;
  overallComplexity?: number;
  metrics?: ComplexityMetric[];
  codeSections?: CodeSection[];
  onSectionSelect?: (sectionId: string) => void;
  onDownloadReport?: () => void;
  results?: any[];
  selectedFileIndex?: number;
  onFileSelect?: (index: number) => void;
}

const ComplexityDashboard: React.FC<ComplexityDashboardProps> = ({
  fileName = "No file analyzed",
  overallComplexity = 0,
  metrics = [
    {
      name: "Cyclomatic Complexity",
      value: 12,
      maxValue: 30,
      severity: "medium" as const,
      description:
        "Measures the number of linearly independent paths through the code",
    },
    {
      name: "Nesting Depth",
      value: 5,
      maxValue: 10,
      severity: "medium" as const,
      description: "Maximum level of nested control structures",
    },
    {
      name: "Statement Count",
      value: 245,
      maxValue: 500,
      severity: "low" as const,
      description: "Total number of executable statements",
    },
  ],
  codeSections = [
    {
      id: "section1",
      name: "DATA DIVISION",
      code: "       DATA DIVISION.\n       FILE SECTION.\n       FD  CUSTOMER-FILE\n           LABEL RECORDS ARE STANDARD\n           RECORD CONTAINS 100 CHARACTERS.\n       01  CUSTOMER-RECORD.\n           05  CUSTOMER-ID       PIC X(5).\n           05  CUSTOMER-NAME     PIC X(20).\n           05  CUSTOMER-ADDRESS  PIC X(50).\n           05  CUSTOMER-BALANCE  PIC 9(7)V99.",
      complexity: 1,
      issues: [],
    },
    {
      id: "section2",
      name: "PROCEDURE DIVISION",
      code: "       PROCEDURE DIVISION.\n       MAIN-LOGIC.\n           PERFORM INITIALIZATION\n           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE = 'Y'\n           PERFORM TERMINATION\n           STOP RUN.\n\n       INITIALIZATION.\n           OPEN INPUT CUSTOMER-FILE\n           OPEN OUTPUT REPORT-FILE\n           PERFORM READ-RECORD\n           MOVE SPACES TO REPORT-LINE.",
      complexity: 4,
      issues: ["Multiple nested PERFORM statements", "Missing error handling"],
    },
  ],
  onSectionSelect = () => {},
  onDownloadReport = () => {},
  results = [],
  selectedFileIndex = 0,
  onFileSelect = () => {},
}) => {
  // Use results data if available
  const currentResult = results[selectedFileIndex];

  // Extract data from the current result if available
  const currentFileName = currentResult ? currentResult.fileName : fileName;
  const currentOverallComplexity = currentResult
    ? currentResult.analysis.overallComplexity
    : overallComplexity;
  const currentMetrics = currentResult
    ? currentResult.analysis.metrics
    : metrics;
  const currentCodeSections = currentResult
    ? currentResult.analysis.codeSections
    : codeSections;
  const getSeverityColor = (severity: string) => {
    switch (severity) {
      case "low":
        return "bg-green-500";
      case "medium":
        return "bg-yellow-500";
      case "high":
        return "bg-red-500";
      default:
        return "bg-gray-500";
    }
  };

  const getSeverityBadge = (severity: string) => {
    switch (severity) {
      case "low":
        return (
          <Badge variant="secondary" className="bg-green-100 text-green-800">
            Low
          </Badge>
        );
      case "medium":
        return (
          <Badge variant="secondary" className="bg-yellow-100 text-yellow-800">
            Medium
          </Badge>
        );
      case "high":
        return (
          <Badge variant="secondary" className="bg-red-100 text-red-800">
            High
          </Badge>
        );
      default:
        return <Badge variant="secondary">Unknown</Badge>;
    }
  };

  const getOverallComplexityLabel = () => {
    if (overallComplexity < 3) return "Low";
    if (overallComplexity < 7) return "Medium";
    return "High";
  };

  const getOverallComplexityColor = () => {
    if (overallComplexity < 3) return "text-green-500";
    if (overallComplexity < 7) return "text-yellow-500";
    return "text-red-500";
  };

  return (
    <div className="w-full bg-background p-4 rounded-lg">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h2 className="text-2xl font-bold">Complexity Analysis</h2>
          <p className="text-muted-foreground">{currentFileName}</p>
        </div>
        <Button
          onClick={onDownloadReport}
          variant="outline"
          className="flex items-center gap-2"
        >
          <Download size={16} />
          Download Report
        </Button>
      </div>

      {results.length > 1 && (
        <div className="mb-6">
          <h3 className="text-sm font-medium mb-2">Select File</h3>
          <div className="flex flex-wrap gap-2">
            {results.map((result, index) => (
              <Button
                key={index}
                variant={selectedFileIndex === index ? "default" : "outline"}
                size="sm"
                onClick={() => onFileSelect(index)}
              >
                {result.fileName}
              </Button>
            ))}
          </div>
        </div>
      )}

      <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-6">
        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium">
              Overall Complexity
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex items-end gap-2">
              <span
                className={`text-4xl font-bold ${getOverallComplexityColor()}`}
              >
                {currentOverallComplexity}/10
              </span>
              <span className="text-muted-foreground mb-1">
                {getOverallComplexityLabel()}
              </span>
            </div>
          </CardContent>
        </Card>

        {currentMetrics.map((metric, index) => (
          <Card key={index}>
            <CardHeader className="pb-2">
              <div className="flex justify-between">
                <CardTitle className="text-sm font-medium">
                  {metric.name}
                </CardTitle>
                {getSeverityBadge(metric.severity)}
              </div>
            </CardHeader>
            <CardContent>
              <div className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span>{metric.value}</span>
                  <span className="text-muted-foreground">
                    {metric.maxValue}
                  </span>
                </div>
                <Progress
                  value={(metric.value / metric.maxValue) * 100}
                  className="h-2"
                />
                <p className="text-xs text-muted-foreground">
                  {metric.description}
                </p>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>

      <Tabs defaultValue="code">
        <TabsList>
          <TabsTrigger value="code">Code Sections</TabsTrigger>
          <TabsTrigger value="issues">Issues</TabsTrigger>
        </TabsList>

        <TabsContent value="code" className="mt-4">
          <Accordion type="single" collapsible className="w-full">
            {currentCodeSections.map((section) => (
              <AccordionItem key={section.id} value={section.id}>
                <AccordionTrigger className="hover:bg-muted/50 px-4 rounded-md">
                  <div className="flex items-center gap-2">
                    <FileCode size={16} />
                    <span>{section.name}</span>
                    {section.issues.length > 0 && (
                      <Badge
                        variant="outline"
                        className="ml-2 bg-amber-100 text-amber-800"
                      >
                        {section.issues.length}{" "}
                        {section.issues.length === 1 ? "issue" : "issues"}
                      </Badge>
                    )}
                  </div>
                </AccordionTrigger>
                <AccordionContent>
                  <div className="bg-muted p-4 rounded-md font-mono text-sm whitespace-pre overflow-x-auto">
                    {section.code}
                  </div>
                  <div className="mt-4 flex justify-between">
                    <div className="flex items-center gap-2">
                      <Info size={16} className="text-muted-foreground" />
                      <span className="text-sm">
                        Complexity: {section.complexity}
                      </span>
                    </div>
                    <Button
                      size="sm"
                      variant="ghost"
                      onClick={() => onSectionSelect(section.id)}
                    >
                      View Details
                    </Button>
                  </div>
                </AccordionContent>
              </AccordionItem>
            ))}
          </Accordion>
        </TabsContent>

        <TabsContent value="issues" className="mt-4">
          <Card>
            <CardContent className="pt-6">
              {currentCodeSections.some(
                (section) => section.issues.length > 0,
              ) ? (
                <div className="space-y-4">
                  {currentCodeSections
                    .filter((section) => section.issues.length > 0)
                    .map((section) => (
                      <div
                        key={section.id}
                        className="pb-4 border-b border-border last:border-0"
                      >
                        <h3 className="font-medium mb-2">{section.name}</h3>
                        <ul className="space-y-2">
                          {section.issues.map((issue, idx) => (
                            <li key={idx} className="flex gap-2 text-sm">
                              <AlertCircle
                                size={16}
                                className="text-amber-500 mt-0.5 shrink-0"
                              />
                              <span>{issue}</span>
                            </li>
                          ))}
                        </ul>
                      </div>
                    ))}
                </div>
              ) : (
                <div className="text-center py-8 text-muted-foreground">
                  <p>No issues detected</p>
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default ComplexityDashboard;
