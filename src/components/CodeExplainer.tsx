import React from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Badge } from "@/components/ui/badge";
import { Separator } from "@/components/ui/separator";
import { Info, Code, Lightbulb, FileCode } from "lucide-react";

interface CodeExplainerProps {
  codeExplanation?: {
    structure: string;
    keyConstructs: Array<{ name: string; description: string }>;
    modernizationTips: Array<{ title: string; description: string }>;
    summary: string;
  };
  isLoading?: boolean;
  selectedSection?: string;
  explanation?: string;
  code?: string;
  results?: any[];
  selectedFileIndex?: number;
}

const CodeExplainer = ({
  codeExplanation = {
    structure:
      "The COBOL code structure will be analyzed and displayed here once you upload a file.",
    keyConstructs: [
      {
        name: "PERFORM",
        description: "Used to execute a paragraph or section and return",
      },
      { name: "IF/ELSE", description: "Conditional logic construct" },
      {
        name: "EVALUATE",
        description: "Similar to switch/case in modern languages",
      },
    ],
    modernizationTips: [
      {
        title: "Reduce nesting",
        description: "Excessive nesting makes code harder to maintain",
      },
      {
        title: "Use structured programming",
        description: "Avoid GO TO statements when possible",
      },
    ],
    summary:
      "Upload a COBOL file to see a detailed explanation of its structure and complexity.",
  },
  isLoading = false,
  selectedSection = "all",
  explanation = "",
  code = "",
  results = [],
  selectedFileIndex = 0,
}: CodeExplainerProps) => {
  // Use results data if available
  const currentResult = results[selectedFileIndex];
  const currentExplanation = currentResult
    ? currentResult.explanation
    : codeExplanation;
  return (
    <div className="h-full w-full bg-background border-l">
      <Card className="h-full rounded-none border-0 shadow-none">
        <CardHeader className="px-4 py-3 border-b">
          <CardTitle className="text-lg flex items-center gap-2">
            <FileCode className="h-5 w-5" />
            Code Explainer
          </CardTitle>
        </CardHeader>
        <CardContent className="p-0">
          {isLoading ? (
            <div className="p-4 flex flex-col gap-4">
              <div className="h-6 bg-muted animate-pulse rounded-md w-3/4"></div>
              <div className="h-24 bg-muted animate-pulse rounded-md"></div>
              <div className="h-6 bg-muted animate-pulse rounded-md w-1/2"></div>
              <div className="h-32 bg-muted animate-pulse rounded-md"></div>
            </div>
          ) : (
            <Tabs defaultValue="structure" className="w-full">
              <div className="px-4 pt-2">
                <TabsList className="w-full grid grid-cols-3">
                  <TabsTrigger
                    value="structure"
                    className="flex items-center gap-1"
                  >
                    <Info className="h-4 w-4" />
                    <span className="hidden sm:inline">Structure</span>
                  </TabsTrigger>
                  <TabsTrigger
                    value="constructs"
                    className="flex items-center gap-1"
                  >
                    <Code className="h-4 w-4" />
                    <span className="hidden sm:inline">Constructs</span>
                  </TabsTrigger>
                  <TabsTrigger value="tips" className="flex items-center gap-1">
                    <Lightbulb className="h-4 w-4" />
                    <span className="hidden sm:inline">Tips</span>
                  </TabsTrigger>
                </TabsList>
              </div>

              <ScrollArea className="h-[calc(100vh-12rem)] p-4">
                <TabsContent value="structure" className="mt-2 space-y-4">
                  <div>
                    <h3 className="text-md font-medium mb-2">Code Structure</h3>
                    <p className="text-sm text-muted-foreground whitespace-pre-line">
                      {currentExplanation.structure}
                    </p>
                  </div>

                  <Separator />

                  <div>
                    <h3 className="text-md font-medium mb-2">Summary</h3>
                    <p className="text-sm text-muted-foreground">
                      {currentExplanation.summary}
                    </p>
                  </div>
                </TabsContent>

                <TabsContent value="constructs" className="mt-2">
                  <h3 className="text-md font-medium mb-3">
                    Key COBOL Constructs
                  </h3>
                  <div className="space-y-4">
                    {currentExplanation.keyConstructs.map(
                      (construct, index) => (
                        <div key={index} className="border rounded-md p-3">
                          <div className="flex items-center justify-between mb-2">
                            <Badge variant="outline" className="font-mono">
                              {construct.name}
                            </Badge>
                          </div>
                          <p className="text-sm text-muted-foreground">
                            {construct.description}
                          </p>
                        </div>
                      ),
                    )}
                  </div>
                </TabsContent>

                <TabsContent value="tips" className="mt-2">
                  <h3 className="text-md font-medium mb-3">
                    Modernization Suggestions
                  </h3>
                  <div className="space-y-4">
                    {currentExplanation.modernizationTips.map((tip, index) => (
                      <div key={index} className="border rounded-md p-3">
                        <div className="flex items-center gap-2 mb-2">
                          <Lightbulb className="h-4 w-4 text-yellow-500" />
                          <h4 className="font-medium">{tip.title}</h4>
                        </div>
                        <p className="text-sm text-muted-foreground">
                          {tip.description}
                        </p>
                      </div>
                    ))}
                  </div>
                </TabsContent>
              </ScrollArea>
            </Tabs>
          )}
        </CardContent>
      </Card>
    </div>
  );
};

export default CodeExplainer;
