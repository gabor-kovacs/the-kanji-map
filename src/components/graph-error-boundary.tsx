"use client";

import { catchError, type ErrorInfo } from "next/error";
import { RefreshCcwIcon } from "lucide-react";
import { Button } from "./ui/button";

function GraphErrorFallback(
  { onSwitchTo2D }: { onSwitchTo2D?: () => void },
  { error, reset }: ErrorInfo,
) {
  return (
    <div className="size-full flex flex-col items-center justify-center gap-3 p-4 text-center">
      <p className="text-sm text-muted-foreground">Failed to render the graph.</p>
      {error instanceof Error && (
        <p
          className="max-w-md truncate text-xs text-muted-foreground/70"
          title={error.message}
        >
          {error.message}
        </p>
      )}
      <div className="flex gap-2">
        <Button variant="outline" size="sm" onClick={reset}>
          <RefreshCcwIcon className="size-4" />
          Try again
        </Button>
        {onSwitchTo2D && (
          <Button variant="outline" size="sm" onClick={onSwitchTo2D}>
            Switch to 2D
          </Button>
        )}
      </div>
    </div>
  );
}

export const GraphErrorBoundary = catchError(GraphErrorFallback);
