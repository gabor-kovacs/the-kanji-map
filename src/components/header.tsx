import { InfoIcon } from "lucide-react";
import Link from "next/link";
import LogoSVG from "./logo";
import { buttonVariants } from "./ui/button";
import { cn } from "@/lib/utils";
import { ThemeSwitcherButton } from "./theme-switcher";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "@/components/ui/tooltip";

export const Header = ({
  route,
  className,
}: {
  route?: string;
  className?: string;
}) => {
  return (
    <div
      className={cn(
        "flex items-center justify-between h-12 border-b",
        className
      )}
    >
      <div className="flex items-center h-ful">
        <Link href={`/`} className="flex h-full items-center">
          <LogoSVG className="h-full py-2 px-4 w-14 inline-block" />
          <h1 className="text-lg font-extrabold text-nowrap">The Kanji Map</h1>
        </Link>
      </div>
      <div className="flex px-4 gap-2">
        <Tooltip>
          <TooltipTrigger asChild>
            <Link
              href={route === "about" ? "/ " : "about"}
              className={cn(
                buttonVariants({ variant: "ghost", size: "icon" }),
                route === "about" ? "!bg-accent !text-accent-foreground" : ""
              )}
            >
              <InfoIcon className={cn("size-5")} />
            </Link>
          </TooltipTrigger>
          <TooltipContent>
            <p>About this website</p>
          </TooltipContent>
        </Tooltip>
        <Tooltip>
          <TooltipTrigger asChild>
            <ThemeSwitcherButton />
          </TooltipTrigger>
          <TooltipContent>
            <p>Change theme</p>
          </TooltipContent>
        </Tooltip>
      </div>
    </div>
  );
};
