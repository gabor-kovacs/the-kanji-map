import { Header } from "@/components/header";

export default function NotFound() {
  return (
    <>
      <Header className="w-full" />
      <div className="size-full grid place-items-center">
        <h1 className="font-bold text-xl">404 - Not Found</h1>
      </div>
    </>
  );
}
