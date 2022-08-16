ALTER TABLE "pictures" RENAME COLUMN "base64" TO "data";
ALTER TABLE "pictures" ADD COLUMN "mime" TEXT;