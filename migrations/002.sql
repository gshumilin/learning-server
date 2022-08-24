ALTER TABLE users ALTER COLUMN password TYPE text;
UPDATE users SET password='1aa66b393b8ab47d2489de43016f5f39ca8c2412737c6cdc5f7bc33de2f6dbed' WHERE id=1;