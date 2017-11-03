ALTER TABLE todos ADD COLUMN live_at timestamptz;
UPDATE todos SET live_at = created_at;
ALTER TABLE todos ALTER COLUMN live_at SET NOT NULL;
ALTER TABLE todos ALTER COLUMN live_at SET DEFAULT now();
