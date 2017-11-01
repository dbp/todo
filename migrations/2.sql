CREATE TABLE dones (
  id SERIAL PRIMARY KEY,
  todo_id integer NOT NULL REFERENCES todos(id),
  created_at timestamptz NOT NULL DEFAULT now()
);

INSERT INTO dones (todo_id, created_at) (SELECT id, done_at FROM todos WHERE done_at IS NOT NULL);

ALTER TABLE todos DROP COLUMN done_at;
