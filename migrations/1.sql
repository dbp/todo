CREATE TABLE notifications (
  id serial PRIMARY KEY,
  todo_id INTEGER NOT NULL REFERENCES todos(id),
  created_at timestamptz NOT NULL DEFAULT now()
);
