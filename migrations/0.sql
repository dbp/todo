CREATE TABLE modes (
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL,
       account TEXT NOT NULL
);
CREATE TABLE todos (
       id serial PRIMARY KEY,
       description text NOT NULL,
       created_at timestamptz NOT NULL DEFAULT now(),
       mode_id integer NOT NULL REFERENCES modes(id),
       snooze_till timestamptz,
       deadline_at timestamptz,
       repeat_at integer,
       repeat_times integer,
       magnitude integer,
       done_at timestamptz
);
CREATE TABLE changes (
       id serial PRIMARY KEY,
       typ text NOT NULL,
       todo_id INTEGER NOT NULL REFERENCES todos(id),
       payload text,
       created_at timestamptz NOT NULL DEFAULT now()
);
