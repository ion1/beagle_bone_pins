pragma foreign_keys = on;

begin;
  create table mpu_pins (
    id         text primary key not null,
    linux_name text null
  );

  create table signal_types (
    id   text primary key not null,
    name text not null
  );
  insert into signal_types (id, name) values ('A', 'Analog');
  insert into signal_types (id, name) values ('I', 'Input');
  insert into signal_types (id, name) values ('O', 'Output');
  insert into signal_types (id, name) values ('IO', 'Input/Output');
  insert into signal_types (id, name) values ('IOD', 'Input/Output, Open Drain');
  insert into signal_types (id, name) values ('PWR', 'Power');
  insert into signal_types (id, name) values ('GND', 'Ground');

  create table signals (
    id             text primary key not null,
    signal_type_id text not null,
    gpio_num       integer null,
    linux_pwm_name text null,
    foreign key (signal_type_id) references signal_types (id)
  );

  create table mpu_pins_signals (
    mpu_pin_id text not null,
    mode       integer null,
    signal_id  text not null,
    unique (mpu_pin_id, mode),
    foreign key (mpu_pin_id) references mpu_pins (id),
    foreign key (signal_id) references signals (id)
  );

  create table bb_pins (
    id          text primary key not null,
    name        text not null,
    mpu_pin_id  text null,
    foreign key (mpu_pin_id) references mpu_pins (id)
  );
commit;
