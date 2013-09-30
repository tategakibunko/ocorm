open Ocorm

module TestUser = struct
  let table_name = "test_user"
  let props = [
    ("user_id", StringProperty [NotNull; MinLength 3; MaxLength 16]);
    ("email", StringProperty [NotNull]);
    ("age", IntProperty [NotNull]);
  ]
end

