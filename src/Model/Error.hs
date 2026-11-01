-- Lỗi có kiểu của toàn chương trình. Các hàm có thể thất bại trả về
-- @Either GameError a@ thay vì làm chương trình dừng bất ngờ
module Model.Error (GameError (..), describeError) where

data GameError
  = MapParseError FilePath String     -- file map sai cú pháp (kèm dòng/cột)
  | ConfigParseError FilePath String  -- file cấu hình sai
  | FileMissing FilePath              -- file không tồn tại
  | FileError FilePath String         -- lỗi đọc/ghi file khác
  | CorruptSave FilePath String       -- file lưu bị hỏng
  | NetworkError String               -- lỗi socket
  | ProtocolError String              -- thông điệp mạng không hợp lệ
  | LineTooLong Int                   -- dòng mạng vượt quá giới hạn byte
  deriving (Show, Eq)

-- Mô tả lỗi cho người dùng (ASCII để in ra console Windows an toàn)
describeError :: GameError -> String
describeError (MapParseError p m)    = "map error in " ++ p ++ ":\n" ++ m
describeError (ConfigParseError p m) = "config error in " ++ p ++ ":\n" ++ m
describeError (FileMissing p)        = "file not found: " ++ p
describeError (FileError p m)        = "file error (" ++ p ++ "): " ++ m
describeError (CorruptSave p m)      = "corrupt save file " ++ p ++ ": " ++ m
describeError (NetworkError m)       = "network error: " ++ m
describeError (ProtocolError m)      = "protocol error: " ++ m
describeError (LineTooLong n)        = "line longer than " ++ show n ++ " bytes"