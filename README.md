## TANK BATTLE

Game bắn xe tăng 2D (Gloss): chơi 2 người, 3 bản đồ đọc từ file, tự động lưu bảng điểm, gửi kết quả trận qua socket TCP.

- Phiên bản: GHC 9.6.6 (Stack snapshot `lts-22.28`, xem `stack.yaml`).
- Thư viện ngoài: gloss, parallel, stm, async, megaparsec, network, filepath, deepseq; test: hspec, QuickCheck, directory.

### Cách chơi
| | P1 (hồng) | P2 (xanh) |
|---|---|---|
| Tiến / lùi | W / S | Lên / Xuống |
| Quay | A / D | Trái / Phải |
| Bắn | Space | Enter |

Menu có 2 nút: **2 PLAYERS**, **HELP**. Sau đó chọn map và PLAY. Phím **M** quay về menu. Bắn có hồi chiêu 20 frame (~0,33 giây). Đạn gây 20 sát thương, gạch vỡ sau 2 phát, thép không vỡ.

## Hướng dẫn chạy
- B1: tải source code từ link github sau: 
- B2: giải nén, mở source code trong vs code
- B3: mở terminal, chạy lệnh` stack build ` cài GHC và biên dịch các module
- B4: chạy ` .\run.bat ` để chạy game

Các lệnh chạy khác
```
stack test                          # chạy toàn bộ kiểm thử
.\run.bat --lazy-demo               # demo đánh giá lười/chặt
.\run.bat --bench 300 +RTS -N1 -s   # benchmark song song, so với -N4
.\run.bat -- 9000                   # server nhận kết quả trận (cổng 9000)
.\run.bat --stats localhost 9000    # hỏi bảng điểm của server
```

Nếu `stack build` báo lỗi hpack/cabal, xóa file `tank-battle.cabal` rồi build lại (Stack sẽ sinh lại từ `package.yaml`).


## Khi gặp lỗi `unknown GLUT entry glutInit` (Windows)
### 5.1. Tìm thư viện GLUT trên máy
Mở PowerShell và chạy: ` Get-ChildItem C:\ -Filter freeglut.dll -Recurse -ErrorAction SilentlyContinue | Select-Object FullName `
### 5.2. Xác định đường dẫn thư mục chứa DLL
Ví dụ kết quả tìm kiếm:

C:\Program Files\Dell\DTP\DiagnosticsSubAgent\IPDT\freeglut.dll

Trong đó:
•	freeglut.dll: Tên file thư viện.
•	C:\Program Files\Dell\DTP\DiagnosticsSubAgent\IPDT: Đường dẫn thư mục chứa DLL.
Chỉ lấy đường dẫn thư mục, không lấy tên file DLL.
### 5.3. Thêm đường dẫn DLL vào PATH
Nếu đã xác định được DLL tương thích, nhập lệnh:

$env:PATH = "DUONG_DAN_THU_MUC_CHUA_DLL;" + $env:PATH

Thay DUONG_DAN_THU_MUC_CHUA_DLL bằng đường dẫn thực tế trên máy.

Ví dụ:

$env:PATH = "C:\Program Files\Dell\DTP\DiagnosticsSubAgent\IPDT;" + $env:PATH

Đây chỉ là ví dụ từ một máy Dell, không phải đường dẫn mặc định trên mọi máy.

Lưu ý: Không phải DLL nào tìm được cũng tương thích. Nếu máy không có DLL hoặc thêm PATH vẫn không chạy được, cần cài GLUT phù hợp. Không nên sử dụng DLL bất kỳ chỉ vì có tên freeglut.dll.

### 5.4. Chạy lại game

Lưu ý: Lệnh thêm $env:PATH chỉ có hiệu lực trong Terminal hiện tại. Khi mở Terminal mới, có thể cần nhập lại nếu chưa cấu hình PATH lâu dài.