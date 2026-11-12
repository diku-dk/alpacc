# Example client for the native server protocol of alpacc-generated
# programs (C or CUDA backend).  See docs/wire-protocols.md.
#
# It shows the intended embedding pattern:
#   1. Run `<binary> --layout` once to learn the native type sizes.
#   2. Start `<binary> --server` and keep it running.
#   3. Per request: write one counted batch (u64 count=1 + one frame),
#      flush, read the echoed count and exactly one response.
#
# All integers are in host byte order (little-endian).
#
# Usage:
#   python3 server_client.py <binary> lexer  <text> [<text> ...]
#   python3 server_client.py <binary> both   <text> [<text> ...]
#   python3 server_client.py <binary> parser <token-id> [<token-id> ...]
#
# e.g. with a combined build of grammars/arithmetic.alp:
#   python3 server_client.py ./prog both "1 + 2 * x"

import struct
import subprocess
import sys


def get_layout(binary):
    """Query the native type sizes (in bytes) from the binary."""
    layout = {}
    for line in subprocess.check_output([binary, "--layout"]).decode().splitlines():
        key, _, val = line.partition("=")
        layout[key.strip()] = int(val)
    return layout


def u64(v):
    return struct.pack("<Q", v)


def send_request(proc, payload_items, n):
    """Write one counted batch: u64 count=1, u64 frame_len, u64 n, payload."""
    payload = b"".join(payload_items)
    content = u64(n) + payload
    proc.stdin.write(u64(1) + u64(len(content)) + content)
    proc.stdin.flush()


def read_exact(stream, size):
    buf = stream.read(size)
    if len(buf) != size:
        raise EOFError("server closed the stream mid-response")
    return buf


def read_int(stream, size, signed=False):
    return int.from_bytes(read_exact(stream, size), "little", signed=signed)


def read_response(proc, kind, layout):
    """Read one response frame and decode it into Python values."""
    terminal = layout["terminal_t"]
    production = layout.get("production_t")
    index = layout.get("index_t")

    num_responses = read_int(proc.stdout, 8)
    if num_responses != 1:
        raise ValueError(f"expected an echoed count of 1, got {num_responses}")

    valid = read_exact(proc.stdout, 1)[0]
    if valid == 0:
        return None  # input was rejected

    count = read_int(proc.stdout, 8)
    if kind == "lexer":
        # (terminal id, start, end) per lexeme
        return [
            (
                read_int(proc.stdout, terminal),
                read_int(proc.stdout, index, signed=True),
                read_int(proc.stdout, index, signed=True),
            )
            for _ in range(count)
        ]
    if kind == "parser":
        # production ids of the leftmost derivation
        return [read_int(proc.stdout, production) for _ in range(count)]
    # both: preorder CST nodes (is_terminal, parent, id, start, end)
    return [
        (
            read_exact(proc.stdout, 1)[0],
            read_int(proc.stdout, index, signed=True),
            read_int(proc.stdout, production),
            read_int(proc.stdout, index, signed=True),
            read_int(proc.stdout, index, signed=True),
        )
        for _ in range(count)
    ]


def main():
    if len(sys.argv) < 4 or sys.argv[2] not in ("lexer", "parser", "both"):
        sys.exit(__doc__ or "usage: server_client.py <binary> lexer|parser|both <input>...")
    binary, kind, inputs = sys.argv[1], sys.argv[2], sys.argv[3:]

    layout = get_layout(binary)
    print(f"layout: {layout}")

    proc = subprocess.Popen([binary, "--server"],
                            stdin=subprocess.PIPE, stdout=subprocess.PIPE)

    for inp in inputs:
        if kind == "parser":
            tokens = [int(t) for t in inp.split()]
            items = [t.to_bytes(layout["terminal_t"], "little") for t in tokens]
            send_request(proc, items, len(tokens))
        else:
            data = inp.encode()
            send_request(proc, [data], len(data))

        result = read_response(proc, kind, layout)
        print(f"input: {inp!r}")
        if result is None:
            print("  rejected")
        elif kind == "lexer":
            for t, s, e in result:
                print(f"  terminal {t}: [{s}, {e})")
        elif kind == "parser":
            print(f"  derivation: {result}")
        else:
            for i, (is_term, parent, node_id, s, e) in enumerate(result):
                what = "terminal" if is_term else "production"
                span = f" [{s}, {e})" if is_term else ""
                print(f"  node {i}: {what} {node_id}, parent {parent}{span}")

    proc.stdin.close()  # EOF ends the server loop
    proc.wait()


if __name__ == "__main__":
    main()
