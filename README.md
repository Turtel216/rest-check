# RestCheck

**RestCheck** is a lightweight, declarative CLI tool for testing and validating REST APIs. Designed for backend developers who prefer the terminal over GUI-heavy tools like Postman and for usage inside CI/CD pipelines. RestCheck uses a simple Domain Specific Language (DSL) to define HTTP requests and assertions.

---

## Features

* **Declarative Syntax:** Define tests in simple, readable `.rc` files.
* **Full HTTP Support:** GET, POST, PUT, DELETE with custom headers.
* **JSON Body Support:** Send complex JSON payloads easily.
* **Deep Validation:**
 - Check Status Codes.
 - Set strict Latency thresholds (Performance testing).
 - Validate JSON fields using dot-notation (e.g., `user.address.city`).
 - Text body substring matching.
* **Colorized Output:** Instant visual feedback (Pass/Fail) in the terminal.

---

## Installation

### Prerequisites

You need the **Haskell Tool Stack** installed.

* [Install GHCup (recommended)](https://www.haskell.org/ghcup/)

### Build from Source

```bash
# Clone the repository
git clone https://gitlab.com/papakonstantinou/rest-check.git
cd restcheck

# Build with Cabal 
cabal build

```

---

## Usage

### 1. Create a Test File

Create a file named `api_test.rc`. The syntax is designed to be readable at a glance.

**Example: Testing a User Profile**

```text
# Define the Request
GET https://jsonplaceholder.typicode.com/users/1
Header: Authorization: Bearer my-secret-token
Header: Accept: application/json

# Define Expectations
EXPECT Status 200
EXPECT Latency < 500
EXPECT JsonPath "username" == "Bret"
EXPECT JsonPath "address.city" == "Gwenborough"

```

**Example: Creating a Resource (POST)**

```text
POST https://reqres.in/api/users
Header: Content-Type: application/json

BODY
{
    "name": "Morpheus",
    "job": "leader"
}

EXPECT Status 201
EXPECT BodyContains "createdAt"

```

### 2. Run the Tool

Pass the config file to the CLI:

```bash
restcheck api_test.rc

```

### 3. View Output

```text
--- Reading Config ---
Running GET request to https://jsonplaceholder.typicode.com/users/1...
  -> Status: 200
  -> Latency: 85ms

--- Verifying Expectations ---
[PASS] ExpectStatus 200
[PASS] ExpectLatency < 500
[PASS] ExpectJsonPath "username" == "Bret"
[PASS] ExpectJsonPath "address.city" == "Gwenborough"

```

---

## CI/CD Integration

RestCheck is designed to run in headless environments. It returns standard exit codes (`0` for success, `1` for failure), making it perfect for CI/CD pipelines.

**Example: GitHub Actions Workflow**

```yaml
name: API Regression Test
on: [push]
jobs:
  test-api:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Build RestCheck
        run: cabal install
      - name: Run API Tests
        run: restcheck tests/user_api.rc

```

---

## DSL Reference

The RestCheck DSL follows a strict line-based format:

1. **Request Line:** `METHOD URL` (Must be the first non-comment line).
2. **Headers:** `Header: Key: Value`.
3. **Body Block (Optional):** Starts with `BODY`, ends at the first `EXPECT`.
4. **Expectations:**
* `EXPECT Status <Int>` - Checks HTTP status code.
* `EXPECT Latency < <Int>` - Checks response time in milliseconds.
* `EXPECT BodyContains "<String>"` - Checks if raw body contains a substring.
* `EXPECT JsonPath "<path>" == <Value>` - Checks specific JSON fields.

---

## Contributing

Contributions are welcome!

1. Fork the project.
2. Create your feature branch (`git checkout -b feature/AmazingFeature`).
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
4. Push to the branch (`git push origin feature/AmazingFeature`).
5. Open a Pull Request.

---

## License

Distributed under the MIT License. See `LICENSE` for more information.
