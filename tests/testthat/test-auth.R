# OAuth1 signing is the riskiest new code; these tests run without network.

test_that("HMAC-SHA1 base64 helper matches known RFC 2202 vectors", {
  # RFC 2202 test case 2 (key="Jefe")
  sig <- openssl::sha1(charToRaw("what do ya want for nothing?"), key = charToRaw("Jefe"))
  expect_equal(paste(as.character(sig), collapse = ""),
               "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")
})

test_that("oauth_encode follows RFC 3986", {
  expect_equal(Rtumblr:::oauth_encode("Ladies Man"), "Ladies%20Man")
  expect_equal(Rtumblr:::oauth_encode("a+b=c"), "a%2Bb%3Dc")
  expect_equal(Rtumblr:::oauth_encode("safe-_.~"), "safe-_.~")
  expect_equal(Rtumblr:::oauth_encode("name"), "name")
})

test_that("oauth1_signature produces a complete, valid signature", {
  oauth <- Rtumblr:::oauth1_signature(
    url = "https://api.tumblr.com/v2/user/info",
    method = "GET",
    consumer_key = "ck", consumer_secret = "cs",
    token = "tok", token_secret = "toksec",
    params = list(limit = 20)
  )
  expect_true(all(c("oauth_consumer_key", "oauth_nonce", "oauth_signature_method",
                    "oauth_timestamp", "oauth_token", "oauth_version",
                    "oauth_signature") %in% names(oauth)))
  expect_equal(oauth$oauth_signature_method, "HMAC-SHA1")
  # signature is valid base64 of a 20-byte SHA1 HMAC (28 chars incl padding)
  expect_match(oauth$oauth_signature, "^[A-Za-z0-9+/]+=*$")
  expect_equal(nchar(oauth$oauth_signature), 28L)
})

test_that("oauth_header builds an OAuth Authorization header", {
  hdr <- Rtumblr:::oauth_header(list(oauth_consumer_key = "ck", oauth_signature = "ab=="))
  expect_named(hdr, c("Authorization", "Accept"))
  expect_match(hdr[["Authorization"]], "^OAuth ")
  expect_match(hdr[["Authorization"]], 'oauth_consumer_key="ck"')
  # signature value must be percent-encoded inside the header
  expect_match(hdr[["Authorization"]], 'oauth_signature="ab%3D%3D"')
})

test_that("token env var parses 2 and 4 fields", {
  two <- Rtumblr:::get_rtumblr_token_from_envvar  # function ref
  withr::with_envvar(c(RTUMBLR_TOKEN = "ck;cs"), {
    cr <- two()
    expect_equal(cr$consumer_key, "ck")
    expect_equal(cr$consumer_secret, "cs")
    expect_null(cr$oauth_token)
  })
  withr::with_envvar(c(RTUMBLR_TOKEN = "ck;cs;tok;toksec"), {
    cr <- two()
    expect_equal(cr$oauth_token, "tok")
    expect_equal(cr$oauth_token_secret, "toksec")
  })
})
