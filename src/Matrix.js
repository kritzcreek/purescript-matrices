var replicate = function (n) {
  return function (v) {
    return Array(n).fill(v)
  }
}

module.exports = {replicate: replicate}
