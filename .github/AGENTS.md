# AGENTS.md: cboe-sim/.github

- `workflows/ci.yml`: one job. Checkout, the shared `ox-setup` action (opam switch cache
  keyed on the pins list in the workflow), `dune build`, `dune test`, then the reusable
  obj-magic workflow from `winterchill-trading/infra`.
- The pins list must match this repository's sibling dependencies. When `dune-project`
  gains a dependency, add its pin in the same commit, in dependency order.
- Concurrency is `cancel-in-progress: true`; push once per finished change.
- To check a run: one `gh run list --limit 1`, then at most one `gh run watch <id>
  --exit-status`. Never poll.
