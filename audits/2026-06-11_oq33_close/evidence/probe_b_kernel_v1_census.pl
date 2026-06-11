/* Probe B — same branch census over the ARCHIVED kernel_v1 corpus (fresh process).
   Overlay-took-effect control: run_probe prints the resolved corpus dir and the
   corpus_constraint/1 count BEFORE any census number; expected kernel_v1 + ~1106. */

:- ['../audits/2026-06-11_oq33_close/evidence/probe_a_live_census.pl'].

run_probe_b :-
    format("=== Probe B: branch census, kernel_v1 overlay ===~n"),
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    run_probe.
