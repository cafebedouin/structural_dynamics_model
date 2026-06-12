% ============================================================================
% GUARD EXCLUSIONS — per-story exclusions from guard arms, ruled not derived
% ============================================================================
% OQ-114 ruling (operator, 2026-06-12; package:
% audits/2026-06-12_oq114_archive_probe/WRITEUP.md): institutional_trust_erosion
% is EXCLUDED from the unanimity guard's nl_certification_chain arm — its
% authored dissent (powerless-snare, institutional-piton) and the live FCR
% firing are two independent instruments agreeing in the fail-open direction,
% so it stays EXAMINED when its authored cells retire at Phase C.
%
% KILL CONDITIONS (both directions, pinned at the ruling — ISSUES.md OQ-114):
%   - flips IN (delete the fact) if the snare/piton dissent resolves: the
%     seat-dissent traced to a mechanism the instruments discount (e.g. its
%     duplicate-seat pairs shown to be authoring noise) or the FCR firing
%     shown spurious by its own witness;
%   - becomes PERMANENT-WITH-REASON if the dissent confirms as a genuine
%     non-mountain reading (then "why does C certify it" feeds the
%     general-mechanism item).
%
% FAIL-CLOSED CONTRACT (consumer side, signature_detection): the C arm runs
% ONLY when this module's table is present (current_predicate check); if the
% list is absent/unreadable the C arm DISABLES (old pre-C behavior: no
% protection, everything examined) — never silent protection. Do not make
% this module optional-by-convenience; it is loaded by stack.pl.
% ============================================================================

:- module(guard_exclusions, [
    nl_chain_exclusion/2
]).

%% nl_chain_exclusion(?Constraint, ?Reason)
% Pre-cohort-zero id kept for archival sweeps (kernel_v2_test2 overlays load
% this guard too); the live referent is the _c0 redraw, anchored through
% seeded_from (provenance) — identity authored forward, never name-recovered.
nl_chain_exclusion(institutional_trust_erosion, oq114_substantive_dissent).
nl_chain_exclusion(institutional_trust_erosion_c0, oq114_substantive_dissent).
