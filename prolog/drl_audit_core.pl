% ============================================================================
% DR-AUDIT CORE: drl_audit_core.pl
% ============================================================================
:- module(drl_audit_core, [
    effective_extraction/3,
    structural_signature/3,
    ontological_fraud_check/2,
    omega_risk/4
]).

:- use_module(constraint_indexing). % Canonical source for power_modifier/2
:- use_module(config).             % Single source of truth for thresholds

% --- II-D: CHI CALCULATION (chi) ---
% chi(C, P) = X_base(C) * pi(P)
% Uses constraint_indexing:power_modifier/2 as the single source of truth.
effective_extraction(X_base, Power, Chi) :-
    constraint_indexing:power_modifier(Power, Modifier),
    Chi is X_base * Modifier.

% --- III-A: STRUCTURAL SIGNATURES ---
% Uses χ (power-scaled) thresholds from config §5B (logic.md alignment).
% These are quick-check audit signatures operating on pre-computed Chi values.

% Tangled Rope (TR): Hybrid coordination/extraction
structural_signature(Chi, Data, tangled_rope) :-
    config:param(tangled_rope_chi_floor, FloorX),
    config:param(tangled_rope_chi_ceil, CeilX),
    Chi >= FloorX,
    Chi =< CeilX,
    member(coord_function(true), Data),
    !.
% Snare (N): Pure extraction
structural_signature(Chi, _, snare) :-
    config:param(snare_chi_floor, FloorX),
    Chi >= FloorX,
    !.
% Rope (R): Pure coordination
structural_signature(Chi, _, rope) :-
    config:param(rope_chi_ceiling, CeilX),
    Chi =< CeilX,
    !.
% Mountain (M): Default/Unscaled limit
structural_signature(_, _, mountain).

% --- IV-B: Omega Risks ---
omega_risk(snare, rope, type_iii, "Extraction masked as coordination.").
omega_risk(snare, mountain, type_vi, "Naturalization of extraction.").

% --- IV-C: FORENSIC AUDIT TOOLS (Diagnostic Flags) ---

% False Summit (FM): High enforcement masquerading as natural law
% Logic: Claimed=mountain AND epsilon > 0.70 (logic.md Rule FM)
ontological_fraud_check(Data, fm_alert) :-
    member(claimed_type(mountain), Data),
    member(suppression_score(Epsilon), Data),
    Epsilon > 0.70, !.

% Zero Degrees of Freedom (Z): The absolute trap
% Logic: epsilon -> 1.0 AND chi -> 1.0
ontological_fraud_check(Data, z_alert) :-
    member(suppression_score(Epsilon), Data),
    Epsilon >= 0.90,
    member(base_extractiveness(X_base), Data),
    X_base >= 0.85, !.

ontological_fraud_check(_, coherent).
