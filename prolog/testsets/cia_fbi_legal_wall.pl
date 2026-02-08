% ============================================================================
% CONSTRAINT STORY: cia_fbi_legal_wall
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_cia_fbi_legal_wall, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cia_fbi_legal_wall
 * human_readable: The CIA/FBI Intelligence-Criminal "Wall" (pre-PATRIOT Act)
 * domain: political/legal
 * * SUMMARY:
 * The "Wall" was a set of legal and procedural barriers intended to separate
 * foreign intelligence from domestic criminal investigations to protect civil
 * liberties. In practice, it became a rigid organizational silo
 * that prevented the sharing of critical terrorism data between the CIA and
 * FBI field offices, directly contributing to the failure to prevent the 9/11 attacks.
 * * KEY AGENTS:
 * - FBI Field Agent: Subject (Powerless), unable to access known intelligence.
 * - DOJ Office of Intelligence Policy and Review (OIPR): Beneficiary (Institutional), enforcing the legal separation.
 * - 9/11 Commission: Auditor (Analytical), assessing the structural failure retrospectively.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cia_fbi_legal_wall, 0.70). % High extraction of national security and agent effectiveness.
domain_priors:suppression_score(cia_fbi_legal_wall, 0.85).   % Actively punished information sharing across the divide.
domain_priors:theater_ratio(cia_fbi_legal_wall, 0.10).       % The procedures were functional, not theatrical, but had perverse outcomes.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cia_fbi_legal_wall, extractiveness, 0.70).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The institutional justification was that it was a necessary coordination rule.
narrative_ontology:constraint_claim(cia_fbi_legal_wall, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(cia_fbi_legal_wall). % Enforced by DOJ OIPR and FISA court precedent.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(cia_fbi_legal_wall, justice_department_legal_purists).
narrative_ontology:constraint_victim(cia_fbi_legal_wall, national_security_and_field_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For an FBI field agent, the Wall was a trap that prevented them from doing their job.
% χ = 0.70 (ε) * 1.5 (π(powerless)) * 0.8 (σ(local)) = 0.84. This high effective
% extraction, combined with high suppression, is a clear Snare.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For a DOJ official, the Wall was a necessary coordination mechanism (Rope) to
% protect civil liberties and ensure legal cases were not tainted by intelligence methods.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Retrospectively, the Wall is a classic Tangled Rope. It had a genuine
% coordination function (beneficiary exists) but also produced severe asymmetric
% extraction (victim exists) and required active enforcement.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cia_fbi_legal_wall_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cia_fbi_legal_wall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cia_fbi_legal_wall, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(tangled_rope_analytical_view) :-
    % Verify the analytical observer correctly identifies it as a Tangled Rope.
    constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base extraction is high enough to warrant Snare/Tangled Rope classification.
    domain_priors:base_extractiveness(cia_fbi_legal_wall, E),
    E >= 0.46.

:- end_tests(cia_fbi_legal_wall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a constraint that was well-intentioned but catastrophically dysfunctional.
 * The base extractiveness (0.70) represents the extraction of national security, not wealth.
 * The suppression score (0.85) is high because the Wall was not a passive barrier; it was
 * actively and punitively enforced, creating a culture of non-collaboration.
 *
 * The Perspectival Gap is stark:
 * - The DOJ (Institutional) saw it as a Rope, coordinating legal standards.
 * - The FBI Agent (Powerless) experienced it as a Snare, preventing them from connecting
 *   dots and making them ineffective.
 * - The Analytical view must be Tangled Rope because both of the above are true. The
 *   constraint had both a coordination function and a severe extractive one.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is a classic case of Mandatrophy. A rule designed for one purpose (protecting
 * civil liberties) ossified and created a vulnerability far greater than the one it
 * was meant to prevent. Classifying it as a Tangled Rope is crucial. A simple Snare
 * classification would miss the fact that the constraint's enforcers genuinely
 * believed they were performing a vital coordination function. The Tangled Rope
 * captures this tragic duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cia_fbi_legal_wall,
    'Would a pre-9/11 system without the Wall have produced domestic surveillance abuses that outweighed the security gain?',
    'Comparative analysis of civil liberty violations in Five Eyes nations with integrated domestic intelligence during the same period.',
    'If Yes, the Wall was a tragic Rope that failed. If No, the Wall was a pure Snare from its inception.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cia_fbi_legal_wall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the ossification of the Wall from a procedural guideline
% into a rigid, extractive barrier. Extraction accumulated as legal precedents
% hardened and inter-agency distrust grew.
%
% Theater ratio over time (remained low and functional):
narrative_ontology:measurement(cia_fbi_legal_wall_tr_t0, cia_fbi_legal_wall, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cia_fbi_legal_wall_tr_t5, cia_fbi_legal_wall, theater_ratio, 5, 0.10).
narrative_ontology:measurement(cia_fbi_legal_wall_tr_t10, cia_fbi_legal_wall, theater_ratio, 10, 0.10).

% Extraction over time (shows accumulation as procedures hardened):
narrative_ontology:measurement(cia_fbi_legal_wall_ex_t0, cia_fbi_legal_wall, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(cia_fbi_legal_wall_ex_t5, cia_fbi_legal_wall, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cia_fbi_legal_wall_ex_t10, cia_fbi_legal_wall, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The Wall was fundamentally an enforcement mechanism for a legal distinction.
narrative_ontology:coordination_type(cia_fbi_legal_wall, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */