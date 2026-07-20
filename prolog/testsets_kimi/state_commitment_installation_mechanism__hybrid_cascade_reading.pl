% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade State Commitment Installation
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   A mechanism of state formation and cultural authority in which new
 *   commitments (laws, religions, administrative norms) are initiated at the
 *   political apex and projected downward, but their territorial
 *   stabilization depends on fringe communities and local intermediaries
 *   adapting and legitimating them through local interpretation. The state
 *   gains scalable stability; the fringe bears the labor of translation and
 *   the cost of suppressed prior practice. This constraint is one reading of
 *   the contested kernel 'state_commitment_installation_mechanism'; its
 *   siblings are the endogenous_climb_reading (legitimacy generated from the
 *   fringe upward) and the exogenous_imposition_reading (authority alone
 *   sufficient for installation).
 *
 * KEY AGENTS:
 *   - state_apex: Agenda-setter (institutional/arbitrage) â initiates commitments and can alter them
 *   - central_elites: Beneficiary (powerful/constrained) â gain from uniform territorial rule
 *   - local_intermediaries: Payer (moderate/constrained) â bear interpretation labor and absorb resistance
 *   - fringe_communities: Primary target (powerless/identity_locked) â must adapt identity and practice
 *   - excluded_resisters: Excluded (powerless/trapped) â rejected by the legitimating process
 *   - historical_sociologist_observer: Analytical observer (analytical/analytical) â maps the mechanism comparatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.65).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade State Commitment Installation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '4c8d3865-fdb9-4aae-a31f-83cb63652bbd').
narrative_ontology:cs_kernel_codification('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', formalized).
narrative_ontology:cs_authority_grounding('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', lineage).
narrative_ontology:cs_interpretation_layer_present('4c8d3865-fdb9-4aae-a31f-83cb63652bbd').
narrative_ontology:cs_reading_relation('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_axiom('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', foundational, apex_initiation_necessary_for_scale).
narrative_ontology:cs_axiom_status(apex_initiation_necessary_for_scale, holdable).
narrative_ontology:cs_axiom_grounding('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', apex_initiation_necessary_for_scale, empirically_contingent).
narrative_ontology:cs_axiom('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', foundational, fringe_validation_necessary_for_stability).
narrative_ontology:cs_axiom_status(fringe_validation_necessary_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', fringe_validation_necessary_for_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', hierarchical_mandate_reference).
narrative_ontology:cs_drift_state('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', modern_bureaucratic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c8d3865-fdb9-4aae-a31f-83cb63652bbd', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, central_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, local_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates new legal, religious, or cultural commitments at the center and projects them downward through administrative and symbolic infrastructure. Can alter or abandon commitments if they fail to stabilize, though this carries legitimacy costs.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the uniform application of apex commitments across the territory, which stabilizes their property rights, status hierarchies, and rule frameworks. Their exit is limited by mutual dependence on the apex structure.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, central_elites, beneficiary,
    powerful, generational, constrained, national).

% Translate apex commitments into local idioms, manage the ritual and administrative labor of legitimation, and absorb partial resistance from their communities. They occupy a structurally ambiguous position: they gain local standing as interpreters but bear the cost of smoothing over contradictions between center and periphery.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, local_intermediaries, payer,
    moderate, biographical, constrained, regional).

% Must adapt practices, rituals, and identity markers to accommodate the new commitments cascading from the apex. Their validation is required for the mechanism to stabilize, yet they bear the cost of altering lifeways and suppressing prior attachments. Exit is difficult because community survival is tied to the local ecological and political niche.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_communities, payer,
    powerless, biographical, identity_locked, local).

% Reject both the apex commitment and the local interpretation that smooths its adoption. They are not present in the legitimating dialogue because their outright rejection disqualifies them from the fringe-validation process; they are silenced or expelled.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, excluded_resisters, excluded,
    powerless, immediate, trapped, local).

% Maps the two-phase adoption process across historical cases, comparing state-formation episodes where hybrid cascade succeeded against those where it failed or mutated into pure imposition or endogenous rebellion.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, historical_sociologist_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the territorial stabilization of new political and cultural commitments by combining apex initiative with local adaptation, solving the scale problem of extending uniform rule across heterogeneous populations.
% TRANSFER_FUNCTION: Moves authority and symbolic obligation from apex to fringe; moves the labor of interpretation, ritual adaptation, and resistance-absorption from fringe communities and local intermediaries to the center's stability benefit.
% ABSENT_VOICES: Outright resisters who reject both apex and local interpretation; rival cultural authorities outside the state-fringe nexus; communities whose local practice is illegible to the cascade and are therefore bypassed entirely.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade vanished, apex commitments would not achieve territorial penetration without either collapsing into pure coercion (exogenous imposition) or fragmenting into local autonomies (endogenous climb). The two-phase structure is load-bearing for the state's scale.
% FOUNDING_PROBLEM: How to extend new state commitments across culturally heterogeneous territories and populations without generating rebellion (pure imposition is fragile) or waiting indefinitely for organic adoption (fringe-climb is too slow for state-building).
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists such as Michael Mann and Charles Tilly attest the scale-heterogeneity problem from outside the state beneficiary seat; comparative state-formation archives and colonial records corroborate the two-phase pattern, though state-sponsored historiography also claims the mechanism is still necessary for national integration.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because the mechanism systematically transfers the costs of adaptation and legitimation to the fringe while the apex captures stability. Suppression is moderate (0.58) because local interpretation absorbs resistance and reduces the need for raw coercion, though enforcement remains necessary. Theater rises from 0.20 to 0.50 over the interval as the cascade ritualizes: later stages repeat the form of fringe consultation long after the substantive negotiation has ended. Accessibility collapse (0.48) is moderate because alternatives (rebellion, autonomous community, exit to neighboring polities) persist but are rendered costly. Resistance (0.45) is moderate because partial resistance is structurally absorbed rather than eliminated. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the apex seat, the hybrid cascade is necessary coordination: without fringe validation, commitments cannot achieve scale, and the state would fragment. From the fringe seat, the same structure is asymmetric extraction: the center initiates, the periphery pays, and 'local interpretation' is the price of political survival. The engine computes this divergence from the structural data rather than resolving it by authorial fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   State_apex and central_elites are structural beneficiaries: they collect territorial stability and uniform rule (d near the beneficiary end). Fringe_communities and local_intermediaries are structural targets: they bear the labor of interpretation, adaptation, and resistance absorption (d near the target end). Local_intermediaries sit slightly closer to symmetric than fringe_communities because their mediating role offers constrained mobility and local status, though their exit remains bounded by the necessity of the mediating position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhow to extend commitments across heterogeneous territoryâwas genuine in early state formation. Mandatrophy risk emerges if modern states continue the ritual of fringe validation (consultative assemblies, 'local ownership' rhetoric) long after actual commitment installation is driven by bureaucracy, media, and surveillance. The rising theater_ratio over the interval captures this drift toward performative maintenance. A mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges would flag a captured or zombie mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_kernel_reading_ambiguity,
    'Is the hybrid cascade reading empirically distinct from its siblings, or does it merely name an intermediate zone between pure imposition and pure endogenous climb?',
    'Comparative historical analysis identifying cases where apex initiative without fringe validation failed, and fringe validation without apex initiation failed, versus cases where both were present and state formation succeeded.',
    'If the hybrid is merely a zone, it should decompose into contextual variants of the sibling readings; if structurally distinct, it warrants separate epsilon and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_kernel_reading_ambiguity, conceptual, 'Whether hybrid cascade is a distinct mechanism or an intermediate composite.').

omega_variable(
    fringe_validation_as_extraction,
    'Does fringe validation represent a reciprocal coordination benefit (fringe gains membership and protection) or an asymmetric extraction of compliance labor?',
    'Measure fringe welfare outcomes, rebellion frequency, and exit costs in historical cases of state formation relative to pre-commitment baselines and non-cascade comparisons.',
    'If extraction dominates, the computed directionality for fringe seats approaches full target; if reciprocal, the constraint moves toward rope classification for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_validation_as_extraction, empirical, 'Whether fringe validation is coordination or extraction.').

omega_variable(
    local_interpretation_suppression_mask,
    'Does local interpretation absorb resistance through genuine cultural synthesis, or does it function as delegated suppression that masks coercion behind local agency?',
    'Trace the fate of excluded resisters and the rate of prior-practice extinction in zones with strong local intermediaries versus zones with direct apex rule.',
    'If delegated suppression, the effective suppression metric is higher than structural measures suggest; if genuine synthesis, the coordination function is stronger than the extraction metric implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_interpretation_suppression_mask, empirical, 'Whether local interpretation synthesizes or suppresses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid cascade reading of the state_commitment_installation_mechanism kernel, distinct from endogenous_climb (fringe-up legitimacy generation) and exogenous_imposition (top-down authority alone). The epsilon values differ because this reading posits a two-phase mechanism with both coordination and asymmetric extraction, whereas endogenous_climb has lower apex extraction and exogenous_imposition has higher fringe extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
