% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto Lock-In
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the minoritarian veto reading of the
 *   supermajority_threshold kernel. Under this reading, a constitutional
 *   supermajority requirement for amendmentsâsuch as the two-thirds or
 *   three-fourths thresholds common in federal systemsâfunctions not as a
 *   consensus-forcing device but as a structural snare. The threshold
 *   empowers blocking minorities to entrench the status quo against
 *   contemporary majoritarian will, converting historical privilege into
 *   permanent veto power. Identifiable beneficiaries (entrenched elites and
 *   status quo beneficiaries) capture the institutional rents of the
 *   arrangement, while victims (contemporary majorities and reform movements)
 *   bear the costs of thwarted democratic reform. The claim is snare; the
 *   metrics are authored independently to describe the constraint's actual
 *   operation as a high-extraction, actively enforced barrier to
 *   constitutional change.
 *
 * KEY AGENTS:
 *   - Entrenched elites (powerful/arbitrage): Collect institutional rents from status quo entrenchment.
 *   - Contemporary majorities (organized/constrained): Bear the structural extraction of blocked reform.
 *   - Constitutional arbiters (institutional/analytical): Administer and interpret the threshold, maintaining its enforceability.
 *   - Reform movements (moderate/constrained): Organize around amendment campaigns that die at the threshold.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.85).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto Lock-In").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '5c29886f-d177-4e02-8362-e7dd4888e5d6').
narrative_ontology:cs_kernel_codification('5c29886f-d177-4e02-8362-e7dd4888e5d6', formalized).
narrative_ontology:cs_authority_grounding('5c29886f-d177-4e02-8362-e7dd4888e5d6', lineage).
narrative_ontology:cs_interpretation_layer_present('5c29886f-d177-4e02-8362-e7dd4888e5d6').
narrative_ontology:cs_reading_relation('5c29886f-d177-4e02-8362-e7dd4888e5d6', supermajority_threshold__consensus_safeguard_reading, forecloses).
narrative_ontology:cs_reading_relation('5c29886f-d177-4e02-8362-e7dd4888e5d6', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('5c29886f-d177-4e02-8362-e7dd4888e5d6', foundational, supermajority_constitutes_minoritarian_veto).
narrative_ontology:cs_axiom_status(supermajority_constitutes_minoritarian_veto, holdable).
narrative_ontology:cs_axiom_grounding('5c29886f-d177-4e02-8362-e7dd4888e5d6', supermajority_constitutes_minoritarian_veto, empirically_contingent).
narrative_ontology:cs_axiom('5c29886f-d177-4e02-8362-e7dd4888e5d6', foundational, contemporary_majority_sovereignty_over_historical_settlement).
narrative_ontology:cs_axiom_status(contemporary_majority_sovereignty_over_historical_settlement, holdable).
narrative_ontology:cs_axiom_grounding('5c29886f-d177-4e02-8362-e7dd4888e5d6', contemporary_majority_sovereignty_over_historical_settlement, deontological).
narrative_ontology:cs_reference_frame('5c29886f-d177-4e02-8362-e7dd4888e5d6', historical_constitutional_settlement).
narrative_ontology:cs_drift_state('5c29886f-d177-4e02-8362-e7dd4888e5d6', contemporary_majoritarian_politics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5c29886f-d177-4e02-8362-e7dd4888e5d6', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold disproportionate influence under the existing constitutional order and benefit from the supermajority barrier that prevents redistributive or structural reforms that would dilute their power.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    powerful, generational, arbitrage, national).

% Receive favorable policy outcomes and institutional protections from the current constitutional settlement and rely on the threshold to obstruct amendments that would alter the status quo distribution of rights and resources.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    organized, biographical, mobile, national).

% Command electoral majorities in favor of constitutional reform but encounter the supermajority threshold as a binding ceiling that converts their popular mandate into legislative impasse.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, constrained, national).

% Organize and mobilize popular support for constitutional amendment; their energy and resources are absorbed by amendment campaigns that are halted at the threshold without producing structural change.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_movements, payer,
    moderate, biographical, constrained, national).

% Interpret and enforce the supermajority threshold in amendment processes; their rulings on procedural validity and vote counts determine whether amendments reach ratification. Their institutional role depends on the threshold remaining a binding constitutional requirement.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_arbiters, agenda_setter,
    institutional, generational, analytical, national).

% Will inherit the constitutional order locked in by the threshold but hold no present vote or voice in its amendment; they are structurally absent from the political calculus of blocking and ratification.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, excluded_future_generations, excluded,
    powerless, civilizational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None under this reading â the claimed coordination function of filtering transient majoritarian passion from durable constitutional consensus is not operative. The threshold instead captures reform energy and preserves existing power structures.
% TRANSFER_FUNCTION: Moves effective constitutional amendment power from contemporary electoral majorities to organized blocking minorities and entrenched elites, converting popular mandates into permanent status quo protection.
% ABSENT_VOICES: Future generations who will live under the entrenched constitutional order but were not party to its design; disenfranchised groups whose representation is diluted by the threshold; majoritarian reform movements whose proposals die at the threshold without reaching the agenda.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, majoritarian constitutional reform would become achievable, status quo protections would erode, and the existing distribution of institutional power would reorganize around majoritarian amendment procedures.
% FOUNDING_PROBLEM: The threshold was built to prevent hasty or faction-driven constitutional change and to ensure amendments enjoy broad, cross-cutting societal support.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the benefiting parties attest that the threshold's original design may have aimed at consensus, but contemporary comparative constitutional scholarship and democratic theory from outside the status quo beneficiaries increasingly view the threshold as producing minoritarian veto lock-in rather than deliberative stability; majoritarian reform movements attest the problem is dead, while entrenched elites assert it remains live.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85 at interval end) because the threshold systematically transfers amendment capacity from contemporary majorities to blocking minorities. Suppression is high (0.78) because the constraint persists through institutional enforcement and the active exclusion of majoritarian alternatives, not through voluntary coordination. Theater ratio rises over the interval (0.55) as the justification shifts from genuine consensus-seeking to ritualistic invocations of constitutional stability. Accessibility collapse is substantial (0.75) because once the threshold is constitutionalized, simple-majority amendment alternatives are legally foreclosed. Resistance is moderate-high (0.60) because reform movements consistently mobilize against the barrier but are structurally blocked.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (entrenched elites, status quo beneficiaries) experience the constraint as protective stability; the payer seats (contemporary majorities, reform movements) experience it as an impassable barrier to democratic self-correction. The constitutional arbiters sit near symmetric but with institutional identity-lock toward preservation. The engine computes this divergence from the structural data; the authored claim of snare reflects the structural extraction observed from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites and status quo beneficiaries are structural beneficiaries (low d, subsidized by the constraint's blocking function). Contemporary majorities and reform movements are structural targets (high d, extraction concentrated through disenfranchisement). Constitutional arbiters derive institutional authority from administering the threshold, placing them near the beneficiary end despite their neutral framing. The directionality derivation follows from beneficiary/victim declarations plus exit options: victims are constrained, beneficiaries are mobile or arbitrage-capable.
 *
 * MANDATROPHY ANALYSIS:
 *   The minoritarian veto reading prevents false coordination classification by stripping away the consensus-safeguard framing. If the threshold genuinely solved a coordination problem, its metrics would show lower extraction and lower suppression; instead, extraction accumulates over time as the status quo diverges from majoritarian preferences, and suppression intensifies to defend the threshold against reform pressure. This is mandatrophy resolved: the founding problem of preventing hasty change is dead, yet the arrangement persists and extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_threshold_naturalness,
    'Is the supermajority threshold a genuinely functional consensus mechanism in some contexts, or is it always a minoritarian veto in disguise?',
    'Comparative analysis across constitutional systems where thresholds are met versus blocked, measuring whether supermajority outcomes track broad social agreement or concentrated minority preference.',
    'If functional consensus is demonstrable in some cases, classification shifts toward tangled_rope or rope; if the veto dynamic is universal, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_threshold_naturalness, conceptual, 'Whether the threshold ever genuinely coordinates consensus or is inherently extractive.').

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the suppression experienced by reform movements primarily structural (the rule itself) or internalized (belief that constitutional stability is intrinsically valuable regardless of democratic cost)?',
    'Post-mobilization trajectory analysis â do movements persist or demobilize after encountering the threshold, and do they adopt the stability frame as their own?',
    'If internalized, effective suppression exceeds the structural measure because the target population carries the constraint''s legitimacy narrative after failed reform attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Structural versus internalized suppression mechanism for reform coalitions.').

omega_variable(
    status_quo_beneficiary_ambiguity,
    'Do status quo beneficiaries actively capture rents from the threshold, or do they merely avoid losses relative to a counterfactual majoritarian reform?',
    'Distributional analysis of policy outcomes under the threshold compared to simple-majority counterfactuals estimated through comparative case studies.',
    'If beneficiaries only avoid losses, extraction may be lower than measured; if they actively capture additional rents, snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(status_quo_beneficiary_ambiguity, empirical, 'Whether beneficiary gains are active capture or passive loss-avoidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supermaj_minoritarian_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(supermaj_minoritarian_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(supermaj_minoritarian_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(supermaj_minoritarian_tr_t60, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(supermaj_minoritarian_tr_t80, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(supermaj_minoritarian_tr_t100, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(supermaj_minoritarian_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(supermaj_minoritarian_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(supermaj_minoritarian_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(supermaj_minoritarian_be_t60, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(supermaj_minoritarian_be_t80, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(supermaj_minoritarian_be_t100, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(supermaj_minoritarian_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(supermaj_minoritarian_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(supermaj_minoritarian_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(supermaj_minoritarian_su_t60, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(supermaj_minoritarian_su_t80, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(supermaj_minoritarian_su_t100, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the supermajority_threshold kernel, decomposed per the Îµ-invariance principle because the kernel's evaluation changes structurally depending on whether the threshold is read as consensus safeguard, adaptive gradient, or minoritarian veto. Each reading carries distinct Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
