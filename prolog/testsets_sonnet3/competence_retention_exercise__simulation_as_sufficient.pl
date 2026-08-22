% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient Reading of Competence-Retention Practice
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint is the simulation-as-sufficient reading of a contested
 *   kernel about how catastrophe-avoidance competence is maintained in
 *   high-reliability organizations (nuclear, aviation, acute medicine). This
 *   reading holds that high-fidelity simulation is not merely rehearsal for
 *   the real thing but a structurally equivalent exercise of the underlying
 *   cognitive and procedural competence — the demands of correctly diagnosing
 *   and responding under simulated time pressure, incomplete information, and
 *   cascading failure are held to be functionally identical to the demands of
 *   a real event. The reading is authored as tangled_rope because it does
 *   possess a genuine coordination function (nobody wants to train competence
 *   by causing real disasters) that is bundled with an asymmetric extraction:
 *   the entities that control simulator design, certification, and pass
 *   thresholds (vendors, training directorates, regulators, leadership)
 *   capture legitimacy and liability protection, while frontline operators
 *   and the exposed public bear the tail risk if the equivalence claim
 *   understates the difference between simulated and real catastrophic
 *   dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.42).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient Reading of Competence-Retention Practice").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '69a5f833-92e3-467f-8acd-55872a09db8c').
narrative_ontology:cs_kernel_codification('69a5f833-92e3-467f-8acd-55872a09db8c', formalized).
narrative_ontology:cs_authority_grounding('69a5f833-92e3-467f-8acd-55872a09db8c', expertise).
narrative_ontology:cs_interpretation_layer_present('69a5f833-92e3-467f-8acd-55872a09db8c').
narrative_ontology:cs_reading_relation('69a5f833-92e3-467f-8acd-55872a09db8c', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('69a5f833-92e3-467f-8acd-55872a09db8c', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('69a5f833-92e3-467f-8acd-55872a09db8c', foundational, cognitive_procedural_equivalence_thesis).
narrative_ontology:cs_axiom_status(cognitive_procedural_equivalence_thesis, holdable).
narrative_ontology:cs_axiom_grounding('69a5f833-92e3-467f-8acd-55872a09db8c', cognitive_procedural_equivalence_thesis, empirically_contingent).
narrative_ontology:cs_axiom('69a5f833-92e3-467f-8acd-55872a09db8c', secondary, simulator_metrics_are_valid_competence_proxies).
narrative_ontology:cs_axiom_status(simulator_metrics_are_valid_competence_proxies, holdable).
narrative_ontology:cs_axiom_grounding('69a5f833-92e3-467f-8acd-55872a09db8c', simulator_metrics_are_valid_competence_proxies, instrumental).
narrative_ontology:cs_reference_frame('69a5f833-92e3-467f-8acd-55872a09db8c', procedural_competence_as_transferable_skill).
narrative_ontology:cs_drift_state('69a5f833-92e3-467f-8acd-55872a09db8c', post_simulator_scandal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('69a5f833-92e3-467f-8acd-55872a09db8c', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_directorates).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_certifiers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, operations_leadership).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, cognitive_fidelity_equivalence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the simulator-based recertification regime, sets the passing metrics, and controls what counts as demonstrated competence. Benefits organizationally from a stable, auditable, repeatable credentialing pipeline that does not require anyone to be exposed to real disaster.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_directorates, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell and continuously upgrade the high-fidelity training platforms that the regime depends on. Their revenue model is directly tied to the claim that simulation is structurally equivalent to the real event; they fund research supporting fidelity-equivalence claims.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Accept simulator performance as the legal proxy for competence in licensing decisions. Benefit from a defensible, low-cost, low-liability certification process that does not require waiting for or engineering near-catastrophic conditions.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_certifiers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_certifiers, agenda_setter).

% Rely on simulator pass rates to demonstrate organizational safety compliance to boards, insurers, and regulators. Insulated from the operational reality that simulator scenarios are curated and rarely capture the full combinatorial surprise of true system failure.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operations_leadership, beneficiary,
    institutional, generational, arbitrage, national).

% Must requalify on simulators on a fixed schedule; their careers depend on passing scored scenarios designed by others. They bear the gap if the simulator's fidelity assumption is wrong — when a real event exceeds the trained envelope, they are the ones physically present and blamed for 'failure to apply training.'
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Live or work near the facilities/systems this competence regime is meant to protect. They have no visibility into whether simulator fidelity actually tracks real catastrophic dynamics; they absorb the tail risk if the equivalence claim is false and an under-prepared operator faces a scenario the simulator never modeled.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk, payer,
    powerless, generational, trapped, regional).

% Publish findings questioning whether simulator scenarios capture the full stress physiology, organizational chaos, and multi-system cascading failure of real catastrophes. Their critiques rarely reach certification-standard revision processes controlled by vendors and regulators.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_researchers, excluded,
    moderate, generational, constrained, national).

% Evaluates whether the fidelity-equivalence claim holds structurally across domains (aviation, nuclear, medicine) or is a locally convenient institutional fiction that varies with how much genuine transfer-of-competence evidence exists.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulator_vendors).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, scalable, humane way to maintain and verify catastrophe-avoidance competence without deliberately exposing people to real disasters — solving the genuine problem that real catastrophic events are (rightly) rare, unrepeatable, and too costly to use as the primary training mechanism.
% TRANSFER_FUNCTION: Moves certification legitimacy and liability protection toward training directorates, vendors, and regulators who control simulator design and passing criteria, while moving the residual risk of untested fidelity gaps onto frontline operators (blamed individually when reality diverges from the trained envelope) and the public exposed to the systems they operate.
% ABSENT_VOICES: Safety researchers who study divergence between simulator scenarios and actual catastrophic dynamics are structurally outside the certification-standard-setting process; frontline operators who experience the gap firsthand have no channel to challenge simulator scenario design without appearing to indict their own competence.
% DISAPPEARANCE_RATIONALE: If the simulation-as-sufficient standard were abandoned, certification regimes would need an alternative competence-verification mechanism (real-incident apprenticeship, near-miss review boards, or extended supervised practice), vendor revenue built on fidelity-equivalence claims would collapse, and regulators would face immediate pressure to redesign licensing frameworks — a substantial institutional rearrangement.
% FOUNDING_PROBLEM: Real catastrophic events (reactor accidents, aviation disasters, mass-casualty medical failures) are too rare, too costly in human terms, and too irreversible to serve as the primary vehicle for building and verifying operator competence — organizations needed a way to train and certify people without waiting for or causing disasters.
% FOUNDING_PROBLEM_CORROBORATION: Simulator vendors and training directorates attest the founding problem remains fully live and that simulation fully substitutes for it. Independent safety researchers and post-incident investigation boards (e.g., findings from aviation and nuclear incident reviews) attest that simulator scenarios are curated to known failure modes and have repeatedly failed to anticipate the actual cascading dynamics of real catastrophic events — corroboration from outside the certifying and vendor seats is mixed rather than confirmatory.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).
:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate rather than severe because the coordination function is real and substantial — simulator training genuinely builds procedural competence that transfers to real events in many measured domains (aviation checklists, ACLS protocols). But it is nonzero and rising because the certification apparatus increasingly measures simulator performance itself as the terminal metric (Goodhart drift, reflected in the rising theater_ratio from 0.20 to 0.40) rather than treating simulator performance as one imperfect proxy among several for real-world catastrophe-avoidance capability. Suppression (0.38) reflects that alternative competence-verification channels (structured near-miss review, extended supervised real-world apprenticeship) are institutionally marginalized in favor of the scalable, liability-clean simulator pathway, though this suppression is moderate rather than severe because near-miss review boards do exist in most of these domains, just subordinated to simulator-based certification as the binding legal standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Training directorates, simulator vendors, and regulatory certifiers sit near the beneficiary end: they set the terms of what counts as demonstrated competence, capture the revenue and liability-protection value of the regime, and face minimal personal exposure if the fidelity-equivalence claim proves wrong in a specific real event. Frontline operators sit closer to the target end: they are individually accountable for outcomes that depend on an equivalence assumption they did not design and cannot contest without appearing to indict their own training record. The downstream public sits at the extreme target end with no institutional voice at all — trapped exit, powerless, bearing the tail-risk consequence of any systematic gap between simulator scenarios and true catastrophic dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifying and maintaining competence without requiring real catastrophes — remains partially live (real disasters remain genuinely too costly and irreversible to use as primary training vehicles), which is why this is authored as tangled_rope rather than snare: there is a real coordination function underneath the extraction. Mandatrophy risk arises specifically where simulator performance metrics have drifted from being evidence of competence to being the definition of competence — a classic proxy-substitution pattern the rising theater_ratio is intended to flag for downstream detection, without asserting that the entire regime is illegitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_fidelity_equivalence_ambiguity,
    'Does high-fidelity simulation actually replicate the full cognitive and physiological demand profile of a genuine catastrophe (acute stress physiology, irreversible stakes, organizational chaos, novel combinatorial failure modes), or does it only replicate the procedurally scriptable subset of that demand profile?',
    'Comparative studies of operator performance and physiological stress markers in simulated versus real catastrophic events (where such paired data exists, e.g. post-incident debriefs compared against prior simulator scores for the same individuals) would establish how much of the demand profile transfers.',
    'If fidelity is genuinely equivalent, this reading is closer to a rope (real coordination function, minimal extraction); if fidelity is systematically overclaimed for scenarios outside the trained envelope, the extraction component grows and the constraint drifts toward snare for the operators blamed when reality exceeds the simulator''s scripted range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_fidelity_equivalence_ambiguity, empirical, 'Whether simulator fidelity genuinely captures the full demand profile of real catastrophic events or only its scriptable subset.').

omega_variable(
    committer_kernel_disagreement_location,
    'This story is one reading (simulation_as_sufficient) of a three-way contested kernel (competence_retention_exercise) whose siblings are catastrophe_as_necessary and near_miss_as_bridge. Where precisely is the disagreement located structurally: is it about whether cognitive equivalence exists, whether stakes/irreversibility matter independently of cognition, or whether real-world calibration data is a necessary supplement regardless of simulator fidelity?',
    'Decompose the disagreement into its component claims and test each independently: (1) cognitive-procedural equivalence (this reading''s core claim), (2) stakes/visceral-consequence necessity (catastrophe_as_necessary''s core claim), (3) sufficiency of partial real-world feedback for calibration (near_miss_as_bridge''s core claim). These may not be mutually exclusive in practice — an organization could hold all three as complementary inputs rather than competing sufficiency claims.',
    'If the disagreement is really about (2) stakes/irreversibility rather than (1) cognitive equivalence, this reading''s core premise may be talking past its sibling rather than contradicting it, which would argue for ''influences'' or ''coexists_with'' relations rather than any foreclosure between siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locating precisely which component of the three-way kernel disagreement this reading actually addresses versus its siblings.').

omega_variable(
    regulatory_capture_of_equivalence_standard,
    'Is the simulator-fidelity-equivalence standard set by genuinely independent technical assessment, or is it shaped by the same vendors and training directorates who benefit financially and institutionally from the standard being satisfied by their existing products?',
    'Audit trail of who sits on the standard-setting bodies that define ''high-fidelity'' thresholds for certification purposes, and whether vendor representatives or vendor-funded research disproportionately shape those thresholds relative to independent safety science input.',
    'Significant vendor/regulator overlap in standard-setting would strengthen the tangled_rope reading (coordination function real, but captured) and could push the classification toward snare if the coordination function is shown to be largely pretextual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_of_equivalence_standard, empirical, 'Whether the fidelity standard is set independently or by parties who benefit from it being satisfied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t4, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 4, 0.24).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.28).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 12, 0.31).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.34).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.37).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t4, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t4, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t12, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'competence retention' claim per the epsilon-invariance principle. simulation_as_sufficient (this file) authors ε=0.42 with a tangled_rope claim reflecting genuine coordination bundled with captured certification legitimacy. catastrophe_as_necessary would author a different ε reflecting the cost of requiring real catastrophic exposure as the learning mechanism. near_miss_as_bridge would author a third ε reflecting a middle position where partial real-world feedback validates but does not replace simulation. Each is a structurally distinct claim about what suffices for competence maintenance, not a different measurement of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
