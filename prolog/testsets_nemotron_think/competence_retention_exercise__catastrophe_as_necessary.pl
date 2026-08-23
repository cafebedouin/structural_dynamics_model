% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe as Necessary Teacher for Organizational Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The constraint is the belief — institutionalized in high-hazard industry
 *   regulation, consulting practice, and academic theory — that only actual
 *   catastrophic events provide the visceral stakes required for genuine
 *   organizational competence retention. This reading of the
 *   competence_retention_exercise kernel asserts that simulation, near-miss
 *   analysis, and synthetic training are fundamentally insufficient because
 *   they lack 'real' consequences. The belief coordinates industry around
 *   catastrophe preparedness but extracts catastrophes as the tuition for
 *   learning. Over the 1984–2024 interval, simulation fidelity has increased
 *   dramatically while the catastrophe-necessary framing has hardened,
 *   suppressing investment in alternatives and making organizations more
 *   vulnerable during incident-free periods (the 'safety paradox').
 *
 * KEY AGENTS:
 *   - hro_theorists_consultants: Primary agenda_setter (institutional/analytical) — defines the learning paradigm
 *   - regulated_organizations: Primary payer (organized/constrained) — bears catastrophe costs and compliance burden
 *   - catastrophe_victims: Primary victim (powerless/trapped) — absorbs human cost of 'necessary' catastrophes
 *   - simulation_advocates: Excluded (organized/mobile) — holds evidence of competence without catastrophe
 *   - regulators_justifying_authority: Secondary agenda_setter (institutional/analytical) — uses catastrophe narrative to sustain mandate
 *   - independent_safety_researchers: Observer (analytical/analytical) — documents counterexamples
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.78).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.75).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.78).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, snare).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe as Necessary Teacher for Organizational Competence").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '4bcfc4f3-8d47-4db4-a5c0-c65fc998deef').
narrative_ontology:cs_kernel_codification('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', distributed).
narrative_ontology:cs_authority_grounding('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', practice).
narrative_ontology:cs_interpretation_layer_present('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef').
narrative_ontology:cs_reading_relation('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', foundational, catastrophe_necessary_for_genuine_competence).
narrative_ontology:cs_axiom_status(catastrophe_necessary_for_genuine_competence, holdable).
narrative_ontology:cs_axiom_grounding('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', catastrophe_necessary_for_genuine_competence, empirically_contingent).
narrative_ontology:cs_axiom('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', secondary, simulation_cannot_produce_visceral_stakes).
narrative_ontology:cs_axiom_status(simulation_cannot_produce_visceral_stakes, holdable).
narrative_ontology:cs_axiom_grounding('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', simulation_cannot_produce_visceral_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', catastrophe_as_necessary_framework).
narrative_ontology:cs_drift_state('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', contemporary_safety_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4bcfc4f3-8d47-4db4-a5c0-c65fc998deef', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, hro_theorists_consultants).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, regulators_justifying_authority).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, regulated_organizations).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, catastrophe_victims).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, taxpayers_public).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, visceral_stakes_required_for_deep_learning).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, simulation_inherently_incomplete).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and promote frameworks arguing that only catastrophic failure provides the visceral stakes necessary for genuine organizational learning. Author seminal texts, consult to high-hazard industries, and shape regulatory expectations. Their professional standing and revenue depend on this framing being accepted as natural law rather than contestable claim.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, hro_theorists_consultants, agenda_setter,
    institutional, generational, analytical, global).

% Operate in high-hazard domains (nuclear, aviation, chemical, healthcare) under regulatory regimes influenced by the catastrophe-necessary framing. They bear the cost of actual catastrophes (financial, reputational, human) and face pressure to underinvest in simulation/near-miss systems because those are framed as insufficient. Exit is constrained by licensing, capital lock-in, and regulatory capture.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, regulated_organizations, payer,
    organized, biographical, constrained, national).

% Workers, nearby communities, and passengers who suffer death, injury, or displacement when the 'necessary catastrophe' occurs. They have no voice in the organizational learning calculus and no exit from the risk imposed by the belief that catastrophes are pedagogically necessary.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, catastrophe_victims, payer,
    powerless, immediate, trapped, local).

% Bear the fiscal externalities of catastrophic events (emergency response, environmental remediation, litigation, insurance market disruption) and the opportunity cost of underinvestment in prevention. Their exit is constrained by citizenship and diffuse impact.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, taxpayers_public, payer,
    moderate, biographical, constrained, national).

% Researchers and practitioners developing high-fidelity simulation, synthetic environments, and near-miss analytics who argue these provide sufficient stakes for competence retention. They are structurally excluded from regulatory standard-setting and industry consensus bodies dominated by the catastrophe-necessary framing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_advocates, excluded,
    organized, biographical, mobile, global).

% Safety professionals who build organizational learning from minor incidents and near-misses, demonstrating competence maintenance without catastrophe. Their evidence is dismissed as 'not real stakes' by the dominant framing, excluding them from authoritative guideline committees.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, near_miss_practitioners, excluded,
    moderate, biographical, mobile, global).

% Academic researchers studying organizational learning across domains (nuclear navy, commercial aviation, healthcare) who observe that some organizations maintain high competence without catastrophic reset events. They analyze the constraint from outside the practitioner consensus.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, independent_safety_researchers, observer,
    analytical, generational, analytical, global).

% Regulatory agencies that invoke the catastrophe-necessary narrative to justify expansive oversight authority, post-accident investigation powers, and resistance to simulation-based regulatory acceptance. Catastrophes validate their mandate; simulation-based competence would undermine their institutional rationale.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, regulators_justifying_authority, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, hro_theorists_consultants).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational vigilance and resource allocation toward catastrophe preparedness by establishing a shared belief that only real failure generates genuine learning — aligning regulators, operators, and theorists around a common stakes framework.
% TRANSFER_FUNCTION: Transfers organizational resilience, safety margins, and human welfare into actual catastrophic events that serve as 'reset' mechanisms for competence; the cost of the catastrophe is the tuition for the learning.
% ABSENT_VOICES: Simulation advocates and near-miss learning practitioners who have demonstrated competence maintenance without catastrophic events (e.g., US Nuclear Navy, commercial aviation's safety record). They are excluded from the consensus bodies that define 'acceptable' learning methodologies.
% DISAPPEARANCE_RATIONALE: If the belief that catastrophes are necessary vanished overnight, regulated organizations would rapidly redirect investment from catastrophe acceptance to high-fidelity simulation, near-miss analytics, and synthetic training environments. Regulatory frameworks would shift from post-accident investigation to continuous competence verification. The catastrophe-consulting industry would lose its foundational premise.
% FOUNDING_PROBLEM: How to maintain genuine competence and organizational mindfulness in ultra-high-reliability systems where accidents are extremely rare and operators never experience real failure.
% FOUNDING_PROBLEM_CORROBORATION: Independent corroboration from US Nuclear Navy (maintains reactor safety competence without catastrophic accidents since 1961), commercial aviation (hull-loss rate declined 95% while simulation fidelity increased), and Weick/Sutcliffe HRO research showing mindfulness is sustained by preoccupation with failure, not failure itself. The catastrophe-necessary claim is contested by these empirically demonstrated counterexamples.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint demands actual catastrophic events — with their attendant deaths, environmental damage, and financial ruin — as the price of competence. Suppression (0.75) is high because the framing actively dismisses simulation and near-miss data as 'not real stakes,' suppressing massive investment in alternatives. Theater ratio (0.42) is moderate: the coordination function (shared vigilance framework) is real but increasingly performative as evidence accumulates that competence persists without catastrophe. Accessibility collapse (0.78) is high because the belief structure treats alternatives as categorically insufficient, not merely inferior. Resistance (0.45) is moderate: simulation advocates and near-miss practitioners exist but are excluded from authoritative standard-setting.
 *
 * PERSPECTIVAL GAP:
 *   From the hro_theorists_consultants and regulators_justifying_authority seats (agenda_setters, analytical exit), the constraint appears as a Mountain — a natural law of organizational learning. From the regulated_organizations seat (payer, constrained exit), it appears as a Snare — they pay the catastrophe tuition while being told alternatives don't work. From the catastrophe_victims seat (payer, trapped exit), it is pure extraction with no coordination benefit. The engine computes this divergence from the structural data; the authored claim (snare) reflects the structural reality from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda setters (theorists, regulators) are structural beneficiaries: they collect professional standing, consulting revenue, and institutional mandate from the catastrophe-necessary framing (d near 0.0). Regulated organizations are payers: they bear catastrophe costs and are constrained from adopting alternatives (d near 0.8). Catastrophe victims are trapped payers with no voice (d = 1.0). Simulation advocates and near-miss practitioners are excluded — their exit is mobile but their structural influence is suppressed. The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence in ultra-safe systems) was live in 1984 when simulation was primitive. By 2024, the problem is contested: nuclear navy and aviation demonstrate competence retention without catastrophe. The arrangement persists because the catastrophe-necessary framing benefits agenda setters (theorists, regulators) and because the cost of fixing (overhauling regulatory acceptance of simulation, retraining safety culture) is prohibitive for any single organization. This is mandatrophy: the mandate (catastrophe as teacher) has outlived its function (competence maintenance) but persists through institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the catastrophe-necessary claim a genuine structural law of organizational learning, or a constructed constraint that benefits identifiable agenda-setters?',
    'Longitudinal study of organizations that maintain competence without catastrophes (nuclear navy, commercial aviation) vs. those that adopt the catastrophe-necessary framing. Measure competence decay rates under different learning regimes.',
    'If genuine natural law, the constraint is a Mountain and extraction is the price of reality. If constructed, it is a Snare/Tangled Rope and the extraction is avoidable rent. This is the core false-summit question for this kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the catastrophe-necessary claim describes a natural law or a beneficiary-serving construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of simulation/near-miss alternatives structural (regulatory barriers, funding allocation) or internalized (practitioners genuinely believe simulation cannot work)?',
    'Survey safety professionals across domains on their belief in simulation sufficiency; correlate with regulatory environment and organizational investment patterns. Track belief change after exposure to counterexample organizations.',
    'If structural, suppression lifts with regulatory reform. If internalized, suppression persists after barrier removal — the constraint has colonized the practitioners'' epistemic framework, raising effective suppression above the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative learning modalities.').

omega_variable(
    coordination_extraction_boundary,
    'Does the catastrophe-necessary framing provide a genuine coordination function (shared vigilance framework) that is inseparable from its extraction, or is the coordination story pure cover?',
    'Compare organizational outcomes under catastrophe-necessary framing vs. simulation-sufficient framing on metrics: time-to-competence, error rates during incident-free periods, recovery speed from near-misses.',
    'If coordination function is genuine and inseparable, the constraint is a Tangled Rope (hybrid). If coordination is cover, it is a pure Snare. The current metrics lean Snare but the coordination claim is non-trivial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 1984, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1984, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1984, 0.15).
narrative_ontology:measurement(comp_tr_t1994, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1994, 0.22).
narrative_ontology:measurement(comp_tr_t2004, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(comp_tr_t2014, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2014, 0.37).
narrative_ontology:measurement(comp_tr_t2024, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t1984, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1984, 0.45).
narrative_ontology:measurement(comp_be_t1994, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement(comp_be_t2004, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2004, 0.61).
narrative_ontology:measurement(comp_be_t2014, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(comp_be_t2024, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1984, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1984, 0.35).
narrative_ontology:measurement(comp_su_t1994, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1994, 0.45).
narrative_ontology:measurement(comp_su_t2004, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(comp_su_t2014, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(comp_su_t2024, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.08).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_retention_exercise kernel. The sibling readings (simulation_as_sufficient, near_miss_as_bridge) instantiate different constraints with different ε values, beneficiary/victim structures, and classifications. The kernel label 'competence retention exercise' conflates these structurally distinct claims. This file and its siblings form a constraint family linked by affects_constraints. The ε-invariance principle requires separate stories because the extraction profile differs radically: catastrophe_as_necessary demands real catastrophes (high ε), while simulation_as_sufficient claims near-zero extraction cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
