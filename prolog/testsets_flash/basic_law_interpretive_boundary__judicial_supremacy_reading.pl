% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Basic Laws (Israel)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of the Basic
 *   Laws' interpretive boundary in Israel, where the Supreme Court asserts
 *   the authority to interpret and enforce the Basic Laws as a higher-order
 *   legal framework, including the power to invalidate contradictory
 *   legislation passed by the Knesset. This reading positions the Supreme
 *   Court as the ultimate arbiter of constitutional legality, binding the
 *   legislative branch to its interpretations. It is a contested reading,
 *   with significant political and legal resistance from those advocating for
 *   parliamentary sovereignty.
 *
 * KEY AGENTS:
 *   - supreme_court: Agenda setter (institutional/arbitrage) — interprets and enforces Basic Laws, invalidates legislation.
 *   - knesset: Payer (institutional/constrained) — legislative body whose laws are subject to judicial review.
 *   - rights_claimants: Beneficiary (moderate/mobile) — individuals or groups who can petition the court to protect their rights under the Basic Laws.
 *   - legislative_majority: Payer (organized/constrained) — the political coalition whose legislative agenda can be blocked by judicial review.
 *   - legal_scholars: Observer (analytical/analytical) — analyze the constitutional framework and the balance of power between branches.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy over Basic Laws (Israel)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '58a282d1-8867-431a-9b43-e2adce41d07a').
narrative_ontology:cs_kernel_codification('58a282d1-8867-431a-9b43-e2adce41d07a', formalized).
narrative_ontology:cs_authority_grounding('58a282d1-8867-431a-9b43-e2adce41d07a', lineage).
narrative_ontology:cs_interpretation_layer_present('58a282d1-8867-431a-9b43-e2adce41d07a').
narrative_ontology:cs_reading_relation('58a282d1-8867-431a-9b43-e2adce41d07a', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('58a282d1-8867-431a-9b43-e2adce41d07a', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('58a282d1-8867-431a-9b43-e2adce41d07a', foundational, basic_laws_are_supreme_law).
narrative_ontology:cs_axiom_status(basic_laws_are_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('58a282d1-8867-431a-9b43-e2adce41d07a', basic_laws_are_supreme_law, conventional).
narrative_ontology:cs_axiom('58a282d1-8867-431a-9b43-e2adce41d07a', foundational, judicial_review_is_inherent_to_constitutionalism).
narrative_ontology:cs_axiom_status(judicial_review_is_inherent_to_constitutionalism, holdable).
narrative_ontology:cs_axiom_grounding('58a282d1-8867-431a-9b43-e2adce41d07a', judicial_review_is_inherent_to_constitutionalism, deontological).
narrative_ontology:cs_reference_frame('58a282d1-8867-431a-9b43-e2adce41d07a', constitutional_supremacy_with_judicial_review).
narrative_ontology:cs_drift_state('58a282d1-8867-431a-9b43-e2adce41d07a', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('58a282d1-8867-431a-9b43-e2adce41d07a', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, legislative_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order legal framework and asserts the power to invalidate legislation that contradicts them. This position grants the Court significant institutional power and influence over the legislative process.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% The legislative body whose power to enact laws is constrained by the Supreme Court's interpretive authority. Its legislation can be nullified, forcing it to reconsider or abandon policy initiatives.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset, payer,
    institutional, biographical, constrained, national).

% Individuals and groups who can petition the Supreme Court to protect their rights, as enshrined in the Basic Laws, against legislative or executive action. They gain a powerful avenue for redress and a check on majoritarian power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, biographical, mobile, national).

% The political coalition that forms the government and seeks to implement its agenda through legislation. Its ability to pass laws is directly impacted by the threat or reality of judicial invalidation, leading to frustration and political resistance.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legislative_majority, payer,
    organized, immediate, constrained, national).

% Academics and legal experts who analyze the constitutional framework, the Supreme Court's jurisprudence, and the balance of power between branches. They contribute to the discourse but do not directly participate in the enforcement or payment of the constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of legal norms, ensuring that ordinary legislation conforms to fundamental constitutional principles embodied in the Basic Laws, thereby providing legal certainty and protecting individual rights.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the Basic Laws from the Knesset (as a simple legislative body) to the Supreme Court, and transfers legislative power from the Knesset to the Court in cases of invalidation.
% ABSENT_VOICES: Advocates for pure parliamentary sovereignty, who believe the elected legislature should have the final say on all laws, are structurally marginalized in this reading. They would argue that judicial invalidation is an undemocratic overreach.
% DISAPPEARANCE_RATIONALE: If the Supreme Court's interpretive supremacy vanished, the Knesset would immediately regain full legislative authority, potentially passing laws that contradict previously established constitutional principles. Rights protections would become entirely dependent on legislative majorities, and the legal system would lose its higher-order normative anchor, leading to significant legal and political instability.
% FOUNDING_PROBLEM: The absence of a formal, entrenched constitution in Israel created ambiguity regarding the hierarchy of laws and the protection of fundamental rights, leading to a need for a mechanism to ensure legislative adherence to core principles.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil society organizations, alongside the Supreme Court itself, attest that the problem of ensuring constitutional adherence and rights protection remains live, especially given the lack of a fully codified constitution. While the Knesset disputes the Court's specific interpretation, the underlying need for a higher legal framework is widely acknowledged by independent legal experts.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the Supreme Court's power to invalidate legislation directly extracts legislative authority from the Knesset. Suppression (0.70) is also high, as the Court actively enforces its interpretive authority, suppressing legislative alternatives that contradict its reading of the Basic Laws. The theater ratio is low (0.10) because the Court's actions are genuinely functional in asserting its power, not merely performative. The increasing extractiveness and suppression over the interval reflect the gradual strengthening of judicial review in Israel.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's perspective, this is a necessary 'rope' for upholding constitutional principles and protecting individual rights. From the Knesset's perspective, particularly the legislative majority, it is a 'snare' that usurps democratic legislative power. Rights claimants experience it as a 'rope' providing a vital check on legislative overreach. The engine's per-seat classification will reflect these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court and rights claimants are beneficiaries (d near 0.0) as they gain power and protection from this constraint. The Knesset and the legislative majority are targets (d near 1.0) as their legislative power is directly curtailed. The active enforcement by the Supreme Court ensures the constraint's persistence despite political resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is far from mandatrophy. Its mandate, the interpretation and enforcement of Basic Laws, is actively contested and exercised. The classification as a Tangled Rope reflects the genuine coordination function (establishing a higher legal order) intertwined with significant asymmetric extraction (from the legislature by the judiciary). The ongoing resistance from the Knesset prevents it from being a settled Mountain, and the clear beneficiaries and victims prevent it from being a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine expression of judicial supremacy, or a temporary phase in a broader contest over the Basic Laws'' interpretive boundary?',
    'Future legislative action (e.g., an override clause or a Basic Law explicitly limiting judicial review) or a shift in judicial doctrine.',
    'If a temporary phase, the constraint''s stability and long-term extractiveness are lower than currently measured; if a settled supremacy, its institutional power is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''judicial_supremacy_reading'' of the ''basic_law_interpretive_boundary'' kernel. Sibling readings (''parliamentary_sovereignty_reading'', ''balanced_contestation_reading'') would alter the distribution of interpretive authority and the effective power of the Supreme Court.').

omega_variable(
    legitimacy_of_judicial_invalidation,
    'Is the Supreme Court''s power to invalidate legislation derived from a clear constitutional mandate, or is it an assertion of power that lacks explicit textual grounding?',
    'Historical analysis of constitutional intent, comparative legal scholarship on unwritten constitutionalism, and public acceptance/rejection of judicial rulings.',
    'If explicitly mandated, the constraint''s legitimacy is higher, reducing resistance; if an asserted power, its legitimacy is contested, increasing resistance and the need for active enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_invalidation, empirical, 'The extent to which judicial invalidation is textually grounded versus judicially asserted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_boundary' kernel. Each reading represents a distinct structural claim about the locus of constitutional authority in Israel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
