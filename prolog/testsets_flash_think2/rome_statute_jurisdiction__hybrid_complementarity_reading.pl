% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute's Hybrid Complementarity Jurisdiction
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'hybrid complementarity' reading
 *   of the Rome Statute's jurisdiction. This reading emphasizes the delicate
 *   balance struck by the complementarity principle: the International
 *   Criminal Court (ICC) possesses residual universal jurisdiction over
 *   atrocity crimes, but it defers primarily to national jurisdictions. The
 *   ICC acts only when states are genuinely unwilling or unable to prosecute.
 *   This creates a hybrid authority structure, asserting international
 *   justice while respecting state sovereignty, but often leading to tension
 *   and challenges in enforcement. The claimed type is 'tangled_rope' because
 *   it genuinely coordinates international justice efforts but does so
 *   through a mechanism that extracts sovereign authority from states,
 *   requiring active enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.55).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute's Hybrid Complementarity Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3').
narrative_ontology:cs_kernel_codification('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', formalized).
narrative_ontology:cs_authority_grounding('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', lineage).
narrative_ontology:cs_interpretation_layer_present('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3').
narrative_ontology:cs_reading_relation('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', foundational, international_crimes_transcend_borders).
narrative_ontology:cs_axiom_status(international_crimes_transcend_borders, holdable).
narrative_ontology:cs_axiom_grounding('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', international_crimes_transcend_borders, deontological).
narrative_ontology:cs_axiom('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', foundational, states_retain_primary_jurisdiction).
narrative_ontology:cs_axiom_status(states_retain_primary_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', states_retain_primary_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', balanced_sovereignty_justice).
narrative_ontology:cs_drift_state('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', contemporary_icc_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a012ddc-9d1b-4cca-9e91-0c3bd3fa1ff3', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutor_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocities).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocates).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_cooperating_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates investigations and prosecutions for international crimes, asserting the ICC's jurisdiction while seeking cooperation from states. It navigates the tension between universal justice and state sovereignty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutor_office, agenda_setter,
    institutional, generational, constrained, global).

% Seek justice and accountability for atrocity crimes when national systems are unwilling or unable to provide it. The ICC offers a potential, albeit often slow and difficult, path to redress.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocities, beneficiary,
    powerless, generational, trapped, global).

% Promote the mandate of the ICC and advocate for stronger international criminal justice. They benefit from the existence of the ICC as a mechanism for accountability.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% Have ratified the Rome Statute, ceding some sovereign autonomy to the ICC while retaining primary jurisdiction under the complementarity principle. They are expected to cooperate with ICC investigations and warrants.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute, payer,
    powerful, generational, constrained, national).

% Are either not parties to the Rome Statute or refuse to cooperate with the ICC, resisting its assertion of jurisdiction over their nationals or territory. They face potential warrants and political pressure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_cooperating_states, payer,
    institutional, generational, constrained, national).

% Face prosecution by the ICC for international crimes when national systems fail. They bear the direct costs of the ICC's jurisdiction, including potential arrest and trial.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals, payer,
    powerless, biographical, trapped, global).

% Argue for strict state consent as the sole basis for international jurisdiction, viewing any ICC action beyond this as illegitimate overreach. Their arguments are debated but do not directly shape ICC operations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_legal_scholars, excluded,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international criminal justice that coordinates efforts to prosecute atrocity crimes (genocide, war crimes, crimes against humanity) by deferring to national jurisdictions first, then acting as a court of last resort.
% TRANSFER_FUNCTION: Transfers the ultimate responsibility for prosecuting international crimes from unwilling or unable national courts to the ICC, and transfers the burden of enforcement and accountability to the international community, while extracting some sovereign autonomy from states.
% ABSENT_VOICES: States that refuse to ratify the Rome Statute or cooperate with the ICC, and legal scholars who advocate for absolute state sovereignty, are structurally excluded from the ICC's operational decision-making, though their positions are debated in international legal discourse.
% DISAPPEARANCE_RATIONALE: If the ICC and the complementarity mechanism vanished, there would be a significant vacuum in international criminal justice, leading to increased impunity for atrocity crimes, a return to purely national (and often politicized) justice, and a loss of a crucial deterrent against such crimes.
% FOUNDING_PROBLEM: The widespread impunity for atrocity crimes due to the failure of national justice systems to prosecute them, leading to a lack of accountability for the most serious international offenses.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN reports, and independent legal experts consistently document ongoing impunity for atrocity crimes and the continued need for an international court, corroborating the problem's persistence from outside the ICC's direct beneficiaries.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'tangled_rope' classification reflects the dual nature of the complementarity mechanism: it coordinates international efforts to end impunity (benefiting victims and advocates) but does so by asserting a form of jurisdiction that extracts sovereignty from states (victims are non-cooperating states and accused individuals). Active enforcement is required to overcome state resistance and ensure cooperation. Extractiveness is moderate-high (0.65) due to the assertion of international authority over sovereign states, even if deferred. Suppression is moderate (0.55) as the ICC relies on state cooperation but can issue warrants and conduct investigations independently. Theater ratio is low (0.20) as the ICC's actions are generally aimed at real justice outcomes, despite operational constraints.
 *
 * PERSPECTIVAL GAP:
 *   State parties often view the complementarity principle as a crucial safeguard of their sovereignty, ensuring national systems retain primary jurisdiction. In contrast, victims and some advocates may see it as a necessary but sometimes frustrating deferral of justice, slowing accountability. The ICC itself navigates this tension, asserting its mandate while seeking cooperation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC Prosecutor's Office, victims of atrocities, and international justice advocates are structural beneficiaries, as the constraint provides a mechanism for justice where national systems fail. State parties and non-cooperating states are payers, as they cede or resist aspects of their sovereignty. Accused individuals are direct targets of the ICC's jurisdiction. The balance of deference and assertion means that even state parties experience some extraction of their exclusive sovereign authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ending impunity for atrocity crimes) remains live and highly relevant. The complementarity mechanism prevents it from becoming a pure snare by ensuring national systems retain primary jurisdiction, but its enforcement against unwilling states can feel highly extractive. This hybrid reading prevents mislabeling it as a pure rope (ignoring sovereign costs) or a pure snare (ignoring the coordination function and deference to national systems).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_effectiveness,
    'Is the complementarity principle genuinely encouraging national prosecutions, or is it primarily a loophole for states to avoid ICC jurisdiction?',
    'Empirical studies tracking national prosecution rates for atrocity crimes in states under ICC scrutiny, compared to states not under scrutiny, and analysis of ICC deferral decisions.',
    'If primarily a loophole, the constraint''s effective extractiveness from victims (due to delayed/denied justice) is higher, and its coordination function is weaker. If genuinely effective, its rope-like qualities are stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_effectiveness, empirical, 'Whether complementarity achieves its stated goal or enables impunity.').

omega_variable(
    sovereignty_vs_justice_balance,
    'Is the current balance between state sovereignty and international justice, as embodied by complementarity, optimal for achieving accountability?',
    'This is a conceptual/preference question, resolvable through normative debate and policy choices by states and international bodies, rather than empirical data alone.',
    'A shift in the preferred balance would lead to re-interpretation of the Statute, potentially increasing the ICC''s direct jurisdiction (more extractive for states) or further deferring to states (less effective for victims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_justice_balance, conceptual, 'Normative question about the ideal balance between state sovereignty and international justice.').

omega_variable(
    enforcement_capacity_gap,
    'Can the ICC effectively enforce its jurisdiction against powerful non-cooperating states without a standing enforcement mechanism?',
    'Analysis of historical ICC enforcement challenges, success rates of arrest warrants against non-cooperating states, and the impact of political pressure vs. direct enforcement capacity.',
    'If enforcement capacity is critically lacking, the constraint''s effective suppression is lower, and its theater_ratio might be higher for cases involving powerful states, indicating a gap between asserted and actual authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'The gap between ICC''s asserted jurisdiction and its practical enforcement power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(rome_tr_t2003, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2003, 0.19).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(rome_tr_t2013, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(rome_tr_t2023, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(rome_be_t2003, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement(rome_be_t2013, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(rome_be_t2023, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.45).
narrative_ontology:measurement(rome_su_t2003, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2003, 0.48).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.51).
narrative_ontology:measurement(rome_su_t2013, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2013, 0.53).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement(rome_su_t2023, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_norms).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, state_sovereignty_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
