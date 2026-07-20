% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Weapons
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the categorical_prohibition_reading of the
 *   ihl_distinction_proportionality kernel. It holds that Martens Clause
 *   principles of humanity and public conscience prohibit autonomous weapons
 *   per se, regardless of technical performance, because machine-decided
 *   killing violates human dignity in itself. The reading is contested within
 *   international humanitarian law by the human_agency_reading (which
 *   requires irreducible human judgment at the lethal moment) and the
 *   outcomes_based_reading (which evaluates LAWS solely on compliance with
 *   distinction and proportionality). The constraint extracts heavily from
 *   advanced military states while benefiting non-advanced states and
 *   anti-militarist civil society, and it requires active diplomatic and
 *   normative enforcement to persist against technological and strategic
 *   resistance.
 *
 * KEY AGENTS:
 *   - ICRC and humanitarian organizations: Agenda-setter (institutional/global) â promotes the categorical reading through legal interpretation and diplomacy.
 *   - Anti-militarist civil society: Beneficiary (organized/global) â gains legitimacy and policy influence from a bright-line ban.
 *   - Non-advanced states: Beneficiary (moderate/global) â gain relative strategic security by prohibiting adversaries from fielding LAWS.
 *   - Advanced military states: Payer (powerful/global) â bear the cost of forgoing military-technological advantage and contest the norm.
 *   - Defense industry sector: Excluded (organized/global) â structurally barred from the diplomatic forums where the norm is set.
 *   - International legal scholars: Observer (analytical/global) â assess the coherence of the reading with existing IHL frameworks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.82).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.75).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60').
narrative_ontology:cs_kernel_codification('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', fixed_text).
narrative_ontology:cs_authority_grounding('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', lineage).
narrative_ontology:cs_interpretation_layer_present('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60').
narrative_ontology:cs_reading_relation('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', foundational, autonomous_lethality_violates_dignity_per_se).
narrative_ontology:cs_axiom_status(autonomous_lethality_violates_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', autonomous_lethality_violates_dignity_per_se, deontological).
narrative_ontology:cs_axiom('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', foundational, martens_clause_categorical_prohibition).
narrative_ontology:cs_axiom_status(martens_clause_categorical_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', martens_clause_categorical_prohibition, conventional).
narrative_ontology:cs_reference_frame('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', martens_clause_human_dignity_framework).
narrative_ontology:cs_drift_state('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', contemporary_laws_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e3ec6dd-7ad6-4ff7-a05c-dbc070dd6e60', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, non_advanced_states).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_states).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_per_se_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promotes the categorical prohibition reading of the Martens Clause through legal opinions, diplomatic convenings, and advocacy; frames any machine-decided killing as a per se violation of principles of humanity. Cannot exit IHL framework without abandoning organizational mandate.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, icrc_and_humanitarian_organizations, agenda_setter,
    institutional, civilizational, constrained, global).

% Advocacy coalitions and peace organizations whose legitimacy, funding, and public support are strengthened by a bright-line ban on autonomous weapons; they benefit from the categorical framing because it closes technical loopholes that outcomes-based assessments would leave open.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, constrained, global).

% States lacking autonomous weapons capability or the industrial base to develop them; they gain relative strategic security when adversaries are legally prohibited from fielding LAWS, and they advance this interest through diplomatic support for the categorical ban in the CCW and UNGA.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, non_advanced_states, beneficiary,
    moderate, generational, constrained, global).

% States with advanced autonomous systems research and development programs; they bear the cost of forgoing a potential military-technological advantage and of contesting the norm in diplomatic forums where they are framed as outliers against humanitarian consensus.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_states, payer,
    powerful, civilizational, constrained, global).

% Developers and manufacturers of autonomous military systems who are structurally excluded from the humanitarian diplomatic forums where the categorical prohibition is negotiated; their operational and technical perspectives are treated as illegitimate in the norm-setting space, yet their market exists only if states resist the ban.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_industry_sector, excluded,
    organized, biographical, trapped, global).

% Analyze the competing readings of the Martens Clause and assess whether the categorical prohibition is coherent with existing distinction and proportionality frameworks; they observe the structural contest without being party to the military or diplomatic costs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line humanitarian boundary that prevents machine-decided killing in armed conflict, stabilizing shared expectations around the necessity of human moral agency in lethal decisions and preventing a race toward fully autonomous force application.
% TRANSFER_FUNCTION: Transfers strategic military advantage away from states capable of fielding autonomous weapons and toward states and civil society actors who benefit from constraining technological escalation in warfare; also transfers diplomatic legitimacy to the humanitarian institutions that successfully promulgate the norm.
% ABSENT_VOICES: Defense industry developers and military operators who design and would deploy autonomous systems are largely excluded from the humanitarian diplomatic forums where the categorical prohibition is promoted; their operational perspectives are treated as illegitimate in the norm-setting space.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished, advanced states would accelerate LAWS deployment, the current diplomatic equilibrium around the Martens Clause would collapse, and the humanitarian governance architecture would shift toward outcomes-based or human-agency regulatory frameworks; non-advanced states would face renewed strategic vulnerability.
% FOUNDING_PROBLEM: The erosion of human control over life-and-death decisions in armed conflict and the risk that autonomous weapons would circumvent the protective purpose of IHL by removing moral deliberation from lethal force application.
% FOUNDING_PROBLEM_CORROBORATION: Anti-militarist civil society and the ICRC attest the problem is live. Advanced military states and defense analysts attest the problem is manageable through technical compliance with distinction and proportionality; independent legal scholars note the founding concern is genuine but dispute whether categorical prohibition is the necessary solution.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82) because the categorical prohibition bans an entire technology class and imposes strategic costs on advanced states regardless of whether autonomous systems could satisfy distinction and proportionality. Suppression is substantial (0.75) because the norm must actively suppress the alternative of outcomes-based or human-agency regulatory frameworks through diplomatic pressure, treaty negotiation, and stigmatization of LAWS development. Theater ratio is moderate (0.45): the humanitarian concern is genuine, but a growing share of normative activity is performative diplomatic signaling that outpaces enforceable legal obligation. Resistance is high (0.80) because major military powers openly contest the reading and continue LAWS development. The measurement series share a single time grid so that every metric is sampled at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC and civil society seats, the constraint is essential humanitarian coordination preventing the moral collapse of armed conflict; from the advanced military states seat, it is asymmetric extraction of strategic advantage dressed in humanitarian language. The engine computes this divergence from the structural data: agenda-setters and beneficiaries experience a coordination function with low effective extraction, while the payer seat experiences high effective extraction. Non-advanced states and civil society have different power levels and exit options than advanced states, so directionality differs even though all are state or state-adjacent actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (anti-militarist civil society, non_advanced_states) derive low directionality because the constraint subsidizes their strategic and normative positions. The agenda-setter (ICRC) also sits near the beneficiary end because its institutional legitimacy is reinforced by the prohibition. The payer (advanced_military_states) sits near the full-target end because the constraint extracts strategic advantage from them and they have only constrained exit (openly repudiating IHL carries diplomatic cost). The excluded defense industry is trapped: it cannot enter the norm-setting conversation and its commercial prospects are suppressed by the norm.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling the categorical prohibition as either pure humanitarian coordination (Rope) or pure strategic extraction (Snare). The constraint carries a genuine coordination functionâpreserving human moral agency in lethal decisionsâbut also asymmetric extraction: non-advanced states gain security they did not earn, and civil society gains influence from a position that costs them nothing. If the humanitarian coordination story were cover and the norm were driven solely by strategic parity seeking, the absence of a genuine coordination function would push the computed type toward Snare. The R5 genealogy interview shows the founding problem is contested, which flags potential future mandatrophy if LAWS technology proves capable of superior distinction and proportionality while the categorical ban persists on dignity grounds alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_parity_vs_human_dignity,
    'Does the categorical prohibition primarily serve humanitarian human dignity, or does it function as a strategic parity mechanism for states lacking LAWS capability?',
    'Analyze CCW and UNGA voting coalitions alongside advocacy funding flows and state strategic assessments to determine whether non-advanced states would abandon the prohibition if they acquired LAWS capability.',
    'If driven primarily by strategic parity, the coordination function is weaker and the constraint shifts toward Snare in computed classification; if driven primarily by dignity, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_parity_vs_human_dignity, empirical, 'Ambiguity between humanitarian dignity motive and strategic parity motive').

omega_variable(
    enforcement_illusion,
    'Is the categorical prohibition enforceable absent a supranational enforcement mechanism, or does it rely on self-interested state compliance and reputational costs?',
    'Observational tracking of state LAWS development programs against their diplomatic positions; measurement of actual restraint versus rhetorical adherence.',
    'If enforcement is largely illusory, the theater_ratio is higher than measured and the constraint is more performative than extractive; if enforcement is real via treaty ratification and sanctions, extraction is effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_illusion, empirical, 'Whether the prohibition is enforceable or largely theater').

omega_variable(
    dignity_per_se_contestability,
    'Is the claim that machine-decided killing violates human dignity per se empirically contestable, or is it a deontological axiom that remains stable regardless of technological performance?',
    'Jurisprudential and philosophical analysis of whether advancing LAWS capabilities could ever override the dignity claim, or whether the claim is definitionally immune to technological refutation.',
    'If contestable, the constraint may face axiom_overriding drift as technology improves, shifting its authority grounding; if immune, the constraint functions as a deontological mountain-claim within IHL despite its constructed features.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_per_se_contestability, conceptual, 'Whether the dignity axiom is definitionally immune to technological challenge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_cat_prohib_tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ihl_cat_prohib_tr_t5, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ihl_cat_prohib_tr_t10, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ihl_cat_prohib_tr_t15, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(ihl_cat_prohib_tr_t20, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ihl_cat_prohib_tr_t25, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(ihl_cat_prohib_tr_t30, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ihl_cat_prohib_be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ihl_cat_prohib_be_t5, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(ihl_cat_prohib_be_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(ihl_cat_prohib_be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(ihl_cat_prohib_be_t20, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(ihl_cat_prohib_be_t25, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(ihl_cat_prohib_be_t30, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ihl_cat_prohib_su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ihl_cat_prohib_su_t5, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(ihl_cat_prohib_su_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(ihl_cat_prohib_su_t15, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(ihl_cat_prohib_su_t20, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(ihl_cat_prohib_su_t25, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(ihl_cat_prohib_su_t30, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ihl_distinction_proportionality kernel. The categorical_prohibition_reading asserts a per se ban on autonomous weapons derived from the Martens Clause, while the human_agency_reading focuses on irreducible human judgment at the lethal moment, and the outcomes_based_reading evaluates LAWS solely on compliance with distinction and proportionality. These are structurally distinct constraints linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
