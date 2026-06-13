% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Internal Spiritual Struggle and Defensive War (Quranic Corpus Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents the mainstream, ethically constrained
 *   interpretation of 'jihad' within Islamic jurisprudence, emphasizing
 *   internal spiritual struggle (jihad al-nafs) and defensive armed response
 *   (jihad al-qital) strictly limited by proportionality and non-combatant
 *   immunity. It is rooted in the Quranic corpus and early Islamic practice,
 *   serving as a foundational ethical framework for Muslim conduct in peace
 *   and war. This reading is a 'mountain' in its claimed type due to its deep
 *   scriptural grounding and broad scholarly consensus, presenting itself as
 *   an irreducible feature of Islamic ethics.
 *
 * KEY AGENTS:
 *   - muslim_community_at_peace: Beneficiary (organized/mobile) — benefits from ethical clarity and peace
 *   - non_muslim_civilians: Beneficiary (powerless/trapped) — protected by non-combatant immunity
 *   - islamic_scholars_and_jurists: Agenda Setter (institutional/constrained) — interpret and uphold the reading
 *   - islamic_states_and_governments: Payer (institutional/constrained) — constrained by ethical limits on warfare
 *   - extremist_groups_and_militants: Excluded (powerless/trapped) — operate outside this legitimate framework
 *   - international_humanitarian_law_bodies: Observer (institutional/analytical) — evaluate alignment with global ethics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.2).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, mountain).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Spiritual Struggle and Defensive War (Quranic Corpus Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "islamic_jurisprudence/political_theology").

domain_priors:emerges_naturally(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '2f144b6b-fb04-465a-a483-0ef5f87c7613').
narrative_ontology:cs_kernel_codification('2f144b6b-fb04-465a-a483-0ef5f87c7613', fixed_text).
narrative_ontology:cs_authority_grounding('2f144b6b-fb04-465a-a483-0ef5f87c7613', lineage).
narrative_ontology:cs_interpretation_layer_present('2f144b6b-fb04-465a-a483-0ef5f87c7613').
narrative_ontology:cs_reading_relation('2f144b6b-fb04-465a-a483-0ef5f87c7613', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f144b6b-fb04-465a-a483-0ef5f87c7613', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('2f144b6b-fb04-465a-a483-0ef5f87c7613', foundational, jihad_primarily_spiritual).
narrative_ontology:cs_axiom_status(jihad_primarily_spiritual, holdable).
narrative_ontology:cs_axiom_grounding('2f144b6b-fb04-465a-a483-0ef5f87c7613', jihad_primarily_spiritual, deontological).
narrative_ontology:cs_axiom('2f144b6b-fb04-465a-a483-0ef5f87c7613', foundational, armed_jihad_defensive_only).
narrative_ontology:cs_axiom_status(armed_jihad_defensive_only, holdable).
narrative_ontology:cs_axiom_grounding('2f144b6b-fb04-465a-a483-0ef5f87c7613', armed_jihad_defensive_only, deontological).
narrative_ontology:cs_axiom('2f144b6b-fb04-465a-a483-0ef5f87c7613', secondary, non_combatant_immunity_absolute).
narrative_ontology:cs_axiom_status(non_combatant_immunity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2f144b6b-fb04-465a-a483-0ef5f87c7613', non_combatant_immunity_absolute, deontological).
narrative_ontology:cs_reference_frame('2f144b6b-fb04-465a-a483-0ef5f87c7613', quranic_sunnah_ethical_framework).
narrative_ontology:cs_drift_state('2f144b6b-fb04-465a-a483-0ef5f87c7613', contemporary_extremist_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2f144b6b-fb04-465a-a483-0ef5f87c7613', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community_at_peace).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, islamic_states_and_governments).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, islamic_ethics_of_war).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, peaceful_coexistence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the constraint by promoting internal spiritual development and ensuring that armed conflict is a last resort, preserving life and stability. This reading fosters a sense of security and ethical conduct within the community.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community_at_peace, beneficiary,
    organized, generational, mobile, global).

% Are protected by the constraint's emphasis on non-combatant immunity and proportionality, ensuring they are not targets of aggression. Their safety is a direct outcome of this interpretation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_civilians, beneficiary,
    powerless, immediate, trapped, global).

% Are the primary interpreters and custodians of this reading, responsible for articulating its principles and applying them to contemporary contexts. Their authority is derived from their knowledge of the Quran and Sunnah.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars_and_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Are constrained by this reading to only declare armed jihad defensively and under strict conditions, requiring significant restraint and adherence to international norms. This limits their military options and requires adherence to ethical guidelines.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_states_and_governments, payer,
    institutional, generational, constrained, national).

% Are explicitly excluded from legitimate interpretation and application of jihad under this reading, as their actions violate its core principles of proportionality, non-combatant immunity, and state authority. They operate outside this framework.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, extremist_groups_and_militants, excluded,
    powerless, immediate, trapped, regional).

% Observe and evaluate the application of this reading in practice, noting its alignment with principles of just war and protection of civilians. They provide an external, analytical perspective on its ethical implications.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Muslim community's understanding of 'jihad' to prioritize internal spiritual struggle and defensive, ethically constrained armed response, preventing unauthorized aggression and protecting non-combatants.
% TRANSFER_FUNCTION: Transfers the burden of ethical restraint and adherence to proportionality from individual actors to state authorities and religious scholars, while transferring security and moral clarity to the broader Muslim and non-Muslim populations.
% ABSENT_VOICES: Extremist groups and militants are absent from the legitimate discourse, as their interpretations are deemed outside the bounds of this reading. They would advocate for an unconstrained, offensive, and individualistic understanding of jihad.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the interpretation of jihad would become far more contested and potentially violent. The ethical constraints on armed conflict would weaken, leading to increased aggression, civilian casualties, and internal strife within Muslim communities, as well as heightened conflict with non-Muslims.
% FOUNDING_PROBLEM: The need to define and constrain the concept of 'jihad' within Islamic scripture to prevent indiscriminate violence, ensure ethical conduct in warfare, and prioritize spiritual development over worldly conquest, particularly after early Muslim expansion.
% FOUNDING_PROBLEM_CORROBORATION: Leading Islamic universities (e.g., Al-Azhar), international Islamic legal bodies, and mainstream Muslim scholars consistently corroborate this reading, emphasizing its scriptural basis and historical application. Their consensus, independent of any single state's agenda, supports the ongoing relevance of these constraints.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, ExtMetricName, E),
    domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jihad_quranic_corpus__defensive_spiritual_reading),
    narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.15) and suppression (0.2) are low because this reading is largely self-enforcing through religious conviction and scholarly consensus, rather than coercive. Its 'mountain' classification reflects its deep scriptural and ethical grounding, making it appear as an unchangeable aspect of Islamic law. The low theater ratio (0.1) indicates that its stated function (ethical guidance) genuinely aligns with its operation. The slight increase in extractiveness and suppression over time reflects the ongoing need to actively counter alternative, more aggressive interpretations, which requires continuous scholarly effort and sometimes state-level enforcement of this ethical framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Muslim community and non-Muslim civilians, this is a protective and beneficial constraint. For Islamic states, it imposes significant ethical and strategic limitations, making them 'payers' of restraint. Extremist groups are 'excluded' entirely, as their actions are fundamentally incompatible with this reading. The engine's per-seat classification should reflect these divergent experiences, with beneficiaries experiencing it as a Rope or Mountain, and states as a constrained Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The Muslim community and non-Muslim civilians are clear beneficiaries (d near 0.0) due to the peace and protection this reading affords. Islamic scholars and jurists act as agenda-setters, upholding and interpreting the constraint, benefiting from its moral authority. Islamic states are payers (d near 1.0) as they bear the costs of restraint and adherence to ethical limits in military action. Extremist groups are targets of the constraint's definitional boundaries, effectively excluded from its legitimate application.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists mandatrophy by continuously re-asserting its ethical foundations against more aggressive interpretations. Its persistence is not due to inertia but to ongoing scholarly and communal commitment to its principles. The classification as a Mountain (claimed) with low extraction and suppression prevents mislabeling it as a Snare or Tangled Rope, which would incorrectly imply it is primarily extractive or coercively maintained. The presence of beneficiaries and the 'emerges_naturally: true' flag, combined with omegas, triggers FSM to evaluate if it's a false summit, given its ethical benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ethics,
    'Is this reading of jihad a genuine natural law (an irreducible ethical feature of Islam) or a constructed ethical framework that benefits identifiable agents?',
    'Comparative theological and jurisprudential analysis across diverse Islamic schools of thought and historical periods to identify consistent, trans-temporal ethical principles versus context-dependent interpretations.',
    'If genuinely a natural law, its Mountain classification is robust. If primarily constructed, its benefits to peace and non-Muslims would reclassify it as a Rope, acknowledging its coordination function while removing the ''natural'' claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ethics, conceptual, 'Ambiguity between inherent ethical principle and constructed interpretation.').

omega_variable(
    state_authority_legitimacy,
    'To what extent is the requirement for state authority in declaring armed jihad a foundational principle of this reading, versus a pragmatic adaptation to modern political structures?',
    'Historical jurisprudential research into pre-modern Islamic legal opinions on who holds the authority to declare jihad in the absence of a unified caliphate or strong state structures.',
    'If a pragmatic adaptation, the constraint''s ''suppression'' of individual or non-state actors'' declarations of jihad might be re-evaluated as a Tangled Rope, where state authority extracts a monopoly on violence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy, empirical, 'Foundational vs. pragmatic nature of state authority requirement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of extremist interpretations structural (e.g., state counter-terrorism efforts) or internalized (e.g., theological rejection by mainstream Muslims)?',
    'Analysis of the post-state-collapse trajectory of extremist groups: if their influence persists or grows in the absence of state suppression, it suggests a stronger internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the theological rejection carries the suppression with it. If purely structural, removal of state power would lead to immediate resurgence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for extremist interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 610, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t610, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 610, 0.05).
narrative_ontology:measurement(jiha_tr_t1000, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(jiha_tr_t1500, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(jiha_tr_t1900, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t610, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 610, 0.1).
narrative_ontology:measurement(jiha_be_t1000, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(jiha_be_t1500, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1500, 0.13).
narrative_ontology:measurement(jiha_be_t1900, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t610, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 610, 0.15).
narrative_ontology:measurement(jiha_su_t1000, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1000, 0.17).
narrative_ontology:measurement(jiha_su_t1500, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1500, 0.18).
narrative_ontology:measurement(jiha_su_t1900, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1900, 0.19).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_law_of_nations).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, muslim_minority_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jihad_quranic_corpus' kernel. Its ethical constraints on warfare influence the broader 'islamic_law_of_nations' and 'muslim_minority_rights' by promoting peaceful coexistence and protection of non-Muslims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
