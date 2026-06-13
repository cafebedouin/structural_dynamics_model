% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary (Balancing Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'balancing' reading of speech protection,
 *   where First Amendment interests are weighed against other constitutional
 *   values and demonstrated harms on a case-by-case basis. This approach
 *   leads to a fluid boundary between protected and unprotected speech, with
 *   the judiciary playing a central role in defining these limits. It is a
 *   reading that prioritizes flexibility and contextual judgment over rigid
 *   categorical rules, often resulting in intermediate scrutiny for complex
 *   speech issues like coded speech or systemic harm.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter (institutional/constrained)
 *   - speakers_of_controversial_speech: Primary payer (moderate/constrained)
 *   - public_order_advocates: Beneficiary (organized/mobile)
 *   - advocacy_groups: Payer (organized/constrained)
 *   - legislature: Excluded (institutional/constrained)
 *   - constitutional_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.45).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.3).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary (Balancing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, 'e3d26185-3d1c-4cdf-969a-403c12b0257c').
narrative_ontology:cs_kernel_codification('e3d26185-3d1c-4cdf-969a-403c12b0257c', fixed_text).
narrative_ontology:cs_authority_grounding('e3d26185-3d1c-4cdf-969a-403c12b0257c', lineage).
narrative_ontology:cs_interpretation_layer_present('e3d26185-3d1c-4cdf-969a-403c12b0257c').
narrative_ontology:cs_reading_relation('e3d26185-3d1c-4cdf-969a-403c12b0257c', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3d26185-3d1c-4cdf-969a-403c12b0257c', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('e3d26185-3d1c-4cdf-969a-403c12b0257c', foundational, first_amendment_is_not_absolute).
narrative_ontology:cs_axiom_status(first_amendment_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e3d26185-3d1c-4cdf-969a-403c12b0257c', first_amendment_is_not_absolute, deontological).
narrative_ontology:cs_axiom('e3d26185-3d1c-4cdf-969a-403c12b0257c', foundational, judicial_role_to_balance_competing_rights).
narrative_ontology:cs_axiom_status(judicial_role_to_balance_competing_rights, holdable).
narrative_ontology:cs_axiom_grounding('e3d26185-3d1c-4cdf-969a-403c12b0257c', judicial_role_to_balance_competing_rights, conventional).
narrative_ontology:cs_reference_frame('e3d26185-3d1c-4cdf-969a-403c12b0257c', post_brandenburg_flexible_interpretation).
narrative_ontology:cs_drift_state('e3d26185-3d1c-4cdf-969a-403c12b0257c', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e3d26185-3d1c-4cdf-969a-403c12b0257c', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, public_order_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_of_controversial_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, advocacy_groups).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, constitutional_pluralism).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, judicial_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary arbiter of speech cases, weighing competing constitutional values and societal harms. Their decisions define the boundaries of protected speech, often through complex, multi-factor tests. Benefits from expanded interpretive authority.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals or groups whose speech is deemed harmful or offensive, facing potential legal consequences or restrictions. They bear the cost of uncertainty and the risk of their speech being unprotected after the fact. Their 'exit' is self-censorship or legal challenge.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_of_controversial_speech, payer,
    moderate, immediate, constrained, local).

% Groups and individuals who prioritize public safety, civility, and the protection of vulnerable groups from harmful speech. They benefit from the judiciary's willingness to balance speech rights against other values, leading to restrictions on certain types of expression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, public_order_advocates, beneficiary,
    organized, biographical, mobile, national).

% Organizations that champion free speech principles, often representing speakers whose expression is challenged. They expend significant resources litigating cases and advocating for broader speech protections, bearing the costs of an unpredictable legal landscape.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, advocacy_groups, payer,
    organized, biographical, constrained, national).

% While capable of passing laws affecting speech, their actions are subject to judicial review under this balancing framework. They are 'excluded' from setting definitive, categorical speech rules without judicial oversight, often leading to legislative caution or deference.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, legislature, excluded,
    institutional, generational, constrained, national).

% Analyze and critique the evolving jurisprudence of speech, identifying patterns, inconsistencies, and implications of the balancing approach. They do not directly participate in enforcement but influence future legal arguments and public understanding.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for adjudicating conflicts between free speech and other societal interests, allowing the legal system to adapt to new forms of expression and evolving understandings of harm without rigid, absolute rules.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to define speech boundaries to the judiciary, from speakers who might otherwise claim absolute protection. It also transfers the burden of demonstrating harm to those seeking to restrict speech, but allows for that burden to be met through contextual balancing.
% ABSENT_VOICES: Those who advocate for a purely categorical approach to speech (either absolutist or harm-based) are structurally marginalized by a system that prioritizes case-by-case balancing. They would argue for greater predictability and less judicial discretion.
% DISAPPEARANCE_RATIONALE: If the balancing framework vanished, the legal system would either default to an absolutist or harm-limited approach, fundamentally altering the landscape of speech rights. The judiciary's role would be drastically curtailed, and the types of speech protected or restricted would change immediately.
% FOUNDING_PROBLEM: The inherent tension between the First Amendment's broad language and the need to protect other fundamental rights and societal interests (e.g., privacy, equality, public safety) from the potential harms of speech.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legal practitioners across the ideological spectrum acknowledge the ongoing challenge of reconciling free speech with other values. While the specific balance is contested, the existence of the underlying tension is widely corroborated by legal history and contemporary social debates, even by those who advocate for different solutions.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs of uncertainty and potential restriction borne by speakers, but also the benefit of a flexible system for public order. Suppression (0.30) is present due to judicial enforcement of speech limits, but it's not absolute, as speech is often protected. Theater ratio (0.10) is low, as the balancing act is a genuine, active process, not mere performance. The metrics show a slight increase in extractiveness over time, reflecting the growing complexity of speech issues and the increasing demands for balancing.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary and legitimate coordination mechanism for a complex society. From the perspective of speakers and advocacy groups, it can feel like an unpredictable and extractive system that chills speech and requires constant vigilance and litigation. Public order advocates, however, see it as a beneficial mechanism for protecting societal values.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from the expanded interpretive authority and flexibility (low d). Public order advocates also benefit from the ability to restrict harmful speech (low d). Speakers of controversial speech and advocacy groups bear the costs of uncertainty and potential restriction (high d). The legislature is structurally excluded from setting definitive rules, and constitutional scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing reading prevents mislabeling by acknowledging both the coordination function (adapting speech law to new contexts) and the extractive elements (costs to speakers, judicial power). It avoids the pitfall of a 'false summit' by recognizing that the 'naturalness' of speech limits is actively constructed through judicial interpretation, not inherent. The ongoing contestation over the 'founding problem status' (contested) highlights that while the problem of balancing speech is live, the *method* of balancing is itself a source of ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_predictability,
    'Does the case-by-case balancing approach provide sufficient predictability for speakers, or does it create an undue chilling effect due to uncertainty?',
    'Empirical studies on self-censorship rates among speakers in areas subject to balancing tests, compared to areas with more categorical rules.',
    'If uncertainty leads to significant chilling effects, the effective suppression and extractiveness of this reading are higher than measured, potentially pushing it closer to a Snare for speakers. If predictability is sufficient, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_predictability, empirical, 'The trade-off between judicial flexibility and speaker predictability.').

omega_variable(
    balancing_vs_absolutism_framing,
    'Is the ''balancing'' approach a necessary adaptation of First Amendment principles to modern harms, or a conceptual retreat from core free speech values?',
    'Conceptual analysis of the philosophical underpinnings of free speech and the First Amendment, examining whether balancing is consistent with its original intent or a reinterpretation.',
    'If framed as a retreat, the ''absolutist_reading'' gains conceptual ground, potentially shifting the perceived legitimacy of this reading downward. If framed as a necessary adaptation, this reading''s legitimacy is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_vs_absolutism_framing, conceptual, 'The fundamental conceptual disagreement over the nature of free speech.').

omega_variable(
    systemic_harm_measurement,
    'How reliably can ''demonstrated harms'' be measured, especially for systemic or cumulative harms (e.g., hate speech''s impact on marginalized groups) that are not direct, imminent incitement?',
    'Development of robust, interdisciplinary methodologies for measuring systemic social harms, and their acceptance within legal frameworks.',
    'If systemic harms are difficult to reliably demonstrate, the balancing test may disproportionately favor speech, effectively increasing protection. If they can be reliably measured, it strengthens the justification for restricting speech, increasing effective extraction from speakers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_harm_measurement, empirical, 'The empirical challenge of measuring systemic harms in speech cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__balancing_reading, theater_ratio, 1969, 0.08).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__balancing_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_boundary__balancing_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__balancing_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__balancing_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__balancing_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__balancing_reading, base_extractiveness, 1969, 0.35).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__balancing_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__balancing_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__balancing_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__balancing_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__balancing_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__balancing_reading, suppression_requirement, 1969, 0.25).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__balancing_reading, suppression_requirement, 1980, 0.27).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__balancing_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__balancing_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__balancing_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__balancing_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, online_content_moderation).

% DUAL FORMULATION NOTE:
% This constraint is the 'balancing_reading' of the 'speech_protection_boundary' kernel, which also includes 'absolutist_reading' and 'harm_limited_reading'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
