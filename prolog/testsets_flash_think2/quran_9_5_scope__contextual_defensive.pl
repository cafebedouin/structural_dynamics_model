% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quranic Verse 9:5 as Contextual Defensive Command
 *   domain: Islamic Jurisprudence / Hermeneutics / Political Theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contextual_defensive' reading of
 *   Quranic Verse 9:5. This reading asserts that the verse addresses specific
 *   historical circumstances in 7th-century Medina concerning treaty-breaking
 *   polytheist tribes, and does not abrogate other peaceful verses. It
 *   prioritizes treaty obligations and limits warfare to defensive actions
 *   only. This interpretation is crucial for integrationist Muslim-majority
 *   states and peaceful pluralist Muslims seeking to reconcile Islamic
 *   teachings with modern international norms and interfaith coexistence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.35).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.45).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quranic Verse 9:5 as Contextual Defensive Command").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "Islamic Jurisprudence / Hermeneutics / Political Theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde').
narrative_ontology:cs_kernel_codification('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', fixed_text).
narrative_ontology:cs_authority_grounding('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', lineage).
narrative_ontology:cs_interpretation_layer_present('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde').
narrative_ontology:cs_reading_relation('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', foundational, quranic_coherence_no_abrogation).
narrative_ontology:cs_axiom_status(quranic_coherence_no_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', quranic_coherence_no_abrogation, deontological).
narrative_ontology:cs_axiom('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', foundational, defensive_warfare_only).
narrative_ontology:cs_axiom_status(defensive_warfare_only, holdable).
narrative_ontology:cs_axiom_grounding('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', defensive_warfare_only, conventional).
narrative_ontology:cs_reference_frame('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', medinan_treaty_ethics).
narrative_ontology:cs_drift_state('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', contemporary_hermeneutic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7fd8dffc-e7c8-4aeb-9135-fcfbf8b4cdde', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_pluralist_muslims).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, non_muslim_minorities_in_muslim_lands).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, aggressive_jihadist_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states adopt and promote this interpretive framework to justify peaceful coexistence, uphold international treaties, and counter extremist narratives. They benefit from the stability and legitimacy this reading provides.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, agenda_setter,
    institutional, generational, constrained, global).

% Individuals and communities who seek to live peacefully with non-Muslims and uphold universal ethical principles. This reading provides theological justification for their worldview and protects them from accusations of apostasy or weakness.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_pluralist_muslims, beneficiary,
    organized, biographical, mobile, global).

% Historically, these were the specific tribes in 7th-century Medina who violated treaties with the early Muslim community, becoming targets of defensive action. They bore the direct costs of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes, payer,
    powerless, immediate, trapped, local).

% Modern groups who advocate for universal offensive jihad based on a literalist, abrogating interpretation of 9:5. This contextual-defensive reading directly constrains their theological justification and actions, making them structural victims of its widespread adoption.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, aggressive_jihadist_groups, payer,
    organized, biographical, constrained, global).

% Scholars who specialize in historical context, hermeneutics, and the broader Quranic corpus to argue for this contextual-defensive interpretation. They actively shape and disseminate this understanding.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, islamic_scholars_contextualists, agenda_setter,
    institutional, generational, analytical, global).

% Scholars who adhere to a literalist interpretation, often arguing for the abrogation of peaceful verses by 9:5. They are excluded from the interpretive consensus of this reading and their views are actively countered by its proponents.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, islamic_scholars_literalists, excluded,
    powerful, generational, identity_locked, global).

% These communities benefit from the theological and legal framework that prioritizes their protection, treaty obligations, and peaceful coexistence, as opposed to interpretations that would target them.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, non_muslim_minorities_in_muslim_lands, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for inter-group relations based on treaty adherence, defensive action, and the preservation of peaceful verses, preventing indiscriminate aggression and promoting coexistence within Islamic jurisprudence.
% TRANSFER_FUNCTION: Transfers theological legitimacy and security to those upholding treaties and advocating for defensive warfare only; transfers constraint and potential defensive action to those violating treaties or advocating for offensive aggression.
% ABSENT_VOICES: Literalist scholars and aggressive jihadist groups are structurally excluded from the interpretive consensus of this reading; they would argue for a universal, abrogating interpretation of 9:5.
% DISAPPEARANCE_RATIONALE: If this contextual-defensive interpretation vanished, the default understanding of 9:5 might shift towards more aggressive, universalist readings, leading to increased conflict, undermining peaceful coexistence efforts, and destabilizing inter-religious relations globally.
% FOUNDING_PROBLEM: The problem was to regulate conflict and establish clear rules of engagement with specific treaty-breaking polytheist tribes in 7th-century Medina, while upholding the broader Quranic emphasis on peace, justice, and treaty obligations.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as 'live' by contemporary integrationist Islamic scholars who see the ongoing challenge of preventing misapplication of religious texts to justify aggression. Historical accounts of the Medinan period and the broader Quranic corpus emphasizing peace and justice also corroborate the original intent to regulate specific conflicts, not to mandate universal aggression. This is corroborated by independent historical and theological analysis from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is relatively low because this reading limits the scope of conflict and extraction to specific, justified defensive actions against treaty violators, rather than universal aggression. Suppression (0.45) is moderate, reflecting the active effort required to counter and suppress more aggressive, literalist interpretations within Islamic discourse. Theater ratio (0.10) is low as this reading is about genuine theological and ethical application, not performative maintenance. Accessibility collapse (0.20) is low because peaceful alternatives are prioritized, not collapsed. Resistance (0.40) is moderate, as this reading faces ongoing contestation from literalist factions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of literalist scholars and aggressive jihadist groups, this reading would be seen as a distortion or weakening of divine command, potentially even as an act of apostasy. They would experience it as a severe suppression of what they believe to be a core religious obligation, leading to a dramatically different classification (likely a Snare or Tangled Rope from their seat).
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist Muslim-majority states and peaceful pluralist Muslims are beneficiaries, as this reading provides theological grounding for their policies of peace and coexistence. Non-Muslim minorities also benefit from the protection and stability it offers. Historically, treaty-breaking polytheist tribes were targets. In the modern context, aggressive jihadist groups are victims, as this reading directly undermines their theological justifications for offensive warfare.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_definitiveness,
    'Is the historical evidence for the specific 7th-century Medinan context of Verse 9:5 sufficiently definitive to rule out broader applications, or is it open to alternative historical interpretations?',
    'Further archaeological, textual, and historical-critical research into the early Islamic period, particularly concerning the precise circumstances of the revelation and the nature of the ''polytheist tribes'' mentioned.',
    'If the historical context is less definitive, the ''contextual_defensive'' reading''s claim to limit 9:5''s scope is weakened, potentially increasing its extractiveness and suppression as it relies more on interpretive choice than historical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_definitiveness, empirical, 'Ambiguity regarding the definitive historical context of Quran 9:5.').

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (naskh) itself a valid hermeneutic principle in Islamic jurisprudence, or is its application to 9:5 a contested interpretive choice?',
    'Consensus shifts among leading Islamic jurists and theologians regarding the methodological principles of naskh, or the development of alternative hermeneutic frameworks that explicitly reject it.',
    'If abrogation is widely rejected, the ''abrogating_universal'' reading loses its theological foundation, strengthening the ''contextual_defensive'' reading and reducing the suppression required to maintain it. If abrogation is affirmed, the ''contextual_defensive'' reading faces greater internal theological challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'Contestation over the theological validity of the abrogation doctrine in interpreting Quran 9:5.').

omega_variable(
    modern_state_legitimacy_vs_scripture,
    'To what extent is this ''contextual_defensive'' reading driven by a genuine theological re-evaluation, versus a pragmatic need for Muslim-majority states to align with modern international law and human rights norms?',
    'Analysis of the internal theological arguments presented by proponents of this reading, compared with the political and social pressures faced by Muslim-majority states in the international system. Examination of whether similar interpretive shifts occur in contexts without such external pressures.',
    'If primarily pragmatic, the reading''s theological legitimacy might be perceived as weaker by some internal audiences, increasing internal resistance. If genuinely theological, its persuasive power within Islamic discourse is stronger, reducing the need for active suppression of opposing views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_state_legitimacy_vs_scripture, preference, 'Motivation for the ''contextual_defensive'' reading: theological conviction vs. political pragmatism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.1).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.1).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.1).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.1).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__contextual_defensive, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__contextual_defensive, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__contextual_defensive, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__contextual_defensive, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
