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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 Contextual-Defensive Interpretation
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific hermeneutical reading of Quranic
 *   Verse 9:5, which interprets the verse as addressing a particular
 *   7th-century Medinan context involving treaty-breaking polytheist tribes.
 *   This reading asserts that 9:5 does not abrogate other peaceful verses,
 *   and that Islamic jurisprudence prioritizes treaty obligations and
 *   defensive warfare only. It is a 'tangled_rope' because it genuinely
 *   coordinates peaceful statecraft and pluralism among its beneficiaries,
 *   but actively extracts from and suppresses alternative, more aggressive
 *   interpretations, particularly those used by extremist groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.45).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.6).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 Contextual-Defensive Interpretation").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '4768764e-a122-4f1f-acfa-1bc9d65e1363').
narrative_ontology:cs_kernel_codification('4768764e-a122-4f1f-acfa-1bc9d65e1363', fixed_text).
narrative_ontology:cs_authority_grounding('4768764e-a122-4f1f-acfa-1bc9d65e1363', lineage).
narrative_ontology:cs_interpretation_layer_present('4768764e-a122-4f1f-acfa-1bc9d65e1363').
narrative_ontology:cs_reading_relation('4768764e-a122-4f1f-acfa-1bc9d65e1363', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('4768764e-a122-4f1f-acfa-1bc9d65e1363', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('4768764e-a122-4f1f-acfa-1bc9d65e1363', foundational, quranic_coherence_no_abrogation).
narrative_ontology:cs_axiom_status(quranic_coherence_no_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('4768764e-a122-4f1f-acfa-1bc9d65e1363', quranic_coherence_no_abrogation, deontological).
narrative_ontology:cs_axiom('4768764e-a122-4f1f-acfa-1bc9d65e1363', foundational, defensive_warfare_only).
narrative_ontology:cs_axiom_status(defensive_warfare_only, holdable).
narrative_ontology:cs_axiom_grounding('4768764e-a122-4f1f-acfa-1bc9d65e1363', defensive_warfare_only, conventional).
narrative_ontology:cs_reference_frame('4768764e-a122-4f1f-acfa-1bc9d65e1363', classical_medinan_treaty_law).
narrative_ontology:cs_drift_state('4768764e-a122-4f1f-acfa-1bc9d65e1363', contemporary_extremist_challenge, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4768764e-a122-4f1f-acfa-1bc9d65e1363', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_pluralist_advocates).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, extremist_militant_groups).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_polytheist_tribes_historical).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states adopt and promote this interpretation to justify peaceful foreign policy, uphold international treaties, and counter extremist narratives. They benefit from a theological framework that supports their geopolitical interests in stability and cooperation, but are constrained by internal and external pressures from alternative interpretations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, agenda_setter,
    institutional, generational, constrained, national).

% Scholars, activists, and civil society groups who champion this interpretation to promote interfaith dialogue, human rights, and peaceful coexistence. They gain theological legitimacy for their advocacy, but their influence is contingent on broader acceptance of this reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_pluralist_advocates, beneficiary,
    organized, biographical, mobile, global).

% These groups are delegitimized and actively opposed by this interpretation, which denies their theological basis for offensive jihad and indiscriminate violence. They bear the cost of being branded as deviant and face state and international opposition justified by this reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, extremist_militant_groups, payer,
    powerless, immediate, trapped, global).

% The historical figures and their interpretive methods from the 7th-century Medinan context, whose specific circumstances and rulings are invoked as the primary evidence for this reading. They are observed and interpreted, not active agents in the contemporary contest.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_jurists_medinan_context, observer,
    analytical, civilizational, analytical, universal).

% Adherents of the view that Quran 9:5 abrogates all prior peaceful verses, establishing universal offensive jihad. They are structurally excluded from the interpretive framework of the 'contextual_defensive' reading, which explicitly rejects their core premise. They operate in a competing interpretive space.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogating_universal_proponents, excluded,
    powerful, generational, constrained, global).

% Scholars and thinkers who advocate for a 'progressive synthesis' reading, emphasizing the Quran's broader ethical trajectory over literalist application of specific verses. While sharing a non-literalist approach, their methodology differs from the 'contextual_defensive' reading, making them observers of this specific interpretive contest.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, progressive_synthesis_proponents, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate Islamic jurisprudence and state policy towards upholding treaty obligations, engaging in defensive warfare only, and fostering peaceful coexistence with non-Muslims, by grounding these principles in a specific contextual reading of Quran 9:5.
% TRANSFER_FUNCTION: Transfers theological legitimacy and moral authority from interpretations advocating universal offensive jihad to those supporting defensive warfare and treaty adherence. It also transfers political capital to states and actors who align with this peaceful interpretation.
% ABSENT_VOICES: Proponents of the 'abrogating_universal' reading are structurally excluded from the interpretive framework of this constraint. If present, they would argue that 9:5 establishes a universal, offensive obligation, rendering peaceful verses obsolete, and that this 'contextual-defensive' reading is a distortion of Islamic law.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the theological landscape would likely shift towards more literalist or abrogating readings of Quran 9:5. This would undermine the jurisprudential basis for peaceful coexistence, treaty obligations, and defensive-only warfare in many Muslim-majority contexts, potentially leading to increased internal and external conflict and a reorientation of state policies.
% FOUNDING_PROBLEM: The founding problem was to reconcile seemingly contradictory Quranic verses regarding warfare and peace, and to provide a jurisprudential basis for Muslim states and communities to engage in peaceful international relations while maintaining the right to self-defense, particularly in response to extremist interpretations.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Islamic scholars, international relations experts, and human rights organizations consistently corroborate the ongoing need for this interpretation. They cite its role in countering extremist narratives, promoting interfaith harmony, and providing a theological foundation for modern Muslim-majority states to operate within international law. This corroboration comes from outside the immediate beneficiaries of the interpretation.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) stems from the delegitimization and active opposition to extremist groups and their interpretations, which are 'victims' of this constraint. Suppression (0.6) is significant because this reading requires active scholarly and political effort to marginalize and counter competing interpretations that advocate for offensive jihad. The theater ratio is low (0.15) as the interpretation is genuinely applied in policy and jurisprudence, though some performative aspects exist in public discourse. The claimed type 'tangled_rope' reflects the dual function of coordinating peaceful actors while actively extracting from and suppressing those who violate its norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of integrationist states and pluralist advocates, this interpretation is a necessary and beneficial coordination mechanism for peace. From the perspective of extremist groups, it is an oppressive distortion of divine command, actively suppressing what they believe to be true. The engine's classification as 'tangled_rope' captures this inherent asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist Muslim-majority states and peaceful pluralist advocates are beneficiaries, as this interpretation provides a theological framework for their desired policies and advocacy (low d). Extremist militant groups are targets, as their theological justifications are directly challenged and suppressed (high d). Historical figures are observers, providing the context for the interpretation. Proponents of the abrogating-universal reading are excluded, as their core premise is foreclosed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_accuracy,
    'To what extent is the specific 7th-century Medinan context invoked by this reading historically and textually verifiable, and not a post-hoc justification for modern policy?',
    'Comprehensive historical-critical analysis of early Islamic sources, independent of contemporary political agendas, to establish the precise circumstances of 9:5''s revelation and initial application.',
    'If the historical context is found to be less specific or more ambiguous than claimed, the ''contextual_defensive'' reading''s foundation weakens, potentially increasing the legitimacy of alternative interpretations. If strongly corroborated, its authority is enhanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_accuracy, empirical, 'Verifiability of the historical context grounding the interpretation.').

omega_variable(
    suppression_mechanism_legitimacy,
    'Is the suppression of ''abrogating_universal'' interpretations primarily due to scholarly consensus on textual evidence, or due to political pressure from states seeking to delegitimize extremist groups?',
    'Analysis of scholarly discourse and fatwas (religious edicts) for internal consistency and reliance on textual/historical arguments versus alignment with state policy objectives. Examine funding sources and institutional affiliations of prominent scholars.',
    'If suppression is primarily political, the constraint''s ''tangled_rope'' nature is amplified, indicating a stronger extractive component where state power leverages theological interpretation. If primarily scholarly, it reinforces the coordination function of establishing a coherent, defensible jurisprudence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_legitimacy, conceptual, 'Nature of suppression: scholarly consensus vs. political pressure.').

omega_variable(
    coexistence_norms_robustness,
    'How robust are the ''coexistence norms'' structurally protected by this reading against internal pressures from more conservative interpretations or external geopolitical shifts?',
    'Longitudinal study of state policies, interfaith relations, and public discourse in Muslim-majority countries over several decades, observing shifts in the application and rhetoric surrounding these norms in response to various pressures.',
    'If norms prove fragile, the ''rope'' aspect of coordination is weaker, and the ''tangled'' aspect of maintaining the interpretation against internal dissent becomes more prominent. If robust, it strengthens the claim of genuine, stable coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coexistence_norms_robustness, empirical, 'Stability of coexistence norms under pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1950, quran_9_5_scope__contextual_defensive, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(qura_tr_t1970, quran_9_5_scope__contextual_defensive, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(qura_tr_t1990, quran_9_5_scope__contextual_defensive, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(qura_tr_t2010, quran_9_5_scope__contextual_defensive, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(qura_tr_t2025, quran_9_5_scope__contextual_defensive, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(qura_be_t1950, quran_9_5_scope__contextual_defensive, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(qura_be_t1970, quran_9_5_scope__contextual_defensive, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(qura_be_t1990, quran_9_5_scope__contextual_defensive, base_extractiveness, 1990, 0.41).
narrative_ontology:measurement(qura_be_t2010, quran_9_5_scope__contextual_defensive, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(qura_be_t2025, quran_9_5_scope__contextual_defensive, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1950, quran_9_5_scope__contextual_defensive, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(qura_su_t1970, quran_9_5_scope__contextual_defensive, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(qura_su_t1990, quran_9_5_scope__contextual_defensive, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(qura_su_t2010, quran_9_5_scope__contextual_defensive, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(qura_su_t2025, quran_9_5_scope__contextual_defensive, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_9_5_scope' kernel, alongside 'abrogating_universal' and 'progressive_synthesis'. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
