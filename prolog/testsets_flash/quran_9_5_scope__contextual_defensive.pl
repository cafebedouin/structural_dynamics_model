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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 as Contextual Defensive Warfare
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific hermeneutical reading of Quranic
 *   Verse 9:5, interpreting it as a directive applicable only to specific
 *   7th-century Medinan contexts involving treaty-breaking polytheist tribes,
 *   rather than a universal abrogation of peaceful verses. It prioritizes
 *   treaty obligations and defensive warfare. This reading is crucial for
 *   integrationist Muslim-majority states and peaceful pluralist Muslims, as
 *   it provides a theological basis for coexistence and stable international
 *   relations. The constraint's low extractiveness and suppression reflect
 *   its function as a coordination mechanism for ethical conduct rather than
 *   a tool for coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.15).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.2).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 as Contextual Defensive Warfare").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/hermeneutics/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'c8b2c8f0-da16-46c1-b1b0-a19c42324337').
narrative_ontology:cs_kernel_codification('c8b2c8f0-da16-46c1-b1b0-a19c42324337', fixed_text).
narrative_ontology:cs_authority_grounding('c8b2c8f0-da16-46c1-b1b0-a19c42324337', lineage).
narrative_ontology:cs_interpretation_layer_present('c8b2c8f0-da16-46c1-b1b0-a19c42324337').
narrative_ontology:cs_reading_relation('c8b2c8f0-da16-46c1-b1b0-a19c42324337', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('c8b2c8f0-da16-46c1-b1b0-a19c42324337', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('c8b2c8f0-da16-46c1-b1b0-a19c42324337', foundational, quranic_verses_context_dependent).
narrative_ontology:cs_axiom_status(quranic_verses_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('c8b2c8f0-da16-46c1-b1b0-a19c42324337', quranic_verses_context_dependent, conventional).
narrative_ontology:cs_axiom('c8b2c8f0-da16-46c1-b1b0-a19c42324337', foundational, treaty_obligations_sacrosanct).
narrative_ontology:cs_axiom_status(treaty_obligations_sacrosanct, holdable).
narrative_ontology:cs_axiom_grounding('c8b2c8f0-da16-46c1-b1b0-a19c42324337', treaty_obligations_sacrosanct, deontological).
narrative_ontology:cs_reference_frame('c8b2c8f0-da16-46c1-b1b0-a19c42324337', classical_defensive_jihad_framework).
narrative_ontology:cs_drift_state('c8b2c8f0-da16-46c1-b1b0-a19c42324337', contemporary_extremist_misinterpretations, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c8b2c8f0-da16-46c1-b1b0-a19c42324337', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_pluralist_muslims).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_aggressors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from a hermeneutic that allows for peaceful coexistence and treaty obligations, enabling stable international relations and internal pluralism. This reading provides a theological basis for their foreign policy and domestic governance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, mobile, national).

% Individuals and communities who seek to live peacefully within diverse societies and uphold universal ethical principles find theological justification and support in this contextual reading, which aligns with their lived experience and values.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_pluralist_muslims, beneficiary,
    organized, biographical, mobile, global).

% Historically, these were the specific polytheist tribes in 7th-century Medina who violated treaties and initiated hostilities. In contemporary application, this refers to any group or state that breaks peace treaties and engages in unprovoked aggression, becoming a legitimate target of defensive action.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_breaking_aggressors, payer,
    powerful, immediate, constrained, local).

% These groups reject the contextual-defensive reading, insisting on an abrogating-universal interpretation to justify offensive warfare. They are excluded from the legitimate interpretive community of this reading and their actions are condemned by its adherents.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, jihadist_extremist_groups, excluded,
    organized, generational, identity_locked, global).

% Scholars and jurists who historically and contemporarily advocate for this contextual reading, emphasizing the specific historical circumstances of revelation, the importance of treaty obligations, and the defensive nature of warfare in Islam. They shape the interpretive discourse.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_jurists_contextualists, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding and application of Quranic verses related to warfare, ensuring that military action is restricted to defensive contexts and respects treaty obligations, thereby fostering peace and stability within and between Muslim-majority societies.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist, universalizing readings to contextual, historically-grounded ones, shifting the burden of proof for military action to demonstrate prior aggression or treaty violation.
% ABSENT_VOICES: Jihadist and extremist groups, who advocate for an abrogating-universal interpretation, are actively excluded from the interpretive discourse of this reading. They would argue that this reading 'weakens' Islam's mandate for global dominance.
% DISAPPEARANCE_RATIONALE: If this contextual-defensive reading disappeared, the interpretive landscape would be dominated by more literalist and universalizing interpretations, potentially leading to increased justification for offensive warfare and undermining efforts for peaceful coexistence in Muslim-majority states.
% FOUNDING_PROBLEM: The problem of reconciling seemingly contradictory Quranic verses on warfare (peaceful vs. combative) and providing a coherent framework for Muslim conduct in both peace and conflict, especially regarding treaty obligations and defensive action.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as contemporary extremist groups continue to misinterpret Quranic verses to justify violence. Corroboration comes from international relations scholars, human rights organizations, and a broad consensus among mainstream Islamic scholars and institutions, all of whom actively work to counter extremist narratives and promote peaceful interpretations.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).

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
 *   The low extractiveness (0.15) and suppression (0.2) reflect that this reading primarily functions as a framework for ethical conduct and coordination, rather than a mechanism for extracting resources or coercing populations. Its 'victims' are only those who actively violate treaties and initiate aggression, making the constraint's application conditional and defensive. The theater ratio is low (0.05) because the interpretive work is genuinely aimed at ethical guidance, not performative justification for other ends. Accessibility collapse is high (0.8) because once this contextual understanding is adopted, alternative interpretations (e.g., universal offensive jihad) are largely foreclosed within this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of integrationist states and peaceful Muslims, this reading is a vital 'rope' for ethical governance and interfaith relations. From the perspective of extremist groups, it is a 'snare' that undermines what they perceive as the true, universal command for offensive jihad. The engine's classification will reflect the structural reality of the constraint as authored, which aligns with the 'rope' perspective for its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist Muslim-majority states and peaceful pluralist Muslims are beneficiaries, as this reading supports their goals of stability and coexistence. Treaty-breaking aggressors are the targets/payers, as the constraint legitimizes defensive action against them. Jihadist extremist groups are excluded, as their interpretation is fundamentally opposed to this reading's core tenets. Classical jurists act as agenda-setters, actively shaping and defending this interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively counters mandatrophy by re-contextualizing a verse often used to justify perpetual conflict. It prevents the mislabeling of a historically specific directive as a universal, timeless command, thereby preserving the ethical integrity of Islamic jurisprudence and preventing its degradation into a tool for unprovoked aggression. The 'live' status of the founding problem (reconciling verses) confirms its ongoing relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_certainty,
    'How definitively can the specific 7th-century Medinan context of Verse 9:5 be established and universally agreed upon by all interpretive schools?',
    'Consensus among historical and exegetical scholars across diverse Islamic traditions, supported by robust textual and archaeological evidence.',
    'Higher certainty strengthens this reading''s claim to historical accuracy, making it more difficult for universalizing interpretations to gain traction. Lower certainty leaves room for alternative readings to assert broader applicability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_certainty, empirical, 'The degree of certainty regarding the specific historical context of Quran 9:5.').

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (naskh) itself a valid hermeneutical principle for Quranic interpretation, and if so, what are its precise rules and limitations?',
    'A comprehensive re-evaluation of naskh by leading Islamic legal theorists and exegetes, leading to a revised and widely accepted methodology for its application, or its rejection as a principle.',
    'If naskh is rejected or severely limited, the ''abrogating_universal'' reading loses its primary theological tool. If its rules are clarified to exclude 9:5''s universal application, this reading is strengthened. If naskh is broadly affirmed without clear limits, this reading faces ongoing challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'The conceptual validity and precise application of the doctrine of abrogation in Quranic hermeneutics.').

omega_variable(
    coexistence_norm_primacy,
    'Does the Quran''s overall ethical trajectory and emphasis on justice and peace (as argued by the ''progressive_synthesis'' reading) structurally supersede literalist interpretations of specific verses?',
    'A shift in interpretive consensus within mainstream Islamic thought towards prioritizing macro-ethical principles over atomistic literalism, as evidenced by jurisprudential rulings and educational curricula.',
    'If macro-ethical primacy is established, this reading gains stronger support from a broader Quranic framework, potentially influencing its ''coexists_with'' relationship with the ''progressive_synthesis'' reading towards a more ''influences'' dynamic. If not, it remains a distinct, context-specific reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_norm_primacy, preference, 'The primacy of macro-ethical Quranic norms over literalist interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t350, quran_9_5_scope__contextual_defensive, theater_ratio, 350, 0.05).
narrative_ontology:measurement(qura_tr_t700, quran_9_5_scope__contextual_defensive, theater_ratio, 700, 0.05).
narrative_ontology:measurement(qura_tr_t1050, quran_9_5_scope__contextual_defensive, theater_ratio, 1050, 0.05).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__contextual_defensive, theater_ratio, 1400, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qura_be_t350, quran_9_5_scope__contextual_defensive, base_extractiveness, 350, 0.12).
narrative_ontology:measurement(qura_be_t700, quran_9_5_scope__contextual_defensive, base_extractiveness, 700, 0.13).
narrative_ontology:measurement(qura_be_t1050, quran_9_5_scope__contextual_defensive, base_extractiveness, 1050, 0.14).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__contextual_defensive, base_extractiveness, 1400, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(qura_su_t350, quran_9_5_scope__contextual_defensive, suppression_requirement, 350, 0.17).
narrative_ontology:measurement(qura_su_t700, quran_9_5_scope__contextual_defensive, suppression_requirement, 700, 0.18).
narrative_ontology:measurement(qura_su_t1050, quran_9_5_scope__contextual_defensive, suppression_requirement, 1050, 0.19).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__contextual_defensive, suppression_requirement, 1400, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
