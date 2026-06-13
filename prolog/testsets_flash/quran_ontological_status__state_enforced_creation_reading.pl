% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Created Qur'an Doctrine (Mihna)
 *   domain: islamic_theology/political_authority
 *
 * SUMMARY:
 *   This constraint describes the historical period of the Mihna (833-848 CE)
 *   in the Abbasid Caliphate, where the theological doctrine of the Qur'an
 *   being 'created' (Mu'tazilite position) was enforced by state power
 *   through an inquisition. This transformed a metaphysical dispute into a
 *   mechanism of political and intellectual suppression. The constraint is a
 *   'snare' because its primary function was extraction of obedience and
 *   suppression of dissent, with the theological coordination story serving
 *   as cover for caliphal power consolidation.
 *
 * KEY AGENTS:
 *   - caliphal_authority: Agenda-setter (institutional/arbitrage) — benefits from doctrinal control
 *   - mu_tazilite_scholars: Beneficiary (organized/mobile) — benefits from state backing
 *   - traditionalist_scholars: Payer (moderate/trapped) — bears persecution
 *   - literalist_communities: Payer (powerless/identity_locked) — bears doctrinal coercion
 *   - scholarly_pluralism: Victim (analytical/analytical) — suppressed intellectual diversity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.85).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.92).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Created Qur'an Doctrine (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '0fc08ad2-1fb5-464e-a219-8891d805f27c').
narrative_ontology:cs_kernel_codification('0fc08ad2-1fb5-464e-a219-8891d805f27c', formalized).
narrative_ontology:cs_authority_grounding('0fc08ad2-1fb5-464e-a219-8891d805f27c', extraction).
narrative_ontology:cs_interpretation_layer_present('0fc08ad2-1fb5-464e-a219-8891d805f27c').
narrative_ontology:cs_reading_relation('0fc08ad2-1fb5-464e-a219-8891d805f27c', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('0fc08ad2-1fb5-464e-a219-8891d805f27c', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('0fc08ad2-1fb5-464e-a219-8891d805f27c', foundational, quran_is_created_and_temporal).
narrative_ontology:cs_axiom_status(quran_is_created_and_temporal, holdable).
narrative_ontology:cs_axiom_grounding('0fc08ad2-1fb5-464e-a219-8891d805f27c', quran_is_created_and_temporal, theological).
narrative_ontology:cs_axiom('0fc08ad2-1fb5-464e-a219-8891d805f27c', foundational, caliph_is_supreme_interpreter).
narrative_ontology:cs_axiom_status(caliph_is_supreme_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('0fc08ad2-1fb5-464e-a219-8891d805f27c', caliph_is_supreme_interpreter, conventional).
narrative_ontology:cs_reference_frame('0fc08ad2-1fb5-464e-a219-8891d805f27c', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('0fc08ad2-1fb5-464e-a219-8891d805f27c', post_mihna_abandonment, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0fc08ad2-1fb5-464e-a219-8891d805f27c', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.85) because the state directly coerced scholars to affirm a specific theological position, punishing dissent severely. Suppression is also very high (0.92) due to the inquisition tribunals, imprisonment, and torture used to enforce compliance. Theater ratio is low (0.15) because the enforcement was direct and brutal, not merely performative; the theological justification was a cover, but the coercion was real. The Mihna was a clear instance of state power converting a theological claim into a mechanism for political control and suppression of intellectual rivals.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the caliphal authority, this was a necessary act of religious purification and consolidation of legitimate rule. From the perspective of traditionalist scholars and literalist communities, it was an unjust persecution and an assault on fundamental religious truth. The engine's classification as a Snare reflects the latter, emphasizing the coercive and extractive nature of the state's actions.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal authority and Mu'tazilite scholars were clear beneficiaries, gaining political and intellectual dominance respectively. Traditionalist scholars and literalist communities were direct targets, suffering persecution and doctrinal coercion. Scholarly pluralism itself was a victim, as intellectual diversity was suppressed. The 'trapped' and 'identity_locked' exit options for victims reflect the severe consequences of non-compliance and the deep fusion of identity with the traditional doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mihna's mandate was to establish caliphal religious authority and enforce a specific theological doctrine. This mandate was ultimately resolved by the Caliph al-Mutawakkil, who ended the Mihna in 848 CE, restoring traditionalist scholars and abandoning the state-enforced Mu'tazilite position. The constraint's function as a tool of state control over theology atrophied when the political will to enforce it waned, leading to its eventual collapse. The classification as a Snare prevents mislabeling this as a genuine coordination effort, highlighting its coercive nature from the outset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Was the Mihna primarily a theological dispute that escalated into political enforcement, or a political power play that leveraged a theological dispute?',
    'Analysis of caliphal motivations and actions preceding the Mihna, and the political consequences of its enforcement and eventual abandonment.',
    'If primarily political, the extractiveness and suppression metrics are more accurately attributed to state power consolidation; if primarily theological, the constraint highlights the dangers of doctrinal absolutism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'The primary driver of the Mihna: political power or theological conviction.').

omega_variable(
    long_term_impact_on_pluralism,
    'To what extent did the Mihna permanently alter the trajectory of Islamic scholarly pluralism, beyond its immediate suppression?',
    'Comparative historical analysis of intellectual diversity in subsequent centuries, examining the emergence and suppression of new theological schools.',
    'If the impact was permanent, the ''scholarly_pluralism'' victim seat''s long-term costs are higher; if pluralism recovered, the constraint''s long-term suppressive force was limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_pluralism, empirical, 'The lasting effect of the Mihna on intellectual diversity in Islamic thought.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of traditionalist scholars structural (state power, legal barriers) or internalized (fear, self-censorship after initial persecution)?',
    'Post-Mihna scholarly output and dissent: if suppression persisted after the Mihna''s official end, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as scholars carried the suppression with them after the Mihna''s end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting scholars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 848).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.1).
narrative_ontology:measurement(qura_tr_t836, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 836, 0.12).
narrative_ontology:measurement(qura_tr_t839, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 839, 0.15).
narrative_ontology:measurement(qura_tr_t842, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 842, 0.18).
narrative_ontology:measurement(qura_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.17).
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.15).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.75).
narrative_ontology:measurement(qura_be_t836, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 836, 0.8).
narrative_ontology:measurement(qura_be_t839, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 839, 0.85).
narrative_ontology:measurement(qura_be_t842, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 842, 0.88).
narrative_ontology:measurement(qura_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.87).
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.85).
narrative_ontology:measurement(qura_su_t836, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 836, 0.9).
narrative_ontology:measurement(qura_su_t839, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 839, 0.92).
narrative_ontology:measurement(qura_su_t842, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 842, 0.9).
narrative_ontology:measurement(qura_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.88).
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, caliphal_legitimacy_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel, specifically the state-enforced Mu'tazilite doctrine during the Mihna. It is linked to the purely theological 'created_reading' and 'uncreated_reading' as it represents the political enforcement of one of these theological positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
