% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 as Abrogating Universal Offensive Jihad
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint models the interpretation of Quran 9:5 (the 'Verse of the
 *   Sword') as abrogating all prior peaceful verses, thereby establishing
 *   universal offensive jihad as a standing legal obligation until
 *   polytheists submit or convert. This reading is a foundational ideological
 *   pillar for various expansionist jihadi movements and radical clerical
 *   authorities. It transforms all non-Muslims into legitimate targets in the
 *   absence of formal submission, authorizing first-strike violence and
 *   suppressing any frameworks for peaceful coexistence. The claimed type is
 *   'snare' because its coordination story (unifying the Muslim community) is
 *   a cover for pure extraction (violence, subjugation, resource transfer)
 *   from identifiable victims, maintained through active enforcement and
 *   suppression of alternatives.
 *
 * KEY AGENTS:
 *   - expansionist_jihadi_movements: Primary agenda-setter and beneficiary (organized/identity_locked) — actively enforces and benefits from ideological justification and conquest.
 *   - radical_clerics: Primary beneficiary (powerful/constrained) — propagates the reading, gaining influence and authority.
 *   - non_muslim_populations: Primary victim (powerless/trapped) — directly targeted by violence, forced conversion, or subjugation.
 *   - moderate_muslim_scholars: Victim (moderate/constrained) — advocates for alternative readings, facing threats and marginalization.
 *   - muslim_civilians_in_conflict_zones: Victim (powerless/trapped) — suffers displacement and violence in conflicts fueled by this interpretation.
 *   - international_human_rights_organizations: Observer (organized/analytical) — documents impact, challenges legitimacy, but cannot directly alter enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.95).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 as Abrogating Universal Offensive Jihad").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '815c94f9-0c62-40a4-a5c3-dab28ec14d5a').
narrative_ontology:cs_kernel_codification('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', fixed_text).
narrative_ontology:cs_authority_grounding('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', lineage).
narrative_ontology:cs_interpretation_layer_present('815c94f9-0c62-40a4-a5c3-dab28ec14d5a').
narrative_ontology:cs_reading_relation('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', foundational, verse_9_5_abrogates_peaceful_verses).
narrative_ontology:cs_axiom_status(verse_9_5_abrogates_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', verse_9_5_abrogates_peaceful_verses, theological).
narrative_ontology:cs_axiom('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', foundational, universal_offensive_jihad_is_obligatory).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_is_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', universal_offensive_jihad_is_obligatory, theological).
narrative_ontology:cs_reference_frame('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', classical_abrogating_jihad_doctrine).
narrative_ontology:cs_drift_state('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', contemporary_global_context, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('815c94f9-0c62-40a4-a5c3-dab28ec14d5a', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, radical_clerics).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, doctrine_of_abrogation_nasikh).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, supremacy_of_islamic_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups interpret 9:5 as a divine mandate for perpetual offensive warfare against non-Muslims until their submission or conversion. They actively enforce this interpretation through violence and recruitment, benefiting from the ideological justification for their actions and the resources gained through conquest.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements, agenda_setter,
    organized, generational, identity_locked, global).

% These religious authorities propagate the abrogating_universal reading, gaining immense influence, followers, and often material support. Their careers and authority are built upon this interpretation, making any deviation a threat to their standing.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, radical_clerics, beneficiary,
    powerful, biographical, constrained, global).

% These populations are directly targeted by violence, forced conversion, or subjugation under this interpretation. Their options are resistance (often futile), flight, or submission, bearing the full cost of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations, payer,
    powerless, immediate, trapped, local).

% These scholars advocate for contextual or progressive readings of 9:5, often facing severe threats, persecution, or marginalization from proponents of the abrogating_universal view. They bear the cost of intellectual suppression and personal risk.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars, payer,
    moderate, biographical, constrained, global).

% Caught in conflicts fueled by this interpretation, they suffer displacement, violence, and the breakdown of social order. They are often forced to choose sides or become victims of both sides, bearing immense human cost.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% These organizations document atrocities and advocate for the protection of civilians and religious minorities, challenging the legitimacy of actions justified by this interpretation. They analyze the constraint's impact but cannot directly alter its enforcement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For proponents, it coordinates a unified ideological and military front for the expansion of a specific interpretation of Islamic rule, providing clear directives for engagement with non-Muslims.
% TRANSFER_FUNCTION: Transfers resources, territory, and populations from non-Muslim entities to expansionist jihadi movements, along with ideological legitimacy and authority to radical clerics.
% ABSENT_VOICES: Victims of violence, religious minorities, and dissenting Muslim voices are actively suppressed or eliminated, preventing any challenge to the interpretation's legitimacy. Their absence ensures the narrative of divine mandate remains unchallenged within the enforcing groups.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the ideological justification for many jihadi movements would collapse, leading to a significant reduction in religiously motivated violence, a re-evaluation of interfaith relations, and a shift in the internal dynamics of Islamic jurisprudence. The geopolitical landscape would fundamentally alter.
% FOUNDING_PROBLEM: The problem of establishing and expanding the early Islamic state's authority and territorial control in a hostile 7th-century Arabian context, particularly concerning polytheist tribes perceived as a threat.
% FOUNDING_PROBLEM_CORROBORATION: Proponents claim the problem is 'live' as a universal, eternal command. However, a vast body of historical scholarship and the consensus of mainstream Islamic jurisprudence (outside the benefiting parties) attests that the specific historical context of 7th-century Arabia is 'dead' as a direct, universal legal command for offensive warfare today. Independent historical and textual analysis corroborates the context-specific nature of the original directive.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is extremely high (0.9) because this interpretation mandates the subjugation or elimination of non-Muslims, leading to immense human and material costs for victims. Suppression is also very high (0.95) due to the active use of violence, intimidation, and ideological coercion to silence dissent and eliminate alternatives to this interpretation. The theater ratio is low (0.1) because the constraint's function is directly and brutally enacted; there is little performative cover for its core operations. The historical measurements show a rise in extractiveness and suppression, reflecting the increasing prominence and enforcement of this interpretation by various groups since the late 20th century.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of expansionist jihadi movements and radical clerics, this constraint is a divinely ordained command, a 'rope' for unifying the faithful and establishing justice. From the perspective of non-Muslim populations and moderate Muslim scholars, it is a 'snare' of pure extraction and violence. The engine's classification will reflect the latter due to the high extractiveness and suppression, despite the proponents' 'rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist jihadi movements and radical clerics are clear beneficiaries, as the constraint provides ideological justification, recruits, and resources for their goals (low directionality). Non-Muslim populations, moderate Muslim scholars, and Muslim civilians in conflict zones are direct targets, bearing the full brunt of violence, subjugation, and suppression (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (establishing early Islamic state authority) is 'dead' in its original context, yet the arrangement persists with 'world_rearranges' consequences if it vanished. This indicates a severe case of mandatrophy, where an interpretation designed for a specific historical exigency has been re-purposed to justify ongoing extraction. The classification as 'snare' prevents mislabeling this as a legitimate coordination mechanism, highlighting its coercive and extractive nature despite its proponents' claims of divine mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_vs_universal_application,
    'Is Quran 9:5 a context-specific directive for 7th-century Arabia, or a universal, eternal legal command for all times and places?',
    'Comprehensive historical-critical analysis of early Islamic sources, linguistic analysis of the Quranic text, and comparative study of classical and modern jurisprudential interpretations, particularly those outside the benefiting parties.',
    'If context-specific, the constraint''s legitimacy for universal application collapses, reducing its extractiveness and suppression to near zero. If universal, its current high extractiveness and suppression are ideologically justified for its proponents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_context_vs_universal_application, empirical, 'Ambiguity regarding the historical scope and applicability of Quran 9:5.').

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (nasikh wa mansukh) itself a valid hermeneutical principle, or is it a later jurisprudential construct used to reconcile conflicting verses?',
    'Theological and jurisprudential debate within Islamic scholarship, examining the Quranic text for internal evidence of abrogation and the historical development of the doctrine.',
    'If the doctrine of abrogation is invalid, the premise for 9:5 superseding peaceful verses collapses, fundamentally altering the constraint''s structural basis and reducing its extractiveness. If valid, the abrogating_universal reading gains a stronger internal theological justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'Uncertainty regarding the theological validity of the abrogation doctrine.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers, violence) or internalized (ideological indoctrination, fear of apostasy)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals leaving jihadi groups still self-censor or fear reprisal), reclassify as partially internalized. Analysis of indoctrination methods and psychological impact on adherents.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit, making genuine freedom of thought or action extremely difficult even in safe environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ideological contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1980, quran_9_5_scope__abrogating_universal, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(qura_tr_t1990, quran_9_5_scope__abrogating_universal, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(qura_tr_t2000, quran_9_5_scope__abrogating_universal, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(qura_tr_t2010, quran_9_5_scope__abrogating_universal, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(qura_tr_t2024, quran_9_5_scope__abrogating_universal, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t1980, quran_9_5_scope__abrogating_universal, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(qura_be_t1990, quran_9_5_scope__abrogating_universal, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(qura_be_t2000, quran_9_5_scope__abrogating_universal, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(qura_be_t2010, quran_9_5_scope__abrogating_universal, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(qura_be_t2024, quran_9_5_scope__abrogating_universal, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1980, quran_9_5_scope__abrogating_universal, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(qura_su_t1990, quran_9_5_scope__abrogating_universal, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(qura_su_t2000, quran_9_5_scope__abrogating_universal, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(qura_su_t2010, quran_9_5_scope__abrogating_universal, suppression_requirement, 2010, 0.95).
narrative_ontology:measurement(qura_su_t2024, quran_9_5_scope__abrogating_universal, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_state_legitimacy_claims).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, interfaith_dialogue_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_9_5_scope' kernel. Its high extractiveness and suppression contrast sharply with contextual and progressive readings, which emphasize peace and defensive warfare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
