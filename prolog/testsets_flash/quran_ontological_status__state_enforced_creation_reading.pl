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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Created Qur'an Doctrine (Mihna)
 *   domain: islamic_theology/political_authority
 *
 * SUMMARY:
 *   This constraint describes the historical period of the Mihna
 *   (Inquisition) in the Abbasid Caliphate, where the Mu'tazilite doctrine of
 *   the Qur'an's createdness was enforced by state power. It represents a
 *   specific reading of the Qur'an's ontological status that was weaponized
 *   for political control, transforming a theological debate into a mechanism
 *   of suppression and extraction. The constraint is a snare because its
 *   primary function was coercive extraction of conformity, with the
 *   coordination story (doctrinal unity) serving as cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.9).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.95).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Created Qur'an Doctrine (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'c45f73dd-8494-4a7c-ba70-bb8c6bace388').
narrative_ontology:cs_kernel_codification('c45f73dd-8494-4a7c-ba70-bb8c6bace388', formalized).
narrative_ontology:cs_authority_grounding('c45f73dd-8494-4a7c-ba70-bb8c6bace388', extraction).
narrative_ontology:cs_interpretation_layer_present('c45f73dd-8494-4a7c-ba70-bb8c6bace388').
narrative_ontology:cs_reading_relation('c45f73dd-8494-4a7c-ba70-bb8c6bace388', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('c45f73dd-8494-4a7c-ba70-bb8c6bace388', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('c45f73dd-8494-4a7c-ba70-bb8c6bace388', foundational, caliph_defines_orthodoxy).
narrative_ontology:cs_axiom_status(caliph_defines_orthodoxy, holdable).
narrative_ontology:cs_axiom_grounding('c45f73dd-8494-4a7c-ba70-bb8c6bace388', caliph_defines_orthodoxy, conventional).
narrative_ontology:cs_axiom('c45f73dd-8494-4a7c-ba70-bb8c6bace388', foundational, quran_is_created).
narrative_ontology:cs_axiom_status(quran_is_created, holdable).
narrative_ontology:cs_axiom_grounding('c45f73dd-8494-4a7c-ba70-bb8c6bace388', quran_is_created, deontological).
narrative_ontology:cs_reference_frame('c45f73dd-8494-4a7c-ba70-bb8c6bace388', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('c45f73dd-8494-4a7c-ba70-bb8c6bace388', post_mihna_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c45f73dd-8494-4a7c-ba70-bb8c6bace388', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Abbasid Caliphs (al-Ma'mun, al-Mu'tasim, al-Wathiq) who initiated and enforced the Mihna. They used the doctrine of the created Qur'an to assert their authority in religious matters and to purge dissenting scholars, consolidating political and doctrinal control.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% The rationalist theological school whose doctrine of the created Qur'an was adopted and enforced by the state. They gained temporary political backing and influence, seeing their theological position elevated to state dogma, and their rivals suppressed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars, beneficiary,
    powerful, biographical, mobile, regional).

% Scholars like Ahmad ibn Hanbal who upheld the doctrine of the uncreated Qur'an. They faced imprisonment, torture, and public humiliation for refusing to affirm the state-mandated doctrine. Their careers and lives were directly threatened by the Mihna.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    powerless, biographical, trapped, regional).

% Communities and laypeople who adhered to the traditionalist view of the Qur'an. They faced pressure to conform, saw their religious leaders persecuted, and experienced a chilling effect on open theological discourse.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    moderate, generational, constrained, local).

% The general intellectual environment that allowed for diverse theological interpretations. It was suppressed by the state's enforcement of a single doctrine, leading to a narrowing of acceptable discourse and a chilling effect on independent thought.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate theological understanding under a single, state-sanctioned doctrine, thereby unifying religious and political authority under the Caliph.
% TRANSFER_FUNCTION: Transferred doctrinal legitimacy and political power from independent religious scholars to the Caliphate, and from traditionalist schools to the Mu'tazilite rationalists. It extracted conformity and intellectual freedom from dissenting scholars.
% ABSENT_VOICES: Any nascent movements for religious freedom or separation of religious and political authority were entirely absent and suppressed. Their arguments for intellectual autonomy would have been met with severe state repression.
% DISAPPEARANCE_RATIONALE: If the Mihna and its enforcement vanished, the theological landscape would immediately diversify, traditionalist schools would regain prominence, and the Caliphate's claim to absolute religious authority would be severely undermined. The power dynamics of the era would fundamentally shift.
% FOUNDING_PROBLEM: The Caliphate faced challenges to its religious authority from independent scholars and sought to consolidate power by asserting its right to define orthodox doctrine, particularly against traditionalist interpretations that limited caliphal religious prerogatives.
% FOUNDING_PROBLEM_CORROBORATION: The Caliphate's own decrees and historical accounts from both Mu'tazilite and traditionalist sources attest to the Caliph's desire for doctrinal control as a means of political consolidation. Historians and political theorists outside the immediate beneficiaries corroborate this reading of the founding problem as a power struggle.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.9) because the state directly extracted intellectual freedom and physical well-being from dissenting scholars. Suppression is extremely high (0.95) due to the use of state-sanctioned inquisition, imprisonment, and torture to enforce doctrinal conformity. Theater ratio is low (0.1) because the enforcement was brutally real, not performative; the 'coordination' of theological unity was a direct consequence of coercion, not voluntary alignment. The metrics reflect the historical reality of the Mihna.
 *
 * PERSPECTIVAL GAP:
 *   From the Caliphate's perspective, this was a necessary act of governance to unify the Ummah and assert legitimate religious authority. From the perspective of traditionalist scholars, it was an unjust and tyrannical imposition of a heterodox doctrine, a pure act of extraction and suppression. The engine's classification as a snare from the victim's seat captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority and Mu'tazilite scholars are clear beneficiaries, gaining political and doctrinal ascendancy. Traditionalist scholars and literalist communities are direct targets, bearing the full brunt of the state's coercive power. Scholarly pluralism, as an abstract good, is also a victim, as its very existence was suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Was the Mihna primarily a theological dispute that escalated to state enforcement, or a political power play that instrumentalized a theological doctrine?',
    'Detailed historical analysis of caliphal motivations, internal court politics, and the timing of the Mihna relative to other political challenges to Abbasid authority.',
    'If primarily political, the extractiveness and suppression are even more clearly a function of state power, reinforcing the snare classification. If primarily theological, it highlights the danger of doctrinal disputes being co-opted by state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Ambiguity of political vs. theological drivers of the Mihna.').

omega_variable(
    long_term_doctrinal_impact,
    'Did the Mihna''s enforcement of the created Qur''an doctrine have a lasting impact on Islamic theology, or was its influence primarily political and temporary?',
    'Analysis of post-Mihna theological developments, the eventual decline of Mu''tazilism, and the resurgence of traditionalist schools (e.g., Ash''arism).',
    'If the doctrinal impact was temporary, it underscores the fragility of state-enforced orthodoxy against deeply held religious beliefs. If lasting, it suggests a more profound, albeit coercive, reshaping of theological discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_doctrinal_impact, empirical, 'The long-term theological legacy of the Mihna''s enforced doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 847).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.7).
narrative_ontology:measurement(qura_be_t837, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 837, 0.8).
narrative_ontology:measurement(qura_be_t840, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 840, 0.85).
narrative_ontology:measurement(qura_be_t843, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 843, 0.88).
narrative_ontology:measurement(qura_be_t847, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 847, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.75).
narrative_ontology:measurement(qura_su_t837, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 837, 0.85).
narrative_ontology:measurement(qura_su_t840, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 840, 0.9).
narrative_ontology:measurement(qura_su_t843, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 843, 0.93).
narrative_ontology:measurement(qura_su_t847, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 847, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, islamic_scholarly_pluralism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, caliphal_religious_authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
