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
 *   domain: islamic_theology/political_authority/philosophy_of_language
 *
 * SUMMARY:
 *   This constraint describes the historical period of the Mihna (833-848 CE)
 *   during the Abbasid Caliphate, where the Mu'tazilite doctrine of the
 *   Qur'an's createdness was enforced by the state through an inquisition.
 *   Scholars who refused to affirm this doctrine were persecuted. This is one
 *   reading of the broader 'quran_ontological_status' kernel, focusing on the
 *   state's coercive intervention. The other readings (uncreated_reading,
 *   created_reading) describe the theological claims without state
 *   enforcement.
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
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority/philosophy_of_language").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '5715a933-3f9b-4ecd-b163-e13b96dc7fcf').
narrative_ontology:cs_kernel_codification('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', formalized).
narrative_ontology:cs_authority_grounding('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', extraction).
narrative_ontology:cs_interpretation_layer_present('5715a933-3f9b-4ecd-b163-e13b96dc7fcf').
narrative_ontology:cs_reading_relation('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', foundational, caliph_as_religious_arbiter).
narrative_ontology:cs_axiom_status(caliph_as_religious_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', caliph_as_religious_arbiter, conventional).
narrative_ontology:cs_axiom('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', foundational, quran_is_created_divine_speech).
narrative_ontology:cs_axiom_status(quran_is_created_divine_speech, holdable).
narrative_ontology:cs_axiom_grounding('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', quran_is_created_divine_speech, deontological).
narrative_ontology:cs_reference_frame('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', post_mihna_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5715a933-3f9b-4ecd-b163-e13b96dc7fcf', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Abbasid Caliphs (al-Ma'mun, al-Mu'tasim, al-Wathiq) who initiated and enforced the Mihna. They used the doctrine of the created Qur'an to assert their authority in religious matters, consolidate power, and suppress dissent, viewing doctrinal control as a political tool.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% The rationalist theological school whose doctrine of the created Qur'an was elevated to state dogma. They benefited from caliphal backing, gaining institutional power and suppressing rival schools of thought, though their influence waned after the Mihna's end.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_scholars, beneficiary,
    powerful, biographical, mobile, regional).

% Scholars like Ahmad ibn Hanbal who upheld the traditional view of the Qur'an as uncreated. They were subjected to inquisition (mihna), imprisonment, torture, and public humiliation for refusing to affirm the state-mandated doctrine. Their careers and lives were at risk.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    powerless, biographical, trapped, regional).

% Communities and laypeople who adhered to the traditional, literalist understanding of the Qur'an. They faced pressure to conform, saw their respected scholars persecuted, and experienced a chilling effect on religious discourse. Their options were limited to quiet dissent or outward compliance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    moderate, generational, constrained, local).

% The broader intellectual environment that allowed for diverse theological interpretations. The Mihna actively suppressed this pluralism, forcing a single doctrine and punishing deviation, thereby narrowing the scope of legitimate scholarly inquiry and debate.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    powerless, generational, identity_locked, regional).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint aimed to coordinate theological understanding within the Islamic empire under a single, state-sanctioned doctrine, thereby asserting caliphal authority over religious interpretation and reducing internal theological disputes.
% TRANSFER_FUNCTION: Transferred religious legitimacy and interpretive authority from independent scholars and diverse schools of thought to the caliphal state and its favored Mu'tazilite theologians. It also transferred suffering and persecution to dissenting scholars.
% ABSENT_VOICES: The voices of future generations of Islamic scholars who would later reject the Mihna's methods and re-establish a broader theological discourse were absent. Also, the voices of those who believed in a purely spiritual or non-coercive approach to theological disputes were suppressed.
% DISAPPEARANCE_RATIONALE: If the Mihna and the state's enforcement of the created Qur'an doctrine had vanished overnight, the theological landscape would have remained pluralistic, traditionalist scholars would not have been persecuted, and the caliphate's attempt to control religious dogma would have failed, leading to a different trajectory for Islamic intellectual history.
% FOUNDING_PROBLEM: The caliphate faced challenges to its religious authority and sought to unify theological discourse under a rationalist framework, believing it would strengthen the state and align Islam with Greek philosophy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and later Islamic scholars (e.g., al-Ash'ari, Ibn Taymiyyah) attest that the caliphate's attempt to impose a single doctrine ultimately failed, and the problem of theological unity was not resolved by coercion. The Mihna is widely viewed as a historical aberration, not a successful resolution to a live problem.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.9) because the constraint directly imposed severe costs (imprisonment, torture, death) on those who dissented from the state-mandated doctrine. Suppression is also extremely high (0.95) due to the direct use of state power, inquisition tribunals, and the lack of any safe exit for dissenting scholars. Theater ratio is low (0.1) because the Mihna was a direct, brutal exercise of power with little performative cover; its function was overtly coercive. Accessibility collapse is high (0.8) as public alternatives to the state doctrine were actively suppressed. Resistance is high (0.7) because many scholars, most famously Ahmad ibn Hanbal, actively resisted despite severe consequences.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the caliphate and Mu'tazilite scholars, the Mihna was a necessary measure to establish religious truth and unity, a form of 'coordination' through rational theology. From the perspective of traditionalist scholars and the broader populace, it was a brutal imposition of power, a snare designed to extract conformity and suppress dissent. The engine's classification as a snare captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority and Mu'tazilite scholars are beneficiaries (d near 0.0) as they gained political and intellectual dominance through the Mihna. Traditionalist scholars and literalist communities are clear targets (d near 1.0) as they bore the full brunt of the state's coercive power. Scholarly pluralism, as a non-agent, is excluded and suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_motivation,
    'To what extent was the Mihna driven by genuine theological conviction (Mu''tazilite doctrine) versus political expediency (caliphal power consolidation)?',
    'Analysis of primary historical sources, caliphal decrees, and Mu''tazilite writings for explicit statements of intent and the balance of theological vs. political justifications.',
    'If primarily theological, the constraint is a more ''pure'' (though still coercive) attempt at doctrinal coordination. If primarily political, the theological claim is a cover story for power extraction, reinforcing its snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_motivation, empirical, 'Ambiguity in the primary motivation for the Mihna.').

omega_variable(
    mihna_as_kernel_reading,
    'Is this constraint a distinct reading of the ''quran_ontological_status'' kernel, or merely an enforcement mechanism for the ''created_reading''?',
    'Conceptual analysis of whether the state enforcement fundamentally alters the nature of the theological claim itself, or merely its propagation. The current framing asserts it creates a distinct constraint.',
    'If merely an enforcement mechanism, this story should be linked to ''created_reading'' via ''influences'' rather than being a distinct reading. If distinct, its unique coercive structure warrants its own classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mihna_as_kernel_reading, conceptual, 'Whether state enforcement constitutes a distinct kernel reading or just a coercive layer.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state power, inquisition) or internalized (fear, self-censorship)?',
    'Post-Mihna scholarly output: if suppression of certain ideas persisted after the Mihna''s formal end, it suggests internalized suppression. Historical accounts of individual scholars'' choices under duress.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the fear and self-censorship carried by scholars after the Mihna''s end would continue to shape discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism during and after the Mihna.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 848).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.15).
narrative_ontology:measurement(qura_tr_t838, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 838, 0.12).
narrative_ontology:measurement(qura_tr_t843, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 843, 0.11).
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.8).
narrative_ontology:measurement(qura_be_t838, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 838, 0.85).
narrative_ontology:measurement(qura_be_t843, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 843, 0.88).
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.85).
narrative_ontology:measurement(qura_su_t838, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 838, 0.9).
narrative_ontology:measurement(qura_su_t843, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 843, 0.93).
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel, focusing on the state's coercive enforcement of the created Qur'an doctrine. It is distinct from the purely theological 'created_reading' and 'uncreated_reading' by its inclusion of state power and inquisition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
