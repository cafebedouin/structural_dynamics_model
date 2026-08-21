% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shūgō as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint represents the 'incoherence reading' of shinbutsu-shūgō,
 *   arguing that prior to the Meiji Restoration, there was no stable, unified
 *   ontological commitment integrating Shinto and Buddhism. Instead, their
 *   coexistence was a pragmatic, institutionally tolerated incoherence. This
 *   reading highlights how the lack of a coherent philosophical foundation
 *   made the separation of Shinto and Buddhism (Shinbutsu Bunri) by the Meiji
 *   state relatively easy, facilitating state-building and the creation of
 *   State Shinto. The constraint is classified as a Tangled Rope because it
 *   provided a coordination function (local religious coexistence) but also
 *   enabled asymmetric extraction (Meiji state's consolidation of power by
 *   dismantling syncretic institutions).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.65).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shūgō as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '04d97842-923e-4d27-a64f-a40a58c2ebb4').
narrative_ontology:cs_kernel_codification('04d97842-923e-4d27-a64f-a40a58c2ebb4', distributed).
narrative_ontology:cs_authority_grounding('04d97842-923e-4d27-a64f-a40a58c2ebb4', extraction).
narrative_ontology:cs_interpretation_layer_present('04d97842-923e-4d27-a64f-a40a58c2ebb4').
narrative_ontology:cs_reading_relation('04d97842-923e-4d27-a64f-a40a58c2ebb4', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('04d97842-923e-4d27-a64f-a40a58c2ebb4', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('04d97842-923e-4d27-a64f-a40a58c2ebb4', foundational, no_unified_ontological_framework).
narrative_ontology:cs_axiom_status(no_unified_ontological_framework, holdable).
narrative_ontology:cs_axiom_grounding('04d97842-923e-4d27-a64f-a40a58c2ebb4', no_unified_ontological_framework, empirically_contingent).
narrative_ontology:cs_axiom('04d97842-923e-4d27-a64f-a40a58c2ebb4', secondary, pragmatic_coexistence_not_integration).
narrative_ontology:cs_axiom_status(pragmatic_coexistence_not_integration, holdable).
narrative_ontology:cs_axiom_grounding('04d97842-923e-4d27-a64f-a40a58c2ebb4', pragmatic_coexistence_not_integration, empirically_contingent).
narrative_ontology:cs_reference_frame('04d97842-923e-4d27-a64f-a40a58c2ebb4', pre_meiji_ontological_ambiguity).
narrative_ontology:cs_drift_state('04d97842-923e-4d27-a64f-a40a58c2ebb4', meiji_restoration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('04d97842-923e-4d27-a64f-a40a58c2ebb4', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shinto_priests_post_meiji).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutions_pre_meiji).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, local_communities_pre_meiji).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively sought to establish a unified national identity and state religion. They benefited from the perceived ontological incoherence of shinbutsu-shūgō, as it allowed for a 'clean' separation and elevation of Shinto, facilitating state control and ideological consolidation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the state-mandated separation of Shinto and Buddhism, gaining institutional prominence, state support, and a clearer, elevated role in national rituals. Their previous syncretic practices were often suppressed or reinterpreted.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shinto_priests_post_meiji, beneficiary,
    organized, biographical, mobile, national).

% Suffered significant losses of land, property, and influence during the forced separation. Their syncretic practices, which often integrated Shinto elements, were dismantled, leading to a decline in their traditional roles and economic base.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutions_pre_meiji, payer,
    powerful, generational, constrained, local).

% Experienced the forced separation as a disruption of long-standing religious practices and community identity. Their local shrines and temples, often shared or integrated, were forcibly separated, leading to confusion, loss of cultural heritage, and imposition of new, state-sanctioned religious norms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_communities_pre_meiji, payer,
    powerless, biographical, trapped, local).

% Analyze the historical and philosophical underpinnings of shinbutsu-shūgō, often highlighting the lack of a consistent, unified ontological framework prior to the Meiji separation. They observe the political utility of this 'incoherence' for state-building.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, scholarly_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prior to Meiji, it allowed diverse local religious practices to coexist under a broad, flexible framework without requiring strict doctrinal consistency, facilitating social cohesion at a local level.
% TRANSFER_FUNCTION: Post-Meiji, it transferred religious authority and resources from syncretic Buddhist-Shinto institutions to a state-controlled Shinto establishment, consolidating ideological power for the Meiji state.
% ABSENT_VOICES: Local religious practitioners and communities whose syncretic traditions were forcibly dismantled by the Meiji state. Their voices, rooted in lived practice rather than abstract ontology, were suppressed in favor of a state-imposed narrative of 'pure' Shinto.
% DISAPPEARANCE_RATIONALE: If the historical 'incoherence' of shinbutsu-shūgō had been universally recognized and acted upon earlier, the Meiji state's religious policies might have taken a different form, or faced greater resistance. The ease of separation, predicated on this perceived incoherence, fundamentally reshaped Japanese religious and political landscape.
% FOUNDING_PROBLEM: The Meiji state faced the problem of consolidating national identity and centralizing power, which required a unified ideological foundation, free from perceived foreign (Buddhist) influences.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists corroborate that the problem of national unity and ideological control was central to the Meiji Restoration. The 'incoherence' reading provides a structural explanation for the relative ease with which the state could implement its separation policies, as there was no deeply entrenched, unified ontological commitment to resist.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the Meiji state leveraged this perceived incoherence to dismantle existing religious structures and seize assets, imposing a new, state-centric religious order. Suppression is also high, reflecting the active enforcement of Shinbutsu Bunri, including violence against Buddhist institutions. Theater ratio is moderate, as the state's narrative of 'restoring' pure Shinto had a performative aspect, masking the political utility of the separation. The rising extractiveness and suppression over the interval reflect the intensification of state policies during the Meiji era.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Meiji state, the 'incoherence' was a problem to be solved, and the separation a necessary act of purification and modernization. From the perspective of local communities, the 'incoherence' was a lived reality of flexible, integrated practice, and the separation a violent disruption. The engine's classification captures the extractive nature of the state's intervention, which leveraged the perceived ontological instability.
 *
 * DIRECTIONALITY LOGIC:
 *   The Meiji state builders and post-Meiji Shinto priests are beneficiaries, gaining power and resources from the separation. Pre-Meiji Buddhist institutions and local communities are victims, bearing the costs of forced dismantling and disruption of traditional practices. Scholarly observers analyze the historical dynamics without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_for_incoherence,
    'To what extent does historical and textual evidence definitively prove a lack of stable ontological commitment, versus a flexible, context-dependent understanding that appears incoherent to modern analytical frameworks?',
    'Further interdisciplinary research integrating textual analysis, archaeological findings, and anthropological studies of local religious practices to reconstruct pre-Meiji understandings.',
    'Stronger evidence for genuine incoherence would bolster this reading''s claim that separation was structurally ''easy.'' Evidence for a coherent, albeit flexible, syncretism would weaken this reading and strengthen the ''syncretic reading,'' suggesting the Meiji separation was a more forceful imposition against an integrated system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_for_incoherence, empirical, 'The degree to which pre-Meiji shinbutsu-shūgō was truly ontologically incoherent.').

omega_variable(
    political_utility_vs_philosophical_truth,
    'Was the ''incoherence'' primarily a philosophical observation, or a politically useful narrative constructed by the Meiji state to justify its policies?',
    'Analysis of Meiji-era propaganda and official documents, comparing their portrayal of shinbutsu-shūgō with contemporary scholarly and popular understandings.',
    'If primarily a political construction, the extractiveness and suppression metrics would be more strongly attributed to state opportunism rather than a ''natural'' consequence of existing ontological instability. This would shift the classification closer to a pure Snare, as the coordination story (resolving incoherence) would be revealed as cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_utility_vs_philosophical_truth, conceptual, 'The role of political utility in framing shinbutsu-shūgō as incoherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1868, 1912).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.2).
narrative_ontology:measurement(shin_tr_t1878, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1878, 0.3).
narrative_ontology:measurement(shin_tr_t1888, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1888, 0.35).
narrative_ontology:measurement(shin_tr_t1898, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1898, 0.38).
narrative_ontology:measurement(shin_tr_t1908, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1908, 0.39).
narrative_ontology:measurement(shin_tr_t1912, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1912, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.4).
narrative_ontology:measurement(shin_be_t1878, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1878, 0.55).
narrative_ontology:measurement(shin_be_t1888, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1888, 0.6).
narrative_ontology:measurement(shin_be_t1898, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1898, 0.63).
narrative_ontology:measurement(shin_be_t1908, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1908, 0.64).
narrative_ontology:measurement(shin_be_t1912, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1912, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.5).
narrative_ontology:measurement(shin_su_t1878, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1878, 0.65).
narrative_ontology:measurement(shin_su_t1888, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1888, 0.68).
narrative_ontology:measurement(shin_su_t1898, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1898, 0.69).
narrative_ontology:measurement(shin_su_t1908, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1908, 0.7).
narrative_ontology:measurement(shin_su_t1912, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1912, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, state_shinto_ideological_control).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, buddhist_sectarian_fragmentation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
