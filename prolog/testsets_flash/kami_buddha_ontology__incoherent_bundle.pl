% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as an Incoherent Institutional Bundle
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint models Shinbutsu-shugo not as a coherent theological
 *   system, but as an institutionally sustained bundle of contradictory
 *   commitments. It simultaneously fuses and separates kami and buddhas,
 *   operates with both hierarchical and reciprocal relationships, and remains
 *   systematized in practice while resisting full theoretical
 *   systematization. This 'incoherent bundle' is maintained by religious
 *   institutions due to its practical efficacy and broad appeal, despite the
 *   theoretical costs borne by those seeking coherence. The constraint's
 *   persistence relies on active enforcement of this flexible, contradictory
 *   framework, suppressing attempts at rigid definition or complete
 *   separation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.6).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.7).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as an Incoherent Institutional Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '4f8fc815-beff-4e48-b036-607c5a633964').
narrative_ontology:cs_kernel_codification('4f8fc815-beff-4e48-b036-607c5a633964', distributed).
narrative_ontology:cs_authority_grounding('4f8fc815-beff-4e48-b036-607c5a633964', practice).
narrative_ontology:cs_interpretation_layer_present('4f8fc815-beff-4e48-b036-607c5a633964').
narrative_ontology:cs_reading_relation('4f8fc815-beff-4e48-b036-607c5a633964', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('4f8fc815-beff-4e48-b036-607c5a633964', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('4f8fc815-beff-4e48-b036-607c5a633964', foundational, ontological_contradiction_is_functional).
narrative_ontology:cs_axiom_status(ontological_contradiction_is_functional, holdable).
narrative_ontology:cs_axiom_grounding('4f8fc815-beff-4e48-b036-607c5a633964', ontological_contradiction_is_functional, conventional).
narrative_ontology:cs_axiom('4f8fc815-beff-4e48-b036-607c5a633964', secondary, institutional_flexibility_trumps_coherence).
narrative_ontology:cs_axiom_status(institutional_flexibility_trumps_coherence, holdable).
narrative_ontology:cs_axiom_grounding('4f8fc815-beff-4e48-b036-607c5a633964', institutional_flexibility_trumps_coherence, instrumental).
narrative_ontology:cs_reference_frame('4f8fc815-beff-4e48-b036-607c5a633964', pre_meiji_syncretic_practice).
narrative_ontology:cs_drift_state('4f8fc815-beff-4e48-b036-607c5a633964', contemporary_academic_analysis, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4f8fc815-beff-4e48-b036-607c5a633964', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shinto_buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, religious_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theological_coherence_seekers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, state_shinto_ideologues).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions historically managed the fusion and separation of kami and buddhas, benefiting from the flexibility and broad appeal of the syncretic system. They actively maintain the contradictory practices and narratives, as attempts to enforce strict coherence or separation have historically led to instability and loss of influence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinto_buddhist_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from a flexible religious framework that accommodates diverse spiritual needs and ritual practices, allowing them to engage with both kami and buddhas without strict ontological commitments. Their practical efficacy is prioritized over theoretical consistency.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Scholars and practitioners who seek a consistent, unified theological understanding of kami and buddhas find their efforts frustrated by the inherent contradictions of Shinbutsu-shugo. They bear the cost of intellectual dissonance and the inability to construct a stable, coherent ontology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theological_coherence_seekers, payer,
    powerless, biographical, identity_locked, global).

% Historically attempted to impose a strict separation of Shinto and Buddhism (Shinbutsu-bunri) to elevate Shinto as the national religion. They found the deeply ingrained, incoherent bundle of Shinbutsu-shugo resistant to their efforts, ultimately failing to fully eradicate its influence, bearing the cost of an unachieved ideological purity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, state_shinto_ideologues, payer,
    powerful, generational, constrained, national).

% Academics and philosophers who analyze the historical and structural dynamics of Shinbutsu-shugo, identifying its inherent contradictions and the institutional mechanisms that sustain them. They are outside the system of belief and practice, seeking to understand its operation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse religious practices and beliefs within a single cultural framework, allowing for flexible engagement with both indigenous kami and imported Buddhist deities, thereby maintaining social cohesion and institutional stability across different spiritual needs.
% TRANSFER_FUNCTION: Transfers the burden of ontological consistency from religious institutions and practitioners to an implicit acceptance of contradiction, in exchange for practical efficacy and broad spiritual accessibility. It also transfers cultural capital and institutional longevity to the religious establishments that manage this bundle.
% ABSENT_VOICES: Strict monotheists or philosophical purists who demand absolute ontological consistency would object to the sustained contradictions, but their frameworks are external to the historical development and institutional logic of Shinbutsu-shugo in Japan.
% DISAPPEARANCE_RATIONALE: If the institutional mechanisms sustaining the incoherent bundle of Shinbutsu-shugo vanished, the complex interplay of Shinto and Buddhist practices, rituals, and beliefs that define much of Japanese religious life would collapse. Temples and shrines would face an existential crisis of identity, and practitioners would lose a flexible framework for spiritual engagement, leading to a profound reorganization of religious and cultural landscape.
% FOUNDING_PROBLEM: The historical encounter between indigenous Japanese kami worship and imported Buddhism created a need to integrate or reconcile two distinct religious systems to avoid conflict and maximize spiritual benefit for the populace.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion and cultural anthropologists corroborate that the challenge of integrating diverse spiritual traditions remains a live, ongoing process, even if the specific historical forms of Shinbutsu-shugo have evolved. The need for practical religious synthesis, rather than strict ontological purity, continues to shape religious life, as attested by ongoing syncretic practices and popular religious attitudes.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost of intellectual dissonance and the suppression of alternative, more coherent theological frameworks. Suppression (0.7) is high because the institutional power of Shinto-Buddhist establishments actively resists attempts to dismantle or rigidly define the syncretic bundle, as evidenced by historical failures of Shinbutsu-bunri (separation). The theater ratio (0.4) indicates that while there's genuine religious function, a significant portion of institutional activity is dedicated to managing and performing the contradictions rather than resolving them. Accessibility collapse is moderate (0.45) because while alternatives for coherent theology are suppressed, practical religious engagement remains accessible.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Shinto-Buddhist institutions, the incoherent bundle is a successful, adaptive strategy for religious and cultural continuity. From the perspective of theological coherence seekers, it is a source of intellectual frustration and an obstacle to deeper understanding. The engine will compute these divergent experiences based on the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto-Buddhist institutions and religious practitioners are beneficiaries, as the incoherent bundle provides them with flexibility, broad appeal, and institutional stability. Theological coherence seekers and state Shinto ideologues are victims, as their efforts to impose consistency or separation are frustrated by the constraint's persistence. The institutional power of the agenda-setters (Shinto-Buddhist institutions) ensures the bundle's survival, making exit difficult for those who desire a different religious structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_theological_coherence,
    'To what extent is the ''incoherence'' a feature of institutional strategy versus a genuine theological impossibility?',
    'Comparative study of other syncretic traditions that achieved greater theological coherence, or analysis of internal debates within Japanese religious history that attempted systematization.',
    'If primarily institutional, the constraint is more extractive, as it actively suppresses theological development for institutional gain. If genuinely theological, the constraint is closer to a mountain, reflecting an irreducible complexity of the subject matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_theological_coherence, conceptual, 'Distinguishing between strategic and inherent incoherence.').

omega_variable(
    practical_efficacy_vs_ontological_truth,
    'Does the practical efficacy of Shinbutsu-shugo (e.g., ritual success, social cohesion) justify its ontological contradictions, or does the lack of ontological truth undermine its long-term legitimacy?',
    'Longitudinal studies of religious adherence and cultural impact, particularly during periods of external challenge or internal reform movements, to see if practical benefits outweigh theoretical costs over time.',
    'If practical efficacy is the dominant driver, the constraint''s ''rope'' aspects are stronger. If ontological truth becomes a critical factor for legitimacy, the ''snare'' aspects (extraction of intellectual honesty) are amplified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practical_efficacy_vs_ontological_truth, preference, 'Weighing practical benefits against theoretical consistency.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of coherent theological alternatives structural (institutional power, historical precedent) or internalized (practitioners'' comfort with ambiguity, cultural conditioning)?',
    'Analysis of post-Shinbutsu-bunri (Meiji era separation) theological developments: if coherent systems emerged rapidly, suppression was structural; if ambiguity persisted, it was partly internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the ''incoherent bundle'' more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__incoherent_bundle, theater_ratio, 300, 0.25).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.3).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__incoherent_bundle, theater_ratio, 900, 0.35).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(kami_tr_t1500, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1500, 0.4).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 300, 0.48).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 900, 0.6).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement(kami_be_t1500, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1500, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 900, 0.7).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(kami_su_t1500, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1500, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kami_buddha_ontology' kernel, focusing on its nature as an institutionally sustained bundle of contradictions. It is linked to sibling readings that propose alternative coherent interpretations (honji_suijaku_monism, domain_partition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
