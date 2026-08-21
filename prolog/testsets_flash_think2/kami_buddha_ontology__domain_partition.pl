% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami and Buddha Ontological Domain Partition
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'domain_partition' reading of the
 *   'kami_buddha_ontology' kernel, which asserts that kami and buddhas are
 *   ontologically distinct entities governing separate functional domains:
 *   Shinto for life, purity, and the living, and Buddhism for death,
 *   impurity, and the deceased. This framework provides a clear,
 *   non-conflicting understanding for practitioners and institutions. The
 *   constraint is claimed as a Mountain because, for its adherents, it
 *   represents a fundamental, unchangeable truth about the nature of reality
 *   and religious order, even though it is a human theological construct. The
 *   low extractiveness and suppression reflect its primary function as a
 *   coordinating conceptual framework rather than a coercive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.1).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.15).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.1).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, mountain).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami and Buddha Ontological Domain Partition").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:emerges_naturally(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '9f18310d-b511-4e55-875b-a06a9d2aff18').
narrative_ontology:cs_kernel_codification('9f18310d-b511-4e55-875b-a06a9d2aff18', implicit).
narrative_ontology:cs_authority_grounding('9f18310d-b511-4e55-875b-a06a9d2aff18', practice).
narrative_ontology:cs_interpretation_layer_present('9f18310d-b511-4e55-875b-a06a9d2aff18').
narrative_ontology:cs_reading_relation('9f18310d-b511-4e55-875b-a06a9d2aff18', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('9f18310d-b511-4e55-875b-a06a9d2aff18', kami_buddha_ontology__incoherent_bundle, forecloses).
narrative_ontology:cs_axiom('9f18310d-b511-4e55-875b-a06a9d2aff18', foundational, kami_buddha_ontologically_distinct).
narrative_ontology:cs_axiom_status(kami_buddha_ontologically_distinct, holdable).
narrative_ontology:cs_axiom_grounding('9f18310d-b511-4e55-875b-a06a9d2aff18', kami_buddha_ontologically_distinct, deontological).
narrative_ontology:cs_axiom('9f18310d-b511-4e55-875b-a06a9d2aff18', foundational, functional_domains_separate).
narrative_ontology:cs_axiom_status(functional_domains_separate, holdable).
narrative_ontology:cs_axiom_grounding('9f18310d-b511-4e55-875b-a06a9d2aff18', functional_domains_separate, conventional).
narrative_ontology:cs_reference_frame('9f18310d-b511-4e55-875b-a06a9d2aff18', traditional_functional_separation).
narrative_ontology:cs_drift_state('9f18310d-b511-4e55-875b-a06a9d2aff18', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9f18310d-b511-4e55-875b-a06a9d2aff18', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents who find clarity and meaning in the distinct roles of kami for life and purity, and buddhas for death and impurity. Their religious practice is structured by this understanding.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_practitioners, beneficiary,
    moderate, biographical, constrained, national).

% Adherents who find clarity and meaning in the distinct roles of buddhas for death and impurity, and kami for life and purity. Their religious practice is structured by this understanding.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_practitioners, beneficiary,
    moderate, biographical, constrained, national).

% The institutional body responsible for maintaining Shinto traditions, rituals, and theological understandings. They actively uphold the distinct domain of kami.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).

% The institutional body responsible for maintaining Buddhist traditions, rituals, and theological understandings. They actively uphold the distinct domain of buddhas.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, agenda_setter,
    institutional, generational, identity_locked, national).

% Academics who study the historical and theological development of Shinto and Buddhism in Japan, analyzing the various interpretations of their relationship.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, scholars_of_japanese_religion, observer,
    analytical, generational, analytical, global).

% Individuals whose personal religious practice blends Shinto and Buddhist elements in ways that challenge strict ontological or functional separation. Their perspective is often marginalized by institutional frameworks.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, syncretic_practitioners, excluded,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, diffuse).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear conceptual framework for understanding the distinct roles and domains of kami and buddhas, preventing confusion and conflict in religious practice and ritual responsibilities within Japanese society.
% TRANSFER_FUNCTION: Transfers conceptual clarity and distinct ritual responsibilities to practitioners and institutions, avoiding ontological ambiguity and ensuring smooth functioning of life-cycle and death-related rites.
% ABSENT_VOICES: Syncretic practitioners, folk religion adherents, and some historical scholars who emphasize the historical fusion (shinbutsu-shūgō) rather than strict separation would challenge this domain partition. Their voices are often outside the institutional theological discourse.
% DISAPPEARANCE_RATIONALE: If this ontological distinction vanished, the foundational conceptual framework for Japanese religious life and death rituals would collapse. Religious institutions would face significant reinterpretation challenges, and practitioners would lose a clear guide for understanding the sacred, leading to widespread conceptual and practical reorganization.
% FOUNDING_PROBLEM: To reconcile the distinct origins and practices of indigenous Shinto and imported Buddhism in Japan by establishing a clear, non-conflicting conceptual framework for their coexistence and functional complementarity.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and historical texts document centuries of theological and practical efforts to define the relationship between Shinto and Buddhism, supporting the problem's historical and ongoing relevance in Japanese religious thought and practice. This corroboration comes from outside the immediate benefiting institutions.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, ExtMetricName, E),
    domain_priors:suppression_score(kami_buddha_ontology__domain_partition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kami_buddha_ontology__domain_partition),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.1) and suppression (0.15) reflect that this constraint primarily functions as a conceptual framework for understanding and coordinating religious practice, rather than extracting resources or coercing behavior. The high accessibility_collapse (0.85) and low resistance (0.1) are consistent with a Mountain claim: for adherents, this ontological distinction is a fundamental truth that makes alternative understandings difficult to access or accept, and thus meets little internal resistance. The low theater_ratio (0.05) indicates it is a genuine belief system, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents and institutional leaders, this domain partition is a natural and beneficial ordering of the sacred. From an external analytical perspective, or that of syncretic practitioners, it is a constructed framework that may obscure historical fusion or alternative understandings. The engine's classification will reflect the structural benefits to the declared beneficiaries, while omegas address the constructed nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto and Buddhist practitioners, as well as their respective priesthoods/clergy, are beneficiaries. They gain conceptual clarity and a stable framework for their religious identities and practices. There are no direct victims, as the constraint primarily coordinates understanding. Syncretic practitioners are 'excluded' as their practices challenge the strict separation, but they are not directly 'victimized' by the constraint's operation in the same way a Snare's targets are.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_natural_law_vs_construct,
    'Is this ontological distinction a genuine feature of reality (a theological natural law), or a constructed framework that benefits specific religious institutions by clarifying their domains and preventing conflict?',
    'Comparative theological analysis across different religious traditions and historical studies of the development of this specific doctrine. The presence of identifiable beneficiaries (religious institutions) suggests a constructed element.',
    'If primarily a construct, the ''Mountain'' classification would be re-evaluated towards a ''Rope'' (coordination) or ''Tangled Rope'' (if extraction is identified through institutional power dynamics). If a genuine theological natural law, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_natural_law_vs_construct, conceptual, 'Ambiguity between theological natural law and institutional construct.').

omega_variable(
    coherence_of_domain_partition,
    'Is this domain partition a truly coherent and consistent framework, or does it contain internal contradictions or unresolved tensions, as suggested by the ''incoherent_bundle'' sibling reading?',
    'Detailed philosophical and theological analysis of the internal consistency of the ''domain_partition'' framework, examining how it handles edge cases or historical periods of greater syncretism.',
    'If found to be internally inconsistent or to rely on unacknowledged contradictions, the constraint''s stability and ''Mountain'' classification would be challenged, potentially shifting towards a ''Piton'' (if maintained theatrically) or ''Tangled Rope'' (if inconsistencies are leveraged for institutional benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_of_domain_partition, conceptual, 'Internal coherence of the domain partition claim.').

omega_variable(
    ontological_distinction_vs_identity,
    'Is the ontological distinction between kami and buddhas truly fundamental, or are they ultimately identical or manifestations of a single underlying reality, as posited by the ''honji_suijaku_monism'' sibling reading?',
    'Further theological and philosophical inquiry into the nature of divinity and sacred beings within Japanese religious thought, and comparative studies with other traditions that posit similar relationships between local deities and universal principles.',
    'If a fundamental identity were established, this ''domain_partition'' reading would be superseded, and the ''honji_suijaku_monism'' reading would become the dominant framework, leading to a re-evaluation of the classification based on its structural properties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_distinction_vs_identity, conceptual, 'Fundamental distinction vs. identity of kami and buddhas.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kami_tr_t50, kami_buddha_ontology__domain_partition, theater_ratio, 50, 0.05).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__domain_partition, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(kami_be_t50, kami_buddha_ontology__domain_partition, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__domain_partition, base_extractiveness, 100, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(kami_su_t50, kami_buddha_ontology__domain_partition, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__domain_partition, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
