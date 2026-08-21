% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami and Buddhas Govern Separate Domains (Domain Partition Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'domain partition' reading of the
 *   Shinbutsu ontological substrate kernel. This reading posits that kami and
 *   buddhas govern separate, non-overlapping domains (this-world vs.
 *   afterlife), and their coexistence is functional and pragmatic, not based
 *   on an underlying ontological unity. This perspective emphasizes the
 *   distinct institutional and ritual practices of Shinto and Buddhism, and
 *   views syncretism as a pragmatic arrangement rather than a metaphysical
 *   fusion. The metrics reflect a functional coordination with moderate
 *   enforcement to maintain boundaries, rather than high extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.35).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.45).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami and Buddhas Govern Separate Domains (Domain Partition Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '00be3bf4-1811-40a6-8c2d-4898ec7c7d34').
narrative_ontology:cs_kernel_codification('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', formalized).
narrative_ontology:cs_authority_grounding('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', lineage).
narrative_ontology:cs_interpretation_layer_present('00be3bf4-1811-40a6-8c2d-4898ec7c7d34').
narrative_ontology:cs_reading_relation('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', foundational, kami_buddha_domain_separation).
narrative_ontology:cs_axiom_status(kami_buddha_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', kami_buddha_domain_separation, conventional).
narrative_ontology:cs_axiom('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', foundational, functional_coexistence_over_ontological_unity).
narrative_ontology:cs_axiom_status(functional_coexistence_over_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', functional_coexistence_over_ontological_unity, conventional).
narrative_ontology:cs_reference_frame('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', meiji_shinbutsu_bunri_edict).
narrative_ontology:cs_drift_state('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', post_world_war_ii_secularization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('00be3bf4-1811-40a6-8c2d-4898ec7c7d34', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, kami_worshippers).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the domain of kami worship, benefiting from clear boundaries and institutional autonomy. They actively define and maintain the separation from Buddhist practices.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Administers the domain of Buddhist practice, benefiting from clear boundaries and institutional autonomy. They define and maintain the separation from Shinto practices.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Benefit from clear ritual and spiritual guidance, knowing which practices pertain to kami and which to buddhas, reducing confusion and ensuring 'pure' worship.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, kami_worshippers, beneficiary,
    moderate, biographical, mobile, local).

% Benefit from clear ritual and spiritual guidance, knowing which practices pertain to buddhas and which to kami, reducing confusion and ensuring 'pure' practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_practitioners, beneficiary,
    moderate, biographical, mobile, local).

% Historically enforced the separation (e.g., during Shinbutsu-bunri), and for this reading, they observe and sometimes mediate the functional coexistence, ensuring public order and distinct institutional roles.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_authorities, observer,
    institutional, generational, analytical, national).

% Their practices, which often blend kami and Buddhist elements, are deemed 'impure' or 'confused' by this reading. They are excluded from the official discourse of distinct domains and may face social or institutional pressure to conform to the partition.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_practitioners, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides clear, non-overlapping spiritual and ritual domains for kami and buddhas, allowing distinct religious practices and institutional structures to flourish without ontological conflict.
% TRANSFER_FUNCTION: Transfers clarity of religious identity and institutional autonomy to Shinto and Buddhist institutions and practitioners, by partitioning spiritual authority and ritual spaces.
% ABSENT_VOICES: Syncretic practitioners who experience kami and buddhas as unified or fluid, and scholars who argue for an incoherent historical development, would challenge the neat domain partition, but are marginalized by this reading's framework.
% DISAPPEARANCE_RATIONALE: If the clear domain partition and its enforcement vanished overnight, the distinct institutional structures and ritual practices of Shinto and Buddhism would lose their foundational justification, leading to confusion, competition, and a need to redefine their relationship, potentially reverting to earlier forms of fusion or conflict.
% FOUNDING_PROBLEM: To reconcile the presence of indigenous kami worship with the introduction of Buddhism, preventing conflict and allowing both traditions to thrive by assigning them distinct, non-overlapping spheres of influence (this-world vs. afterlife).
% FOUNDING_PROBLEM_CORROBORATION: Historical records of early attempts to integrate or distinguish kami and Buddhist practices, and ongoing theological discussions within both traditions that seek to maintain distinct identities, corroborate the problem. The Meiji-era Shinbutsu-bunri edicts also attest to the perceived need for such a partition, even if its implementation was coercive.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) and suppression (0.45) are moderate, reflecting the active enforcement required to maintain distinct religious domains, particularly during the Meiji era's Shinbutsu-bunri (separation of kami and buddhas). While the reading emphasizes functional coexistence, the historical implementation involved state-backed coercion against syncretic practices. The theater ratio is low (0.20) because the functional separation was a genuine goal, not merely performative. Accessibility collapse is moderate (0.40) as alternative syncretic practices were constrained but not entirely eliminated. Resistance is also moderate (0.30) from those who preferred fusion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Shinto and Buddhist institutions, this arrangement is a functional rope, providing necessary coordination for distinct religious identities. From the perspective of syncretic practitioners, it is a more extractive constraint, suppressing their integrated worldview and practices. The engine will compute this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto and Buddhist institutions, along with their respective worshippers, are beneficiaries, gaining clarity and autonomy within their defined domains. Syncretic practitioners are victims, as their blended practices are suppressed or excluded by this framework. State authorities act as observers, historically enforcing the partition and mediating its functional aspects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_distinction,
    'Is the separation between kami and buddhas truly functional and pragmatic, or does it reflect an underlying ontological distinction?',
    'Comparative theological analysis across different historical periods and philosophical schools within Japan, examining whether the distinction is consistently maintained at a metaphysical level or primarily at a practical/institutional one.',
    'If the distinction is found to be primarily ontological, this reading''s ''rope'' classification might shift towards ''mountain'' (as a natural division). If it''s purely functional, the ''rope'' classification is reinforced, emphasizing human coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_distinction, conceptual, 'Ambiguity regarding the nature of the kami-buddha separation.').

omega_variable(
    coherence_of_the_partition,
    'Is the ''domain partition'' a genuinely coherent framework, or is it an imposed, artificial construct that masks an underlying incoherence?',
    'Historical and sociological analysis of the actual lived religious experience of practitioners, comparing it against the idealized partition. Evidence of widespread, persistent syncretic practices despite enforcement would challenge the coherence.',
    'If the partition is found to be largely incoherent in practice, this reading''s classification might shift towards ''snare'' or ''tangled_rope'', reflecting the coercive maintenance of an artificial distinction. If coherent, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_of_the_partition, empirical, 'Whether the domain partition is a coherent framework or an artificial construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 1868, 1918).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1868, 0.18).
narrative_ontology:measurement(shin_tr_t1878, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1878, 0.19).
narrative_ontology:measurement(shin_tr_t1888, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1888, 0.19).
narrative_ontology:measurement(shin_tr_t1898, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1898, 0.2).
narrative_ontology:measurement(shin_tr_t1908, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1908, 0.2).
narrative_ontology:measurement(shin_tr_t1918, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1918, 0.2).

% Extraction over time
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1868, 0.3).
narrative_ontology:measurement(shin_be_t1878, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1878, 0.32).
narrative_ontology:measurement(shin_be_t1888, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1888, 0.33).
narrative_ontology:measurement(shin_be_t1898, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1898, 0.34).
narrative_ontology:measurement(shin_be_t1908, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1908, 0.34).
narrative_ontology:measurement(shin_be_t1918, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1918, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1868, 0.4).
narrative_ontology:measurement(shin_su_t1878, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1878, 0.42).
narrative_ontology:measurement(shin_su_t1888, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1888, 0.43).
narrative_ontology:measurement(shin_su_t1898, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1898, 0.44).
narrative_ontology:measurement(shin_su_t1908, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1908, 0.44).
narrative_ontology:measurement(shin_su_t1918, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1918, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_ontological_substrate' kernel, focusing on the functional separation of kami and buddhas. It is linked to sibling readings that propose ontological fusion or an incoherent bundle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
