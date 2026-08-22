% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Domain Partition (Life/Death Functional Split)
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   The domain partition reading presents kami and buddhas as ontologically
 *   distinct entities governing complementary functional domains: Shinto for
 *   life, purity, and the living; Buddhism for death, impurity, and the
 *   deceased. This reading emerged as state policy during the Meiji
 *   Restoration (1868 shinbutsu bunri) but draws on pre-existing practical
 *   complementarity. It is one of three contested readings of the kami-buddha
 *   ontology kernel — the others being honji_suijaku_monism (identity: kami
 *   as traces of buddhas) and incoherent_bundle (no coherent kernel, just
 *   institutionalized contradiction). This reading claims the partition is a
 *   stable coordination solution; its metrics describe a constraint that was
 *   highly extractive and theatrical under state enforcement but has relaxed
 *   into a low-extraction, low-theater arrangement sustained by institutional
 *   inertia and household habit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.28).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.42).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.28).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Domain Partition (Life/Death Functional Split)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, 'b82c4db1-51c5-4980-9279-506a95a148ed').
narrative_ontology:cs_kernel_codification('b82c4db1-51c5-4980-9279-506a95a148ed', implicit).
narrative_ontology:cs_authority_grounding('b82c4db1-51c5-4980-9279-506a95a148ed', practice).
narrative_ontology:cs_interpretation_layer_present('b82c4db1-51c5-4980-9279-506a95a148ed').
narrative_ontology:cs_reading_relation('b82c4db1-51c5-4980-9279-506a95a148ed', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('b82c4db1-51c5-4980-9279-506a95a148ed', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('b82c4db1-51c5-4980-9279-506a95a148ed', foundational, kami_buddha_ontological_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('b82c4db1-51c5-4980-9279-506a95a148ed', kami_buddha_ontological_distinctness, conventional).
narrative_ontology:cs_axiom('b82c4db1-51c5-4980-9279-506a95a148ed', foundational, life_death_ritual_domain_separation).
narrative_ontology:cs_axiom_status(life_death_ritual_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('b82c4db1-51c5-4980-9279-506a95a148ed', life_death_ritual_domain_separation, conventional).
narrative_ontology:cs_reference_frame('b82c4db1-51c5-4980-9279-506a95a148ed', pre_meiji_fluid_syncretism).
narrative_ontology:cs_drift_state('b82c4db1-51c5-4980-9279-506a95a148ed', postwar_religious_freedom, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b82c4db1-51c5-4980-9279-506a95a148ed', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_monastic_orders).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, householder_families).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, mountain_ascetics).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, popular_practitioners).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_complementarity_without_fusion).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, ontological_parallelism_kami_buddha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain authority over life-cycle rituals (birth, marriage, harvest, groundbreaking) and purity rites. Their institutional continuity depends on the domain partition remaining intelligible to parishioners; they do not administer death rites but their ritual calendar is structured by the complementarity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, beneficiary,
    organized, generational, identity_locked, national).

% Control death rituals (funerals, memorial services, ancestor veneration) and impurity management. The partition secures their revenue base and doctrinal distinctiveness; they do not compete for life-cycle rituals but their institutional identity is constituted through the complementary domain.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_monastic_orders, beneficiary,
    institutional, generational, identity_locked, national).

% Navigate both domains pragmatically — Shinto for weddings and births, Buddhism for funerals and ancestors. They benefit from a clear, socially legible division of ritual labor that reduces coordination costs at life transitions. Exit means adopting a single tradition for all rites, which is socially marked as eccentric.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, householder_families, beneficiary,
    moderate, biographical, constrained, local).

% Practice shugendo, which historically fused kami and buddha worship on sacred mountains. The domain partition marginalizes their integrative practice by assigning each element to a separate institutional home; they bear the cost of being rendered incoherent by the very partition that stabilizes the two main traditions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, mountain_ascetics, payer,
    moderate, biographical, constrained, regional).

% Engage in syncretic worship at household altars and local shrines without doctrinal discrimination. The partition imposes a theoretical clarity that does not match their practice; they pay in cognitive dissonance when institutional authorities police boundary violations (e.g., meat taboos at shrines, Buddhist statues in Shinto precincts).
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, popular_practitioners, payer,
    powerless, immediate, trapped, local).

% Implemented the 1868 shinbutsu bunri (separation edicts) to forcibly disentangle the domains, elevating the partition from practical arrangement to state policy. They set the legal and institutional frame that hardened the complementarity into exclusion; their authority derived from the partition's utility for nation-building.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, meiji_state_shinto_architects, agenda_setter,
    institutional, generational, arbitrage, national).

% Analyze the partition as a historically contingent arrangement that stabilized after centuries of fusion (shinbutsu-shugo). They document the institutional interests, state interventions, and popular practices that sustain or contest the domain division. They neither benefit from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, contemporary_religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legible division of ritual labor between two major traditions: Shinto handles purity, life transitions, and this-worldly flourishing; Buddhism handles impurity, death transitions, and other-worldly liberation. This reduces negotiation costs at every life passage and allows each tradition to specialize its institutional infrastructure.
% TRANSFER_FUNCTION: Moves ritual authority, material offerings, and parishioner allegiance across the life/death boundary. Families direct birth/marriage offerings to shrines and funeral/ancestral offerings to temples; the partition ensures these flows do not compete. Mountain ascetics lose integrative offerings that would cross the boundary.
% ABSENT_VOICES: Pre-Meiji shugendo lineages that maintained fusion practices on sacred mountains; their institutional memory was disrupted by forced separation. Also absent: the dead themselves, whose ontological status the partition adjudicates without their consent.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, the ritual economy would reorganize: shrines and temples would compete for the full life-cycle, householders would face coordination costs at every transition, and the institutional identities of both priesthoods would dissolve into a single contested field. The Meiji legal framework that hardens the partition would lose its object.
% FOUNDING_PROBLEM: After centuries of shinbutsu-shugo (fusion), institutional confusion and doctrinal incoherence made it difficult for parishioners to know which tradition to approach for which need. The partition solved the practical coordination problem of ritual access by assigning non-overlapping domains.
% FOUNDING_PROBLEM_CORROBORATION: Meiji state architects attested the partition solved institutional confusion (their rationale for the 1868 separation edicts). Contemporary scholars (Kuroda Toshio, Hardacre Helen) attest the partition was a state-imposed simplification of a more fluid historical reality; mountain ascetic lineages attest the founding problem was manufactured to justify state control over religious institutions.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaked at 0.45 in 1868 when the state forcibly separated merged institutions, destroying shugendo sites and seizing assets — mountain ascetics and popular practitioners bore massive costs. By 1945, post-war religious freedom dismantled state enforcement, dropping extraction to 0.22. The recent rise to 0.28 reflects new institutional competition: shrines and temples now market life-cycle packages competitively, creating mild rent-seeking. Theater ratio tracks the same arc: 0.65 under state Shinto (performative enforcement of a partition the state itself created), falling to 0.15 by 1970 when the partition became habitual, then rising slightly as institutions perform 'traditional purity' for tourism. Suppression requirement mirrors theater: high when the state policed boundaries, low when practice naturalized, rising again as heritage management polices 'authenticity.'
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (priesthoods, households) experience the partition as a low-extraction coordination rope: it works, it is legible, it requires little enforcement. The payer seats (ascetics, popular practitioners) experience it as a mild snare: it erases their practice, it imposes categories that do not fit, it persists because the beneficiaries control the institutions. The engine will compute this divergence from the structural data — the claim (rope) reflects the beneficiary experience; the metrics reflect the aggregate.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priesthood and Buddhist orders are beneficiaries (organized, identity_locked) — they collect ritual fees and maintain institutional distinctiveness through the partition. Householder families are beneficiaries (moderate, constrained) — they gain coordination clarity. Mountain ascetics are payers (moderate, constrained) — their integrative practice is structurally illegible. Popular practitioners are payers (powerless, trapped) — they bear cognitive costs without institutional representation. Meiji architects were agenda_setters (institutional, arbitrage) — they imposed the partition but exited the religious field entirely. Scholars are observers (analytical, analytical). Directionality derives from who controls the boundary vs. who is bounded by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional confusion under fusion) was real but manufactured by the state as a pretext for control. The partition solved it — but the solution outlived the state that imposed it. Today the arrangement persists not because the coordination problem requires it (householders navigate fluidly; popular practice ignores boundaries) but because the institutional beneficiaries (priesthoods) have identity-locked exit: their professional self-concept is constituted by the domain distinction. This is mandatrophy — the mandate (coordination) is dead but the constraint persists via identity lock. The founding_problem_status = contested captures this: state architects say solved, scholars say manufactured, ascetics say never existed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_vs_organic_origin,
    'Is the domain partition an organic folk taxonomy that the Meiji state merely recognized, or a state invention imposed on a fluid syncretic field?',
    'Comparative analysis of pre-Meiji ritual manuals, household registers, and mountain practice records to assess whether life/death domain assignment was already stable before 1868.',
    'If organic, the partition is a genuine coordination rope with low extraction. If state-invented, the Meiji enforcement period is the constraint''s true origin and the current low extraction reflects decay of a snare — reclassification toward piton or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_organic_origin, empirical, 'Whether the partition predates state enforcement or was created by it.').

omega_variable(
    popular_practice_vs_institutional_doctrine,
    'Do householders and local practitioners actually operate within the partition, or do they ignore it while institutions police it?',
    'Ethnographic survey of contemporary ritual behavior: tracking whether families use Shinto for weddings AND Buddhism for funerals as a clean split, or mix elements (e.g., Shinto purification at Buddhist funerals, Buddhist amulets at Shinto shrines).',
    'If popular practice ignores the partition, the constraint''s extraction is primarily institutional (priesthoods policing boundaries) not coordinative (households using boundaries) — supports higher extractiveness for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_practice_vs_institutional_doctrine, empirical, 'Gap between institutional doctrine and lived practice.').

omega_variable(
    shugendo_survival_trajectory,
    'Did the Meiji suppression of shugendo eliminate the fusion tradition, or did it persist underground and resurface in contemporary spiritual movements?',
    'Historical tracing of shugendo lineages through the Meiji suppression, post-war revival, and current new religious movements that explicitly fuse kami and buddha worship.',
    'If shugendo persisted as a live alternative, the partition never achieved full accessibility collapse — the constraint is less mountain-like, more rope/snare hybrid. If eliminated, the partition''s suppression was effective and accessibility_collapse is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shugendo_survival_trajectory, empirical, 'Whether the main alternative to the partition survived suppression.').

omega_variable(
    committer_framing_ambiguity,
    'Does the domain_partition reading genuinely foreclose honji_suijaku_monism, or do they coexist as different institutional commitments?',
    'Analyze whether any single institution or community holds both readings simultaneously without contradiction (e.g., a temple-shrine complex that teaches honji_suijaku doctrine while operating domain-partition ritual schedules).',
    'If forecloses, the readings are mutually exclusive frameworks — the kernel is genuinely contested. If coexists_with, the kernel hosts stable pluralism — the contest is institutional, not logical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, conceptual, 'Logical relationship between domain_partition and honji_suijaku_monism readings of the kami_buddha_ontology kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbdpart_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.65).
narrative_ontology:measurement(kbdpart_tr_t1890, kami_buddha_ontology__domain_partition, theater_ratio, 1890, 0.42).
narrative_ontology:measurement(kbdpart_tr_t1945, kami_buddha_ontology__domain_partition, theater_ratio, 1945, 0.28).
narrative_ontology:measurement(kbdpart_tr_t1970, kami_buddha_ontology__domain_partition, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(kbdpart_tr_t2000, kami_buddha_ontology__domain_partition, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(kbdpart_tr_t2024, kami_buddha_ontology__domain_partition, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(kbdpart_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.45).
narrative_ontology:measurement(kbdpart_be_t1890, kami_buddha_ontology__domain_partition, base_extractiveness, 1890, 0.38).
narrative_ontology:measurement(kbdpart_be_t1945, kami_buddha_ontology__domain_partition, base_extractiveness, 1945, 0.22).
narrative_ontology:measurement(kbdpart_be_t1970, kami_buddha_ontology__domain_partition, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(kbdpart_be_t2000, kami_buddha_ontology__domain_partition, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(kbdpart_be_t2024, kami_buddha_ontology__domain_partition, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(kbdpart_su_t1868, kami_buddha_ontology__domain_partition, suppression_requirement, 1868, 0.88).
narrative_ontology:measurement(kbdpart_su_t1890, kami_buddha_ontology__domain_partition, suppression_requirement, 1890, 0.72).
narrative_ontology:measurement(kbdpart_su_t1945, kami_buddha_ontology__domain_partition, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(kbdpart_su_t1970, kami_buddha_ontology__domain_partition, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(kbdpart_su_t2000, kami_buddha_ontology__domain_partition, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(kbdpart_su_t2024, kami_buddha_ontology__domain_partition, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, meiji_state_shinto_architecture).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, postwar_religious_freedom_framework).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, heritage_management_authenticity_policing).

% DUAL FORMULATION NOTE:
% Part of the kami_buddha_ontology constraint family with honji_suijaku_monism and incoherent_bundle. This reading (domain_partition) asserts ontological distinctness + functional complementarity; honji_suijaku_monism asserts ontological identity + hierarchical manifestation; incoherent_bundle denies coherent kernel. The three readings differ in ε: domain_partition ε=0.28 (coordination with legacy extraction), honji_suijaku_monism ε≈0.15 (doctrinal coherence, low extraction), incoherent_bundle ε≈0.65 (institutionalized contradiction as extraction mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, institutional, 0.15).
constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, organized, 0.2).
constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, moderate, 0.65).
constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
