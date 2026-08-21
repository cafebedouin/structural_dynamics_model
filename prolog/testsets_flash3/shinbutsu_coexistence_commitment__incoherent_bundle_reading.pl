% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle (Incoherent Bundle Reading)
 *   domain: religious_studies/history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'incoherent bundle' reading of
 *   Shinbutsu-shugo, arguing that the historical coexistence of Shinto and
 *   Buddhism in Japan was not a coherent syncretism but an unstable
 *   aggregation maintained by institutional power and deliberate ambiguity.
 *   The system avoided ontological reconciliation, allowing both traditions
 *   to benefit from shared resources and local influence. The Meiji
 *   government's Shinbutsu-bunri (separation) policy is seen not as
 *   destroying a unified system, but as revealing its inherent incoherence
 *   and the fragility of its power-based maintenance. The constraint is
 *   claimed as a Tangled Rope because it provided a coordination function
 *   (institutional coexistence) but also extracted resources and suppressed
 *   alternative, more coherent theological developments, requiring active
 *   enforcement of ambiguity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.65).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle (Incoherent Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '90e55d43-83ce-4bdf-a569-2b4b91ebd44a').
narrative_ontology:cs_kernel_codification('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', implicit).
narrative_ontology:cs_authority_grounding('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', extraction).
narrative_ontology:cs_interpretation_layer_present('90e55d43-83ce-4bdf-a569-2b4b91ebd44a').
narrative_ontology:cs_reading_relation('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', foundational, ontological_incoherence_is_structural).
narrative_ontology:cs_axiom_status(ontological_incoherence_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', ontological_incoherence_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', foundational, ambiguity_maintained_by_power).
narrative_ontology:cs_axiom_status(ambiguity_maintained_by_power, holdable).
narrative_ontology:cs_axiom_grounding('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', ambiguity_maintained_by_power, empirically_contingent).
narrative_ontology:cs_reference_frame('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', power_maintained_ambiguity_framework).
narrative_ontology:cs_drift_state('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', meiji_restoration_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('90e55d43-83ce-4bdf-a569-2b4b91ebd44a', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_elites).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, common_worshippers).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, intellectual_reformers).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, power_maintains_ambiguity_thesis).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_restoration_revealed_incoherence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the combined religious sites, collected offerings, and held significant land and social influence. Benefited from the ambiguity that allowed them to integrate local kami worship into Buddhist frameworks without resolving ontological contradictions. Actively resisted attempts to clarify or separate the traditions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temples, agenda_setter,
    institutional, generational, constrained, national).

% Coexisted with Buddhist temples, often sharing precincts and administrative structures. Benefited from the integrated system that provided resources and legitimacy. Their distinct identity was often subsumed or blurred, but the arrangement secured their institutional survival and local influence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrines, agenda_setter,
    institutional, generational, constrained, national).

% Supported the ambiguous shinbutsu-shugo system as it reinforced their social status and provided a stable framework for local governance and ritual life. They benefited from the lack of clear doctrinal boundaries, which prevented challenges to their syncretic practices.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_elites, beneficiary,
    powerful, biographical, mobile, local).

% Participated in rituals that blended Shinto and Buddhist elements, often without a clear understanding of the underlying theological distinctions. They bore the costs of offerings and labor for both traditions, and their spiritual practices were shaped by the institutional arrangements rather than a coherent theology. Their options were limited to local practices.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, common_worshippers, payer,
    powerless, immediate, trapped, local).

% Challenged the doctrinal incoherence and institutional power of shinbutsu-shugo, advocating for a 'pure' Shinto or a more rationalized Buddhism. They faced institutional resistance and suppression, but their critiques laid the groundwork for the Meiji separation. They paid in social capital and career advancement.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, intellectual_reformers, payer,
    moderate, generational, constrained, national).

% Acted as an external force that exploited the inherent incoherence of shinbutsu-shugo to implement the Shinbutsu-bunri (separation of Kami and Buddhas) policy. They sought to establish Shinto as the state religion, thereby consolidating imperial authority and dismantling the power of Buddhist institutions. Their intervention revealed the bundle's fragility.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_government, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for local religious practice and institutional coexistence between Shinto shrines and Buddhist temples, allowing for shared resources and ritual spaces without requiring ontological reconciliation.
% TRANSFER_FUNCTION: Transferred offerings, land, and social influence to the combined religious institutions (temples and shrines) from common worshippers and local communities, in exchange for ritual services and spiritual legitimation.
% ABSENT_VOICES: Scholars and theologians who sought a coherent philosophical or theological basis for religious practice were often marginalized or suppressed, as their inquiries threatened the deliberate ambiguity that sustained the system. They would have argued for doctrinal clarity and institutional reform.
% DISAPPEARANCE_RATIONALE: If the institutional power maintaining the ambiguous shinbutsu-shugo bundle had vanished prior to Meiji, the religious landscape would have fragmented into distinct Shinto and Buddhist practices, or new syncretic forms would have emerged based on local needs, rather than a centrally enforced ambiguity. The Meiji separation demonstrated how deeply the system was embedded in institutional power.
% FOUNDING_PROBLEM: To integrate indigenous Kami worship with the newly introduced Buddhism, allowing for the peaceful coexistence and mutual benefit of both traditions and their associated institutions, without requiring a definitive theological synthesis.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji government's Shinbutsu-bunri policy, driven by nationalist ideology and a desire to dismantle Buddhist power, effectively demonstrated that the 'problem' of integration was solved by institutional force, not theological coherence. Intellectual reformers and later historians corroborate that the system was maintained by power and ambiguity, not by a genuine, stable synthesis; the problem was 'solved' by avoiding it, and the solution collapsed when power shifted.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the system channeled resources from common worshippers to institutions without providing a coherent theological framework, effectively extracting spiritual and material labor for an ambiguous benefit. Suppression is also high (0.7) as the system actively resisted intellectual challenges to its incoherence and maintained its structure through institutional power. Theater ratio is moderate (0.4) because while rituals and practices were genuinely performed, a significant portion of institutional effort went into maintaining the ambiguous bundle rather than developing a unified theology. The increasing extractiveness and suppression over time reflect the growing institutionalization and ossification of the system prior to its collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist temples and Shinto shrines (agenda-setters), the system was a functional coordination mechanism that ensured their institutional survival and prosperity. From the perspective of common worshippers and intellectual reformers (payers/victims), it was an opaque, extractive system that suppressed doctrinal clarity and imposed an incoherent religious practice. The Meiji government, as an external agenda-setter, saw an opportunity to dismantle a powerful, ambiguous system for political gain.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temples and Shinto shrines were primary beneficiaries and agenda-setters, actively maintaining the ambiguity for institutional gain. Local elites also benefited from the social stability it provided. Common worshippers were payers, bearing the costs of offerings and participating in rituals without clear theological grounding. Intellectual reformers were also payers, as their attempts to introduce coherence were suppressed. The Meiji government, while acting as an agenda-setter in its dismantling, was a beneficiary of the prior system's incoherence, which made it vulnerable to political intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the system as a pure Rope (ignoring extraction and suppression) or a pure Snare (ignoring its genuine, albeit ambiguous, coordination function for institutional coexistence). The 'incoherent bundle' reading highlights that the mandate (integrating traditions) was never truly fulfilled in a coherent sense, but rather maintained through power. The Meiji separation revealed the mandatrophy, as the 'founding problem' of integration was shown to be 'dead' in its original form, replaced by a power-maintenance function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_as_feature_or_bug,
    'Was the ambiguity inherent in Shinbutsu-shugo a deliberate, functional feature that enabled institutional coexistence, or a fundamental flaw that rendered it incoherent?',
    'Analysis of primary sources (theological treatises, institutional records) for explicit statements regarding the role of ambiguity, and comparative studies of other syncretic traditions that achieved greater or lesser ontological coherence.',
    'If a deliberate feature, the coordination function is stronger, and the extraction is more a ''cost of coordination''. If a fundamental flaw, the extraction is higher, as resources were diverted to maintain an inherently unstable system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, conceptual, 'Whether the ambiguity was a functional design choice or a structural defect.').

omega_variable(
    meiji_bunri_causality,
    'Did the Meiji Shinbutsu-bunri policy primarily cause the collapse of shinbutsu-shugo, or did it merely accelerate an inevitable collapse due to inherent incoherence and growing internal pressures?',
    'Counterfactual historical analysis: what would have happened without Meiji intervention? Examination of pre-Meiji intellectual movements and their impact on the system''s stability.',
    'If Meiji caused the collapse, the system was more robust than this reading suggests. If Meiji merely revealed existing fragility, this reading''s emphasis on inherent incoherence is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_causality, empirical, 'The role of Meiji policy in the collapse of shinbutsu-shugo.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the measured suppression of alternative theological developments structural (institutional power, lack of platforms) or internalized (worshippers'' acceptance of ambiguity, lack of critical tools)?',
    'Post-Meiji intellectual trajectory: if coherent theological developments rapidly emerged after institutional barriers were removed, suppression was primarily structural. If ambiguity persisted in popular practice, internalized suppression played a larger role.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as the lack of clarity was self-perpetuating. If structural, the system''s coercive nature is more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1650, 0.32).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1700, 0.35).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.37).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1800, 0.39).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1600, 0.55).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1650, 0.58).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1800, 0.63).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1868, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1700, 0.65).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.68).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1800, 0.69).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1868, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel. This 'incoherent_bundle_reading' posits that the system was maintained by power and ambiguity, not coherent fusion or clear partition. It contrasts with the 'syncretic_fusion_reading' (which emphasizes ontological unity) and the 'domain_partition_reading' (which emphasizes functional separation without unity). All three are distinct constraints derived from the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
