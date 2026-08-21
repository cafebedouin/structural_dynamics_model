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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle Maintained by Power
 *   domain: religious_studies/history/philosophy_of_religion
 *
 * SUMMARY:
 *   This constraint models Shinbutsu-shugo (the syncretic fusion of Shinto
 *   and Buddhism in Japan) from the perspective that it was never a coherent
 *   theological or ontological system, but rather an 'incoherent bundle'
 *   maintained through deliberate ambiguity and institutional power. Its
 *   persistence was due to the benefits it conferred upon the ruling elite
 *   and religious institutions, rather than any intrinsic spiritual or
 *   philosophical unity. The Meiji Restoration's forced separation
 *   (Shinbutsu-bunri) is seen as revealing this underlying incoherence,
 *   rather than creating a new division.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.8).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.85).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle Maintained by Power").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/history/philosophy_of_religion").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '3b452511-9b9d-4465-8466-47d308015a77').
narrative_ontology:cs_kernel_codification('3b452511-9b9d-4465-8466-47d308015a77', implicit).
narrative_ontology:cs_authority_grounding('3b452511-9b9d-4465-8466-47d308015a77', extraction).
narrative_ontology:cs_interpretation_layer_present('3b452511-9b9d-4465-8466-47d308015a77').
narrative_ontology:cs_reading_relation('3b452511-9b9d-4465-8466-47d308015a77', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b452511-9b9d-4465-8466-47d308015a77', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('3b452511-9b9d-4465-8466-47d308015a77', foundational, ontological_ambiguity_is_functional).
narrative_ontology:cs_axiom_status(ontological_ambiguity_is_functional, holdable).
narrative_ontology:cs_axiom_grounding('3b452511-9b9d-4465-8466-47d308015a77', ontological_ambiguity_is_functional, conventional).
narrative_ontology:cs_axiom('3b452511-9b9d-4465-8466-47d308015a77', foundational, institutional_power_sustains_incoherence).
narrative_ontology:cs_axiom_status(institutional_power_sustains_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('3b452511-9b9d-4465-8466-47d308015a77', institutional_power_sustains_incoherence, conventional).
narrative_ontology:cs_reference_frame('3b452511-9b9d-4465-8466-47d308015a77', functional_ambiguity_as_institutional_norm).
narrative_ontology:cs_drift_state('3b452511-9b9d-4465-8466-47d308015a77', meiji_restoration_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3b452511-9b9d-4465-8466-47d308015a77', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ruling_elite).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_purist_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_scholars_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the combined system, benefiting from dual patronage, landholdings, and spiritual authority. Their institutional identity was deeply fused with the ambiguous coexistence, making exit unthinkable until forced.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefited from the social cohesion and legitimacy provided by the integrated religious system, using its ambiguity to avoid theological conflict and maintain control over spiritual practices. They actively supported its maintenance through patronage and legal frameworks.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ruling_elite, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ruling_elite, agenda_setter).

% Supported both Shinto shrines and Buddhist temples through offerings, labor, and participation in rituals, often without a clear distinction between the two. Their spiritual practices were integrated into the ambiguous bundle, with little agency to disentangle them.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Developed complex interpretive frameworks to rationalize the coexistence, often through honji suijaku theories. They gained intellectual authority and institutional roles, but their careers and identities were bound to maintaining the ambiguous system.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_scholars_interpreters, beneficiary,
    organized, generational, identity_locked, national).

% Advocated for a clear separation of Kami worship from Buddhism, viewing the syncretic bundle as a corruption of indigenous traditions. Their voices were largely suppressed or marginalized by the dominant institutional power until the Meiji Restoration.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_purist_movements, excluded,
    organized, generational, constrained, national).

% A new political power that saw the ambiguous Shinbutsu-shugo as an obstacle to national unity and a pure 'national religion'. They actively enforced the Shinbutsu-bunri (separation of Kami and Buddhas) decree, dismantling the long-standing bundle.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_government, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for the coexistence and integration of indigenous Kami worship and imported Buddhism, allowing for dual patronage and avoiding overt theological conflict across diverse spiritual practices.
% TRANSFER_FUNCTION: Transferred spiritual authority, institutional power, and material resources (land, offerings) to the combined shrine-temple complexes and the ruling elite, in exchange for social cohesion and spiritual services to local communities.
% ABSENT_VOICES: Shinto purists and those seeking clear ontological distinctions were largely excluded from the dominant discourse, their arguments for separation suppressed by the institutional power that benefited from the ambiguous bundle.
% DISAPPEARANCE_RATIONALE: The forced separation of Kami and Buddhas during the Meiji Restoration led to a massive reorganization of religious institutions, land ownership, and spiritual practices, fundamentally altering the landscape of Japanese religion and society.
% FOUNDING_PROBLEM: To integrate and manage the relationship between indigenous Kami worship and the newly introduced, powerful Buddhist tradition, avoiding conflict and leveraging both for social and political stability.
% FOUNDING_PROBLEM_CORROBORATION: Historians and modern religious scholars widely attest that the original problem of integrating two distinct spiritual systems was 'solved' not by the bundle's inherent coherence, but by its eventual forced separation. The Meiji government's actions explicitly aimed to resolve what it perceived as the incoherence of the prior system.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.8) reflects the significant resources and authority accumulated by shrine-temple complexes and the ruling elite through the dual system. Suppression (0.85) was high due to the active marginalization of Shinto purist movements and any attempts to clarify or separate the traditions. The theater ratio (0.6) indicates that while some genuine spiritual functions existed, a substantial portion of the system's operation involved maintaining the 'performance' of ambiguity to avoid conflict and preserve institutional power. Accessibility collapse (0.75) was high as there were few viable alternatives for religious practice outside the established syncretic framework. Resistance (0.4) was present but largely contained until the external pressure of the Meiji government.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the shrine-temple complexes and ruling elite, the Shinbutsu-shugo was a functional, if complex, system for social and spiritual management. From the perspective of local communities, it was simply the way things were, requiring their support. Shinto purists, however, experienced it as a suppressive force that obscured true Kami worship. The Meiji government viewed it as an anachronistic, incoherent system ripe for dismantling.
 *
 * DIRECTIONALITY LOGIC:
 *   The shrine-temple complexes and ruling elite were clear beneficiaries, collecting resources and wielding authority. Local communities were payers, providing support without significant agency. Buddhist scholars, while beneficiaries of institutional roles, also bore the intellectual cost of maintaining ambiguity. Shinto purists were victims, their alternative vision suppressed. The Meiji government acted as an external agenda-setter, dismantling the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests the constraint was a snare, where any coordination function (e.g., social cohesion) was secondary to the extraction of power and resources. The 'mandate' of spiritual integration was a cover for institutional maintenance. The Meiji bunri effectively resolved this mandatrophy by forcibly separating the institutions, revealing the underlying power dynamics that held the 'incoherent bundle' together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_as_feature_vs_bug,
    'Was the ambiguity inherent in Shinbutsu-shugo a deliberate, functional feature that enabled social and institutional stability, or an inherent flaw that ultimately led to its collapse?',
    'Comparative historical analysis of other syncretic traditions and their stability mechanisms, alongside detailed examination of contemporary theological and institutional justifications for ambiguity.',
    'If primarily a functional feature, the ''snare'' classification might be too strong, suggesting a ''tangled_rope'' with a genuine (if extractive) coordination function. If an inherent flaw, the ''snare'' classification is reinforced, highlighting the system''s fragility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_vs_bug, conceptual, 'Examines whether Shinbutsu-shugo''s ambiguity was a design choice or a structural weakness.').

omega_variable(
    meiji_bunri_revelation_vs_creation,
    'Did the Meiji Shinbutsu-bunri (separation) merely reveal a pre-existing, underlying incoherence in Shinbutsu-shugo, or did it actively create a new, artificial separation where a functional (if ambiguous) system once existed?',
    'Analysis of pre-Meiji theological debates and popular practices for evidence of inherent tensions or clear distinctions, compared with the explicit ideological motivations and enforcement mechanisms of the Meiji government.',
    'If it revealed incoherence, this reading''s ''snare'' classification is strengthened. If it created separation, the prior system might be reclassified as a ''tangled_rope'' or even a ''rope'' from certain perspectives, with the Meiji government acting as an external force rather than a revealer of truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_revelation_vs_creation, empirical, 'Assesses whether Meiji separation exposed or imposed incoherence.').

omega_variable(
    genuine_coordination_function_extent,
    'To what extent did Shinbutsu-shugo genuinely coordinate spiritual life and social order, beyond serving as a cover for institutional extraction?',
    'Sociological and anthropological studies of local religious practices and community cohesion during the period, assessing the perceived benefits of the integrated system for ordinary people.',
    'If a significant, non-extractive coordination function is identified, the constraint might shift towards a ''tangled_rope'' or even a ''rope'' from the perspective of local communities, acknowledging a more balanced cost-benefit for some participants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_coordination_function_extent, empirical, 'Quantifies the non-extractive coordination benefits of Shinbutsu-shugo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1000, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1000, 0.5).
narrative_ontology:measurement(shin_tr_t1170, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1170, 0.55).
narrative_ontology:measurement(shin_tr_t1340, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1340, 0.6).
narrative_ontology:measurement(shin_tr_t1510, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1510, 0.62).
narrative_ontology:measurement(shin_tr_t1680, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1680, 0.65).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1868, 0.6).

% Extraction over time
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(shin_be_t1170, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1170, 0.75).
narrative_ontology:measurement(shin_be_t1340, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1340, 0.78).
narrative_ontology:measurement(shin_be_t1510, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1510, 0.8).
narrative_ontology:measurement(shin_be_t1680, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1680, 0.82).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1868, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(shin_su_t1170, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1170, 0.75).
narrative_ontology:measurement(shin_su_t1340, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1340, 0.8).
narrative_ontology:measurement(shin_su_t1510, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1510, 0.83).
narrative_ontology:measurement(shin_su_t1680, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1680, 0.85).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1868, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_coexistence_commitment' kernel, each representing a distinct structural interpretation of the historical Shinbutsu-shugo. This 'incoherent_bundle_reading' emphasizes institutional power and ambiguity over theological coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
