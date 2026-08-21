% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition Reading
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the functional partition of Kami and Buddhist
 *   deities in Japan, where each tradition governs separate existential
 *   domains (Kami for life, purity, harvest; Buddhas for death, salvation,
 *   afterlife) without requiring deep ontological unification. This reading
 *   emphasizes the practical, emergent coordination that allowed both
 *   traditions to thrive in parallel. The constraint is claimed as a Tangled
 *   Rope because while it provides genuine coordination benefits, it
 *   implicitly suppresses alternative theological frameworks that seek
 *   unification, benefiting the established religious institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.35).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.45).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'e13ddfa7-30de-4598-b141-a70a4cca1ce0').
narrative_ontology:cs_kernel_codification('e13ddfa7-30de-4598-b141-a70a4cca1ce0', implicit).
narrative_ontology:cs_authority_grounding('e13ddfa7-30de-4598-b141-a70a4cca1ce0', practice).
narrative_ontology:cs_interpretation_layer_present('e13ddfa7-30de-4598-b141-a70a4cca1ce0').
narrative_ontology:cs_reading_relation('e13ddfa7-30de-4598-b141-a70a4cca1ce0', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('e13ddfa7-30de-4598-b141-a70a4cca1ce0', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('e13ddfa7-30de-4598-b141-a70a4cca1ce0', foundational, kami_buddha_functional_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_functional_distinction, holdable).
narrative_ontology:cs_axiom_grounding('e13ddfa7-30de-4598-b141-a70a4cca1ce0', kami_buddha_functional_distinction, conventional).
narrative_ontology:cs_axiom('e13ddfa7-30de-4598-b141-a70a4cca1ce0', foundational, ontological_non_unification).
narrative_ontology:cs_axiom_status(ontological_non_unification, holdable).
narrative_ontology:cs_axiom_grounding('e13ddfa7-30de-4598-b141-a70a4cca1ce0', ontological_non_unification, deontological).
narrative_ontology:cs_reference_frame('e13ddfa7-30de-4598-b141-a70a4cca1ce0', pre_meiji_functional_partition).
narrative_ontology:cs_drift_state('e13ddfa7-30de-4598-b141-a70a4cca1ce0', contemporary_post_shinto_directive, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e13ddfa7-30de-4598-b141-a70a4cca1ce0', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, religious_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_seeking_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the domain of Kami worship, purity, life events, and harvest rituals. They benefit from a clear, non-overlapping spiritual jurisdiction and the associated social and financial support from practitioners.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrines, agenda_setter,
    institutional, generational, constrained, national).

% Maintain the domain of Buddhist practice, death rituals, salvation, and the afterlife. They benefit from a clear, non-overlapping spiritual jurisdiction and the associated social and financial support from practitioners.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temples, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a clear division of spiritual labor, knowing which tradition to approach for specific life events or concerns. This reduces cognitive load and potential conflict in their religious lives.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, religious_practitioners, beneficiary,
    moderate, biographical, mobile, local).

% Their efforts to develop a coherent, ontologically unified theology of Kami and Buddhas are implicitly constrained by the prevailing functional partition. While not explicitly forbidden, their work often struggles for widespread acceptance against the established practical division.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_seeking_unification, payer,
    moderate, generational, constrained, national).

% Analyze the historical and contemporary dynamics of Shinbutsu coexistence, documenting the functional partition and its implications without directly participating in its maintenance or being subject to its spiritual authority.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, scholars_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows two distinct religious traditions (Shinto and Buddhism) to coexist and serve different spiritual needs within the same society without direct conflict or requiring complex theological synthesis, by assigning them separate, non-overlapping existential domains.
% TRANSFER_FUNCTION: Transfers spiritual authority and ritual responsibility for specific life domains (e.g., birth, marriage, harvest to Kami; death, afterlife to Buddhas) to their respective institutions, along with the associated social support and resources.
% ABSENT_VOICES: Theologians or practitioners who might seek a deeper ontological unification or challenge the functional partition are implicitly marginalized; their arguments for synthesis often fail to gain traction against the established practical division.
% DISAPPEARANCE_RATIONALE: If the functional partition vanished overnight, the Japanese religious landscape would become chaotic, with overlapping claims of spiritual authority, potential conflict between institutions, and confusion for practitioners regarding appropriate rituals for life and death events. The existing social and institutional structures would need to fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of integrating or managing the coexistence of indigenous Kami worship and imported Buddhism in Japan without one tradition subsuming the other or causing societal conflict, while providing comprehensive spiritual services for all aspects of life.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, anthropological studies of Japanese religious practice, and contemporary sociological surveys consistently demonstrate the persistence of this functional division in popular belief and institutional roles, even after periods of state-mandated separation. Scholars of Japanese religion widely corroborate the ongoing relevance of this coordination problem.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as the system, while functional, channels resources and authority to specific institutions based on this division, potentially at the expense of alternative religious expressions. Suppression is moderate (0.45) because while there's no overt coercion against theological unification, the established social and institutional practices effectively marginalize such efforts. Theater ratio is low (0.15) as the functional partition is a deeply ingrained and genuinely practiced aspect of Japanese religious life, not merely a performance. The metrics show a slight, gradual increase over a long historical period, reflecting the solidification of institutional roles and the subtle hardening of boundaries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of most practitioners and the religious institutions, this domain partition is a natural and beneficial coordination mechanism. From the perspective of theologians seeking a unified understanding, it represents a structural barrier to deeper intellectual and spiritual integration. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto shrines and Buddhist temples are clear beneficiaries and agenda-setters, as they directly manage and profit from their respective domains. Religious practitioners are beneficiaries, gaining clarity and stability in their spiritual lives. Theologians seeking unification are payers/victims, as their intellectual and spiritual pursuits are implicitly constrained by the dominant practical partition. Scholars are observers, analyzing the system without being subject to its spiritual authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_popular_partition,
    'To what extent is the domain partition a conscious theological commitment by religious elites versus an emergent, pragmatic arrangement in popular practice?',
    'Analysis of historical theological treatises versus ethnographic studies of folk religious practices across different periods.',
    'If primarily theological, the suppression of unification efforts is more deliberate and extractive. If primarily popular, the constraint is more of an emergent social norm with less intentional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_popular_partition, empirical, 'Distinguishing elite theological intent from popular religious pragmatism in maintaining the partition.').

omega_variable(
    meiji_era_impact_reversibility,
    'Was the Meiji-era Shinbutsu-Bunri (separation of Kami and Buddhas) a temporary disruption to an underlying partition, or did it fundamentally alter the nature of coexistence?',
    'Longitudinal studies of religious practice and institutional structures before and after the Meiji period, assessing the degree of ''re-fusion'' or re-establishment of the partition.',
    'If temporary, the partition is a more robust, enduring feature. If fundamental, the contemporary partition is a new construct, potentially with different underlying dynamics and extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_era_impact_reversibility, empirical, 'Assessing the long-term impact and reversibility of state-mandated religious separation on the domain partition.').

omega_variable(
    coherence_of_partition_claim,
    'Is the ''domain partition'' itself a coherent and stable concept, or does it mask underlying ambiguities and overlaps in practice?',
    'Detailed ethnographic research into individual practitioners'' beliefs and rituals, looking for instances where the ''domains'' are blurred or integrated in ways not captured by the formal partition.',
    'If the partition is found to be highly ambiguous in practice, the ''coordination'' function is weaker, and the constraint might lean more towards an ''incoherent_bundle_reading'' or a more extractive ''tangled_rope'' if ambiguity benefits institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_of_partition_claim, conceptual, 'Examining the practical coherence and stability of the claimed domain partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 1000, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.11).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1600, 0.13).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1800, 0.14).
narrative_ontology:measurement(shin_tr_t2000, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1000, 0.3).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.31).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1400, 0.32).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1600, 0.33).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1800, 0.34).
narrative_ontology:measurement(shin_be_t2000, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 2000, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1200, 0.41).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1400, 0.42).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1600, 0.43).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1800, 0.44).
narrative_ontology:measurement(shin_su_t2000, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 2000, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel, focusing on the functional domain partition. It is distinct from the 'syncretic_fusion_reading' (ontological unification) and 'incoherent_bundle_reading' (lack of coherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
