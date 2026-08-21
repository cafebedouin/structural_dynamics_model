% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Shinbutsu Ontological Substrate: Syncretic Fusion Reading
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story describes the 'syncretic fusion' reading of the
 *   shinbutsu ontological substrate, where kami and buddhas are understood as
 *   fundamentally unified at a metaphysical level, and the honji suijaku
 *   theory is seen as describing this deep truth rather than a mere
 *   institutional arrangement. This reading emphasizes the inherent
 *   compatibility and ultimate non-duality of the two traditions, leading to
 *   high institutional entanglement and resistance to separation. The metrics
 *   reflect a stable, low-extractive 'rope' that genuinely coordinates
 *   spiritual life, with moderate suppression against dissenting views.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.3).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.4).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Shinbutsu Ontological Substrate: Syncretic Fusion Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb').
narrative_ontology:cs_kernel_codification('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', formalized).
narrative_ontology:cs_authority_grounding('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', lineage).
narrative_ontology:cs_interpretation_layer_present('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb').
narrative_ontology:cs_reading_relation('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', kami_buddha_ontological_unity, deontological).
narrative_ontology:cs_axiom('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', foundational, honji_suijaku_as_metaphysical_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_as_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', honji_suijaku_as_metaphysical_truth, theological).
narrative_ontology:cs_reference_frame('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', pre_meiji_syncretic_orthodoxy).
narrative_ontology:cs_drift_state('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', contemporary_religious_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a7b2b9fa-e50f-4061-8a25-a40fe24dbcfb', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_religious_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, practitioners_of_syncretic_faiths).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_doctrine_as_metaphysical_truth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (temples, shrines, and hybrid establishments) actively maintain and propagate the understanding of kami and buddhas as fundamentally unified. Their legitimacy and operational model depend on this fusion, resisting any attempts at strict separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_religious_institutions, agenda_setter,
    institutional, generational, constrained, national).

% For these individuals, the ontological unity provides a coherent spiritual framework, allowing them to engage with both Shinto and Buddhist practices without perceived contradiction. Their spiritual identity is often deeply intertwined with this syncretic understanding.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, practitioners_of_syncretic_faiths, beneficiary,
    moderate, biographical, identity_locked, local).

% These scholars analyze the historical, theological, and social implications of the syncretic fusion reading, often engaging in debates with other interpretations of shinbutsu relations. They do not directly benefit or pay, but their analysis shapes understanding.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% Historically, state authorities (especially during the Meiji era) attempted to enforce a strict separation of Shinto and Buddhism (shinbutsu bunri). From the perspective of this reading, their efforts were an external imposition that failed to grasp the deep ontological reality, and they are now largely excluded from shaping this internal theological understanding.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_authorities_post_meiji, excluded,
    institutional, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological and ritual framework for Japanese religious life, allowing for seamless integration of indigenous kami worship and imported Buddhist practices, resolving potential conflicts between distinct pantheons.
% TRANSFER_FUNCTION: Transfers spiritual coherence and legitimacy to hybrid religious practices and institutions, from the metaphysical understanding of unity to the lived experience of practitioners.
% ABSENT_VOICES: Strict Shinto purists or Buddhist exclusivists who would argue for the absolute distinctness of kami and buddhas are marginalized in this framework, as their theological premises are deemed incompatible with the underlying ontological truth.
% DISAPPEARANCE_RATIONALE: If the ontological unity of kami and buddhas were suddenly disproven or rejected, the entire edifice of syncretic Japanese religious practice would collapse. Institutions would lose their foundational justification, and practitioners would face a profound spiritual crisis, necessitating a complete re-evaluation of their faith.
% FOUNDING_PROBLEM: The historical encounter between indigenous Japanese kami worship and the introduction of Buddhism, requiring a theological framework to reconcile and integrate these distinct spiritual traditions.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence and flourishing of syncretic religious practices and institutions, as well as ongoing theological discourse within these traditions, corroborates the live status of the problem. Academic scholars also attest to the historical and ongoing need for such reconciliation, even if they analyze it differently.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.3) because this reading primarily serves to integrate and provide spiritual coherence, with benefits widely distributed among practitioners and institutions. Suppression (0.4) exists against purist or separatist views, but it's more theological/social than coercive. Theater ratio is low (0.1) as the practices genuinely reflect the underlying belief. Accessibility collapse is high (0.7) because for adherents, the unified framework is the primary and most coherent way to engage with Japanese spirituality. Resistance is low (0.2) because this reading was historically dominant and widely accepted prior to modern state interventions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of syncretic religious institutions and practitioners, this is a genuine 'rope' that provides essential spiritual coordination. From the perspective of state authorities attempting separation (e.g., Meiji era), it might be viewed as a 'tangled rope' or 'snare' due to its resistance to external reordering, but this reading itself does not acknowledge such a perspective as valid for its own internal logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic religious institutions are agenda-setters and beneficiaries, as their existence and practices are validated by this ontological understanding. Practitioners are beneficiaries, finding spiritual coherence. Academic observers are neutral. State authorities (post-Meiji) are excluded, as their attempts to impose separation are seen as external to the metaphysical truth this reading asserts.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a deeply held theological and cultural coordination mechanism as pure extraction. While state-enforced separation (shinbutsu bunri) later attempted to dismantle this fusion, the 'syncretic fusion' reading itself represents a genuine, historically evolved coordination of spiritual concepts, not a mandate that has atrophied. The persistence of syncretic practices even after state intervention suggests its deep roots.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''syncretic_fusion_reading'' of the ''shinbutsu_ontological_substrate'' kernel?',
    'Comparative textual analysis of primary sources and theological treatises from different historical periods, cross-referenced with ethnographic studies of contemporary religious practice.',
    'If misidentified, the classification of this constraint would be inaccurate, potentially leading to a ''conceptual'' reclassification if it aligns better with a sibling reading or an entirely new constraint if it represents a distinct interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the precise identification of this constraint as a specific reading of the shinbutsu ontological substrate kernel.').

omega_variable(
    ontological_vs_functional_distinction,
    'Is the ''honji suijaku'' concept primarily a metaphysical truth (as this reading claims) or a functional/institutional arrangement for coexistence?',
    'Analysis of pre-Meiji theological debates and the lived religious experience of practitioners, distinguishing between explicit doctrinal claims and pragmatic institutional adaptations.',
    'If primarily functional, the extractiveness and suppression metrics might be higher, as the ''unity'' would be a constructed justification for institutional power rather than a shared spiritual truth, potentially shifting the classification towards a ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_distinction, empirical, 'Distinguishes between the ontological claim of unity and a functional interpretation of honji suijaku.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 700, 0.05).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 700, 0.25).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 900, 0.28).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1200, 0.3).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1500, 0.29).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1868, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 700, 0.3).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1200, 0.4).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1500, 0.38).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1868, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
