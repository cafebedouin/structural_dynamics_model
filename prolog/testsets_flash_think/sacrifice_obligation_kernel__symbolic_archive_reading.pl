% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (Cultural Preservation Reading)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'symbolic archive' reading of sacrifice
 *   law, where the study of these ancient texts is understood as a means of
 *   cultural preservation and identity formation, rather than a fulfillment
 *   of an active halakhic (legal) obligation. It emphasizes continuity and
 *   memory, explicitly disclaiming any coercive or performative demands. The
 *   metrics reflect this non-extractive, voluntary engagement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (Cultural Preservation Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '6cf3183a-dde8-47eb-a428-75674cf07c30').
narrative_ontology:cs_kernel_codification('6cf3183a-dde8-47eb-a428-75674cf07c30', fixed_text).
narrative_ontology:cs_authority_grounding('6cf3183a-dde8-47eb-a428-75674cf07c30', practice).
narrative_ontology:cs_interpretation_layer_present('6cf3183a-dde8-47eb-a428-75674cf07c30').
narrative_ontology:cs_reading_relation('6cf3183a-dde8-47eb-a428-75674cf07c30', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cf3183a-dde8-47eb-a428-75674cf07c30', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cf3183a-dde8-47eb-a428-75674cf07c30', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('6cf3183a-dde8-47eb-a428-75674cf07c30', foundational, sacrifice_law_as_cultural_heritage).
narrative_ontology:cs_axiom_status(sacrifice_law_as_cultural_heritage, holdable).
narrative_ontology:cs_axiom_grounding('6cf3183a-dde8-47eb-a428-75674cf07c30', sacrifice_law_as_cultural_heritage, conventional).
narrative_ontology:cs_axiom('6cf3183a-dde8-47eb-a428-75674cf07c30', foundational, no_active_halakhic_obligation).
narrative_ontology:cs_axiom_status(no_active_halakhic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6cf3183a-dde8-47eb-a428-75674cf07c30', no_active_halakhic_obligation, conventional).
narrative_ontology:cs_reference_frame('6cf3183a-dde8-47eb-a428-75674cf07c30', post_temple_diaspora_cultural_preservation).
narrative_ontology:cs_drift_state('6cf3183a-dde8-47eb-a428-75674cf07c30', contemporary_diaspora_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6cf3183a-dde8-47eb-a428-75674cf07c30', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, lay_community).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_scholars).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, identity_preservation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of ancient texts and practices as cultural heritage, which reinforces a shared sense of identity and historical continuity across generations. Its existence is intertwined with the continuity of this cultural archive.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% Interpret, teach, and transmit sacrifice law as a cultural and historical archive. They shape the discourse around this reading, ensuring its intellectual and pedagogical continuity. Their careers and intellectual identity are often tied to this interpretive work.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_scholars, agenda_setter,
    institutional, generational, analytical, global).

% Engages with the study of sacrifice law as a means of connecting with their heritage and strengthening their Jewish identity, without feeling bound by unfulfillable ritual obligations. They can choose to participate in this study or not, and can find other avenues for cultural engagement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, lay_community, beneficiary,
    moderate, biographical, mobile, global).

% Believe that sacrifice obligations are divinely suspended until a messianic era and that study should maintain operational readiness for future performance. This reading's non-halakhic stance excludes their perspective on the texts' ultimate purpose.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, messianic_restorationists, excluded,
    organized, generational, identity_locked, global).

% Adhere to readings that emphasize active halakhic obligation (either through study as performance or actual physical performance). This reading's explicit rejection of a halakhic claim for sacrifice law places it outside their interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities_of_other_readings, excluded,
    institutional, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__symbolic_archive_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__symbolic_archive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of ancient religious texts and practices as cultural heritage, ensuring continuity of collective memory and identity across generations.
% TRANSFER_FUNCTION: Transfers cultural knowledge and historical understanding from ancient texts to contemporary generations, fostering a shared sense of identity without imposing ritual obligations.
% ABSENT_VOICES: Those who believe in a live, performative, or messianically suspended obligation would object, arguing that this reading diminishes the halakhic (legal) weight of the texts. They are excluded from this reading's framework by its explicit non-halakhic stance.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the cultural and historical significance of sacrifice law would diminish, potentially leading to a loss of collective memory and a weakening of Jewish identity, as the texts would either be ignored or re-interpreted solely through a lens of active obligation.
% FOUNDING_PROBLEM: The challenge of maintaining the relevance and continuity of ancient religious texts and practices, particularly those no longer ritually performed, in a way that strengthens collective identity without imposing an unfulfillable legal burden.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and cultural anthropologists attest to the ongoing challenge of cultural transmission for non-practiced rituals. Many contemporary Jewish educators and community leaders, outside the specific halakhic authorities, corroborate the need for this approach to maintain engagement with the tradition.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because this reading imposes no binding obligations or costs; engagement is voluntary and beneficial. Suppression is negligible (0.05) as there is no coercion to participate or to interpret the texts in a specific way. Theater ratio is low (0.10) because the act of study and cultural transmission is considered genuine and functional within this framework. Accessibility collapse and resistance are also very low (0.10, 0.05) as alternatives for cultural engagement exist, and no one is compelled to adhere to this specific interpretation.
 *
 * PERSPECTIVAL GAP:
 *   This reading stands in contrast to others that assert a live halakhic obligation (either through study as performance or actual physical performance) or a divinely suspended obligation. While this reading focuses on cultural continuity, other readings emphasize legal adherence or messianic anticipation, leading to fundamentally different interpretations of the texts' contemporary relevance.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective memory and the lay community are clear beneficiaries, gaining cultural continuity and identity without cost. Jewish scholars act as agenda-setters, shaping and transmitting this interpretive framework. There are no victims, as no party is coerced or extracted from. Other readings' adherents are 'excluded' from this framework's interpretive scope, as their core premises are not acknowledged as halakhically binding within this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Is this ''symbolic_archive_reading'' a genuine cultural interpretation, or a rationalization for avoiding a perceived (but unfulfillable) halakhic obligation?',
    'Analysis of historical and theological texts for explicit statements on the nature of post-Temple sacrifice law, and ethnographic study of community motivations for engaging with this reading.',
    'If primarily a rationalization, the effective extractiveness of the underlying (unfulfillable) obligation might be higher, as this reading serves as a coping mechanism rather than a freestanding cultural practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Ambiguity regarding the true nature of this reading''s motivation.').

omega_variable(
    alternative_reading_impact,
    'How would the structural properties of this ''symbolic_archive_reading'' change if a sibling reading (e.g., ''study_as_exercise_reading'') gained widespread dominance?',
    'Comparative analysis of communities where other readings are dominant, examining their engagement patterns, perceived obligations, and resistance levels.',
    'If ''study_as_exercise_reading'' became dominant, this reading''s non-halakhic stance might be marginalized, potentially increasing perceived extractiveness for those who prefer a purely cultural approach, or increasing suppression if the ''study as exercise'' view became prescriptive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_impact, empirical, 'Impact of sibling reading dominance on this constraint''s structure.').

omega_variable(
    halakhic_status_ambiguity,
    'Is the explicit ''no halakhic claim'' stance of this reading a definitive structural feature, or is it implicitly contested by the mere existence of the texts within a halakhic tradition?',
    'Theological and jurisprudential analysis of the concept of ''dormant'' or ''non-binding'' halakha within the tradition, and the role of communal consensus in defining obligation.',
    'If implicitly contested, the ''no halakhic claim'' might be a form of ''soft suppression'' of alternative interpretations, or a subtle form of extraction from those who feel a latent obligation, even if not explicitly stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(halakhic_status_ambiguity, conceptual, 'The location of disagreement regarding the halakhic status of sacrifice law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 80, 0.05).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(sacr_su_t40, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(sacr_su_t80, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 80, 0.05).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'sacrifice_obligation_kernel', each with different structural properties and implications for halakhic obligation. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
