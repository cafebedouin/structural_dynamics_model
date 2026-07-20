% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native-Generation Criterion for Language Vitality
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This constraint instantiates the native_generation_reading of the
 *   living_language_status kernel. It defines a language as living
 *   exclusively through generational mother-tongue transmission in daily
 *   life, delegitimizing liturgical continuity as preservation of a corpse.
 *   The criterion serves secular nationalist movements by grounding
 *   linguistic sovereignty in a demographically verifiable standard, while
 *   extracting recognition and political standing from liturgical-only
 *   communities. The reading is structurally distinct from its siblings: it
 *   forecloses the liturgical_preservation_reading and coexists with the
 *   literary_continuity_reading as a competing paradigm in sociolinguistics.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movement: Primary beneficiary (organized/identity_locked) â gains territorial legitimacy via linguistic sovereignty
 *   - liturgical_only_communities: Primary victim (powerless/identity_locked) â bears delegitimization and exclusion from recognition
 *   - state_language_policy_institution: Agenda setter (institutional/constrained) â operationalizes the criterion in census and education policy
 *   - academic_sociolinguists: Observer (organized/analytical) â produces the frameworks that naturalize or critique the standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.58).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.62).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Generation Criterion for Language Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '37401d65-b2e1-4aa3-8f10-b9e9d36d8c89').
narrative_ontology:cs_kernel_codification('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', formalized).
narrative_ontology:cs_authority_grounding('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', extraction).
narrative_ontology:cs_interpretation_layer_present('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89').
narrative_ontology:cs_reading_relation('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', foundational, native_mother_tongue_transmission_defines_vitality).
narrative_ontology:cs_axiom_status(native_mother_tongue_transmission_defines_vitality, holdable).
narrative_ontology:cs_axiom_grounding('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', native_mother_tongue_transmission_defines_vitality, conventional).
narrative_ontology:cs_axiom('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', foundational, liturgical_use_is_non_vital_preservation).
narrative_ontology:cs_axiom_status(liturgical_use_is_non_vital_preservation, holdable).
narrative_ontology:cs_axiom_grounding('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', liturgical_use_is_non_vital_preservation, conventional).
narrative_ontology:cs_reference_frame('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', native_sovereignty_framework).
narrative_ontology:cs_drift_state('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', post_decolonization_multilingual_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37401d65-b2e1-4aa3-8f10-b9e9d36d8c89', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives territorial and political legitimacy from the existence of a demographically verifiable native-speaking population. The native-generation criterion converts linguistic demography into sovereignty claims, marginalizing competing religious-national narratives that rest on liturgical continuity.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movement, beneficiary,
    organized, generational, identity_locked, national).

% Maintains sacred texts, ritual recitation, and religious study in the language. Under the native-generation criterion, this continuity is reclassified as preservation of a non-viable entity, denying the community institutional recognition, funding, and political standing accorded to living language communities.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    powerless, generational, identity_locked, local).

% Operates census categories, education standards, and funding eligibility that embed the native-generation definition. Revising the criterion would trigger political crises over sovereignty and resource allocation, constraining institutional exit.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_language_policy_institution, agenda_setter,
    institutional, generational, constrained, national).

% Develop and apply language vitality indices that often privilege native-speaker counts. Some frameworks reinforce the native-generation standard while others critique its political embeddedness; their analytical position permits mobility across theoretical frameworks.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, academic_sociolinguists, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a demographically verifiable standard for distinguishing living from dead languages, enabling coordinated state language policy, census classification, and international funding allocation around a single observable criterion.
% TRANSFER_FUNCTION: Moves political recognition, institutional funding, and cultural legitimacy from liturgical-only communities to secular nationalist movements by conditioning living-language status exclusively on generational mother-tongue transmission in daily life.
% ABSENT_VOICES: Religious scholars, liturgical authorities, and diasporic communities who treat sacred continuity as vitality; they are excluded from standard-setting bodies that operationalize language vitality metrics.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion vanished, census categories would reorganize, education budgets would shift toward liturgical and heritage programs, and the territorial legitimacy claims of secular nationalist movements would lose their primary demographic anchor.
% FOUNDING_PROBLEM: How to establish objective, territorially grounded criteria for language vitality that can anchor national sovereignty and justify state language policy in contexts where religious, diasporic, or liturgical competing claims challenge territorial coherence.
% FOUNDING_PROBLEM_CORROBORATION: Secular nationalist movements and state institutions attest the problem remains live. International religious bodies, minority-rights organizations, and post-colonial linguistic scholars from outside the beneficiary set attest the criterion is a constructed political tool rather than a neutral response to a coordination problem; anthropological studies document liturgical communities experience the standard as delegitimizing.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint moves substantial recognition and resources away from liturgical communities toward native-speaking populations and their political representatives, yet it simultaneously coordinates language policy around a verifiable standard. Suppression (0.62) reflects the institutional marginalization of liturgical definitions in state and international vitality frameworks. Theater_ratio (0.42) captures the performative maintenance of native-speaker census categories and vitality indices that persist beyond their empirical utility, particularly as digital and diasporic communities complicate the daily-life criterion.
 *
 * PERSPECTIVAL GAP:
 *   From the nationalist seat, the criterion is a necessary coordination device for linguistic sovereignty and state-building; from the liturgical seat, it is an enforced extraction of cultural standing that renders their continuity invisible. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular nationalist movement sits near the beneficiary end because the constraint subsidizes sovereignty claims with an objective-seeming demographic criterion. Liturgical communities sit near the target end because the constraint directly extracts their standing and reclassifies their practice as non-vital. State institutions sit mid-range because they are both enforcers and prisoners of the nationalist frame they administer. Academic observers occupy an analytical position with mobility across frameworks, yielding a moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate â to distinguish living from dead languages for policy purposes â remains live in state and international practice, so mandatrophy is not declared. However, the rising theater_ratio over the interval signals drift toward performative maintenance, suggesting the criterion's enforcement now exceeds its original coordinating function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_empirical_vs_conventional,
    'Is the native-generation criterion an empirically discovered feature of language vitality or a conventional political construct enacted by nationalist institutions?',
    'Cross-cultural comparison of language vitality assessments and historical analysis of the criterion''s emergence in 19th-century nationalist linguistics versus pre-nationalist frameworks.',
    'If the criterion is purely conventional, the constraint''s extraction is political domination masquerading as scientific taxonomy; if it tracks genuine empirical regularities, part of the extraction may be treated as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_empirical_vs_conventional, conceptual, 'Whether the native-generation standard is empirical fact or political convention').

omega_variable(
    liturgical_community_exclusion_mechanism,
    'Are liturgical communities excluded from living-language status by structural barriers (state recognition systems) or by internalized framing (they accept the corpse narrative)?',
    'Ethnographic study of liturgical community self-classification and mobilization: do they claim vitality or accept the native-generation verdict?',
    'If internalized, effective suppression exceeds the structural measure because the target carries the constraint with them; if purely structural, removal of policy barriers may permit rapid reclamation of standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_community_exclusion_mechanism, empirical, 'Structural versus internalized suppression in liturgical communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(livi_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(livi_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(livi_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(livi_tr_t80, living_language_status__native_generation_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(livi_tr_t100, living_language_status__native_generation_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(livi_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(livi_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(livi_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(livi_be_t80, living_language_status__native_generation_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(livi_be_t100, living_language_status__native_generation_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(livi_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(livi_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(livi_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(livi_su_t80, living_language_status__native_generation_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(livi_su_t100, living_language_status__native_generation_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the native_generation_reading of the living_language_status kernel. It is decomposed from the liturgical_preservation_reading and literary_continuity_reading per the Îµ-invariance principle because the three readings instantiate structurally distinct constraints with different beneficiary/victim structures and Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
