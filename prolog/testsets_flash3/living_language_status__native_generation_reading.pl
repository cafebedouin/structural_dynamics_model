% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Living Language Status: Native Generational Transmission Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a 'living language' exclusively by native
 *   generational transmission in daily life, dismissing other forms of
 *   linguistic continuity (like liturgical use or literary production) as
 *   insufficient for vitality. It is a reading of the
 *   'living_language_status' kernel, instantiated from the perspective of
 *   secular nationalist movements. The constraint functions as a Tangled
 *   Rope, coordinating national identity and language policy while extracting
 *   legitimacy and resources from communities whose linguistic practices do
 *   not conform to this narrow definition.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movements: Agenda-setter (institutional/generational) — defines and enforces the standard.
 *   - linguistic_revitalization_programs: Beneficiary (organized/generational) — receives support based on this definition.
 *   - liturgical_only_communities: Payer (powerless/generational) — marginalized and delegitimized by this definition.
 *   - diaspora_communities: Payer (moderate/biographical) — face pressure and judgment for non-native transmission.
 *   - sociolinguists: Observer (analytical/generational) — analyze the impact of this definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.45).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.6).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status: Native Generational Transmission Reading").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '6f408874-fa52-4edc-be8b-36576158561a').
narrative_ontology:cs_kernel_codification('6f408874-fa52-4edc-be8b-36576158561a', formalized).
narrative_ontology:cs_authority_grounding('6f408874-fa52-4edc-be8b-36576158561a', extraction).
narrative_ontology:cs_interpretation_layer_present('6f408874-fa52-4edc-be8b-36576158561a').
narrative_ontology:cs_reading_relation('6f408874-fa52-4edc-be8b-36576158561a', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('6f408874-fa52-4edc-be8b-36576158561a', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('6f408874-fa52-4edc-be8b-36576158561a', foundational, generational_transmission_is_sole_vitality_criterion).
narrative_ontology:cs_axiom_status(generational_transmission_is_sole_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('6f408874-fa52-4edc-be8b-36576158561a', generational_transmission_is_sole_vitality_criterion, conventional).
narrative_ontology:cs_axiom('6f408874-fa52-4edc-be8b-36576158561a', secondary, liturgical_use_is_not_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_is_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('6f408874-fa52-4edc-be8b-36576158561a', liturgical_use_is_not_vitality, conventional).
narrative_ontology:cs_reference_frame('6f408874-fa52-4edc-be8b-36576158561a', modern_nation_state_linguistic_sovereignty).
narrative_ontology:cs_drift_state('6f408874-fa52-4edc-be8b-36576158561a', contemporary_multicultural_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6f408874-fa52-4edc-be8b-36576158561a', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, linguistic_revitalization_programs).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements define national identity through a 'living' language, actively promoting its use in daily life and education. They gain legitimacy and political power by framing their language as a marker of sovereignty and vitality, often at the expense of other linguistic forms.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    institutional, generational, constrained, national).

% These programs receive funding and institutional support based on the premise that a language must be natively transmitted to be truly alive. They benefit from the policy and cultural emphasis on generational transmission, even if their efforts face significant challenges.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, linguistic_revitalization_programs, beneficiary,
    organized, generational, constrained, national).

% These communities maintain their language primarily through religious texts and rituals. They are often marginalized or delegitimized by the 'native generation' reading, which dismisses their linguistic practices as insufficient for 'vitality,' leading to cultural and social pressure.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    powerless, generational, identity_locked, local).

% Many diaspora communities struggle with generational transmission of their ancestral language, often maintaining it through cultural institutions, media, or limited home use. This reading can impose a sense of linguistic failure or inadequacy on them, despite their efforts at preservation.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_communities, payer,
    moderate, biographical, constrained, global).

% Academically study language vitality, often engaging with various definitions. They observe the social and political impacts of this reading, analyzing its effects on language policy and community identity without directly participating in its enforcement or benefiting from its operation.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, sociolinguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national identity and educational policy around a specific definition of linguistic vitality, mobilizing resources for language teaching and cultural promotion based on native generational transmission.
% TRANSFER_FUNCTION: Transfers cultural legitimacy, institutional support, and national identity from communities that prioritize native generational transmission to those that do not, often leading to the marginalization of liturgical or literary-only language practices.
% ABSENT_VOICES: Linguistic minorities within nation-states, whose languages may not meet the 'native generation' criteria, are often excluded from policy discussions and resource allocation, despite their unique forms of linguistic vitality.
% DISAPPEARANCE_RATIONALE: If this definition vanished, national language policies would need to be fundamentally re-evaluated, potentially leading to more inclusive definitions of linguistic vitality and a redistribution of resources towards diverse forms of language maintenance. The legitimacy of many nationalist projects would be challenged.
% FOUNDING_PROBLEM: The need to define and promote a 'national' language as a cornerstone of modern nation-state identity, distinguishing it from colonial or religious linguistic hegemonies.
% FOUNDING_PROBLEM_CORROBORATION: Secular nationalist movements and linguistic revitalization programs attest to the ongoing need for a clear definition of a 'living' national language to counter historical linguistic suppression. Sociolinguists, from an analytical seat, corroborate the historical context of nation-building and language standardization, even if they critique the specific definition.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).
:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it imposes a specific standard, it also provides a framework for language revitalization. Suppression (0.6) is significant, as it actively delegitimizes and marginalizes alternative forms of language maintenance, often backed by state policy. The theater ratio (0.1) is low, indicating that the efforts to promote native transmission are largely genuine, even if the definition itself is contested. The historical measurements show a rise in extractiveness and suppression as nationalist movements gained strength and institutionalized this definition, with a slight decline in recent years due to increased academic and community resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular nationalist movements, this constraint is a necessary tool for cultural preservation and national cohesion. From the perspective of liturgical-only or diaspora communities, it is an arbitrary and harmful imposition that devalues their linguistic heritage and efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movements and linguistic revitalization programs are beneficiaries, as this definition grants them legitimacy and resources. Liturgical-only and diaspora communities are payers, as they bear the cost of marginalization and pressure to conform. Sociolinguists act as observers, analyzing the structural impacts.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a simple 'Rope' (pure coordination) by highlighting the asymmetric extraction from communities that do not meet the 'native generation' criteria. It also avoids classifying it as a 'Snare' (pure extraction) by acknowledging the genuine coordination function for national identity and language revitalization, even if that function is selectively applied. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating an ongoing dispute over its original mandate and current function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality_ambiguity,
    'Is ''native generational transmission'' the sole valid criterion for linguistic vitality, or do other forms of continuous use (e.g., liturgical, literary) also constitute a ''living'' language?',
    'A shift in academic consensus or policy frameworks to a more inclusive, multi-criteria definition of language vitality, acknowledging diverse forms of linguistic continuity.',
    'If resolved towards a broader definition, the constraint''s extractiveness and suppression would decrease, as marginalized communities would gain legitimacy. This could shift the classification towards a Rope or even a Piton if the enforcement of the narrow definition atrophies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality_ambiguity, conceptual, 'Ambiguity in the core definition of a ''living language'' and its criteria.').

omega_variable(
    secular_nationalist_intent,
    'To what extent is the emphasis on ''native generational transmission'' genuinely about linguistic vitality, versus a tool for constructing a homogenous national identity and marginalizing minority groups?',
    'Historical analysis of policy outcomes, examination of resource allocation patterns, and ethnographic studies of affected communities to discern the primary drivers and effects of the policy.',
    'If primarily a tool for national identity construction and marginalization, the constraint''s extractiveness and suppression would be re-evaluated as higher, potentially shifting it closer to a Snare. If genuinely about linguistic health, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_nationalist_intent, empirical, 'The underlying intent and primary function of the ''native generation'' definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1900, living_language_status__native_generation_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(livi_tr_t1930, living_language_status__native_generation_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(livi_tr_t1960, living_language_status__native_generation_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(livi_tr_t1990, living_language_status__native_generation_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(livi_tr_t2024, living_language_status__native_generation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t1900, living_language_status__native_generation_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(livi_be_t1930, living_language_status__native_generation_reading, base_extractiveness, 1930, 0.38).
narrative_ontology:measurement(livi_be_t1960, living_language_status__native_generation_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(livi_be_t1990, living_language_status__native_generation_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(livi_be_t2024, living_language_status__native_generation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1900, living_language_status__native_generation_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(livi_su_t1930, living_language_status__native_generation_reading, suppression_requirement, 1930, 0.5).
narrative_ontology:measurement(livi_su_t1960, living_language_status__native_generation_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(livi_su_t1990, living_language_status__native_generation_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(livi_su_t2024, living_language_status__native_generation_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'living_language_status' kernel, focusing on native generational transmission. It is structurally distinct from the 'liturgical_preservation_reading' and 'literary_continuity_reading' due to differing criteria for linguistic vitality and distinct beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
