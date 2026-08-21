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
 *   human_readable: Living Language Status: Native Generational Transmission Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' exclusively by its
 *   generational transmission as a mother tongue in daily life, explicitly
 *   dismissing other forms of preservation, such as liturgical recitation, as
 *   mere 'preservation of a corpse.' It functions as one reading of the
 *   broader 'living_language_status' kernel, primarily benefiting secular
 *   nationalist movements seeking linguistic sovereignty and cultural
 *   revitalization activists. The constraint's 'tangled_rope' classification
 *   reflects its dual nature: it coordinates efforts for native transmission
 *   while simultaneously extracting legitimacy and resources from communities
 *   practicing other forms of language preservation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.65).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.75).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status: Native Generational Transmission Reading").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '200a5c3e-67a7-47cd-a941-da9dab2a7544').
narrative_ontology:cs_kernel_codification('200a5c3e-67a7-47cd-a941-da9dab2a7544', formalized).
narrative_ontology:cs_authority_grounding('200a5c3e-67a7-47cd-a941-da9dab2a7544', extraction).
narrative_ontology:cs_interpretation_layer_present('200a5c3e-67a7-47cd-a941-da9dab2a7544').
narrative_ontology:cs_reading_relation('200a5c3e-67a7-47cd-a941-da9dab2a7544', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('200a5c3e-67a7-47cd-a941-da9dab2a7544', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('200a5c3e-67a7-47cd-a941-da9dab2a7544', foundational, generational_transmission_is_sole_vitality_criterion).
narrative_ontology:cs_axiom_status(generational_transmission_is_sole_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('200a5c3e-67a7-47cd-a941-da9dab2a7544', generational_transmission_is_sole_vitality_criterion, conventional).
narrative_ontology:cs_axiom('200a5c3e-67a7-47cd-a941-da9dab2a7544', foundational, mother_tongue_acquisition_is_definitional).
narrative_ontology:cs_axiom_status(mother_tongue_acquisition_is_definitional, holdable).
narrative_ontology:cs_axiom_grounding('200a5c3e-67a7-47cd-a941-da9dab2a7544', mother_tongue_acquisition_is_definitional, conventional).
narrative_ontology:cs_reference_frame('200a5c3e-67a7-47cd-a941-da9dab2a7544', modern_nation_state_linguistic_sovereignty).
narrative_ontology:cs_drift_state('200a5c3e-67a7-47cd-a941-da9dab2a7544', contemporary_multiculturalism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('200a5c3e-67a7-47cd-a941-da9dab2a7544', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, cultural_revitalization_activists).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, literary_language_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements define national identity through linguistic sovereignty, promoting the idea that a 'true' national language must be transmitted natively and generationally. They gain political legitimacy and cultural capital by framing other forms of language preservation as insufficient or 'dead'.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    institutional, generational, constrained, national).

% Communities whose language is primarily preserved through religious ritual and study, but not as a daily mother tongue. This constraint delegitimizes their efforts, framing their language as a 'corpse' and undermining their cultural claims, despite centuries of continuous use.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    powerless, civilizational, identity_locked, local).

% Academics who study language vitality, endangerment, and revitalization. They may critically analyze or endorse this definition, influencing policy and public discourse, but are not directly subject to its extractive force.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, linguistic_scholars, observer,
    analytical, biographical, analytical, global).

% Activists working to revive or strengthen languages through native transmission. This definition provides a strong justification for their work and helps secure funding and institutional support, often at the expense of other preservation models.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, cultural_revitalization_activists, beneficiary,
    organized, biographical, constrained, national).

% Advocates who believe a language's vitality is demonstrated by its continued use in new literary, philosophical, and intellectual production, even if not widely spoken as a mother tongue. This constraint dismisses their criteria, framing their efforts as insufficient for 'living' status.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, literary_language_advocates, payer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate and prioritize efforts and resources towards the generational transmission of languages as mother tongues, aligning with a specific vision of linguistic vitality.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and political focus from other forms of language preservation (e.g., liturgical, literary) to those emphasizing native, daily-life transmission. It also transfers cultural authority to groups promoting this definition.
% ABSENT_VOICES: Advocates for broader definitions of language vitality, including those focused on literary output, ritual use, or community identity, whose perspectives are explicitly rejected by this constraint's narrow criteria.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the criteria for language vitality would become much more diverse and inclusive. Communities currently delegitimized would find their preservation efforts recognized, shifting resources, academic discourse, and political support towards a pluralistic understanding of linguistic life.
% FOUNDING_PROBLEM: To establish a clear, measurable, and politically useful criterion for language vitality that supports nation-state building and cultural sovereignty, distinguishing 'living' languages from those preserved only in historical or ritual contexts.
% FOUNDING_PROBLEM_CORROBORATION: While secular nationalist movements and some revitalization groups strongly attest to the problem's live status, sociolinguists and cultural anthropologists often corroborate the *existence* of the debate over language vitality criteria, even if they contest the exclusivity or political motivations of this specific definition. Historical records show this debate emerging with modern nationalism.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is moderate-high because this definition actively delegitimizes and disempowers communities whose language preservation methods do not fit the 'native generational transmission' model, effectively extracting their cultural capital and claims to vitality. Suppression (0.75) is high because the constraint requires active promotion and enforcement of this narrow definition, often through educational policies and public discourse, to suppress alternative understandings of language vitality. The 'corpse' framing is a direct suppressive act. Theater ratio is low (0.15) as the constraint is a definitional claim, not a performance; its function is to define and categorize, which it does directly. Accessibility collapse is high (0.7) because it severely limits the pathways to 'living' status, collapsing alternatives. Resistance is moderate (0.6) from communities whose practices are delegitimized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular nationalist movements, this constraint is a necessary coordination mechanism for national identity and linguistic health. From the perspective of liturgical communities, it is a deeply extractive and suppressive act that denies the living reality of their language and culture. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movements and cultural revitalization activists are primary beneficiaries (low d) as this definition provides a powerful ideological tool for their goals, justifying resource allocation and policy. Liturgical-only communities and literary language advocates are targets (high d) because their forms of language preservation are explicitly devalued and delegitimized, leading to a loss of status and resources. Linguistic scholars act as observers, analyzing the impact and validity of such definitions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent definition of language vitality, or is it primarily a political reading of the ''living_language_status'' kernel?',
    'Analysis of the historical and political context of its emergence, particularly its correlation with nation-state formation and linguistic nationalism.',
    'If primarily a political reading, its classification as a ''tangled_rope'' is reinforced, highlighting the instrumental use of definitional claims for extraction. If it were a purely academic definition, its extractiveness might be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the political vs. academic nature of the definition.').

omega_variable(
    impact_on_language_policy,
    'To what extent does this specific definition of ''living language'' actually shape national language policies and resource allocation, versus being a rhetorical claim?',
    'Empirical study of national language academies, educational curricula, and funding decisions in countries where this definition is prominent.',
    'If it strongly shapes policy, the measured extractiveness and suppression are accurate reflections of its real-world impact. If it''s largely rhetorical, the effective extraction might be lower than the base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_language_policy, empirical, 'Assesses the real-world policy impact of the definition.').

omega_variable(
    alternative_vitality_metrics,
    'Are there alternative, equally robust metrics for language vitality that do not rely solely on native generational transmission, and how would their adoption change the classification?',
    'Development and adoption of sociolinguistic frameworks that incorporate literary output, ritual use, community identity, and digital presence as valid indicators of vitality.',
    'If such alternatives gain traction, the current constraint''s suppression and extractiveness would decrease, potentially reclassifying it from a ''tangled_rope'' to a ''rope'' or even a ''piton'' as its exclusive claim atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_vitality_metrics, conceptual, 'Examines the impact of alternative definitions of language vitality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t2000, living_language_status__native_generation_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(livi_tr_t2010, living_language_status__native_generation_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(livi_tr_t2020, living_language_status__native_generation_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(livi_tr_t2030, living_language_status__native_generation_reading, theater_ratio, 2030, 0.15).
narrative_ontology:measurement(livi_tr_t2040, living_language_status__native_generation_reading, theater_ratio, 2040, 0.15).
narrative_ontology:measurement(livi_tr_t2050, living_language_status__native_generation_reading, theater_ratio, 2050, 0.15).

% Extraction over time
narrative_ontology:measurement(livi_be_t2000, living_language_status__native_generation_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(livi_be_t2010, living_language_status__native_generation_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(livi_be_t2020, living_language_status__native_generation_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(livi_be_t2030, living_language_status__native_generation_reading, base_extractiveness, 2030, 0.63).
narrative_ontology:measurement(livi_be_t2040, living_language_status__native_generation_reading, base_extractiveness, 2040, 0.64).
narrative_ontology:measurement(livi_be_t2050, living_language_status__native_generation_reading, base_extractiveness, 2050, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t2000, living_language_status__native_generation_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(livi_su_t2010, living_language_status__native_generation_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(livi_su_t2020, living_language_status__native_generation_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(livi_su_t2030, living_language_status__native_generation_reading, suppression_requirement, 2030, 0.73).
narrative_ontology:measurement(livi_su_t2040, living_language_status__native_generation_reading, suppression_requirement, 2040, 0.74).
narrative_ontology:measurement(livi_su_t2050, living_language_status__native_generation_reading, suppression_requirement, 2050, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'living_language_status' kernel. Each reading offers a distinct definition of language vitality, with different beneficiaries and victims, and is modeled as a separate constraint. This reading (native_generation_reading) explicitly forecloses the validity of the other two as criteria for 'living' status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
