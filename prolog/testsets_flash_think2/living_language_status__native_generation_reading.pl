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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   This constraint defines a language as 'living' exclusively by its
 *   generational transmission as a mother tongue in daily life, explicitly
 *   framing liturgical recitation as insufficient for vitality. This reading
 *   is often adopted by secular nationalist movements seeking to establish
 *   linguistic sovereignty and promote specific forms of cultural identity.
 *   It functions as a Tangled Rope, coordinating efforts for native
 *   transmission while extracting legitimacy and resources from communities
 *   practicing alternative forms of language preservation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.65).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.75).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status: Native Generational Transmission Reading").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '48164aea-0832-4d2f-af8e-c6a43eb6576e').
narrative_ontology:cs_kernel_codification('48164aea-0832-4d2f-af8e-c6a43eb6576e', formalized).
narrative_ontology:cs_authority_grounding('48164aea-0832-4d2f-af8e-c6a43eb6576e', extraction).
narrative_ontology:cs_interpretation_layer_present('48164aea-0832-4d2f-af8e-c6a43eb6576e').
narrative_ontology:cs_reading_relation('48164aea-0832-4d2f-af8e-c6a43eb6576e', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('48164aea-0832-4d2f-af8e-c6a43eb6576e', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('48164aea-0832-4d2f-af8e-c6a43eb6576e', foundational, generational_transmission_is_life).
narrative_ontology:cs_axiom_status(generational_transmission_is_life, holdable).
narrative_ontology:cs_axiom_grounding('48164aea-0832-4d2f-af8e-c6a43eb6576e', generational_transmission_is_life, empirically_contingent).
narrative_ontology:cs_axiom('48164aea-0832-4d2f-af8e-c6a43eb6576e', foundational, mother_tongue_primacy).
narrative_ontology:cs_axiom_status(mother_tongue_primacy, holdable).
narrative_ontology:cs_axiom_grounding('48164aea-0832-4d2f-af8e-c6a43eb6576e', mother_tongue_primacy, conventional).
narrative_ontology:cs_reference_frame('48164aea-0832-4d2f-af8e-c6a43eb6576e', modern_linguistic_vitality_framework).
narrative_ontology:cs_drift_state('48164aea-0832-4d2f-af8e-c6a43eb6576e', contemporary_sociolinguistic_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('48164aea-0832-4d2f-af8e-c6a43eb6576e', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, linguistic_revival_activists).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, cultural_heritage_institutions).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, traditional_religious_authorities).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, linguistic_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, modern_nation_state_ideology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and enforce this definition of a living language, using it to justify policies that prioritize native generational transmission and often delegitimize other forms of language preservation. They gain legitimacy and cultural cohesion from linguistic sovereignty.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    institutional, generational, mobile, national).

% Benefit from the institutional and public support this definition provides to their efforts to revive and normalize languages as mother tongues in daily life. Their work is validated and resourced by this framework.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, linguistic_revival_activists, beneficiary,
    organized, biographical, constrained, national).

% Receive funding and mandates to implement programs that align with this definition, such as language immersion schools or family transmission initiatives. Their institutional mission is reinforced by this framework.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, cultural_heritage_institutions, beneficiary,
    institutional, generational, constrained, national).

% Their centuries-old practices of language preservation through ritual and study are delegitimized and framed as insufficient or even as preserving a 'dead' language. They bear the cost of cultural marginalization and loss of recognition for their efforts.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    powerless, generational, identity_locked, local).

% Their authority over sacred languages, often maintained through liturgical and scholarly traditions, is challenged by this definition. They face pressure to adapt to secular definitions of vitality or risk losing influence and legitimacy in broader society.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, traditional_religious_authorities, payer,
    organized, generational, identity_locked, regional).

% Study the dynamics of language vitality, often providing empirical data and theoretical frameworks that can be used to support or challenge this definition. They analyze its impact without directly participating in its enforcement or extraction.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, sociolinguists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and community efforts, resources, and educational policies towards establishing and maintaining languages as living mother tongues, ensuring their generational transmission in daily life.
% TRANSFER_FUNCTION: Transfers legitimacy, public resources, and cultural capital from traditional/liturgical forms of language preservation to secular, native-transmission-focused initiatives, often at the expense of communities whose languages are not primarily transmitted generationally.
% ABSENT_VOICES: Communities and scholars who advocate for alternative definitions of language vitality, such as those emphasizing literary output, ritual use, or symbolic importance, are often marginalized or excluded from policy-making and public discourse on language status.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the discourse around language vitality would broaden significantly, allowing other forms of preservation (liturgical, literary, symbolic) to gain legitimacy. This would likely shift funding, policy priorities, and public perception, reorganizing how language preservation efforts are conceived and supported globally.
% FOUNDING_PROBLEM: The perceived decline or 'death' of languages due to lack of native generational transmission, often linked to colonial legacies, globalization, and the dominance of lingua francas, threatening cultural distinctiveness and national identity.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic surveys, demographic data, and educational institutions corroborate the ongoing decline of native transmission for many languages. However, the interpretation of this decline as 'death' and the exclusive focus on native transmission are contested by scholars and communities outside the benefiting parties.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because this definition actively delegitimizes and marginalizes other forms of language preservation, effectively extracting their cultural capital and diverting resources. Suppression is also high (0.75) as it requires active enforcement through educational policies, funding decisions, and public discourse to maintain its dominance and suppress alternative views. The theater ratio is low (0.20) because the efforts to promote native transmission are genuinely functional, even if the underlying definition is extractive. Accessibility collapse is moderate (0.60) as it collapses the 'living' status for languages not meeting the criteria, while resistance is high (0.70) due to strong counter-arguments from affected communities and scholars.
 *
 * PERSPECTIVAL GAP:
 *   Secular nationalist movements and linguistic revival activists experience this constraint as a legitimate and necessary framework for language revitalization and national cohesion. In contrast, liturgical-only communities and traditional religious authorities experience it as an extractive force that devalues their heritage and threatens their cultural continuity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movements and linguistic revival activists are primary beneficiaries, gaining legitimacy and resources for their agendas. Cultural heritage institutions also benefit by aligning their missions with this framework. Liturgical-only communities and traditional religious authorities are victims, as their practices are delegitimized and they bear the cost of cultural marginalization. Sociolinguists act as observers, analyzing the constraint's dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction from alternative preservation methods) or a pure Snare (which would ignore the genuine coordination function for native transmission). It highlights the hybrid nature where a coordination story serves to justify asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_as_natural_law_or_construct,
    'Is this definition of a ''living language'' an objective, natural fact about language vitality, or a social construct serving specific political and cultural agendas?',
    'Cross-cultural comparative studies of language maintenance and revitalization, examining outcomes under different definitional frameworks, and historical analysis of the definition''s emergence in relation to nationalist movements.',
    'If a construct, its extractive and suppressive elements are more clearly revealed as policy choices rather than inevitable truths, supporting reclassification towards Snare. If a natural law, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_as_natural_law_or_construct, conceptual, 'Ambiguity regarding the naturalness vs. constructedness of the ''living language'' definition.').

omega_variable(
    impact_on_liturgical_communities,
    'Does the delegitimization of liturgical preservation truly lead to the ''death'' of a language for those communities, or does it merely shift its social function and perceived status?',
    'Longitudinal ethnographic studies of communities whose languages are primarily maintained liturgically, assessing their internal vitality, cultural transmission, and resilience despite external delegitimization.',
    'If internal vitality persists, the effective extraction from these communities might be lower than perceived, suggesting the constraint''s impact is more about external status and resource allocation than existential threat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_liturgical_communities, empirical, 'Ambiguity about the actual existential impact of this definition on liturgical-only language communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1900, living_language_status__native_generation_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(livi_tr_t1925, living_language_status__native_generation_reading, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(livi_tr_t1950, living_language_status__native_generation_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(livi_tr_t1975, living_language_status__native_generation_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(livi_tr_t2000, living_language_status__native_generation_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(livi_tr_t2025, living_language_status__native_generation_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(livi_be_t1900, living_language_status__native_generation_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(livi_be_t1925, living_language_status__native_generation_reading, base_extractiveness, 1925, 0.5).
narrative_ontology:measurement(livi_be_t1950, living_language_status__native_generation_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(livi_be_t1975, living_language_status__native_generation_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(livi_be_t2000, living_language_status__native_generation_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(livi_be_t2025, living_language_status__native_generation_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1900, living_language_status__native_generation_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(livi_su_t1925, living_language_status__native_generation_reading, suppression_requirement, 1925, 0.6).
narrative_ontology:measurement(livi_su_t1950, living_language_status__native_generation_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(livi_su_t1975, living_language_status__native_generation_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement(livi_su_t2000, living_language_status__native_generation_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(livi_su_t2025, living_language_status__native_generation_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'living_language_status' kernel, each representing a distinct structural claim about language vitality. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
