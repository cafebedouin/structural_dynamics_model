% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity via Liturgical Preservation
 *   domain: sociolinguistics/religious/cultural
 *
 * SUMMARY:
 *   This constraint describes the continuity of Hebrew as maintained through
 *   its role in religious ritual and the transmission of sacred texts within
 *   traditional Jewish communities. It asserts that Hebrew 'lives' through
 *   this symbolic preservation, independent of daily generative use by native
 *   speakers. The constraint is claimed as a Rope by its adherents, as it
 *   coordinates a vital cultural and religious function, but its metrics
 *   reflect the significant effort and social pressure required to maintain
 *   it against secularizing forces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.65).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.55).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity via Liturgical Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/religious/cultural").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'b03843ba-ac6c-4a83-82b0-4325ca653875').
narrative_ontology:cs_kernel_codification('b03843ba-ac6c-4a83-82b0-4325ca653875', fixed_text).
narrative_ontology:cs_authority_grounding('b03843ba-ac6c-4a83-82b0-4325ca653875', lineage).
narrative_ontology:cs_interpretation_layer_present('b03843ba-ac6c-4a83-82b0-4325ca653875').
narrative_ontology:cs_reading_relation('b03843ba-ac6c-4a83-82b0-4325ca653875', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('b03843ba-ac6c-4a83-82b0-4325ca653875', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('b03843ba-ac6c-4a83-82b0-4325ca653875', foundational, hebrew_sacred_language_divine_origin).
narrative_ontology:cs_axiom_status(hebrew_sacred_language_divine_origin, holdable).
narrative_ontology:cs_axiom_grounding('b03843ba-ac6c-4a83-82b0-4325ca653875', hebrew_sacred_language_divine_origin, theological).
narrative_ontology:cs_axiom('b03843ba-ac6c-4a83-82b0-4325ca653875', foundational, ritual_textual_transmission_sufficient_for_continuity).
narrative_ontology:cs_axiom_status(ritual_textual_transmission_sufficient_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b03843ba-ac6c-4a83-82b0-4325ca653875', ritual_textual_transmission_sufficient_for_continuity, conventional).
narrative_ontology:cs_reference_frame('b03843ba-ac6c-4a83-82b0-4325ca653875', unbroken_chain_of_transmission_and_recitation).
narrative_ontology:cs_drift_state('b03843ba-ac6c-4a83-82b0-4325ca653875', post_enlightenment_secularization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b03843ba-ac6c-4a83-82b0-4325ca653875', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, traditional_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_scholars).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, liturgical_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_forces).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, assimilating_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities actively maintain and transmit Hebrew through ritual, prayer, and textual study. Their identity is deeply intertwined with this practice, and they benefit from the continuity of their sacred language and heritage. Exiting means a fundamental shift in religious and cultural identity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, traditional_jewish_communities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, traditional_jewish_communities, beneficiary).

% Scholars dedicate their careers to the study, interpretation, and transmission of Hebrew texts and liturgical traditions. They are key enforcers and beneficiaries of this constraint, as their professional identity and authority are grounded in its persistence.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_scholars, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, religious_scholars, beneficiary).

% Synagogues, yeshivas, and other religious educational bodies provide the infrastructure for ritual recitation and textual transmission. They benefit from the continued adherence to these practices, which ensures their institutional relevance and funding.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, liturgical_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, liturgical_institutions, beneficiary).

% These forces, often cultural or governmental, promote assimilation into dominant languages and secular lifestyles, implicitly or explicitly de-emphasizing the importance of traditional religious language use. They bear the 'cost' of resistance from traditional communities.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_forces, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, secularizing_forces, excluded).

% Individuals who, due to social or economic pressures, reduce or abandon their use of Hebrew in ritual and study. They bear the social cost of deviating from communal norms but gain integration into broader society. The constraint extracts their time and effort if they try to maintain it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, assimilating_individuals, payer,
    powerless, biographical, mobile, local).

% Advocates for a fully generative, native-speaker Hebrew (like modern Israeli Hebrew) who believe liturgical preservation alone is insufficient for a language to truly 'live'. They are excluded from the dominant discourse of this constraint, which prioritizes ritual and text.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, language_revitalization_activists, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, traditional_jewish_communities).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of Hebrew as a sacred language and textual tradition across generations and diasporic communities, ensuring continuity of religious practice and cultural identity through shared ritual and study.
% TRANSFER_FUNCTION: Transfers time, effort, and intellectual resources from individuals and communities into ritual observance, textual study, and institutional maintenance, ensuring the intergenerational transmission of Hebrew's sacred and cultural functions.
% ABSENT_VOICES: Advocates for a purely secular, generative Hebrew (like modern Israeli Hebrew) or those who view Hebrew primarily as a historical artifact rather than a living sacred language. They would argue for different metrics of vitality and different methods of preservation, but are excluded by the constraint's framing.
% DISAPPEARANCE_RATIONALE: If the commitment to liturgical preservation and textual transmission vanished overnight, Hebrew would cease to function as a sacred language for many communities, leading to a profound shift in religious practice, cultural identity, and the relationship to foundational texts. The global network of traditional Jewish life would be fundamentally altered.
% FOUNDING_PROBLEM: The historical threat of linguistic and cultural assimilation in diaspora, and the desire to maintain a direct, unbroken link to sacred texts, ancestral traditions, and a shared religious identity across dispersed communities.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders, historians of Jewish culture, and sociolinguists studying language death and revitalization attest to the historical and ongoing challenges of maintaining minority languages and traditions in diaspora. Scholarly works on Jewish history and ethnography corroborate the persistent threat of assimilation.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.65) because maintaining this tradition demands substantial time, effort, and conformity from individuals and communities, often in the face of modernizing pressures. Suppression (0.55) is primarily social and cultural, exerted through communal norms and religious education, rather than legal coercion. Theater ratio is low (0.20) because the ritual and textual practices are genuinely functional for the stated goal of preservation, not merely performative. Accessibility collapse is moderate (0.40) as alternatives like assimilation or adopting other languages exist, but are often seen as undesirable by adherents. Resistance (0.50) comes from secularizing trends and individuals seeking to integrate into broader society.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional communities, this constraint is a vital Rope, coordinating the preservation of an irreplaceable heritage. From the perspective of secularizing individuals, it can feel like a Snare, demanding adherence to practices that seem increasingly irrelevant or burdensome. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional Jewish communities, religious scholars, and liturgical institutions are the primary beneficiaries and agenda-setters, as they actively maintain and derive identity and purpose from this continuity. Secularizing forces and assimilating individuals are the targets, as the constraint demands effort and conformity from them, or they bear the 'cost' of resisting its influence. Language revitalization activists are excluded, as their vision for Hebrew's 'life' differs fundamentally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_vitality_definition,
    'What constitutes ''living'' for a language, especially a sacred one? Does it require native speakers and daily generative use, or is ritual recitation and textual study sufficient?',
    'Philosophical and sociolinguistic debate on definitions of language vitality, combined with community self-definition and functional analysis of language use.',
    'If generative use is deemed essential for a language to ''live,'' this constraint''s claim of ''continuity'' is weakened, potentially reclassifying it as a Piton (theatrical maintenance) or Snare (extracting effort for a non-living outcome). If ritual/textual use is sufficient, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_vitality_definition, conceptual, 'Ambiguity in the definition of language vitality for sacred languages.').

omega_variable(
    secularization_impact_on_transmission,
    'To what extent do modern secularizing forces genuinely threaten the continuity of Hebrew via liturgical preservation, versus merely shifting its social context?',
    'Longitudinal ethnographic studies and demographic analysis of traditional Jewish communities, tracking adherence to ritual and textual study over generations.',
    'If secularization is found to severely erode the capacity for transmission, the constraint''s suppression and extractiveness may be higher than currently estimated, reflecting the increased effort required to maintain it. If the tradition proves resilient, these metrics may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularization_impact_on_transmission, empirical, 'Empirical impact of secularization on the efficacy of liturgical preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 1000, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1000, hebrew_continuity__liturgical_preservation, theater_ratio, 1000, 0.18).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_continuity__liturgical_preservation, theater_ratio, 1200, 0.19).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_continuity__liturgical_preservation, theater_ratio, 1400, 0.19).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_continuity__liturgical_preservation, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_continuity__liturgical_preservation, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_continuity__liturgical_preservation, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1000, hebrew_continuity__liturgical_preservation, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(hebr_be_t1200, hebrew_continuity__liturgical_preservation, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(hebr_be_t1400, hebrew_continuity__liturgical_preservation, base_extractiveness, 1400, 0.63).
narrative_ontology:measurement(hebr_be_t1600, hebrew_continuity__liturgical_preservation, base_extractiveness, 1600, 0.64).
narrative_ontology:measurement(hebr_be_t1800, hebrew_continuity__liturgical_preservation, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(hebr_be_t2000, hebrew_continuity__liturgical_preservation, base_extractiveness, 2000, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1000, hebrew_continuity__liturgical_preservation, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(hebr_su_t1200, hebrew_continuity__liturgical_preservation, suppression_requirement, 1200, 0.52).
narrative_ontology:measurement(hebr_su_t1400, hebrew_continuity__liturgical_preservation, suppression_requirement, 1400, 0.53).
narrative_ontology:measurement(hebr_su_t1600, hebrew_continuity__liturgical_preservation, suppression_requirement, 1600, 0.54).
narrative_ontology:measurement(hebr_su_t1800, hebrew_continuity__liturgical_preservation, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(hebr_su_t2000, hebrew_continuity__liturgical_preservation, suppression_requirement, 2000, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel, each representing a distinct structural claim about how Hebrew lives. This reading focuses on liturgical and textual preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
