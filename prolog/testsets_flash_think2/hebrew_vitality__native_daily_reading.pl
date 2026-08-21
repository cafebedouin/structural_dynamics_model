% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality: Native Daily Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.8).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.85).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality: Native Daily Reading").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '42632fab-602c-408d-ae42-24690bb9bea8').
narrative_ontology:cs_kernel_codification('42632fab-602c-408d-ae42-24690bb9bea8', formalized).
narrative_ontology:cs_authority_grounding('42632fab-602c-408d-ae42-24690bb9bea8', extraction).
narrative_ontology:cs_interpretation_layer_present('42632fab-602c-408d-ae42-24690bb9bea8').
narrative_ontology:cs_reading_relation('42632fab-602c-408d-ae42-24690bb9bea8', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('42632fab-602c-408d-ae42-24690bb9bea8', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('42632fab-602c-408d-ae42-24690bb9bea8', foundational, vitality_equals_native_vernacular_use).
narrative_ontology:cs_axiom_status(vitality_equals_native_vernacular_use, holdable).
narrative_ontology:cs_axiom_grounding('42632fab-602c-408d-ae42-24690bb9bea8', vitality_equals_native_vernacular_use, conventional).
narrative_ontology:cs_axiom('42632fab-602c-408d-ae42-24690bb9bea8', secondary, ritual_recitation_is_not_life).
narrative_ontology:cs_axiom_status(ritual_recitation_is_not_life, holdable).
narrative_ontology:cs_axiom_grounding('42632fab-602c-408d-ae42-24690bb9bea8', ritual_recitation_is_not_life, conventional).
narrative_ontology:cs_reference_frame('42632fab-602c-408d-ae42-24690bb9bea8', modern_hebrew_as_national_vernacular).
narrative_ontology:cs_drift_state('42632fab-602c-408d-ae42-24690bb9bea8', contemporary_globalized_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('42632fab-602c-408d-ae42-24690bb9bea8', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, hebrew_revivalists).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_adherents).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, secular_nationalism_as_revitalization_engine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted and institutionalized modern Hebrew as the national language, seeing native, daily use as essential for national identity and sovereignty. Benefits from the cultural and political cohesion a living vernacular provides.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).

% Dedicated individuals and organizations who championed the transformation of Hebrew from a liturgical language to a spoken vernacular. Their work and ideology are validated by the success of native generation.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_revivalists, beneficiary,
    organized, biographical, mobile, national).

% Communities and individuals who primarily engage with Hebrew as a sacred language for prayer, study, and religious texts. They bear the cost of their form of Hebrew being deemed 'not life' or 'mere preservation,' facing cultural devaluation and pressure to adopt vernacular norms.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_adherents, payer,
    organized, generational, identity_locked, global).

% Jewish communities outside Israel who maintain diverse forms of Hebrew engagement, including liturgical, academic, and cultural. They may feel their modes of continuity are devalued by the exclusive focus on native generation within the Israeli national project.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, payer,
    organized, generational, constrained, global).

% Academics and researchers who study the sociolinguistics of Hebrew revival and its impact on various communities. They analyze the processes and outcomes without direct ideological stake in the 'vitality' definition.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, linguistic_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a shared, living vernacular language for a modern national identity, enabling daily communication, cultural production, and state administration, thereby unifying a diverse population.
% TRANSFER_FUNCTION: Transfers linguistic authority, educational resources, and cultural prestige from traditional liturgical and diaspora-centric forms of Hebrew engagement to a modern, natively generated vernacular, primarily within the context of the Israeli state.
% ABSENT_VOICES: Ultra-Orthodox communities and some traditionalist diaspora groups who maintain Hebrew solely as a sacred language for prayer and study, rejecting its secularization and nationalization. They would argue that ritual use *is* vitality and that secular native generation constitutes desacralization.
% DISAPPEARANCE_RATIONALE: If the constraint (the ideology and institutional enforcement of native generation as the sole criterion for vitality) vanished, the ideological and institutional drive for a purely vernacular Hebrew would weaken. Other forms of Hebrew (liturgical, academic) might gain more prominence, and the national identity tied exclusively to a living vernacular would be challenged, leading to a re-evaluation of language policy and cultural priorities.
% FOUNDING_PROBLEM: The perceived lack of a living, spoken language for the Jewish people, hindering national self-determination and modern cultural expression, leading to reliance on other languages (Yiddish, Ladino, Arabic, European languages) and a fragmented cultural identity.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians, educators, and sociolinguists attest to the historical problem and the success of the revival project. Liturgical adherents and some diaspora communities contest the 'dead' status of liturgical Hebrew, arguing for its continuous vitality and the ongoing relevance of their traditions, suggesting the 'problem' is reframed to justify the constraint.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_ambiguity,
    'Is ''vitality'' solely defined by native, daily use, or can it encompass other forms of continuous engagement (liturgical, scholarly, cultural)?',
    'A shift in sociolinguistic consensus or institutional policy to recognize multiple valid forms of linguistic vitality, or empirical studies demonstrating the functional equivalence of different engagement modes for cultural continuity.',
    'If vitality is broadened, the constraint''s extractiveness from liturgical traditions would decrease, potentially reclassifying it closer to a Rope or even a Piton if the original mandate atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''linguistic vitality'' and its criteria.').

omega_variable(
    desacralization_impact,
    'What is the full, long-term impact of the desacralization of Hebrew on religious communities, their cultural continuity, and the broader Jewish identity?',
    'Longitudinal ethnographic studies and theological analyses within affected communities, assessing changes in religious practice, identity formation, and intergenerational transmission of tradition.',
    'If the desacralization is found to cause severe, unmitigated harm to religious and cultural continuity, the constraint''s effective extraction from these communities would be amplified, pushing it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desacralization_impact, empirical, 'The extent of harm caused by the desacralization of Hebrew for religious communities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative Hebrew uses structural (institutional policy, funding allocation, educational curricula) or internalized (social pressure, self-perception of ''dead'' language among non-vernacular users)?',
    'Analysis of post-policy-change linguistic behaviors: if alternative uses flourish when structural barriers are removed, suppression was primarily structural. If they remain marginalized, internalized suppression is significant.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measure suggests, as the ''victims'' carry the suppression with them even if external barriers lessen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative Hebrew uses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_vitality__native_daily_reading, theater_ratio, 1940, 0.14).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.16).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_vitality__native_daily_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__native_daily_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__native_daily_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement(hebr_be_t1940, hebrew_vitality__native_daily_reading, base_extractiveness, 1940, 0.56).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.64).
narrative_ontology:measurement(hebr_be_t1980, hebrew_vitality__native_daily_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__native_daily_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__native_daily_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(hebr_su_t1940, hebrew_vitality__native_daily_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(hebr_su_t1980, hebrew_vitality__native_daily_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__native_daily_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__native_daily_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_vitality' kernel, focusing on native, daily generation as the sole criterion for vitality. It is linked to sibling readings that emphasize liturgical use or a hybrid approach, each representing a distinct structural claim about Hebrew's status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
