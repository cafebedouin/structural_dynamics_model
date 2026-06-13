% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Imposed Practices
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint describes the process by which a state attempts to impose
 *   new cultural practices (e.g., dress codes, calendar systems, language)
 *   through a 'hybrid scaffolding' approach. This involves top-down mandates
 *   reinforced by ideological messaging and the modeling of desired behaviors
 *   by urban elites, aiming to generate a quasi-endogenous pull. Unlike pure
 *   decree (which often fails) or pure bottom-up evolution (which is slow),
 *   this method achieves partial displacement, often resulting in hybrid
 *   practices. The case of dress codes, where elite adoption and ideological
 *   framing led to partial success and hybrid forms, is a key example,
 *   contrasting with calendar reforms that often failed due to pure decree.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.6).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Imposed Practices").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'ab8e909c-3056-4128-a768-31d40586db02').
narrative_ontology:cs_kernel_codification('ab8e909c-3056-4128-a768-31d40586db02', formalized).
narrative_ontology:cs_authority_grounding('ab8e909c-3056-4128-a768-31d40586db02', lineage).
narrative_ontology:cs_interpretation_layer_present('ab8e909c-3056-4128-a768-31d40586db02').
narrative_ontology:cs_reading_relation('ab8e909c-3056-4128-a768-31d40586db02', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab8e909c-3056-4128-a768-31d40586db02', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('ab8e909c-3056-4128-a768-31d40586db02', foundational, legitimacy_requires_quasi_endogenous_pull).
narrative_ontology:cs_axiom_status(legitimacy_requires_quasi_endogenous_pull, holdable).
narrative_ontology:cs_axiom_grounding('ab8e909c-3056-4128-a768-31d40586db02', legitimacy_requires_quasi_endogenous_pull, empirically_contingent).
narrative_ontology:cs_axiom('ab8e909c-3056-4128-a768-31d40586db02', secondary, pure_decree_insufficient_for_deep_change).
narrative_ontology:cs_axiom_status(pure_decree_insufficient_for_deep_change, holdable).
narrative_ontology:cs_axiom_grounding('ab8e909c-3056-4128-a768-31d40586db02', pure_decree_insufficient_for_deep_change, empirically_contingent).
narrative_ontology:cs_reference_frame('ab8e909c-3056-4128-a768-31d40586db02', state_led_modernization_through_cultural_synthesis).
narrative_ontology:cs_drift_state('ab8e909c-3056-4128-a768-31d40586db02', post_colonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab8e909c-3056-4128-a768-31d40586db02', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity_markers).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_traditional_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, local_cultural_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus driving the imposition of new cultural practices, believing them essential for national progress and international standing. It invests in propaganda, education, and elite modeling to create a 'modern' identity.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project, agenda_setter,
    institutional, generational, constrained, national).

% These groups benefit from aligning with the state's modernization agenda, gaining social status, economic opportunities, and access to power. They act as models for the new practices, reinforcing the ideological messaging.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity_markers, beneficiary,
    powerful, biographical, mobile, national).

% These populations are the primary targets of practice displacement. They bear the cost of abandoning traditional ways, often without access to the benefits of the 'modern' practices, leading to cultural alienation and economic marginalization. Their identity is deeply tied to existing practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_traditional_populations, payer,
    powerless, generational, identity_locked, local).

% These institutions (e.g., religious bodies, traditional craft guilds, local governance structures) are undermined by the imposed practices. They lose authority, resources, and relevance as the state promotes new norms, but cannot easily resist due to state power.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, local_cultural_institutions, payer,
    moderate, generational, constrained, regional).

% Academics, NGOs, and other states observing the process of cultural imposition, often evaluating it against human rights norms or theories of state-building. Their analysis can influence international opinion and aid flows.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify a diverse population under a common set of 'modern' cultural practices and national identity, facilitating state administration and projecting an image of progress internationally.
% TRANSFER_FUNCTION: Transfers cultural capital, social legitimacy, and economic opportunity from traditional practices and local institutions to new, state-sanctioned practices and their adherents, particularly urban elites.
% ABSENT_VOICES: Traditional cultural leaders and dissenting intellectuals who advocate for the preservation of indigenous practices or alternative modernization paths are often suppressed or marginalized, preventing their perspectives from entering the official discourse.
% DISAPPEARANCE_RATIONALE: If the state's scaffolding efforts vanished, the imposed practices would likely recede in many areas, and traditional practices would reassert themselves, leading to a resurgence of local cultural forms and potentially a fragmentation of national identity.
% FOUNDING_PROBLEM: The state perceived a lack of national unity, 'backwardness' in traditional practices, and a need to align with global norms of modernity to ensure national survival and prosperity.
% FOUNDING_PROBLEM_CORROBORATION: The state's official narratives and some segments of the urban elite continue to attest to the problem's live status, citing ongoing challenges in national cohesion and global competitiveness. However, rural populations and cultural preservationists contest this, arguing the 'problem' was a pretext for cultural domination, as documented by independent historians and anthropologists.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it has a genuine coordination function (national unity, modernization) but also involves significant asymmetric extraction and requires active enforcement. Extractiveness (0.6) reflects the cost borne by traditional populations forced to abandon practices. Suppression (0.7) is high due to state power used to marginalize traditional institutions and promote new norms. Theater ratio (0.4) indicates that while there's genuine effort towards modernization, a significant portion of the activity is performative, aimed at projecting an image of success rather than achieving deep, internalized change across all segments of society. The cyclical nature of the measurements reflects periods of intensified state campaigns followed by periods of resistance and adaptation.
 *
 * PERSPECTIVAL GAP:
 *   The state and urban elites perceive this as a necessary and beneficial modernization project, a 'scaffold' for progress. Rural populations and traditional institutions, however, experience it as an extractive imposition, a 'snare' that undermines their identity and way of life. The engine's per-seat classification will reflect this divergence based on their declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'state modernization project' and 'urban elites' are beneficiaries (d near 0.0-0.2) as they gain power, status, and align with their self-conception of progress. 'Rural traditional populations' and 'local cultural institutions' are victims (d near 0.8-1.0) as they bear the costs of cultural displacement and loss of autonomy. The 'identity_locked' exit option for rural populations reflects the deep fusion of their identity with traditional practices, making exit from those practices extremely costly.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'Rope' (ignoring extraction) or a pure 'Snare' (ignoring the genuine, if contested, coordination function of state-building and modernization). The 'Tangled Rope' classification highlights the hybrid nature where a legitimate-sounding coordination story coexists with asymmetric extraction, sustained by active enforcement and ideological scaffolding. The 'founding_problem_status' being 'live' but 'contested' further underscores this tension, indicating that the original mandate is still invoked but its current operation is questioned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_legitimacy,
    'Is the perceived legitimacy of the imposed practices a natural outcome of their ''superiority'' (as claimed by the state), or a constructed effect of state power and ideological scaffolding?',
    'Longitudinal studies of post-state-collapse societies: if the imposed practices persist without state enforcement, it suggests a degree of internalized legitimacy; if they rapidly recede, it points to constructed legitimacy.',
    'If constructed, the constraint''s extractiveness and suppression are higher than perceived by beneficiaries; if natural, the coordination function is stronger and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_legitimacy, empirical, 'Distinguishing genuine adoption from coerced compliance.').

omega_variable(
    scaffolding_vs_snare,
    'At what point does ''scaffolding'' intended to transition to new practices become a permanent ''snare'' for those who cannot exit traditional ways?',
    'Analysis of exit options and economic mobility for rural populations: if access to benefits of new practices remains systematically denied, the scaffolding has become a snare.',
    'If it''s a permanent snare, the constraint''s effective extraction for victims is higher, and its claimed coordination function is largely cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_vs_snare, conceptual, 'The boundary between temporary support and permanent extraction.').

omega_variable(
    reading_difference_on_efficacy,
    'Does the ''hybrid_scaffolding_reading'' accurately capture the efficacy of imposed practices, or do the ''exogenous_override_reading'' or ''endogenous_climb_reading'' offer a more accurate account of success/failure in specific domains?',
    'Comparative historical analysis across different domains of imposition (e.g., calendar reform vs. dress codes) within the same state, evaluating the degree of displacement and internalization achieved by each approach.',
    'If other readings prove more accurate for specific domains, this reading''s generalizability is limited, and the overall ''legitimacy_of_imposed_practice'' kernel is more fragmented than this reading suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_difference_on_efficacy, empirical, 'Assessing the domain-specific validity of the hybrid scaffolding model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 1920, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1930, 0.3).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1960, 0.4).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1980, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1930, 0.5).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1940, 0.6).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1980, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1980, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimacy_of_imposed_practice' kernel, focusing on the hybrid scaffolding approach. It contrasts with the 'exogenous_override_reading' (pure decree) and 'endogenous_climb_reading' (pure bottom-up adoption), which would yield different extractiveness and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
