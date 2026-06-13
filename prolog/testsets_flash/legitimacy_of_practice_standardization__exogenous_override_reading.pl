% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: Legitimacy of Practice Standardization (Exogenous Override Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint describes the legitimacy of practice standardization when
 *   state authority decrees it for collective benefit (modernization, fiscal
 *   stability, international alignment). This reading emphasizes abrupt legal
 *   imposition, strong enforcement, and surface compliance that often masks
 *   persistent underground traditional practices. The 'double life' of
 *   populations maintaining both official and traditional practices is a
 *   stable equilibrium, not a transitional phase. Rural populations, for
 *   example, might maintain lunar calendars for decades despite official
 *   adoption of a Gregorian calendar.
 *
 * KEY AGENTS:
 *   - modernizing_state_elites: Agenda setter (institutional/mobile) — initiates and enforces changes.
 *   - traditional_rural_populations: Payer (powerless/identity_locked) — bears costs, maintains underground practices.
 *   - urban_professional_classes: Beneficiary (powerful/mobile) — benefits from modernization, adopts new practices.
 *   - cultural_conservatives: Payer (moderate/constrained) — resists changes on ideological grounds.
 *   - international_observers: Observer (analytical/analytical) — monitors modernization efforts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.75).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "Legitimacy of Practice Standardization (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'eaacc964-fc53-40a0-9a77-494faa6dfd46').
narrative_ontology:cs_kernel_codification('eaacc964-fc53-40a0-9a77-494faa6dfd46', formalized).
narrative_ontology:cs_authority_grounding('eaacc964-fc53-40a0-9a77-494faa6dfd46', extraction).
narrative_ontology:cs_interpretation_layer_present('eaacc964-fc53-40a0-9a77-494faa6dfd46').
narrative_ontology:cs_reading_relation('eaacc964-fc53-40a0-9a77-494faa6dfd46', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('eaacc964-fc53-40a0-9a77-494faa6dfd46', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('eaacc964-fc53-40a0-9a77-494faa6dfd46', foundational, state_sovereignty_over_social_practice).
narrative_ontology:cs_axiom_status(state_sovereignty_over_social_practice, holdable).
narrative_ontology:cs_axiom_grounding('eaacc964-fc53-40a0-9a77-494faa6dfd46', state_sovereignty_over_social_practice, conventional).
narrative_ontology:cs_axiom('eaacc964-fc53-40a0-9a77-494faa6dfd46', foundational, collective_benefit_justifies_imposition).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_imposition, holdable).
narrative_ontology:cs_axiom_grounding('eaacc964-fc53-40a0-9a77-494faa6dfd46', collective_benefit_justifies_imposition, instrumental).
narrative_ontology:cs_reference_frame('eaacc964-fc53-40a0-9a77-494faa6dfd46', rational_state_modernization).
narrative_ontology:cs_drift_state('eaacc964-fc53-40a0-9a77-494faa6dfd46', post_colonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eaacc964-fc53-40a0-9a77-494faa6dfd46', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_professional_classes).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, cultural_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and enforce top-down practice changes (e.g., calendar reform, dress codes) to align the nation with perceived international standards, enhance fiscal stability, or project an image of modernity. They benefit from increased state control and international legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_elites, agenda_setter,
    institutional, generational, mobile, national).

% Are compelled to adopt new practices that often conflict with deeply ingrained cultural and religious norms. They face penalties for non-compliance but often maintain traditional practices underground, leading a 'double life'. Their identity is fused with traditional ways, making genuine exit unthinkable.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_rural_populations, payer,
    powerless, generational, identity_locked, local).

% Benefit from the perceived modernization and alignment with international norms, which can open up new economic and social opportunities. They often readily adopt the new practices, reinforcing the state's narrative of progress.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_professional_classes, beneficiary,
    powerful, biographical, mobile, national).

% Resist the imposed changes on ideological grounds, viewing them as an erosion of national identity or religious values. They may organize passive resistance or engage in limited public protest, but face state suppression.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, cultural_conservatives, payer,
    moderate, generational, constrained, national).

% Monitor the state's modernization efforts, often providing aid or diplomatic recognition based on perceived progress. They may not fully grasp the internal resistance or the 'double life' phenomenon.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize social practices across a diverse population, enabling centralized administration, national unity, and international alignment (e.g., a single calendar for state functions, uniform dress for public service).
% TRANSFER_FUNCTION: Transfers cultural autonomy and traditional legitimacy from local communities and traditional authorities to the central state, in exchange for perceived 'modernity' and 'progress'.
% ABSENT_VOICES: Traditional religious leaders and local community elders, whose authority is directly undermined by state-decreed practice changes, are often excluded from the decision-making process. Their voices would articulate the deep cultural costs and the persistence of alternative legitimacies.
% DISAPPEARANCE_RATIONALE: If the state's authority to unilaterally decree practice changes vanished, many traditional practices would resurface openly, and the 'double life' would cease. Local communities would revert to or openly integrate their preferred calendars, dress, and rituals, leading to a more pluralistic but less centrally controlled social fabric.
% FOUNDING_PROBLEM: The perceived need for national unity, administrative efficiency, and international recognition in a rapidly modernizing world, often framed as overcoming 'backwardness' or 'fragmentation'.
% FOUNDING_PROBLEM_CORROBORATION: Modernizing state elites consistently attest that the founding problems of national cohesion and international alignment remain live. International observers often corroborate the state's narrative of modernization, though they may not fully understand the internal resistance. Traditional populations, however, contest the premise that their practices constitute a 'problem'.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate national practices for collective benefit (e.g., a unified calendar for administration), but it does so through asymmetric extraction and suppression. Extractiveness (0.65) is high due to the imposition of costs on traditional groups and the transfer of cultural authority. Suppression (0.75) is significant, as the state actively enforces compliance through legal means and penalties. Theater ratio (0.55) is also high, reflecting the gap between official compliance and the widespread, persistent 'double life' where traditional practices continue covertly. The state performs modernization, but the underlying social reality is more complex.
 *
 * PERSPECTIVAL GAP:
 *   Modernizing state elites perceive this as a necessary Rope for national development, while traditional rural populations experience it as a Snare, forcing them to abandon or hide their cultural heritage. Urban professional classes may see it as a beneficial Rope, aligning with their interests. The engine's per-seat classification will reflect these divergences based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing state elites are clear beneficiaries (d=0.0-0.2) as they gain control and legitimacy. Traditional rural populations and cultural conservatives are targets (d=0.8-1.0) as they bear the costs of forced change and suppression. Urban professional classes are beneficiaries (d=0.2-0.4) due to alignment with state goals and new opportunities. International observers are analytical (d=0.5) and do not directly participate in the extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading highlights how a constraint, initially framed as a necessary modernization Rope, can drift into a Tangled Rope or even a Snare if the 'collective benefit' becomes a cover for state power consolidation and cultural extraction. The high theater ratio and persistent resistance indicate that the mandate for genuine coordination is significantly compromised by the extractive and suppressive mechanisms. The 'double life' phenomenon is a key indicator of this drift, showing that the constraint's official function is largely performative for a significant portion of the population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persistence_of_double_life,
    'Is the ''double life'' (surface compliance, underground tradition) a stable equilibrium or a transitional phase towards full assimilation?',
    'Longitudinal ethnographic studies tracking practice adherence over multiple generations in affected communities, observing the rate of decay or resilience of traditional practices.',
    'If stable, the constraint''s theater ratio and suppression are higher than initially measured, as the state''s ''success'' is largely performative. If transitional, the constraint''s long-term extractiveness may be lower, as resistance eventually wanes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_of_double_life, empirical, 'Stability of dual practice in the face of state imposition.').

omega_variable(
    collective_benefit_vs_state_interest,
    'To what extent does the ''collective benefit'' claimed by the state genuinely accrue to the entire population, versus primarily serving the interests of the modernizing state elites?',
    'Independent, disaggregated economic and social impact assessments across different population segments, comparing stated benefits with actual outcomes, particularly for marginalized groups.',
    'If benefits are concentrated, the constraint''s extractiveness is higher and its coordination function is weaker, pushing it closer to a Snare. If benefits are genuinely diffuse, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_vs_state_interest, empirical, 'Distribution of claimed collective benefits.').

omega_variable(
    framing_of_legitimacy_source,
    'Is the legitimacy of practice change derived from the state''s sovereign authority, or from the perceived ''rationality'' and ''progress'' of the new practices themselves?',
    'Analysis of state propaganda, legal justifications, and public discourse. If arguments primarily appeal to state power, it reinforces the ''exogenous override'' reading. If they emphasize inherent superiority of new practices, it leans towards ''endogenous displacement'' framing.',
    'If legitimacy is purely state-derived, the suppression metric is more central to its persistence. If it relies on perceived rationality, the constraint is more vulnerable to intellectual resistance and alternative framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_legitimacy_source, conceptual, 'Source of legitimacy for practice change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 1920, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1930, 0.4).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1940, 0.5).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1950, 0.55).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1960, 0.58).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1970, 0.57).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1980, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1960, 0.67).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1970, 0.66).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1980, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1960, 0.77).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1970, 0.76).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1980, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimacy_of_practice_standardization' kernel, focusing on exogenous state-decreed change. Other readings (endogenous_displacement_reading, dual_practice_equilibrium_reading) represent alternative structural claims about how practice change becomes legitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
