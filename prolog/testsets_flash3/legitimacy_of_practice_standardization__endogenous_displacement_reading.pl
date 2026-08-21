% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Practice Standardization: Endogenous Displacement Reading
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous displacement' reading of
 *   practice standardization, where legitimacy is derived from voluntary
 *   adoption driven by perceived utility or cultural evolution. It posits
 *   that genuine and lasting change emerges from within a society, rather
 *   than being imposed from above. Examples include the gradual adoption of
 *   new agricultural techniques, fashion trends, or linguistic shifts.
 *   Resistance to such changes is seen as temporary friction, eventually
 *   overcome by the inherent advantages or cultural fit of the new practice.
 *   The structural delta for this reading would show gradual adoption curves,
 *   regional variation, elite-to-mass diffusion, and resistance as temporary
 *   friction, with 'double life' (simultaneous use of old and new practices)
 *   as a transitional phase.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, mountain).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Standardization: Endogenous Displacement Reading").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

domain_priors:emerges_naturally(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '4ae99059-b1c2-415e-bc94-96d80d2d1990').
narrative_ontology:cs_kernel_codification('4ae99059-b1c2-415e-bc94-96d80d2d1990', implicit).
narrative_ontology:cs_authority_grounding('4ae99059-b1c2-415e-bc94-96d80d2d1990', practice).
narrative_ontology:cs_interpretation_layer_present('4ae99059-b1c2-415e-bc94-96d80d2d1990').
narrative_ontology:cs_reading_relation('4ae99059-b1c2-415e-bc94-96d80d2d1990', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ae99059-b1c2-415e-bc94-96d80d2d1990', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('4ae99059-b1c2-415e-bc94-96d80d2d1990', foundational, utility_drives_legitimate_change).
narrative_ontology:cs_axiom_status(utility_drives_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('4ae99059-b1c2-415e-bc94-96d80d2d1990', utility_drives_legitimate_change, empirically_contingent).
narrative_ontology:cs_axiom('4ae99059-b1c2-415e-bc94-96d80d2d1990', foundational, cultural_evolution_is_organic).
narrative_ontology:cs_axiom_status(cultural_evolution_is_organic, holdable).
narrative_ontology:cs_axiom_grounding('4ae99059-b1c2-415e-bc94-96d80d2d1990', cultural_evolution_is_organic, empirically_contingent).
narrative_ontology:cs_reference_frame('4ae99059-b1c2-415e-bc94-96d80d2d1990', gradual_organic_cultural_shift).
narrative_ontology:cs_drift_state('4ae99059-b1c2-415e-bc94-96d80d2d1990', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4ae99059-b1c2-415e-bc94-96d80d2d1990', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_theory).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffusion_of_innovation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations that voluntarily adopt new practices (e.g., Gregorian calendar, Western dress) due to perceived utility or cultural alignment. They benefit from the practical advantages and social integration, experiencing the change as natural evolution.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations, beneficiary,
    organized, generational, mobile, regional).

% Academics and policymakers who interpret practice changes through the lens of endogenous cultural evolution and utility-driven adoption. They see gradual, voluntary shifts as the legitimate path to modernization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, observer,
    analytical, civilizational, analytical, global).

% Groups whose authority or identity is tied to older practices. They bear the cost of cultural displacement and loss of influence as new practices gain traction, but their resistance is seen as temporary friction against an inevitable, beneficial tide.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites, payer,
    moderate, biographical, constrained, local).

% In this reading, state authorities are not the primary drivers of legitimate change; their decrees are seen as either reflecting existing endogenous shifts or as illegitimate impositions. Their role is minimized or viewed with skepticism regarding true legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authorities, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates social coordination around new, more efficient, or culturally resonant practices by allowing them to emerge organically and displace older ones through perceived utility and voluntary adoption.
% TRANSFER_FUNCTION: Transfers social legitimacy and practical utility from older, less efficient practices to newer, more beneficial ones, driven by collective choice and cultural evolution.
% ABSENT_VOICES: The 'exogenous override' perspective, which argues for the legitimacy of state-mandated change, is absent from this reading's core justification. It would argue that some changes require top-down imposition for collective benefit.
% DISAPPEARANCE_RATIONALE: If this principle of legitimacy vanished, practices would still change based on utility and cultural evolution; the underlying social dynamics are robust. Only the *interpretation* of their legitimacy would be affected, not the process itself.
% FOUNDING_PROBLEM: How to distinguish genuine, beneficial societal evolution from arbitrary or coercive impositions, ensuring that practice changes are truly aligned with collective well-being and cultural resonance.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists and cultural anthropologists outside of direct state or traditional power structures corroborate that societies continually adapt practices based on utility and cultural fit, and that legitimacy often accrues to changes that emerge organically.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_unchanged).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, ExtMetricName, E),
    domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legitimacy_of_practice_standardization__endogenous_displacement_reading),
    narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.1) reflect the core premise that legitimate change is voluntary and utility-driven, not coercive. Any 'extraction' is the minor cost of adapting to a new, more efficient practice, and 'suppression' is minimal, representing social pressure or the natural obsolescence of older ways. The high accessibility collapse (0.85) signifies that once the utility of a new practice is understood, alternatives (older practices) naturally become less viable. Resistance is low (0.05) because the change is perceived as beneficial. The claimed type is 'mountain' because this reading frames the process as a natural, almost inevitable, law of cultural evolution.
 *
 * PERSPECTIVAL GAP:
 *   This reading would compute as a Mountain for all seats, as it describes a natural process. However, other readings (e.g., exogenous override) would compute as Snares or Tangled Ropes for populations experiencing imposed change, highlighting a significant perspectival gap on the source of legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopting populations are beneficiaries, gaining utility and social integration. Modernization theorists are observers, whose theories are vindicated by this process. Traditional elites are payers, as their authority tied to older practices erodes, but this is framed as a natural, non-coercive displacement. State authorities are largely excluded from the legitimacy-granting mechanism in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinguishing_voluntary_from_coerced_adoption,
    'How can ''voluntary adoption'' be reliably distinguished from ''coerced adoption'' where the coercion is diffuse or structural (e.g., economic necessity, social pressure to conform to a dominant power''s norms)?',
    'Longitudinal ethnographic studies tracking individual and community-level decision-making, combined with counterfactual analysis of alternative pathways in the absence of external pressures. Examine the ''double life'' phenomenon: if practices persist in private despite public adoption, it suggests coercion.',
    'If much ''voluntary'' adoption is found to be subtly coerced, the extractiveness and suppression metrics of this constraint would need to be significantly re-evaluated upwards, potentially reclassifying it from a Mountain to a Snare or Tangled Rope, as the ''naturalness'' claim would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinguishing_voluntary_from_coerced_adoption, empirical, 'Ambiguity in the voluntariness of practice adoption.').

omega_variable(
    natural_evolution_vs_ideological_framing,
    'Is the ''endogenous displacement'' reading a description of a natural process, or an ideological framing used to legitimize certain forms of modernization and obscure the role of power?',
    'Critical discourse analysis of historical narratives and policy documents, examining who benefits from this framing and whose agency is minimized. Compare with alternative historical accounts that emphasize power dynamics.',
    'If primarily an ideological framing, the ''emerges_naturally'' flag would be false, and the constraint would be reclassified as a constructed constraint (e.g., a Rope or Snare), with higher extractiveness and suppression, reflecting the hidden costs and coercive elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_evolution_vs_ideological_framing, conceptual, 'Whether ''natural evolution'' is a descriptive or ideological claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 1800, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(legi_tr_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1850, 0.04).
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1900, 0.04).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(legi_be_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(legi_be_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(legi_su_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1850, 0.09).
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1900, 0.09).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
