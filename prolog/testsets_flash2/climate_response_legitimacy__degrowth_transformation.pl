% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation for Climate Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth transformation' reading of
 *   legitimate climate response. It posits that genuine climate action
 *   necessitates a fundamental shift away from the growth imperative in
 *   wealthy nations, involving structural economic changes like universal
 *   basic services, reduced working hours, and democratic firm ownership.
 *   This is framed as a Snare because it requires significant extraction from
 *   current generations in developed economies to benefit future generations
 *   and ecosystems, enforced against powerful incumbent interests and deeply
 *   ingrained societal norms. The high extractiveness and suppression reflect
 *   the scale of the required societal transformation and the resistance it
 *   would face.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.85).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.9).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, snare).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation for Climate Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, 'a06926ec-8945-4130-914f-0ec42dac2144').
narrative_ontology:cs_kernel_codification('a06926ec-8945-4130-914f-0ec42dac2144', distributed).
narrative_ontology:cs_authority_grounding('a06926ec-8945-4130-914f-0ec42dac2144', diffuse_epistemic).
narrative_ontology:cs_reading_relation('a06926ec-8945-4130-914f-0ec42dac2144', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('a06926ec-8945-4130-914f-0ec42dac2144', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_axiom('a06926ec-8945-4130-914f-0ec42dac2144', foundational, infinite_growth_on_finite_planet_impossible).
narrative_ontology:cs_axiom_status(infinite_growth_on_finite_planet_impossible, holdable).
narrative_ontology:cs_axiom_grounding('a06926ec-8945-4130-914f-0ec42dac2144', infinite_growth_on_finite_planet_impossible, empirically_contingent).
narrative_ontology:cs_axiom('a06926ec-8945-4130-914f-0ec42dac2144', foundational, intergenerational_equity_requires_degrowth).
narrative_ontology:cs_axiom_status(intergenerational_equity_requires_degrowth, holdable).
narrative_ontology:cs_axiom_grounding('a06926ec-8945-4130-914f-0ec42dac2144', intergenerational_equity_requires_degrowth, deontological).
narrative_ontology:cs_reference_frame('a06926ec-8945-4130-914f-0ec42dac2144', ecological_limits_framework).
narrative_ontology:cs_drift_state('a06926ec-8945-4130-914f-0ec42dac2144', contemporary_policy_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a06926ec-8945-4130-914f-0ec42dac2144', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, global_ecosystems).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_developed_economy_citizens).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, growth_dependent_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expected to bear the costs of structural economic transformation, including reduced material consumption, shorter working hours, and shifts in ownership. Their identity is often tied to consumer culture and economic growth, making exit from this paradigm difficult.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_developed_economy_citizens, payer,
    organized, biographical, identity_locked, national).

% Benefit from a stable climate, reduced ecological degradation, and a more equitable economic system, without relying on unproven technological fixes. They have no direct voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Benefit from reduced resource extraction, lower pollution, and a more stable climate. They are a non-agent entity whose 'benefit' is the cessation of harm.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, global_ecosystems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__degrowth_transformation, global_ecosystems).

% Face existential threat from policies aimed at dismantling the growth imperative and transitioning to a post-fossil fuel economy. They actively resist these changes through lobbying and political influence.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, fossil_fuel_industries, payer,
    institutional, immediate, constrained, global).

% Government agencies, financial institutions, and corporations whose mandates and structures are predicated on continuous economic growth. They would need fundamental re-imagining and face significant internal resistance to degrowth policies.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_dependent_institutions, payer,
    institutional, biographical, identity_locked, national).

% Propose and champion the policies of degrowth, including universal basic services, working time reduction, and democratic firm ownership. They seek to fundamentally transform economic systems for ecological and social justice.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% Largely operate within a growth-oriented paradigm and often dismiss degrowth as economically unfeasible or undesirable. Their models and policy recommendations typically exclude the structural transformations advocated by degrowth.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, mainstream_economists, excluded,
    institutional, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global efforts to reduce ecological overshoot and ensure intergenerational equity by aligning economic activity with planetary boundaries and social well-being, rather than endless growth.
% TRANSFER_FUNCTION: Transfers wealth, resources, and decision-making power from current wealthy generations and growth-dependent industries to future generations, global ecosystems, and local communities, through mechanisms like reduced consumption, wealth redistribution, and democratic control of production.
% ABSENT_VOICES: Mainstream economists and political leaders who are deeply invested in the growth paradigm are largely absent from the core degrowth discourse, often dismissing it as radical or impractical. Their models and policy frameworks do not account for the proposed structural changes.
% DISAPPEARANCE_RATIONALE: If the imperative for degrowth transformation vanished, the current growth-oriented economic system would continue, leading to escalating climate and ecological crises. The world would rearrange itself towards a path of increased environmental degradation and intergenerational injustice, as the fundamental drivers of the problem would remain unchecked.
% FOUNDING_PROBLEM: The observed failure of conventional climate policies (mitigation, adaptation) to address the root causes of ecological overshoot and climate change, specifically the inherent conflict between infinite economic growth and finite planetary resources, leading to intergenerational injustice.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists, climate scientists, and social justice movements corroborate the founding problem, citing ongoing ecological degradation, rising inequality, and the inadequacy of market-based solutions. This corroboration comes from outside the direct beneficiaries of degrowth policies.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because it demands a substantial reduction in material consumption and a reorientation of economic activity, directly impacting the lifestyles and expectations of citizens in wealthy nations. Suppression (0.90) is also very high, as the implementation of such policies would require overcoming immense political, economic, and cultural resistance from entrenched interests (fossil fuel industries, growth-dependent institutions) and deeply held societal values. The theater ratio is low (0.10) because the proposed changes are direct and structural, leaving little room for performative gestures without genuine transformation. The increasing extractiveness and suppression over time reflect the escalating urgency of climate action and the growing recognition of the deep structural changes required.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this is a necessary and just transformation, a 'rope' to a sustainable future. From the perspective of current developed economy citizens and growth-dependent institutions, it is a 'snare' that extracts their wealth and autonomy, enforced by an ideological agenda. The engine's classification as a Snare reflects the structural reality of the extraction and suppression required from the current system's beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and global ecosystems are the primary beneficiaries (d near 0.0), as they gain a stable climate and ecological health. Current developed economy citizens, fossil fuel industries, and growth-dependent institutions are the primary targets (d near 1.0), as they bear the direct costs of reduced consumption, economic restructuring, and loss of power/profit. Degrowth advocates act as agenda-setters, pushing for these transformations. Mainstream economists are excluded, as their paradigm is fundamentally challenged.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_barrier,
    'Is the proposed degrowth transformation politically feasible within existing democratic structures, or would it require a different political system?',
    'Empirical observation of policy implementation in nations attempting similar structural changes, or comparative political analysis of governance models capable of enacting such transformations.',
    'If politically infeasible, the constraint''s effective suppression is even higher, as the ''exit'' for the current system is blocked by political inertia, potentially leading to a reclassification towards a more entrenched Snare or even a Piton if the rhetoric persists without action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_barrier, empirical, 'Uncertainty regarding the political viability of degrowth policies.').

omega_variable(
    economic_model_validity,
    'Are the economic models underpinning degrowth (e.g., post-growth economics) robust and capable of delivering societal well-being without growth, or do they contain unforeseen negative consequences?',
    'Long-term empirical studies of degrowth-oriented policies in practice, or rigorous comparative modeling against conventional economic frameworks.',
    'If degrowth models prove unstable or harmful, the justification for the extraction from current generations would collapse, potentially reclassifying the constraint as a pure Snare without a legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_model_validity, empirical, 'Validity of degrowth economic models.').

omega_variable(
    identity_lock_strength,
    'How deeply is the identity of citizens in wealthy nations tied to consumerism and economic growth, and how resistant are these identities to change?',
    'Sociological and psychological studies on cultural values, consumer behavior, and responses to degrowth narratives; analysis of social movements and counter-movements.',
    'If identity lock is stronger than estimated, the effective suppression is higher, requiring more coercive force or a longer, more difficult cultural transformation, pushing the constraint further into the Snare category.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Strength of identity lock-in to growth paradigm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.13).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.12).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.11).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__degrowth_transformation, theater_ratio, 40, 0.1).
narrative_ontology:measurement(clim_tr_t50, climate_response_legitimacy__degrowth_transformation, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(clim_be_t50, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(clim_su_t50, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_response_legitimacy' kernel. This 'degrowth_transformation' reading proposes fundamental economic restructuring, influencing (and being influenced by) the 'mitigation_priority' and 'adaptation_priority' readings by challenging their underlying assumptions about growth and technological solutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
