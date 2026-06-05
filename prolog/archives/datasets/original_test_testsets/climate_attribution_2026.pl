% ============================================================================
% CONSTRAINT STORY: climate_attribution_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_attribution_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_attribution_2026
 *   human_readable: Extreme Weather Attribution Science
 *   domain: scientific/political/economic
 *
 * SUMMARY:
 *   Extreme Weather Attribution is a scientific field that quantifies the
 *   degree to which human-induced climate change alters the frequency and
 *   intensity of specific weather events. This constraint is not the weather
 *   itself, but the scientific and political framework for assigning cause
 *   and liability. It functions as a coordination mechanism for researchers
 *   and policymakers while simultaneously creating a powerful tool for
 *   extraction, primarily through legal liability and international 'Loss and
 *   Damage' negotiations. Its power lies in its ability to transform a
 *   diffuse, global problem into a set of specific, quantifiable harms linked
 *   to identifiable actors.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Industry & High-Emitting Nations: Primary targets (powerful/trapped) — face legal and financial liability from the science's conclusions.
 *   - Vulnerable Nations & Communities: Primary beneficiaries (organized/constrained) — gain a tool to demand compensation and policy action.
 *   - Climate Scientists & Institutions: Secondary beneficiaries (institutional/arbitrage) — develop and deploy the methodology, gaining funding and relevance.
 *   - Policymakers & Legal Systems: Mediating actors — use the science as a basis for regulation, litigation, and international agreements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_attribution_2026, 0.55).
domain_priors:suppression_score(climate_attribution_2026, 0.65).
domain_priors:theater_ratio(climate_attribution_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_attribution_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_attribution_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_attribution_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_attribution_2026, tangled_rope).
narrative_ontology:human_readable(climate_attribution_2026, "Extreme Weather Attribution Science").
narrative_ontology:topic_domain(climate_attribution_2026, "scientific/political/economic").

domain_priors:requires_active_enforcement(climate_attribution_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_attribution_2026, vulnerable_nations_and_communities).
narrative_ontology:constraint_beneficiary(climate_attribution_2026, climate_scientists_and_institutions).
narrative_ontology:constraint_beneficiary(climate_attribution_2026, policymakers_seeking_action).
narrative_ontology:constraint_victim(climate_attribution_2026, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_attribution_2026, high_emitting_nations).
narrative_ontology:constraint_victim(climate_attribution_2026, climate_denialist_narratives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL FUEL INDUSTRY (SNARE) — Despite immense power, the industry is trapped by accumulating, non-refutable data. Attribution science creates a direct causal link between their product and specific damages, forming the basis for legal and financial liability. From this view, the science is a weaponized information system designed for extraction. d is high due to victim status and trapped exit, leading to χ > 0.66.
constraint_indexing:constraint_classification(climate_attribution_2026, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE SCIENTISTS (ROPE) — For the scientists developing the methods, attribution is a pure coordination tool. It creates a standardized, verifiable methodology for understanding climate systems, solving a collective action problem in research. As primary beneficiaries with high exit (can move to other research), they experience negative effective extraction (χ < 0).
constraint_indexing:constraint_classification(climate_attribution_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: VULNERABLE NATIONS (TANGLED ROPE) — For nations facing existential threats from climate change (e.g., small island states), attribution science is a hybrid. It's a coordination tool to build coalitions and make claims in international forums (e.g., Loss and Damage Fund), but it's also a tool for potential extraction from high-emitting nations. They are beneficiaries of the tool but victims of the underlying phenomena, with constrained options.
constraint_indexing:constraint_classification(climate_attribution_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRO-ACTION POLICYMAKER (SCAFFOLD) — A policymaker in a developed nation sees attribution science as a temporary support structure to build political will for climate policy. The goal is to use this evidence to create durable regulations, after which event-by-event attribution becomes less critical for policymaking. The 'sunset' is the point where climate risk is fully integrated into all planning and is no longer a contested political issue.
constraint_indexing:constraint_classification(climate_attribution_2026, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view confirms the base classification. The constraint provides a genuine coordination function (a common scientific standard) while simultaneously enabling a powerful, asymmetric extractive claim (liability for damages). It is a classic example of a knowledge system that both organizes and divides.
constraint_indexing:constraint_classification(climate_attribution_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_attribution_2026_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_attribution_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_attribution_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) is high because the science directly enables claims for massive financial transfers (trillions in potential damages) from one set of actors to another. The extraction is not automatic but is structurally encoded in the information produced. Suppression (0.65) is also high, as the methodology systematically suppresses alternative explanations for extreme weather trends (e.g., 'natural variability alone') by quantifying their unlikelihood. It establishes a dominant epistemic framework. Theater (0.20) is relatively low; while media reporting can be performative, the underlying science is highly functional and rigorously peer-reviewed.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For scientists, it is a Rope—a neutral tool for understanding the world. For vulnerable nations, it is a Tangled Rope—a necessary instrument of justice that also highlights their powerlessness. For the fossil fuel industry, it is a Snare—a closing trap of legal and financial liability constructed from data they cannot refute. For a forward-looking policymaker, it is a Scaffold—a temporary tool to build a permanent, climate-aware policy regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vulnerable nations, scientists) experience the constraint as a coordination tool, leading to lower derived 'd' values and thus lower effective extraction (χ). Victims (implicated industries/nations) experience it as a targeted mechanism. Their 'trapped' exit status maximizes their 'd' value, pushing the effective extraction χ into the Snare classification threshold from their perspective. The system correctly models how the same set of facts can be a tool for one group and a weapon against another.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by showing that a scientific framework is never just 'neutral information' once it enters a political and economic context. Classifying it as a pure Rope would ignore its profound extractive implications. Classifying it as a pure Snare would ignore its genuine function in coordinating scientific understanding and policy. The Tangled Rope classification, anchored by the analytical perspective, correctly identifies its dual nature, while the perspectival analysis shows how different actors can legitimately experience it as one or the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_admissibility,
    'Will attribution studies be consistently accepted in courts of law as sufficient evidence to establish legal liability for climate-related damages?',
    'Tracking legal precedents in climate litigation cases worldwide where attribution science is presented as evidence.',
    'If yes, the constraint''s classification shifts heavily towards Snare for implicated industries. If no, it remains a Tangled Rope, with its extractive function limited to the political sphere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_admissibility, empirical, 'Whether attribution science will be accepted as a basis for legal liability.').

omega_variable(
    model_dependency,
    'To what extent are attribution results dependent on the specific climate models used, and could systemic model biases over- or under-state the human contribution?',
    'Inter-model comparison projects and analysis of model performance against historical observations for different classes of extreme events.',
    'High model dependency would weaken its function as a Rope (coordination standard) and increase its perceived theater. Low dependency strengthens its classification as a robust Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_dependency, empirical, 'The degree to which results depend on specific climate models.').

omega_variable(
    action_threshold,
    'What level of statistical confidence in attribution (e.g., ''10x more likely'') is required to trigger significant policy change or financial commitments (e.g., Loss and Damage fund payouts)?',
    'Analysis of policy decisions and international agreements following the publication of high-confidence attribution studies.',
    'A high threshold for action means the constraint''s extractive potential is rarely realized, making it function more like a Piton (performative science). A low threshold makes its Snare-like properties more potent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(action_threshold, preference, 'The political and social confidence level required for action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_attribution_2026, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2004, climate_attribution_2026, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(clim_tr_t2014, climate_attribution_2026, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(clim_tr_t2024, climate_attribution_2026, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2004, climate_attribution_2026, base_extractiveness, 2004, 0.15).
narrative_ontology:measurement(clim_be_t2014, climate_attribution_2026, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(clim_be_t2024, climate_attribution_2026, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_attribution_2026, information_standard).
narrative_ontology:affects_constraint(climate_attribution_2026, fossil_fuel_subsidies).
narrative_ontology:affects_constraint(climate_attribution_2026, international_climate_agreements).
narrative_ontology:affects_constraint(climate_attribution_2026, corporate_liability_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
