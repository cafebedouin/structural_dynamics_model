% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The baseload necessity reading of climate mitigation legitimacy treats
 *   grid stability as a hard physical constraint that mandates centralized
 *   dispatchable generationâprincipally nuclearâas the foundation of any
 *   serious decarbonization strategy. Under this reading, renewable energy is
 *   framed as valuable but inherently supplementary because it cannot deliver
 *   the continuous, controllable output required by modern electrical
 *   systems. The constraint channels public finance, licensing priority, and
 *   long-term capital toward nuclear and incumbent baseload assets while
 *   categorizing renewable-only pathways as technically inadequate. This
 *   story authors the reading as a tangled rope: it coordinates genuine grid
 *   reliability concerns, but asymmetrically benefits concentrated baseload
 *   incumbents and the nuclear industry while constraining renewable
 *   developers and distributed-energy advocates.
 *
 * KEY AGENTS:
 *   - Nuclear industry: Primary beneficiary (powerful/generational/constrained) â designated as necessary infrastructure receiving public finance and guarantees.
 *   - Incumbent baseload utilities: Secondary beneficiary (institutional/generational/constrained) â long-lived asset values preserved by resource adequacy frameworks.
 *   - Renewable project developers: Primary target (moderate/biographical/constrained) â market expansion capped by the presumption of renewable inadequacy.
 *   - Resource adequacy regulators: Agenda-setter (institutional/generational/constrained) â embed baseload assumptions into standards and markets.
 *   - Distributed renewable advocates: Excluded voice (moderate/biographical/constrained) â technical alternatives excluded from planning frameworks.
 *   - Climate policy analysts: Analytical observer (analytical/generational/analytical) â tracks capital allocation and emissions efficiency across pathways.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'dd097975-5a5f-42eb-84f9-5ef3bfc2bac3').
narrative_ontology:cs_kernel_codification('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', formalized).
narrative_ontology:cs_authority_grounding('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', expertise).
narrative_ontology:cs_interpretation_layer_present('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3').
narrative_ontology:cs_reading_relation('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', foundational, dispatchable_baseload_physical_necessity).
narrative_ontology:cs_axiom_status(dispatchable_baseload_physical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', dispatchable_baseload_physical_necessity, empirically_contingent).
narrative_ontology:cs_axiom('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', foundational, renewable_inadequacy_for_grid_stability).
narrative_ontology:cs_axiom_status(renewable_inadequacy_for_grid_stability, holdable).
narrative_ontology:cs_axiom_grounding('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', renewable_inadequacy_for_grid_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', centralized_grid_stability_framework).
narrative_ontology:cs_drift_state('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', post_renewable_cost_parity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dd097975-5a5f-42eb-84f9-5ef3bfc2bac3', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_baseload_utilities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_project_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives policy designation as necessary baseload infrastructure, accessing public finance, loan guarantees, and streamlined licensing justified by the grid stability imperative. Their commercial viability and investor confidence depend on the constraint classifying renewables as inadequate for reliable decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, national).

% Own long-lived dispatchable generation assets whose market value and capacity payments are preserved by resource adequacy frameworks that embed baseload necessity. Benefit from regulatory structures treating firm capacity as non-substitutable regardless of storage or demand-side alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_baseload_utilities, beneficiary,
    institutional, generational, constrained, national).

% Face caps on market share, interconnection priority, and financing because planning frameworks presume their output is inherently variable and non-dispatchable. Must accept supplementary or peaking roles, limiting return on capital and preventing renewable-only portfolio development.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_project_developers, payer,
    moderate, biographical, constrained, national).

% Write and enforce resource adequacy standards, capacity markets, and interconnection rules that embed baseload assumptions into grid planning. Their technical mandate to prevent outages is interpreted under this reading as requiring firm dispatchable capacity, which constrains their ability to incorporate high-renewable scenarios.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, resource_adequacy_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Promote decentralized renewable generation, demand flexibility, and storage as sufficient for decarbonization. Their modeling and policy proposals are rarely admitted into official resource adequacy assessments because the baseload frame treats distributed resources as inherently inadequate regardless of empirical performance in high-renewable jurisdictions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_renewable_advocates, excluded,
    moderate, biographical, constrained, national).

% Evaluate decarbonization pathways against cost, speed, and emissions reduction potential. They observe the capital allocation consequences of the baseload constraint and track whether the engineering rationale matches current grid flexibility economics.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains grid reliability and resource adequacy by ensuring continuous dispatchable generation is available to balance variable renewable output and prevent supply-demand mismatch that could cause instability or blackouts.
% TRANSFER_FUNCTION: Moves capital investment, policy support, licensing priority, and regulatory accommodation from distributed renewable expansion toward centralized nuclear and baseload infrastructure, while assigning renewable developers supplementary or peaking market roles.
% ABSENT_VOICES: Distributed energy advocates and 100% renewable system researchers argue that storage, demand response, and sector coupling eliminate the need for baseload; they are structurally excluded from resource adequacy planning and interconnection standard-setting.
% DISAPPEARANCE_RATIONALE: If the baseload necessity constraint disappeared, resource adequacy standards would be rewritten around flexibility and storage, nuclear projects would lose their privileged necessary status and financing, capital would reallocate toward distributed renewables, and grid operators would redesign markets around variable rather than firm capacity.
% FOUNDING_PROBLEM: Variable renewable energy introduces intermittency that, without continuous dispatchable generation, risks grid instability, frequency deviation, and blackouts.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and nuclear industry attest the problem remains live and requires firm capacity. Renewable industry and academic grid modelers attest that storage and demand response have matured to address intermittency; independent system operators in high-renewable regions report reliability without baseload, corroborating from outside the beneficiary set.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial capital reallocation and pathway blocking the constraint accomplishes. Suppression (0.58) tracks the active policy, regulatory, and discursive work required to maintain baseload's privileged status against improving renewable and storage economics. Theater ratio (0.35) captures the growing performative gap between baseload necessity rhetoric and evolving grid engineering practice in high-renewable jurisdictions. Accessibility collapse (0.65) reflects that renewable alternatives are technically knowable but politically and institutionally inaccessible once the baseload frame dominates planning. Resistance (0.55) records sustained pushback from renewable developers, distributed-energy researchers, and jurisdictions demonstrating high-renewable reliability. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (nuclear industry, baseload incumbents) experience the constraint as necessary infrastructure and legitimate coordination; the payer seat (renewable developers) experiences it as a technology-lock mechanism that extracts market opportunity and policy space. The excluded seat (distributed-energy advocates) experiences it as epistemic erasure from resource adequacy discourse. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and incumbent baseload utilities are structural beneficiaries (low d, subsidized by the constraint's operation and policy priority). Renewable project developers are structural targets (high d, extraction directed at their market expansion and access to capital). Resource adequacy regulators sit as agenda-setters with moderate dâthey enforce the constraint but are partly captured by the engineering paradigm and institutional history it embodies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâgrid instability from variable renewable penetration without firm backupâwas genuine in early renewable integration. Its status is contested because storage, demand response, and advanced grid management have matured substantially. The constraint risks mandatrophy if it persists by framing baseload as categorically necessary even after technical conditions have shifted; the rising theater_ratio and extractiveness trajectory over the interval monitor this drift toward performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_status_of_baseload_claim,
    'Is the claim that renewables cannot provide reliable baseload at scale empirically true given current storage and grid flexibility technologies, or is it a constructed scarcity that serves incumbent interests?',
    'Cross-jurisdictional natural experiments: grids with >80% renewable penetration that maintain reliability without baseload would falsify the necessity claim; persistent reliance on baseload in such grids would support it.',
    'If falsified, the constraint''s extraction is revealed as pure rent transfer to baseload incumbents (snare migration); if supported, the coordination function is validated and extraction is the necessary cost of reliability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_of_baseload_claim, empirical, 'Whether baseload necessity is physical law or incumbent construction').

omega_variable(
    capital_concentration_mechanism,
    'Does the baseload necessity constraint concentrate capital in long-lived assets as an intended extraction mechanism or as an unavoidable side effect of engineering requirements?',
    'Comparative analysis of grid flexibility investments: if cheaper flexibility alternatives are systematically excluded from resource adequacy modeling, the concentration is structural extraction; if no cheaper alternatives exist, it is coordination cost.',
    'Would reclassify the constraint''s extractiveness and inform whether the theater_ratio tracks real engineering conservatism or performative maintenance of incumbent advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_concentration_mechanism, conceptual, 'Whether capital concentration is intentional extraction or coordination cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel, which decomposes into four structurally distinct claims about decarbonization strategy. Each reading has a different beneficiary/victim structure, empirical referent, and Îµ value; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
