% ============================================================================
% CONSTRAINT STORY: organization_floor_c0
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organization_floor_c0, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: organization_floor_c0
 *   human_readable: Organization Floor in Democratic Policy Markets
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   The organization floor in democratic policy markets describes a
 *   structural threshold below which mass preferences cannot be traded
 *   regardless of intensity or population size. Preferences become
 *   transactable only when coupled to organized infrastructure: warrantable
 *   leadership, punishment capability, delivery mechanisms, and continuous
 *   negotiating presence. This constraint appears as a natural law from most
 *   analytical perspectives — an emergent property of transaction costs,
 *   collective action problems, and information asymmetries in representative
 *   democracy. However, the systematic presence of identifiable beneficiaries
 *   who capture advantage from the floor's existence triggers false summit
 *   evaluation under the framework's FSM signature. KEY AGENTS (by structural
 *   relationship): - Organized labor unions, professional associations,
 *   industry coalitions, advocacy organizations: Primary beneficiaries
 *   (organized/powerful, mobile to arbitrage exit) — systematic advantage
 *   from floor's existence - Unorganized mass publics, diffuse consumer
 *   interests, geographically dispersed groups: Primary victims (powerless,
 *   trapped exit) — preferences structurally non-transactable - Political
 *   scientists: Analytical observers — study floor as emergent property -
 *   Institutional reformers: Excluded voices — propose floor-lowering
 *   mechanisms, face organized resistance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organization_floor_c0, 0.42).
domain_priors:suppression_score(organization_floor_c0, 0.38).
domain_priors:theater_ratio(organization_floor_c0, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organization_floor_c0, extractiveness, 0.42).
narrative_ontology:constraint_metric(organization_floor_c0, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(organization_floor_c0, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(organization_floor_c0, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(organization_floor_c0, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organization_floor_c0, mountain).
narrative_ontology:human_readable(organization_floor_c0, "Organization Floor in Democratic Policy Markets").
narrative_ontology:topic_domain(organization_floor_c0, "political_economy/democratic_theory/institutional_analysis").

domain_priors:emerges_naturally(organization_floor_c0).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organization_floor_c0, organized_labor_unions).
narrative_ontology:constraint_beneficiary(organization_floor_c0, professional_associations).
narrative_ontology:constraint_beneficiary(organization_floor_c0, industry_coalitions).
narrative_ontology:constraint_beneficiary(organization_floor_c0, advocacy_organizations).
narrative_ontology:constraint_victim(organization_floor_c0, unorganized_mass_publics).
narrative_ontology:constraint_victim(organization_floor_c0, diffuse_consumer_interests).
narrative_ontology:constraint_victim(organization_floor_c0, geographically_dispersed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain institutional infrastructure to aggregate member preferences, negotiate with political actors, deliver votes and campaign resources, and punish defection. The organization floor makes their preferences tradable while unorganized workers' preferences remain non-transactable, creating systematic advantage in policy markets regardless of preference intensity.
narrative_ontology:constraint_stakeholder(organization_floor_c0, organized_labor_unions, beneficiary,
    organized, generational, mobile, national).

% Convert diffuse professional interests into credible policy positions through credentialing authority, expert testimony infrastructure, and campaign contribution bundling. The floor ensures their organized voice carries weight disproportionate to membership size because they can deliver warrantable commitments while unorganized professionals cannot.
narrative_ontology:constraint_stakeholder(organization_floor_c0, professional_associations, beneficiary,
    organized, generational, mobile, national).

% Pool resources across firms to maintain lobbying operations, fund research, coordinate messaging, and deliver campaign contributions with credible conditionality. The organization floor converts their preferences into tradable assets while consumer preferences of equal or greater intensity remain structurally non-transactable.
narrative_ontology:constraint_stakeholder(organization_floor_c0, industry_coalitions, beneficiary,
    powerful, biographical, arbitrage, national).

% Build donor bases, maintain policy expertise, coordinate grassroots mobilization, and claim to speak for constituencies. The floor makes their organized voice tradable in policy markets while the diffuse publics they claim to represent cannot directly transact their own preferences.
narrative_ontology:constraint_stakeholder(organization_floor_c0, advocacy_organizations, beneficiary,
    organized, generational, constrained, national).

% Hold policy preferences that may be intense and widely shared but lack organizational infrastructure to make them tradable. Cannot credibly commit to vote delivery, cannot punish defection at scale, cannot maintain continuous negotiating presence. Their preferences exist but remain structurally non-transactable regardless of population size or intensity.
narrative_ontology:constraint_stakeholder(organization_floor_c0, unorganized_mass_publics, payer,
    powerless, immediate, trapped, national).

% Bear concentrated costs from policies negotiated by organized producer groups but cannot organize to trade their own preferences because per-capita stakes are too low to justify organization costs. The floor ensures their losses remain uncompensated while organized groups capture gains.
narrative_ontology:constraint_stakeholder(organization_floor_c0, diffuse_consumer_interests, payer,
    powerless, immediate, trapped, national).

% Share common interests across jurisdictions but face prohibitive coordination costs. The organization floor makes their preferences non-transactable even when aggregate intensity exceeds that of geographically concentrated organized groups who can trade in policy markets.
narrative_ontology:constraint_stakeholder(organization_floor_c0, geographically_dispersed_groups, payer,
    powerless, biographical, trapped, continental).

% Study the organization floor as an emergent property of transaction costs and collective action problems. Measure the systematic advantage it creates for organized interests and debate whether the floor is a natural law of democratic politics or a constructed constraint that could be altered by institutional design.
narrative_ontology:constraint_stakeholder(organization_floor_c0, political_scientists, observer,
    analytical, generational, analytical, global).

% Propose mechanisms to lower organization costs or create alternative channels for preference aggregation: participatory budgeting, citizen assemblies, quadratic voting, digital organizing platforms. Their proposals would reduce the floor's height but face resistance from existing organized interests who benefit from current thresholds.
narrative_ontology:constraint_stakeholder(organization_floor_c0, institutional_reformers, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the warrantability problem in democratic policy exchange: politicians need credible commitments that preferences are real, deliverable, and punishable if betrayed. Organization provides the infrastructure for these commitments.
% TRANSFER_FUNCTION: Transfers policy influence from unorganized mass publics to organized blocs. Organized groups gain systematic access to policy markets; unorganized groups with equal or greater preference intensity remain structurally excluded from trade.
% ABSENT_VOICES: Institutional reformers who would lower the floor through alternative aggregation mechanisms are structurally marginal to the policy markets the floor governs. Unorganized publics cannot voice their exclusion because the floor itself prevents them from organizing to do so.
% DISAPPEARANCE_RATIONALE: If the floor vanished, organized interests claim chaos would follow: politicians could not distinguish real from manufactured preferences, policy markets would collapse into noise, and governance would become impossible. Reformers claim alternative aggregation mechanisms could provide warrantability without the current exclusionary threshold, making the floor a constructed rather than natural constraint.
% FOUNDING_PROBLEM: Early democratic systems faced the warrantability crisis: how can representatives distinguish genuine constituency preferences from noise, and how can constituents credibly commit to reward or punish representatives for policy choices?
% FOUNDING_PROBLEM_CORROBORATION: Political scientists across theoretical traditions attest the warrantability problem remains live and organization provides one solution. However, institutional reformers and democratic theorists outside the organized-interest framework attest that alternative mechanisms (sortition, participatory budgeting, digital platforms) can solve warrantability without the current exclusionary floor, suggesting the specific height and form of the floor is constructed rather than inevitable.
narrative_ontology:disappearance_verdict(organization_floor_c0, contested).
narrative_ontology:founding_problem_status(organization_floor_c0, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(organization_floor_c0, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-12',
    'cohort_zero_regen', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'temperature=0.2').
narrative_ontology:story_seed(organization_floor_c0, 'organization_floor', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organization_floor_c0_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(organization_floor_c0, ExtMetricName, E),
    domain_priors:suppression_score(organization_floor_c0, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(organization_floor_c0),
    narrative_ontology:constraint_metric(organization_floor_c0, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(organization_floor_c0, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(organization_floor_c0_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the floor creates systematic advantage for organized interests over unorganized publics with equal or greater preference intensity, but the advantage operates through exclusion from trade rather than direct transfer. Suppression is moderate-low (0.38) because the floor does not actively prevent organization — it sets the threshold for transactability, and groups can organize if they clear coordination costs. Theater ratio is low (0.22) because the warrantability function is real and necessary; the question is whether the current floor height is natural or constructed. Accessibility collapse is high (0.78) because once the transaction cost logic is understood, alternatives to organization-based warrantability appear structurally difficult. Resistance is moderate-low (0.31) because most actors treat the floor as inevitable, though institutional reformers contest this.
 *   
 *   The measurement series shows gradual extraction accumulation over four decades as organized interests have learned to exploit the floor more systematically, and theater ratio rising as the warrantability justification increasingly covers rent-seeking behavior. Both series are observed historical trajectories, not projections.
 *
 * PERSPECTIVAL GAP:
 *   From the organized beneficiary seats, the floor appears as genuine coordination infrastructure they built and maintain — the warrantability problem is real, organization solves it, and their advantage is the return on investment in coordination capacity. From the powerless victim seats, the same structure operates as enforced exclusion — their preferences are as intense and their numbers as large, but the floor makes them structurally non-transactable. From the analytical seat, the floor appears as an emergent property of transaction costs, but the presence of systematic beneficiaries who would resist floor-lowering reforms suggests the 'natural law' framing may be a false summit. The engine computes these divergent classifications from the structural data; the claimed type (mountain) versus the measured extraction and beneficiary structure is the gap the FSM signature exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Organized beneficiaries (unions, associations, coalitions) sit near the beneficiary end of directionality: they built the organizational infrastructure and capture systematic advantage from the floor's existence, with mobile to arbitrage exit options allowing them to shift resources across policy domains. Unorganized victims (mass publics, diffuse consumers, dispersed groups) sit near the target end: they bear the costs of exclusion from policy markets, with trapped exit because the floor itself prevents them from organizing to escape it. Political scientists sit at analytical: they observe the structure without being positioned within it. The floor's extractiveness is amplified for trapped victims and damped for mobile beneficiaries in the engine's effective extraction computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The organization floor presents a classic mandatrophy risk: it solves a real coordination problem (warrantability in policy exchange) while simultaneously creating systematic extraction (organized interests gain advantage over unorganized publics with equal preference intensity). The constraint is not pure extraction — the warrantability function is genuine and necessary. But it is also not pure coordination — the specific height and form of the floor benefits organized interests in ways that exceed the minimum necessary for warrantability. Alternative mechanisms (participatory budgeting, sortition, digital platforms, quadratic voting) could potentially solve warrantability at lower exclusionary cost, but organized interests resist these reforms precisely because the current floor height is advantageous to them. The mandatrophy analysis prevents mislabeling this as pure mountain (ignoring the systematic beneficiaries and their resistance to reform) or pure snare (ignoring the genuine warrantability function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_floor_height,
    'Is the current height of the organization floor a natural consequence of irreducible transaction costs, or is it constructed and maintainable at the current level because organized interests benefit from keeping it high?',
    'Natural experiments from jurisdictions that successfully implement alternative aggregation mechanisms (participatory budgeting, citizen assemblies, digital platforms) while maintaining policy market functionality. If warrantability holds at lower organization thresholds, the current floor height is constructed.',
    'If the floor height is constructed rather than natural, the constraint reclassifies from mountain to tangled rope: genuine coordination function (warrantability) coupled with asymmetric extraction (organized interests benefit from exclusionary threshold). If the floor height is natural, it remains mountain despite beneficiary presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_floor_height, empirical, 'Whether current floor height is natural law or constructed constraint.').

omega_variable(
    alternative_warrantability_mechanisms,
    'Can alternative institutional designs provide warrantability in policy exchange without requiring the current level of organizational infrastructure?',
    'Comparative institutional analysis of participatory budgeting systems, sortition-based assemblies, quadratic voting mechanisms, and digital organizing platforms. Measure whether these mechanisms can credibly solve the warrantability problem (distinguish real from manufactured preferences, enable credible commitments) at lower organization costs.',
    'If alternative mechanisms can provide warrantability at lower thresholds, the organization floor is not a natural law but a path-dependent institutional choice that benefits current organized interests. This would support reclassification to tangled rope and justify floor-lowering reforms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_warrantability_mechanisms, conceptual, 'Whether warrantability requires current organizational threshold.').

omega_variable(
    beneficiary_resistance_to_reform,
    'Do organized interests actively resist institutional reforms that would lower the organization floor, and if so, does this resistance indicate the floor''s current height serves their interests rather than being a natural minimum?',
    'Historical analysis of organized interest responses to proposed reforms: participatory democracy initiatives, digital organizing platforms, campaign finance reforms that would reduce organization advantages. Measure whether resistance correlates with threat to organizational advantage.',
    'If organized interests systematically resist floor-lowering reforms, this suggests the current floor height is constructed to serve their interests rather than being a natural minimum for democratic functionality. This would support false summit reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_resistance_to_reform, empirical, 'Whether beneficiary resistance indicates constructed rather than natural floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organization_floor_c0, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orga_tr_t0, organization_floor_c0, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(orga_tr_t0, observed).
narrative_ontology:measurement(orga_tr_t8, organization_floor_c0, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(orga_tr_t8, observed).
narrative_ontology:measurement(orga_tr_t16, organization_floor_c0, theater_ratio, 16, 0.17).
narrative_ontology:measurement_basis(orga_tr_t16, observed).
narrative_ontology:measurement(orga_tr_t24, organization_floor_c0, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(orga_tr_t24, observed).
narrative_ontology:measurement(orga_tr_t32, organization_floor_c0, theater_ratio, 32, 0.21).
narrative_ontology:measurement_basis(orga_tr_t32, observed).
narrative_ontology:measurement(orga_tr_t40, organization_floor_c0, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(orga_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(orga_be_t0, organization_floor_c0, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(orga_be_t0, observed).
narrative_ontology:measurement(orga_be_t8, organization_floor_c0, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(orga_be_t8, observed).
narrative_ontology:measurement(orga_be_t16, organization_floor_c0, base_extractiveness, 16, 0.36).
narrative_ontology:measurement_basis(orga_be_t16, observed).
narrative_ontology:measurement(orga_be_t24, organization_floor_c0, base_extractiveness, 24, 0.39).
narrative_ontology:measurement_basis(orga_be_t24, observed).
narrative_ontology:measurement(orga_be_t32, organization_floor_c0, base_extractiveness, 32, 0.41).
narrative_ontology:measurement_basis(orga_be_t32, observed).
narrative_ontology:measurement(orga_be_t40, organization_floor_c0, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(orga_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(organization_floor_c0, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organization_floor_c0, resource_allocation).
narrative_ontology:boltzmann_floor_override(organization_floor_c0, 0.18).
narrative_ontology:affects_constraint(organization_floor_c0, collective_action_problem).
narrative_ontology:affects_constraint(organization_floor_c0, transaction_cost_barrier).
narrative_ontology:affects_constraint(organization_floor_c0, warrantability_requirement_policy_exchange).

% DUAL FORMULATION NOTE:
% The organization floor is one reading of a broader constraint family around preference aggregation in representative democracy. Related constraints include the collective action problem (why organization is costly), transaction cost barriers (why warrantability requires infrastructure), and warrantability requirements in policy exchange (why politicians need credible commitments). Each has different ε values and beneficiary structures; they are linked but distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
