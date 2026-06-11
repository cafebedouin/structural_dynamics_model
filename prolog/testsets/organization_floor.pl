% ============================================================================
% CONSTRAINT STORY: organization_floor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organization_floor, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: organization_floor
 *   human_readable: Organization Floor in Democratic Policy Markets
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   The organization floor describes a structural threshold in democratic
 *   policy markets: mass preferences become tradable only when coupled to
 *   organized blocs with warrantable leadership, punishment capability, and
 *   delivery infrastructure. Below this floor, preferences are
 *   non-transactable regardless of population size or intensity. This
 *   constraint appears as a natural law from most perspectives — an emergent
 *   property of transaction cost economics and collective action problems.
 *   However, the presence of identifiable beneficiaries (organized labor,
 *   professional associations, industry coalitions) who capture systematic
 *   advantage from the floor's existence triggers false summit evaluation.
 *   The constraint exhibits low extraction (0.15) because the floor genuinely
 *   solves coordination problems, but the extraction is non-zero because
 *   organized blocs benefit from the entry barrier the floor creates. The
 *   theater ratio (0.20) reflects modest performative content: some
 *   organizational infrastructure is maintained for signaling purposes rather
 *   than functional coordination. The constraint's accessibility collapse
 *   (0.88) is high — once you understand that unorganized preferences cannot
 *   trade in policy markets, alternative pathways largely disappear.
 *   Resistance (0.12) is low — the constraint is widely accepted as a fact of
 *   political life, though aspiring organizers work to cross the threshold.
 *
 * KEY AGENTS:
 *   - Unorganized Citizens: Primary non-beneficiary (powerless/trapped) — hold preferences but cannot convert them to policy influence without crossing the organization floor
 *   - Aspiring Organizers: Moderate agents (moderate/constrained) — recognize the floor as a surmountable threshold requiring specific organizational technology; work to build the infrastructure needed to cross it
 *   - Established Unions: Primary beneficiary (organized/mobile) — have crossed the floor and now benefit from tradable preference bundles; the floor protects their position by raising entry costs for competitors
 *   - Professional Associations: Institutional beneficiary (institutional/arbitrage) — operate well above the floor with full arbitrage capacity; capture value by creating tradability
 *   - Industry Coalitions: Institutional beneficiary (institutional/arbitrage) — organized blocs that benefit from the floor's existence by maintaining oligopolistic position in policy markets
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the floor as an emergent property of transaction cost economics, but risks naturalizing a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organization_floor, 0.15).
domain_priors:suppression_score(organization_floor, 0.25).
domain_priors:theater_ratio(organization_floor, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organization_floor, extractiveness, 0.15).
narrative_ontology:constraint_metric(organization_floor, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(organization_floor, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(organization_floor, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(organization_floor, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organization_floor, mountain).
narrative_ontology:human_readable(organization_floor, "Organization Floor in Democratic Policy Markets").
narrative_ontology:topic_domain(organization_floor, "political_economy/democratic_theory/institutional_analysis").

domain_priors:emerges_naturally(organization_floor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organization_floor, organized_labor_blocs).
narrative_ontology:constraint_beneficiary(organization_floor, professional_associations).
narrative_ontology:constraint_beneficiary(organization_floor, industry_coalitions).
narrative_ontology:constraint_beneficiary(organization_floor, institutional_advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(organization_floor, established_unions).
narrative_ontology:constraint_victim(organization_floor, aspiring_organizers).
narrative_ontology:constraint_vindicates(organization_floor, collective_action_theory).
narrative_ontology:constraint_vindicates(organization_floor, transaction_cost_economics).
narrative_ontology:constraint_vindicates(organization_floor, olsonian_logic_of_collective_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold policy preferences, sometimes intensely, but lack organizational infrastructure to convert preferences into tradable political influence. Cannot access policy markets as individual actors. Exit requires crossing the organization floor by building or joining a bloc with warrantable leadership and delivery capability.
narrative_ontology:constraint_stakeholder(organization_floor, unorganized_citizens, excluded,
    powerless, immediate, trapped, local).

% Working to build organizational infrastructure (leadership selection, punishment mechanisms, delivery systems) to cross the floor. Bear the coordination costs and startup risks. Constrained by resource requirements and collective action problems but not trapped — the floor is surmountable over biographical time with sufficient effort and resources.
narrative_ontology:constraint_stakeholder(organization_floor, aspiring_organizers, payer,
    moderate, biographical, constrained, regional).

% Have crossed the organization floor and now operate as recognized blocs in policy markets. Benefit from tradable preference bundles and from the entry barrier the floor creates for potential competitors. Invest in maintaining organizational infrastructure (union density, bloc cohesion, delivery capability) to preserve position above the floor.
narrative_ontology:constraint_stakeholder(organization_floor, established_unions, beneficiary,
    organized, generational, mobile, national).

% Institutional actors operating well above the organization floor with full arbitrage capacity. Convert diffuse professional preferences into negotiable policy positions. Capture value by creating tradability where none existed for individual professionals. Minimal extraction — the association's coordination function is genuine.
narrative_ontology:constraint_stakeholder(organization_floor, professional_associations, beneficiary,
    institutional, generational, arbitrage, national).

% Organized blocs representing industry interests in policy markets. Benefit from the organization floor by maintaining oligopolistic position — the floor raises entry costs for new competitors and ensures that only well-resourced, organized actors can trade in policy markets. Actively defend high organizational requirements through institutional design.
narrative_ontology:constraint_stakeholder(organization_floor, industry_coalitions, beneficiary,
    institutional, generational, arbitrage, national).

% Study the organization floor as an emergent property of transaction cost economics and collective action theory. Analytical position with no direct stake in the constraint's operation. Risk naturalizing contingent institutional arrangements by treating the floor as a universal constant rather than investigating whether its height varies with institutional design.
narrative_ontology:constraint_stakeholder(organization_floor, political_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(organization_floor, diffuse).
narrative_ontology:fixing_cost_class(organization_floor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The organization floor solves the collective action problem in policy markets by creating tradable preference bundles. Without organizational infrastructure (warrantable leadership, punishment capability, delivery mechanisms), individual preferences cannot credibly commit to policy positions or deliver votes/resources. The floor enables coordination by establishing minimum viable infrastructure for preference aggregation and commitment.
% TRANSFER_FUNCTION: The constraint transfers policy influence from unorganized citizens (who hold preferences but cannot trade them) to organized blocs (who can aggregate preferences into tradable bundles). The transfer is not primarily monetary but positional: organized actors gain access to policy markets that unorganized actors cannot enter. Secondary transfers include resources flowing to organizational infrastructure (union dues, association fees, coalition funding) to maintain position above the floor.
% ABSENT_VOICES: Unorganized citizens with intense preferences who cannot access policy markets. They are not literally absent from the political system but are structurally excluded from policy negotiation because they lack organizational coupling. Their preferences are visible in polls and elections but non-tradable in policy markets. They are 'here' as voters but 'not here' as negotiating parties.
% DISAPPEARANCE_RATIONALE: If the organization floor disappeared overnight — if individual preferences became directly tradable in policy markets without organizational coupling — the entire structure of democratic politics would rearrange. Unions, professional associations, and industry coalitions would lose their intermediary function. Policy responsiveness would shift from organized blocs to direct preference intensity. The change would be structural, not marginal: the floor is not a friction on an underlying market but a constitutive feature of how preferences become tradable.
% FOUNDING_PROBLEM: The organization floor emerged to solve the transaction cost and commitment problems inherent in aggregating diffuse preferences into coherent policy positions. In the absence of organizational infrastructure, individual citizens cannot credibly commit to policy positions, cannot punish defection, and cannot deliver coordinated action. The floor represents the minimum viable infrastructure needed to make preferences tradable in policy markets.
% FOUNDING_PROBLEM_CORROBORATION: Transaction cost economics (Coase, Williamson) and collective action theory (Olson) provide theoretical grounding. Empirical political science (Schlozman, Skocpol, Hacker & Pierson) documents persistent correlation between organizational infrastructure and policy responsiveness across multiple democracies and time periods. The problem is corroborated by observers outside the beneficiary set: political scientists studying representation gaps, journalists documenting policy unresponsiveness to mass preferences, and reform advocates working to lower organizational barriers.
narrative_ontology:disappearance_verdict(organization_floor, world_rearranges).
narrative_ontology:founding_problem_status(organization_floor, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED CITIZEN (MOUNTAIN) — Experiences the organization floor as an immutable barrier. Individual preferences, no matter how intensely held, cannot translate into policy influence without organizational infrastructure. The constraint appears as a natural law of political markets: you cannot trade what you cannot warrant delivery on.
constraint_indexing:constraint_classification(organization_floor, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ASPIRING ORGANIZER (MOUNTAIN) — Recognizes the floor as a structural threshold requiring specific organizational technology (leadership selection, punishment mechanisms, delivery infrastructure). Sees the constraint as changeable in principle over biographical time but immutable at immediate scale. The floor is not arbitrary — it reflects real coordination costs.
constraint_indexing:constraint_classification(organization_floor, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED UNION (ROPE) — Experiences the organization floor as a coordination mechanism that solves the collective action problem. The floor creates tradable preference bundles where none existed before. Net beneficiary: the constraint enables rather than extracts. Union density and bloc infrastructure are investments that pay returns in policy responsiveness.
constraint_indexing:constraint_classification(organization_floor, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL ASSOCIATION (ROPE) — Institutional actor with full arbitrage capacity. The organization floor is pure coordination infrastructure: it converts diffuse professional preferences into negotiable policy positions. Minimal extraction — the association captures value by creating tradability where none existed.
constraint_indexing:constraint_classification(organization_floor, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the organization floor appears as a structural feature of any preference aggregation system operating under information asymmetry and commitment problems. The floor is not a policy choice but an emergent property of transaction cost economics: preferences become tradable only when coupled to credible commitment mechanisms. This is the natural law view — but the presence of identifiable beneficiaries (organized blocs) triggers false summit evaluation.
constraint_indexing:constraint_classification(organization_floor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organization_floor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organization_floor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organization_floor, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(organization_floor, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organization_floor, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(organization_floor, ExtMetricName, E),
    domain_priors:suppression_score(organization_floor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(organization_floor),
    narrative_ontology:constraint_metric(organization_floor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(organization_floor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(organization_floor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low but non-zero. The organization floor genuinely solves collective action problems — it creates tradability where none existed, which is a coordination function. However, the floor also creates systematic advantage for those who have already crossed it by raising entry costs for new competitors. The extraction is the rent captured by incumbent organized blocs from the barrier-to-entry effect. This is substantially lower than the original hypothesis might suggest because much of what appears as extraction is actually legitimate coordination cost. Suppression (0.25): Low-moderate. The floor creates barriers to political influence for unorganized citizens, but these barriers are not primarily coercive — they reflect real coordination costs. Suppression increases modestly over the interval as organizational requirements become more complex (professionalized staff, legal compliance, digital infrastructure). Theater ratio (0.20): Low. Most organizational infrastructure serves genuine coordination functions (leadership selection, preference aggregation, delivery mechanisms). Some infrastructure is maintained for signaling purposes (office buildings, formal titles, ritualized procedures), but the performative content is modest. Accessibility collapse (0.88): Very high. Once you understand that preferences require organizational coupling to become tradable, alternative pathways largely disappear. You cannot wish away transaction costs or commitment problems. Resistance (0.12): Very low. The organization floor is widely accepted as a structural feature of democratic politics. Resistance comes primarily from aspiring organizers working to cross the threshold, not from fundamental rejection of the floor's existence.
 *
 * PERSPECTIVAL GAP:
 *   The organization floor demonstrates a critical pattern: a constraint can appear as a natural law (mountain) from multiple perspectives while still having identifiable beneficiaries who capture systematic advantage from its existence. The unorganized citizen experiences the floor as an immutable barrier — individual action cannot overcome it. The aspiring organizer sees it as a structural threshold requiring specific technology but changeable over biographical time. The established union and professional association experience it as coordination infrastructure that enables rather than extracts — they are net beneficiaries. The analytical observer sees an emergent property of transaction cost economics. All of these perspectives are structurally accurate. The false summit question is whether the floor's height (the minimum viable organizational infrastructure required) is a universal constant determined by transaction costs, or whether it is partly constructed by institutional design choices that benefit incumbent organized blocs. If the latter, then what appears as a natural law is partly a contingent arrangement that naturalizes incumbent advantage.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the organization floor. Unorganized citizens are non-beneficiaries — they hold preferences but cannot trade them, so they experience the floor as a barrier (high d, though not maximum because the floor is not actively extracting from them, merely excluding them). Aspiring organizers are working to cross the threshold — they bear the cost of building organizational infrastructure but are not yet beneficiaries (moderate d). Established unions and professional associations are clear beneficiaries — they have crossed the floor and now benefit from tradable preference bundles, plus they gain from the entry barrier the floor creates for competitors (low d, negative effective extraction). The analytical observer has no direct stake in the constraint's operation (d near 0.5, symmetric). The key insight: beneficiary status does not automatically disqualify a mountain classification, but it does trigger false summit evaluation. The question is whether the beneficiaries are capturing rents from a natural coordination requirement or from a constructed barrier that naturalizes their advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a critical mandatrophy: how to distinguish genuine natural laws (mountains) from false summits (constructed constraints that naturalize incumbent advantage). The organization floor appears as a mountain from most perspectives because it reflects real transaction costs and coordination problems. However, the presence of systematic beneficiaries (organized blocs who gain from the entry barrier) means the constraint cannot be accepted as a pure natural law without investigation. The false summit detector evaluates whether the floor's height is a universal constant or a constructed feature of specific institutional arrangements. If cross-national comparison shows that floor height varies with institutional design (ballot access rules, recognition thresholds, procedural barriers), and if organized blocs actively defend high organizational requirements, then the mountain classification naturalizes a contingent arrangement. The mandatrophy is not 'is this a mountain or not?' but 'what evidence would distinguish a genuine transaction cost minimum from a constructed barrier?' The omega variables specify the empirical tests that would resolve this question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    floor_height_variability,
    'Is the organization floor''s height (minimum viable organizational infrastructure) a universal constant or does it vary with institutional context, technology, and political culture?',
    'Cross-national comparison of minimum viable organization size and infrastructure requirements for policy influence; historical analysis of how communication technology and institutional design affect floor height',
    'If universal constant: genuine mountain (transaction costs are invariant). If context-dependent: the floor is a constructed feature of specific institutional arrangements, and the mountain classification naturalizes contingent design choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(floor_height_variability, empirical, 'Whether organization floor height is universal or context-dependent').

omega_variable(
    digital_organizing_threshold,
    'Do digital organizing tools (social media, crowdfunding, distributed coordination platforms) lower the organization floor, or do they merely shift the bottleneck to different organizational requirements?',
    'Comparative analysis of policy responsiveness for digitally-organized vs traditionally-organized groups controlling for preference intensity and population size; measurement of whether digital tools reduce minimum viable infrastructure or just change its form',
    'If floor is lowered: the constraint is weakening (scaffold dynamics). If bottleneck shifts: the floor is adapting but not disappearing, supporting mountain classification with evolving manifestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_organizing_threshold, empirical, 'Whether digital tools lower the organization floor or shift the bottleneck').

omega_variable(
    beneficiary_naturalization,
    'Is the organization floor a genuine transaction cost minimum (mountain) or a constructed barrier that benefits existing organized blocs by raising entry costs for new competitors (false summit)?',
    'Analysis of whether organized blocs actively defend high organizational requirements through institutional design (ballot access rules, recognition thresholds, procedural barriers); measurement of correlation between floor height and incumbent bloc advantage',
    'If genuine transaction cost: mountain classification holds. If constructed barrier: false summit — organized blocs benefit from naturalizing a contingent institutional arrangement as an immutable coordination requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalization, conceptual, 'Whether the floor is a natural transaction cost or a constructed entry barrier').

omega_variable(
    preference_intensity_threshold,
    'Does the organization floor apply uniformly across preference intensities, or do sufficiently intense preferences (existential threats, identity-core issues) bypass the floor through spontaneous coordination?',
    'Historical analysis of crisis mobilization and spontaneous collective action; measurement of whether high-intensity preferences generate policy responsiveness without formal organizational infrastructure',
    'If uniform application: supports mountain (the floor is a structural feature regardless of intensity). If intensity bypass exists: the floor is a coordination mechanism for routine politics but not a universal law — intensity can substitute for organization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_intensity_threshold, empirical, 'Whether high-intensity preferences can bypass the organization floor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organization_floor, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(org_floor_theater_1950, organization_floor, theater_ratio, 0, 0.15).
narrative_ontology:measurement(org_floor_theater_1970, organization_floor, theater_ratio, 20, 0.18).
narrative_ontology:measurement(org_floor_theater_1990, organization_floor, theater_ratio, 40, 0.2).
narrative_ontology:measurement(org_floor_theater_2010, organization_floor, theater_ratio, 60, 0.22).
narrative_ontology:measurement(org_floor_theater_2025, organization_floor, theater_ratio, 75, 0.2).

% Extraction over time
narrative_ontology:measurement(org_floor_extract_1950, organization_floor, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(org_floor_extract_1970, organization_floor, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(org_floor_extract_1990, organization_floor, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(org_floor_extract_2010, organization_floor, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(org_floor_extract_2025, organization_floor, base_extractiveness, 75, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(org_floor_suppress_1950, organization_floor, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(org_floor_suppress_1970, organization_floor, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(org_floor_suppress_1990, organization_floor, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(org_floor_suppress_2010, organization_floor, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(org_floor_suppress_2025, organization_floor, suppression_requirement, 75, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organization_floor, resource_allocation).
narrative_ontology:affects_constraint(organization_floor, ballot_access_requirements).
narrative_ontology:affects_constraint(organization_floor, campaign_finance_thresholds).
narrative_ontology:affects_constraint(organization_floor, union_recognition_procedures).
narrative_ontology:affects_constraint(organization_floor, lobbying_registration_rules).

% DUAL FORMULATION NOTE:
% The organization floor is upstream of specific institutional barriers (ballot access, campaign finance, union recognition) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting the specific rules and their enforcement; the organization floor has its own extractiveness reflecting the transaction cost minimum and any constructed entry barrier layered on top of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
