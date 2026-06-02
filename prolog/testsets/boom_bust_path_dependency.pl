% ============================================================================
% CONSTRAINT STORY: boom_bust_path_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boom_bust_path_dependency, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: boom_bust_path_dependency
 *   human_readable: Alberta Boom-Bust Fiscal Cycle (Path Dependency)
 *   domain: economic/policy
 *
 * SUMMARY:
 *   Alberta's boom-bust fiscal cycle exemplifies the Piton type: a policy
 *   framework that was once functionally optimized for a particular context
 *   (maximizing resource extraction and reinvestment during commodity booms)
 *   but has degraded into institutional theater maintained by political
 *   inertia and beneficiary lock-in. The constraint emerges from three
 *   interlocking policy choices: (1) keeping resource royalty rates among the
 *   lowest in North America, (2) rejecting the Heritage Fund model proven by
 *   Norway and used by other commodity exporters, and (3) resisting
 *   progressive taxation that would smooth revenues across economic cycles.
 *   These choices create structural fiscal volatility — government revenue
 *   swings 20-40% between boom and bust, forcing destructive cycles of
 *   expansion and contraction in public services. The constraint persists not
 *   because it is an inevitable feature of commodity dependence
 *   (counterfactual comparisons with Norway and other jurisdictions show that
 *   alternative policies significantly reduce volatility) but because the
 *   incumbent political coalition and resource extraction industry benefit
 *   from maintaining it. The theater_ratio of 0.78 reflects the high
 *   performative content: political rhetoric celebrates booms as inevitable
 *   successes and treats busts as exogenous market shocks beyond policy
 *   control, obscuring that the volatility is policy-induced and therefore
 *   policy-addressable. The constraint is a Piton because its primary
 *   function (optimizing extraction during booms) has been attained, yet the
 *   policy framework persists through institutional inertia and political
 *   fear of policy change.
 *
 * KEY AGENTS:
 *   - Public Service System: Primary victim (powerless/trapped) — schools, hospitals, social services bear the full cost of fiscal instability with no exit option
 *   - Workers and Households: Secondary victim (moderate/constrained) — experience both coordination benefits (boom-time employment) and extraction (wage volatility, service degradation)
 *   - Resource Extraction Industry: Primary beneficiary (institutional/arbitrage) — benefits from low royalty rates, minimal windfall taxation, public infrastructure investment during booms; can threaten relocation if policy terms worsen
 *   - Incumbent Political Coalition: Secondary beneficiary (institutional/arbitrage) — captures electoral credit during booms; maintains political control through resource-sector support
 *   - Reform Coalition: Organized challenger (organized/constrained) — proposes Heritage Fund and counter-cyclical budgeting; faces high barriers to implementation
 *   - Fiscal Policy Framework: Institutional actor (institutional/arbitrage) — the policy ritual itself persists through discursive naturalization of boom-bust as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boom_bust_path_dependency, 0.38).
domain_priors:suppression_score(boom_bust_path_dependency, 0.52).
domain_priors:theater_ratio(boom_bust_path_dependency, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boom_bust_path_dependency, extractiveness, 0.38).
narrative_ontology:constraint_metric(boom_bust_path_dependency, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(boom_bust_path_dependency, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boom_bust_path_dependency, piton).
narrative_ontology:human_readable(boom_bust_path_dependency, "Alberta Boom-Bust Fiscal Cycle (Path Dependency)").
narrative_ontology:topic_domain(boom_bust_path_dependency, "economic/policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, resource_extraction_industry).
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, incumbent_political_coalition).
narrative_ontology:constraint_victim(boom_bust_path_dependency, fiscal_stability).
narrative_ontology:constraint_victim(boom_bust_path_dependency, public_service_funding).
narrative_ontology:constraint_victim(boom_bust_path_dependency, intergenerational_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC SERVICE SYSTEM (SNARE) — Schools, hospitals, social services face serial crises during downturns; expansion during booms is temporary and politically rewarded, contraction during busts is painful and inevitable. No exit from fiscal volatility — the system is structurally trapped in the boom-bust cycle. Maximum extraction: bears full cost of instability while lacking agency to change the constraint.
constraint_indexing:constraint_classification(boom_bust_path_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WORKERS AND HOUSEHOLDS (TANGLED ROPE) — Experience genuine coordination benefits from resource-sector employment during booms and access to publicly funded services. But also experience extraction: wage volatility, job insecurity, service degradation during busts. Can migrate out (constrained exit) but at career and social cost. Mixed experience — both coordination and extraction present.
constraint_indexing:constraint_classification(boom_bust_path_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESOURCE EXTRACTION INDUSTRY (ROPE) — Experiences the constraint as coordination: low royalty rates, minimal windfall taxation, and public infrastructure investment during booms create favorable operating environment. Benefits from the cycle without bearing stabilization costs. Arbitrage capacity allows arbitration between jurisdictions (can threaten relocation if terms worsen). Net beneficiary.
constraint_indexing:constraint_classification(boom_bust_path_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INCUMBENT POLITICAL COALITION (ROPE) — The policy consensus (low royalties, no Heritage Fund, limited progressive taxation) is reinforced by campaign financing from resource sector and political acceptance of boom-bust as inevitable. During booms, the coalition wins re-election on revenue growth; during busts, opposition is fragmented and resource-sector support remains solid. Arbitrage: can credibly threaten to leave office if policy changes (electoral mobility). Net beneficiary from the constraint.
constraint_indexing:constraint_classification(boom_bust_path_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Economists, social advocates, and opposition parties see the constraint's dysfunction clearly and propose alternatives (Heritage Fund, progressive taxation, counter-cyclical budgeting). But face high barriers to implementation: resource-sector opposition, political inertia, public acceptance of boom-bust as natural. Experience extraction (marginalization from policymaking) but also benefit from the constraint's visibility — reform proposals gain traction during busts when public appetite for change peaks. Constrained exit: can organize and advocate but cannot unilaterally override the incumbent coalition.
constraint_indexing:constraint_classification(boom_bust_path_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE POLICY RITUAL (PITON) — The boom-bust cycle itself has become performative theater. Policy rhetoric celebrates the boom as inevitable success ('the resource sector drives growth'), and busts are treated as exogenous shocks ('markets beyond our control'). The high theater_ratio reflects that much political discourse is about maintaining the narrative of resource-sector inevitability rather than actually addressing fiscal volatility. The constraint persists through institutional inertia — path dependency has become a cultural fact — even though the policy toolkit exists to escape it (Heritage Funds are proven models; counter-cyclical budgeting is standard practice). The constraint's primary function has atrophied; it is maintained because changing it would require the incumbent coalition to accept loss of control and the resource sector to accept reduced leverage.
constraint_indexing:constraint_classification(boom_bust_path_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN, FALSE SUMMIT CANDIDATE) — From a global/civilizational perspective, the boom-bust cycle could be naturalized as inherent to commodity-dependent economies: resource prices are volatile, wealth depends on exports, fiscal volatility is unavoidable. But this view naturalizes what is actually a policy choice: Norway, Canada's other provinces, and commodity-exporting nations demonstrate that Heritage Funds, progressive taxation, and counter-cyclical budgeting reduce (though do not eliminate) volatility. The mountain classification is a false summit — the constraint is sustained by political inertia and beneficiary opposition, not by the laws of economics.
constraint_indexing:constraint_classification(boom_bust_path_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boom_bust_path_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(boom_bust_path_dependency, TR),
    TR >= 0.70.

:- end_tests(boom_bust_path_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from public services and households through fiscal instability, but the extraction is not as severe as a pure Snare because beneficiaries also provide genuine coordination services (resource sector drives employment, public infrastructure investment improves productivity). The measurement trajectory (0.28 → 0.33 → 0.38) reflects that extractiveness has increased over the interval as beneficiaries have accumulated advantages during sustained boom periods and as public service systems have become more fragile. Suppression (0.52): Moderate-high. Significant barriers prevent escape: (1) political economy barriers (resource-sector campaign financing locks in incumbent coalition), (2) institutional barriers (path dependency in budget processes and public service structures), (3) discursive barriers (boom-bust naturalized as inevitable). But suppression is not total — reform coalitions exist, opposition parties propose alternatives, and some policy change is politically possible during crisis periods. Theater ratio (0.78): High and rising. Political rhetoric focuses on celebrating booms and explaining busts, creating theatrical narrative that obscures policy-addressability. The rise in theater_ratio from 0.62 to 0.78 reflects increasing discursive naturalization of boom-bust as inevitable feature of commodity dependence, despite evidence that policy alternatives reduce volatility. The Piton classification derives from this high theater combined with visible policy dysfunction — the constraint persists despite clear evidence of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximum: from Rope (beneficiaries) to Snare (victims) to Piton (institutional theater). No two perspectives agree on classification. This signals a highly contested constraint with clear winners and losers. The beneficiaries' experience of coordination (Rope) coexists with the victims' experience of pure extraction (Snare). The institutional view reveals the constraint has degraded into theater — the policy framework persists not because it coordinates or extracts effectively but because changing it would require political realignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) depends on their structural position relative to the extraction flow. Beneficiaries with exit options (resource sector, incumbent coalition) experience low or negative chi — the constraint delivers benefits. Victims with trapped or constrained exit (public services, workers) experience high chi — the constraint delivers extraction. Organized challengers (reform coalition) experience moderate chi — they can articulate alternatives but face implementation barriers. The mechanism is the directionality formula: beneficiaries' d is pulled toward 0 (beneficiary direction), victims' d pulled toward 1 (target direction). Applied to the baseline metrics, this produces the observed perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The boom-bust constraint resolves the mandatrophy by showing that Piton is a distinct category from pure extraction (Snare). The Piton classification indicates that the constraint's primary functional purpose (coordination of resource booms for maximum extraction and reinvestment) has been attained, yet the institutional framework persists despite degradation and despite available alternatives. The mandatrophy question — 'Is this extractive coordination or pure extraction?' — is answered by the theater_ratio gate: when theater exceeds 0.70, the constraint has shifted from functional to performative. The high theater_ratio (0.78) indicates that political rhetoric and institutional inertia now maintain the constraint more than either coordination benefits or extraction benefits. This is the Piton signal: the constraint still extracts from victims, but the primary mechanism is no longer functional — it is institutional maintenance through discursive naturalization and political path dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_beneficiary_lock,
    'Is the boom-bust constraint maintained primarily by institutional inertia and political path dependency, or primarily by the structural lock-in of beneficiaries (resource sector and incumbent coalition) whose interests depend on the constraint?',
    'Counterfactual analysis: What barriers prevent policy change? Political economy analysis of resource-sector campaign financing and political coalition structure. Comparative analysis with jurisdictions that successfully transitioned away from boom-bust (Norway, Saskatchewan-oil/minerals).',
    'If path dependency dominates: constraint may be reversible through institutional redesign or political realignment without direct beneficiary opposition. If beneficiary lock dominates: constraint requires political defeat of the incumbent coalition or resource-sector cooperation — higher barrier to change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_beneficiary_lock, conceptual, 'Whether constraint persistence is due to institutional inertia or beneficiary lock-in').

omega_variable(
    royalty_rate_counterfactual,
    'If Alberta had adopted Norway-style Heritage Fund and higher progressive taxation from 1970s onward, how much fiscal volatility would be reduced? What share of current volatility is policy-induced vs economically necessary?',
    'Historical simulation modeling: apply Norway''s policy parameters to Alberta''s resource revenue stream; compare predicted fiscal stability to actual outcome. Literature review of commodity-fund effectiveness across jurisdictions.',
    'If volatility reduction > 60%: constraint is significantly policy-induced; classification shifts toward snare/extraction. If volatility reduction < 20%: constraint is closer to a genuine natural law; classification toward mountain more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royalty_rate_counterfactual, empirical, 'Estimate of volatility reduction from Heritage Fund and progressive taxation').

omega_variable(
    public_service_adaptation_ceiling,
    'Can public service systems (education, healthcare, social services) functionally adapt to boom-bust fiscal cycles through built-in flexibility, or is there a structural ceiling beyond which volatility becomes destructive regardless of adaptation?',
    'Comparative analysis of educational outcomes, healthcare quality, social service utilization across stable vs volatile funding regimes. Longitudinal analysis of Alberta public services through boom-bust cycles: hiring freezes, program cuts, and downstream effects on service quality.',
    'If adaptation possible: constraint is extractive but manageable (Tangled Rope). If ceiling exists: extraction is severe and cumulative (Snare). High confidence would shift victim classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_service_adaptation_ceiling, empirical, 'Structural limits of public service adaptation to fiscal volatility').

omega_variable(
    political_coalition_stability_under_policy_change,
    'If policy shifted toward Heritage Fund and counter-cyclical budgeting, would the incumbent political coalition fragment, or would beneficiaries accept the new constraint in exchange for other policy concessions?',
    'Political simulation and electoral analysis. Case studies from provinces and nations that successfully shifted resource-based policies (Norway''s transition, Saskatchewan, Albertan provincial comparisons). Survey analysis of resource-sector and voter preferences under alternative scenarios.',
    'If coalition fragments: policy change is politically possible but requires electoral defeat of incumbents. If coalition adapts: policy change is possible through negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_coalition_stability_under_policy_change, preference, 'Political viability of escaping the boom-bust constraint').

omega_variable(
    false_summit_grounding,
    'Is the boom-bust cycle presented as ''natural to commodity economies'' (natural law framing) or as ''policy choice'' (contingent institutional framing)? Which framing dominates political and public discourse?',
    'Content analysis of political rhetoric, media coverage, and policy documents. Tracking of how busts are explained: as market outcomes vs as policy failures. Public opinion surveys on whether boom-bust is seen as inevitable or changeable.',
    'If natural law framing dominates: false summit is operational (policy naturalized); constraint is maintained through discursive lock. If policy framing visible: false summit is contested; change is politically possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_grounding, conceptual, 'Whether boom-bust is framed as natural law or policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boom_bust_path_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bbpd_tr_t0, boom_bust_path_dependency, theater_ratio, 0, 0.62).
narrative_ontology:measurement(bbpd_tr_t5, boom_bust_path_dependency, theater_ratio, 5, 0.7).
narrative_ontology:measurement(bbpd_tr_t10, boom_bust_path_dependency, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(bbpd_be_t0, boom_bust_path_dependency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bbpd_be_t5, boom_bust_path_dependency, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(bbpd_be_t10, boom_bust_path_dependency, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bbpd_su_t0, boom_bust_path_dependency, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bbpd_su_t5, boom_bust_path_dependency, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(bbpd_su_t10, boom_bust_path_dependency, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boom_bust_path_dependency, resource_allocation).
narrative_ontology:affects_constraint(boom_bust_path_dependency, public_service_fiscal_fragility).
narrative_ontology:affects_constraint(boom_bust_path_dependency, intergenerational_resource_equity).
narrative_ontology:affects_constraint(boom_bust_path_dependency, regulatory_capture_resource_sector).

% DUAL FORMULATION NOTE:
% The boom-bust path dependency decomposes into multiple downstream constraints. The fiscal volatility constraint (this story) operates at the macro-policy level; public service degradation operates at the service-delivery level; resource-sector regulatory capture operates at the institutional-capture level. Each has its own extractiveness value reflecting its structural position. They are linked by the upstream boom-bust cycle: volatility drives service degradation, and regulatory capture locks in the low-royalty-rate policy that sustains volatility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(boom_bust_path_dependency, institutional, 0.12).
constraint_indexing:directionality_override(boom_bust_path_dependency, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
