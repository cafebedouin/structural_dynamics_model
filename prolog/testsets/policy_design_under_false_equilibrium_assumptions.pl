% ============================================================================
% CONSTRAINT STORY: policy_design_under_false_equilibrium_assumptions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_policy_design_under_false_equilibrium_assumptions, []).

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
 *   constraint_id: policy_design_under_false_equilibrium_assumptions
 *   human_readable: Policy Design Under False Equilibrium Assumptions
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Policy design under false equilibrium assumptions creates a structural
 *   tension between the technical requirements of governance and the
 *   institutional incentives that preserve analytically questionable models.
 *   When policymakers design using equilibrium frameworks that assume no
 *   systematic disequilibrium, and conditions change or assumptions are
 *   violated, the policy fails to adapt — not because of implementation
 *   errors, but because the underlying model assumes away the very mechanisms
 *   that drive policy failure. This constraint exhibits Tangled Rope
 *   characteristics: there is genuine coordination function (equilibrium
 *   simplification does enable delegation and political communication)
 *   alongside asymmetric extraction (implementation actors and affected
 *   stakeholders bear costs of policy mismatch; policy establishment and
 *   incumbent interests benefit from stability assumptions). The constraint's
 *   theater ratio (0.68) reflects that equilibrium methodology persists as
 *   ritual in many policy contexts despite documented empirical failures —
 *   the methodology conveys authority and simplicity, which policymakers
 *   value even when the models make poor predictions. The core extraction
 *   mechanism is distributional: policies designed under false assumptions
 *   lock relative positions (incumbent firms maintain market share,
 *   established sector advantages persist) while imposing adaptation costs on
 *   non-incumbents and frontline implementers.
 *
 * KEY AGENTS:
 *   - Policy Establishment: Primary beneficiary (institutional/arbitrage) — central banks, treasuries, regulatory agencies; benefits from equilibrium assumption's simplicity and legitimating power; high exit optionality
 *   - Incumbent Economic Interests: Primary beneficiary (institutional/arbitrage) — large firms, rent-seeking sectors; benefit from policies that lock relative positions through false stability assumptions
 *   - Frontline Implementation Actors: Primary victim (powerless/trapped) — bureaucrats, field workers; must execute policies based on violated assumptions; bear reputational and career cost of policy failure; no exit authority
 *   - Affected Stakeholders: Secondary victim (moderate/constrained) — businesses, individuals, communities targeted by policy; face adaptation costs from policy misdirection; high exit barriers
 *   - Policy Reform Coalition: Mixed (organized/constrained) — economists, think tanks, researchers; coordinate on improved methodology but captured by same institutional incentives for theoretical elegance
 *   - International Policy Adopters: Mixed (moderate/mobile) — developing nations copying frameworks calibrated to different structural conditions; mobile globally but constrained domestically
 *   - Equilibrium Theoretical Tradition: Institutional (institutional/arbitrage) — intellectual apparatus persisting through inertia; careers built on equilibrium methodology; piton classification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice as analytical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(policy_design_under_false_equilibrium_assumptions, 0.58).
domain_priors:suppression_score(policy_design_under_false_equilibrium_assumptions, 0.62).
domain_priors:theater_ratio(policy_design_under_false_equilibrium_assumptions, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(policy_design_under_false_equilibrium_assumptions, extractiveness, 0.58).
narrative_ontology:constraint_metric(policy_design_under_false_equilibrium_assumptions, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(policy_design_under_false_equilibrium_assumptions, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(policy_design_under_false_equilibrium_assumptions, tangled_rope).
narrative_ontology:human_readable(policy_design_under_false_equilibrium_assumptions, "Policy Design Under False Equilibrium Assumptions").
narrative_ontology:topic_domain(policy_design_under_false_equilibrium_assumptions, "political_economy/governance").

domain_priors:requires_active_enforcement(policy_design_under_false_equilibrium_assumptions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(policy_design_under_false_equilibrium_assumptions, policy_establishment).
narrative_ontology:constraint_beneficiary(policy_design_under_false_equilibrium_assumptions, incumbent_economic_interests).
narrative_ontology:constraint_victim(policy_design_under_false_equilibrium_assumptions, policy_adapters).
narrative_ontology:constraint_victim(policy_design_under_false_equilibrium_assumptions, system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE IMPLEMENTATION ACTOR (SNARE) — Field workers, street-level bureaucrats, and local administrators tasked with implementing policies designed under false equilibrium assumptions face a structural trap. They must execute policies that assume static conditions in dynamic environments. When policies fail due to changed conditions or violated assumptions, implementation actors bear reputational and career cost. No exit — they cannot refuse implementation, and they have no authority to revise the underlying policy frame. Maximum experienced extraction.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED STAKEHOLDER (SNARE) — Businesses, individuals, and communities targeted by policy suffer from policy failures cascading from false equilibrium assumptions. High exit costs: relocating, changing business models, or adapting to repeated policy revisions. Suppression is structural — they are regulated targets with limited legal recourse. The constraint extracts from them through instability, misdirected incentives, and adaptation costs.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICY REFORM COALITION (TANGLED ROPE) — Professional economists, think tanks, and advocacy groups push for more rigorous policy design. They coordinate on better methodology (dynamic stochastic general equilibrium, agent-based modeling, empirical behavioral evidence). But they are also captured by the same false equilibrium logic — they publish in journals that reward novelty over robustness, they compete for funding based on theoretical innovation, and they face suppression when their findings threaten incumbent interests. Mixed: genuine coordination on improving policy methodology alongside extraction through gatekeeping and epistemic authority.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY ESTABLISHMENT (ROPE) — Central banks, treasury departments, and regulatory agencies design and defend equilibrium-based frameworks. They experience the constraint as coordination: the equilibrium assumption simplifies communication, enables delegation of authority, and reduces political controversy ('we follow the science'). High exit optionality — they can shift methodologies without personal cost. Benefits from the constraint through legitimacy and centralized control.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INCUMBENT ECONOMIC INTEREST (ROPE) — Large firms, financial institutions, and rent-seeking actors benefit from policies designed under false equilibrium assumptions because these assumptions lock in existing relative positions. Incumbent firms can lobby for assumptions that preserve their margins; new entrants face false regulatory constraints. The incumbent sees the constraint as a coordination mechanism that stabilizes their position through policy legitimacy.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL POLICY ADOPTER (TANGLED ROPE) — Developing nations and policy-learning jurisdictions copy equilibrium-based frameworks from leading economies, gaining apparent methodological legitimacy. But the imported frameworks are calibrated to different structural conditions (different demographics, institutions, asset bases). Policy failure cascades from the mismatch. Exit is mobile at the global level (adopt different frameworks) but constrained domestically (IMF conditionality, capital flow dependence, domestic elite capture). Mixed coordination and extraction.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: EQUILIBRIUM THEORETICAL TRADITION (PITON) — The intellectual apparatus (Walrasian equilibrium, rational expectations hypothesis, perfect competition benchmarks) persists through institutional inertia despite substantive empirical failures. Textbooks present equilibrium models as pedagogical scaffolds but practitioners treat them as descriptive reality. The constraint is maintained by career incentives (modeling equilibrium is publishable; documenting disequilibrium is seen as data description not theory), not by function. Theater ratio is high: the ritual of equilibrium analysis persists because alternatives have not fully replaced it and because the ritual confers authority.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational frame, some degree of model simplification is inherent to policy design: all models are wrong, policymakers must choose between competing imperfect representations. The equilibrium assumption is presented as a necessary simplification, not a contingent choice. However, the structural data contradicts this naturalization — the constraint is maintained by career incentives, institutional authority, and incumbent interest protection, not by inherent analytical limits. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(policy_design_under_false_equilibrium_assumptions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(policy_design_under_false_equilibrium_assumptions, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(policy_design_under_false_equilibrium_assumptions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(policy_design_under_false_equilibrium_assumptions, TR),
    TR >= 0.70.

:- end_tests(policy_design_under_false_equilibrium_assumptions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts by imposing adaptation costs on non-incumbents while locking incumbent positions through stability assumptions. The extraction is not total (some policy learning occurs; incumbent advantage is not absolute) but structural and systematic. Theater ratio (0.68): High and rising. Equilibrium analysis in policy is substantially performative — it confers authority and enables delegation despite documented empirical failures. The ratio has increased over the interval as policy complexity has risen while equilibrium models have become less adequate descriptively. Suppression (0.62): Moderate-high. Structural barriers to exit include: (a) institutional inertia (central banks continue equilibrium frameworks because alternatives require new staffing and retraining), (b) political barriers (shifting from equilibrium to non-equilibrium models appears to concede that prior policymakers were wrong), (c) epistemic barriers (non-equilibrium policy design is genuinely harder methodologically), (d) incumbent lobbying against frameworks that would expose distributional consequences. Suppression is not total — dissent exists and alternative frameworks are developing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates large perspectival gaps across structural positions. The policy establishment sees primarily coordination (Rope) — equilibrium simplifies communication and legitimizes technocratic authority. Incumbent interests see coordination (Rope) — the assumption of stable competitive positioning. Frontline implementers see extraction (Snare) — they must execute policies that fail because assumptions are violated, and they bear the cost. Affected stakeholders see extraction (Snare) or mixed dynamics (Tangled Rope depending on sector). The reform coalition sees mixed dynamics (Tangled Rope) — they coordinate on better methodology but are suppressed by institutional incentives. International adopters see tangled dynamics — benefits from apparent methodological legitimacy alongside extraction from framework mismatch. The equilibrium tradition itself appears as piton (degraded institutional ritual). The civilizational analytical observer risks seeing natural law (Mountain — 'all models are simplifications') but structural data reveals this as false naturalization: the persistence is institutional choice, not logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values reflect structural position relative to extraction flow. Frontline implementers are fully trapped (d ≈ 0.95) — they have no authority to revise policy and bear full cost of failure. Affected stakeholders face high barriers to exit but some options exist (d ≈ 0.75). The reform coalition is organized but captured (d ≈ 0.40 — they benefit from methodological innovation but lose out to incumbent capture). Policy establishment and incumbents are full beneficiaries (d ≈ 0.10 — extraction flows toward them). International adopters face mobile exit globally but constrained domestically (d ≈ 0.55). The analytical observer experiences no material extraction but faces identity lock if their professional reputation is built on equilibrium methodology (d ≈ 0.70 if identity-locked, but this perspective uses analytical exit). The chi formula χ = ε × f(d) × σ(S) produces variation: frontline implementers experience highest chi (high d, high f(d)); beneficiaries experience lowest; organized reformers experience moderate chi despite moderate d because of global scope amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through demonstrating how different perspectives on the same extractive structure are legitimate: the policy establishment genuinely solves a coordination problem (how to communicate complex analysis to political actors); frontline implementers genuinely face extraction (bearing cost of policy mismatch). Both are true simultaneously. The mandatrophy resolution is that the *beneficiaries* see primarily coordination because they are positioned to benefit from it, while *victims* see primarily extraction because they bear the cost. The constraint is not 'is this coordination or extraction?' but 'how is the coordination benefit distributed?' Tangled Rope classification captures this: genuine coordination function (yes, equilibrium simplification does enable delegation) alongside asymmetric extraction (yes, the benefits concentrate on incumbents and policymakers while costs disperse to implementers and adapters). The false mountain perspective (naturalizing equilibrium as necessary simplification) is unmasked by examining whether the institutional choices (who benefits, who pays cost) are analytically necessary or contingent on institutional design. They are contingent — alternative policy methodologies exist, but career incentives and incumbent interests preserve the current framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    assumption_vs_description_boundary,
    'Are equilibrium assumptions presented as necessary simplifications or as accurate descriptions of actual economic dynamics?',
    'Textual analysis of policy documents and academic presentations: frequency of caveats, explicit statements of model limitations, and contradiction between stated assumptions and policy justification',
    'If assumptions are explicitly caveated: constraint is lower-extractiveness coordination (Rope with justified simplification). If assumptions are presented as fact: extractiveness is higher (Snare, because false beliefs suppress adaptive policymaking).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assumption_vs_description_boundary, empirical, 'Whether equilibrium assumptions are framed as simplifications or descriptions').

omega_variable(
    policy_failure_attribution_mechanism,
    'When equilibrium-based policies fail, are failures attributed to violated assumptions (opening learning) or to implementation/exogenous shocks (preserving assumption set)?',
    'Post-hoc analysis of policy failure case studies; tracking of attribution statements in policy review documents and media coverage',
    'If failures trigger assumption revision: constraint enables adaptive policymaking (lower extractiveness, Rope dominant). If failures are externalized: constraint traps policymakers in iterative failure (higher extractiveness, Snare/Tangled Rope dominant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_failure_attribution_mechanism, empirical, 'Attribution mechanism for policy failure').

omega_variable(
    methodological_alternative_viability,
    'Do epistemologically rigorous alternatives to equilibrium-based policy design exist and are they technically feasible?',
    'Survey of agent-based modeling, dynamic stochastic modeling, empirical behavioral frameworks; assessment of computational feasibility and predictive accuracy on historical policy problems',
    'If alternatives are viable: policy establishment''s adherence to equilibrium is choice, not necessity; extractiveness and suppression are higher. If alternatives are incomplete: some degree of equilibrium assumption may be justified structural simplification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_alternative_viability, empirical, 'Viability of alternatives to equilibrium-based policy design').

omega_variable(
    career_incentive_capture,
    'To what degree does the persistence of equilibrium-based policy design reflect genuine analytical necessity versus career/institutional incentives for mathematical elegance and theoretical closure?',
    'Analysis of publication patterns in economics and policy journals; comparison of citation rates for equilibrium-based versus non-equilibrium policy work; tracking of funding and promotion patterns',
    'If primarily necessary: constraint is legitimate theoretical choice (lower extraction framing). If primarily incentive-driven: constraint is institutional lock-in (higher extraction, piton characteristics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_incentive_capture, empirical, 'Role of career incentives in equilibrium framework persistence').

omega_variable(
    distributional_incidence_visibility,
    'Are the distributional consequences of equilibrium-based policy design (who bears adaptation costs, who benefits from stability assumptions) explicitly modeled and communicated?',
    'Analysis of policy impact assessments; presence/absence of distributional tables; comparison of beneficiary vs victim groups'' access to policy design processes',
    'If highly visible: stakeholders can organize counter-pressure (lower suppression, higher exit options). If obscured: suppression is higher and beneficiary/victim asymmetry persists unchallenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_incidence_visibility, empirical, 'Visibility of distributional consequences in policy design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(policy_design_under_false_equilibrium_assumptions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdfea_tr_t0, policy_design_under_false_equilibrium_assumptions, theater_ratio, 0, 0.5).
narrative_ontology:measurement(pdfea_tr_t5, policy_design_under_false_equilibrium_assumptions, theater_ratio, 5, 0.62).
narrative_ontology:measurement(pdfea_tr_t10, policy_design_under_false_equilibrium_assumptions, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pdfea_be_t0, policy_design_under_false_equilibrium_assumptions, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pdfea_be_t5, policy_design_under_false_equilibrium_assumptions, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(pdfea_be_t10, policy_design_under_false_equilibrium_assumptions, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(policy_design_under_false_equilibrium_assumptions, enforcement_mechanism).
narrative_ontology:affects_constraint(policy_design_under_false_equilibrium_assumptions, macroeconomic_stabilization_bias).
narrative_ontology:affects_constraint(policy_design_under_false_equilibrium_assumptions, regulatory_capture_equilibrium_defense).
narrative_ontology:affects_constraint(policy_design_under_false_equilibrium_assumptions, incumbent_advantage_lock_in).

% DUAL FORMULATION NOTE:
% Policy design under false equilibrium assumptions is upstream of specific policy failures (interest rate policy, labor regulation, financial stability frameworks). Each downstream constraint has its own extractiveness reflecting the particular domain; this story models the general structural mechanism by which equilibrium assumptions concentrate extraction. Decomposition: generic policy design constraint (this story, ε=0.58) vs domain-specific instantiations (macroeconomic stabilization ε=0.65, regulatory capture ε=0.72, incumbent lock-in ε=0.48).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(policy_design_under_false_equilibrium_assumptions, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
