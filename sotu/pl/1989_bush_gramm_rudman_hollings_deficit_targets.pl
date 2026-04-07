% ============================================================================
% CONSTRAINT STORY: 1989_bush_gramm_rudman_hollings_deficit_targets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1989_bush_gramm_rudman_hollings_deficit_targets, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: 1989_bush_gramm_rudman_hollings_deficit_targets
 *   human_readable: Gramm-Rudman-Hollings Statutory Deficit Reduction Targets
 *   domain: fiscal_policy/governance
 *
 * SUMMARY:
 *   The Gramm-Rudman-Hollings Act (1985) and its successor mechanisms (Budget
 *   Enforcement Act, PAYGO rules) establish statutory constraints on federal
 *   deficits through mandatory spending reduction triggers (sequestration) if
 *   deficit targets are exceeded. The mechanism legally binds Congress and
 *   the executive branch to deficit reduction, requiring that new tax
 *   revenues (projected at $80B+ over the initial decade) flow toward deficit
 *   reduction rather than program expansion. This creates a structural
 *   tension: the constraint genuinely coordinates fiscal discipline and
 *   prevents the political tragedy of the commons where unlimited lobbying
 *   for spending increases drives deficits; simultaneously, it extracts from
 *   programs and program beneficiaries by routing revenues away from
 *   discretionary spending. The constraint exhibits all six classification
 *   types depending on perspective: snare from the agency and constituent
 *   viewpoints (no exit, extraction forces triage); rope from the
 *   deficit-conscious taxpayer viewpoint (coordination mechanism protecting
 *   their interests); tangled rope from the organized congressional budget
 *   perspective (mixed enforcement and coordination benefits); piton from the
 *   long-term institutional viewpoint (sequestration triggers are frequently
 *   suspended, making the mechanism increasingly performative); mountain from
 *   the civilizational analytical perspective (naturalizing the fiscal
 *   mathematics constraint as an immutable law rather than a legislative
 *   choice). The theater ratio has increased over time as the gap between
 *   statutory mandate and actual implementation has widened through repeated
 *   exemptions and accounting workarounds.
 *
 * KEY AGENTS:
 *   - Deficit-Conscious Taxpayers: Primary beneficiary (institutional/arbitrage) — benefit from constraint preventing spending increases they oppose; can arbitrage between tax-cut and deficit-cut advocacy
 *   - Agency Administrators: Primary victim (powerless/trapped) — cannot expand programs despite new revenue; must choose triage within fixed budgets; career consequences for non-compliance
 *   - Service-Dependent Constituents: Secondary victim (powerless/trapped) — face program contractions and foregone expansions; no agency in budgeting process
 *   - Congressional Budget Committee: Organized institutional actor (organized/constrained) — gains enforcement authority but faces political pressure from spending advocates; coordinating body whose power is enhanced but constrained
 *   - Future Taxpayers/Fiscal Stability Advocates: Diffuse beneficiary (powerful/arbitrage) — benefit from reduced accumulation of debt; typically absent from legislative process
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing fiscal mathematics as justification for legislative choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1989_bush_gramm_rudman_hollings_deficit_targets, 0.52).
domain_priors:suppression_score(1989_bush_gramm_rudman_hollings_deficit_targets, 0.65).
domain_priors:theater_ratio(1989_bush_gramm_rudman_hollings_deficit_targets, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1989_bush_gramm_rudman_hollings_deficit_targets, extractiveness, 0.52).
narrative_ontology:constraint_metric(1989_bush_gramm_rudman_hollings_deficit_targets, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(1989_bush_gramm_rudman_hollings_deficit_targets, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1989_bush_gramm_rudman_hollings_deficit_targets, tangled_rope).
narrative_ontology:human_readable(1989_bush_gramm_rudman_hollings_deficit_targets, "Gramm-Rudman-Hollings Statutory Deficit Reduction Targets").
narrative_ontology:topic_domain(1989_bush_gramm_rudman_hollings_deficit_targets, "fiscal_policy/governance").

domain_priors:requires_active_enforcement(1989_bush_gramm_rudman_hollings_deficit_targets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1989_bush_gramm_rudman_hollings_deficit_targets, deficit_conscious_taxpayers).
narrative_ontology:constraint_beneficiary(1989_bush_gramm_rudman_hollings_deficit_targets, future_fiscal_stability_advocates).
narrative_ontology:constraint_victim(1989_bush_gramm_rudman_hollings_deficit_targets, discretionary_spending_programs).
narrative_ontology:constraint_victim(1989_bush_gramm_rudman_hollings_deficit_targets, agency_budget_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED AGENCY ADMINISTRATOR (SNARE) — Program heads and agency leaders face rigid spending caps enforced by sequestration. No exit from the constraint without Congressional exemption (politically costly). New revenues must flow to deficit reduction, not program expansion, regardless of program merit or social need. Maximum extraction from agency perspective: growth is impossible, only triage among existing commitments. Career consequences for failure to manage within constraints.
constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SERVICE-DEPENDENT CONSTITUENT (SNARE) — Beneficiaries of discretionary programs (education, infrastructure, research, social services) cannot exit the constraint. Programs cannot expand to meet demand; in periods of sequestration, programs contract. The constituent bears the cost of foregone services without agency in the budgetary process. Extraction is asymmetric: the constraint routes resources away from visible services to abstract deficit reduction.
constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFICIT-CONSCIOUS CONSTITUENCY (ROPE) — Taxpayers and fiscal conservatives benefit from the constraint as a coordination solution: it prevents the political tragedy of the commons where every interest group lobbies for spending increases. The constraint protects them from fiscal instability while enabling them to redirect new revenues away from programs they oppose. Low experienced extraction — beneficiaries have substantial arbitrage options (tax-cut advocacy, spending-cut leverage) and perceive the constraint as coordination mechanism protecting their interests.
constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL BUDGET COMMITTEE (TANGLED ROPE) — Organized institutional actors (budget committees, appropriations chairs) experience mixed coordination and extraction. The constraint provides coordination: it forces hard choices and prevents unlimited spending growth. But it also extracts: it concentrates budgeting power in the hands of deficit-focused committees and limits the discretionary flexibility that program advocates would otherwise retain. Committees benefit from the enforcement mechanism (enhanced oversight authority) while other committees suffer (mandatory cuts bypass normal appropriations process). Active enforcement required to maintain the coordination function.
constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SEQUESTRATION ENFORCEMENT RITUAL (PITON) — The statutory mechanism requires automated sequestration (across-the-board spending cuts) if deficit targets are missed. In practice, this trigger is frequently suspended via Congressional exemptions, special appropriations bills, or accounting manipulations. The ritual persists as a performative discipline mechanism: Congress repeatedly threatens sequestration while simultaneously exempting priority programs. Theater ratio rises as the gap between statutory mandate and actual implementation widens. The enforcement mechanism is maintained through institutional inertia despite low functional teeth.
constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FISCAL GRAVITY VIEW (MOUNTAIN) — From a long-term analytical perspective, Gramm-Rudman-Hollings reflects an immutable constraint of fiscal mathematics: total spending cannot persistently exceed total revenue without accumulating debt that eventually becomes unsustainable. The statutory mechanism formalizes what is, at the civilizational horizon, a natural law of political economy. However, the constraint is not naturally emerging — it is legislated. The engine's false summit detector will flag this as naturalization of a contingent institutional choice.
constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1989_bush_gramm_rudman_hollings_deficit_targets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1989_bush_gramm_rudman_hollings_deficit_targets, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1989_bush_gramm_rudman_hollings_deficit_targets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1989_bush_gramm_rudman_hollings_deficit_targets, TR),
    TR >= 0.70.

:- end_tests(1989_bush_gramm_rudman_hollings_deficit_targets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint routes new revenues ($80B+ initially projected) to deficit reduction rather than program expansion. This is a significant extraction from the perspective of programs seeking growth, but not as severe as a pure snare: programs can still operate within existing budgets, and the extraction is applied transparently through statutory mechanism rather than through coercion or deception. The value reflects that the constraint has genuine extractive force (it prevents spending that would otherwise occur) while acknowledging that the coordination function is real (it prevents fiscal instability). The trajectory from 0.35 to 0.52 reflects increasing pressure as deficit-reduction targets become harder to meet without deeper program cuts. Suppression (0.65): Moderate-high. Agencies and programs face rigid spending caps with limited exit options. Sequestration threatens across-the-board cuts. Political appeals for exemptions are possible but costly (require legislative action). The suppression is not total — agencies can lobby, Congress can exempt priority programs, and accounting workarounds exist — but the structural barriers to normal growth are substantial. Theater ratio (0.58): Moderate-high and rising. Initial implementation (1986-1990) involved genuine enforcement through sequestration triggers. However, as targets became politically difficult, Congress repeatedly suspended sequestration (1990 suspension, 2011 caps relief). The constraint persists through procedural inertia and the symbolic discipline it represents, but the gap between statutory mandate and actual implementation has widened. The rising theater ratio (0.42 → 0.65 by year 9) reflects increasing reliance on the performative aspect of the constraint — the threat of sequestration — rather than actual implementation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. From below (agency/constituent), the constraint is experienced as pure extraction (snare) — growth is impossible, resources flow toward an abstract goal (deficit reduction) rather than visible services. From the beneficiary side (deficit-conscious taxpayers), the constraint is experienced as coordination (rope) — it prevents the spending growth they oppose. From an organized institutional perspective (budget committees), the constraint is mixed (tangled rope) — enforcement authority is gained at the cost of political conflict. From a long-term institutional perspective, the constraint is degraded (piton) — it has increasingly become a performative ritual of target-setting and suspension rather than actual enforcement. The analytical observer risks seeing this as an immutable fiscal law (mountain) rather than recognizing it as a contingent institutional arrangement that could be modified or removed. The perspectival gap reveals that 'the' deficit constraint does not exist independent of the observer's structural position and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to the extraction flow. Deficit-conscious taxpayers have low d (they are beneficiaries with arbitrage options) — the constraint protects their interests and they can exit by voting for deficit-cut politicians. Agencies and constituents have high d (they are victims with trapped exit options) — they bear the cost of foregone spending without meaningful agency in the budgeting process. The Congressional Budget Committee has moderate d (organized/constrained) — they gain enforcement authority (benefit) but face political pressure (cost). The piton classification derives from the theater gate: the ratio of performative discipline (threats of sequestration, the ritual of target-setting) to actual implementation (actual automatic cuts) has risen over time as the mechanism has faced increasing pressure from spending advocates. The mountain classification from the analytical perspective is perspectival — it naturalizes fiscal mathematics as an immutable constraint, but the structural data reveals this as a false summit: the statutory mechanism is a legislative choice, not a law of nature.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves the mandatrophy by demonstrating that extraction and coordination are not mutually exclusive properties but perspectival artifacts. From the beneficiary (deficit-conscious constituency), the constraint is genuinely coordinating — it solves the problem of unlimited spending growth that would otherwise occur through political competition. From the victim (agencies and constituents), the constraint is genuinely extracting — it redirects resources away from visible services toward an abstract fiscal target. The 'correct' classification depends on which agent's perspective is the reference frame. The rising theater ratio indicates degradation of the coordination function over time as the mechanism faces political pressure and increasingly relies on suspension rather than enforcement. If theater continues to rise above 0.70, the constraint transitions from tangled_rope toward piton (inertial theater without functional teeth). The constraint's stability depends on the continued political belief that deficit reduction is worth the service-expansion costs — if that belief erodes, the mechanism collapses into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sequestration_effectiveness,
    'Does the threat of automatic sequestration actually constrain spending behavior, or do ad-hoc exemptions and suspensions render it performative?',
    'Historical analysis of sequestration triggers vs actual implementation rates; comparison of budgeting patterns in years with active vs suspended mechanisms; measurement of exemption frequency and scope',
    'If effective: constraint functions as true snare/rope hybrid. If performative: constraint is piton (inertial theater). Classification shifts based on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sequestration_effectiveness, empirical, 'Whether sequestration threat produces genuine spending constraint vs performative ritual').

omega_variable(
    deficit_reduction_vs_revenue_shifting,
    'Does the constraint genuinely reduce deficits, or does it primarily shift where tax revenues are allocated without changing the underlying fiscal imbalance?',
    'Econometric analysis of deficit trends with/without GRH; decomposition of deficit reduction into actual spending cuts vs revenue reallocation vs growth effects; counterfactual modeling of spending without constraint',
    'If genuine reduction: constraint functions as coordination mechanism creating fiscal discipline. If revenue shifting: extraction is transferred rather than reduced, and the constraint masks rather than solves the underlying fiscal problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_reduction_vs_revenue_shifting, empirical, 'Whether constraint reduces deficits or merely redirects revenue allocation').

omega_variable(
    political_feasibility_ceiling,
    'Is the constraint''s stringency level politically sustainable, or does the accumulated pressure from frustrated spending advocates eventually force suspension or gutting of the mechanism?',
    'Historical tracking of suspension votes and exemption legislation; analysis of political coalition strength supporting continuation vs repeal; generational cohort analysis of deficit tolerance',
    'If sustainable: constraint can function as stable coordination mechanism. If unsustainable: constraint degrades toward piton (inertial theater without functional bite).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_ceiling, conceptual, 'Political sustainability of statutory deficit targets').

omega_variable(
    accounting_gimmickry_proliferation,
    'To what extent do agencies and committees circumvent the constraint through off-budget spending, asset sales, timing manipulations, and other accounting mechanisms rather than actual budget reduction?',
    'Forensic budget analysis identifying off-budget programs, asset sales, timing shifts; comparison of statutory deficit targets vs actual fiscal impact; tracking of accounting-rule changes over time',
    'If proliferation is high: the constraint becomes a target for workaround behavior, and measured extractiveness is overstated — actual fiscal discipline is lower than the statutory targets suggest. Theater ratio rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accounting_gimmickry_proliferation, empirical, 'Prevalence of accounting mechanisms circumventing deficit targets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1989_bush_gramm_rudman_hollings_deficit_targets, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grh_tr_t0, 1989_bush_gramm_rudman_hollings_deficit_targets, theater_ratio, 0, 0.42).
narrative_ontology:measurement(grh_tr_t3, 1989_bush_gramm_rudman_hollings_deficit_targets, theater_ratio, 3, 0.52).
narrative_ontology:measurement(grh_tr_t6, 1989_bush_gramm_rudman_hollings_deficit_targets, theater_ratio, 6, 0.58).
narrative_ontology:measurement(grh_tr_t9, 1989_bush_gramm_rudman_hollings_deficit_targets, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(grh_be_t0, 1989_bush_gramm_rudman_hollings_deficit_targets, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(grh_be_t3, 1989_bush_gramm_rudman_hollings_deficit_targets, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(grh_be_t6, 1989_bush_gramm_rudman_hollings_deficit_targets, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(grh_be_t9, 1989_bush_gramm_rudman_hollings_deficit_targets, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1989_bush_gramm_rudman_hollings_deficit_targets, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(1989_bush_gramm_rudman_hollings_deficit_targets, 0.12).
narrative_ontology:affects_constraint(1989_bush_gramm_rudman_hollings_deficit_targets, federal_budget_political_economy).
narrative_ontology:affects_constraint(1989_bush_gramm_rudman_hollings_deficit_targets, mandatory_spending_entitlement_lock_in).

% DUAL FORMULATION NOTE:
% Gramm-Rudman-Hollings is upstream of specific appropriations constraints and downstream of broader fiscal policy debates. The deficit-reduction targets themselves are the primary structural object; specific program impacts are downstream effects. The mechanism's extractiveness derives from the revenue routing rule (new revenues to deficit reduction), not from particular program cuts. Different programs experience different extraction levels based on their flexibility and political priority, which are captured in separate constraint stories linked to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1989_bush_gramm_rudman_hollings_deficit_targets, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
