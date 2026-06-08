% ============================================================================
% CONSTRAINT STORY: tariff_revenue_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tariff_revenue_volatility, []).

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
 *   constraint_id: tariff_revenue_volatility
 *   human_readable: Tariff Revenue Volatility and Fiscal Uncertainty
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The tariff revenue volatility constraint emerges from the intersection of
 *   executive trade authority, judicial review of statutory delegation, and
 *   congressional appropriations power. The structural delta — $166B+ in
 *   potential refunds from CIT ruling, uncertain appeal outcome, and July
 *   tariff expiration — creates acute fiscal uncertainty that extracts from
 *   revenue predictability while benefiting executive discretion advocates
 *   and import-dependent industries. The constraint is a snare from the
 *   perspective of revenue predictability (powerless/trapped), congressional
 *   appropriations authority (moderate/constrained), and the analytical
 *   observer (analytical/analytical). It appears as rope from the perspective
 *   of beneficiaries (institutional/arbitrage) who gain from either refunds
 *   or executive flexibility. Fiscal responsibility coalitions experience it
 *   as tangled rope: they coordinate public discourse around budget
 *   predictability but are victims of policy outcomes that ignore their
 *   advocacy. The theater ratio (0.58) reflects that CBP refund processing
 *   and CIT appeal procedures are partly performative — they adjudicate
 *   narrow statutory questions without addressing the underlying
 *   constitutional tension between executive trade authority and
 *   congressional appropriations control.
 *
 * KEY AGENTS:
 *   - Revenue Predictability: Primary victim (powerless/trapped) — abstract collective good with no advocate and no exit; bears full cost of volatility
 *   - Congressional Appropriations Authority: Secondary victim (moderate/constrained) — constitutionally empowered but structurally bypassed by executive tariff discretion; high barriers to reasserting control
 *   - State Budget Offices: Secondary victim (moderate/constrained) — coordinate fiscal planning around federal revenue but bear asymmetric cost when volatility cascades to transfers and grants
 *   - Import-Dependent Industries: Primary beneficiary (institutional/arbitrage) — gain from refund opportunities and litigation leverage; $166B+ potential subsidy
 *   - Executive Discretion Advocates: Primary beneficiary (institutional/arbitrage) — benefit from tariff authority concentration; volatility is a feature enabling rapid policy adjustment
 *   - Trade Policy Consultants: Secondary beneficiary (institutional/arbitrage) — extract rents from advising on tariff litigation, exemption applications, and supply chain restructuring
 *   - Fiscal Responsibility Coalitions: Mixed position (organized/constrained) — coordinate advocacy but are victims of policy outcomes; tangled rope experience
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structural extraction from fiscal predictability and appropriations authority without corresponding accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tariff_revenue_volatility, 0.68).
domain_priors:suppression_score(tariff_revenue_volatility, 0.72).
domain_priors:theater_ratio(tariff_revenue_volatility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tariff_revenue_volatility, extractiveness, 0.68).
narrative_ontology:constraint_metric(tariff_revenue_volatility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tariff_revenue_volatility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tariff_revenue_volatility, snare).
narrative_ontology:human_readable(tariff_revenue_volatility, "Tariff Revenue Volatility and Fiscal Uncertainty").
narrative_ontology:topic_domain(tariff_revenue_volatility, "public_finance/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(tariff_revenue_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tariff_revenue_volatility, executive_discretion_advocates).
narrative_ontology:constraint_beneficiary(tariff_revenue_volatility, import_dependent_industries).
narrative_ontology:constraint_beneficiary(tariff_revenue_volatility, trade_policy_consultants).
narrative_ontology:constraint_victim(tariff_revenue_volatility, revenue_predictability).
narrative_ontology:constraint_victim(tariff_revenue_volatility, fiscal_planning_capacity).
narrative_ontology:constraint_victim(tariff_revenue_volatility, congressional_appropriations_authority).
narrative_ontology:constraint_victim(tariff_revenue_volatility, state_budget_offices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REVENUE PREDICTABILITY (SNARE) — The abstract collective good of predictable federal revenue cannot exit the constraint, cannot organize advocacy, and bears the full cost of volatility. Budget offices must plan around $166B+ in potential refunds with no control over CIT appeal timing or tariff renewal decisions. Maximum extraction: the fiscal commons has no voice and no alternative.
constraint_indexing:constraint_classification(tariff_revenue_volatility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL APPROPRIATIONS (SNARE) — Constitutionally empowered but structurally constrained by executive tariff discretion under IEEPA and Section 232. Cannot exit the volatility created by unilateral executive trade policy. High barriers to reasserting appropriations control: requires supermajority override of executive action or legislative repeal of delegation statutes against executive resistance. Experiences the constraint as extraction — constitutional authority is nominally present but functionally bypassed.
constraint_indexing:constraint_classification(tariff_revenue_volatility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE BUDGET OFFICES (TANGLED ROPE) — Coordinate fiscal planning around federal revenue projections but bear asymmetric cost when tariff volatility cascades to federal transfers and grant programs. Benefit from the federal revenue system (grants, Medicaid matching, infrastructure funding) but are victims of its unpredictability. Cannot exit federal fiscal dependence; constrained by constitutional structure. Mixed coordination and extraction.
constraint_indexing:constraint_classification(tariff_revenue_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IMPORT-DEPENDENT INDUSTRIES (ROPE) — Benefit from tariff uncertainty through refund opportunities and litigation leverage. $166B+ in potential refunds represents direct subsidy to importers who challenged tariff authority. Arbitrage exit: can shift supply chains, lobby for exemptions, or litigate for refunds. Experience the constraint as coordination: the legal and administrative apparatus for challenging tariffs is a service they consume, not a cost they bear.
constraint_indexing:constraint_classification(tariff_revenue_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE DISCRETION ADVOCATES (ROPE) — Benefit from tariff authority concentration in executive branch. Revenue volatility is a feature, not a bug: it reflects executive flexibility to respond to trade conditions without legislative constraint. Arbitrage exit: can shift between statutory authorities (IEEPA, Section 232, Section 301) as judicial or legislative challenges arise. Experience the constraint as coordination: the legal framework enables rapid trade policy adjustment.
constraint_indexing:constraint_classification(tariff_revenue_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FISCAL RESPONSIBILITY COALITIONS (TANGLED ROPE) — Organized advocacy groups (Committee for a Responsible Federal Budget, Concord Coalition, etc.) coordinate around deficit reduction and budget predictability but are victims of tariff revenue volatility that undermines fiscal planning. Benefit from the public discourse infrastructure (media access, congressional testimony, think tank platforms) but cannot force executive or legislative action to stabilize revenue. Constrained exit: can advocate but not compel. Mixed coordination (public deliberation) and extraction (policy outcomes ignore advocacy).
constraint_indexing:constraint_classification(tariff_revenue_volatility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, tariff revenue volatility is a structural consequence of executive trade authority concentration without corresponding fiscal accountability mechanisms. The constraint extracts from revenue predictability and congressional appropriations authority while benefiting executive discretion and import-dependent industries. The volatility is not inherent to trade policy (other democracies legislate tariffs) but is a contingent feature of U.S. statutory delegation and judicial deference to executive trade authority. High extraction, high suppression, substantial theater (CBP processing rituals, CIT appeal procedures that do not address underlying authority questions).
constraint_indexing:constraint_classification(tariff_revenue_volatility, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tariff_revenue_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tariff_revenue_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tariff_revenue_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tariff_revenue_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tariff_revenue_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from revenue predictability (fiscal planning capacity degraded by $166B+ uncertainty), congressional appropriations authority (constitutional power functionally bypassed), and state budget offices (federal transfer volatility cascades to state fiscal planning). Beneficiaries capture substantial value: import-dependent industries gain refund opportunities, executive discretion advocates preserve policy flexibility, trade consultants extract rents from navigating uncertainty. The extraction is asymmetric and substantial. Suppression (0.72): High. Revenue predictability cannot exit (abstract collective good with no agency). Congressional appropriations authority faces high barriers to exit: requires supermajority override or legislative repeal against executive resistance. State budget offices cannot exit federal fiscal dependence (constitutional structure). Alternatives are suppressed: legislative tariff authority would require overcoming executive veto and interest group opposition; automatic revenue stabilizers would require constitutional amendment or major statutory reform. Theater ratio (0.58): Moderate-high. CBP refund processing follows administrative procedures that appear rigorous but do not address whether executive tariff authority is constitutionally sound. CIT appeal procedures adjudicate narrow statutory interpretation questions (IEEPA scope) without resolving the underlying nondelegation or appropriations clause tensions. The legal ritual is partly functional (determines refund eligibility) and partly performative (avoids constitutional questions). Theater has increased over the interval as the gap between formal procedure and substantive accountability has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a sharp perspectival gap between victims and beneficiaries. Revenue predictability and congressional appropriations authority experience pure extraction (snare) — they bear costs with no corresponding benefit and cannot exit. Import-dependent industries and executive discretion advocates experience pure coordination (rope) — they benefit from the legal and administrative infrastructure that enables tariff challenges and executive flexibility. State budget offices and fiscal responsibility coalitions experience mixed coordination and extraction (tangled rope) — they benefit from federal fiscal infrastructure but are victims of its unpredictability. The analytical observer identifies the constraint as a snare because the structural data shows high extraction, high suppression, and asymmetric benefit distribution. The beneficiaries' rope perspective is their genuine experience, but it does not negate the extraction borne by victims. The perspectival gap is the measurement: victims see a trap, beneficiaries see a service, and the analytical observer sees extraction masked as executive flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Revenue predictability is a powerless victim with trapped exit — maximum directionality toward full target (d ≈ 0.95), producing maximum effective extraction. Congressional appropriations authority is a moderate victim with constrained exit — high directionality (d ≈ 0.75) but not maximum because some legislative tools remain available (budget reconciliation, appropriations riders). State budget offices are moderate victims with constrained exit — high directionality (d ≈ 0.70) modulated slightly by their ability to adjust state fiscal policy at the margin. Import-dependent industries are institutional beneficiaries with arbitrage exit — low directionality toward beneficiary end (d ≈ 0.15), producing negative effective extraction (subsidy). Executive discretion advocates are institutional beneficiaries with arbitrage exit — low directionality (d ≈ 0.10), experiencing the constraint as pure coordination benefit. Fiscal responsibility coalitions are organized agents with constrained exit — moderate directionality (d ≈ 0.50), experiencing mixed extraction and coordination. The analytical observer computes effective extraction from the structural data and identifies the constraint as a snare: high base extraction, high suppression, identifiable victims, and beneficiaries who gain from the volatility they help sustain.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the snare classification is structurally determined by the victim's perspective and the analytical observer's measurement, not by the beneficiary's experience. Import-dependent industries genuinely experience the constraint as coordination (rope) because they benefit from refund opportunities and litigation leverage. Executive discretion advocates genuinely experience it as coordination because tariff authority concentration enables rapid policy adjustment. But these rope perspectives do not make the constraint a rope — they reveal that the beneficiaries are net extractors from the fiscal commons. The snare classification captures the structural reality: revenue predictability is trapped, congressional appropriations authority is bypassed, and the volatility persists because beneficiaries gain from it. The mandatrophy is not 'which type is correct?' but 'whose perspective determines classification?' The answer: the victim's perspective and the analytical observer's structural measurement determine the constraint's type, while the beneficiary's perspective explains why the constraint persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cit_appeal_outcome_uncertainty,
    'Will the CIT ruling against IEEPA tariff authority be upheld on appeal, and if so, what is the scope of refund liability?',
    'Federal Circuit or Supreme Court ruling on IEEPA statutory interpretation; CBP administrative determination of refund eligibility and processing timeline',
    'If upheld with broad refund scope: $166B+ fiscal shock, potential executive authority curtailment, legislative pressure to restore tariff revenue. If overturned: executive discretion preserved, revenue volatility continues under different statutory authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cit_appeal_outcome_uncertainty, empirical, 'CIT appeal outcome and refund liability scope').

omega_variable(
    july_tariff_renewal_mechanism,
    'Will expiring July tariffs be renewed via executive action (new statutory authority), legislative action (congressional tariff bill), or allowed to lapse?',
    'Executive orders invoking alternative statutory authorities (Section 232, Section 301); congressional legislation on tariff renewal or trade authority reform; observable through Federal Register notices and legislative tracking',
    'If renewed via executive action: volatility persists under different legal framework. If renewed via legislation: congressional appropriations authority partially restored, volatility reduced. If lapsed: revenue loss, potential trade retaliation, pressure for alternative revenue sources.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(july_tariff_renewal_mechanism, preference, 'Mechanism for July tariff renewal or replacement').

omega_variable(
    revenue_substitution_feasibility,
    'Can tariff revenue volatility be offset by alternative revenue sources (tax increases, spending cuts, deficit financing) without triggering fiscal crisis or political backlash?',
    'Congressional Budget Office revenue projections; bond market response to deficit increases; legislative action on tax or spending bills; observable through Treasury borrowing costs and appropriations outcomes',
    'If substitution is feasible: volatility is absorbed without structural fiscal crisis, but extraction from revenue predictability continues. If infeasible: fiscal crisis forces either tariff stabilization (legislative action) or spending cuts (extraction shifts to program beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_substitution_feasibility, empirical, 'Feasibility of offsetting tariff revenue volatility').

omega_variable(
    constitutional_nondelegation_revival,
    'Will the Supreme Court revive nondelegation doctrine to constrain executive tariff authority, or will judicial deference to executive trade policy continue?',
    'Supreme Court rulings on major questions doctrine, nondelegation, or statutory interpretation of trade authority statutes; observable through cert grants and opinions in trade cases',
    'If nondelegation revived: executive tariff authority curtailed, congressional appropriations control restored, revenue volatility reduced but legislative gridlock risk increases. If deference continues: executive discretion preserved, volatility persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_nondelegation_revival, conceptual, 'Judicial doctrine on executive trade authority limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tariff_revenue_volatility, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_vol_theater_t0, tariff_revenue_volatility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tariff_vol_theater_t2, tariff_revenue_volatility, theater_ratio, 2, 0.48).
narrative_ontology:measurement(tariff_vol_theater_t4, tariff_revenue_volatility, theater_ratio, 4, 0.53).
narrative_ontology:measurement(tariff_vol_theater_t6, tariff_revenue_volatility, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(tariff_vol_extract_t0, tariff_revenue_volatility, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tariff_vol_extract_t2, tariff_revenue_volatility, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(tariff_vol_extract_t4, tariff_revenue_volatility, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(tariff_vol_extract_t6, tariff_revenue_volatility, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tariff_vol_suppress_t0, tariff_revenue_volatility, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(tariff_vol_suppress_t2, tariff_revenue_volatility, suppression_requirement, 2, 0.63).
narrative_ontology:measurement(tariff_vol_suppress_t4, tariff_revenue_volatility, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(tariff_vol_suppress_t6, tariff_revenue_volatility, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tariff_revenue_volatility, enforcement_mechanism).
narrative_ontology:affects_constraint(tariff_revenue_volatility, executive_trade_authority_delegation).
narrative_ontology:affects_constraint(tariff_revenue_volatility, congressional_appropriations_control).
narrative_ontology:affects_constraint(tariff_revenue_volatility, federal_state_fiscal_transfers).

% DUAL FORMULATION NOTE:
% Tariff revenue volatility is downstream of executive trade authority delegation (the statutory framework that enables unilateral tariff imposition) and affects congressional appropriations control (the constitutional authority undermined by revenue unpredictability) and federal-state fiscal transfers (the cascade mechanism by which federal volatility propagates to state budgets). Each of these is a distinct constraint with its own extractiveness value. The volatility constraint has high extractiveness (0.68) reflecting the fiscal uncertainty itself; the upstream delegation constraint would have its own extractiveness reflecting the constitutional tension; the downstream transfer constraint would reflect the state-level fiscal impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tariff_revenue_volatility, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
