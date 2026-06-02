% ============================================================================
% CONSTRAINT STORY: wage_growth_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wage_growth_suppression, []).

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
 *   constraint_id: wage_growth_suppression
 *   human_readable: Wage Growth Suppression in Labor Markets
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Wage growth suppression operates as a multi-layered institutional
 *   constraint that extracts labor value through coordination of employer
 *   behavior and fragmentation of worker bargaining power. The constraint
 *   exhibits characteristics across all six classification types depending on
 *   the observer's structural position relative to the suppression mechanism.
 *   A wage worker experiences absolute suppression (snare) with trapped exit
 *   options. An organized labor coalition experiences mixed coordination and
 *   extraction (tangled rope) — unions coordinate collective wage defense but
 *   operate within increasingly hostile legal frameworks. A capital-holding
 *   corporation experiences wage suppression as pure coordination (rope) — it
 *   solves the collective action problem of wage bidding competition. A
 *   worker with retraining capacity experiences temporary suppression with
 *   exit pathways (scaffold). The regulatory framework itself is largely
 *   performative (piton). And from the analytical perspective, suppression
 *   can be naturalized as an inevitable supply-demand law (mountain), though
 *   this naturalization masks the contingent institutional mechanisms that
 *   actively maintain suppression. The constraint's theater ratio (0.55)
 *   reflects that much wage suppression operates through non-visible
 *   mechanisms: outsourcing threats, automation pressure, gig economy
 *   fragmentation, immigration-supply rhetoric, and anti-union enforcement —
 *   rather than through transparent wage-cutting announcements.
 *
 * KEY AGENTS:
 *   - Wage Workers: Primary victims (powerless/trapped) — depend on labor sale for survival; face suppression through skill competition, outsourcing threats, and employment-at-will vulnerability
 *   - Labor Unions and Worker Associations: Secondary organized actors (organized/constrained) — coordinate collective bargaining but operate within legal frameworks that restrict organizing capacity
 *   - Capital-Holding Corporations and Shareholders: Primary beneficiaries (institutional/arbitrage) — capture wage suppression value as profit; can reallocate capital, automate, or offshore to enforce suppression
 *   - Executive Management: Secondary beneficiary (powerful/arbitrage) — compensation packages increase as wage suppression lowers labor costs relative to profits
 *   - Retraining and Education Programs: Moderate actors (moderate/mobile) — provide temporary escape routes from lowest-wage suppression but have limited effectiveness and scale
 *   - Labor Standards Agencies and Minimum Wage Framework: Institutional actors (institutional/arbitrage) — maintain performative enforcement while real suppression mechanisms operate outside regulatory scope
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional suppression as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wage_growth_suppression, 0.58).
domain_priors:suppression_score(wage_growth_suppression, 0.68).
domain_priors:theater_ratio(wage_growth_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wage_growth_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(wage_growth_suppression, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wage_growth_suppression, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wage_growth_suppression, snare).
narrative_ontology:human_readable(wage_growth_suppression, "Wage Growth Suppression in Labor Markets").
narrative_ontology:topic_domain(wage_growth_suppression, "economic/labor").

domain_priors:requires_active_enforcement(wage_growth_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wage_growth_suppression, capital_holders).
narrative_ontology:constraint_beneficiary(wage_growth_suppression, executive_management).
narrative_ontology:constraint_victim(wage_growth_suppression, wage_workers).
narrative_ontology:constraint_victim(wage_growth_suppression, labor_bargaining_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE WORKER (SNARE) — Trapped by economic necessity and geographic immobility. Limited alternative income sources; exit from labor market means destitution. Faces suppression through strikebreaking, employment-at-will doctrine, skill competition, immigration pressure, and outsourcing threats. No meaningful agency over own wage trajectory. Maximum experienced extraction.
constraint_indexing:constraint_classification(wage_growth_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR UNION COALITION (TANGLED ROPE) — Organized actors (unions, worker associations) experience mixed coordination and extraction. The constraint itself required active enforcement to build (union suppression, strikebreaking, right-to-work legislation). Unions coordinate collective wage bargaining (genuine coordination function) but operate within an increasingly hostile institutional framework that extracts value from collective action capacity. Constrained exit due to legal barriers and employer retaliation capacity.
constraint_indexing:constraint_classification(wage_growth_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-HOLDING CORPORATION (ROPE) — Experiences wage suppression as a coordination mechanism that solves the collective action problem of competitive wage-bidding. Without suppression, firms would bid wages up during tight labor markets. The constraint coordinates firm behavior (prevents wage competition) and returns profits to shareholders. Net beneficiary with arbitrage options (capital reallocation, offshoring, automation).
constraint_indexing:constraint_classification(wage_growth_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISPLACED WORKER WITH RETRAINING (SCAFFOLD) — Moderate agent with some mobility through educational and retraining programs. Early-career workers can potentially exit suppression through skill development and sector switching. Theater moderate (retraining programs are partially performative). Has sunset framing — investment in human capital is seen as a pathway out, though retraining outcomes are mixed and the pathway is temporally uncertain.
constraint_indexing:constraint_classification(wage_growth_suppression, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MINIMUM WAGE REGULATORY FRAMEWORK (PITON) — Minimum wage adjustments, wage index formulas, and labor standards enforcement are substantially performative. The real suppression mechanisms (outsourcing capacity, automation threats, gig economy fragmentation, immigration pressure) operate outside the regulatory framework. Minimum wage enforcement persists through institutional inertia, but its actual wage-floor function has degraded as the economy shifted toward sectors and arrangements that circumvent it. Theater ratio high; actual coordination capacity low.
constraint_indexing:constraint_classification(wage_growth_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal economic perspective, wage suppression can appear as an immutable law of supply and demand: when labor supply exceeds demand, wages fall. This perspective naturalizes suppression as inevitable market function rather than as a structured institutional arrangement. However, the structural data contradicts the mountain gate — active enforcement, organized beneficiary coalitions, and suppression mechanisms are contingent policy choices, not natural laws. False summit detection applies.
constraint_indexing:constraint_classification(wage_growth_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wage_growth_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wage_growth_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wage_growth_suppression, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wage_growth_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wage_growth_suppression, TR),
    TR >= 0.70.

:- end_tests(wage_growth_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Wage suppression extracts measurable value from workers through prevention of wage growth that would otherwise track productivity. Real wages have grown substantially below labor productivity growth over the 40-year interval, with the gap accumulating as profit to capital holders. The measurement trajectory shows extractiveness increasing from 0.35 to 0.58 as suppression mechanisms intensified (union density decline, outsourcing capacity expansion, gig economy growth). Suppression (0.68): High. Multiple overlapping mechanisms prevent wage growth: (1) employment-at-will legal frameworks allowing wage suppression threats, (2) union suppression through right-to-work laws and strikebreaking capacity, (3) outsourcing and offshoring threats that discipline wage demands, (4) automation capacity that substitutes labor, (5) immigration-supply rhetoric that depresses worker confidence in scarcity value, (6) gig economy fragmentation that prevents collective bargaining. Theater ratio (0.55): Moderate. Some suppression operates through visible mechanisms (wage announcements, published compensation surveys) but substantial suppression is indirect: outsourcing capacity is implied but rarely executed for every individual; automation threatens but doesn't immediately eliminate jobs; immigration supply pressure is abstract. The theater has increased over the interval as explicit wage suppression became less socially acceptable and shifted to indirect mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates strong perspectival divergence. Capital holders see wage suppression as coordination (Rope) — it solves the competitive wage-bidding problem that would emerge from labor scarcity. They experience it as a mechanism that keeps wages rational and prevents profit erosion. Wage workers see pure extraction (Snare) — wages do not grow with their productivity, and they cannot exit without destitution. Organized workers see mixed extraction and coordination (Tangled Rope) — unions coordinate collective wage defense but face intensifying legal suppression, creating an extractive overlay on the coordination function. The regulatory framework sees itself as managing wages fairly (Piton) — minimum wage and labor standards persist through institutional momentum, but these regulations have become largely performative as real suppression operates through mechanisms outside their scope. The analytical observer risks seeing natural law (Mountain) — supply and demand for labor inevitably determine wages — but this naturalizes the contingent institutional choices (outsourcing permission, union suppression, immigration policy) that shape the supply-demand balance itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The wage suppression constraint flows from capital holders toward wage workers. Beneficiaries are capital holders (investors, executives) who receive increased profits when wages are suppressed relative to productivity. Victims are wage workers whose labor value is captured without corresponding wage growth. The directionality is highly asymmetric: beneficiaries have arbitrage options (capital reallocation, automation, offshoring) that give them exit capacity to enforce suppression; victims have trapped exit options (economic dependency makes non-participation or exit impossible). This asymmetry produces high d (directionality toward victims) and high f(d), amplifying the experienced extractiveness. Organized actors (unions) have constrained rather than trapped exit — they retain capacity to organize but face legal and enforcement barriers that increase the cost of exercising this capacity. Moderate agents (retraining workers) have mobile exit — they can potentially leave suppressed sectors through skill development, reducing their d value.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in wage suppression resolves by recognizing that the constraint is genuinely extractive (snare from the worker perspective) rather than coordinative (rope from the beneficiary perspective). The key diagnostic is the presence of asymmetric enforcement: beneficiaries have arbitrage options that allow them to enforce suppression (outsourcing, automation, capital flight), while victims have trapped exit options (economic necessity forces continued labor market participation). A purely coordinative rope would have symmetric enforcement — both sides could exit or enforce equally. The tangled rope classification for organized workers reflects that suppression also coordinates worker behavior (unions suppress internal wage competition, enforce seniority rules) — there is a genuine coordination component — but this is embedded within asymmetric extraction enabled by capital mobility. The piton classification for the regulatory framework reveals that institutional responses to suppression (minimum wage increases, labor standards) are substantially performative theater rather than effective constraint-breakers. This reflects the degradation of regulatory capacity over the 40-year interval as capital mobility increased and union density declined. The analytical observer's mountain classification is a false summit — it naturalizes suppression as an inevitable economic law rather than recognizing it as the product of specific institutional choices about labor law, trade policy, immigration policy, and capital mobility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_vs_policy_causation,
    'What portion of wage suppression is driven by automation and technical change versus deliberate institutional suppression policies?',
    'Sectoral analysis comparing wage growth in heavily automated sectors to non-automated sectors; international comparison of wage trajectories in countries with stronger labor institutional frameworks; productivity-to-wage ratio decomposition',
    'If primarily technical: classification shifts toward mountain (supply/demand natural law) and the constraint is less tractable. If primarily institutional: classification confirms snare and policy interventions are causally relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_vs_policy_causation, empirical, 'Attribution of wage suppression to automation versus policy').

omega_variable(
    labor_supply_elasticity_at_saturation,
    'At what level of labor supply/demand imbalance does suppression become the dominant mechanism versus voluntary labor market exit and workforce participation decline?',
    'Cross-country labor participation rates versus wage growth rates; demographic analysis of workforce dropout; regional unemployment versus wage suppression correlation',
    'If saturation point is low: labor exit through non-participation is the primary escape mechanism for powerless agents (shifts classification toward constrained rather than trapped). If saturation is high: suppression persists even with workforce shrinkage (confirms trapped exit option).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_at_saturation, empirical, 'Labor supply elasticity and saturation point for suppression dominance').

omega_variable(
    institutional_capture_of_labor_standards,
    'Are labor standards enforcement agencies captured by capital interests, making them part of the suppression mechanism rather than external regulators?',
    'Analysis of enforcement disparities across firm size and sector; revolving-door personnel flows between industry and labor agencies; documented cases of agency enforcement laxity in suppression-relevant violations',
    'If captured: the piton perspective is correct — minimum wage and standards enforcement are performative theater. If independent: regulatory framework retains some constraint capacity against suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_labor_standards, empirical, 'Institutional capture of labor standards enforcement').

omega_variable(
    global_wage_convergence_versus_local_suppression,
    'Is wage suppression in high-income countries driven by global labor arbitrage (natural economic pressure) or by deliberate policy choices to maximize global wage differential extraction?',
    'Comparative analysis of wage suppression in countries with high vs low immigration restrictions; correlation between offshoring capacity and domestic wage suppression; historical comparison of wage growth before/after globalization policy shifts',
    'If driven by arbitrage: suppression is a market consequence of policy choices about trade and immigration (contingent, tractable). If driven by deliberate policy for extraction: suppression is a direct institutional design choice (more directly extractive but also more directly reversible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_wage_convergence_versus_local_suppression, conceptual, 'Attribution of suppression to global arbitrage versus deliberate policy extraction').

omega_variable(
    union_decline_causation_feedback_loop,
    'Does union decline cause wage suppression or does wage suppression cause union decline? What is the causal structure of the feedback loop?',
    'Time-series analysis of union density and real wage growth with lag identification; case studies of union decline in sectors with and without prior wage suppression; analysis of strikebreaking and anti-union policy as causes versus consequences',
    'If suppression is primary cause of union decline: breaking suppression could enable union recovery (policy lever exists). If union decline is primary: suppression persists through worker organization failure (harder to reverse through single policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_decline_causation_feedback_loop, empirical, 'Causal relationship between union decline and wage suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wage_growth_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wage_tr_t0, wage_growth_suppression, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wage_tr_t20, wage_growth_suppression, theater_ratio, 20, 0.48).
narrative_ontology:measurement(wage_tr_t40, wage_growth_suppression, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(wage_be_t0, wage_growth_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wage_be_t20, wage_growth_suppression, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(wage_be_t40, wage_growth_suppression, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wage_growth_suppression, resource_allocation).
narrative_ontology:boltzmann_floor_override(wage_growth_suppression, 0.12).
narrative_ontology:affects_constraint(wage_growth_suppression, union_suppression_legal_framework).
narrative_ontology:affects_constraint(wage_growth_suppression, outsourcing_capacity_expansion).
narrative_ontology:affects_constraint(wage_growth_suppression, gig_economy_fragmentation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wage_growth_suppression, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
