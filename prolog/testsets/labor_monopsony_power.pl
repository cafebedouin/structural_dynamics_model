% ============================================================================
% CONSTRAINT STORY: labor_monopsony_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_monopsony_power, []).

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
 *   constraint_id: labor_monopsony_power
 *   human_readable: Labor Monopsony Power and Wage Extraction
 *   domain: labor_economics/employment
 *
 * SUMMARY:
 *   Labor monopsony power emerges when a single employer or a small number of
 *   employers dominate the labor supply in a geographic region or skill
 *   domain, enabling below-competitive wage-setting. This constraint exhibits
 *   genuine coordination between employer and workers (they must coordinate
 *   production) alongside asymmetric extraction (employers capture wage
 *   surplus due to power imbalance). The constraint is not pure extraction
 *   (Snare) because the employer-worker relationship genuinely solves a
 *   collective action problem — assembling a workforce, organizing
 *   production, negotiating terms. But coordination is not pure (Rope)
 *   because the employer's power generates suppression: workers have few exit
 *   options, creating wage formation asymmetry. The measurement trajectory
 *   shows increasing extractiveness over the 20-year interval (0.42 → 0.58)
 *   driven by: labor union density erosion, geographic housing cost
 *   concentration, increasing credential requirements reducing cross-industry
 *   mobility, and supply-chain consolidation increasing employer
 *   concentration. Theater ratio remains low (0.35) because monopsony
 *   wage-setting is direct and functional, not performative — the firm
 *   genuinely needs to set wages to attract labor; the constraint doesn't
 *   operate through ritual.
 *
 * KEY AGENTS:
 *   - Monopsony Employer: Primary beneficiary (institutional/arbitrage) — captures wage surplus; can relocate, automate, or shift labor sourcing; net beneficiary with low exit costs
 *   - Worker (geographically immobile): Primary victim (powerless/trapped) — few employment alternatives; faces relocation barriers; bears full extraction burden
 *   - Constrained Urban Worker: Secondary victim (moderate/constrained) — faces job-search costs, credential specificity, skill switching barriers; organized labor can represent
 *   - Labor Union: Organized victim/competitor (organized/constrained) — exerts countervailing power through collective bargaining; can strike, mobilize, or organize workers
 *   - Consumer Base: Secondary beneficiary (institutional/arbitrage) — benefits from monopsony wage suppression passed through to lower prices; diffuse, unorganized
 *   - Regulatory Framework: Institutional mechanism (institutional/arbitrage) — intended to constrain monopsony but often degraded through weak enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_monopsony_power, 0.58).
domain_priors:suppression_score(labor_monopsony_power, 0.68).
domain_priors:theater_ratio(labor_monopsony_power, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_monopsony_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_monopsony_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(labor_monopsony_power, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_monopsony_power, tangled_rope).
narrative_ontology:human_readable(labor_monopsony_power, "Labor Monopsony Power and Wage Extraction").
narrative_ontology:topic_domain(labor_monopsony_power, "labor_economics/employment").

domain_priors:requires_active_enforcement(labor_monopsony_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_monopsony_power, monopsony_employer).
narrative_ontology:constraint_beneficiary(labor_monopsony_power, firm_shareholders).
narrative_ontology:constraint_beneficiary(labor_monopsony_power, consumer_base).
narrative_ontology:constraint_victim(labor_monopsony_power, worker_mobility).
narrative_ontology:constraint_victim(labor_monopsony_power, wage_formation_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILE WORKER (SNARE) — In single-employer labor markets (rural regions, company towns, specialized skill locales), workers face material barriers to exit: relocation costs, housing ties, family considerations, lack of alternative employment. High suppression from geographic/economic immobility. Employer wage-setting is experienced as extraction with minimal coordination benefit. No exit option; maximum experienced chi.
constraint_indexing:constraint_classification(labor_monopsony_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONSTRAINED URBAN WORKER (TANGLED ROPE) — In metropolitan labor markets with multiple employers but high job-search costs, skill-specificity, credential barriers, or industry concentration, workers face significant but surmountable exit costs. The employer-worker relationship coordinates production (genuine coordination benefit) while extracting wage surplus. Suppression high but not total; organized labor can exert countervailing power.
constraint_indexing:constraint_classification(labor_monopsony_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONOPSONY EMPLOYER (ROPE) — From the firm's perspective, labor monopsony power is a coordination mechanism: the firm structures compensation and working conditions, workers coordinate their labor supply in response. The firm experiences the relationship as coordination that solves a collective action problem (assembling a workforce). Arbitrage options available: shift production, automate, relocate, or hire from alternative labor pools. Net beneficiary; experienced extraction runs toward the firm.
constraint_indexing:constraint_classification(labor_monopsony_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION (TANGLED ROPE) — Organized labor represents a coordination mechanism (wage-setting through collective bargaining) that simultaneously extracts from the monopsony firm's surplus. Union extraction depends on institutional power: strength in organizing, legal protections, and industry density. Suppression runs in both directions — union faces employer pressure to decertify or relocate; firm faces labor action and wage pressure. Experienced extraction is moderate because both parties can organize and exercise countervailing power.
constraint_indexing:constraint_classification(labor_monopsony_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Minimum wage laws, labor standards, antitrust enforcement, and labor organizing protections are institutional mechanisms intended to constrain monopsony extraction. In practice, regulatory enforcement is often weak and theatrical: minimum wages set below prevailing market rates, labor law violations routinely ignored, antitrust cases rare. The regulatory framework persists as a formality (theater_ratio high) despite degraded enforcement capacity. Real monopsony extraction bypasses or absorbs these constraints through compliance theater.
constraint_indexing:constraint_classification(labor_monopsony_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, labor monopsony power represents a genuine structural tension: firms must coordinate labor supply (coordination function requires wage flexibility) while labor markets concentrate power (extraction flows from power imbalance). This perspective sees both the coordination and extraction mechanisms as intrinsic to how modern labor markets operate under capital concentration.
constraint_indexing:constraint_classification(labor_monopsony_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_monopsony_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_monopsony_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_monopsony_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_monopsony_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_monopsony_power, TR),
    TR >= 0.70.

:- end_tests(labor_monopsony_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Monopsony wage suppression is documented empirically at 10-25% of competitive wage rates in concentrated labor markets. The extractiveness value (0.58) reflects that the extraction is real and substantial but not total — some firms do offer competitive wages even in concentrated markets, and union wage premiums (15-25%) show workers can capture back part of surplus with organized power. The value is lower than pure extraction (Snare at 0.70+) because the coordination function is genuine and creates value both sides benefit from (employment itself). Suppression (0.68): High. Multiple barriers constrain worker exit options: geographic immobility from housing costs and family ties, credential specificity limiting cross-industry mobility, search costs and information asymmetries, and declining union coverage reducing countervailing power. However, suppression is not total (0.90+) — some workers can move, some can switch industries, and some can organize. Theater ratio (0.35): Low. Monopsony extraction operates through direct wage-setting, not performative mechanisms. Employers genuinely must set wages to attract labor; the constraint doesn't hide behind ritual or justificatory theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The immobile worker sees a snare: no exit, high suppression, pure extraction. The constrained urban worker sees tangled rope: both coordination and extraction, some organized power. The monopsony firm sees rope: coordination mechanism that solves labor assembly. The union sees tangled rope from the opposite direction: they are extracting countervailing power from the firm while coordinating worker interests. The regulatory framework sees piton: intended to constrain but degraded in practice. The analytical observer at civilizational scope sees tangled rope as the irreducible structural tension between modern labor market coordination and capital concentration. The gap reveals that 'monopsony power' is not a single constraint but a perspectival function: identical structural conditions produce six different classification types depending on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power atom, exit options, and structural relationship to extraction flow. Powerless trapped workers experience maximum d (0.95), producing high f(d) and high experienced χ. The monopsony employer experiences low d (0.05-0.15) because they benefit from the extraction and have arbitrage exit options. Organized labor experiences moderate d (0.50-0.65) because they are organized enough to exert countervailing force but not enough to fully eliminate extraction. The analytical observer at civilizational scope experiences moderate-high d (0.72) because the structural tension between coordination and concentration is irreducible at that time scale. The regulatory framework experiences low d from a formal perspective (intended beneficiary of workers) but the actual enforcement weakness means the real d is higher — a directionality override would move this from 0.15 to 0.35-0.40 to reflect regulatory capture.
 *
 * MANDATROPHY ANALYSIS:
 *   MONOPSONY RESOLVES MANDATROPHY BY INSTITUTIONAL DECOMPOSITION: The constraint avoids the false summit trap (mountain) by having genuine coordination content (benef iciaries, production requirement) and genuine extraction asymmetry (victims, power imbalance). The beneficiary (employer) experiences rope because they genuinely coordinate labor supply. The victim (worker) experiences snare because they genuinely face extraction. The organized agent (union) experiences tangled rope from both sides — coordination through collective bargaining + extraction of surplus back from the firm. The analytical observer sees tangled rope because the structural irreducibility (must coordinate + power concentrated) is real. No perspective naturalizes the constraint as an immutable law. The regulatory framework perspective (piton) is significant: it represents the institutional response to monopsony that is intended to resolve the extraction but operates with degraded enforcement. This is exactly where Scaffold analysis becomes relevant — if regulatory enforcement strengthens or alternative labor market mechanisms (remote work, skill standardization, union revitalization) emerge, the constraint's time horizon shortens and it should reclassify toward Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monopsony_definition_boundary,
    'What concentration threshold (HHI, wage elasticity of labor supply) defines monopsony vs oligopsony vs competitive labor market?',
    'Empirical measurement of wage-setting power: compare wage offers to alternative opportunities; estimate labor supply elasticity to individual firms; measure HHI in local labor markets',
    'If threshold high (strict definition): fewer markets classified as monopsony, constraint appears less severe. If threshold low (loose definition): constraint appears endemic to modern labor markets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monopsony_definition_boundary, empirical, 'Threshold defining monopsony market concentration').

omega_variable(
    wage_suppression_attribution,
    'How much wage suppression is attributable to monopsony power vs productivity differences, skill mismatch, or labor quality variation?',
    'Regression analysis controlling for worker human capital; comparison of identical workers'' wages across different local labor markets; before/after studies of mergers that increase labor market concentration',
    'If monopsony explains >30% of wage variance: extractiveness estimate (0.58) is conservative. If <10%: extractiveness should be lower (0.35-0.40 range).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_attribution, empirical, 'Attribution of wage suppression to monopsony power').

omega_variable(
    countervailing_power_efficacy,
    'Do unions, professional associations, or worker collectives successfully constrain monopsony extraction, or does monopsony power persistently exceed countervailing force?',
    'Historical analysis of union wage premiums in concentrated vs competitive markets; measurement of union density erosion over time; comparison of monopsony wage suppression in unionized vs non-unionized sectors',
    'If countervailing power effective: constraint should be classified as Tangled Rope from most perspectives. If ineffective: constraint should be Snare or Scaffold (temporary union countervailing power being eroded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countervailing_power_efficacy, empirical, 'Whether countervailing labor power constrains monopsony extraction').

omega_variable(
    geographic_mobility_trend,
    'Is worker geographic mobility increasing (due to remote work, digital job markets) or decreasing (due to housing costs, credential geography, climate constraints)?',
    'Longitudinal measurement of interstate migration rates; comparison of job search radius pre/post-pandemic; analysis of remote work adoption in concentrated labor markets',
    'If mobility increasing: monopsony power should decline over interval; measurements show decreasing extractiveness. If mobility decreasing: monopsony deepens; measurements show increasing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_mobility_trend, empirical, 'Trend in worker geographic mobility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_monopsony_power, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmp_tr_t0, labor_monopsony_power, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lmp_tr_t10, labor_monopsony_power, theater_ratio, 10, 0.32).
narrative_ontology:measurement(lmp_tr_t20, labor_monopsony_power, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(lmp_be_t0, labor_monopsony_power, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lmp_be_t10, labor_monopsony_power, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(lmp_be_t20, labor_monopsony_power, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_monopsony_power, resource_allocation).
narrative_ontology:affects_constraint(labor_monopsony_power, union_decline_institutional_erosion).
narrative_ontology:affects_constraint(labor_monopsony_power, geographic_wage_disparities).
narrative_ontology:affects_constraint(labor_monopsony_power, automation_labor_replacement).

% DUAL FORMULATION NOTE:
% Labor monopsony power is upstream of multiple downstream constraints: union decline is an institutional response to concentrated employer power, geographic wage disparities are a symptom of regional labor market concentration, and automation is an employer strategy enabled by monopsony conditions (low wage pressure reduces automation cost-benefit). Each downstream constraint has its own extractiveness value and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_monopsony_power, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
