% ============================================================================
% CONSTRAINT STORY: intergenerational_wage_compression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intergenerational_wage_compression, []).

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
 *   constraint_id: intergenerational_wage_compression
 *   human_readable: Intergenerational Wage Compression
 *   domain: labor_economics/demographic_policy
 *
 * SUMMARY:
 *   Intergenerational wage compression describes the structural flattening of
 *   wage progression across age cohorts in advanced labor markets since
 *   approximately 1980. Young workers entering the labor force after this
 *   period face suppressed entry-level wages and dampened wage-age slopes
 *   compared to their predecessors. This constraint exhibits the Tangled Rope
 *   structure: genuine coordination functions (wage floor protection for
 *   incumbent workers, predictable cost structures for employers) coupled
 *   with asymmetric extraction (permanent lifetime earnings loss for younger
 *   cohorts). The suppression (0.65) reflects multiple barriers:
 *   seniority-based hiring practices, credential inflation that delays labor
 *   market entry, geographic immobility costs, and lack of intergenerational
 *   political organizing. The theater ratio (0.48, rising to 0.48) reflects
 *   increasing rhetorical cover: 'human capital investment,' 'market
 *   efficiency,' and 'earning your stripes' narratives legitimize what is
 *   structurally an intergenerational transfer of pension and wage-floor
 *   costs from older to younger cohorts. The extractiveness trajectory (0.28
 *   → 0.58) shows accumulation: as cohorts age in place without wage
 *   catch-up, the lifetime extraction becomes visible; when viewed at entry,
 *   extractiveness appears moderate; when viewed across working life, it
 *   becomes severe.
 *
 * KEY AGENTS:
 *   - Young Labor Cohort: Primary victim (powerless/trapped) — faces locked-in lower wage trajectory from market entry; cannot exit labor market without extreme cost; bears permanent extraction
 *   - Incumbent Workforce: Primary beneficiary (institutional/arbitrage) — protected by seniority structures and defined-benefit pensions funded partly by compressed young-cohort wages; experiences constraint as coordination mechanism defending acquired compensation
 *   - Employers: Secondary beneficiary (institutional/arbitrage) — benefit from flat wage structures reducing labor costs and negotiation complexity; arbitrage through outsourcing and automation
 *   - Labor Movement Institutions: Institutional actor (organized/constrained) — union leadership coordinates seniority protection (genuine coordination) while blocking intergenerational mobility and apprenticeship access (extraction mechanism); constrained by declining power and internal legitimacy loss
 *   - Capital Asset Holders: Tertiary beneficiary (institutional/arbitrage) — compressed wages divert income away from wage earners toward returns on capital and financial assets
 *   - Policy/Narrative Institutions: Institutional actor (institutional/arbitrage) — propagate 'human capital' framing that naturalizes compression as investment; maintain performative language about 'entry-level development' while structural compression persists
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — identifies the constraint as a demographic-economic coupling where pension liabilities of aging cohorts are structurally transferred to younger cohorts through labor market institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intergenerational_wage_compression, 0.58).
domain_priors:suppression_score(intergenerational_wage_compression, 0.65).
domain_priors:theater_ratio(intergenerational_wage_compression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intergenerational_wage_compression, extractiveness, 0.58).
narrative_ontology:constraint_metric(intergenerational_wage_compression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(intergenerational_wage_compression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intergenerational_wage_compression, tangled_rope).
narrative_ontology:human_readable(intergenerational_wage_compression, "Intergenerational Wage Compression").
narrative_ontology:topic_domain(intergenerational_wage_compression, "labor_economics/demographic_policy").

domain_priors:requires_active_enforcement(intergenerational_wage_compression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intergenerational_wage_compression, incumbent_workforce).
narrative_ontology:constraint_beneficiary(intergenerational_wage_compression, employers_wage_restraint).
narrative_ontology:constraint_beneficiary(intergenerational_wage_compression, capital_asset_holders).
narrative_ontology:constraint_victim(intergenerational_wage_compression, younger_labor_cohorts).
narrative_ontology:constraint_victim(intergenerational_wage_compression, future_wage_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG WORKER TRAPPED (SNARE) — Faces flattened wage progression and suppressed entry-level wages despite equal or superior education. Cannot exit labor market; bears full extraction. Career earnings permanently compressed by cohort entry timing. Maximum experienced extraction.
constraint_indexing:constraint_classification(intergenerational_wage_compression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER WORKER CONSTRAINED (TANGLED ROPE) — Some wage growth through experience but significantly dampened by compressed floor. Benefits from seniority-based wage structures but extraction occurs through wage ceiling suppression relative to pre-1980s cohorts. Constrained by sector-specific skills and relocation costs. Moderate experienced extraction with some coordination benefit.
constraint_indexing:constraint_classification(intergenerational_wage_compression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT SENIORITY STRUCTURE (ROPE) — Benefits from wage floor protection and defined benefit pension structures locked in before compression began. Experiences the constraint as coordination: protecting acquired wages requires enforcing seniority-based layoff protections and wage step progression. Net beneficiary through institutional arbitrage — can maintain current compensation while younger cohorts absorb compression.
constraint_indexing:constraint_classification(intergenerational_wage_compression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYER INSTITUTIONS (ROPE) — Benefit from flat wage structures and compressed entry-level wages as coordination mechanism for labor cost control. Flattening reduces wage variation and negotiation friction while suppressing total labor costs. Exit through outsourcing/automation creates arbitrage advantage. Net beneficiary.
constraint_indexing:constraint_classification(intergenerational_wage_compression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR MOVEMENT ORGANIZED (TANGLED ROPE) — Union leadership coordinates seniority protection and wage floors for current members (genuine coordination function) while simultaneously blocking wage growth for new entrants through restrictive hiring practices and apprenticeship limitations. Active enforcement of two-tier wage systems. Organized power but constrained by declining membership and political legitimacy loss from generational division.
constraint_indexing:constraint_classification(intergenerational_wage_compression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HUMAN CAPITAL NARRATIVE (PITON) — Policy and corporate rhetoric frames compressed wages as investment in 'talent development' and 'market efficiency.' The narrative persists despite decoupling from actual skill-building or productivity gains. Theater ratio high (0.48 reflects performative investment language while actual wage growth flattens). The human capital mythology maintains institutional legitimacy for wage compression while its functional purpose has degraded.
constraint_indexing:constraint_classification(intergenerational_wage_compression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits genuine coordination (protecting wage levels during inflation/productivity shifts) coupled with asymmetric extraction (intergenerational transfer of costs from older to younger cohorts). Effective extraction chi=0.58 represents coordination value offset by distributional harm. From civilizational scope, the constraint reveals structural coupling between demographic pyramids, pension liabilities, and labor market institutions.
constraint_indexing:constraint_classification(intergenerational_wage_compression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intergenerational_wage_compression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intergenerational_wage_compression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intergenerational_wage_compression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intergenerational_wage_compression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intergenerational_wage_compression, TR),
    TR >= 0.70.

:- end_tests(intergenerational_wage_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the period. Initial value (0.28) reflects the emergence of compression in 1980s as a discrete phenomenon; intermediate value (0.42) reflects accumulation as compressed cohorts age; final value (0.58) reflects full lifetime extraction visibility. The metric measures the wage differential between compressed cohorts and pre-compression career wage profiles, adjusted for productivity growth. Suppression (0.65): Moderate-high and structural. Multiple barriers reinforce wage floor: seniority-based hiring locks out competition from outside; credential inflation delays market entry; geographic immobility costs suppress inter-regional wage arbitrage; skill-specificity (sector-specific credentials) increases switching costs; generational political division prevents collective action for wage policy reform. Theater ratio (0.48): Moderate and rising. Initial narratives emphasize 'entry-level development' (0.22); mid-period adds 'market efficiency' and 'skill-biased technical change' (0.35); current period saturates with 'human capital investment,' 'flexibility,' and 'gig economy opportunity' language (0.48) while wages remain compressed. Theater has not reached piton levels (≥0.70) because genuine wage differentiation still exists (seniority steps, sector variation), but performative language is increasing faster than structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the power of indexical classification to reveal distributional conflicts hidden by institutional framing. All parties experience the same structural phenomenon (flattened wage progression) but perceive radically different types: beneficiaries see Rope (coordination), victims see Snare (extraction), organized actors see mixed Tangled Rope with institutional leverage. The gap reveals that 'intergenerational wage compression' is not a single constraint but a distributional conflict structured by power asymmetries. Young workers cannot exit (trapped), cannot coordinate (political silence), and cannot reframe (narratives are set by institutional actors with more power). This is the signature of suppression ≥ 0.60: the target cannot contest the frame or organize against it. The labor movement's Tangled Rope perspective is diagnostically revealing: leadership genuinely coordinates for current members (authentic rope function) while simultaneously blocking intergenerational mobility (extraction mechanism). This is how Tangled Ropes hide — by performing coordination for one subgroup while extracting from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their power level, exit options, and structural relationship to the extraction flow. Young workers with trapped exit options and victim status experience d ≈ 0.95, yielding high f(d) ≈ 1.42 — maximum experienced extractiveness chi. Mid-career workers with constrained exit and partial victim/beneficiary status experience d ≈ 0.68, yielding moderate f(d) ≈ 1.05. Incumbent unions with organized power but constrained options and beneficiary status experience d ≈ 0.45, yielding moderate f(d) ≈ 0.58. Employers with institutional power, arbitrage options, and clear beneficiary status experience d ≈ 0.15, yielding f(d) ≈ -0.01 (negative effective extraction — they gain coordination value). The analytical observer at civilizational scope with analytical exit experiences d ≈ 0.72, yielding f(d) ≈ 1.15, which combined with global scope modifier σ(S)=1.2 produces chi ≈ 0.83 at the analytical context — high but not maximum, because the analytical view captures both coordination and extraction functions.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION NEEDED: Intergenerational wage compression decomposes into structurally distinct claims with different ε values: (1) Seniority-wage coupling (ε ≈ 0.35, Rope or Tangled Rope depending on productivity correlation) — whether seniority-based pay reflects skill differences or institutional lock. (2) Pension-wage transfer (ε ≈ 0.62, Snare) — the intergenerational transfer mechanism whereby young cohort wages fund older cohort pensions. (3) Credential inflation (ε ≈ 0.48, Tangled Rope) — education requirements inflating faster than wage returns, delaying labor market entry. (4) Narrative capture (ε ≈ 0.40, Piton) — 'human capital' framing naturalizing compression. The current story aggregates these into a single ε ≈ 0.58 (Tangled Rope at civilizational/analytical scale). Mandatrophy is resolved by recognizing that each component has different ε and different resolution pathways: seniority can be decoupled from pay (policy intervention); pensions can be restructured (political economy problem); credential inflation can be reversed (education reform); narrative capture requires institutional legitimacy loss. The constraint appears unified at the aggregate level but decomposes into separable mechanisms at lower scope. Future work should decompose into constraint family and track each ε separately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seniority_vs_productivity_mechanism,
    'Are wages compressed because seniority-based pay reflects true productivity differences, or because institutional structures enforce seniority independent of productivity?',
    'Wage-productivity correlation analysis within firms; comparison of wage growth profiles across seniority-based vs productivity-based compensation models; examination of wage-skill mismatch metrics over time',
    'If seniority correlates with productivity: compression is partly coordination (returning value to skilled workers). If seniority decouples from productivity: compression is pure extraction with institutional cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seniority_vs_productivity_mechanism, empirical, 'Whether seniority-based wages reflect true productivity differences').

omega_variable(
    generational_pension_transfer,
    'What proportion of compressed young-cohort wages funds unfunded or underfunded pension liabilities of older cohorts?',
    'Actuarial analysis of pension asset-liability gaps; wage suppression benefit redistribution accounting; demographic simulation of pension solvency under alternative wage structures',
    'If high proportion: constraint is primarily intergenerational transfer mechanism (extraction classification strengthens). If low proportion: wage suppression is independent of pension dynamics (extraction mechanism is labor market structure, not demographic transfer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_pension_transfer, empirical, 'Proportion of wage compression funding unfunded pension liabilities').

omega_variable(
    exit_option_availability_for_younger_cohorts,
    'Do younger workers have meaningful exit options (international migration, alternative labor markets, education retraining) or is the ''trapped'' classification too severe?',
    'International wage comparison; analysis of emigration flows by age cohort; cost-benefit analysis of education/retraining pathways vs. accepting compressed wages; measurement of actual mobility rates',
    'If exit options are real and used: reclassify young worker exit_options to ''constrained'' or ''mobile'' rather than ''trapped'' — extraction decreases. If exit is blocked by debt/credential requirements: ''trapped'' confirmed — extraction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability_for_younger_cohorts, empirical, 'Whether younger cohorts have meaningful exit options from wage compression').

omega_variable(
    technology_vs_institutional_causation,
    'Is wage compression driven by technological displacement and skill-biased change (structural, hard to reverse) or by institutional choices in labor markets, wage setting, and trade policy (contingent, reversible)?',
    'Comparative analysis of wage compression across countries with different institutional/policy regimes; decomposition of wage growth into productivity, bargaining power, and institutional components; historical counterfactual analysis of wage outcomes under alternative policy sets',
    'If technology-driven (structural): mountain perspective gains credibility — compression is immutable. If institution-driven: snare/tangled_rope confirmed — constraint is contingent and reversible through policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_vs_institutional_causation, conceptual, 'Whether wage compression is driven by technology or institutional choices').

omega_variable(
    political_economy_of_generational_silence,
    'Why do young workers not organize politically to reverse wage compression despite measurable lifetime earnings loss?',
    'Analysis of age-cohort political participation and collective action; examination of narrative framing in media and policy discourse; identification of institutional barriers to intergenerational political coordination',
    'If silence reflects powerlessness: suppression=0.65 confirmed. If silence reflects identity capture (young workers internalize ''paying dues'' narrative): identity_locked classification warranted despite structural mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_of_generational_silence, empirical, 'Political economy of low organizing activity among compressed cohorts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intergenerational_wage_compression, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iwc_tr_t0, intergenerational_wage_compression, theater_ratio, 0, 0.22).
narrative_ontology:measurement(iwc_tr_t10, intergenerational_wage_compression, theater_ratio, 10, 0.35).
narrative_ontology:measurement(iwc_tr_t20, intergenerational_wage_compression, theater_ratio, 20, 0.48).
narrative_ontology:measurement(iwc_tr_t30, intergenerational_wage_compression, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(iwc_be_t0, intergenerational_wage_compression, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(iwc_be_t10, intergenerational_wage_compression, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(iwc_be_t20, intergenerational_wage_compression, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(iwc_be_t30, intergenerational_wage_compression, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intergenerational_wage_compression, resource_allocation).
narrative_ontology:affects_constraint(intergenerational_wage_compression, pension_funding_crisis).
narrative_ontology:affects_constraint(intergenerational_wage_compression, credential_inflation_spiral).
narrative_ontology:affects_constraint(intergenerational_wage_compression, labor_movement_generational_fragmentation).

% DUAL FORMULATION NOTE:
% Intergenerational wage compression is downstream of structural demographic and policy shifts (aging population, pension underfunding, globalization of labor supply, credential inflation) and upstream of political economy outcomes (generational resentment, reduced fertility, intergenerational mobility collapse, pension system viability). Linked constraints reflect decomposition per ε-invariance principle: seniority-wage coupling has distinct empirical status from pension transfer mechanism; both couple through labor market institutions but have different structural origins and resolution pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intergenerational_wage_compression, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
