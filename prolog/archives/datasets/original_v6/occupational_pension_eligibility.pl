% ============================================================================
% CONSTRAINT STORY: occupational_pension_eligibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_occupational_pension_eligibility, []).

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
 *   constraint_id: occupational_pension_eligibility
 *   human_readable: Occupational Pension Eligibility Requirements
 *   domain: labor/financial/social
 *
 * SUMMARY:
 *   Occupational pension eligibility systems create a structural constraint
 *   that simultaneously coordinates long-term retirement saving and extracts
 *   security from workers outside permanent employment. The constraint
 *   exhibits the full range of DR classifications depending on observer
 *   position. For permanent workers, occupational pensions enable genuine
 *   savings coordination. For precarious workers excluded by service-length
 *   and contract-type requirements, the same rules function as a Snare —
 *   their retirement insecurity subsidizes the stability of those within the
 *   system. The constraint has intensified over the 30-year interval
 *   (extractiveness rising from 0.42 to 0.58) as labor market composition
 *   shifted toward precarity without equivalent pension architecture
 *   adaptation. Theater ratio has increased modestly (0.35 to 0.48),
 *   reflecting that eligibility justifications (fund solvency, adverse
 *   selection prevention) remain plausible but increasingly detached from
 *   labor market reality. The occupational pension model was designed for a
 *   labor market of stable single-employer careers; it persists largely
 *   through institutional inertia as that labor market has fundamentally
 *   transformed.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victims (powerless/trapped) — systematically excluded by contract-type and service-length requirements; bear retirement insecurity cost to enable fund stability for insiders
 *   - Career Changers: Secondary victims (moderate/constrained) — face vesting penalties and accrual loss when transitioning between occupations; constrained but not trapped
 *   - Incumbent Permanent Workers: Primary beneficiaries (institutional/arbitrage) — accumulate pension wealth within single-employer trajectory; experience eligibility rules as coordination mechanism enabling predictable savings
 *   - Pension Fund Administrators: Secondary beneficiaries (institutional/arbitrage) — benefit from clear eligibility gates enabling actuarial modeling; maintain fund stability through restricted eligible population
 *   - Labor Organizations: Organized advocates (organized/constrained) — negotiate on behalf of workers but cannot unilaterally redesign pension architecture; constrained by need for employer participation in reform
 *   - Occupational Pension Institution: Institutional actor (institutional/arbitrage) — maintains degraded eligibility framework through inertia; sees own design as necessary but increasingly difficult to defend
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing specific eligibility rules as immutable when only existence of some gate is structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(occupational_pension_eligibility, 0.58).
domain_priors:suppression_score(occupational_pension_eligibility, 0.62).
domain_priors:theater_ratio(occupational_pension_eligibility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(occupational_pension_eligibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(occupational_pension_eligibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(occupational_pension_eligibility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(occupational_pension_eligibility, tangled_rope).
narrative_ontology:human_readable(occupational_pension_eligibility, "Occupational Pension Eligibility Requirements").
narrative_ontology:topic_domain(occupational_pension_eligibility, "labor/financial/social").

domain_priors:requires_active_enforcement(occupational_pension_eligibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(occupational_pension_eligibility, pension_fund_administrators).
narrative_ontology:constraint_beneficiary(occupational_pension_eligibility, employer_sponsors).
narrative_ontology:constraint_beneficiary(occupational_pension_eligibility, incumbent_permanent_workforce).
narrative_ontology:constraint_victim(occupational_pension_eligibility, precarious_workers).
narrative_ontology:constraint_victim(occupational_pension_eligibility, career_changers).
narrative_ontology:constraint_victim(occupational_pension_eligibility, gig_economy_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by income dependency on contract roles that systematically exclude pension eligibility. No realistic exit from precarity within biographical timeframe. Bears full extraction: sacrifices retirement security for current income necessity. Maximum suppression — material barriers (no stable employer) prevent exit.
constraint_indexing:constraint_classification(occupational_pension_eligibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER CHANGER (TANGLED ROPE) — Faces significant but surmountable barriers to pension eligibility after vocational shift. Vesting requirements and service continuity rules create genuine coordination function (fund stability) alongside asymmetric extraction (penalties for transitions). Can exit with substantial cost (lost accrual years, reduced retirement income). Constrained mobility — structural change incurs pension loss, not material trap.
constraint_indexing:constraint_classification(occupational_pension_eligibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PERMANENT WORKER (ROPE) — Benefits from pension accumulation within stable single-employer trajectory. Experiences the constraint as coordination: eligibility rules create predictable long-term saving, enabling employer contribution strategy and workforce retention. Net beneficiary through arbitrage — can leverage credential portability within permanent tier.
constraint_indexing:constraint_classification(occupational_pension_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PENSION FUND ADMINISTRATOR (ROPE) — Experiences eligibility rules as pure coordination mechanism: minimum service requirements, vesting schedules, and contribution caps create actuarial stability. Fund administrators benefit from clear eligibility gates that reduce adverse selection and enable predictable liability modeling. Arbitrage position — can adjust fund strategy based on eligible population definitions.
constraint_indexing:constraint_classification(occupational_pension_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ORGANIZATION (TANGLED ROPE) — Organized agents (unions, worker advocacy groups) see both coordination function (negotiated pension levels achieve collective security goal) and extraction mechanism (eligibility rules systematically exclude non-permanent tiers, fragmenting worker leverage). Constrained exit: unions can negotiate but cannot unilaterally overhaul occupational pension architecture without employer participation. Moderate effective extraction because organized actors have negotiating power even if not total structural power.
constraint_indexing:constraint_classification(occupational_pension_eligibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OCCUPATIONAL PENSION INSTITUTION (PITON) — The occupational pension model itself is a degraded institutional form: designed in mid-20th century for stable single-employer careers that no longer represent majority labor market experience. Theater ratio (0.48) reflects that eligibility rituals (service length minimums, vesting cliffs) maintain performative legitimacy as 'protection of fund solvency' while actual function (risk pooling) could operate under alternative eligibility designs. Institutional inertia preserves the form despite changing labor market reality. The institution sees its own eligibility framework as necessary but increasingly difficult to justify.
constraint_indexing:constraint_classification(occupational_pension_eligibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of occupational pension eligibility gate is inherent to risk-pooling mechanisms: actuarial models require defined populations to function. This perspective risks naturalizing the specific institutional arrangement (service length, vesting cliffs, single-employer basis) as immutable, when the true natural law is only the existence of *some* eligibility boundary. The false summit here is conflating 'gates exist' (necessary) with 'these gates are necessary' (contingent).
constraint_indexing:constraint_classification(occupational_pension_eligibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(occupational_pension_eligibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(occupational_pension_eligibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(occupational_pension_eligibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(occupational_pension_eligibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(occupational_pension_eligibility, TR),
    TR >= 0.70.

:- end_tests(occupational_pension_eligibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts retirement security from precarious workers to enable stable actuarial modeling for permanent workers. The extraction is substantial but not maximal because some precarious workers do accumulate pension benefits (through periods of permanent employment, supplementary savings), and the system is partially redesigned through workplace pensions in some jurisdictions. The rising trajectory (0.42→0.58) reflects intensifying extraction as precarity has increased without eligibility expansion. Suppression (0.62): Moderate-high. Material barriers (income dependency on contract roles, lack of stable employer connection) trap precarious workers. Psychological suppression exists too — the fiction that occupational pensions are universal creates false consensus that precarious workers are exceptions rather than systematic exclusions. Institutional suppression operates through regulatory capture: eligibility rules are written by and for permanent-tier beneficiaries. Theater ratio (0.48): Moderate. Eligibility justifications (fund solvency, protection against adverse selection) have genuine actuarial rationale, but increasing disconnection from labor market reality raises theater content. The constraint's performative element is maintaining the fiction that occupational pensions can serve a two-tier labor market when architecture assumes permanent employment universality.
 *
 * PERSPECTIVAL GAP:
 *   Permanent workers see Rope (coordination enabling savings); precarious workers see Snare (extraction trap). Labor organizations see Tangled Rope (mixed coordination and extraction with negotiating power). The institutional system sees Piton (degraded ritual). The analytical observer risks Mountain (naturalizing contingent rules as necessary). These gaps are not measurement errors — they are structural. The constraint genuinely produces different classifications across the observation site because it creates asymmetric relationships: some agents coordinate and accumulate, others are trapped and excluded. No single classification applies to all observers. The Tangled Rope classification from the analytical baseline reflects that the constraint has both genuine coordination function (fund stability, long-term savings enablement) and systematic extraction (precarity subsidizes security).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural position: power level, exit options, and relationship to extraction flow. Precarious workers have high d (0.92+) because they are trapped victims — no exit option, minimal power, full extraction experienced. Career changers have moderate-high d (0.65-0.75) because they are victims with constrained exit — they can change careers at cost. Incumbent permanent workers have low d (0.10-0.20) because they are beneficiaries with arbitrage options — extraction flows toward them. Pension administrators have very low d (0.05-0.15) because they are beneficiaries with maximum institutional arbitrage. The labor organization has moderate d (0.50-0.60) because they are organized but constrained — they have negotiating power but not unilateral redesign authority. These directionality values drive the χ formula: precarious workers experience maximum chi because high d × moderate ε × scope modifier = experienced extraction. Permanent workers experience negative or zero chi because low d dampens the base extractiveness. The institutional perspective (Piton) has χ dampened by theater ratio elevation — the coordination function is increasingly performative.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by demonstrating that occupational pension eligibility is NOT pure extraction (Snare) despite the precarious worker experiencing it as Snare. From the analytical baseline, the classification is Tangled Rope because: (1) genuine coordination function exists — eligibility rules enable actuarial stability and predictable long-term savings for permanent workers and pension administrators; (2) systematic asymmetric extraction occurs — precarious workers subsidize that stability through exclusion and retirement insecurity; (3) active enforcement is required — eligibility gates are maintained through regulatory design and institutional practice; (4) the beneficiary/victim divide is clear — permanent workers and administrators benefit; precarious workers bear costs. The mandatrophy is resolved by rejecting the single-perspective classification (which would be Snare) and accepting the multi-perspectival reading: the constraint is Tangled Rope from the analytical view, decomposing into Snare (precarious), Rope (permanent), Piton (institution), Tangled Rope (labor organizations), and false Mountain (naive analytical observer). The false mountain prevention here is critical: the analyst must resist naturalizing 'actuarial necessity' as justification for specific eligibility rules that could be redesigned while preserving actuarial stability through alternative designs (portable pensions, multi-employer schemes, risk pooling across career patterns). The specific rules are institutional choice, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gig_economy_redesign_feasibility,
    'Can occupational pension eligibility be redesigned to accommodate portfolio careers and gig work without destroying actuarial stability?',
    'Pilot programs with portable accrual across employers; analysis of success/failure of multi-employer pension schemes (Netherlands, Scandinavia) in labor-flexible economies; actuarial modeling of alternative eligibility criteria',
    'If feasible: the current eligibility rules are optimization choice, not structural necessity. Classification shifts toward institutional capture narrative (Tangled Rope with negotiated change potential). If not feasible: the occupational pension model is genuinely incompatible with modern labor markets, requiring replacement rather than reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gig_economy_redesign_feasibility, empirical, 'Whether pension eligibility can accommodate portfolio careers').

omega_variable(
    precarity_cause_attribution,
    'To what extent does occupational pension eligibility rules CAUSE precarious contracting versus reflect employer preference for flexibility that would exist regardless?',
    'Comparative analysis of labor market structure in high-occupational-pension vs low-occupational-pension economies; counterfactual analysis of employer hiring practices if pension eligibility were extended to all workers; historical analysis of when and why employers adopted contract labor strategies',
    'If eligibility rules cause precarity: classification is Snare for precarious workers (the constraint creates the trap). If rules reflect employer preference for flexibility: classification may degrade to structural feature of labor markets themselves (false mountain). If partial: the constraint is Tangled Rope capturing part of the causal chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precarity_cause_attribution, empirical, 'Causal role of pension eligibility in precarious employment').

omega_variable(
    solidarity_fragmentation_mechanism,
    'Does the occupational pension two-tier system (eligible/ineligible) actively suppress class solidarity and worker organizing capacity, or merely reflect pre-existing worker fragmentation?',
    'Analysis of union organizing success rates by eligibility tier; measurement of wage-setting coordination across tiers; historical examination of labor power during periods of pension inclusion expansion vs contraction; comparative study of organizing in unified-pension vs fragmented-pension labor markets',
    'If actively suppressive: the constraint is designed Snare (beneficiaries consciously exclude to fragment labor). If merely reflective: the constraint is emergent Tangled Rope. Classification consequence: design-Snare implies intentional maintenance; emergent Tangled Rope implies inertial maintenance. Different remediation strategies follow.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(solidarity_fragmentation_mechanism, empirical, 'Whether pension eligibility actively suppresses worker organizing').

omega_variable(
    replacement_rate_validity,
    'Is the occupational pension''s replacement rate (the percentage of pre-retirement income it provides) accurate for the populations it actually serves, or systematized underestimate that preserves the fiction of adequacy?',
    'Longitudinal analysis of actual retirement income replacement for eligible vs ineligible cohorts; comparison of pre-retirement to post-retirement living costs; measurement of how frequently eligible beneficiaries supplement occupational pensions with additional income or asset drawdown; international comparison with countries using different pension structures',
    'If replacement rate is accurate and adequate: the occupational pension serves its coordination function genuinely. If systematically underestimated: the institution performs theater (maintaining legitimacy while delivering inadequate protection), shifting Piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(replacement_rate_validity, empirical, 'Validity of occupational pension replacement rate claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(occupational_pension_eligibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(occ_pens_tr_t0, occupational_pension_eligibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(occ_pens_tr_t15, occupational_pension_eligibility, theater_ratio, 15, 0.42).
narrative_ontology:measurement(occ_pens_tr_t30, occupational_pension_eligibility, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(occ_pens_be_t0, occupational_pension_eligibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(occ_pens_be_t15, occupational_pension_eligibility, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(occ_pens_be_t30, occupational_pension_eligibility, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(occupational_pension_eligibility, resource_allocation).
narrative_ontology:affects_constraint(occupational_pension_eligibility, labor_market_precarity).
narrative_ontology:affects_constraint(occupational_pension_eligibility, retirement_income_inequality).
narrative_ontology:affects_constraint(occupational_pension_eligibility, occupational_mobility_penalty).

% DUAL FORMULATION NOTE:
% The occupational pension eligibility constraint is downstream of labor market structure choices (employer preferences for contract flexibility) and upstream of retirement income inequality outcomes. The constraint family decomposes into: (1) labor_market_precarity — the choice to use contracts rather than permanent positions; (2) occupational_pension_eligibility — the rules that exclude contract workers from pensions; (3) retirement_income_inequality — the outcome of systematic exclusion. Each has its own ε value: precarity may be near-Mountain (structural feature of capital accumulation), pension eligibility is Tangled Rope (mixed coordination and extraction), retirement inequality is Snare (cumulative extraction over lifetime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(occupational_pension_eligibility, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
