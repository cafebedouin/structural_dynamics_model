% ============================================================================
% CONSTRAINT STORY: 1994_clinton_family_medical_leave_act
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1994_clinton_family_medical_leave_act, []).

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
 *   constraint_id: 1994_clinton_family_medical_leave_act
 *   human_readable: Unpaid Leave Guarantee for Workers During Family and Medical Emergencies
 *   domain: labor/employment_regulation
 *
 * SUMMARY:
 *   The Family and Medical Leave Act (FMLA) of 1993, implemented in 1995,
 *   guarantees US workers 12 weeks of unpaid, job-protected leave annually
 *   for specified family and medical events (birth, adoption, serious health
 *   conditions, family member illness, military family leave). The constraint
 *   operates as a reframing: it transforms family care from an individual
 *   hardship absorbed outside the labor market into an institutional problem
 *   that the labor market must accommodate. This reframing redistributes
 *   costs from workers (who previously lost wages or jobs when facing family
 *   emergencies) to employers (who absorb temporary workforce disruption).
 *   The constraint exhibits a perspectival gradient: low-income workers
 *   without savings cannot afford unpaid leave and experience extraction
 *   (snare); middle-income workers can absorb wage loss and experience mixed
 *   coordination-extraction (tangled rope); large employers benefit from
 *   predictable policies and experience coordination (rope); small employers
 *   face disproportionate disruption and experience extraction (snare);
 *   unions see opportunity and obligation (tangled rope); labor department
 *   enforcement is performative (piton); paid leave advocates see this as
 *   temporary scaffolding toward wage-replacement systems. The rising theater
 *   ratio (0.25 → 0.32) reflects that nominal leave protections diverge
 *   increasingly from effective protection — retaliation risk and wage loss
 *   create an enforcement gap between what the law promises and what workers
 *   actually experience.
 *
 * KEY AGENTS:
 *   - Low-income workers without savings: Primary victim (powerless/trapped) — cannot afford unpaid leave despite guarantee; face disproportionate retaliation risk
 *   - Middle-income workers with savings: Secondary beneficiary (moderate/constrained) — can use leave but experience career damage and retaliation risk
 *   - Large corporate employers: Primary beneficiary (institutional/arbitrage) — leverage leave policies for workforce planning and compliance; reduce HR administrative complexity
 *   - Small business owners: Primary victim (moderate/constrained) — absorb workforce disruption without slack; high extraction relative to firm size
 *   - Labor unions and worker advocacy: Organized actors (organized/constrained) — advance worker protection but become responsible for enforcement and retaliation prevention
 *   - Labor department enforcement: Institutional maintenance (institutional/arbitrage) — maintain regulatory apparatus with performative compliance checking; limited capacity for retaliation detection
 *   - Paid family leave advocates: Organized agents (organized/constrained) — view unpaid leave as transitional; building state-level paid leave systems as sunset mechanism
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — sees leave guarantee as genuine institutional reframing of care work; core coordination function intact despite extraction layers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1994_clinton_family_medical_leave_act, 0.38).
domain_priors:suppression_score(1994_clinton_family_medical_leave_act, 0.48).
domain_priors:theater_ratio(1994_clinton_family_medical_leave_act, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1994_clinton_family_medical_leave_act, extractiveness, 0.38).
narrative_ontology:constraint_metric(1994_clinton_family_medical_leave_act, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1994_clinton_family_medical_leave_act, theater_ratio, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1994_clinton_family_medical_leave_act, tangled_rope).
narrative_ontology:human_readable(1994_clinton_family_medical_leave_act, "Unpaid Leave Guarantee for Workers During Family and Medical Emergencies").
narrative_ontology:topic_domain(1994_clinton_family_medical_leave_act, "labor/employment_regulation").

domain_priors:requires_active_enforcement(1994_clinton_family_medical_leave_act).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1994_clinton_family_medical_leave_act, workers_with_caregiving_obligations).
narrative_ontology:constraint_beneficiary(1994_clinton_family_medical_leave_act, vulnerable_workers_low_income).
narrative_ontology:constraint_victim(1994_clinton_family_medical_leave_act, employers_small_business).
narrative_ontology:constraint_victim(1994_clinton_family_medical_leave_act, labor_market_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKER (SNARE) — Despite the leave guarantee, the worker is trapped: unpaid leave means loss of wages they cannot absorb, and taking leave creates visibility risk and career damage. The constraint nominally protects job security but extracts through wage loss and employer resentment. Maximum suppression — worker cannot afford the leave even though it is guaranteed.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME WORKER WITH SAVINGS (TANGLED ROPE) — Experiences genuine coordination: can actually use the leave without economic catastrophe. Also experiences extraction: workplace visibility damage, retaliation risk, reduced promotion likelihood for taking extended leave. The constraint both enables and constrains — real protection alongside real cost.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE CORPORATE EMPLOYER (ROPE) — Benefits from the constraint as coordination mechanism: predictable leave policies enable workforce planning, reduce ad-hoc negotiations, create uniform treatment reducing discrimination liability. Large firms can absorb workforce disruption; the leave guarantee actually reduces HR administrative complexity. Net beneficiary through arbitrage — lower compliance risk and operational predictability than discretionary systems.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SMALL BUSINESS OWNER (SNARE) — Cannot exit: employer mandate applies regardless of business size. Extraction is asymmetric: small firms absorb workforce disruption costs with minimal slack and no administrative budget for compliance. Cannot arbitrage like large firms. Suppression is high — business continuity is genuinely threatened by leave-taking in thin-staffed operations.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LABOR UNIONS (TANGLED ROPE) — Organized agents see genuine coordination: the leave mandate sets a floor, enabling union negotiation for additional benefits. But also experience extraction: unions become responsible for preventing employer retaliation and maintaining enforcement, creating administrative burden. The constraint both advances and constrains union activity.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR DEPARTMENT ENFORCEMENT (PITON) — Maintains performative compliance checking: most violations (retaliation, wage theft) are structurally difficult to prove and underenforced. The regulatory apparatus persists through institutional momentum rather than effective verification. Theater ratio is high because enforcement gap between nominal guarantee and actual protection is wide.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: PAID FAMILY LEAVE MOVEMENT (SCAFFOLD) — See unpaid leave as temporary coordination layer with a sunset: several US states are implementing paid family leave (CA, NJ, NY, CT, MA), which would replace unpaid leave with wage-replacement. The unpaid leave mandate is transitional, expected to be superseded by paid leave systems. Low theater because the mechanism is genuinely functional — it protects most workers most of the time.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, the leave guarantee solves a genuine coordination problem: how to allocate family care within market economies. The constraint reframes care as a social coordination problem (requiring leave infrastructure) rather than as individual hardship. Low extractiveness because the primary function is coordination — the distribution of care work across time.
constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1994_clinton_family_medical_leave_act_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1994_clinton_family_medical_leave_act, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1994_clinton_family_medical_leave_act, TR),
    TR >= 0.70.

:- end_tests(1994_clinton_family_medical_leave_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, between coordination-only (rope ~0.35) and pure extraction (snare ~0.60). The constraint solves a real coordination problem (distributing caregiving work across labor markets) but creates asymmetric costs. Large employers benefit from predictability; small employers absorb disproportionate disruption; workers gain job security but lose wages. The base value reflects that coordination is primary but extraction is substantial. The rising trajectory (0.28 → 0.38) indicates increasing gap between nominal and effective protection as retaliation dynamics strengthen over time. Suppression (0.48): Moderate-high. Multiple barriers constrain actual leave usage: wage loss for low-income workers, retaliation risk, employer resentment, workplace visibility costs, promotion penalty. The suppression reflects that the legal guarantee does not eliminate material constraints on exercise. Suppression is structural (external barriers) not internalized. Theater ratio (0.32): Low-moderate, indicating that the mechanism is substantially functional but contains performative elements. The leave guarantee is real and does protect many workers; the theater derives from retaliation that occurs *after* the leave is taken (not during application) and from enforcement gaps. The rising trajectory reflects that theater is increasing as retaliation becomes more sophisticated and harder to detect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival differentiation driven by income level and firm size. The income stratification is critical: the same nominal protection (12 weeks job security) produces snare classification for workers without wage-replacement savings but tangled_rope or rope for workers with savings. This is not a different constraint viewed from different angles — it is the same constraint producing different extractiveness values for different agents based on their ability to absorb wage loss. The firm-size stratification is similarly critical: large employers experience the constraint as coordination (rope) because they have administrative capacity and workforce slack; small employers experience extraction (snare) because they lack both. The enforcement gap (performative retaliation detection) means that the nominal leave protection diverges from effective protection, creating a theater ratio that should be lower than it is. The piton classification of labor department enforcement reflects that the agency maintains the regulatory apparatus through institutional inertia despite limited actual retaliation detection capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint creates asymmetric extraction primarily through wage loss and retaliation risk, not through denial of leave itself. The leave is granted (that is the coordination component) but the cost of taking it is borne asymmetrically: workers earning below median income lose wages they cannot absorb; workers with savings can absorb the loss. Small employers face involuntary workforce disruption; large employers can plan around it. The beneficiary structure is institutional (employers, especially large ones) and class-stratified (workers with savings vs. without). The victim structure is similarly stratified: low-income workers are primary victims; small businesses are secondary victims; the labor market's continuity is a tertiary victim (minor). The extraction mechanism is not coercive (workers are not forced to take leave) but is extractive through cost asymmetry (the benefit is identical but the cost differs wildly by income). Retaliation is the enforcement mechanism maintaining extraction — it suppresses the effective exercise of the right by making visibility costly. The derivation chain correctly produces different d values for different income groups and firm sizes, which is why the perspectival gap is both real and persistent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled rope (not pure coordination and not pure extraction) and that the extractiveness is distributed unequally across income groups and firm sizes. The constraint solves a real coordination problem: how do labor markets accommodate caregiving obligations without forcing workers to choose between employment and family emergencies? The solution — job-protected leave — is a coordination innovation. But the solution creates asymmetric costs because it is unpaid leave, which means wage loss is the cost of exercising the right. For workers without savings, the wage loss is extractive; for workers with savings, it is a manageable cost. The tangled rope classification holds at the societal level, but the perspectival gradient (snare for powerless, rope for beneficiaries) reveals that the coordination is real but the extraction is real and stratified. The rising theater ratio indicates that the extraction is increasingly hidden behind performative retaliation (hard to prove, hard to prosecute). The paid leave transition (state-level wage-replacement systems) would reduce extractiveness by eliminating wage loss, potentially shifting the overall classification from tangled rope toward rope. Until that transition, the constraint remains extractive for low-income workers despite its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unpaid_wage_loss_mitigation,
    'How much do unpaid leave workers actually lose in wages, and what fraction of workers can absorb this loss without economic harm?',
    'Survey data on leave-taking rates by income quintile; wage loss calculations; correlation between leave eligibility and actual leave usage by income group; longitudinal tracking of health, caregiving completion, and economic stress post-leave',
    'If high-income workers dominate leave usage while low-income workers cannot afford it: the constraint is extractive for the powerless (Snare classification correct). If low-income workers do use leave successfully: constraint functions as rope/tangled_rope. The empirical distribution drives classification accuracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unpaid_wage_loss_mitigation, empirical, 'Wage loss absorption capacity across income groups').

omega_variable(
    employer_retaliation_prevalence,
    'What fraction of workers who take leave experience retaliation, demotion, or reduced advancement, and what is the effective magnitude of this cost relative to wage loss?',
    'Longitudinal wage/promotion tracking comparing workers who take leave vs. matched controls; survey data on retaliation experiences; litigation records for retaliation claims; correlation between leave-taking and career trajectories',
    'If retaliation is rare and low-cost: the constraint functions as intended (Rope/Tangled Rope). If retaliation is common and significant: the constraint''s nominal protection is largely theater, and the effective extraction is high (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_retaliation_prevalence, empirical, 'Prevalence and magnitude of employer retaliation for leave-taking').

omega_variable(
    small_business_viability_impact,
    'Do small businesses (< 50 employees) experience measurable workforce disruption, revenue loss, or closure rates attributable to leave-mandate compliance?',
    'Business closure rate trends pre/post FMLA; survival analysis controlling for confounders; small-business owner surveys on leave-mandate burden; administrative cost studies for compliance',
    'If impacts are substantial: small business extraction is real (Snare classification for small employers confirmed). If impacts are minimal: small employers experience the same rope coordination benefits as large firms. The firm-size effect on extractiveness is critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_business_viability_impact, empirical, 'Small business viability impact of leave mandates').

omega_variable(
    paid_leave_substitution_timeline,
    'What is the realistic timeline for paid family leave to replace unpaid leave across the US labor market, and does the unpaid leave guarantee accelerate or impede paid leave adoption?',
    'State-level paid leave policy diffusion analysis; legislative tracking; cost-benefit analyses of state paid leave programs; correlation between unpaid leave coverage and paid leave adoption rates',
    'If paid leave is likely (10-20 years): scaffold classification is correct and unpaid leave is genuinely temporary. If paid leave stalls: unpaid leave becomes a long-term extraction mechanism (Snare). The sunset timeline determines whether this is scaffold or entrenched snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paid_leave_substitution_timeline, preference, 'Realistic timeline and likelihood of paid family leave substitution').

omega_variable(
    enforcement_capacity_retaliation_detection,
    'Can the labor department actually detect and prosecute employer retaliation for leave-taking, or is enforcement structurally limited to obvious violations?',
    'Retaliation complaint/enforcement rates vs. estimated prevalence; case outcomes; compliance audit coverage; analysis of enforcement agency capacity and budget',
    'If enforcement is effective: piton classification is incorrect and the constraint functions as intended. If enforcement is structurally limited (retaliation is hard to prove, resources are scarce): the constraint is largely performative and piton/snare classifications are correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_retaliation_detection, empirical, 'Labor department enforcement capacity for retaliation detection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1994_clinton_family_medical_leave_act, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(leave_tr_t0, 1994_clinton_family_medical_leave_act, theater_ratio, 0, 0.25).
narrative_ontology:measurement(leave_tr_t5, 1994_clinton_family_medical_leave_act, theater_ratio, 5, 0.3).
narrative_ontology:measurement(leave_tr_t10, 1994_clinton_family_medical_leave_act, theater_ratio, 10, 0.32).

% Extraction over time
narrative_ontology:measurement(leave_be_t0, 1994_clinton_family_medical_leave_act, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(leave_be_t5, 1994_clinton_family_medical_leave_act, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(leave_be_t10, 1994_clinton_family_medical_leave_act, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1994_clinton_family_medical_leave_act, resource_allocation).
narrative_ontology:affects_constraint(1994_clinton_family_medical_leave_act, workplace_flexibility_negotiation).
narrative_ontology:affects_constraint(1994_clinton_family_medical_leave_act, maternal_mortality_access).
narrative_ontology:affects_constraint(1994_clinton_family_medical_leave_act, childcare_market_structure).

% DUAL FORMULATION NOTE:
% Unpaid leave guarantee coordinates family care with labor market participation but creates asymmetric wage-loss costs. Upstream constraint: the patriarchal division of care work that makes family obligations a labor-market crisis (solved by leave coordination). Downstream constraints: maternal mortality/health outcomes improved by leave access; childcare market structure changed by leave availability; workplace flexibility negotiation patterns altered by leave norms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1994_clinton_family_medical_leave_act, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
