% ============================================================================
% CONSTRAINT STORY: non_compete_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_non_compete_agreements, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: non_compete_agreements
 *   human_readable: Non-Compete Agreements (Post-Employment Restraint)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Non-compete agreements represent a structural extraction mechanism that
 *   has evolved significantly over three decades. Beginning as
 *   industry-specific tools (primarily in software and biotech) to protect
 *   genuine investments in proprietary knowledge and employee training,
 *   non-competes have metastasized into mass-market agreements imposed on
 *   low-wage workers (fast food franchise managers, hair stylists, warehouse
 *   supervisors) with minimal negotiating power and often zero legitimate
 *   protectable interest. The constraint exhibits the six-type perspectival
 *   structure: departing employees (powerless/trapped) experience pure snare;
 *   employers (institutional/arbitrage) experience coordination; the labor
 *   market aggregately experiences tangled extraction; legal enforcement is
 *   partially performative (piton); the pro-worker reform coalition
 *   (organized/mobile) sees a sunset mechanism in progress. The decreasing
 *   theater ratio (0.52→0.48) indicates that the constraint has become MORE
 *   functional (less performative) over time — not because enforcement got
 *   stronger, but because employers shifted from court-based enforcement
 *   (which involves rituals and reasonableness standards) to contractual
 *   threat and settlement dynamics, which require less theatrical
 *   justification. The rising extractiveness (0.35→0.58) reflects expansion
 *   from high-skill, high-wage workers (where legitimate interest arguments
 *   are stronger) to low-skill, low-wage workers (where protection rationales
 *   are weaker).
 *
 * KEY AGENTS:
 *   - Departing Employee: Primary victim (powerless/trapped) — bears full cost of restraint period; faces unemployment or forced relocation or loss of skill application
 *   - Incumbent Employer: Primary beneficiary (institutional/arbitrage) — captures value from restraint on competitive pressure and knowledge diffusion
 *   - Early-Career Professional: Secondary victim (moderate/constrained) — greater mobility than powerless agent but substantial opportunity costs
 *   - High-Mobility Professional: Secondary victim (powerful/mobile) — most exit capacity but still faces significant extraction if negotiating power is unequal
 *   - Competitive Labor Market: Aggregate victim (organized/constrained) — efficiency losses from reduced job mobility and wage competition
 *   - Legal Enforcement Apparatus: Institutional enforcer (institutional/constrained) — maintains performative regime through courts; declining theater indicates shift to contractual threat
 *   - Pro-Worker Reform Coalition: Organized beneficiary-of-sunset (organized/mobile) — includes labor advocates, portable benefits proponents, and state regulators building alternative regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(non_compete_agreements, 0.58).
domain_priors:suppression_score(non_compete_agreements, 0.72).
domain_priors:theater_ratio(non_compete_agreements, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(non_compete_agreements, extractiveness, 0.58).
narrative_ontology:constraint_metric(non_compete_agreements, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(non_compete_agreements, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(non_compete_agreements, snare).
narrative_ontology:human_readable(non_compete_agreements, "Non-Compete Agreements (Post-Employment Restraint)").
narrative_ontology:topic_domain(non_compete_agreements, "economic/political").

domain_priors:requires_active_enforcement(non_compete_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(non_compete_agreements, incumbent_employer).
narrative_ontology:constraint_victim(non_compete_agreements, departing_employee).
narrative_ontology:constraint_victim(non_compete_agreements, competitive_labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPARTING EMPLOYEE (SNARE) — Trapped between unemployment and contractual prohibition on using learned skills. Exit is illusory: signing the agreement is employment condition (coerced); leaving employment invokes the agreement (trapped). No alternative income path available within geographic scope. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EARLY-CAREER PROFESSIONAL (SNARE) — More mobility than powerless agent but severe constraints. Can relocate geographically or wait out time period, but opportunity cost is substantial (lost wages, career discontinuity). Restrictions most severe for industry-specialized roles. d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-MOBILITY PROFESSIONAL (SNARE) — Greater exit capacity (can relocate internationally, switch industries, negotiate waiver). But extraction still severe: either waives years of career advancement or pays substantial liquidated damages. Enforcement asymmetry favors employer with legal resources. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT EMPLOYER (ROPE) — Experiences constraint as coordination mechanism: protects genuine proprietary knowledge investment and training, enables planning around employee knowledge. Also provides extraction benefit (reduced competitive pressure). d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Negative effective extraction = strong beneficiary.
constraint_indexing:constraint_classification(non_compete_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: LABOR MARKET / COMPETITIVE EFFICIENCY (TANGLED ROPE) — Non-competes provide coordination benefit (allocative efficiency of training investment) but also extract through reduced job mobility and wage competition. Suppression is structural: departing workers cannot signal skills to competitors. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.50. Active enforcement (litigation threat) is required to maintain the mixed regime.
constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL ENFORCEMENT APPARATUS (PITON) — Non-compete enforcement is substantially performative. Courts struggle with reasonableness standards (duration, geography, legitimate protectable interest) and rarely grant full injunctions; settlement and threat dynamics do most enforcement work. Theater_ratio≈0.48 indicates moderate performative content — legal machinery is real but its actual restraining force is smaller than the formal contract suggests. Institutional inertia maintains the regime because alternatives (garden leave, liquidated damages, knowledge lockup) have their own costs.
constraint_indexing:constraint_classification(non_compete_agreements, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, some form of post-employment restraint appears inherent to any knowledge-intensive economy: employers must protect legitimate investments in training and proprietary knowledge, AND workers must have career mobility, AND these interests conflict. But this perspective risks naturalizing what is a contingent political choice about the balance between capital and labor property rights. The structural data (ε=0.58, suppression=0.72, theater=0.48) indicates this is NOT a mountain: the high suppression reveals active coercion, not natural law. False summit.
constraint_indexing:constraint_classification(non_compete_agreements, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: PRO-WORKER REFORM COALITION (SCAFFOLD) — Organized labor, portable benefits advocates, and some states (California) see non-compete agreements as a temporary regime being actively phased out through legal invalidation and alternative mechanisms (stake-in-enterprise, portable retirement, non-disparagement as substitute). d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28. Low effective extraction because coalition has agency and clear sunset path (legislative action, forum-shopping to worker-friendly jurisdictions).
constraint_indexing:constraint_classification(non_compete_agreements, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(non_compete_agreements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(non_compete_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(non_compete_agreements, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(non_compete_agreements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(non_compete_agreements, TR),
    TR >= 0.70.

:- end_tests(non_compete_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly through forced unemployment or relocation, lost wages, and career discontinuity. However, it is not maximum (0.72+) because: (1) some employers offer garden leave or waive enforcement; (2) workers can sometimes negotiate duration/geography; (3) high-mobility workers (powerful/mobile) have meaningful exit options; (4) geographic/temporal boundaries create natural decay. The extractiveness has risen from 0.35 to 0.58 over 30 years as non-competes expanded from high-skill/high-wage workers (where protection rationales are legitimate) to low-skill workers (where rationales are weak), increasing the extraction asymmetry. Suppression (0.72): High. Departing employees have severely limited alternatives: they cannot reveal they are seeking employment (signals covenant breach), cannot contact customers or colleagues (non-solicitation additions), and must either move or wait out the period. Legitimate alternatives (negotiating waiver, paying liquidated damages) are asymmetric in power. Theater ratio (0.48): Moderate. Non-compete enforcement involves substantial legal machinery (litigation, reasonableness standards, injunction procedures) but the actual restraining power is smaller than the formal contract suggests. Courts frequently modify agreements or refuse injunctions on reasonableness grounds. Real enforcement happens through threat and settlement dynamics, not through completed litigation. The declining theater (0.52→0.48) indicates employers have optimized away from court-based theater toward more efficient contractual threat, which requires less performative justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival divergence. Departing employees see pure snare: trapped, no coordination benefit, pure extraction. Employers see rope: coordination mechanism that enables training investment and knowledge protection. The labor market sees tangled rope: both coordination benefit (allocative efficiency) and extraction (reduced mobility and wage competition). The legal system sees piton: performative machinery (courts, reasonableness standards) masking a threat-based enforcement reality. The reform coalition sees scaffold: temporary regime being actively phased out through legislative action (California, executive orders), alternative mechanisms (portable benefits, stake-in-enterprise), and forum-shopping. The analytical observer risks seeing mountain: 'every knowledge economy needs some form of post-employment restraint' — but the structural data (high suppression, rising extractiveness, moderate theater) reveals this is a false summit. The naturalizing perspective confuses a contingent political choice about capital-labor property rights with an immutable economic law.
 *
 * DIRECTIONALITY LOGIC:
 *   Departing employee: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Early-career professional: Victim + constrained → d≈0.78, f(d)≈1.18. High extraction with some mobility. High-mobility professional: Victim + mobile → d≈0.55, f(d)≈0.75. Moderate extraction; exit options reduce but do not eliminate. Incumbent employer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; experiences constraint as coordination. Labor market aggregate: Mixed (coordination + extraction) + constrained → d≈0.60, f(d)≈0.85. Snare dynamics dominated but coordination function present. Legal enforcement: Institutional + constrained → d≈0.50, f(d)≈0.65 (piton classification comes from theater gate, not chi). Reform coalition: Organized + mobile → d≈0.45, f(d)≈0.48. Low effective extraction because coalition has agency and clear exit path.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing how the same structural phenomenon (post-employment restraint) is either a coordination mechanism or extraction depending entirely on the power asymmetry and exit options of the agent. For high-skill workers who negotiate favorable terms with employers who offer garden leave, the constraint is rope (genuine coordination). For low-wage workers coerced into signing and offered no alternative, it is snare (pure extraction). The SAME contractual mechanism produces different classifications from different positions. The resolution: the constraint cannot be classified as universally rope or universally snare. The correct statement is: 'Non-compete agreements are structured as snares for powerless agents and ropes for institutional beneficiaries.' The mandatrophy dissolves when we recognize that indexical classification captures this asymmetry — there is no single type, there is a perspectival presheaf. The analytical observer's mountain ('inevitable in knowledge economies') is a false summit that naturalizes the extractive asymmetry as if it were a law of nature. The structural data (high suppression, no natural emergence, contingent legal enforcement) refutes the mountain claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_protectable_interest_definition,
    'What qualifies as a legitimate protectable interest justifying post-employment restraint: trade secrets only, customer relationships, confidential processes, or competitive position itself?',
    'Comparative legal analysis across jurisdictions; empirical assessment of whether non-competes actually protect legitimate investments or function as pure rent extraction',
    'Narrow definition: non-competes classified as snare across more perspectives. Broad definition: more perspectives classify as rope (genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_protectable_interest_definition, conceptual, 'Definition of legitimate protectable interests under non-compete enforceability').

omega_variable(
    garden_leave_substitutability,
    'Does paid garden leave (wage continuation during non-compete period) substitute adequately for workers, making the extraction symmetric and converting the snare to a rope or tangled_rope?',
    'Comparison of wages during garden leave vs unemployment; analysis of whether garden leave is offered; study of worker welfare outcomes with vs without garden leave',
    'If garden leave is widely offered: constraint becomes more symmetrical (shifts toward tangled_rope). If rare: extraction remains asymmetric (remains snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(garden_leave_substitutability, empirical, 'Whether garden leave addresses worker harm from non-competes').

omega_variable(
    california_invalidation_effectiveness,
    'Does California''s near-total ban on non-competes (Business & Professions Code §16600) reduce extractiveness through forum-shopping and regulatory competition, or do employers find workarounds (liquidated damages, non-disparagement, restrictive covenants)?',
    'Longitudinal comparison of labor mobility and wages in California vs other states; analysis of litigation patterns post-ban; assessment of whether non-competes are replaced with functionally equivalent restrictions',
    'If ban is effective: non-compete constraint is ending (scaffold sunset is real). If workarounds prevalent: extraction shifts form but persists (snare endures under different names).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(california_invalidation_effectiveness, empirical, 'Effectiveness of non-compete invalidation in California').

omega_variable(
    trade_secret_leakage_empirical_risk,
    'What is the actual empirical rate of harmful trade secret disclosure by departing employees in industries with vs without enforceable non-competes?',
    'Litigation data analysis; industry surveys on actual competitive harm from employee departure; comparison of firms in non-compete jurisdictions vs non-enforceable jurisdictions',
    'If high leakage rate without non-competes: snare classification justified by genuine coordination need. If minimal: non-competes are rent extraction with weak coordination rationale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trade_secret_leakage_empirical_risk, empirical, 'Empirical rate of harmful trade secret disclosure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(non_compete_agreements, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nca_tr_t0, non_compete_agreements, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nca_tr_t15, non_compete_agreements, theater_ratio, 15, 0.5).
narrative_ontology:measurement(nca_tr_t30, non_compete_agreements, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(nca_be_t0, non_compete_agreements, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nca_be_t15, non_compete_agreements, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(nca_be_t30, non_compete_agreements, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(non_compete_agreements, resource_allocation).
narrative_ontology:affects_constraint(non_compete_agreements, mobility_wage_premium).
narrative_ontology:affects_constraint(non_compete_agreements, knowledge_asymmetry_extraction).
narrative_ontology:affects_constraint(non_compete_agreements, employment_at_will_doctrine).

% DUAL FORMULATION NOTE:
% Non-compete agreements are downstream of broader employment relationship structures (at-will employment, asymmetric information, capital-labor power dynamics) but represent a distinct constraint mechanism. The upstream constraint (employment_at_will_doctrine) creates the structural vulnerability that non-competes exploit; the downstream constraint (mobility_wage_premium) models the wage penalty for restraint. Non-competes bridge these two constraints by making the vulnerability mechanical and enforced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
