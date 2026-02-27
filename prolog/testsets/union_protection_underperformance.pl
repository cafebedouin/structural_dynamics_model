% ============================================================================
% CONSTRAINT STORY: union_protection_underperformance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_union_protection_underperformance, []).

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
 *   constraint_id: union_protection_underperformance
 *   human_readable: Just Cause Protection for Underperforming Union Employees
 *   domain: economic/labor
 *
 * SUMMARY:
 *   The Just Cause protection for union employees creates a structural
 *   tension between two legitimate institutional logics: (1) worker security
 *   against arbitrary termination and (2) employer operational efficiency and
 *   meritocratic differentiation. The constraint is a hybrid
 *   coordination-extraction mechanism that solves a real collective action
 *   problem (individual workers cannot bargain against concentrated employer
 *   power) while simultaneously creating moral hazard (insulating poor
 *   performance from discipline). This constraint exemplifies Tangled Rope
 *   precisely because both functions are structurally real and irreducible to
 *   one another. The classification varies sharply across perspectives:
 *   employers experience it as a Snare (trapped by procedural overhead), the
 *   union experiences it as Rope (pure coordination solving collective
 *   action), high performers experience it as mixed Tangled Rope
 *   (coordination benefit + asymmetric extraction), and consumers experience
 *   it as pure Snare (no voice, no exit, degraded service quality). The
 *   theater ratio (0.58) reflects substantial procedural formality in
 *   grievance arbitration that persists even as statutory employment law has
 *   evolved to provide some alternative protections. The extractiveness has
 *   increased from 0.35 to 0.52 over the 40-year interval as union density
 *   declined, concentrating protection in high-density sectors (public
 *   education, transit, public sector) where the coordination function has
 *   degraded relative to its extraction function.
 *
 * KEY AGENTS:
 *   - Protected Union Members (Underperforming): Primary beneficiary (institutional/arbitrage) — receive job security and due process protections without performance discipline
 *   - Union Leadership/Collective Bargaining Institutions: Primary beneficiary (institutional/arbitrage) — maintain institutional base through membership protection and grievance administration
 *   - Employer/Management: Primary victim (powerful/constrained) — trapped by procedural requirements that prevent efficient workforce optimization
 *   - High-Performing Coworkers: Secondary victim (moderate/constrained) — union-protected but experience asymmetric extraction through compression of merit differentiation
 *   - Service Consumers/End Customers: Tertiary victim (powerless/trapped) — degraded service quality from protected underperformance with no exit option
 *   - Competitive Non-Union Employers: Structural beneficiary (powerful/mobile) — gain competitive advantage in labor markets as union-organized competitors face efficiency penalties
 *   - Statutory Regulatory Framework: Institutional actor (institutional/constrained) — Title VII, FMLA, ADEA provide partial substitutes for Just Cause; grievance arbitration persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(union_protection_underperformance, 0.52).
domain_priors:suppression_score(union_protection_underperformance, 0.65).
domain_priors:theater_ratio(union_protection_underperformance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(union_protection_underperformance, extractiveness, 0.52).
narrative_ontology:constraint_metric(union_protection_underperformance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(union_protection_underperformance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(union_protection_underperformance, tangled_rope).
narrative_ontology:human_readable(union_protection_underperformance, "Just Cause Protection for Underperforming Union Employees").
narrative_ontology:topic_domain(union_protection_underperformance, "economic/labor").

domain_priors:requires_active_enforcement(union_protection_underperformance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(union_protection_underperformance, protected_union_members).
narrative_ontology:constraint_beneficiary(union_protection_underperformance, union_leadership).
narrative_ontology:constraint_victim(union_protection_underperformance, employer_operational_efficiency).
narrative_ontology:constraint_victim(union_protection_underperformance, high_performing_coworkers).
narrative_ontology:constraint_victim(union_protection_underperformance, consumer_service_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPLOYER/MANAGEMENT (SNARE) — Trapped by procedural requirements that prevent efficient workforce optimization. Management cannot rapidly separate underperforming workers without exhaustive documentation, progressive discipline, and grievance arbitration. The constraint suppresses alternatives to costly, time-consuming termination processes. While management retains ultimate separation power, the extraction (in the form of operational inefficiency and resource diversion to HR processes) is substantial and difficult to avoid.
constraint_indexing:constraint_classification(union_protection_underperformance, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION/COLLECTIVE BARGAINING INSTITUTIONS (ROPE) — Primary beneficiary. Just Cause provisions are pure coordination mechanisms solving a collective action problem: individual workers cannot negotiate employment security alone, but collective bargaining aggregates their power to demand due process protections. The union experiences this as coordination with low coercive overhead — the constraint aligns worker interests with union institutional survival and protects against arbitrary termination across the workforce.
constraint_indexing:constraint_classification(union_protection_underperformance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTECTED UNION MEMBER/UNDERPERFORMER (TANGLED ROPE) — Experiences hybrid coordination-extraction. Coordination benefit: the Just Cause clause protects against arbitrary termination and provides due process (legitimate value). But asymmetric extraction exists: the constraint insulates poor performance from consequences, creating moral hazard. The member bears no market discipline for underperformance while benefiting from procedural protections. Exit options are constrained — the member cannot exit the union without losing protection, and cannot exit employment without triggering the very termination process they joined to prevent. Mixed experience: genuine protection plus extraction from insulation from performance consequences.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: HIGH-PERFORMING COWORKER (TANGLED ROPE) — Also union-protected, but experiences asymmetric extraction. The constraint provides coordination benefit (job security, due process protection). But it suppresses meritocratic differentiation — high performers cannot easily capture larger pay increases or advancement relative to low performers. The coworker bears the cost of underperformer indiscipline (work redistribution, morale erosion) while the collective bargaining agreement treats all members identically. Suppression is significant: the union contract typically compresses wage bands and seniority rules prevent merit-based advancement. Constrained exit: leaving the union forfeits seniority and protection.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: SERVICE CONSUMER/END CUSTOMER (SNARE) — Powerless victim. When union protection insulates underperformance, service quality degrades (in service sectors, manufacturing, healthcare, education). The consumer has no exit option that avoids the constraint: they are trapped in the market served by the protected union workforce. The extraction is pure — degraded service quality with no corresponding benefit. No consumer has bargaining power to demand performance accountability. This perspective is most salient in public sector unions (public education, transit) where consumers cannot exit without relocating.
constraint_indexing:constraint_classification(union_protection_underperformance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: EMPLOYER WITH MARKET MOBILITY (SCAFFOLD) — Large employers with geographic or sectoral flexibility see Just Cause as a temporary coordination constraint with a sunset path. The constraint persists where union density is high (traditional manufacturing, public sector). But as union density declines nationally and employers shift to non-union or gig-economy workforces, the extraction mechanism weakens. Union-organized workplaces are increasingly confined to specific sectors and regions. Employers can exit through offshoring, automation, or geographic relocation to union-weak jurisdictions. The constraint has 'peak extraction' at high union density and declines as density falls. Estimated sunset: institutional erosion as union membership continues to decline from ~35% (1950s) to ~10% (current).
constraint_indexing:constraint_classification(union_protection_underperformance, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGACY LABOR REGULATION SYSTEM (PITON) — Traditional labor law and union grievance arbitration mechanisms persist largely through institutional inertia. The functional coordination role (preventing arbitrary termination) has been partly displaced by at-will employment + litigation risk, Title VII anti-discrimination enforcement, and FMLA protections that provide some due process without collective bargaining. The grievance arbitration theater persists: formal hearings, burden-of-proof procedures, and arbitrator selection retain ritualistic elements even where the underlying coordination function has been partially replaced by statutory frameworks. Theater ratio (0.58) reflects this: much of the formal grievance process is procedural theater maintained because alternative mechanisms haven't fully replaced it, not because arbitration alone is functionally superior to statutory protections.
constraint_indexing:constraint_classification(union_protection_underperformance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global view, the Just Cause constraint is a hybrid coordination-extraction mechanism. Coordination function: collective bargaining solves a real collective action problem (workers cannot negotiate individually against concentrated employer power). Extraction function: the constraint also insulates rent-seeking behavior (protection of low productivity without market discipline). The constraint persists because both functions are structurally real. It is not naturalized as immutable — the decline in union density shows it is contingent. But neither function is dominant: global labor markets show both high-protection (Nordic, some European) and low-protection (UK post-1980s reforms, US non-union sectors) equilibria coexisting, suggesting the constraint is designed institutional choice, not natural law or inevitable extraction.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_protection_underperformance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(union_protection_underperformance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_protection_underperformance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(union_protection_underperformance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(union_protection_underperformance, TR),
    TR >= 0.70.

:- end_tests(union_protection_underperformance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint creates genuine efficiency costs for employers (documented in grievance arbitration time, HR overhead, reduced management flexibility). But the value (0.52 vs. the initial omega-era estimate of 0.72) reflects that Just Cause also provides legitimate coordination benefits: without it, workers face atomized bargaining against concentrated employer power. The 0.52 reflects the empirical reality that both functions coexist — the constraint is not pure extraction. The increase from 0.35 to 0.52 over the interval reflects institutional drift: as union density declined from ~35% (1950s-1970s) to ~10% (current), Just Cause protection became concentrated in sectors (public sector, select manufacturing) where the coordination function has degraded relative to extraction. Suppression (0.65): High. Just Cause creates substantial suppression of alternatives to collective bargaining protection: (1) statutory employment law (Title VII, FMLA, ADEA) provide some substitutes but are sector-specific and require individual assertion; (2) market discipline through voluntary employer reputation/retention is suppressed by the protected status; (3) merit-based advancement and wage differentiation are suppressed by seniority rules and wage compression typical of union contracts. Theater ratio (0.58): Moderate. Grievance arbitration retains formal procedural elements (hearings, burden of proof, arbitrator selection) that are substantially theatrical — much of the process is about demonstrating procedural fairness rather than reaching functionally optimal decisions. However, the ratio is not as high as traditional hierarchical bureaucracies (0.70+) because arbitration does serve a real function (independent adjudication) and cannot be purely ceremonial without destroying the coordination benefit.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is driven by structural position (beneficiary vs. victim), exit options (arbitrage vs. trapped), and time horizon. Beneficiaries (union members, leadership) with institutional power and arbitrage exit view the constraint as coordination solving collective bargaining failure. Victims with constrained or trapped exit view the same constraint as extraction imposing efficiency costs or service degradation. The gap is irreducible: both perspectives are empirically correct about their structural positions. The gap widens over time as union density declines — in high-density sectors, Just Cause functions more as coordination (Rope); in low-density sectors, it becomes vestigial (Piton) or transitional (Scaffold).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is derived from base extractiveness (0.52), the agent's structural directionality (d), and scope modifiers. Beneficiaries (union members, union leadership) with protected institutional status and arbitrage exit options derive low d values (d ≈ 0.10-0.15), producing negative or minimal χ — they experience the constraint as beneficial. Victims with constrained exit (employers, high performers) or trapped status (customers) derive high d values (d ≈ 0.75-0.95), producing high χ values through the sigmoid function — they experience significant extraction. The employer's powerful status moderates d somewhat (d ≈ 0.65) because power provides some ability to navigate the constraint, but constrained exit options keep d high enough to yield moderate-high χ. High performers' moderate power and constrained union exit (leaving forfeits seniority and protection) yields intermediate d (d ≈ 0.60-0.65), producing moderate χ reflecting mixed experience. The customer's powerless status and trapped (cannot exit without relocating) yields maximum d (d ≈ 0.95), producing maximum χ via the sigmoid despite the customer being outside the labor agreement entirely — the extraction operates through service quality degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY: The constraint initially presents a classification ambiguity — is Just Cause pure extraction (Snare) or genuine coordination (Rope)? The resolution requires distinguishing the coordination function (solving atomized worker bargaining problem) from the extraction function (protecting underperformers from discipline). Both are structurally real. The Tangled Rope classification resolves the ambiguity by asserting that the constraint REQUIRES BOTH functions simultaneously: it provides genuine coordination benefits (due process, collective voice) that are irreducible to extractive rent-seeking, AND it creates asymmetric extraction (moral hazard insulating poor performers) that is irreducible to legitimate worker protection. Mandatrophy is NOT RESOLVED (base_properties.mandatrophy_resolved: false) because the underlying empirical question remains open: Which function dominates empirically in this constraint's current institutional context? (See omegas #1-3.) High-density union sectors (public education, transit, manufacturing) likely show Rope dominating (coordination function strong). Low-density sectors show Piton or degraded Tangled Rope (extraction function persists despite weakened coordination function). Resolving mandatrophy would require determining whether the observed underperformance protection reflects (a) legitimate coordination against employer power asymmetry, or (b) institutional rent-seeking by union leadership insulating members from market discipline. The evidence is mixed and domain-specific.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_measurement_validity,
    'How much of the ''underperformance'' subject to Just Cause protection reflects genuine individual productivity vs. measurement error, system design flaws, or external constraints?',
    'Comparative analysis of individual performance metrics before/after management system redesign; correlation between performance ratings and objective output measures (defect rates, throughput, customer satisfaction); audit of rating consistency across raters and time periods',
    'If measurement error > 40%: much ''protected underperformance'' reflects system failures, not individual extraction. Constraint reclassifies toward Scaffold (temporary coordination fix) or Rope (legitimate measurement problem). If measurement error < 20%: real individual productivity variance exists; protection may reflect genuine rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_measurement_validity, empirical, 'Validity of performance measurement systems').

omega_variable(
    market_discipline_alternative,
    'Would competitive labor markets + statutory anti-discrimination law (Title VII, FMLA, ADEA) provide equivalent or superior worker protection compared to Just Cause in the absence of collective bargaining?',
    'Comparative case analysis: private non-union employers in competitive labor markets with high turnover vs. unionized employers; worker litigation outcomes under Title VII vs. grievance arbitration outcomes; longitudinal wage and employment stability comparisons',
    'If statutory + market discipline is equivalent: Just Cause is a coordination redundancy, and the constraint reclassifies as Piton (vestigial). If statutory protection is inferior: Just Cause remains a genuine coordination good, and Rope/Tangled Rope persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_discipline_alternative, empirical, 'Whether statutory protections adequately replace collective bargaining').

omega_variable(
    morale_and_reciprocity_effects,
    'Does employment security (Just Cause) improve worker morale, engagement, and reciprocal performance, or does protection of underperformers reduce morale in high performers and destroy reciprocity norms?',
    'Experimental/quasi-experimental variation in Just Cause enforcement (within same sector, across similar worksites); measurement of absenteeism, workplace incident rates, voluntary turnover, and team productivity before/after Just Cause tightening or loosening; worker survey data on fairness perceptions',
    'If security improves morale/reciprocity: constraint is Rope (coordination benefit dominates). If protection of underperformers degrades morale: constraint is Snare or Tangled Rope (extraction dominates or mixes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morale_and_reciprocity_effects, empirical, 'Morale and reciprocity effects of employment security').

omega_variable(
    union_density_causality,
    'Does Just Cause protection causally sustain union density, or is declining union density independent of Just Cause enforcement? Would repealing Just Cause accelerate union decline?',
    'Cross-sectional analysis of union density vs. Just Cause strength across jurisdictions and time periods; instrumental variable analysis using exogenous variation in legal framework (e.g., state right-to-work laws); natural experiments from legislation changes',
    'If Just Cause causally sustains unionism: constraint removal would trigger institutional collapse; Scaffold sunset is real and accelerating. If density decline is independent: Just Cause is decoupling from its institutional base; Piton trajectory confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_density_causality, empirical, 'Causal relationship between Just Cause and union institutional sustainability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_protection_underperformance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ujc_tr_t0, union_protection_underperformance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ujc_tr_t20, union_protection_underperformance, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ujc_tr_t40, union_protection_underperformance, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(ujc_be_t0, union_protection_underperformance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ujc_be_t20, union_protection_underperformance, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(ujc_be_t40, union_protection_underperformance, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(union_protection_underperformance, enforcement_mechanism).
narrative_ontology:affects_constraint(union_protection_underperformance, wage_compression_union_sectors).
narrative_ontology:affects_constraint(union_protection_underperformance, public_sector_efficiency_erosion).
narrative_ontology:affects_constraint(union_protection_underperformance, gig_economy_emergence_as_exit_mechanism).

% DUAL FORMULATION NOTE:
% Just Cause protection operates at the junction of two distinct constraint families: (1) the labor bargaining asymmetry constraint (individual worker vs. concentrated employer power), which Just Cause partially addresses, and (2) the moral hazard constraint (how to maintain performance discipline while providing employment security). These are structurally separable — one could imagine labor protections without performance insulation, or performance discipline without atomized bargaining. The Tangled Rope classification arises because both functions are locked together in the current institutional design. Decomposition would require separate constraint stories for the bargaining asymmetry vs. the moral hazard, but the current empirical constraint conflates both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(union_protection_underperformance, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
