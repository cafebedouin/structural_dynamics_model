% ============================================================================
% CONSTRAINT STORY: federal_workplace_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_workplace_enforcement, []).

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
 *   constraint_id: federal_workplace_enforcement
 *   human_readable: Federal Workplace Enforcement Asymmetry
 *   domain: labor_policy/regulatory_enforcement
 *
 * SUMMARY:
 *   Federal workplace enforcement (NLRB, OSHA, DOL wage-and-hour) combines
 *   genuine coordination (minimum standards prevent race-to-the-bottom,
 *   protect smaller employers from undercutting) with asymmetric extraction
 *   (low-wage workers bear suppression costs; large employers arbitrage
 *   compliance; enforcement agencies gain legitimacy without full
 *   enforcement). The constraint exhibits Tangled Rope structure:
 *   beneficiaries include both large employers (who use enforcement as a
 *   market-entry barrier) and enforcement agencies (whose authority depends
 *   on the constraint's existence). Victims include low-wage workers
 *   (trapped, unable to organize due to suppression mechanisms) and worker
 *   collectives seeking to organize (constrained by legal barriers that frame
 *   'protections' as mandatory formal processes that delay and neutralize
 *   campaigns). The theater ratio (0.61) reflects that formal complaint and
 *   NLRB election mechanisms produce performative outputs (closed cases,
 *   settlements, certified elections) without systematic verification that
 *   workplace conditions actually change. Extractiveness has risen from 0.38
 *   to 0.52 over the 40-year interval, indicating that coordination benefits
 *   have been outpaced by extraction layering: outsourcing,
 *   misclassification, and gig work growth have expanded the pool of workers
 *   outside enforcement reach while formal protections have narrowed.
 *
 * KEY AGENTS:
 *   - Low-wage workers: Primary victims (powerless/trapped) — job dependence and retaliation risk prevent exit or complaint
 *   - Worker organizing collectives: Secondary victims (moderate/constrained) — NLRA framing as 'protection' actually channels organizing into delays and reduces mobilization capacity
 *   - Large multinationals: Primary beneficiaries (powerful/mobile) — enforcement rules standardize labor costs and create compliance barriers to entry for competitors
 *   - Small/mid-size employers: Mixed (powerful/constrained) — benefit from large-employer constraint but face higher relative compliance burden
 *   - Enforcement agencies (NLRB/OSHA/DOL): Institutional beneficiaries (institutional/arbitrage) — authority and budgets justified by enforcement regime; set enforcement priorities; experience minimal external accountability
 *   - Formal complaint mechanisms: Theater performers (institutional/arbitrage) — processing complaints and producing settlements without systematic compliance verification
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees the constraint as coordinating baseline protections while extracting worker agency through formalization and outsourcing dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_workplace_enforcement, 0.52).
domain_priors:suppression_score(federal_workplace_enforcement, 0.68).
domain_priors:theater_ratio(federal_workplace_enforcement, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_workplace_enforcement, extractiveness, 0.52).
narrative_ontology:constraint_metric(federal_workplace_enforcement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federal_workplace_enforcement, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_workplace_enforcement, tangled_rope).
narrative_ontology:human_readable(federal_workplace_enforcement, "Federal Workplace Enforcement Asymmetry").
narrative_ontology:topic_domain(federal_workplace_enforcement, "labor_policy/regulatory_enforcement").

domain_priors:requires_active_enforcement(federal_workplace_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_workplace_enforcement, large_employers).
narrative_ontology:constraint_beneficiary(federal_workplace_enforcement, enforcement_agencies).
narrative_ontology:constraint_victim(federal_workplace_enforcement, low_wage_workers).
narrative_ontology:constraint_victim(federal_workplace_enforcement, worker_collective_organizing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER (SNARE) — Structurally trapped: job dependence precludes exit; suppression through at-will employment doctrine; minimal accessible complaint mechanisms; retaliation risk. Enforcement regime extracts compliance (wage theft, unsafe conditions tolerated) with no reciprocal protection.
constraint_indexing:constraint_classification(federal_workplace_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKER COLLECTIVE (TANGLED ROPE) — Constrained by organizing costs, legal barriers, and employer surveillance. Also benefits from enforcement regime: NLRA protections (when enforced), wage-and-hour standards, safety rules. Mixed extraction: the enforcement mechanism simultaneously protects and constrains collective action.
constraint_indexing:constraint_classification(federal_workplace_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENFORCEMENT AGENCY (ROPE) — Experiences enforcement regime as coordination mechanism: standardized rules enable nationwide labor market consistency. Arbitrage exit (can shift enforcement priorities within statutory mandate). Net beneficiary of the constraint: enforcement authority validates institutional role and budget.
constraint_indexing:constraint_classification(federal_workplace_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE EMPLOYER (ROPE) — Mobile exit options (regulatory arbitrage across jurisdictions, offshoring, classification restructuring). Enforcement regime is coordination that standardizes labor costs across competitors. Net beneficiary: high compliance costs are barriers to entry that protect market position.
constraint_indexing:constraint_classification(federal_workplace_enforcement, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SMALL EMPLOYER (TANGLED ROPE) — Constrained: cannot offshore or restructure as easily as multinational; higher compliance burden relative to revenue. Also benefits: enforcement against wage theft competitors protects their reputation and market share. Asymmetric extraction relative to size.
constraint_indexing:constraint_classification(federal_workplace_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: FORMAL COMPLAINT MECHANISM (PITON) — Theater ratio 0.61: workers file complaints at low rates despite violations; agencies conduct investigations that face political pressure to close without substantive remedy; settlements are performative (tiny penalties, no admission). The formal mechanism persists through institutional inertia despite low verification of actual compliance change.
constraint_indexing:constraint_classification(federal_workplace_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Federal enforcement coordinates minimum labor standards (prevents race-to-the-bottom) while extracting enforcement legitimacy. Suppression mechanisms (at-will doctrine, retaliation barriers, cost of organizing) block worker exit. System exhibits genuine coordination (enforceable wage floors, safety standards) alongside asymmetric extraction (low-wage workers bear suppression costs; large employers arbitrage compliance).
constraint_indexing:constraint_classification(federal_workplace_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_workplace_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_workplace_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_workplace_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_workplace_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_workplace_enforcement, TR),
    TR >= 0.70.

:- end_tests(federal_workplace_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint genuinely coordinates baseline standards (wage floors, safety minimums prevent undercutting) but extraction mechanisms have intensified: outsourcing and misclassification exclude growing worker populations from formal enforcement; settlement amounts have not kept pace with violation severity; political cycles have reduced enforcement budgets relative to private sector workforce growth. The base extractiveness increased from 0.38 (1980s peak enforcement relative to workforce) to 0.52 (2020s, enforcement capacity declining while violation opportunities expand). Suppression (0.68): High. Multiple layers: at-will employment doctrine eliminates most retaliation protection; NLRA election process delays (6–12 months typical) allow employer counter-campaigns; fee-shifting and discovery rules favor employer litigation; fear of visa sponsorship loss suppresses complaints from documented immigrants; identity fusion with employer role suppresses conscious complaint in family-owned businesses. Theater ratio (0.61): Moderate-high. NLRB elections produce certified outcomes (performative victory) but not workplace conditions change; OSHA inspections are rare (once per 100 years per facility in many sectors) and penalties are tiny relative to violation cost; DOL wage-and-hour settlements involve penalties that are expenses, not deterrents. The theater has risen from 0.45 (1980s, when enforcement was higher relative to violations) to 0.61 (2020s, when formal mechanisms produce outcomes without effectiveness verification).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (large employer) sees Rope: enforcement standardizes labor costs, preventing competitors from undercutting through wage suppression, and creates barriers to entry. The victim (low-wage worker) sees Snare: exit options are blocked by job dependence, retaliation risk, and outsourcing that removes their job entirely if they organize. The enforcement agency sees Rope: their authority is justified and their budgets are allocated via the enforcement mandate. The worker collective sees Tangled Rope: the formal NLRA machinery offers theoretical protections (genuine coordination benefit) while simultaneously containing and delaying organizing campaigns through mandatory procedures (extraction). The analytical observer sees the same phenomenon: coordination (standardized floors prevent race-to-bottom) and extraction (workers bear suppression costs, large employers capture regulatory benefits through structural asymmetry).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary by agent's structural position. Low-wage workers: trapped exit + victim status → d ≈ 0.92 → high f(d) ≈ 1.38 → experience strong effective extraction despite moderate base ε. Large employers: mobile exit + beneficiary status → d ≈ 0.12 → low f(d) ≈ 0.08 → experience negative/minimal effective extraction (the constraint subsidizes them through market-entry barriers). Enforcement agencies: arbitrage exit + beneficiary status → d ≈ 0.05 → f(d) ≈ -0.10 → experience institutional subsidy from the constraint's existence. Small employers: constrained exit + mixed benefit/cost → d ≈ 0.48 → f(d) ≈ 0.60 → experience moderate extraction asymmetric to size. Worker collectives: constrained exit + victim status → d ≈ 0.72 → f(d) ≈ 1.15 → experience significant extraction despite coordination benefits from formal protections.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that federal enforcement is legitimately Tangled Rope: it contains both genuine coordination (wage floors, safety minimums, protection against retaliation in theory) and asymmetric extraction (enforcement gaps, outsourcing routes, formal procedure delays that suppress organizing). The false summit risk is the analytical mountain classification: 'Federal minimum wage is a natural law of labor markets' or 'Enforcement necessarily creates extraction.' This naturalizes a contingent institutional arrangement. The actual structure: enforcement coordination is real, extraction mechanisms are real, and both grow from the same institutional design. The mandatrophy is resolved by declaring both beneficiaries (large employers gaining cost-standardization and market-entry barriers; agencies gaining authority) and victims (low-wage workers bearing suppression costs; organizing collectives bearing formalization delays), which forces Tangled Rope over either pure Rope or pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_versus_intent,
    'Does low enforcement (penalties, settlement rates) reflect resource constraints or structural preference for lenient treatment of employers?',
    'Comparative analysis: budget appropriations for enforcement agencies vs statutory authority; comparison of settlement amounts to documented violation costs; audit of agency prosecutorial discretion applied to similar violations across employer size classes.',
    'If resource-constrained: constraint reclassifies to Scaffold with sunset clause (increased funding would resolve). If structural preference: constraint is pure Snare extraction (system is deliberately underfunded to protect employers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_versus_intent, empirical, 'Whether low enforcement reflects capacity limits or structural preference').

omega_variable(
    retaliation_suppression_mechanism,
    'Is suppression of worker complaints driven by at-will employment doctrine (structural legal barrier) or by internalized fear of retaliation (identity-locked via identity_locked exit option)?',
    'Field studies of worker decision-making: survey workers in states with at-will vs wrongful termination statutes and track complaint rates; analyze complaint patterns post-legal reform; examine whether workers persist in internalized suppression despite legal protections.',
    'If structural: trapped classification is appropriate. If internalized: identity_locked exit option captures the binding mechanism better — workers are structurally mobile but feel unable to exit due to identity fusion with employer or internalized fear despite legal protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_suppression_mechanism, empirical, 'Whether suppression is structural (legal barriers) or internalized (identity-locked)').

omega_variable(
    collective_organizing_coordination_versus_extraction,
    'Does federal labor law (NLRA protections, unfair labor practice enforcement) genuinely enable worker organizing or does it function primarily to channel grievances into formal mechanisms that prevent effective mobilization?',
    'Historical analysis of organizing success rates before/after NLRA; comparison of successful organizing (card check vs NLRB elections); study of election delays and their impact on campaign viability; examination of injunction patterns.',
    'If genuinely enabling: Tangled Rope classification correct (mixed coordination and extraction). If primarily channeling: reclassifies toward Snare (the ''protection'' mechanism is a suppression mechanism in disguise).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_organizing_coordination_versus_extraction, empirical, 'Whether NLRA enables or suppresses worker organizing').

omega_variable(
    employer_arbitrage_scope,
    'What fraction of workplace violations are addressable through federal enforcement versus outsourced via subcontracting, misclassification, and temporary worker stratification that federal enforcement cannot reach?',
    'Sectoral audit of misclassified workers; comparison of violation rates in regulated sectors vs outsourced/subcontracted chains; analysis of statutory gaps (temp agencies, gig platforms) and their prevalence in violation-prone industries.',
    'If large fraction outsourced: suppression is higher than measured (workers exit compliance regime entirely rather than face formal enforcement); constraint reclassifies as more extractive. If small fraction: suppression metric is approximately accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_arbitrage_scope, empirical, 'Fraction of workplace violations outside federal enforcement reach').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_workplace_enforcement, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fwe_tr_t0, federal_workplace_enforcement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fwe_tr_t20, federal_workplace_enforcement, theater_ratio, 20, 0.58).
narrative_ontology:measurement(fwe_tr_t40, federal_workplace_enforcement, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(fwe_be_t0, federal_workplace_enforcement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fwe_be_t20, federal_workplace_enforcement, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(fwe_be_t40, federal_workplace_enforcement, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_workplace_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_workplace_enforcement, gig_economy_misclassification).
narrative_ontology:affects_constraint(federal_workplace_enforcement, subcontracting_liability_avoidance).
narrative_ontology:affects_constraint(federal_workplace_enforcement, union_election_delay_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_workplace_enforcement, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
