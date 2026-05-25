% ============================================================================
% CONSTRAINT STORY: institutional_path_dependence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_path_dependence, []).

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
 *   constraint_id: institutional_path_dependence
 *   human_readable: Institutional Path Dependence and Lock-in
 *   domain: institutional_analysis/organizational_behavior
 *
 * SUMMARY:
 *   Institutional path dependence is the structural constraint that locks
 *   organizations into established procedures, hierarchies, and
 *   decision-making frameworks even when alternatives would be more efficient
 *   or adaptive. This constraint arises from the combination of sunk costs,
 *   employee specialization in incumbent systems, ecosystem
 *   complementarities, and the coordination benefits of standardized
 *   procedures. What begins as rational coordination (standardized procedures
 *   reduce transaction costs) crystallizes into lock-in as alternatives
 *   become structurally unavailable and internal actors become identity-fused
 *   with incumbent roles. The constraint exhibits all six DR types depending
 *   on the observer's structural position: an immutable natural law (from the
 *   civilizational analytical perspective), a coordination mechanism (from
 *   the institutional incumbent perspective), a temporary bottleneck being
 *   disrupted by technological change (from the disruptive coalition
 *   perspective), a degraded ritual maintained through cultural inertia (from
 *   the bureaucratic perspective), mixed coordination-extraction (from
 *   mid-level managers), and pure extraction constraining reform capacity
 *   (from powerless internal reformers). The extractiveness value (0.52)
 *   reflects that path dependence generates asymmetric benefits: incumbents
 *   and established procedures benefit from continuation; alternative
 *   institutional designs and agents seeking adaptation bear costs. The
 *   suppression value (0.65) reflects high barriers to institutional change:
 *   sunk costs in training and specialization, credential systems that lock
 *   individuals to incumbent roles, ecosystem dependencies, and identity
 *   fusion with organizational roles.
 *
 * KEY AGENTS:
 *   - Incumbent Organizations: Primary beneficiary (institutional/arbitrage) — capture first-mover advantages, ecosystem lock-in, and supplier/employee lock-in; can exit procedures at low cost but benefits prevent exit
 *   - Trapped Reformers: Primary victim (powerless/trapped) — internal agents seeking institutional change face coordination failures, career risk, and role identity constraints that make exit economically infeasible
 *   - Mid-Level Managers: Secondary victim/beneficiary (moderate/constrained) — experience genuine coordination benefits but face high costs of deviation; can exit at significant cost
 *   - Disruptive Coalition: Organized agents (organized/constrained) — new entrants and reform groups see path dependence as temporary, expecting technological discontinuities to break incumbent lock within technology cycles
 *   - Ceremonial Bureaucracy: Institutional actor (institutional/arbitrage) — perpetuates procedures through cultural inertia and role identity; maintains appearance of incumbent systems as functional
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as irreversible laws of organizational evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_path_dependence, 0.52).
domain_priors:suppression_score(institutional_path_dependence, 0.65).
domain_priors:theater_ratio(institutional_path_dependence, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_path_dependence, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_path_dependence, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_path_dependence, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_path_dependence, tangled_rope).
narrative_ontology:human_readable(institutional_path_dependence, "Institutional Path Dependence and Lock-in").
narrative_ontology:topic_domain(institutional_path_dependence, "institutional_analysis/organizational_behavior").

domain_priors:requires_active_enforcement(institutional_path_dependence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_path_dependence, incumbent_organizations).
narrative_ontology:constraint_beneficiary(institutional_path_dependence, established_procedures).
narrative_ontology:constraint_victim(institutional_path_dependence, alternative_institutional_designs).
narrative_ontology:constraint_victim(institutional_path_dependence, adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED REFORMER (SNARE) — Internal agents who perceive the institution's constraints as immutable and inescapable. Career paths, pension obligations, credential systems, and role identity lock them into perpetuating existing procedures. No viable exit exists; reform attempts face coordination problems and face institutional inertia. Maximum extraction: forced to maintain dysfunctional systems or abandon livelihood.
constraint_indexing:constraint_classification(institutional_path_dependence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Genuine coordination function: standardized procedures reduce transaction costs and enable scale. Simultaneous asymmetric extraction: sunk training costs, knowledge specific to incumbent systems, and career risk of deviation create high exit barriers. Can exit at significant cost; cannot exit without cost. Active enforcement through performance metrics tied to incumbent procedures.
constraint_indexing:constraint_classification(institutional_path_dependence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT ORGANIZATION (ROPE) — Perceives path dependence as coordination mechanism: standardized procedures, organizational culture, and ecosystem fit reduce uncertainty and enable predictable operations. Net beneficiary through first-mover advantages, supplier lock-in, and complementary asset control. Can exit existing procedures (low cost) but benefits from path lock prevent exit.
constraint_indexing:constraint_classification(institutional_path_dependence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISRUPTIVE COALITION (SCAFFOLD) — Organized agents (startups, reformers, new entrants) see path dependence as a temporary bottleneck with a sunset clause. Digital transformation, organizational redesign, and technological discontinuities create windows for alternative institutional designs. Low experienced extraction because the coalition has agency, sees alternative pathways, and expects the incumbent constraint to degrade over technology cycles (10-30 year timescale).
constraint_indexing:constraint_classification(institutional_path_dependence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CEREMONIAL BUREAUCRACY (PITON) — Institutional practices persist through inertia despite loss of primary function. Compliance reporting, governance rituals, and procedural theater consume resources but do not accomplish stated objectives. High theater ratio (0.68) reflects that maintaining appearances of the incumbent system becomes more important than actual performance as the system ages. Enforced through cultural inertia and role identity rather than rational coordination.
constraint_indexing:constraint_classification(institutional_path_dependence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, institutional path dependence appears as an immutable law of organizational evolution: all complex systems accumulate structural constraints over time; irreversibility is inherent to history; high-dimensional state spaces make reversal to prior configurations computationally infeasible. However, the structural data contradicts this — the constraint involves human choices, institutional design, and enforced procedures. The 'natural law' framing naturalizes contingent arrangements.
constraint_indexing:constraint_classification(institutional_path_dependence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_path_dependence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_path_dependence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_path_dependence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_path_dependence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_path_dependence, TR),
    TR >= 0.70.

:- end_tests(institutional_path_dependence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The value reflects that path dependence creates asymmetric benefits over time. Early in institutional lifecycles (time 0, ε=0.28), procedures primarily serve coordination functions with minimal extraction. As the institution ages, alternative designs accumulate, but extraction persists — the incumbent system captures disproportionate benefits through network effects, complementary assets, and procedural automation while alternatives are locked out. By time 20, extractiveness has reached 0.52, reflecting significant rent-seeking layered onto coordination. Suppression (0.65): High. Multiple, reinforcing barriers constrain exit: (1) Sunk costs: employees have specialized training in incumbent systems; (2) Role identity: career paths and professional identities fuse with institutional roles; (3) Ecosystem dependencies: suppliers, partners, and customers coordinate with incumbent procedures; (4) Credential lock: qualifications and certifications are specific to incumbent systems. These barriers are partly structural (actually costly to overcome), partly internalized (identity-locked agents overestimate costs). Theater ratio (0.68): High and increasing. Measurement shows progression from 0.35 (functional coordination dominant) to 0.68 (performance theater dominant). The increase reflects Goodhart drift: as incumbents gain power, procedures become decoupled from original objectives; compliance with procedures replaces achievement of goals; institutional survival becomes independent of functional performance. By time 20, the institution spends significant energy maintaining the appearance that incumbent procedures are optimal, rather than improving actual outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why indexical classification is essential. The trapped reformer (powerless/trapped) sees snare: the institution extracts their labor while preventing change, and they cannot exit without destroying their livelihood. The mid-level manager (moderate/constrained) sees tangled rope: the procedures genuinely coordinate operations AND create asymmetric benefits; they experience real mixed dynamics. The incumbent organization (institutional/arbitrage) sees rope: procedures are coordination mechanisms that solve legitimate collective action problems; the beneficiary experiences this as natural optimization, not extraction. The disruptive coalition (organized/constrained) sees scaffold: path dependence is a temporary bottleneck with a sunset as technology creates discontinuities and alternatives become viable. The ceremonial bureaucracy (institutional/arbitrage) sees piton: procedures persist through inertia despite loss of primary function; the role identity and performance expectations maintain an increasingly theatrical system. The analytical observer (analytical/analytical) risks seeing mountain: institutional lock-in appears as an immutable law of complex systems. But this is a false summit: the constraint involves human choices and institutional design, not laws of physics. The perspectival gaps reveal that path dependence's extractiveness depends entirely on which agent's position you measure from.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural relationship to path dependence. Incumbents and established procedures are beneficiaries with low exit costs (arbitrage = d ≈ 0.15): they benefit from continuation and can escape constraint at low cost (they can shift to new procedures if they choose, but the benefits make exit unattractive). Trapped reformers are victims with no exit options (trapped = d ≈ 0.95): they experience maximum extraction because they cannot leave and cannot change the system from within. Mid-level managers are victims with high exit costs but possible escape (constrained = d ≈ 0.70): they experience significant extraction but could potentially exit by moving to organizations with alternative procedures at the cost of retraining and career reset. Disruptive coalitions are targets with partial exit options (constrained = d ≈ 0.55): they experience some extraction through lock-out effects but have agency through technological alternatives and new entry. The sigmoid f(d) transforms these structural positions into experienced extractiveness chi. Trapped agents experience maximum chi; beneficiaries experience minimum or negative chi; moderate agents experience intermediate chi proportional to their exit costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the mandatrophy trap by clearly distinguishing the coordination function from the extraction function. Path dependence genuinely coordinates operations — standardized procedures reduce transaction costs and enable scale. This is rope's real function. But path dependence ALSO extracts: it creates asymmetric benefits for incumbents and locks out alternatives. This is snare's real function. The constraint cannot be reduced to pure coordination (rope) because the extraction is structural and enforced; nor can it be reduced to pure extraction (snare) because genuine coordination benefits exist. Tangled rope is the correct classification because both functions are structural and both require active enforcement. The mandatrophy resolution shows that the 'natural law' framing (mountain) is a false summit: incumbents use the rhetoric of institutional inevitability to justify what is actually a contingent, enforced constraint. The measurement trajectory (extractiveness rising from 0.28 to 0.52 while theater_ratio rises from 0.35 to 0.68) reveals the dynamic that resolves the mandatrophy: as procedures age, their coordination function is increasingly automated and their extraction function becomes increasingly visible and performative. This is exactly what you would expect from tangled rope: the coordination component (once solved, it persists) fades into background while the extraction component (requires continuous enforcement as alternatives accumulate) becomes dominant. The scaffold perspective captures why the constraint eventually destabilizes: disruptive technologies and new entrants create discontinuities that break the incumbent's lock-in without requiring internal reformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_threshold,
    'At what organizational age or complexity does path dependence transition from reversible to irreversible?',
    'Historical case analysis: organizations that successfully underwent major institutional redesigns vs those that failed; measurement of sunk cost dependencies and ecosystem embeddedness at transition points',
    'If threshold is low (5-10 years): path dependence is primarily enforcement-based, not structural. If threshold is high (30+ years): path dependence approaches natural law. Classification sensitivity: affects whether the constraint is snare vs mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_threshold, empirical, 'Organizational age threshold for irreversible path dependence').

omega_variable(
    technological_discontinuity_sufficiency,
    'Do technological discontinuities (digital transformation, platform shifts, AI integration) actually break incumbent path dependencies or merely create new ones?',
    'Longitudinal tracking of organizations pre/post technology adoption; measurement of institutional procedure retention vs replacement; analysis of whether new systems replicate incumbent patterns',
    'If discontinuities break dependencies: scaffold perspective is structural, sunset is real (30-year timescale). If new systems replicate patterns: organizational DNA is invariant across technologies; path dependence is closer to mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_discontinuity_sufficiency, empirical, 'Whether technological shifts break or reproduce path dependencies').

omega_variable(
    exit_cost_measurement_ambiguity,
    'Is the measured suppression (0.65) structural (actual barriers: credentials, sunk training, ecosystem embeddedness) or largely internalized (identity fusion with institutional role)?',
    'Tracking of agents who exit vs those who remain; measurement of post-exit success rates and suppression trajectory; distinction between material barriers and identity lock',
    'If structural: suppression remains after institutional change. If internalized: suppression persists in agents even after exiting, suggesting identity-locked exit rather than constrained exit. Affects classification of internal agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement_ambiguity, empirical, 'Structural vs internalized suppression mechanism in path dependence').

omega_variable(
    coordination_function_authenticity,
    'Does the institutional procedure that creates path dependence actually serve a genuine coordination function, or has the function degraded while the procedure persists?',
    'Performance measurement: does the procedure accomplish its stated objective? Counterfactual analysis: what coordination gaps would arise if the procedure were removed? Measurement of alternatives that accomplish the same function at lower cost.',
    'If genuine coordination: constraint is rope or tangled rope. If degraded function: constraint is piton. Theater_ratio provides a proxy (0.68 suggests significant degradation), but requires validation through functional analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Authenticity of coordination function in incumbent procedures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_path_dependence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pathd_tr_t0, institutional_path_dependence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pathd_tr_t10, institutional_path_dependence, theater_ratio, 10, 0.52).
narrative_ontology:measurement(pathd_tr_t20, institutional_path_dependence, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pathd_be_t0, institutional_path_dependence, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pathd_be_t10, institutional_path_dependence, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(pathd_be_t20, institutional_path_dependence, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_path_dependence, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_path_dependence, institutional_inertia).
narrative_ontology:affects_constraint(institutional_path_dependence, sunk_cost_fallacy).
narrative_ontology:affects_constraint(institutional_path_dependence, organizational_culture_lock).

% DUAL FORMULATION NOTE:
% Institutional path dependence is the parent constraint that encompasses multiple linked dynamics. Sunk cost fallacy (ε≈0.35) is the cognitive mechanism that sustains path dependence; organizational culture lock (ε≈0.58) is the identity-level mechanism; institutional inertia (ε≈0.42) is the structural persistence mechanism. All three are downstream of path dependence and represent different observables of the same underlying constraint family. Path dependence itself has higher extractiveness because it captures the combined enforcement across all three mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_path_dependence, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
