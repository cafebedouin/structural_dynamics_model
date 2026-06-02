% ============================================================================
% CONSTRAINT STORY: credential_inflation_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_inflation_cycle, []).

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
 *   constraint_id: credential_inflation_cycle
 *   human_readable: Credential Inflation Cycle
 *   domain: labor/education/institutional_economics
 *
 * SUMMARY:
 *   The credential inflation cycle represents a structural trap in labor
 *   markets where employers use educational credentials as a hiring filter,
 *   creating incentives for workers to acquire higher credentials to signal
 *   competence. As credential supply increases, the signaling value of any
 *   single credential erodes (Goodhart effect: the measure becomes the
 *   target, losing its information content). Employers then raise credential
 *   requirements to restore discriminatory power. Workers must escalate their
 *   credential acquisition to maintain competitiveness. This creates a
 *   self-reinforcing cycle with increasing extractiveness (longer education,
 *   higher debt, delayed labor market entry) and increasing theater
 *   (credentials become ritualistic proof of throughput rather than
 *   functional skill markers). The constraint exhibits all six types from
 *   different structural positions. Labor entrants see a snare (trapped in
 *   escalating requirements). Incumbents see a tangled rope (protected but
 *   also locked in). Credential issuers see pure rope (coordination and
 *   benefit). Employers see piton (theater-driven ritual, not functional
 *   hiring). Alternative credentialing movements see a solvable scaffold. The
 *   analytical observer risks seeing a mountain (information asymmetry as
 *   natural law), which the structural data contradicts.
 *
 * KEY AGENTS:
 *   - Labor Entrants: Primary victims (powerless/trapped) — must acquire escalating credentials to access employment; bear educational debt and opportunity costs with no alternative pathway
 *   - Incumbent Workers: Secondary beneficiaries (moderate/constrained) — existing credentials maintain wage premiums through artificial scarcity; also locked in by credential devaluation risk
 *   - Universities and Credential Issuers: Primary beneficiaries (institutional/arbitrage) — capture expanding enrollment, tuition revenue, and prestige from credential inflation; benefit from employers' escalating requirements
 *   - Employers: Institutional actors (institutional/constrained) — use credentials as hiring filter despite knowing requirements exceed job function; persist with theater due to coordination failure and competitive pressure
 *   - Alternative Credentialing Providers: Organized agents (organized/mobile) — bootcamps, skills platforms, apprenticeship programs building exit pathways; can defect from traditional credential system but face their own credential inflation risks
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing credential inflation as inherent labor market law rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_inflation_cycle, 0.58).
domain_priors:suppression_score(credential_inflation_cycle, 0.62).
domain_priors:theater_ratio(credential_inflation_cycle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_inflation_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(credential_inflation_cycle, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(credential_inflation_cycle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_inflation_cycle, tangled_rope).
narrative_ontology:human_readable(credential_inflation_cycle, "Credential Inflation Cycle").
narrative_ontology:topic_domain(credential_inflation_cycle, "labor/education/institutional_economics").

domain_priors:requires_active_enforcement(credential_inflation_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_inflation_cycle, credential_issuers).
narrative_ontology:constraint_beneficiary(credential_inflation_cycle, incumbent_workers_with_existing_credentials).
narrative_ontology:constraint_victim(credential_inflation_cycle, labor_entrants).
narrative_ontology:constraint_victim(credential_inflation_cycle, field_skill_distribution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR ENTRANT (SNARE) — Trapped by degree requirements that escalate beyond job function. A high school diploma once secured entry-level work; now a bachelor's degree is required; tomorrow a master's degree may be mandatory. The entrant has no exit: they cannot obtain the job without the credential, cannot obtain the credential without years of unpaid education and debt accumulation, and cannot avoid the cycle because all competitors face the same escalation. Full extraction with maximum suppression.
constraint_indexing:constraint_classification(credential_inflation_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT WORKER (TANGLED ROPE) — Constrained by credential lock-in: holding an existing degree raises switching costs (employer preference for higher credentials reduces mobility), but also benefits from credential gatekeeping (the degree creates artificial scarcity that maintains wage premiums). Mixed: the constraint both protects and imprisons them. They cannot exit without credential devaluation risk, but they benefit from the gate.
constraint_indexing:constraint_classification(credential_inflation_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIAL ISSUER (ROPE) — Benefits from expanding enrollment driven by credential inflation. As employers demand higher qualifications, universities capture more students, tuition revenue, and institutional prestige. Experiences the constraint as beneficial coordination: growing the credential supply solves the signaling problem (employers get a filter, students get a certificate). The issuer has arbitrage options (market share gains, fee increases, program proliferation) and sees the constraint as serving a coordination function.
constraint_indexing:constraint_classification(credential_inflation_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPLOYER (PITON) — Uses credentials as a hiring filter despite acknowledging that the credential level exceeds job requirements. The employer persists with inflated degree requirements because competitors do the same (Goodhart drift: the credential became the proxy for competence, replacing actual skill assessment). The employer knows the requirement is theater (many positions require bachelor's degrees but teach job-specific skills on the job) yet cannot unilaterally lower requirements without risking perception of lower-quality hiring. Piton: the credential gate persists through inertia despite low direct function.
constraint_indexing:constraint_classification(credential_inflation_cycle, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE CREDENTIALING (ORGANIZED ACTIVISTS) (TANGLED ROPE) — Organized agents (bootcamp providers, micro-credential platforms, skills-based hiring advocates) see the cycle as a solvable coordination failure and are building exit pathways. The alternative pathways also extract (bootcamp tuition, platform lock-in, skills verification overhead) but with lower theater and clearer functional mapping (skills → jobs, not degrees → ambiguous general competence). Moderate extraction because they have agency, are building real alternatives, and can exit the traditional credential system.
constraint_indexing:constraint_classification(credential_inflation_cycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, credential inflation is an immutable consequence of information asymmetry in labor markets: employers cannot directly observe worker competence, so they demand signals; as signals proliferate, baseline signals lose discriminatory power, forcing escalation. This is framed as a 'natural law' of labor market dynamics — the Spence signaling equilibrium. However, the structural data contradicts the mountain classification: the constraint is maintained by institutional choices (employers choosing to use credentials, universities choosing to expand, policy choices around credentialing), not by physical or logical necessity. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(credential_inflation_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_inflation_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_inflation_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_inflation_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_inflation_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_inflation_cycle, TR),
    TR >= 0.70.

:- end_tests(credential_inflation_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from labor entrants through required education costs (tuition, opportunity cost, debt service) and delays market entry. The extraction is genuine but not maximal — workers do gain skills and credential value is not zero. The measurement trajectory (0.35 → 0.58 over 30 years) reflects growing credential inflation as requirements escalate beyond job function. Suppression (0.62): Moderate-high. Labor entrants have few alternatives — they cannot compete for jobs without credentials, cannot skip education without career penalties, and cannot collectively bargain credential requirements (institutional choice, not worker preference). Alternatives are emerging (bootcamps, skills-based hiring) but remain marginal. Theater ratio (0.68): High and rising. Credential requirements have increasingly decoupled from actual job skill requirements — the credential became a proxy for throughput (completed program) rather than demonstrated competence. Employers' own surveys show bachelor's degree requirements for roles that require high school-level technical skills, indicating Goodhart drift (the measure replacing the target). The measurement trajectory (0.48 → 0.68 over 30 years) reflects increasing performative content as credentials proliferate without corresponding job evolution.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification across institutional levels and agent positions. The labor entrant sees a snare — they are trapped in escalating requirements with no exit and cannot organize collective defection. The incumbent worker sees a tangled rope — they benefit from gatekeeping protection but are also locked in by credential devaluation and forced to escalate their own credentials as requirements climb. The credential issuer sees pure rope — expanding enrollment is coordination (matching students to verification, employers to filtered candidates) with direct benefit. The employer sees a piton — they use credentials as a hiring filter despite knowing the requirement exceeds job function, persisting with theater because competitors do the same and because coordinating on lower requirements is institutionally difficult. The alternative credentialing movement sees a solvable constraint with sunset potential — bootcamps and skills platforms offer exit from the traditional cycle, though they face their own credential inflation risks. The analytical observer risks mistaking Goodhart dynamics and institutional inertia for a natural law (the Spence signaling equilibrium), but the structural data reveals institutional choices as the binding mechanism: employers could unilaterally reduce degree requirements, universities could resist credential inflation, or policy could regulate credential escalation. The mountain classification is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values derive from each agent's structural relationship to the extraction flow. Labor entrants (powerless/trapped) experience d ≈ 0.95: they are full targets of extraction with no exit options, producing maximum f(d) ≈ 1.42. Incumbent workers (moderate/constrained) experience d ≈ 0.55: they bear extraction (credential devaluation pressures) but also benefit from gatekeeping, producing moderate f(d) ≈ 0.75. Credential issuers (institutional/arbitrage) experience d ≈ 0.05: they are beneficiaries with arbitrage options, producing f(d) ≈ -0.12. Employers (institutional/constrained) experience d ≈ 0.45: they benefit from the credential filter (reduced hiring uncertainty) but are constrained by competitive pressure to maintain ritual requirements, producing f(d) ≈ 0.40. Alternative credentialing agents (organized/mobile) experience d ≈ 0.35: they offer exit pathways and have agency, producing f(d) ≈ 0.15. The analytical observer at civilizational scope produces d ≈ 0.72: the position is structural but not anchored to a particular agent's extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The credential inflation cycle is classified as tangled_rope at the analytical level because it combines genuine coordination (employers need hiring signals, workers need verification, universities need enrollment) with asymmetric extraction (labor entrants bear educational debt and opportunity costs; incumbents and issuers capture wage premiums and revenue). The constraint resolves mandatrophy by identifying that credential inflation is NOT pure extraction (Snare) — it does coordinate labor market signaling — and NOT pure coordination (Rope) — it does extract from labor entrants and shift costs to them. The tangled_rope classification prevents either false positive (calling it pure extraction) or false negative (calling it pure coordination). The theater ratio (0.68) further disambiguates: if the constraint were merely coordinating, theater would be low (credentials would maintain stable signaling value); the rising theater indicates Goodhart drift (credentials becoming rituals rather than signals). The piton perspective from employers confirms the theater: employers maintain inflated requirements not because they functionally need higher qualifications but because they cannot unilaterally defect without signaling lower hiring standards. The constraint thus exhibits both genuine coordination function (the signaling core) and increasing extraction overlay (the inflation spiral), which is precisely the tangled_rope structural definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_decay_rate,
    'What is the rate at which credentials lose discriminatory power as supply increases, and is there a stable equilibrium or permanent escalation?',
    'Historical wage-education premium analysis; cohort analysis of credential value erosion over time; identification of skills where escalation has plateaued',
    'If decay is rapid and permanent: credential inflation is a self-sustaining extraction trap with no equilibrium. If decay slows or plateaus: credential value stabilizes and extraction eventually maxes out. If decay stops: credential inflation was temporary Goodhart drift, now resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_decay_rate, empirical, 'Rate of credential value decay and equilibrium properties').

omega_variable(
    alternative_credential_viability,
    'Can skills-based hiring and alternative credentials (bootcamps, micro-credentials, apprenticeships) actually replace degree-based hiring at scale, or do they replicatecredential inflation within their own domain?',
    'Longitudinal tracking of hiring outcomes for alternative-credential holders; analysis of wage premiums and career progression; identification of whether alternative credentials face inflation pressure',
    'If alternatives are viable: scaffold perspective is correct, sunset is real, and credential inflation is solvable. If alternatives replicate inflation: constraint is structural to labor signaling, not contingent on degrees. Classification shifts from tangled_rope/snare to mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_viability, empirical, 'Whether alternative credentials can sustainably replace traditional degrees').

omega_variable(
    employer_switching_coordination,
    'Can employer coordination on credential requirements (explicit agreements to lower degree requirements) succeed without antitrust violation, and would voluntary coordination stick or face free-rider defection?',
    'Analysis of employer associations and hiring standards initiatives; identification of defection incentives; case studies of coordinated hiring standard changes and their stability',
    'If coordination is possible and stable: extractiveness drops (employers solve the Goodhart problem), classification becomes Rope. If defection is inevitable: constraint is a coordination failure with no internal solution, extractiveness remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_switching_coordination, empirical, 'Feasibility of employer coordination on credential standards').

omega_variable(
    student_debt_as_extraction_mechanism,
    'Is credential inflation functioning as a debt-extraction mechanism (students bear financial cost to benefit incumbents), or is the cost borne by society through tax-subsidized education and foregone productivity?',
    'Analysis of cost allocation: student debt, public subsidy, foregone income during school, opportunity cost; comparison of total cost extraction vs. signaling value delivered',
    'If student-debt-funded: extractiveness may be even higher than 0.58 (labor entrant bears compounding debt burden). If public-subsidized: cost is distributed, but extraction remains (incumbents benefit from gatekeeping regardless of who pays).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(student_debt_as_extraction_mechanism, empirical, 'Debt-based cost allocation as extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_inflation_cycle, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_infl_tr_t0, credential_inflation_cycle, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cred_infl_tr_t10, credential_inflation_cycle, theater_ratio, 10, 0.62).
narrative_ontology:measurement(cred_infl_tr_t20, credential_inflation_cycle, theater_ratio, 20, 0.68).
narrative_ontology:measurement(cred_infl_tr_t30, credential_inflation_cycle, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(cred_infl_be_t0, credential_inflation_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cred_infl_be_t10, credential_inflation_cycle, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cred_infl_be_t20, credential_inflation_cycle, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cred_infl_be_t30, credential_inflation_cycle, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_inflation_cycle, information_standard).
narrative_ontology:affects_constraint(credential_inflation_cycle, student_debt_accumulation).
narrative_ontology:affects_constraint(credential_inflation_cycle, intergenerational_wealth_stratification).
narrative_ontology:affects_constraint(credential_inflation_cycle, skill_training_displacement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
