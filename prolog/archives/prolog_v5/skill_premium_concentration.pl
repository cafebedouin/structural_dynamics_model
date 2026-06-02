% ============================================================================
% CONSTRAINT STORY: skill_premium_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_skill_premium_concentration, []).

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
 *   constraint_id: skill_premium_concentration
 *   human_readable: Skill Premium Concentration in Labor Markets
 *   domain: economic/labor
 *
 * SUMMARY:
 *   The skill premium concentration constraint describes the structural
 *   mechanism by which labor markets allocate higher wages to credentialed
 *   workers while restricting access to those credentials through cost, time,
 *   and gating mechanisms. This constraint exhibits genuine tension between
 *   its coordination and extraction functions. Credentials do solve a real
 *   labor market problem: employers need reliable signals of worker
 *   capability, and standardized credentials reduce information asymmetry.
 *   Simultaneously, credential gatekeeping restricts the supply of certified
 *   workers, maintaining wage premiums that exceed what unrestrained
 *   competition would produce. The extractiveness has increased from 0.35 to
 *   0.58 over two decades as the credential premium has widened and
 *   retraining barriers have risen. The theater ratio (0.48) indicates
 *   moderate performative content — credentials correlate with skill but also
 *   function as positional goods where much of their value derives from
 *   scarcity rather than from intrinsic productivity. Alternative
 *   credentialing pathways (bootcamps, online platforms, apprenticeships) are
 *   building parallel systems with different gating mechanisms, but
 *   traditional credential institutions maintain substantial gatekeeping
 *   power through employer familiarity and established signaling convention.
 *
 * KEY AGENTS:
 *   - Low-Skill Workers: Primary victim (powerless/trapped) — wage stagnation, credential barriers, structural immobility
 *   - Aspiring Credential Seekers: Secondary victim (moderate/constrained) — face debt, time burden, opportunity costs; also benefit from genuine skill development
 *   - Knowledge Employers (Tech/Finance): Primary beneficiary (institutional/arbitrage) — credential system solves talent matching; can access global talent pools
 *   - Educational Credential System: Beneficiary (institutional/arbitrage) — monopoly on certification, coordination value genuine but extractive gatekeeping maintained
 *   - High-Skill Incumbents: Mixed actor (powerful/mobile) — benefit from wage premium protection; experience constraint as protecting professional monopoly
 *   - Alternative Credentialing Coalition: Organized actor (organized/constrained) — building sunset pathways: bootcamps, online platforms, apprenticeships
 *   - Analytical Observer: Structural view (analytical/analytical) — sees both genuine coordination (skill matching) and genuine extraction (credential gatekeeping)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(skill_premium_concentration, 0.58).
domain_priors:suppression_score(skill_premium_concentration, 0.62).
domain_priors:theater_ratio(skill_premium_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skill_premium_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(skill_premium_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(skill_premium_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(skill_premium_concentration, tangled_rope).
narrative_ontology:human_readable(skill_premium_concentration, "Skill Premium Concentration in Labor Markets").
narrative_ontology:topic_domain(skill_premium_concentration, "economic/labor").

domain_priors:requires_active_enforcement(skill_premium_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(skill_premium_concentration, credentialed_professionals).
narrative_ontology:constraint_beneficiary(skill_premium_concentration, educational_institutions).
narrative_ontology:constraint_beneficiary(skill_premium_concentration, knowledge_employers).
narrative_ontology:constraint_victim(skill_premium_concentration, low_skill_workers).
narrative_ontology:constraint_victim(skill_premium_concentration, wage_stagnation_populations).
narrative_ontology:constraint_victim(skill_premium_concentration, credential_excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-SKILL WORKER (SNARE) — Faces structural barriers to skill acquisition: cost of credentialing, time burden of retraining, geographic immobility, family obligations. Wage stagnation while skill premium rises. No viable exit from the constraint. Maximum experienced extraction without coordination benefit.
constraint_indexing:constraint_classification(skill_premium_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING CREDENTIAL SEEKER (TANGLED ROPE) — Constrained by credential costs and opportunity costs but also benefits from genuine credentialing value: market does reward real skill development. The system coordinates skill matching with employment while extracting through credential-gating and debt-financed education. Mixed experience: some real coordination, significant asymmetric costs.
constraint_indexing:constraint_classification(skill_premium_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KNOWLEDGE EMPLOYER (ROPE) — Experiences the constraint as coordination: credential system solves talent matching problem, reduces hiring risk, enables competitive wage differentiation. Benefits from concentration without bearing significant cost. Net beneficiary with arbitrage exit (can source talent globally).
constraint_indexing:constraint_classification(skill_premium_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL CREDENTIAL SYSTEM (ROPE) — Benefits from monopoly on skill certification. Coordination function: provides standardized credential signaling. Low experienced extraction because institutions have arbitrage exit (other credentialing pathways) and the system does deliver some genuine value. Theater ratio moderate because credentials correlate with actual skill, not purely performative.
constraint_indexing:constraint_classification(skill_premium_concentration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-SKILL INCUMBENT (TANGLED ROPE) — Benefits significantly from credential gatekeeping that maintains wage premium. Experiences constraint as both coordination (credentials signal quality) and extraction (protects market share against new entrants). Mobile exit option (can switch sectors/geography) but incentivized to maintain the constraint. Effective extraction from below funds their premium.
constraint_indexing:constraint_classification(skill_premium_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALING COALITION (SCAFFOLD) — Organized agents (bootcamps, online platforms, apprenticeship programs) building parallel pathways with lower barriers and faster completion. Sees the traditional credential constraint as temporary and being bypassed. Has sunset clause: as alternative credentials gain employer recognition, traditional credential gatekeeping loses extractive force. Constrained by incumbent institutional resistance.
constraint_indexing:constraint_classification(skill_premium_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, this constraint genuinely coordinates labor matching: employers need reliable signals of capability, workers need certification. The constraint also genuinely extracts: credential gatekeeping maintains wage premium by restricting supply of certified workers. Both functions are structural and irreducible. Effective extraction (chi) is moderate because the coordination function is genuine.
constraint_indexing:constraint_classification(skill_premium_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skill_premium_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(skill_premium_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(skill_premium_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(skill_premium_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(skill_premium_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The skill premium has widened substantially — credentialed workers earn 50-100% wage premiums over non-credentialed workers in comparable roles. The increase from 0.35 to 0.58 reflects rising barriers to credential attainment: cost of higher education has doubled in real terms over two decades, retraining windows have compressed, and credential inflation means low-skill workers face higher credential requirements for the same roles. However, extractiveness is not higher (e.g., 0.70+) because credentials do correlate with genuine skill gains and employers do benefit from real productivity improvements alongside signaling value. The constraint is hybrid: real coordination + real extraction, neither fully reducible to the other. Suppression (0.62): High. Multiple barriers prevent trapped workers from acquiring credentials: direct costs ($20k-$200k for degree programs), opportunity costs (2-6 years foregone earnings), family/geographic constraints, incomplete information about ROI, and qualification barriers (prior credentials required). These create near-total suppression of exit for poorest populations; mobility and constrained exit are available to middle-income aspiring credential seekers but at high cost. Theater ratio (0.48): Moderate. Credentials signal real skill but also function as positional goods — much of their value derives from relative scarcity. Credentialing institutions maintain performative content (graduation ceremonies, institutional prestige narratives) but credentials do correlate with measurable capability. The theater is lower than pure Piton (which would require theater_ratio ≥ 0.70) because the signaling function, while imperfect, works reasonably well.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classic hybrid dynamics. The trapped victim sees a Snare because suppression is total and exit is inaccessible. The constrained seeker sees a Tangled Rope because they experience both real skill development (genuine coordination) and real extraction costs (debt, opportunity cost). The beneficiary sees a Rope because they experience coordination benefit without bearing the suppression cost. The alternative coalition sees a Scaffold because they have organized agency and see a clear sunset: as alternative credentials mature and employers accept them, the traditional constraint's extractive gatekeeping will lose force. The analytical observer sees Tangled Rope because both functions are structurally real — credentials genuinely coordinate labor matching and genuinely gate access to wage premiums.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness flows from its structural position relative to the constraint. Trapped workers with no exit pay the full extraction cost. Constrained seekers with access to (expensive) exit pay high but not maximum costs, and they gain real skill benefit that partially offsets extraction. Beneficiaries gain from the constraint and experience it as coordination. Organized alternatives have agency and see clear exit paths. The analytical observer integrates across all positions and sees the constraint as genuinely hybrid: the coordination function (skill matching) is real, and the extraction function (wage gatekeeping) is real. Neither can be reduced to the other or dissolved without dissolving both.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID RESOLUTION: This constraint resolves mandatrophy by demonstrating that classification depends on agent position and that the constraint is genuinely hybrid, not mistakenly bifurcated. The constraint is not 'really' coordination with an extraction cover story, nor is it 'really' extraction with a coordination narrative. Both functions are structural. The coordination function is real: employers need skill signals, workers benefit from credential development, the system does match talent to roles more efficiently than unmediated labor markets would. The extraction function is real: credential gatekeeping restricts supply of certified workers, maintaining wage premiums that exceed competitive levels, and the barriers to credential attainment concentrate benefits upward. Neither function can be removed without destroying the other. Removing the credential gatekeeping (extraction) would also remove the skill-signaling function (coordination). Removing credential requirements (coordination) would also remove the wage premium protection (extraction). The constraint is a genuine Tangled Rope: hybrid extraction-coordination that cannot be decomposed into pure types. The mandatrophy resolves by recognizing that some constraints are irreducibly hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_real_value_vs_signaling,
    'What proportion of the skill premium reflects genuine productivity gains from credentials vs. pure signaling power (Spence screening)?',
    'Comparative wage analysis: same-task workers with/without credentials; productivity measurement by task type; cross-country credential requirement variation; longitudinal skill-gain tracking',
    'If high real value (>70%): constraint is primarily Rope (coordination). If high signaling (>50%): constraint is primarily Snare (extraction). Classification sensitivity: if signaling dominates, victim perspectives become more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_real_value_vs_signaling, empirical, 'Proportion of credential premium that reflects genuine skill vs. pure signaling').

omega_variable(
    alternative_credential_employer_acceptance,
    'Are non-traditional credentials (bootcamps, online certifications, apprenticeships) achieving labor market acceptance parity with traditional degrees?',
    'Longitudinal wage tracking for alternative-credentialed workers; employer hiring data on credential acceptance; wage convergence analysis over 10+ years; sector-specific adoption rates',
    'If parity achieved: scaffold sunset is real, constraint extractiveness declining. If stagnant: alternative pathways remain stigmatized, traditional credential monopoly persists, extractiveness rising.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credential_employer_acceptance, empirical, 'Labor market acceptance trajectory of alternative credentials').

omega_variable(
    geographic_credential_arbitrage,
    'Can credential-excluded populations exercise exit by relocating to labor markets with lower credential requirements or different gating mechanisms?',
    'Migration flow analysis: credential-excluded populations to regional/international labor markets; wage gains post-migration; regional wage dispersion for same-skill workers; immigration policy barrier measurement',
    'If exit viable: suppression lower than measured, constraint may downgrade from Snare to Tangled Rope. If exit blocked: exit_options remain ''trapped'', suppression confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_credential_arbitrage, empirical, 'Viability of geographic exit for credential-excluded populations').

omega_variable(
    automation_skill_premium_feedback,
    'Does automation increase or decrease the skill premium by replacing routine tasks at both low and high skill levels?',
    'Sectoral automation impact analysis; wage trend correlation with automation adoption; occupational displacement patterns by skill level; task-level automation substitution analysis',
    'If automation increases premium (high-skill tasks less automatable): constraint becomes more extractive, beneficiary positions strengthen. If automation decreases premium: constraint extractiveness may decline, compressed wage distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_skill_premium_feedback, empirical, 'Automation''s effect on skill premium trajectory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(skill_premium_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skill_tr_t0, skill_premium_concentration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(skill_tr_t10, skill_premium_concentration, theater_ratio, 10, 0.42).
narrative_ontology:measurement(skill_tr_t20, skill_premium_concentration, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(skill_be_t0, skill_premium_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(skill_be_t10, skill_premium_concentration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(skill_be_t20, skill_premium_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(skill_premium_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(skill_premium_concentration, 0.18).
narrative_ontology:affects_constraint(skill_premium_concentration, wage_stagnation).
narrative_ontology:affects_constraint(skill_premium_concentration, credential_inflation).
narrative_ontology:affects_constraint(skill_premium_concentration, educational_debt_accumulation).

% DUAL FORMULATION NOTE:
% Skill premium concentration is upstream of wage stagnation and credential inflation constraints. The coordination function of skill-matching is genuine; the extraction function of gatekeeping is also genuine. Decomposition would artificially separate these coupled functions. Network links show downstream consequences: wage stagnation for credential-excluded populations, credential inflation as employers raise requirements, and educational debt accumulation as credentialing costs rise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(skill_premium_concentration, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
