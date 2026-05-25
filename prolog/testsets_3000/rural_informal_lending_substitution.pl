% ============================================================================
% CONSTRAINT STORY: rural_informal_lending_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rural_informal_lending_substitution, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rural_informal_lending_substitution
 *   human_readable: Rural Informal Lending Substitution Trap
 *   domain: economic/financial/social
 *
 * SUMMARY:
 *   Rural informal lending represents a structural trap where the absence of
 *   formal financial services creates an extraction opportunity that informal
 *   moneylenders efficiently occupy. The constraint operates across local and
 *   regional scales, binding small farmers and agricultural laborers into
 *   high-cost debt cycles through suppression mechanisms rooted in both
 *   material necessity (lack of alternative credit) and social enforcement
 *   (reputation networks, family obligation). The constraint demonstrates how
 *   a coordination function (providing credit where formal banking refuses)
 *   can mask and justify extraction. The theater ratio has increased over the
 *   measurement interval as formal government credit schemes have
 *   proliferated without reducing informal lending prevalence — formal
 *   schemes remain largely inaccessible due to documentation requirements and
 *   misaligned loan amounts, creating a facade of credit availability without
 *   functional substitution. This is the core insight: rural informal lending
 *   substitution is a snare, not because coordination fails, but because the
 *   'coordination' of providing credit is structurally impossible to
 *   disentangle from extraction.
 *
 * KEY AGENTS:
 *   - Small farmers and agricultural laborers: Primary victims (powerless/trapped) — face seasonal liquidity crises with no alternative credit access; debt cycle creates multi-generational entanglement
 *   - Moneylender networks: Primary beneficiaries (institutional/arbitrage) — capture surplus through interest rate premiums and collateral claims; can exit to other regions or forms of capital deployment
 *   - Microfinance institutions and rural NGOs: Secondary actors (organized/constrained) — attempt to provide alternative credit but face moneylender resistance, borrower cultural preference for known lenders, and difficulty achieving scale
 *   - Formal agricultural banks: Institutional actors (powerful/mobile) — have exited or marginalized rural lending due to transaction costs and perceived risk; their absence enables moneylender dominance
 *   - Government agricultural credit schemes: Institutional theater (institutional/arbitrage) — formal programs exist but suffer from bureaucratic barriers, illiteracy/documentation requirements, and misalignment with seasonal borrowing needs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rural_informal_lending_substitution, 0.68).
domain_priors:suppression_score(rural_informal_lending_substitution, 0.72).
domain_priors:theater_ratio(rural_informal_lending_substitution, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rural_informal_lending_substitution, extractiveness, 0.68).
narrative_ontology:constraint_metric(rural_informal_lending_substitution, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rural_informal_lending_substitution, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rural_informal_lending_substitution, snare).
narrative_ontology:human_readable(rural_informal_lending_substitution, "Rural Informal Lending Substitution Trap").
narrative_ontology:topic_domain(rural_informal_lending_substitution, "economic/financial/social").

domain_priors:requires_active_enforcement(rural_informal_lending_substitution).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rural_informal_lending_substitution, moneylender_networks).
narrative_ontology:constraint_victim(rural_informal_lending_substitution, small_farmers).
narrative_ontology:constraint_victim(rural_informal_lending_substitution, agricultural_laborers).
narrative_ontology:constraint_victim(rural_informal_lending_substitution, village_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED FARMER (SNARE) — Trapped by debt cycle with no exit. Faces seasonal cash-flow crises requiring emergency loans at 48-120% annual rates. Collateral demands (land, future harvests) create multi-generational entanglement. Cannot access formal banking due to lack of collateral, credit history, or documentation. Extraction is maximal and inescapable within the biographical horizon.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AGRICULTURAL LABORER (SNARE) — Trapped across generations. Advances from moneylender (often employer-coincident) create debt bondage. Wages are suppressed by informal loan repayment obligations. Cannot exit the region without forfeiting debt claims or facing enforcement through social networks. Intergenerational transmission of debt status to children.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MONEYLENDER NETWORK (ROPE) — Experiences the constraint as a coordination mechanism. They solve the genuine problem of rural liquidity shortage by providing credit where formal banks do not. High interest rates reflect real enforcement costs, information asymmetry, and default risk. They have exit options (shift capital to other regions, invest in formal finance) and benefit from the constraint structure. The constraint coordinates rural credit supply; that it extracts is secondary to their perspective.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RURAL DEVELOPMENT NGOS (TANGLED ROPE) — Organized agents addressing rural credit access see both genuine coordination function (closing credit gap) and embedded extraction (high-cost debt traps). They are constrained by the political economy of formal banking: banks avoid rural lending due to transaction costs and perceived risk. NGOs must navigate social networks, moneylender resistance, and farmer cultural trust (borrowers often prefer known moneylenders to impersonal institutions). They benefit from donor funding focused on rural finance but bear costs of institutional friction.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL FINANCE SECTOR (TANGLED ROPE) — They have coordination function (expanding rural credit access) but also extract through operational costs, collateral demands, and regulatory compliance overhead. They can exit rural lending (shift to urban/commercial credit) and often do, leaving the field to informal lenders. Their relative market absence enables moneylender dominance. The constraint traps formal finance in a coordination role it partially abandons, creating the extraction void.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: GOVERNMENT AGRICULTURAL CREDIT SCHEMES (PITON) — Formal subsidized credit programs (crop loans, agricultural lines of credit) exist but are largely theatrical: take-up rates are low despite subsidized rates, bureaucratic documentation is prohibitive for illiterate farmers, and loan amounts often don't match seasonal cash-flow needs. These schemes persist through policy inertia and donor pressure, not because they function — theater_ratio here is high. Real credit flows remain informal despite formal programs.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, rural informal lending appears as an inevitable natural law: absence of formal credit in dispersed populations requires high-information, relationship-based lending. Information asymmetry about farmer creditworthiness is inherent; high interest rates reflect real risk. But the structural data contradicts mountain: this 'natural law' is maintained by policy choices (banks' decision to exit rural markets), institutional design (formal schemes' bureaucratic inaccessibility), and suppression mechanisms (moneylender enforcement through social networks). The constraint is contingent, not natural.
constraint_indexing:constraint_classification(rural_informal_lending_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rural_informal_lending_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rural_informal_lending_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rural_informal_lending_substitution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rural_informal_lending_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rural_informal_lending_substitution, TR),
    TR >= 0.70.

:- end_tests(rural_informal_lending_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The moneylender system captures substantial surplus through interest rates of 48-120% annually, collateral claims on land and future harvests, and wage suppression (when moneylender is also employer). The extraction has grown over the interval as farmers' debt stocks accumulate and formal finance remains absent. However, extractiveness is not 1.0 because genuine credit provision does occur and some borrowers perceive the service as necessary (reflecting the coordination function). Suppression (0.72): High. Trapped farmers face severe barriers to exit: no alternative credit sources, collateral dependencies, social network enforcement (reputation damage, community pressure), and intergenerational debt transmission. Suppression is not higher because some borrowers do occasionally exit through migration, NGO intervention, or formal scheme access — the barriers are severe but not absolute. Theater ratio (0.58): Moderate-high and rising. Government credit schemes are substantially theatrical: they exist in policy and budget documents but have low farmer participation and do not displace informal lending. The formal programs' failure to substitute is itself performative — they create appearance of formal credit availability without functional delivery. The rising theater reflects increasing policy proliferation without penetration.
 *
 * PERSPECTIVAL GAP:
 *   The farmer and moneylender live in inverted worlds. For the farmer, this is a snare: they are trapped in a debt cycle they cannot escape. For the moneylender, this is a rope: they are solving the legitimate problem of providing credit where formal banks refuse, and the high interest rate is a fair return for the genuine risks they bear (information asymmetry, enforcement costs, default). The NGO perspective (tangled rope) recognizes both: the coordination function is real but the extraction is also real. The formal finance perspective reveals the key structural fact: banks can and do exit rural markets, treating rural lending as insufficiently profitable. Their exit is not forced (they have arbitrage options) — it is chosen. The analytical observer's temptation (mountain: rural credit 'naturally' requires high rates) naturalizes this choice as inevitable. The structural data shows it is not.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from beneficiary/victim declarations and exit options. Trapped farmers with no alternatives experience d approaching 1.0 (full target), producing maximum experienced extractiveness. Moneylenders with arbitrage options and beneficiary status experience d near 0.0 (full beneficiary), producing negative effective extraction from their perspective (the constraint subsidizes them). NGOs and formal banks occupy middle positions with constrained/mobile options and mixed beneficiary/victim status — they experience moderate extraction. The derivation shows why formal finance's absence sustains the snare: as long as banks can exit rural lending without cost, the extracted surplus remains entirely with the trapped borrowers. If policy forced formal finance to price rural credit accurately (including true risk) rather than avoid it, the extraction would become visible as a coordination problem requiring redistribution rather than as a natural interest rate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between the coordination function (providing credit in rural markets) and the extraction mechanism (suppression through debt and social enforcement). The moneylender network does coordinate credit supply — this is not mere extraction theater. But the coordination is parasitic on suppression: without trapped borrowers and social enforcement, the high-interest coordination would collapse as borrowers exited to alternatives. The snare classification is appropriate because the extraction (high interest rates + debt accumulation + intergenerational transmission) is what makes the coordination possible, not a side effect. If you removed the suppression, the coordination would not persist — moneylenders would exit and formal banks would enter only if forced by policy. The mandatrophy is resolved by recognizing that 'rural credit is hard to provide' (true) does not mean 'rural credit requires debt traps' (false) — the latter is a policy choice, not a coordination requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_accessibility_collapse,
    'Is the extractive interest rate (48-120% annually) a fair risk premium for genuine default risk, or an exploitative markup over true default probability?',
    'Comparison of actual default rates in informal lending against comparable formal sector risk premiums; analysis of interest rate variation by borrower type and loan purpose',
    'If rates are risk-calibrated: reclassify as tangled rope (coordination with significant but justified cost). If rates exceed true risk: snare classification strengthens (pure extraction disguised as risk pricing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_accessibility_collapse, empirical, 'Calibration of interest rates to actual default risk').

omega_variable(
    exit_option_availability,
    'Do microfinance institutions, agricultural cooperative credit systems, or bank linkage schemes actually provide functional exit options for trapped borrowers, or do they merely recreate informal lending under different institutional branding?',
    'Longitudinal tracking of borrowers who transition from moneylender to formal/semi-formal credit; measurement of interest rate reduction, debt cycle interruption, and intergenerational transmission cessation',
    'If alternatives provide genuine exit: constraint is structurally escapable (reclassify exit_options as constrained rather than trapped). If alternatives fail: exit options remain illusory and the constraint strengthens as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Whether alternative credit systems provide real exit').

omega_variable(
    suppression_mechanism_social_networks,
    'How much of the measured suppression (0.72) is structural (lack of alternative credit) versus socially enforced (moneylender reputation damage, community pressure, family honor dynamics)?',
    'Ethnographic analysis of enforcement mechanisms; measurement of borrowers'' stated reasons for non-exit; comparison of suppression across regions with varying social network density',
    'If primarily structural: formal finance expansion reduces suppression directly. If primarily social: formal finance alone is insufficient — cultural reframing of default is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_social_networks, empirical, 'Relative contribution of structural vs. social suppression').

omega_variable(
    moneylender_monopoly_fragility,
    'Is the moneylender network a stable, intentionally maintained monopoly, or a fragile equilibrium dependent on formal finance abstention?',
    'Historical analysis of regional markets where formal banking expanded or withdrew; simulation of credit market outcomes under varying assumptions about bank entry thresholds',
    'If stable monopoly: policy must directly target moneylender behavior. If dependent on formal finance exit: policy should focus on incentivizing bank rural expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moneylender_monopoly_fragility, empirical, 'Stability and maintenance of moneylender market dominance').

omega_variable(
    generational_transmission_mechanism,
    'Does debt transmission across generations occur through formal legal mechanisms (debt inheritance) or through social mechanisms (family obligation, cultural internalization)?',
    'Documentation of legal frameworks for debt inheritance; comparison of debt transmission rates where legal inheritance is prohibited vs. where it is permitted; analysis of borrowers'' stated motives for assuming parent debt',
    'If legal: policy intervention (debt forgiveness, inheritance prohibition) directly interrupts transmission. If social: policy must address cultural narratives about filial debt obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transmission_mechanism, empirical, 'Mechanism of intergenerational debt transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rural_informal_lending_substitution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rils_tr_t0, rural_informal_lending_substitution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rils_tr_t10, rural_informal_lending_substitution, theater_ratio, 10, 0.45).
narrative_ontology:measurement(rils_tr_t20, rural_informal_lending_substitution, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rils_be_t0, rural_informal_lending_substitution, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(rils_be_t10, rural_informal_lending_substitution, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(rils_be_t20, rural_informal_lending_substitution, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rural_informal_lending_substitution, resource_allocation).
narrative_ontology:boltzmann_floor_override(rural_informal_lending_substitution, 0.2).
narrative_ontology:affects_constraint(rural_informal_lending_substitution, agricultural_land_fragmentation).
narrative_ontology:affects_constraint(rural_informal_lending_substitution, rural_wage_suppression).
narrative_ontology:affects_constraint(rural_informal_lending_substitution, agricultural_cooperative_failure).

% DUAL FORMULATION NOTE:
% Rural informal lending substitution decomposes into three structurally distinct constraints: (1) the moneylender credit supply mechanism (this story, ε=0.68, Snare), (2) land collateral dependencies that create leverage for debt enforcement (upstream, lower ε but enables suppression), and (3) intergenerational debt transmission as cultural/legal practice (downstream, operates through identity-locked mechanisms not captured in the lending story). Each has different ε values and resolution pathways. This story focuses on the lending mechanism itself; the family structure story would address identity-locked dynamics in debt acceptance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
