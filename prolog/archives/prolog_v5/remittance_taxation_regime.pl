% ============================================================================
% CONSTRAINT STORY: remittance_taxation_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remittance_taxation_regime, []).

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
 *   constraint_id: remittance_taxation_regime
 *   human_readable: Remittance Taxation Regime
 *   domain: economic_policy/international_finance
 *
 * SUMMARY:
 *   Remittance taxation regimes create a structural tension between the
 *   legitimate coordination goal of formalizing international money flows
 *   (reducing fraud, enabling currency stability, documenting household
 *   income) and the extractive extraction of rents from workers with minimal
 *   exit options. The constraint exhibits multiple classification types from
 *   different perspectives. Migrant workers experience pure extraction
 *   (snare) because they are trapped by employment dependency and visa
 *   status. Origin-country governments experience coordination (rope) because
 *   formal remittances enable fiscal capacity and monetary control. Financial
 *   intermediaries experience mixed coordination and extraction (tangled
 *   rope) — compliance costs are real, but rents from formalization are
 *   substantial. The migrant household experiences mixed coordination and
 *   extraction (tangled rope) because formal remittance flows provide fraud
 *   protection but at the cost of taxation. Alternative remittance
 *   technologies (crypto, digital banking) create a sunset logic (scaffold)
 *   because taxation rates will inevitably decline as lower-cost channels
 *   mature. The analytical observer risks seeing taxation as inevitable
 *   (mountain) — an inherent consequence of sovereignty — but the structural
 *   data reveals this as naturalization of policy choices about rates,
 *   enforcement intensity, and exemption categories.
 *
 * KEY AGENTS:
 *   - Migrant Workers: Primary victims (powerless/trapped) — earn income abroad, send remittances home, bear taxation incidence through reduced transfer amounts; no exit due to visa status dependency
 *   - Origin-Country Governments: Primary beneficiaries (institutional/arbitrage) — collect tax revenue from remittance flows; experience formalization as coordination success and currency control mechanism
 *   - Origin-Country Households: Secondary victims (moderate/constrained) — depend on remittances for household income; benefit from formal remittance documentation but bear taxation cost
 *   - Financial Intermediaries: Secondary beneficiaries (powerful/mobile) — extract rents from formal remittance processing; provide coordination service (fraud reduction, settlement); can arbitrage between regulatory regimes
 *   - Alternative Remittance Coalition: Organized agents (organized/mobile) — crypto platforms, digital banks, diaspora networks building lower-cost channels with sunset logic for traditional taxation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent taxation as inherent to sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remittance_taxation_regime, 0.58).
domain_priors:suppression_score(remittance_taxation_regime, 0.62).
domain_priors:theater_ratio(remittance_taxation_regime, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remittance_taxation_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(remittance_taxation_regime, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(remittance_taxation_regime, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remittance_taxation_regime, tangled_rope).
narrative_ontology:human_readable(remittance_taxation_regime, "Remittance Taxation Regime").
narrative_ontology:topic_domain(remittance_taxation_regime, "economic_policy/international_finance").

domain_priors:requires_active_enforcement(remittance_taxation_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remittance_taxation_regime, recipient_governments).
narrative_ontology:constraint_beneficiary(remittance_taxation_regime, financial_intermediaries).
narrative_ontology:constraint_victim(remittance_taxation_regime, migrant_workers).
narrative_ontology:constraint_victim(remittance_taxation_regime, origin_country_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRANT WORKER (SNARE) — Trapped by employment dependency, visa status tied to employer, and remittance taxation that reduces funds sent home. Exit options are minimal: leaving employment means visa revocation and deportation risk. Experiences taxation as pure extraction with high suppression — taxation is enforced through employer withholding and financial system surveillance with no meaningful alternative channels.
constraint_indexing:constraint_classification(remittance_taxation_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORIGIN COUNTRY HOUSEHOLD (TANGLED ROPE) — Constrained by economic dependency on remittances but also coordinating household resource allocation through remittance flows. The taxation regime extracts a share while providing partial coordination services: monitored transfers reduce fraud risk and create financial record trails. Moderate extraction with genuine coordination benefit — not maximal snare because some households benefit from the formalization and reduced default risk.
constraint_indexing:constraint_classification(remittance_taxation_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RECIPIENT GOVERNMENT (ROPE) — Experiences the regime as coordination: formalizing remittance flows through taxation creates a documented money supply, enables currency stability, and generates tax revenue for public goods. Benefits from arbitrage between formal and informal channels — can collect taxes while offering formal transfer services. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(remittance_taxation_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FINANCIAL INTERMEDIARY (TANGLED ROPE) — Benefits from formal remittance channels through transaction fees and compliance infrastructure contracts. Also coordinating settlement risk reduction and compliance with global AML/KYC standards. Powerful and mobile — can arbitrage between regulatory regimes. Experiences mixed extraction and coordination: required compliance overhead is significant but captures substantial rents from the formalization process.
constraint_indexing:constraint_classification(remittance_taxation_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE REMITTANCE COALITION (SCAFFOLD) — Organized actors (blockchain fintech, cooperative credit unions, diaspora networks) are building parallel remittance channels with lower taxation and higher efficiency. The traditional regime has a sunset clause embedded in technology — as cryptocurrency remittances mature and cross-border digital banking becomes cheaper, the extraction mechanism loses force. Low effective extraction because organized actors have exit pathways and can arbitrage toward lower-cost channels.
constraint_indexing:constraint_classification(remittance_taxation_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some taxation of cross-border flows is inherent to sovereign fiscal authority: governments must tax flows within their jurisdiction, and the gap between remitter income and recipient claim is a structural feature of international commerce. This perspective sees taxation as inevitable. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'sovereign taxation' naturalizes what is actually a contingent institutional choice: rates, enforcement intensity, and exemptions are policy variables, not laws of nature.
constraint_indexing:constraint_classification(remittance_taxation_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remittance_taxation_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remittance_taxation_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remittance_taxation_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(remittance_taxation_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remittance_taxation_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime extracts a measurable share of remittances through taxation, fees, and compliance overhead — rising from 0.35 at interval start to 0.58 at interval end. The trajectory reflects tightening compliance enforcement (AML/KYC) and widening tax net as more jurisdictions formally tax remittance flows. The extraction is not maximal (0.72+) because some households voluntarily use formal channels for fraud protection benefits, and organized intermediaries can arbitrage to lower-cost jurisdictions. Suppression (0.62): Moderate-high. Significant barriers include employment visa dependency, wage withholding enforcement, financial system surveillance, and limited access to informal alternatives in developed-country origin labor markets. However, suppression is not maximal because informal channels (hawala, family transfer, cryptocurrency) provide partial escape — suppression is sustained through enforcement intensity, not absolute barrier closure. Theater ratio (0.48): Moderate-low and declining. The regime's coordination function is relatively genuine — formal remittance flows do reduce fraud risk and enable fiscal capacity — but performance is partially theater. AML/KYC compliance is partly performative (checking for terrorist financing in a system where the dominant flow is household support). The declining trajectory reflects fintech platforms and digital banking reducing the performative overhead as technology automates compliance checking.
 *
 * PERSPECTIVAL GAP:
 *   The migrant's snare experience diverges sharply from the government's rope experience. The government sees coordination success (monetary stability, fiscal revenue, documented flows); the migrant sees extraction (taxation + employment dependency). This gap is not a measurement error — it reflects real structural position differences. The migrant has zero negotiating power over the regime; the government designed it. The gap also reveals the constraint is partially illegitimate: if the coordination value were genuinely distributed, the migrant would perceive shared benefit, not pure extraction. The declining theater ratio (0.52 → 0.48) suggests technology is making the coordination function more genuine (automated compliance instead of performative review), which paradoxically may intensify extraction perception among migrants because the performative burden is removed but the taxation remains.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (ε = 0.58) × f(d) × σ(S). Migrant workers with d ≈ 0.95 and local/regional scope experience χ in the 0.72-0.85 range (high). Government with d ≈ 0.05 and national/global scope experiences negative χ (net benefit). Households with d ≈ 0.55 experience χ in the 0.45-0.65 range (moderate). Financial intermediaries with d ≈ 0.25 experience χ in the 0.20-0.35 range (low). Organized alternatives with d ≈ 0.10 experience χ in the 0.15-0.30 range (low). The analytical observer's d ≈ 0.72 and universal scope yields χ in the 0.65-0.75 range. These derived χ values determine how each perspective's classification type (snare, rope, tangled rope, scaffold) emerges from the chi thresholds.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is classified as tangled_rope because it exhibits both genuine coordination (formal remittance flows reduce fraud, enable monetary policy) and asymmetric extraction (migrant workers bear taxation cost with minimal benefit). The coordination function is structural — not mere theater or cover story — which distinguishes this from a pure snare. Origin-country governments genuinely design the regime to formalize flows and capture tax revenue. The extraction is real — not accidental overhead — which distinguishes this from a pure rope. The mandatrophy resolves by acknowledging that the constraint solves a real coordination problem while simultaneously enabling extraction from those with no exit options. This is the core definition of tangled rope: hybrid coordination/extraction where both functions are structurally necessary to the regime's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tax_incidence_empirical_split,
    'What proportion of remittance taxation is borne by migrants (incidence) versus absorbed by intermediaries?',
    'Econometric analysis of remittance flows before/after tax implementation; comparison of wage adjustments and transfer volumes; supply-side elasticity estimates for intermediaries',
    'If migrants bear > 80% of incidence: snare classification strengthened. If incidence splits 50/50: tangled_rope strengthened. If intermediaries absorb > 70%: rope classification for government strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_incidence_empirical_split, empirical, 'Tax incidence split between migrant workers and financial intermediaries').

omega_variable(
    informal_channel_substitution_rate,
    'At what taxation rate does informal (untracked) remittance substitution become the dominant flow mechanism?',
    'Ratio of formal to informal remittances as tax rates vary across countries; hawala/underground banking volume estimates; correlation between tax regime stringency and informal flow prevalence',
    'If substitution rate < 15% (formal remains dominant at high taxation): suppression classification sustained. If > 40%: suppression overstated — high exit to informal channels means effective suppression is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_channel_substitution_rate, empirical, 'Substitution threshold to informal remittance channels').

omega_variable(
    fintech_displacement_timeline,
    'What is the plausible sunset timeline for traditional remittance taxation as blockchain and digital banking mature?',
    'Adoption rates for crypto remittances and cross-border digital banking; regulatory trajectory for decentralized finance; cost-per-transaction comparison with forecast curves',
    'If sunset < 5 years: scaffold classification confirmed — regime is genuinely temporary. If > 20 years: scaffold classification premature — organized alternatives will take longer to mature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fintech_displacement_timeline, empirical, 'Timeline for fintech displacement of traditional remittance taxation').

omega_variable(
    development_coordination_value,
    'Does formalized remittance documentation genuinely enable origin-country development (credit access, business capitalization) or does taxation simply redistribute to government without net development benefit?',
    'Impact evaluation of formal remittance recipients: credit access rates, business formation, educational spending; comparison with informal remittance recipients matched on observable characteristics',
    'If formal remittances generate net development benefits > tax extraction: tangled_rope confirmed (genuine coordination). If benefits < tax cost: snare classification for origin country strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_coordination_value, empirical, 'Whether formal remittance regime creates development coordination benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remittance_taxation_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remit_tr_t0, remittance_taxation_regime, theater_ratio, 0, 0.52).
narrative_ontology:measurement(remit_tr_t5, remittance_taxation_regime, theater_ratio, 5, 0.5).
narrative_ontology:measurement(remit_tr_t10, remittance_taxation_regime, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(remit_be_t0, remittance_taxation_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(remit_be_t5, remittance_taxation_regime, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(remit_be_t10, remittance_taxation_regime, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remittance_taxation_regime, resource_allocation).
narrative_ontology:affects_constraint(remittance_taxation_regime, wage_suppression_enforcement).
narrative_ontology:affects_constraint(remittance_taxation_regime, informal_economy_shadow_growth).

% DUAL FORMULATION NOTE:
% Remittance taxation is upstream of wage-suppression dynamics in labor-export economies and downstream of informal economy growth. The constraint family consists of three stories: (1) formal remittance taxation (this story, ε=0.58, tangled rope), (2) informal remittance channels substitution (ε=0.40, rope with theater), and (3) migrant wage suppression enabled by remittance dependency (ε=0.65, snare with powerful beneficiary). Each story has different ε and different beneficiary/victim declarations; they are linked by causal dependency and institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remittance_taxation_regime, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
