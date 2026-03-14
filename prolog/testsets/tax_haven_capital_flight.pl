% ============================================================================
% CONSTRAINT STORY: tax_haven_capital_flight
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tax_haven_capital_flight, []).

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
 *   constraint_id: tax_haven_capital_flight
 *   human_readable: Tax Haven Capital Flight and Revenue Extraction
 *   domain: economic_policy/taxation
 *
 * SUMMARY:
 *   Tax haven capital flight operates as a snare for trapped domestic
 *   taxpayers and constrained sovereign states, extracting revenue from the
 *   base while benefiting multinational corporations with arbitrage
 *   optionality. The constraint exhibits the full range of DR types across
 *   observer positions: trapped citizens experience pure extraction (snare);
 *   nation-states experience constrained extraction with some coordination
 *   function (tangled rope); multinationals experience coordination (rope);
 *   coalitions like OECD perceive both coordination and extraction (tangled
 *   rope); the international tax architecture sees its own performative role
 *   degraded (piton); the analytical observer risks naturalizing a contingent
 *   institutional arrangement as inherent to capital mobility (false
 *   mountain). The theater ratio (0.58) reflects regulatory theater — BEPS
 *   initiatives, transfer pricing documentation, country-by-country reporting
 *   — that creates appearance of enforcement while capital flight continues
 *   through legal structures. Extractiveness has increased over 20 years
 *   (0.42 → 0.68) as digital capital and IP licensing have outpaced
 *   enforcement mechanisms. The suppression value (0.72) captures the high
 *   barriers to exit for trapped citizens (emigration required) and the
 *   policy race-to-bottom for constrained states (competitive pressure
 *   against capital controls or rate increases).
 *
 * KEY AGENTS:
 *   - Domestic Citizens and Small Businesses: Primary victims (powerless/trapped) — cannot exit tax system without emigrating, face increasing tax burden as capital flies
 *   - Sovereign States: Secondary victims (moderate/constrained) — cannot easily enforce higher rates without capital flight acceleration; face fiscal squeeze and service cuts
 *   - Multinational Corporations: Primary beneficiaries (institutional/arbitrage) — capture tax avoidance through transfer pricing, IP licensing, and haven structures; experience constraint as coordination
 *   - Wealth Concentrators: Secondary beneficiaries (powerful/arbitrage) — benefit from capital mobility and tax avoidance strategies unavailable to salaried workers
 *   - Financial Intermediaries: Tertiary beneficiaries (institutional/arbitrage) — earn fees from tax haven structures, estate planning, and capital flight facilitation
 *   - Coalition of Nation-States: Organized agent (organized/constrained) — perceives both coordination problem (harmonized rates) and extraction pressure from defection incentives
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks seeing capital mobility as natural law rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tax_haven_capital_flight, 0.68).
domain_priors:suppression_score(tax_haven_capital_flight, 0.72).
domain_priors:theater_ratio(tax_haven_capital_flight, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tax_haven_capital_flight, extractiveness, 0.68).
narrative_ontology:constraint_metric(tax_haven_capital_flight, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tax_haven_capital_flight, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tax_haven_capital_flight, snare).
narrative_ontology:human_readable(tax_haven_capital_flight, "Tax Haven Capital Flight and Revenue Extraction").
narrative_ontology:topic_domain(tax_haven_capital_flight, "economic_policy/taxation").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tax_haven_capital_flight, multinational_corporations).
narrative_ontology:constraint_beneficiary(tax_haven_capital_flight, wealth_concentrators).
narrative_ontology:constraint_beneficiary(tax_haven_capital_flight, financial_intermediaries).
narrative_ontology:constraint_victim(tax_haven_capital_flight, sovereign_states).
narrative_ontology:constraint_victim(tax_haven_capital_flight, public_services).
narrative_ontology:constraint_victim(tax_haven_capital_flight, low_income_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED TAX BASE — Individual citizens and small domestic businesses cannot exit the domestic tax system without emigrating. They face full tax obligation on earned income while witnessing capital flight reduce the tax base, forcing higher rates or service cuts. Experiences extraction as snare: trapped in system, no alternatives, maximum experienced coercion.
constraint_indexing:constraint_classification(tax_haven_capital_flight, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOVEREIGN STATE (CONSTRAINED) — Cannot easily exit the system without eliminating source of capital, yet can only constrain capital flight at high cost (capital controls, brain drain acceleration). Faces genuine extraction: revenue loss, service reduction, fiscal pressure. Cannot adopt competitive tax policy without race-to-bottom dynamics.
constraint_indexing:constraint_classification(tax_haven_capital_flight, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL CORPORATION (ROPE) — Experiences the constraint as coordination mechanism: transfer pricing and haven strategies allocate capital efficiently across jurisdictions, minimize global tax drag. Benefits from first-mover advantage in structuring. Experiences constraint as pure coordination with no coercion — full arbitrage optionality.
constraint_indexing:constraint_classification(tax_haven_capital_flight, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COALITION OF NATION-STATES (ORGANIZED) — Sees capital flight as both coordination problem (need harmonized minimum tax rates) and extraction mechanism (enforcement cost, race-to-bottom pressure). OECD/G20 coalitions perceive genuine coordination function (Pillar Two agreements) alongside asymmetric extraction from smaller states unable to enforce higher rates. Constrained by defection incentives.
constraint_indexing:constraint_classification(tax_haven_capital_flight, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL TAX ARCHITECTURE (PITON) — Post-WW2 legal framework (arm's-length principle, permanent establishment doctrine) designed for industrial-era manufacturing is now largely ceremonial for digital capital. Regulatory theater persists (transfer pricing documentation, BEPS initiatives) but functional efficacy is degraded — the architecture cannot prevent capital flight from digital services, data flows, or IP licensing. Maintained through institutional inertia despite acknowledged inadequacy.
constraint_indexing:constraint_classification(tax_haven_capital_flight, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From universal perspective, capital is mobile and taxation is territorial: there is an irreducible structural tension between the mobility of wealth and the territorial basis of taxation. This perspective risks naturalizing a contingent institutional arrangement (the nation-state tax system) as a law of nature. However, the structural data (high suppression, behavioral extraction, coordination by some agents) contradicts full mountain classification — the engine will detect this as false summit.
constraint_indexing:constraint_classification(tax_haven_capital_flight, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tax_haven_capital_flight_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tax_haven_capital_flight, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tax_haven_capital_flight, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tax_haven_capital_flight, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tax_haven_capital_flight, TR),
    TR >= 0.70.

:- end_tests(tax_haven_capital_flight_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significant revenue from domestic tax bases — estimates suggest $21-32 trillion in offshore wealth and $427-605 billion annual tax loss. The extraction is not total (some capital remains, some multinationals accept higher tax burdens) but substantial and growing. The 20-year trajectory (0.42 → 0.68) reflects digitalization outpacing enforcement — IP licensing, data flows, and software licensing are easier to relocate than manufacturing facilities. Suppression (0.72): High. Multiple barriers prevent exit: citizens must emigrate to escape domestic tax obligation; states face capital flight if they raise rates unilaterally; multinationals face political pressure and reputational cost for havens (increasingly), but the barriers remain surmountable through legal structuring. The suppression reflects both legal constraints (tax treaties that facilitate capital mobility) and practical constraints (enforcement costs). Theater ratio (0.58): Moderate-high. Significant regulatory theater: BEPS initiatives generate compliance documents, transfer pricing documentation creates appearance of scrutiny, country-by-country reporting mandates create data flows — but functional enforcement remains limited. Digital capital flows through IP licensing and data licensing in ways that legal documentation cannot effectively track or prevent. The theater has increased over time as regulators respond to capital flight with more documentation requirements rather than structural changes.
 *
 * PERSPECTIVAL GAP:
 *   Multinational corporations (institutional/arbitrage) see rope because the constraint solves their coordination problem: how to legally minimize global tax liability while satisfying multiple jurisdictional requirements. They experience low suppression (legal structures are available, compliance cost is manageable). Trapped citizens (powerless/trapped) see snare because the constraint forces them to bear increasing fiscal burden with no escape. They experience maximum suppression (cannot exit without emigration). Constrained states (moderate/constrained) see tangled rope because capital flight creates both a coordination problem (need harmonized minimum rates) and an extraction mechanism (small states forced to compete with revenue reductions). The OECD coalition (organized/constrained) bridges these: they perceive the coordination problem clearly (Pillar Two as solution) but face extraction from defection incentives (small states or havens that break ranks). The piton perspective reveals that regulatory theater (BEPS, transfer pricing) has replaced functional enforcement: states create documentation and reporting requirements that satisfy political constituencies (appearing to 'do something') while the underlying capital mobility remains unchanged.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by whether agents are net beneficiaries or victims of capital flight. Multinationals with arbitrage optionality derive low d (0.1-0.2): they benefit from the constraint and have high exit optionality (can relocate operations, change structures). Trapped citizens derive high d (0.90-0.98): they bear extraction cost and cannot exit without emigration. Constrained states derive moderate-high d (0.65-0.75): they bear significant extraction cost (revenue loss, service cuts) but have some agency (can attempt enforcement, negotiate coalitions). The directionality chain feeds into chi = ε × f(d) × σ(S): beneficiaries experience low χ (extraction runs toward them); victims experience high χ (extraction runs away from them). At global scope, σ(S)=1.2 amplifies extractiveness for all agents, reflecting that capital flight is a planetary-scale coordination failure.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint genuinely exhibits snare classification when examined from the perspective of trapped victims (citizens, states bearing extraction cost). The coordination function perceived by beneficiaries (multinationals) is real but not dominant — it is coordination that benefits the extractors while harming the extracted-from. This is precisely what tangled rope describes: genuine coordination (capital allocation, efficient global structuring) alongside asymmetric extraction (revenue loss, fiscal squeeze). The piton element (regulatory theater) reflects degradation: when BEPS and transfer pricing standards fail to prevent capital flight, enforcement shifts to documentation and reporting (theater) rather than structural change. The analytical observer's mountain (naturalizing capital mobility as inherent to capital) is a false summit — the structural data shows high suppression and behavioral extraction, not immutability. If capital mobility were truly immutable, we would see: (1) equal capital flight from all tax regimes (we don't — countries with strong enforcement and political will see less flight), (2) zero effectiveness of Pillar Two negotiations (unclear — implementation ongoing), (3) no variation by sector (we see variation: digital capital more mobile than manufacturing). The mountain perspective naturalizes a contingent institutional arrangement (asymmetric enforcement, weak international coordination) as inherent to globalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_flight_voluntary_exit,
    'Is capital flight structurally unavoidable (inherent to global financial systems) or is it an artifact of deliberate institutional design and enforcement choices?',
    'Comparative analysis of tax compliance rates in jurisdictions with strong enforcement vs weak enforcement; modeling counterfactual of fully global minimum tax enforcement with blockchain tracking',
    'If unavoidable: mountain perspective justified — constraint is natural limit of taxation. If contingent: snare classification sustained — extraction mechanism can be prevented by coordinated enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_flight_voluntary_exit, empirical, 'Whether capital flight is structurally unavoidable or contingent on enforcement').

omega_variable(
    wealth_concentration_feedback,
    'Does capital flight reinforce wealth concentration in ways that create secondary extraction mechanisms (political capture, regulatory influence)?',
    'Longitudinal analysis of campaign finance, lobbying intensity, and regulatory capture correlating with capital gains from tax havens; measurement of wealth concentration elasticity to tax haven availability',
    'If feedback loop confirmed: snare is part of larger constraint family (tax_haven_political_capture); extractiveness may be understated at 0.68. If feedback weak: snare is isolated extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_concentration_feedback, empirical, 'Whether capital flight creates secondary political extraction feedback').

omega_variable(
    pillar_two_enforcement_credibility,
    'Can OECD Pillar Two (15% global minimum tax) create stable coordination without defection incentives, or does it require ongoing costly enforcement?',
    'Monitoring of tax haven defection from Pillar Two commitments; measurement of effective minimum tax rates across jurisdictions post-implementation; analysis of resource required for compliance verification',
    'If coordination succeeds: constraint shifts from snare toward tangled_rope or scaffold (sunset via harmonization). If defection emerges: coalition constraint becomes extraction mechanism for compliant states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pillar_two_enforcement_credibility, empirical, 'Whether Pillar Two creates stable coordination or requires ongoing enforcement').

omega_variable(
    digital_versus_physical_capital,
    'Is capital flight fundamentally different for digital/IP-based capital vs physical/manufacturing capital, suggesting two distinct constraints?',
    'Decomposition of capital flight flows by sector; comparative analysis of tax avoidance rates for digital services vs manufacturing; measurement of enforcement capacity differential',
    'If structurally distinct: write separate constraint stories (tax_haven_digital_capital_flight vs tax_haven_manufacturing_capital_flight). Current story conflates observables with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_versus_physical_capital, empirical, 'Whether digital and physical capital flight are structurally distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tax_haven_capital_flight, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taxhaven_tr_t0, tax_haven_capital_flight, theater_ratio, 0, 0.35).
narrative_ontology:measurement(taxhaven_tr_t10, tax_haven_capital_flight, theater_ratio, 10, 0.48).
narrative_ontology:measurement(taxhaven_tr_t20, tax_haven_capital_flight, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(taxhaven_be_t0, tax_haven_capital_flight, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(taxhaven_be_t10, tax_haven_capital_flight, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(taxhaven_be_t20, tax_haven_capital_flight, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tax_haven_capital_flight, resource_allocation).
narrative_ontology:affects_constraint(tax_haven_capital_flight, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(tax_haven_capital_flight, wealth_inequality).
narrative_ontology:affects_constraint(tax_haven_capital_flight, regulatory_capture_by_finance).

% DUAL FORMULATION NOTE:
% Tax haven capital flight is upstream of both sovereign debt crises (states compensate lost revenue through borrowing) and political capture (multinationals lobby against enforcement). Digital capital flight (IP/data licensing) and physical capital flight (manufacturing relocation) have distinct ε values and should be decomposed into separate stories if precision is required: tax_haven_digital_capital_flight (higher ε, harder to enforce) and tax_haven_manufacturing_capital_flight (lower ε, some enforcement success).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tax_haven_capital_flight, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
