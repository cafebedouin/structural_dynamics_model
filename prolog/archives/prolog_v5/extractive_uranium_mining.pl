% ============================================================================
% CONSTRAINT STORY: extractive_uranium_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extractive_uranium_mining, []).

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
 *   constraint_id: extractive_uranium_mining
 *   human_readable: Extractive Uranium Mining and Resource Capture
 *   domain: resource_extraction/environmental/political_economy
 *
 * SUMMARY:
 *   Extractive uranium mining represents a canonical example of pure
 *   extraction (Snare) sustained by severe suppression of alternatives and
 *   asymmetric power. The constraint operates across multiple spatial and
 *   temporal scales: local indigenous communities and ecosystems bear
 *   immediate costs (contamination, displacement, health impacts); host state
 *   governments capture partial rents while bearing environmental liability;
 *   multinational corporations extract profits and relocate;
 *   uranium-importing states secure energy supply while externalizing health
 *   and environmental costs to production sites. The extractiveness has
 *   increased over 60 years (0.42 → 0.68) as mining techniques have scaled,
 *   resource-poor communities have been incorporated into supply chains, and
 *   cumulative waste inventories have accumulated without corresponding
 *   remediation funding. Theater ratio has remained relatively low (0.22 →
 *   0.45), indicating that suppression operates primarily through material
 *   power asymmetries and information control rather than performative
 *   legitimation — the constraint does not disguise itself as coordination
 *   but rather suppresses awareness of its true costs through geographic
 *   separation and discourse management.
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary victims (powerless/trapped) — bear concentrated local costs of extraction with no exit capacity and minimal compensation
 *   - Local Mining Workers: Secondary victims (moderate/constrained) — economically dependent on mining wages while facing occupational health hazards
 *   - Multinational Mining Corporations: Primary beneficiaries (institutional/arbitrage) — capture extraction rents and can relocate operations across jurisdictions
 *   - Host State Governments: Conflicted institutional actor (institutional/constrained) — extract tax revenue while bearing uncompensated environmental liability
 *   - Uranium-Importing States: Secondary beneficiaries (organized/constrained) — secure energy supply at externalized cost to production sites
 *   - Local Ecosystems and Downstream Users: Diffuse victims (powerless/trapped) — contamination affects water systems, agriculture, and public health across generations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals that suppression operates through geographic decoupling that prevents true-cost accounting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extractive_uranium_mining, 0.68).
domain_priors:suppression_score(extractive_uranium_mining, 0.72).
domain_priors:theater_ratio(extractive_uranium_mining, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extractive_uranium_mining, extractiveness, 0.68).
narrative_ontology:constraint_metric(extractive_uranium_mining, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(extractive_uranium_mining, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extractive_uranium_mining, snare).
narrative_ontology:human_readable(extractive_uranium_mining, "Extractive Uranium Mining and Resource Capture").
narrative_ontology:topic_domain(extractive_uranium_mining, "resource_extraction/environmental/political_economy").

domain_priors:requires_active_enforcement(extractive_uranium_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extractive_uranium_mining, multinational_mining_corporations).
narrative_ontology:constraint_beneficiary(extractive_uranium_mining, capital_investors).
narrative_ontology:constraint_beneficiary(extractive_uranium_mining, uranium_importing_states).
narrative_ontology:constraint_victim(extractive_uranium_mining, indigenous_communities).
narrative_ontology:constraint_victim(extractive_uranium_mining, local_ecosystems).
narrative_ontology:constraint_victim(extractive_uranium_mining, nuclear_waste_bearing_populations).
narrative_ontology:constraint_victim(extractive_uranium_mining, downstream_water_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Trapped in place by geography, cultural attachment to ancestral lands, and lack of alternative economic opportunities. Bear full costs of uranium extraction (water contamination, radiation exposure, ecosystem destruction) with no meaningful consent or benefit-sharing. Cannot exit without abandoning identity and territory. Maximum extraction and maximum suppression.
constraint_indexing:constraint_classification(extractive_uranium_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL MINING WORKERS (SNARE) — Constrained by economic dependency and regional labor market limitations. Face high occupational exposure to radiation and physical hazards. Career mobility is low due to skill specificity and geographic isolation of mining operations. High extraction through wage suppression and unsafe working conditions justified by economic necessity.
constraint_indexing:constraint_classification(extractive_uranium_mining, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MULTINATIONAL MINING CORPORATIONS (ROPE) — Experience extraction mechanism as coordination of resource supply chains. Benefit from favorable concession agreements, capital mobility, and ability to externalize environmental costs. Can exit specific jurisdictions and relocate operations. Effective arbitrage across regulatory regimes.
constraint_indexing:constraint_classification(extractive_uranium_mining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: URANIUM-IMPORTING STATES (TANGLED ROPE) — Dependent on uranium for energy security but constrained by nuclear proliferation treaties and fossil fuel transition goals. Experience genuine coordination problem (energy supply) mixed with asymmetric extraction (security dependence on supplier states, outsourced environmental costs). Active enforcement through nuclear trade agreements and supplier relationships.
constraint_indexing:constraint_classification(extractive_uranium_mining, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HOST STATE GOVERNMENTS (TANGLED ROPE) — Extract revenue and employment from uranium mining while bearing responsibility for environmental remediation and public health. Constrained by fiscal pressure, limited enforcement capacity, and corporate legal strategies. Genuine coordination function (managing public resources) mixed with asymmetric extraction (corporations capture rents through tax minimization and weak regulations).
constraint_indexing:constraint_classification(extractive_uranium_mining, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Global nuclear system is structurally dependent on uranium extraction externalities. The constraint persists because the costs are borne by dispersed, powerless agents across time (radioactive waste half-lives span millennia) and geography (contamination crosses borders through water and atmosphere). The analytical view reveals suppression through information asymmetry: true costs of waste management and environmental remediation are systematically underestimated in cost-benefit analyses.
constraint_indexing:constraint_classification(extractive_uranium_mining, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extractive_uranium_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extractive_uranium_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extractive_uranium_mining, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(extractive_uranium_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(extractive_uranium_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, and increasing over the interval. The constraint extracts resource wealth from host countries while externalizing environmental and health costs. The increase from 0.42 to 0.68 reflects that mining scale has increased and that remediation obligations have accumulated faster than funding. Suppression (0.72): Very high and structural. Multiple mechanisms: (1) Geographic separation — mining occurs in poor, remote regions where political voice is weak; (2) Information asymmetry — true costs of waste storage and health impacts are systematically underestimated; (3) Economic dependency — alternative livelihoods are suppressed through land use control and lack of economic diversification; (4) Legal asymmetry — corporate structures enable profit extraction while diffusing liability. Theater ratio (0.45): Moderate and increasing. Early mining periods had minimal environmental discourse (theater_ratio 0.22); as awareness of impacts grew, more performative mechanisms emerged (community consultation, environmental impact assessments, corporate social responsibility programs) that do not alter underlying extraction structures. Theater has increased to 0.45 but remains below 0.70 — the constraint does not depend primarily on theatrical legitimation but on hard suppression through power imbalances.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is extreme and reflects stark power asymmetries. Multinational corporations see Rope (coordination of supply chains) — the constraint solves their logistics problems. Host states see Tangled Rope (genuine revenue need mixed with asymmetric extraction). Uranium importers see Tangled Rope (energy need mixed with outsourced costs). Local workers see Snare (economic trap). Indigenous communities see Snare (territorial and existential trap). The analytical observer sees Snare (pure extraction masked by geographic decoupling). The gap between the beneficiary perspective (Rope) and the victim perspective (Snare) is maximal — they are describing the same structural mechanism from incompatible positions. This gap is diagnostic of severe suppression and power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from beneficiary/victim declarations and exit options. Multinational corporations (institutional/arbitrage) have d ≈ 0.05 — they are beneficiaries with maximum mobility, experiencing negative effective extraction. Host state governments (institutional/constrained) have d ≈ 0.55 — they benefit from tax revenue but are constrained by path-dependency and limited enforcement capacity, experiencing moderate extraction. Uranium-importing states (organized/constrained) have d ≈ 0.50 — they benefit from energy supply but are constrained by supply chain dependencies. Indigenous communities (powerless/trapped) have d ≈ 0.95 — they are pure victims with no exit options, experiencing maximum extraction and maximum f(d). Mining workers (moderate/constrained) have d ≈ 0.70 — constrained by economic dependency but with some occupational mobility potential. The analytical observer (analytical/analytical) has d ≈ 0.72, revealing that the constraint structurally benefits distant consumers at the cost of proximate producers — a canonical extraction geometry.
 *
 * MANDATROPHY ANALYSIS:
 *   Extractive uranium mining does NOT suffer from mandatrophy — the classification is consistently Snare across all perspectives except those of the direct beneficiaries. The constraint does not hide itself as coordination; it hides through geographic and informational distance. The mandatrophy would arise if uranium importing states believed their own nuclear energy narrative (clean, necessary energy requiring minimal environmental cost) — but that is a separate constraint about information suppression and discourse control, not part of the uranium mining constraint itself. The mining constraint is unambiguous: it is extraction. The ambiguity lies in whether consuming states are aware of the true costs, which is a different question about epistemic access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_accounting,
    'What is the true lifecycle cost of uranium extraction when full environmental remediation, perpetual waste storage, and health externalities are included?',
    'Long-term environmental impact assessments; epidemiological studies of mining communities; cost accounting that includes 10,000-year waste containment periods',
    'If true costs exceed stated economic benefits by >50%: constraint reclassifies as pure extraction (Snare) from host state perspective. If true costs are comparable to benefits: coordination function becomes more legible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_accounting, empirical, 'Whether true lifecycle costs exceed stated economic benefits').

omega_variable(
    indigenous_consent_capacity,
    'Do formal consultation mechanisms constitute meaningful consent or performative legitimation of predetermined mining decisions?',
    'Analysis of documented cases where indigenous communities successfully blocked or materially modified uranium mining projects; comparison of outcomes in Free, Prior, and Informed Consent (FPIC) frameworks vs. discretionary consultation',
    'If FPIC enables genuine exit: reclassify as constrained rather than trapped for indigenous communities. If FPIC is theater: confirms maximum suppression and pure snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_consent_capacity, empirical, 'Whether indigenous consent mechanisms are substantive or performative').

omega_variable(
    energy_security_necessity,
    'Is uranium mining necessary for decarbonization and energy security, or does renewable energy substitution make the extraction constraint obsolete within 30 years?',
    'Modeling of global energy transition pathways; cost comparison of nuclear vs. renewable infrastructure; temporal projection of peak uranium demand',
    'If mining is necessary: validates coordination function in host state and importer perspectives. If renewable substitution is viable: reveals constraint as maintaining extraction mechanism that serves historical path-dependency rather than current necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_security_necessity, empirical, 'Whether uranium mining is energy-necessary or path-dependent').

omega_variable(
    remediation_liability_escape,
    'Do corporate liability structures enable permanent escape from environmental remediation obligations through bankruptcy, subsidiary dissolution, or regime change?',
    'Tracking of corporate entities responsible for historical mining sites; analysis of cases where remediation has been abandoned or transferred to public sector; review of bonding and escrow requirements',
    'If escape mechanisms are systematic: confirms asymmetric extraction (corporations extract resources, public bears perpetual costs). If accountability is enforced: mixed extraction-coordination dynamic becomes more credible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remediation_liability_escape, empirical, 'Whether corporate liability structures enable remediation escape').

omega_variable(
    spatial_decoupling_awareness,
    'Do uranium-importing states deliberately suppress awareness of extraction and contamination costs in source countries to maintain political acceptability of nuclear power?',
    'Discourse analysis of nuclear policy communications in importing states; comparison of public awareness of uranium mining impacts in mining vs. consuming countries; analysis of educational curricula and media coverage',
    'If suppression is systematic: confirms snare classification from analytical perspective (information asymmetry prevents meaningful consent). If impacts are transparent: coordination narrative becomes more credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spatial_decoupling_awareness, conceptual, 'Whether importing states suppress awareness of extraction costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extractive_uranium_mining, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eum_tr_t0, extractive_uranium_mining, theater_ratio, 0, 0.22).
narrative_ontology:measurement(eum_tr_t20, extractive_uranium_mining, theater_ratio, 20, 0.35).
narrative_ontology:measurement(eum_tr_t40, extractive_uranium_mining, theater_ratio, 40, 0.45).
narrative_ontology:measurement(eum_tr_t60, extractive_uranium_mining, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(eum_be_t0, extractive_uranium_mining, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eum_be_t20, extractive_uranium_mining, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(eum_be_t40, extractive_uranium_mining, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(eum_be_t60, extractive_uranium_mining, base_extractiveness, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extractive_uranium_mining, resource_allocation).
narrative_ontology:affects_constraint(extractive_uranium_mining, nuclear_waste_perpetual_storage).
narrative_ontology:affects_constraint(extractive_uranium_mining, indigenous_land_dispossession).
narrative_ontology:affects_constraint(extractive_uranium_mining, radioactive_contamination_transboundary).

% DUAL FORMULATION NOTE:
% Uranium mining is upstream of nuclear waste management, which is a separate constraint with its own extractiveness (perpetual public cost, concentrated private benefit). The mining constraint also feeds into indigenous land dispossession constraints specific to particular regions (Australian Aboriginal lands, Niger, Namibia, Kazakhstan). Network decomposition is appropriate because the ε values differ: mining extraction (0.68) is distinct from waste perpetuity (0.85+) and land dispossession (0.72+).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(extractive_uranium_mining, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
