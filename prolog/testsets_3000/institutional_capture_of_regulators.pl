% ============================================================================
% CONSTRAINT STORY: institutional_capture_of_regulators
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_capture_of_regulators, []).

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
 *   constraint_id: institutional_capture_of_regulators
 *   human_readable: Institutional Capture of Regulators
 *   domain: political_economy/regulatory_governance
 *
 * SUMMARY:
 *   Institutional capture of regulators occurs when regulatory agencies
 *   tasked with protecting the public interest become aligned with the
 *   interests of the entities they regulate. This structural phenomenon
 *   creates a constraint that operates across multiple dimensions: the
 *   regulatory agency has a genuine coordination function (reducing
 *   information asymmetries, establishing predictable rules, preventing
 *   destructive competition), yet simultaneously faces career incentives,
 *   information dependency, and revolving-door employment patterns that
 *   systematically bias it toward the regulated industry. The constraint
 *   exhibits tangled rope structure at the core — both genuine coordination
 *   and asymmetric extraction are constitutive features, not failures of
 *   implementation. The extractiveness trajectory (0.42 → 0.68 over 20 years)
 *   reflects how capture deepens through institutional inertia: initial
 *   regulatory autonomy gradually erodes as personnel turnover, industry
 *   relationship-building, and epistemic capture accumulate. Theater ratio
 *   (0.38 → 0.58) shows performative activity increasing as regulatory rules
 *   become more elaborate while their substantive enforcement capacity
 *   declines — classic Goodhart drift where the measurement (rule complexity)
 *   substitutes for the objective (consumer protection).
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — captures favorable rules, barrier-to-entry effects, and predictable regulatory treatment; maintains multiple exit options
 *   - Public Interest (Unorganized): Primary victim (powerless/trapped) — dispersed costs of lax regulation; unable to organize or exit; bears full extraction without countervailing power
 *   - Competing Firms (Non-Captured): Secondary victim (moderate/constrained) — face regulatory disadvantage relative to captured firms; high cost to challenge but some exit options through coalition, arbitrage
 *   - Captured Regulatory Agency: Institutional actor (institutional/constrained) — genuinely coordinates market behavior while being structurally incentivized toward leniency; career paths dependent on industry relationships
 *   - Reform Coalitions: Organized agents (organized/constrained) — transparency advocates, consumer groups, competing industry — perceive capture as reversible through structural reforms with sunset clauses
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees capture as structural tension inherent to any regulatory system balancing coordination needs against capture vulnerability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_capture_of_regulators, 0.68).
domain_priors:suppression_score(institutional_capture_of_regulators, 0.62).
domain_priors:theater_ratio(institutional_capture_of_regulators, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_capture_of_regulators, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_capture_of_regulators, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_capture_of_regulators, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_capture_of_regulators, tangled_rope).
narrative_ontology:human_readable(institutional_capture_of_regulators, "Institutional Capture of Regulators").
narrative_ontology:topic_domain(institutional_capture_of_regulators, "political_economy/regulatory_governance").

domain_priors:requires_active_enforcement(institutional_capture_of_regulators).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_capture_of_regulators, regulated_industry).
narrative_ontology:constraint_victim(institutional_capture_of_regulators, public_interest).
narrative_ontology:constraint_victim(institutional_capture_of_regulators, competing_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED PUBLIC INTEREST (SNARE) — Cannot exit or organize against capture. Bears full cost of lax regulation through environmental degradation, consumer harm, or financial risk. No countervailing power. Extraction is maximal from this agent's perspective; suppression operates through dispersed diffuse harm and epistemic marginalization.
constraint_indexing:constraint_classification(institutional_capture_of_regulators, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING FIRMS (TANGLED ROPE) — Face high costs to challenge regulatory capture (litigation, political pressure, reputation risk) but retain some exit options through lobbying coalitions, regulatory arbitrage to other jurisdictions, or product innovation. Experience mixed extraction and coordination through the regulatory framework itself — captured rules both harm them and provide predictability.
constraint_indexing:constraint_classification(institutional_capture_of_regulators, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Experiences capture as pure coordination benefit. The constraint creates favorable rules and barrier-to-entry effects. Can exit by changing jurisdictions or regulatory posture. Net beneficiary with high agency and multiple arbitrage options.
constraint_indexing:constraint_classification(institutional_capture_of_regulators, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPTURED REGULATORY AGENCY (TANGLED ROPE) — Institutional actor with constrained exit options. The agency genuinely coordinates market behavior and reduces information asymmetries (coordination function); simultaneously, career advancement, revolving-door incentives, and epistemic capture create asymmetric extraction flowing toward the regulated industry. Agency staff see themselves as solving legitimate coordination problems while being structurally incentivized toward leniency. Both coordination and extraction are real and simultaneous.
constraint_indexing:constraint_classification(institutional_capture_of_regulators, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-CAPTURE REFORM MOVEMENTS (SCAFFOLD) — Organized agents (transparency advocates, consumer groups, competing industry coalitions) perceive capture as a temporary governance failure with structural solutions: conflict-of-interest rules, revolving-door restrictions, transparency mandates, and independent enforcement mechanisms. These reforms explicitly have sunset logic — if implemented successfully, they reduce capture incentives. Extraction appears high (suppression via institutional inertia) but temporary because the organized coalition has agency and visibility on exit pathways.
constraint_indexing:constraint_classification(institutional_capture_of_regulators, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, institutional capture is neither purely extractive nor purely a coordination failure. All regulatory systems face the same structural tension: regulators must engage with regulated industries to understand complex technical issues (genuine coordination need) while maintaining independence (extraction prevention). The constraint is tangled because the solution to one problem (information asymmetry requiring regulator-industry dialogue) creates the vulnerability to the other problem (capture through relationship dependency). This perspective identifies the constraint as structural rather than pathological.
constraint_indexing:constraint_classification(institutional_capture_of_regulators, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_capture_of_regulators_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_capture_of_regulators, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_capture_of_regulators, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_capture_of_regulators, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(institutional_capture_of_regulators_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The regulated industry captures favorable treatment worth substantial rent, but extraction is moderated by: (1) regulatory agencies retain some autonomy and formal duty to public interest, (2) competitive pressure from non-captured firms and other jurisdictions, (3) periodic reform movements constrain the most egregious abuses. Suppression (0.62): Moderate-high. Barriers to exit include: (1) public interest groups lack resources and organizational capacity, (2) competing firms face litigation and political costs of challenging capture, (3) information asymmetries favor industry (better technical expertise, more resources), (4) revolving-door employment patterns institutionalize capture. However, suppression is not total — transparency, media scrutiny, and legislative intervention can temporarily reduce it. Theater ratio (0.58): Moderate-high and rising. Regulatory agencies produce elaborate rules and enforcement activity to maintain legitimacy, but substantive impact on industry behavior declines over time as capture deepens. The performative content increases because agencies must appear to regulate while actually accommodating industry preferences. The trajectory shows classic Goodhart drift: rule complexity increases while enforcement rigor decreases, measurement (rule proliferation) substitutes for objective (consumer protection).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (regulated industry) and captured agency see the constraint as coordination: establishing predictable rules, reducing information asymmetry, preventing wasteful competition. The unorganized public sees it as pure extraction: rules designed to benefit industry at public expense. Competing firms see mixed coordination (the regulatory framework itself is useful) and extraction (they are disadvantaged relative to captured competitors). The reform coalition sees a temporary problem with structural solutions (transparency, cooling-off rules, independent enforcement). The analytical observer sees a fundamental structural tension: genuine coordination needs (regulators must understand complex industry realities) create the vulnerability to capture (relationship dependency enables influence). The perspectival gap reveals that 'capture' is not a failure to coordinate but a consequence of how coordination is structured.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status plus exit options. The regulated industry as beneficiary with arbitrage options (d ≈ 0.15) experiences negative effective extraction — the constraint subsidizes them. The public interest as victim with no exit (trapped, d ≈ 0.95) experiences maximum effective extraction. Competing firms as victims with constrained exit (d ≈ 0.70) experience high extraction. The captured agency as institutional actor with constrained exit (d ≈ 0.45) experiences moderate extraction modulated by their mixed role as both coordinator and captured actor. The reform coalition as organized actors with constrained exit but visible alternatives (d ≈ 0.55) experience moderate extraction because they have agency and exit visibility. The directionality spread (0.15 to 0.95) explains why perspectives range from rope (beneficiary) to snare (powerless victim). The captured agency's perspective is explicitly tangled rope because their d-value (0.45) produces a sigmoid f(d) that generates both coordination benefits and asymmetric extraction simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: This constraint demonstrates that 'capture' is not mislabeled coordination or masked extraction but genuinely BOTH. The tangled rope classification is mandatory, not a compromise. The regulated industry experiences this as rope (pure coordination benefit). The public interest experiences it as snare (pure extraction). The agency experiences it as tangled rope (both coordination function and extraction incentive simultaneously constitute the institution). The mandatrophy resolution is perspectival: the classification depends on the agent's structural position. The trap of false natural law (mountain) is precisely the narrative that 'regulatory capture is inevitable and inherent to any regulatory system' — this naturalizes what is actually a contingent institutional arrangement (revolving-door employment, information asymmetries, dispersed public interest costs). The analytical observer risks this false summit; recognizing it as tangled rope (structure amenable to redesign) is the correct classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_primary_driver,
    'Is capture primarily driven by revolving-door employment incentives, ideological alignment between regulators and industry, information dependency, or organizational inertia?',
    'Temporal analysis of regulatory shift timing relative to personnel changes, ideological composition analysis of agency staff, measurement of information flow asymmetries, and comparison of rulemaking patterns with and without turnover',
    'If revolving-door driven: transparency and cooling-off rules can reduce capture. If ideologically driven: capture is more path-dependent and harder to reverse. If information-driven: structural reorganization (independent technical staff) may be required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_primary_driver, empirical, 'Primary mechanism driving regulatory capture').

omega_variable(
    coordination_necessity_threshold,
    'How much regulator-industry technical cooperation is genuinely necessary for effective regulation versus how much enables capture?',
    'Comparative analysis of regulatory outcomes across jurisdictions with different regulator-industry interaction levels; measurement of technical accuracy of rules across institutional designs',
    'If high threshold: strict separation creates information gaps and poor regulation. If low threshold: separation is viable and reduces capture. Classification shifts depending on whether coordination is genuinely necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Balance between necessary coordination and capture vulnerability').

omega_variable(
    public_interest_organization_capacity,
    'Can public interest groups achieve sufficient organizational capacity to countervail industry lobbying and alter the asymmetric power balance?',
    'Tracking organizational funding, staffing, and policy impact of consumer groups, environmental organizations, and public interest coalitions; correlation with capture reversals',
    'If capacity achievable: public interest becomes ''constrained'' or ''organized'' rather than ''powerless/trapped''. Classification of snare shifts to tangled_rope or even rope from public perspective. If capacity blocked: powerless/trapped classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_interest_organization_capacity, empirical, 'Whether public interest can organize counter-power').

omega_variable(
    regulatory_jurisdiction_exit_option,
    'How real is the exit option for regulated firms to shift to permissive jurisdictions or self-regulatory frameworks?',
    'Analysis of jurisdictional arbitrage patterns, cost-benefit analysis of regulatory shopping, measurement of effective regulatory harmonization pressure',
    'If exit is real: regulated industry has genuine arbitrage option, beneficiary position becomes less dependent on capture, constraint shifts toward rope. If exit is blocked (national market size, social license requirements): industry is trapped in jurisdiction, capture becomes more existential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_jurisdiction_exit_option, empirical, 'Feasibility of regulatory arbitrage for regulated firms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_capture_of_regulators, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capture_tr_t0, institutional_capture_of_regulators, theater_ratio, 0, 0.38).
narrative_ontology:measurement(capture_tr_t10, institutional_capture_of_regulators, theater_ratio, 10, 0.48).
narrative_ontology:measurement(capture_tr_t20, institutional_capture_of_regulators, theater_ratio, 20, 0.58).
narrative_ontology:measurement(capture_tr_t5, institutional_capture_of_regulators, theater_ratio, 5, 0.43).

% Extraction over time
narrative_ontology:measurement(capture_be_t0, institutional_capture_of_regulators, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(capture_be_t10, institutional_capture_of_regulators, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(capture_be_t20, institutional_capture_of_regulators, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(capture_be_t5, institutional_capture_of_regulators, base_extractiveness, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_capture_of_regulators, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_capture_of_regulators, 0.12).
narrative_ontology:affects_constraint(institutional_capture_of_regulators, regulatory_arbitrage_asymmetry).
narrative_ontology:affects_constraint(institutional_capture_of_regulators, public_interest_epistemic_marginalization).
narrative_ontology:affects_constraint(institutional_capture_of_regulators, revolving_door_employment_incentives).

% DUAL FORMULATION NOTE:
% Institutional capture is upstream of specific regulatory failures (environmental lax enforcement, financial oversight failure, consumer protection gaps) and downstream of systemic features (revolving-door employment, information asymmetries, organized industry vs dispersed public). Separate stories exist for specific instantiations (e.g., financial sector regulatory capture, environmental agency capture); this constraint models the general structure applicable across domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_capture_of_regulators, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
