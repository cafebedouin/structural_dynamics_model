% ============================================================================
% CONSTRAINT STORY: legacy_system_technical_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_system_technical_debt, []).

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
 *   constraint_id: legacy_system_technical_debt
 *   human_readable: Cumulative Technical Debt in Legacy Monoliths
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Technical debt in legacy monoliths represents a structural constraint
 *   where the benefits of rapid initial development and cost minimization are
 *   concentrated in executive leadership and early teams, while the costs of
 *   maintenance burden and reduced development velocity are deferred to
 *   future teams with no exit option. The constraint exhibits the full
 *   spectrum of DR types: executives experience it as coordination (Rope),
 *   maintenance engineers as pure extraction (Snare), development
 *   organizations as mixed coordination-extraction (Tangled Rope), migration
 *   initiatives as a temporary problem with a sunset (Scaffold),
 *   organizational consensus as degraded ritual (Piton), and analytical
 *   observers risk naturalizing it as an inherent feature of software (false
 *   Mountain). The extractiveness has grown from 0.18 to 0.58 over the
 *   interval as initial shortcuts compound into systemic constraints. The
 *   theater ratio has grown from 0.22 to 0.64, indicating that discussions
 *   about technical debt and modernization efforts increasingly take the form
 *   of performative planning rather than real refactoring — the gap between
 *   stated intentions and actual resource allocation reflects institutional
 *   inertia and the growing cost of change.
 *
 * KEY AGENTS:
 *   - Initial Development Team: Primary beneficiary (institutional/arbitrage) — captured rapid delivery and market advantage; distributed costs to future maintainers
 *   - Executive Leadership & Investors: Primary beneficiary (institutional/arbitrage) — benefited from low upfront costs and fast feature delivery; have exit options (divest, pivot, acquire)
 *   - Future Maintenance Teams: Primary victim (powerless/trapped) — inherit accumulating debt with no exit; bear cost of reduced development velocity and system fragility
 *   - Development Velocity: Victim (powerless/trapped) — abstract measure of system capability that degrades as debt compounds; no advocate or organization
 *   - System Reliability: Victim (powerless/trapped) — abstract quality that suffers as complexity increases and refactoring becomes riskier
 *   - Microservices Migration Initiative: Organized actor (organized/constrained) — attempting to decompose monolith; has agency but faces organizational resistance and resource constraints
 *   - Organizational Leadership: Secondary actor (institutional/arbitrage) — maintains the monolith through inertia despite rhetoric about modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_system_technical_debt, 0.58).
domain_priors:suppression_score(legacy_system_technical_debt, 0.68).
domain_priors:theater_ratio(legacy_system_technical_debt, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_system_technical_debt, extractiveness, 0.58).
narrative_ontology:constraint_metric(legacy_system_technical_debt, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legacy_system_technical_debt, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_system_technical_debt, tangled_rope).
narrative_ontology:human_readable(legacy_system_technical_debt, "Cumulative Technical Debt in Legacy Monoliths").
narrative_ontology:topic_domain(legacy_system_technical_debt, "technological/economic").

domain_priors:requires_active_enforcement(legacy_system_technical_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_system_technical_debt, initial_development_team).
narrative_ontology:constraint_beneficiary(legacy_system_technical_debt, executive_leadership).
narrative_ontology:constraint_beneficiary(legacy_system_technical_debt, short_term_shareholders).
narrative_ontology:constraint_victim(legacy_system_technical_debt, future_maintenance_teams).
narrative_ontology:constraint_victim(legacy_system_technical_debt, system_reliability).
narrative_ontology:constraint_victim(legacy_system_technical_debt, development_velocity).
narrative_ontology:constraint_victim(legacy_system_technical_debt, emerging_feature_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAINTENANCE ENGINEER (SNARE) — Trapped in the monolith with no exit. Bears the cumulative cost of every shortcut decision made by previous teams. Cannot refactor without catastrophic system risk; cannot leave without abandoning career investment. High experienced extraction with maximum suppression.
constraint_indexing:constraint_classification(legacy_system_technical_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DEVELOPMENT ORGANIZATION (TANGLED ROPE) — Benefits from fast initial delivery and reduced upfront costs; suffers from compounding maintenance burden and velocity degradation. Constrained exit: rewriting from scratch is career-ending, but status quo is unsustainable. Mixed coordination (shared codebase enables features) and extraction (shortcuts externalize future costs).
constraint_indexing:constraint_classification(legacy_system_technical_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP & INVESTORS (ROPE) — Benefits from rapid feature delivery and cost minimization during growth phase. Experiences the constraint as coordination: sharing a stable monolith enables scaling. Can arbitrage between systems (divest, acquire, pivot). Net beneficiary — extraction runs toward this agent during the immediate horizon.
constraint_indexing:constraint_classification(legacy_system_technical_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MICROSERVICES MIGRATION INITIATIVE (SCAFFOLD) — Organized refactoring efforts see technical debt as a temporary problem with a sunset: gradual service extraction, domain-driven redesign, and containerization create exit paths. High suppression during transition (massive resource cost) but declining as services decouple. Theater ratio is high initially (many failed migrations) but drops as patterns mature. Has sunset: 3-5 year horizon to decouple critical dependencies.
constraint_indexing:constraint_classification(legacy_system_technical_debt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MONOLITHIC ARCHITECTURE CONSENSUS (PITON) — Historical norm of 'build everything in one codebase' persists through institutional inertia despite acknowledged dysfunction. Debates about monolith vs. microservices are largely performative — many organizations claim to support microservices while defending monolithic production systems. Theater ratio (0.64) reflects the gap between rhetoric (we need to modernize) and practice (we cannot afford to break the monolith). Function has atrophied but the constraint remains.
constraint_indexing:constraint_classification(legacy_system_technical_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some technical debt is inherent to software: every design choice forecloses alternatives, complexity compounds over time, and the gap between initial architecture and evolved requirements is an inevitable feature of systems that survive their original scope. However, the structured data reveals this as a false summit: the magnitude of extraction (0.58) and suppression (0.68) are contingent on institutional choices (decision velocity requirements, funding pressure, team turnover), not universal law.
constraint_indexing:constraint_classification(legacy_system_technical_debt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_system_technical_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_system_technical_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_system_technical_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_system_technical_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_system_technical_debt, TR),
    TR >= 0.70.

:- end_tests(legacy_system_technical_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting significant asymmetry between beneficiaries and victims. Initial developers and executives captured benefits (fast delivery, low costs) while future teams bear costs (maintenance burden, constrained innovation). The value increased from 0.18 to 0.58 over the interval as shortcuts compounded — early extractiveness was lower because the debt was not yet severe; as the monolith aged, extraction became more apparent. Suppression (0.68): High, reflecting substantial barriers to exit. Maintenance teams cannot refactor without catastrophic risk; they cannot leave without abandoning career investment. The monolith's technical criticality (real or perceived) creates suppression through fear of system failure. Rewriting from scratch is organizationally infeasible. Theater ratio (0.64): Moderate-high, reflecting the gap between modernization rhetoric and actual resource allocation. Organizations debate microservices and cloud-native architecture while production monoliths remain unchanged. Planning exercises, architecture reviews, and pilot projects create the appearance of addressing technical debt without committing resources to actual decomposition. The ratio increased from 0.22 to 0.64 because the performative activity (strategic planning, RFPs, vendor evaluations) accelerated while actual refactoring capacity remained flat.
 *
 * PERSPECTIVAL GAP:
 *   Executives and early teams experience the monolith as a coordination mechanism that enabled growth — the shared codebase solved the problem of rapid feature delivery at scale. They have low experienced extraction because they could arbitrage out (exit to new opportunities, divest, pivot). Maintenance teams experience the same system as a Snare — trapped by technical criticality and career sunk costs, with no exit option and compounding costs. Development organizations experience a Tangled Rope: the monolith enables feature delivery (coordination benefit) but increasingly constrains it (extraction cost). The open question — resolvable by omega_1 — is whether this constraint maintains its Snare classification for maintenance teams or whether scaffold-perspective initiatives (microservices, gradual decomposition) actually create exit paths that would shift it to constrained-with-agency (Tangled Rope from the maintenance perspective). The analytical observer's Mountain perspective naturalizes the constraint ('all software accumulates debt') but the structured data reveals this as false — the magnitude of extraction is contingent on organizational choices about refactoring investment, team retention, and debt transparency.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chains from beneficiary/victim declarations and exit options. Executives and initial teams are beneficiaries with arbitrage exit — they benefit from fast delivery and can leave the system for new opportunities, producing low d (~0.15) and negative f(d) (~-0.01). Maintenance teams are victims trapped in the monolith with no exit — they bear compounding costs and cannot reorganize to escape, producing high d (~0.95) and high f(d) (~1.42). Development organizations occupy a middle position (moderate power, constrained exit) — they both benefit from the monolith's coordination function and suffer from its constraints, producing moderate d (~0.65) and moderate f(d) (~1.00). The organized migration initiative has agency and constrained (not trapped) exit — they can pursue gradual decomposition at resource cost but not zero cost, producing moderate d (~0.55) and moderate f(d) (~0.75). The analytical perspective is observer-external, producing d ~0.72 and f(d) ~1.15, but the natural law framing risks underweighting the extraction asymmetry by treating it as inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is genuinely Tangled Rope (not misclassified as pure Rope or pure Snare) because it exhibits both coordination and extraction in structural detail. COORDINATION FUNCTION: The monolith solves a real coordination problem — shared codebase enables teams to build integrated features without constant integration overhead. Early versions provide genuine benefit (fast delivery, low duplication, unified data model). ASYMMETRIC EXTRACTION: Executive beneficiaries and initial developers captured the benefits of rapid delivery and market entry while deferring costs to future teams with no compensation or exit option. The cost structure is deliberately opaque — technical debt is not capitalized as a liability, so the true burden remains invisible to financial oversight (omega_3). ACTIVE ENFORCEMENT: The constraint is maintained through organizational pressure to preserve production stability, career risk for proposing major refactoring, and the sunk-cost fallacy ('we've already invested so much in this system'). Exit paths exist (microservices, strangler pattern, gradual decomposition) but are actively suppressed by resource constraints and organizational inertia. OMEGA RESOLUTION: The mandatrophy is fully resolved by showing that: (1) the coordination function is real but declining as the monolith ages; (2) the extraction is real and asymmetric, with beneficiaries having exit options and victims trapped; (3) the active enforcement is social/organizational, not technical, meaning scaffold-perspective initiatives with institutional commitment could actually succeed; (4) the theater ratio's growth indicates that the constraint is transitioning toward Piton (performative modernization without real change) or toward Scaffold (with genuine sunset if leadership commits resources). The classification depends on which institutional dynamics dominate next — whether refactoring becomes a real priority or remains a strategic aspiration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refactoring_opportunity_cost,
    'What is the true cost of parallel refactoring vs. the perceived cost of production risk and slowed feature velocity?',
    'Comparative analysis: organizations that invested 20-30% of capacity in gradual refactoring vs. those that minimized refactoring; longitudinal tracking of feature velocity, bug rates, and developer retention',
    'If refactoring cost < feature delay cost: the constraint is maintenance theater (Piton), not structural necessity. If refactoring cost > feature delay cost: the constraint is genuine coordination (Rope/Tangled Rope with sunset dynamics). Classification shifts between Scaffold (with exit path) and Snare (with no exit) depending on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refactoring_opportunity_cost, empirical, 'Cost comparison between parallel refactoring and feature velocity loss').

omega_variable(
    criticality_of_monolith_boundaries,
    'Is the apparent criticality of maintaining monolith integrity a true technical requirement or a social/organizational artifact (team ownership boundaries, deployment coupling, knowledge silos)?',
    'Analysis of failure propagation in monoliths with partial decoupling (async messaging, feature flags, strangler pattern implementation); comparison with organizations that successfully decomposed similar systems; measurement of actual vs. perceived coupling',
    'If criticality is technical: the constraint has high suppression due to genuine risk (Mountain view is partially correct). If criticality is organizational: suppression is inflated by culture and process (Scaffold perspective is valid — exit path exists but requires social change, not just technical change). Determines whether maintenance engineers are trapped (Snare) or constrained with agency (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criticality_of_monolith_boundaries, empirical, 'Whether monolith criticality is technical necessity or organizational artifact').

omega_variable(
    debt_capitalization_vs_expensing,
    'If technical debt were properly capitalized as a liability (instead of hidden in development velocity loss), would the extraction mechanism become visible to financial oversight and trigger institutional constraints on debt accumulation?',
    'Experimental: frame technical debt as balance-sheet liability; model expected refactoring costs; present to executive stakeholders with same scrutiny applied to financial debt. Observe whether visibility changes decision-making.',
    'If visibility triggers constraints: executive beneficiaries are practicing extraction through information asymmetry (Snare classification shifts to Tangled Rope with active oversight). If visibility produces no change: beneficiaries have pricing power independent of debt transparency (Rope classification confirmed — coordination benefit outweighs extraction cost). Determines mandatrophy resolution pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_capitalization_vs_expensing, conceptual, 'Whether debt capitalization would change stakeholder behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_system_technical_debt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legacydebt_tr_t0, legacy_system_technical_debt, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legacydebt_tr_t5, legacy_system_technical_debt, theater_ratio, 5, 0.43).
narrative_ontology:measurement(legacydebt_tr_t10, legacy_system_technical_debt, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(legacydebt_be_t0, legacy_system_technical_debt, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(legacydebt_be_t5, legacy_system_technical_debt, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(legacydebt_be_t10, legacy_system_technical_debt, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_system_technical_debt, resource_allocation).
narrative_ontology:affects_constraint(legacy_system_technical_debt, agile_velocity_paradox).
narrative_ontology:affects_constraint(legacy_system_technical_debt, knowledge_silos_in_large_systems).
narrative_ontology:affects_constraint(legacy_system_technical_debt, organizational_coupling_through_code).

% DUAL FORMULATION NOTE:
% Technical debt exists at multiple scales: individual module debt, system-level architectural debt, and organizational knowledge debt. Each scale has distinct extractiveness values and different perspectives may classify at different scales. The primary story (legacy_system_technical_debt) addresses system-level monolithic architecture. Module-level debt and organizational knowledge coupling are separate constraints with their own ε values and may decompose into their own stories if analysis requires fine-grain classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legacy_system_technical_debt, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
