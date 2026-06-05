% ============================================================================
% CONSTRAINT STORY: municipal_government_liability_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_municipal_government_liability_reduction, []).

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
 *   constraint_id: municipal_government_liability_reduction
 *   human_readable: Municipal Government Liability Reduction Through Procedural Compliance
 *   domain: administrative/legal/governance
 *
 * SUMMARY:
 *   Municipal governments face genuine financial risk from liability claims
 *   arising from employee conduct, infrastructure negligence, and policy
 *   decisions. The constraint examined here is the systematic framework of
 *   liability reduction procedures, immunities, and risk management practices
 *   that municipalities employ to suppress remedy access while maintaining
 *   the appearance of due process and accountability. This constraint
 *   exhibits characteristics of both coordination (enabling municipal
 *   operations by managing risk) and extraction (suppressing resident remedy
 *   access through procedural and doctrinal barriers). The municipality
 *   coordinates risk pooling and procedure standardization while extracting
 *   remedy denial from injured parties. The framework combines sovereign
 *   immunity doctrines (legal barriers), notice requirements and claims
 *   filing deadlines (procedural barriers), qualified immunity protections
 *   for government employees (doctrinal barriers), and risk management
 *   practices that discourage claims (institutional barriers). The theater
 *   ratio (0.55) reflects that the framework maintains significant
 *   performative elements — due process procedures, claims processes, and
 *   judicial review — while actual remedy access is substantially suppressed
 *   through upstream barriers that most injured parties never navigate.
 *
 * KEY AGENTS:
 *   - Injured Residents: Primary victims (powerless/trapped) — face insurmountable procedural barriers and immunity doctrines; no meaningful exit from the legal framework
 *   - Community Safety Standards: Secondary victim (powerless/trapped) — abstract accountability standards systematically suppressed through liability reduction; no organized advocate
 *   - Community Advocacy Groups: Secondary beneficiary/victim hybrid (organized/constrained) — coordinate legal resources and issue amplification while bearing litigation costs and low success rates
 *   - Municipal Government: Primary beneficiary (institutional/arbitrage) — captures benefits of liability reduction; genuinely coordinates risk management across jurisdictions
 *   - Municipal Risk Management Industry: Institutional beneficiary (institutional/arbitrage) — brokers, insurers, and consultants experience framework as pure coordination mechanism generating market demand
 *   - Sovereign Immunity Doctrine: Institutional inertia (institutional/constrained) — legal doctrine persisting through path dependence; low functional necessity but high ceremonial importance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes tangled rope structure across all levels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(municipal_government_liability_reduction, 0.58).
domain_priors:suppression_score(municipal_government_liability_reduction, 0.65).
domain_priors:theater_ratio(municipal_government_liability_reduction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(municipal_government_liability_reduction, extractiveness, 0.58).
narrative_ontology:constraint_metric(municipal_government_liability_reduction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(municipal_government_liability_reduction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(municipal_government_liability_reduction, tangled_rope).
narrative_ontology:human_readable(municipal_government_liability_reduction, "Municipal Government Liability Reduction Through Procedural Compliance").
narrative_ontology:topic_domain(municipal_government_liability_reduction, "administrative/legal/governance").

domain_priors:requires_active_enforcement(municipal_government_liability_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(municipal_government_liability_reduction, municipal_government).
narrative_ontology:constraint_beneficiary(municipal_government_liability_reduction, risk_management_departments).
narrative_ontology:constraint_victim(municipal_government_liability_reduction, resident_access_to_remedy).
narrative_ontology:constraint_victim(municipal_government_liability_reduction, injured_parties).
narrative_ontology:constraint_victim(municipal_government_liability_reduction, transparency_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED RESIDENT (SNARE) — Faces procedural barriers (notice requirements, claims filing deadlines, immunity doctrines) with no meaningful exit. Trapped within a legal system that extracts remedy access while performing the appearance of due process. Maximum suppression: procedural complexity prevents understanding of filing requirements; notice periods are designed to be missed; sovereign immunity doctrines remove the jurisdiction.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY SAFETY STANDARDS (SNARE) — Abstract victim: accountability standards for public infrastructure maintenance and employee conduct are systematically suppressed through liability reduction procedures. No organized advocate; no mechanism for exit or self-correction. Safety violations that would trigger liability are pre-emptively eliminated from the record or protected under qualified immunity doctrines.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: COMMUNITY ADVOCACY GROUPS (TANGLED ROPE) — Organized agents with constrained exit: they can litigate against municipalities, but at prohibitive cost and with low probability of success due to immunity doctrines. The constraint coordinates issue amplification and legal resource sharing (genuine coordination function), while extracting from advocates through procedural barriers and litigation costs (asymmetric extraction). Active enforcement is required: cities employ risk management strategies, pre-litigation settlement practices, and administrative procedures specifically designed to discourage claims.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MUNICIPAL RISK MANAGEMENT INDUSTRY (ROPE) — Institutional beneficiary with arbitrage options. Insurance brokers, claims administrators, and risk consultants experience the liability reduction framework as pure coordination: it creates a stable market for their services through standardized procedures, predictable risk profiles, and established settlement patterns. Extraction runs toward these agents via liability reduction that captures pricing power, but they also genuinely solve the municipal coordination problem of spreading risk across jurisdictions.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MUNICIPAL GOVERNMENT (ROPE) — Primary beneficiary. The liability reduction framework is experienced as pure coordination: it enables fiscally sustainable municipal operations by capping liability exposure, pooling risk through inter-governmental insurance, and standardizing claims procedures. The framework genuinely solves the coordination problem of how to maintain public services while managing legal risk. Extraction runs toward the municipality, which captures the benefit of suppressed remedy access.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: SOVEREIGN IMMUNITY DOCTRINE (PITON) — Historical institutional inertia with high theater ratio. The doctrine originated as a principle ensuring government could act without personal liability (England, 19th century), but its current function is largely performative: it maintains the fiction that government is qualitatively different from private actors while simultaneously extracting from residents through liability reduction. The doctrine persists through legal tradition and institutional path dependence rather than serving its original coordination function. Modern qualified immunity modifications (Bivens actions, §1983 actions at state level) represent partial sunset — the doctrine is degrading in real time.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the constraint exhibits both genuine coordination function (enabling municipal operations despite risk exposure) and systematic extraction (suppressing resident remedy access). The framework solves the real problem of municipal bankruptcy from liability claims while simultaneously using that solution to extract accountability from government. The tangled rope classification at the analytical level confirms the mandatrophy: this is not pure coordination and not pure extraction, but a hybrid with asymmetric benefit distribution.
constraint_indexing:constraint_classification(municipal_government_liability_reduction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(municipal_government_liability_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(municipal_government_liability_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(municipal_government_liability_reduction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(municipal_government_liability_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(municipal_government_liability_reduction, TR),
    TR >= 0.70.

:- end_tests(municipal_government_liability_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting both genuine coordination function and systematic remedy suppression. The base value is elevated from a naive 0.35 (pure risk management cost) because the framework has accumulated layers of extraction mechanisms (notice requirements, immunity doctrines, qualified immunity, settlement collusion patterns) that suppress remedy access beyond what is necessary for fiscal sustainability. The measurement trajectory (0.35 → 0.58 over 40 years) shows extraction accumulation — liability reduction procedures have intensified as municipal exposure has grown and as risk management industry has professionalized. Suppression (0.65): Moderate-high, reflecting multiple layers: procedural complexity (notice requirements, filing deadlines, administrative procedures), legal doctrines (sovereign immunity, qualified immunity), institutional practices (risk pool settlement ceilings), and information asymmetries (injured parties do not understand filing requirements or immunity protections). Theater ratio (0.55): Moderate, indicating that the framework maintains legitimate procedural elements (due process, judicial review, administrative procedures) while performing accountability through processes that systematically suppress remedy access. The theater is not maximal (0.55 not 0.75+) because some procedural elements are functionally substantive — notice requirements and administrative review serve some coordination function beyond pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates beneficiaries from victims. Municipal government and risk management industry perceive the framework as rope (coordination solving genuine fiscal risk problem). Injured residents and abstract community standards perceive it as snare (systematic remedy suppression with no escape). Community advocacy groups with constrained exit perceive it as tangled rope (both coordination enabling and extraction suppressing). The gap reveals that beneficiaries and victims are observing genuinely different structural features of the same constraint: beneficiaries see the real coordination function (risk pooling is necessary and beneficial), while victims see the real extraction function (remedy suppression exceeds what is necessary for risk management). Neither perspective is wrong about the mechanisms they observe; they simply occupy incommensurate structural positions. The analytical observer at civilizational scale correctly identifies both functions as present — tangled rope, not rope, and not snare alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position. Injured residents are victims with trapped exit (high d → high f(d) → high χ), experiencing maximum extraction. Community safety standards are abstract victims with no exit (d ≈ 0.95). Community advocacy groups are organized but constrained (d ≈ 0.65), experiencing moderate extraction with some agency. Municipal government is beneficiary with arbitrage exit (d ≈ 0.10), experiencing extraction running toward them (negative χ). Risk management industry is institutional beneficiary with arbitrage (d ≈ 0.05), experiencing pure benefit from the coordination mechanism. Directionality overrides are not required — the derivation chain correctly captures institutional differentiation through exit options (arbitrage vs. constrained vs. trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that liability reduction frameworks are genuinely hybrid: they solve a real coordination problem (municipal fiscal sustainability) while simultaneously extracting through remedy suppression (capturing more benefit than necessary for the coordination function). The mandate is not 'municipalities should manage risk' (real) nor 'municipalities should not suppress remedy access' (also real) — the mandate is 'how much remedy suppression is necessary for fiscal sustainability, and what mechanisms minimize extraction beyond that threshold?' Current frameworks (with theater_ratio ≈ 0.55 and extractiveness ≈ 0.58) bundle coordination and extraction in ways that can be partially unbundled: comparative liability regimes (Australia's strict liability, European no-fault systems, U.S. state variations) provide data on how much extraction is structurally necessary vs. institutionally contingent. The framework resolves to tangled rope by confirming both functions are real and present — not by claiming one function is illusory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immunity_doctrine_purpose_drift,
    'Has sovereign immunity doctrine drifted from protecting government function to protecting government from accountability?',
    'Longitudinal analysis of immunity claims: historical comparison of cases where immunity was claimed for genuine operational necessity vs. cases where immunity protected negligence or misconduct',
    'If drift confirmed: immunity functions as extraction mechanism (high ε supports snare classification for victims). If no drift: immunity is legitimate coordination cost (lower ε supports rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunity_doctrine_purpose_drift, empirical, 'Whether sovereign immunity has drifted from protecting function to protecting accountability avoidance').

omega_variable(
    procedural_barrier_intent,
    'Are complex notice requirements and claims procedures designed as coordination safeguards or as extraction barriers?',
    'Comparative institutional analysis: audit of notice periods, filing requirements, and deadlines across municipalities with different liability outcomes; assessment of whether procedures are calibrated to the cognitive/temporal demands of injured parties',
    'If design as barriers confirmed: suppression increases (0.65 → 0.75+), ε rises, snare classification strengthens. If design as safeguards: suppression may be legitimate coordination cost (0.65 → 0.50).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_barrier_intent, empirical, 'Whether procedural barriers are intentional extraction mechanisms or legitimate safeguards').

omega_variable(
    settlement_collusion_patterns,
    'Do municipal risk pools and intergovernmental insurance agreements suppress remedy access through coordinated settlement ceilings?',
    'Network analysis of settlement amounts across municipal claims; identification of non-competitive settlement patterns or coordinated pricing within risk pools; comparison to private liability settlements for equivalent harms',
    'If collusion detected: institutional beneficiary extraction increases (risk management industry and municipalities capture rent through coordination), confirming tangled rope + institutional beneficiary analysis. If competitive: extraction is lower than base_extractiveness suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_collusion_patterns, empirical, 'Whether municipal risk pools suppress remedy access through coordinated settlement patterns').

omega_variable(
    qualified_immunity_collapse_timeline,
    'What is the structural timeline for qualified immunity collapse and sunset of the liability reduction framework?',
    'Monitoring of federal and state liability reform; tracking of immunity doctrine precedent erosion; assessment of political coalition strength supporting reform',
    'If collapse timeline < 10 years: scaffold classification strengthens (sunset is real and imminent). If collapse timeline > 20 years or indeterminate: framework persists as tangled rope or piton indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qualified_immunity_collapse_timeline, preference, 'Timeline for qualified immunity reform and framework sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(municipal_government_liability_reduction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mun_liab_tr_t0, municipal_government_liability_reduction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mun_liab_tr_t20, municipal_government_liability_reduction, theater_ratio, 20, 0.47).
narrative_ontology:measurement(mun_liab_tr_t40, municipal_government_liability_reduction, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(mun_liab_be_t0, municipal_government_liability_reduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mun_liab_be_t20, municipal_government_liability_reduction, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(mun_liab_be_t40, municipal_government_liability_reduction, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(municipal_government_liability_reduction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(municipal_government_liability_reduction, 0.12).
narrative_ontology:affects_constraint(municipal_government_liability_reduction, qualified_immunity_doctrine).
narrative_ontology:affects_constraint(municipal_government_liability_reduction, municipal_pension_funding).
narrative_ontology:affects_constraint(municipal_government_liability_reduction, public_employee_misconduct_accountability).

% DUAL FORMULATION NOTE:
% Liability reduction operates at the framework level (this constraint). Upstream constraints include specific immunity doctrines (qualified immunity, sovereign immunity) with their own ε values. Downstream constraints include specific institutional practices (settlement collusion, notice requirement design) with ε values reflecting actual implementation choices rather than doctrinal necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
