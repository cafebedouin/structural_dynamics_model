% ============================================================================
% CONSTRAINT STORY: occupational_injury_externalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_occupational_injury_externalization, []).

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
 *   constraint_id: occupational_injury_externalization
 *   human_readable: Occupational Injury Externalization
 *   domain: labor_economics/occupational_health
 *
 * SUMMARY:
 *   Occupational injury externalization is a structural mechanism by which
 *   employers, labor contractors, and intermediary institutions shift the
 *   costs of workplace injuries from those who control work conditions to
 *   workers, public health systems, and disability support mechanisms. The
 *   constraint operates through multiple channels: contractor
 *   misclassification to avoid workers' compensation coverage, regulatory
 *   arbitrage across jurisdictions, weak enforcement of safety standards by
 *   captured regulatory agencies, and information asymmetry that prevents
 *   workers from accurately assessing injury risk. The extractiveness (0.58)
 *   reflects that employers and contractors capture substantial cost
 *   avoidance through these mechanisms, while suppression (0.68) is high
 *   because workers face trapped or severely constrained exit options. The
 *   theater ratio (0.55) indicates that significant institutional
 *   activity—safety training programs, insurance purchases, regulatory
 *   inspections, compliance audits—creates an appearance of injury cost
 *   internalization while permitting systematic externalization to continue.
 *   The constraint exhibits all six DR types from different structural
 *   positions, making it a diagnostic exemplar for how extraction is
 *   constituted through institutional arrangements rather than direct
 *   coercion.
 *
 * KEY AGENTS:
 *   - Injured Workers: Primary victims (powerless/trapped) — bear medical and income costs of injuries; lack exit alternatives during recovery
 *   - Informal/Gig Workers: Secondary victims (moderate/constrained) — face high barriers to alternative employment; concentrated in sectors with highest externalization
 *   - Public Health System: Tertiary victim (powerless/trapped) — absorbs uncompensated emergency care and chronic disease management; universal care mandate prevents cost-shifting back to workers
 *   - Cost-Externalizing Employers: Primary beneficiaries (institutional/arbitrage) — capture cost avoidance through subcontracting, misclassification, and regulatory arbitrage
 *   - Industry Standard-Setting Body: Secondary beneficiary (powerful/constrained) — coordinates safety norms while enabling externalization through weak enforcement; enforces appearance of compliance without substance
 *   - Regulatory Agency: Mixed institutional actor (powerful/constrained) — nominally coordinates workplace safety but systematically under-enforces due to industry capture and resource constraints
 *   - Liability Avoidance Framework: Institutional mechanism (institutional/arbitrage) — perpetuates performative structures (workers' comp insurance, safety training) disconnected from actual injury cost absorption
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing externalization as inevitable feature of market economies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(occupational_injury_externalization, 0.58).
domain_priors:suppression_score(occupational_injury_externalization, 0.68).
domain_priors:theater_ratio(occupational_injury_externalization, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(occupational_injury_externalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(occupational_injury_externalization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(occupational_injury_externalization, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(occupational_injury_externalization, snare).
narrative_ontology:human_readable(occupational_injury_externalization, "Occupational Injury Externalization").
narrative_ontology:topic_domain(occupational_injury_externalization, "labor_economics/occupational_health").

domain_priors:requires_active_enforcement(occupational_injury_externalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(occupational_injury_externalization, cost_externalizing_employers).
narrative_ontology:constraint_beneficiary(occupational_injury_externalization, liability_avoiding_contractors).
narrative_ontology:constraint_victim(occupational_injury_externalization, injured_workers).
narrative_ontology:constraint_victim(occupational_injury_externalization, public_health_system).
narrative_ontology:constraint_victim(occupational_injury_externalization, disability_safety_net).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED WORKER (SNARE) — Trapped by medical debt, job loss, and lack of alternative employment during recovery. Cannot exit the constraint; bears full cost of injury externalization through lost wages, out-of-pocket medical expenses, and disability. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(occupational_injury_externalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMAL/GIG WORKER (SNARE) — Constrained by precarious employment status and lack of worker protections. Faces high costs to exit: retraining, relocation, or formal employment search. Injury externalization falls disproportionately on this group; few alternatives exist in labor market with comparable flexibility and immediate income. High experienced extraction with nominal but costly exit routes.
constraint_indexing:constraint_classification(occupational_injury_externalization, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COST-EXTERNALIZING EMPLOYER (ROPE) — Benefits from coordination of workplace hazard management with minimal internal cost absorption. Uses subcontracting, misclassification, and regulatory arbitrage across jurisdictions to shift injury costs to workers and public systems. Experiences the constraint as a beneficial coordination mechanism that allocates injury risk to those with fewest resources to resist.
constraint_indexing:constraint_classification(occupational_injury_externalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDUSTRY STANDARD-SETTING BODY (TANGLED ROPE) — Coordinates workplace safety norms while simultaneously enabling cost externalization through weak enforcement and high compliance ambiguity. Genuine coordination function (industry members cooperate on safety standards) exists alongside asymmetric extraction (externalization permitted under ambiguous 'reasonable care' standards). Active enforcement required to maintain both the coordination facade and the extraction mechanism.
constraint_indexing:constraint_classification(occupational_injury_externalization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCY (TANGLED ROPE) — Coordinates workplace safety through inspection, standard-setting, and penalty assessment while constrained by industry capture and inadequate enforcement resources. Genuine coordination function (workplace safety standards exist and are communicated) coexists with systematic under-enforcement that permits cost externalization to continue. Active enforcement nominally required but systematically degraded.
constraint_indexing:constraint_classification(occupational_injury_externalization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LIABILITY AVOIDANCE FRAMEWORK (PITON) — Workers' compensation systems, contractor misclassification, and jurisdictional arbitrage create a performative liability structure. The framework maintains the appearance of injury cost internalization (employer insurance requirements, regulatory oversight) while systematically permitting externalization through legal structures. Theater ratio high: much institutional activity (audits, training programs, insurance purchases) is disconnected from actual injury cost absorption. Original function (internalize injury costs to incentivize safety) has atrophied; constraint persists through institutional inertia.
constraint_indexing:constraint_classification(occupational_injury_externalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PUBLIC HEALTH SYSTEM (SNARE) — Bears disproportionate cost of occupational injuries through uncompensated emergency care, chronic disease management, and disability support for workers excluded from formal injury compensation. Cannot exit the constraint; absorbs costs externalized by employers and contractors. Trapped by universal healthcare mandate and inability to deny care to injured persons.
constraint_indexing:constraint_classification(occupational_injury_externalization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a civilizational/universal perspective, occupational injury externalization may appear as an immutable property of market economies: workers cannot absorb injury costs, employers minimize liability through legal structures, and some injuries will always escape compensation. This perspective risks naturalizing what is actually a contingent institutional arrangement enforced through regulatory capture and asymmetric information. Engine will flag as false summit.
constraint_indexing:constraint_classification(occupational_injury_externalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(occupational_injury_externalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(occupational_injury_externalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(occupational_injury_externalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(occupational_injury_externalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(occupational_injury_externalization, TR),
    TR >= 0.70.

:- end_tests(occupational_injury_externalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Employers and contractors capture substantial cost avoidance through multiple mechanisms: workers' compensation insurance costs are lower than actual injury costs in high-hazard sectors, contractor misclassification eliminates insurance requirements entirely, and regulatory arbitrage permits dangerous operations in low-enforcement jurisdictions. The measurement captures the flow of injury costs from employers to workers and public systems. The trajectory from 0.38 to 0.58 reflects increasing externalization as precarious employment grows and regulatory enforcement declines. Suppression (0.68): High. Trapped workers lack alternatives during injury recovery; constrained workers face significant retraining and relocation costs; informal workers have no workers' compensation safety net. Information asymmetry (workers cannot assess injury risk; employers hide hazard data) maintains suppression through cognitive mechanisms. Theater ratio (0.55): Moderate. Substantial institutional activity—OSHA inspections, safety training mandates, workers' comp insurance requirements, industry safety committees—creates an appearance of injury cost internalization. But effectiveness is low: inspection frequency is insufficient to deter violations; training compliance is decoupled from actual hazard reduction; insurance premiums are uncoupled from injury rates; industry committees set weak standards. The theater ratio rising from 0.35 to 0.55 reflects increasing formalization of safety apparatus without corresponding increase in actual hazard control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The injured worker sees a snare: trapped by medical debt and job loss, with zero alternatives. The informal worker sees a snare: constrained by precarious employment status, with high-cost exits. The employer sees a rope: coordination mechanism that efficiently allocates injury risk and enables production. The industry body sees a tangled rope: genuine coordination of safety norms alongside asymmetric extraction through weak enforcement. The regulatory agency sees a tangled rope: nominal coordination of standards alongside systematic under-enforcement due to capture and resource constraints. The liability framework sees a piton: performative safety apparatus (insurance, training, inspections) disconnected from actual injury cost absorption. The public health system sees a snare: trapped by universal care mandate, absorbing costs externalized by employers. The analytical observer risks a false summit: naturalizing externalization as inevitable market outcome, obscuring that it is constituted through specific institutional arrangements (workers' comp design, contractor misclassification laws, regulatory capture) that could be reformed. The perspectival gap reveals that externalization is not a law of nature but a structure maintained through institutional power differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to the extraction flow. Injured workers (powerless/trapped) have d ≈ 0.95 (full targets of extraction) — they bear maximum cost and have zero exit options. Informal workers (moderate/constrained) have d ≈ 0.85 — they bear high costs but have nominal exit routes (job search, retraining) at significant cost. Employers (institutional/arbitrage) have d ≈ 0.05 (full beneficiaries) — they capture cost avoidance and can exit the constraint by relocating to compliant jurisdictions or shifting to fully compliant employment models. Regulatory agencies (powerful/constrained) have d ≈ 0.60 — they are nominally responsible for cost internalization (coordination function) but constrained by capture and resources, making them partial targets of the extraction. Public health systems (powerless/trapped) have d ≈ 0.98 — they are trapped by universal care mandates and cannot refuse to treat occupational injuries; they absorb costs that should have been internalized upstream. The perspectival gap reflects that the beneficiary (employer) sees coordination while all victims see extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLUTION: The constraint is classified as snare because it exhibits high extractiveness (0.58), high suppression (0.68), and high effective extraction χ ≈ 1.15 (from trapped/powerless agent perspective). This classification distinguishes occupational injury externalization from mere coordination failure. The snare classification resolves the mandatrophy by showing that the constraint is not a failed coordination problem (which might be salvageable through better standards or enforcement) but a functioning extraction mechanism. Employers benefit; workers and public systems bear costs; the structure is maintained through regulatory capture and information asymmetry. The tangled rope perspectives (industry body, regulatory agency) describe the coordination facade — the institutional structures that create the appearance of injury cost internalization — but the snare classification from the victim perspective (injured worker, public health system) reveals the underlying extraction. The piton perspective (liability avoidance framework) captures the theatrical dimension: much institutional activity is disconnected from actual injury cost reduction. The false summit perspective (analytical observer) represents the risk of naturalizing this structure as inevitable, obscuring that it is constitutively dependent on specific institutional choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contractor_misclassification_mechanism,
    'Are contractors systematically misclassified to avoid workers'' compensation liability, or is the classification genuinely ambiguous across employment types?',
    'Longitudinal tracking of classification disputes; correlation between contractor status and injury reporting rates; analysis of wage/benefit patterns for workers in the same role classified as employees vs contractors',
    'If systematic misclassification: constraint is primarily extractive (snare). If genuine ambiguity: some extraction is institutional overhead (tangled rope). Classification affects whether the constraint is intentional policy or unintended consequence of employment law ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_misclassification_mechanism, empirical, 'Whether contractor misclassification is systematic or genuinely ambiguous').

omega_variable(
    regulatory_capture_depth,
    'To what extent does industry influence over regulatory agencies prevent enforcement of existing injury cost internalization rules?',
    'Comparison of penalty rates across jurisdictions and industries; analysis of inspection frequency and violation citation rates; longitudinal tracking of regulatory flexibility changes following industry lobbying',
    'If capture is high: regulatory agency classification shifts further toward tangled rope / snare. If capture is low: agency retains genuine coordination function and constraint is primarily snare (workers and public bear costs). Affects whether solution is regulatory reform or structural decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of industry capture over injury regulatory enforcement').

omega_variable(
    informal_sector_injury_visibility,
    'What proportion of occupational injuries in the informal economy are invisible to both employers and public health measurement systems?',
    'Household surveys in informal sectors; comparison of self-reported injury rates to administrative injury records; analysis of healthcare-seeking behavior and barriers in informal worker populations',
    'If invisibility is high: measured extractiveness (0.58) is severe underestimate — true externalization is higher. If invisibility is low: measurement captures most externalization. Affects whether constraint is accurately classified or systematically understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_sector_injury_visibility, empirical, 'Proportion of informal sector occupational injuries invisible to measurement systems').

omega_variable(
    jurisdictional_arbitrage_scope,
    'Do multinational employers systematically locate dangerous operations in low-regulation jurisdictions, or are location decisions driven by other factors with injury externalization as secondary effect?',
    'Analysis of workplace hazard levels vs jurisdiction regulation stringency; comparison of injury rates for same employer across different jurisdictions; tracking of facility closures and relocations following regulatory changes',
    'If systematic: constraint is intentional and globally coordinated extraction. If secondary: constraint is more localized institutional failure. Affects scope classification and whether solution requires global coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_arbitrage_scope, empirical, 'Whether multinational employers systematically use regulatory arbitrage for injury externalization').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.68) structural (legal barriers, economic dependency) or internalized (workers believe injury risk is their responsibility, accept externalization as inevitable)?',
    'Surveys of worker beliefs about injury responsibility and employer liability; analysis of workers'' compensation claim rates vs self-reported injury rates; comparison of claim behavior when legal counsel provided vs without',
    'If structural: suppression persists in reformed systems if barriers remain. If internalized: suppression carries forward even after legal reform — workers maintain beliefs about responsibility. Affects sustainability of policy interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of injury claims is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(occupational_injury_externalization, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oie_tr_t0, occupational_injury_externalization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(oie_tr_t5, occupational_injury_externalization, theater_ratio, 5, 0.45).
narrative_ontology:measurement(oie_tr_t10, occupational_injury_externalization, theater_ratio, 10, 0.55).
narrative_ontology:measurement(oie_tr_t15, occupational_injury_externalization, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(oie_be_t0, occupational_injury_externalization, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(oie_be_t5, occupational_injury_externalization, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(oie_be_t10, occupational_injury_externalization, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(oie_be_t15, occupational_injury_externalization, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(occupational_injury_externalization, enforcement_mechanism).
narrative_ontology:affects_constraint(occupational_injury_externalization, regulatory_capture_labor_standards).
narrative_ontology:affects_constraint(occupational_injury_externalization, contractor_misclassification_tax_avoidance).
narrative_ontology:affects_constraint(occupational_injury_externalization, informal_sector_exclusion).

% DUAL FORMULATION NOTE:
% Occupational injury externalization is upstream of specific labor market outcomes (wage depression, informal sector expansion, disability support demands) and downstream of regulatory capture mechanisms and workers' compensation system design. Separate stories track contractor misclassification (with its own ε reflecting tax and liability avoidance) and informal sector exclusion (with its own ε reflecting access barriers); the injury externalization story captures the aggregate extraction mechanism that links these institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(occupational_injury_externalization, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
