% ============================================================================
% CONSTRAINT STORY: border_security_screening
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_security_screening, []).

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
 *   constraint_id: border_security_screening
 *   human_readable: Border Security Screening Constraint
 *   domain: security/governance/movement
 *
 * SUMMARY:
 *   Border security screening creates a structural constraint on human
 *   mobility that exhibits genuine coordination properties (verifying
 *   identity, threat assessment, authorization status) alongside systematic
 *   extraction from mobile populations. The constraint is enforced by state
 *   security apparatus with near-complete institutional power to set terms,
 *   while mobile populations face suppressed exit options and asymmetric
 *   exposure to surveillance. The theater ratio has increased over the
 *   measurement interval as screening complexity (biometric data collection,
 *   database integration, risk-scoring algorithms) has grown while actual
 *   threat detection efficacy has remained flat or declined relative to the
 *   sophistication of modern threat vectors. The constraint demonstrates the
 *   full range of DR types from different perspectives: pure coordination
 *   (state authority view), pure extraction (undocumented migrant view),
 *   mixed (documented traveler view), degraded ritual (infrastructure view),
 *   temporary problem (technology coalition view), and apparent natural law
 *   (sovereignty view). The divergence between beneficiary and victim
 *   perspectives reveals that screening functions as both legitimate
 *   coordination mechanism and extraction apparatus depending on the
 *   observer's structural position.
 *
 * KEY AGENTS:
 *   - Border State Authority: Primary beneficiary (institutional/arbitrage) — controls screening protocols, captures security data, regulates movement; near-complete exit arbitrage through policy modification
 *   - Undocumented Migrants: Primary victim (powerless/trapped) — face legal prohibition on movement, zero exit alternatives; complete exposure to screening apparatus; experience maximal extraction
 *   - Documented Travelers: Secondary victim (moderate/constrained) — face high costs to avoid screening (time delays, documentation requirements, surveillance exposure); constrained exit options; experience mixed coordination and extraction
 *   - Stateless/Marginalized Groups: Tertiary victim (moderate/identity_locked) — face both material barriers (lack of recognized documentation) and identity-based entrapment (non-recognition by screening systems); internalized exclusion
 *   - Destination Labor Markets: Organized beneficiary (organized/constrained) — experience screening as coordinating labor supply verification alongside extraction through wage suppression and authorization uncertainty
 *   - Legacy Screening Infrastructure: Institutional actor (institutional/arbitrage) — maintains traditional checkpoint systems through inertia; exhibits high theater ratio as modern threat vectors bypass traditional screening
 *   - Biometric Technology Coalition: Organized agents (organized/constrained) — building alternative verification systems with sunset logic; see screening as temporary problem being solved by automation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent state border regime as immutable law of sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_security_screening, 0.58).
domain_priors:suppression_score(border_security_screening, 0.68).
domain_priors:theater_ratio(border_security_screening, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_security_screening, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_security_screening, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(border_security_screening, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_security_screening, tangled_rope).
narrative_ontology:human_readable(border_security_screening, "Border Security Screening Constraint").
narrative_ontology:topic_domain(border_security_screening, "security/governance/movement").

domain_priors:requires_active_enforcement(border_security_screening).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_security_screening, state_security_apparatus).
narrative_ontology:constraint_beneficiary(border_security_screening, surveillance_data_aggregators).
narrative_ontology:constraint_victim(border_security_screening, mobile_population_cohorts).
narrative_ontology:constraint_victim(border_security_screening, marginalized_travelers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED MIGRANT (SNARE) — Faces material barriers to exit (legal prohibition on movement, surveillance tracking, deportation threat). No alternative routes; complete suppression of exit options. Experiences maximal extraction: border screening creates immobilization and forced exposure to state control apparatus.
constraint_indexing:constraint_classification(border_security_screening, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOCUMENTED TRAVELER (TANGLED ROPE) — Faces high but surmountable costs to avoid screening (extended travel times, alternative routes, visa acquisition). Border screening coordinates legitimate security verification with asymmetric extraction: collection of biometric data, surveillance trails, temporal delays that extract economic value from travel time. Mixed experience — both coordination and extraction.
constraint_indexing:constraint_classification(border_security_screening, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BORDER STATE AUTHORITY (ROPE) — Possesses near-complete exit arbitrage (can modify screening protocols, create privileged lanes, exempt diplomatic classes). Experiences screening as pure coordination: verifying identity and threat assessment to enable legitimate movement. Net beneficiary in the immediate frame.
constraint_indexing:constraint_classification(border_security_screening, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGACY SCREENING INFRASTRUCTURE (PITON) — The physical checkpoint systems, visa verification databases, and document examination protocols persist through institutional inertia despite substantial functional degradation. Theater ratio high (0.61): much scanning and form-filling is performative; actual threat detection from screening remains low-signal. Modern threat vectors (visa fraud, synthetic identities, data forgery) bypass traditional screening entirely. Theater persists because alternatives require coordination to implement; infrastructure stakeholders resist displacement.
constraint_indexing:constraint_classification(border_security_screening, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DESTINATION COUNTRY LABOR MARKET (TANGLED ROPE) — Organized agents (employers, labor councils, regulatory bodies) experience screening as coordinating labor supply flows while extracting compliance costs. Screening creates genuine coordination function (verifying work authorization) alongside asymmetric extraction (restricting labor mobility, enabling wage suppression through authorization uncertainty, creating shadow labor markets). Sustained through generational enforcement.
constraint_indexing:constraint_classification(border_security_screening, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: STATELESS OR MARGINALIZED GROUP (SNARE via identity_locked) — Faces both structural barriers (lack of recognized documentation, legal status ambiguity) and identity-based entrapment. Group identity is constituted through exclusion and non-recognition by screening systems; exit would require abandoning the identity constructed through exclusion. Cannot imagine themselves as legible to border systems. Suppression is both structural and internalized.
constraint_indexing:constraint_classification(border_security_screening, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 7: BIOMETRIC ALTERNATIVE PATHWAY COALITION (SCAFFOLD) — Organized agents (international standards bodies, technology companies, progressive border authorities) see screening as a temporary coordination problem being solved by biometric and automated systems that reduce theater and labor intensity. Sunset logic: as iris scanning, automated document verification, and decentralized identity systems mature, traditional human screening will be displaced. Suppression declines as technical alternatives proliferate.
constraint_indexing:constraint_classification(border_security_screening, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / SOVEREIGNTY VIEW (MOUNTAIN) — From a universal civilizational perspective, border screening appears as an immutable law of sovereign statehood: the capacity to regulate entry and exit is foundational to state authority and territorial integrity. Exit from screening is structurally impossible — a state without borders cannot exist as a political unit. However, this perspective naturalizes a contingent institutional arrangement (national border regimes, state monopoly on legitimate movement control) as a law of nature. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(border_security_screening, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_security_screening_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_security_screening, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_security_screening, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_security_screening, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_security_screening, TR),
    TR >= 0.70.

:- end_tests(border_security_screening_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Border screening coordinates legitimate verification functions (identity confirmation, authorization checking) with asymmetric extraction of travel time, biometric data, and surveillance exposure. The value reflects that genuine coordination function exists alongside systematic extraction — not as pure rent-seeking (which would be 0.75+) but as coordination mechanism that extracts while performing its stated function. Measurement trajectory shows increasing extractiveness as screening complexity has grown (biometric integration, algorithmic risk-scoring) without corresponding improvement in threat detection, suggesting that added complexity serves extraction more than coordination. Suppression (0.68): High. Barriers to exit are substantial and structural: legal prohibition on cross-border movement without authorization, biometric tracking, deportation threats, travel document requirements. However, suppression is not total — some populations (passport holders, those with documentation) can navigate screening through high-cost compliance. The suppression value reflects that barriers are severe but not absolute. Theater ratio (0.61): Moderate-high. Traditional human screening involves substantial performative activity: document examination, questioning, risk scoring, database checks. Modern threat vectors (visa fraud, synthetic identity, data forgery) often bypass these traditional checks entirely. Biometric data collection is high-theater: collected at scale, rarely used for actual threat detection, primarily feeds domestic surveillance systems. The theater has increased over the interval as administrative complexity has grown while security efficacy has plateaued.
 *
 * PERSPECTIVAL GAP:
 *   State authority perceives Rope (coordination); undocumented migrant perceives Snare (extraction); documented traveler perceives Tangled Rope (mixed); stateless group perceives identity-locked Snare (extraction with internalization); technology coalition perceives Scaffold (temporary with sunset); sovereignty observer risks Mountain (naturalized). The gap reveals that the constraint's primary function differs depending on structural position: for beneficiaries, it coordinates; for victims, it extracts; for organized challengers, it is a temporary problem.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority (institutional/arbitrage/beneficiary) experiences low d because they control the constraint and possess exit arbitrage — they can modify screening protocols and exempt privileged groups. Undocumented migrant (powerless/trapped/victim) experiences maximum d — they have no alternatives and cannot escape screening apparatus. Documented traveler (moderate/constrained/victim) experiences moderate d — they can exit through costly compliance (extended travel, documentation acquisition). Stateless group (moderate/identity_locked/victim) experiences high d with identity-fusion component — structural barriers combine with internalized non-recognition. Technology coalition (organized/constrained/beneficiary-victim) experiences moderate d — they benefit from current constraint's dysfunction (which creates demand for alternatives) while bearing coordination costs of building those alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the classification depends entirely on the agent's structural position within the extraction apparatus. The state authority genuinely experiences pure coordination (Rope/Mountain) because they control the system. The undocumented migrant genuinely experiences pure extraction (Snare). The documented traveler genuinely experiences mixed coordination and extraction (Tangled Rope). There is no single 'correct' classification — the constraint IS a tangled rope from the analytical frame (combining coordination and extraction with active enforcement), but the beneficiaries experience it as rope and the most vulnerable victims experience it as snare. The apparent contradiction resolves when the indexical tuple is made explicit: each perspective is correct within its index. The sovereign authority's natural law view (Mountain) is a false summit — it naturalizes the contingent institutional arrangement as immutable, when in fact historical comparison shows that movement control, border regimes, and screening architectures are politically contingent choices that vary across states and epochs. The biometric coalition's scaffold view identifies real structural change (automated alternatives are emerging) but presupposes technical determinism about whether alternatives will actually replace traditional screening or merely add layers of theater. The identity-locked perspective for stateless groups identifies a real mechanism (internalized non-recognition) that requires intervention at the cognitive/identity level, not merely the material level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_detection_efficacy_ambiguity,
    'What proportion of detected ''threats'' at borders are genuine security risks versus administrative category violations or identity documentation errors?',
    'Audit of border apprehension records; comparison of security-classified detentions vs. category violations; analysis of downstream prosecution and conviction rates for detained travelers',
    'If genuine threats < 5%: screening is primarily administrative extraction with security theater. If genuine threats > 40%: screening functions as legitimate security coordination. Classification shifts from Snare toward Tangled Rope at moderate perspective level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_detection_efficacy_ambiguity, empirical, 'Proportion of border detentions that represent actual security threats').

omega_variable(
    surveillance_persistence_after_crossing,
    'Does biometric and travel data collected at borders create persistent surveillance trails that extend extraction beyond the crossing event?',
    'Analysis of data retention policies; tracking of downstream use of border-collected biometric data; documentation of linkage between border databases and domestic law enforcement systems',
    'If data is siloed and deleted: suppression is primarily at crossing point (constrains extraction). If data feeds domestic surveillance: suppression is continuous, and experienced extraction is higher than base metrics suggest. Classification impacts theater_ratio assessment and omega resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surveillance_persistence_after_crossing, empirical, 'Whether border screening enables persistent domestic surveillance').

omega_variable(
    documented_versus_undocumented_asymmetry_mechanism,
    'Is the extraction asymmetry between documented and undocumented travelers primarily structural (legal status creates genuine risk differential) or constructed (administrative barriers artificially create the differential)?',
    'Comparative risk analysis of documented vs undocumented border crossers; examination of violation rates, recidivism, and actual threat metrics. Comparison with pre-documentation-requirement historical data.',
    'If structural: classification as Tangled Rope is justified — genuine coordination function exists alongside extraction. If constructed: classification shifts toward Snare — extraction mechanism is primary, coordination is cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_versus_undocumented_asymmetry_mechanism, empirical, 'Whether documented/undocumented asymmetry is structural or constructed').

omega_variable(
    identity_lock_internalization_depth,
    'For stateless and marginalized groups, how much of their immobility is internalized acceptance of non-recognition versus material barriers to documentation acquisition?',
    'Ethnographic documentation of attempted border crossing narratives; analysis of documentation acquisition efforts; study of aspirations and self-conception among stateless populations',
    'If highly internalized: the identity_locked classification is diagnostically accurate, and intervention requires cognitive/identity reframing. If primarily material: classification should be trapped, and intervention requires documentation pathways. Impacts whether suppression is structural or internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization_depth, conceptual, 'Degree of identity-based internalization of exclusion for stateless groups').

omega_variable(
    biometric_alternative_maturity_timeline,
    'At what point do biometric and automated screening systems achieve sufficient reliability and interoperability to genuinely displace traditional human screening as the dominant mechanism?',
    'Tracking deployment of automated biometric systems at major borders; measurement of error rates (false positives, false negatives); documentation of international standardization progress; assessment of cost-per-crossing trends',
    'If timeline < 5 years: Scaffold sunset is real and imminent. If timeline > 15 years: Scaffold perspective is aspirational rather than structural. If never achieved: scaffold collapses to permanent Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biometric_alternative_maturity_timeline, empirical, 'Timeline for biometric systems to achieve viable alternative to human screening').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_security_screening, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bss_tr_t0, border_security_screening, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bss_tr_t5, border_security_screening, theater_ratio, 5, 0.52).
narrative_ontology:measurement(bss_tr_t10, border_security_screening, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(bss_be_t0, border_security_screening, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bss_be_t5, border_security_screening, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bss_be_t10, border_security_screening, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_security_screening, enforcement_mechanism).
narrative_ontology:affects_constraint(border_security_screening, labor_market_access_restriction).
narrative_ontology:affects_constraint(border_security_screening, asylum_determination_bottleneck).
narrative_ontology:affects_constraint(border_security_screening, biometric_data_aggregation).

% DUAL FORMULATION NOTE:
% Border screening decomposes into multiple structurally distinct constraints: (1) identity verification (low ε, primarily coordination), (2) threat assessment (moderate ε, mixed coordination/extraction), (3) movement authorization (high ε, primarily extraction), (4) biometric data collection (moderate ε for screening function, high ε when downstream surveillance is included). The constraint story addresses the integrated system; subsidiary constraints can be decomposed per the ε-invariance principle if specific functions require distinct analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_security_screening, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
