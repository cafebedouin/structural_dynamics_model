% ============================================================================
% CONSTRAINT STORY: legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legalization_reading, []).

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
 *   constraint_id: legalization_reading
 *   human_readable: Legal Substance Market Under Third-Party Harm Constraint
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   substance_control_legitimacy: the legalization reading, which places the
 *   legitimate boundary at individual autonomy over substance use with state
 *   authority limited to preventing third-party harms. Under this reading,
 *   adults who do not harm third parties have the right to use substances
 *   without state interference. The constraint that emerges is not
 *   'prohibition' (which would place users in the victim set) but rather
 *   'third-party harm prevention' — a regulatory infrastructure ensuring that
 *   legalized substances do not create externalities for non-users. This
 *   reading shifts the beneficiary set (users exit, becoming free agents;
 *   vendors enter as legitimate actors) and the victim set (third-party
 *   non-users, including those exposed to impaired operation, secondhand
 *   inhalation, or public health externalities). The extractiveness value
 *   (0.58) reflects moderate asymmetric extraction by vendors and regulatory
 *   authorities rather than the severe extraction characteristic of
 *   prohibition. The theater ratio (0.52) reflects that third-party harm
 *   prevention requires enforcement infrastructure (impaired driving
 *   detection, workplace safety protocols) that has both genuine protective
 *   function and performative elements (testing rituals that detect
 *   impairment but do not prevent it). The constraint is tangled rope at the
 *   institutional level: genuine coordination function (preventing
 *   third-party harms) coupled with asymmetric extraction (vendors benefit
 *   from legalization; third-party non-users bear enforcement and exposure
 *   costs). This reading is structurally distinct from the prohibition
 *   reading (which treats all users as victims subject to state constraint)
 *   and the harm-reduction reading (which decriminalizes without full
 *   legalization and does not grant autonomy unconditionally).
 *
 * KEY AGENTS:
 *   - Competent Adult Users: Primary beneficiary (moderate/mobile) — legalization removes them from victim set; they experience the constraint as coordination infrastructure enabling safe access
 *   - Third-Party Non-Users (Impaired Driving, Secondhand Exposure): Primary victim (powerless/trapped) — do not consent to exposure; cannot exit exposure; bear costs of acute impairment and chronic health effects
 *   - Public Health Regulatory Authority: Institutional actor (institutional/constrained) — enforces third-party harm constraint; experiences tangled rope (genuine coordination function coupled with asymmetric enforcement burden)
 *   - Legitimate Commercial Vendors: Beneficiary (institutional/arbitrage) — legalization creates new market; experience constraint as coordination (licensing, purity standards) enabling legitimate operation and consumer trust
 *   - Pharmaceutical-Adjacent Consolidation: Powerful actor (powerful/mobile) — captures scale advantages and regulatory capture opportunities during formative legalization phase; experiences tangled rope (coordination + market concentration extraction)
 *   - Harm-Reduction Public Health Coalition: Organized agent (organized/constrained) — sees temporary constraint with sunset (technology/social adaptation reducing third-party harm rates); experiences scaffold (low extraction because visible exit path)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent enforcement choices as biochemical inevitability (false summit risk)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legalization_reading, 0.58).
domain_priors:suppression_score(legalization_reading, 0.48).
domain_priors:theater_ratio(legalization_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legalization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legalization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legalization_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legalization_reading, tangled_rope).
narrative_ontology:human_readable(legalization_reading, "Legal Substance Market Under Third-Party Harm Constraint").
narrative_ontology:topic_domain(legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(legalization_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legalization_reading, adult_users).
narrative_ontology:constraint_beneficiary(legalization_reading, legitimate_commercial_vendors).
narrative_ontology:constraint_victim(legalization_reading, third_party_non_users).
narrative_ontology:constraint_victim(legalization_reading, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(legalization_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(legalization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

constraint_indexing:constraint_classification(legalization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(legalization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the measurement interval (0.35 → 0.58). Under legalization, the primary extraction mechanism shifts from state enforcement against users (prohibition reading) to vendor market concentration and regulatory capture. Initial extractiveness is lower because legalization is new, regulatory capture has not yet occurred, and technology for third-party harm prevention is still developing. Over time (5-10 year horizon), extractiveness rises as: (1) vendors consolidate market share and lobby for regulatory relaxation; (2) enforcement infrastructure grows (workplace drug testing, impaired driving detection) and becomes extractive for non-users; (3) public health burden of addiction-related externalities becomes clearer and compensation mechanisms fail to emerge. Suppression (0.48): Moderate. Third-party non-users face significant barriers to exit (cannot refuse geographic jurisdiction without major life disruption; cannot refuse occupational exposure in safety-sensitive roles) but not total physical confinement. Regulatory authority is constrained by political opposition from users and vendors. However, suppression is lower than prohibition (0.60+) because the constraint is narrower (third-party harm only, not all use) and organized coalitions (harm-reduction public health, civil liberties) actively resist suppression escalation. Theater ratio (0.52): Moderate, rising slightly (0.38 → 0.52). Rises because third-party harm prevention relies on testing and enforcement infrastructure (workplace drug testing, police DUI detection) with significant performative content: these rituals signal care but have variable efficacy, and the infrastructure sometimes generates false positives. The initial theater is lower because legalization is new and regulatory infrastructure is still minimal; as enforcement matures, theater increases. The constraint's claimed type (tangled rope) reflects that genuine coordination function (preventing third-party harms) coexists with asymmetric extraction (vendors capture benefits; third-party non-users bear exposure and enforcement costs).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between user autonomy (the defining feature of the legalization reading) and third-party harm victimhood (the structural consequence). Users experience legalization as liberation (rope: coordination without extraction). Third-party non-users experience legalization as victimization (snare: exposure without exit). This gap is not empirically resolvable through better measurements — it is built into the reading's definition. A user with high autonomy and a non-user with high exposure cannot both avoid the tension. The legalization reading prioritizes autonomy; the prohibition reading would prioritize safety; the harm-reduction reading would negotiate the boundary per-substance and per-context. Secondary perspectival gaps: (1) Vendors see coordination (licensing enables trust); regulatory authority sees extraction (enforcement burden); (2) Harm-reduction coalition sees temporary constraint with sunset (technology reducing third-party harm rates); analytical observer sees immutable natural law (biochemical inevitability of impairment). The scaffold perspective (harm-reduction coalition) is analytically crucial: if technology (abuse-deterrent formulations, personalized dosing) or social adaptation (shifting norms around use in safety-sensitive contexts) actually does reduce third-party harm rates, the constraint's extractiveness may decline over generational horizons and the scaffold classification is vindicated. If third-party harm rates remain high despite technological investment, the scaffold becomes aspirational rather than structural, and extractiveness continues rising.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's classification reflects its structural position relative to the third-party harm extraction flow. Users (moderate/mobile) are net beneficiaries — legalization removes them from the victim set; they experience minimal extraction because they have exit (abstinence, moderate use, geographic mobility). Vendors (institutional/arbitrage) are also beneficiaries with high exit (can choose not to seek licenses, can relocate, can shift to adjacent products); they experience the constraint as coordination enabling legitimate operation. Regulatory authority (institutional/constrained) experiences tangled rope because they bear the asymmetric enforcement burden (high cost, imperfect efficacy, political opposition) while users and vendors capture benefits. Corporate consolidation (powerful/mobile) experiences tangled rope differently: they capture market concentration benefits through scale and regulatory capture, but the constraint (third-party harm prevention) creates ongoing enforcement costs that limit extractive upside. Third-party non-users (powerless/trapped) experience snare because they have zero exit (cannot refuse occupational exposure, cannot exit jurisdiction cheaply, cannot organize effectively to oppose use by others) and bear full cost of externalities. The perspectival gap is large: beneficiaries (users, vendors) see rope or constrained tangled rope; victims (third-party non-users) see snare; regulatory authority sees enforcement tangled rope. The analytical observer risks seeing mountain (naturalizing enforcement choices as biochemical inevitability) but structural data reveals false summit (enforcement costs and technology choices are contingent, not immutable).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Does legalization coordinate substance access (rope) or extract from third-party non-users (snare)?' The legalization reading resolves this by declaring that the constraint's primary function is coordination (enabling adult autonomy) coupled with a secondary extraction mechanism (third-party harm prevention that imposes costs on non-users without their consent). Tangled rope is the correct type because both mechanisms are genuinely present and asymmetric: users are liberated (coordination benefit), vendors are legitimized (coordination benefit), but third-party non-users are exposed without exit (extraction burden). The mandatrophy is resolved by measuring and declaring both beneficiaries and victims. If only beneficiaries were declared (users, vendors), the constraint would incorrectly classify as rope (pure coordination). If only victims were declared (third-party non-users), the constraint would incorrectly classify as snare (pure extraction). The tangled rope classification requires both structural data elements: beneficiaries (users, vendors) receiving coordination benefits from access regulation, and victims (third-party non-users) bearing extraction costs from enforced exposure to externalities. The theater ratio rising from 0.38 to 0.52 indicates increasing performativity in the enforcement infrastructure over time — testing and detection rituals accumulate without proportional reduction in harm rates. This signals potential mandatrophy drift: is the constraint becoming a snare disguised as rope (enforcement theater replacing actual harm prevention)? The omega variables on regulatory capture and enforcement cost address this: if vendors capture the regulatory authority, the coordination function degrades and extractiveness becomes dominant (snare). If enforcement costs become prohibitive, the constraint becomes economically inviable and defaults to prohibition or harm-reduction reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the legitimate constraint boundary at individual autonomy (this reading), at zero-harm elimination (prohibition reading), or at minimized harm without autonomy restriction (harm-reduction reading)?',
    'This is a conceptual/preference omega routed through the committer frame. The three readings emit different constraints with different ε values and beneficiary/victim structures. Resolution depends on empirical evidence about externality magnitudes, normative judgments about liberty-versus-safety tradeoffs, and political power of coalitions backing each reading.',
    'If autonomy boundary is correct: legalization reading (this file) stands; users exit victim set; third-party harms become primary constraint. If prohibition boundary is correct: prohibition_reading constraint dominates; all users are victims of state constraint; third-party harms are not the limiting factor. If harm-reduction boundary is correct: harm_reduction_reading constraint dominates; autonomy is not absolute; constraints are negotiated per-substance and per-context.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, preference, 'Which reading of substance control legitimacy kernel is correct').

omega_variable(
    third_party_harm_magnitude_empirical,
    'What is the actual magnitude of third-party harms under legalization compared to prohibition or harm-reduction frameworks?',
    'Comparative epidemiology: impaired driving fatality rates, workplace injury rates, secondhand inhalation exposure measurements, childcare-related incident rates, emergency department burden, across jurisdictions with different regulatory approaches (full legalization vs prohibition vs harm-reduction decriminalization). Adjustment for selection effects (jurisdiction choice, population differences).',
    'If third-party harms are small (< 2% of population materially affected): legalization reading holds; third-party harm constraint is low-extractiveness rope. If third-party harms are large (> 10% of population materially affected): legalization reading needs upward extractiveness revision; snare component becomes dominant; third-party victim set expands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_harm_magnitude_empirical, empirical, 'Magnitude of third-party harms under legalization').

omega_variable(
    regulatory_capture_likelihood,
    'Do the institutional regulatory actors (public health, law enforcement) maintain independence from commercial vendor interests, or do they become captured within 10-20 years post-legalization?',
    'Historical analysis of regulatory outcomes in jurisdictions that have legalized: Do regulatory standards relax over time? Do marketing restrictions erode? Do lobbying expenditures by vendors increase? Do revolving-door hires (regulators moving to vendor positions) accelerate? Comparison with pharmaceutical regulation as a control case.',
    'If independence is maintained: public health perspective (tangled rope with high suppression) is accurate. If capture occurs: vendor perspective becomes more dominant; the constraint shifts toward extraction by legitimate vendors; extractiveness may rise to 0.70+ (snare territory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_likelihood, empirical, 'Risk of regulatory capture by commercial vendors').

omega_variable(
    identity_locked_user_exit,
    'Do substantial numbers of users become identity-locked into substance use through neuroadaptation and identity fusion, making the ''mobile exit'' assumption incorrect?',
    'Longitudinal cohort studies tracking: (a) neurobiological markers of dependence (withdrawal symptom severity, craving intensity); (b) self-reported identity fusion (''this substance use is core to who I am''); (c) exit attempt rates and success rates; (d) whether exit becomes trapped or identity_locked rather than mobile at later time horizons (generational vs biographical).',
    'If identity-locking is rare (< 10% of users): user perspective (rope/mobile) is accurate. If identity-locking is common (> 40% of users): user perspective requires downward exit mobility; may shift from rope toward snare at generational horizon; adds an internal victim set contradicting the legalization reading''s claim that users exit victim status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_user_exit, empirical, 'Whether users become identity-locked despite nominal mobility').

omega_variable(
    third_party_harm_enforcement_cost,
    'Is the enforcement cost of preventing third-party harms (impaired driving detection, workplace testing, etc.) so high that it becomes prohibitive or creates its own extraction mechanism?',
    'Cost accounting: What does it cost to detect and deter impaired driving relative to the damage prevented? What is the false-positive rate in enforcement (innocent people subject to enforcement)? Does the enforcement infrastructure (workplace drug testing, police DUI checkpoints) become a separate extraction mechanism targeting poor and minority populations disproportionately?',
    'If enforcement cost is low relative to harm prevented: regulatory authority perspective (tangled rope) holds. If enforcement cost is high: the third-party harm constraint becomes economically inviable; may default to either prohibition or harm-reduction reading; legalization reading becomes unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_enforcement_cost, empirical, 'Whether third-party harm enforcement is cost-effective').

omega_variable(
    false_summit_biochemical_naturalism,
    'Is the analytical observer''s mountain classification a genuine natural law (biochemical inevitability of impairment) or a false summit naturalizing contingent policy choices?',
    'Neuroscience review: Are there formulations, dosing protocols, or detection technologies that could reduce the rate of third-party harm to negligible levels without requiring abstinence? Does the harm derive from the substance''s biochemistry or from the choice not to invest in mitigation technologies? Comparative case: aviation and fatigue impairment. Alcohol impairs pilots; the constraint (preventing fatigue-impaired flight) is not treated as an immutable natural law but as an engineering/enforcement problem.',
    'If genuine natural law: mountain classification appropriate; third-party harms are inevitable; legalization reading is permanently constrained. If false summit: mountain classification is naturalization; the apparent inevitability is a policy choice; legalization reading becomes viable only if enforcement and technology investments are high enough.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_biochemical_naturalism, conceptual, 'Whether third-party harm is biochemically inevitable or policy-contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legalization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legal_theater_t0, legalization_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(legal_theater_t5, legalization_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(legal_theater_t10, legalization_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(legal_extract_t0, legalization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legal_extract_t5, legalization_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(legal_extract_t10, legalization_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(legalization_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(legalization_reading, impaired_operation_externality).
narrative_ontology:affects_constraint(legalization_reading, substance_vendor_market_consolidation).

% DUAL FORMULATION NOTE:
% Legalization reading is one of three structural interpretations of substance_control_legitimacy kernel. The prohibition reading treats all substance use as victim class under state constraint; ε ≈ 0.72 snare. The harm-reduction reading negotiates autonomy per-substance and per-context; ε ≈ 0.42 tangled rope. Each reading is a distinct constraint with its own ε, beneficiary/victim structure, and temporal trajectory. The three stories are linked via kernel_id and form a constraint family covering different framings of the same contested policy domain. Legalization reading upstream influences: impaired_operation_externality (which harms this constraint is actually preventing) and substance_vendor_market_consolidation (which extraction this constraint enables). These are separate constraint stories that depend on this reading's classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legalization_reading, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
