% ============================================================================
% CONSTRAINT STORY: emergency_response_reach
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_response_reach, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emergency_response_reach
 *   human_readable: Emergency Response Reach Constraint
 *   domain: public_safety/infrastructure
 *
 * SUMMARY:
 *   The emergency response reach constraint describes the structural inequity
 *   in access to emergency medical, fire, and law enforcement services across
 *   geographic and socioeconomic lines. Urban and affluent suburban
 *   populations benefit from dense station networks, paramedic-staffed
 *   ambulances, and response times averaging 4–6 minutes; rural populations
 *   and marginalized urban communities face response times of 30–60+ minutes
 *   and systematically lower-capability resources. This constraint is
 *   classified as a Snare from the perspective of the victims (rural and
 *   marginalized populations) because it combines suppression (geographic
 *   barriers, resource scarcity, implicit triage) with extraction
 *   (disproportionate funding to affluent areas, lower survival outcomes in
 *   underserved zones). From the municipality and state authority
 *   perspectives, the constraint appears as Tangled Rope — genuine
 *   coordination problems (dispersed populations require larger systems;
 *   dense populations achieve efficiency) layered with extraction mechanisms
 *   (funding formulas that systematically disadvantage rural compliance;
 *   regulatory standards that cannot be met within allocated budgets). The
 *   constraint's evolution over the 30-year interval shows increasing
 *   extractiveness as funding gaps have widened and demographic concentration
 *   has sharpened urban-rural divergence. Theater ratio remains moderate
 *   because the actual system design does not heavily employ performative
 *   elements — the constraint operates through straightforward resource
 *   allocation, not through maintaining theatrical compliance with nominal
 *   standards.
 *
 * KEY AGENTS:
 *   - Rural Populations: Primary victim (powerless/trapped) — no geographic mobility option; cannot exit via purchasing alternative services; experience 45–60+ minute response times for life-threatening emergencies
 *   - Marginalized Urban Communities: Primary victim (powerless/trapped) — structurally urban but administratively underserved; implicit triage prioritization and lower station density create gaps despite urban classification; face 12–18 minute response times vs 4–6 minutes in affluent neighborhoods
 *   - Urban Affluent Jurisdictions: Primary beneficiary (institutional/arbitrage) — concentration of resources produces 4–6 minute response times and paramedic-staffed ambulances; can leverage threat of relocation to demand continued resource priority
 *   - State Health Department: Secondary actor (organized/constrained) — mandates universal response-time standards while underfunding rural compliance; enforces regulatory standards that are systematically unachievable in rural zones due to budget structures; constrained by state politics
 *   - County Government (Rural): Secondary actor (organized/constrained) — faces coordination challenge (dispersed population geometry) layered with extraction (insufficient funding to meet mandated standards); must maintain legal service while unable to fund adequately
 *   - Municipal Administration (Urban): Secondary beneficiary (institutional/arbitrage) — structures funding allocation to optimize for dense urban populations; perceives equitable distribution as fiscally impossible; rational from efficiency perspective but preserves extraction by design
 *   - Analytical Observer: Evaluates whether response-reach constraint is natural law (immutable geometry) or constructed extraction (political allocation choice) — current data suggests hybrid with significant constructed component
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_response_reach, 0.58).
domain_priors:suppression_score(emergency_response_reach, 0.65).
domain_priors:theater_ratio(emergency_response_reach, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_response_reach, extractiveness, 0.58).
narrative_ontology:constraint_metric(emergency_response_reach, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(emergency_response_reach, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_response_reach, snare).
narrative_ontology:human_readable(emergency_response_reach, "Emergency Response Reach Constraint").
narrative_ontology:topic_domain(emergency_response_reach, "public_safety/infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_response_reach, urban_affluent_jurisdictions).
narrative_ontology:constraint_beneficiary(emergency_response_reach, municipal_administration).
narrative_ontology:constraint_victim(emergency_response_reach, rural_populations).
narrative_ontology:constraint_victim(emergency_response_reach, marginalized_urban_communities).
narrative_ontology:constraint_victim(emergency_response_reach, low_income_neighborhoods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL PATIENT (SNARE) — Structurally immobile within their geographic location; cannot exit the service gap. Response times of 45+ minutes for cardiac arrest or severe trauma create a de facto death sentence from conditions that would be survivable in urban areas. No alternatives exist — cannot purchase faster ambulance service, cannot relocate without severing livelihood and community. Maximum extraction: trapped between geography and resource scarcity with no exit mechanism.
constraint_indexing:constraint_classification(emergency_response_reach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARGINALIZED URBAN COMMUNITY (SNARE) — Despite urban geography, trapped by resource allocation decisions: fewer stations, longer response times, and implicit triage prioritization of affluent neighborhoods. Cannot exit without residential relocation, which economic barriers prevent. Suppression mechanism is dual: geographic (station density) and administrative (budget allocation). Response time disparity (8 minutes affluent vs 15 minutes marginalized urban) is structural, not accidental.
constraint_indexing:constraint_classification(emergency_response_reach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: URBAN MUNICIPALITY ADMINISTRATION (ROPE) — Experiences the constraint as coordination mechanism: concentrating resources in high-demand urban areas optimizes call-response efficiency and resource utilization. Coverage optimization is a legitimate coordination problem. The municipality perceives equitable distribution as a fiscal impossibility — spreading stations thinly produces worse outcomes everywhere than concentrating capacity. Exit option (arbitrage) comes from ability to shift resources between jurisdictions and to optimize fund allocation across domains.
constraint_indexing:constraint_classification(emergency_response_reach, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RURAL COUNTY GOVERNMENT (TANGLED ROPE) — Faces genuine coordination challenge (dispersed population geometry requires lower-density station networks) but also experiences extraction: federal funding mechanisms favor population-density metrics, creating systematic underfunding of rural systems. Must maintain minimum service legally but cannot fund it adequately. Active enforcement of state EMS regulations combined with insufficient resources produces the hybrid: coordination requirement + asymmetric extraction.
constraint_indexing:constraint_classification(emergency_response_reach, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AFFLUENT RESIDENTIAL AREA (ROPE) — Net beneficiary. Multiple fire stations, paramedic-staffed ambulances, average 4-minute response times. Experiences the system as coordination: quick response enables residents to pursue economic activity safely; infrastructure supports property values and quality of life. Mobile exit option: can organize private security or relocate if dissatisfied, but infrastructure already optimized for their benefit.
constraint_indexing:constraint_classification(emergency_response_reach, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: STATE HEALTH DEPARTMENT (TANGLED ROPE) — Mandates minimum response-time standards (coordination function) while chronically underfunding rural compliance (extraction mechanism). Requires universal 8-minute rural response times but allocates funding by metropolitan-weighted formulas. Active enforcement of the standard combined with resource constraints that make compliance impossible in rural zones produces the hybrid structure. Constrained by budget politics; cannot exit the standard without political backlash.
constraint_indexing:constraint_classification(emergency_response_reach, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FEDERAL EMERGENCY MANAGEMENT (PITON) — The theoretical universal standard (equal access to emergency services) persists as a rhetorical commitment and regulatory framework despite decades of non-compliance and abandonment of enforcement. Theater ratio (0.38) is moderate — actual funding and station placement reveal the framework's degradation, but the normative commitment is repeatedly invoked in policy documents. The framework persists through inertia and because alternatives (openly unequal services) face political resistance.
constraint_indexing:constraint_classification(emergency_response_reach, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational physics perspective, emergency response reach is constrained by irreducible geometric facts: disperse populations require proportionally larger systems to achieve equivalent response times; dense populations achieve faster response with less total resource investment. This perspective sees the differential reach as a natural consequence of geography and physics, not extraction. However, the structural data reveals beneficiary concentration and victim assignment — the constraint's beneficiaries are not random; they are systematically correlated with political power and tax base.
constraint_indexing:constraint_classification(emergency_response_reach, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_response_reach_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_response_reach, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_response_reach, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_response_reach, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_response_reach, TR),
    TR >= 0.70.

:- end_tests(emergency_response_reach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantially from rural and marginalized populations through the mechanism of resource allocation: funding formulas weight population density and call volume, which systematically favor urban areas. The extraction is not maximal (0.70+) because some portion reflects genuine geometric optimization — dispersed populations do require different service models. The measured value reflects that a significant component is choice-based (can point to alternative allocation models that would reduce disparity) rather than purely inevitable. Suppression (0.65): Moderate-high. Multiple suppression mechanisms operate: geographic barriers (dispersed population requires larger system), economic barriers (rural jurisdictions cannot fund adequate services within local tax base), regulatory barriers (state mandates that cannot be met with allocated funding), and implicit triage (dispatch prioritization may informally favor urban areas during resource scarcity). Suppression is not maximal (0.90+) because rural populations can technically exit via relocation or urban residents technically exit via private security, but exit costs are extremely high. Theater ratio (0.38): Low-moderate. The constraint does not rely heavily on performative elements — actual station placement and staffing patterns openly reflect the resource disparity. The theater present is maintenance of the federal universal-access standard as rhetorical commitment while funding realities contradict it. The low theater ratio indicates this is extraction through direct resource allocation, not through maintaining false compliance narratives. Claimed type (Snare): Justified from victim perspectives; the constraint meets snare thresholds (extractiveness ≥ 0.46, suppression ≥ 0.60, χ derivation produces high effective extraction for trapped agents). From beneficiary and state authority perspectives, the constraint appears as Tangled Rope or Rope because they perceive coordination benefits alongside extraction.
 *
 * PERSPECTIVAL GAP:
 *   The rural patient experiences the constraint as pure extraction (Snare) — trapped by geography with no exit, facing significantly reduced survival outcomes for conditions that would be survivable in urban areas. The urban municipality experiences the same constraint as legitimate coordination (Rope) — concentrating resources in high-demand areas is an efficient solution to the geometry problem of dispersed population serving. The state health department experiences it as mixed (Tangled Rope) — mandating standards (coordination) while underfunding compliance (extraction). The analytical observer risks naturalizing the constraint as immutable (Mountain) — geography and physics create inevitable disparity — but the structural data reveals choice: comparable rural systems with higher funding show substantially better outcomes, suggesting the current disparity reflects allocation decisions. The perspectival gap is diagnostic: if the gap is large and stable across time, the constraint is genuinely hybrid (different legitimate perspectives); if the gap is driven by beneficiary framing, it suggests the beneficiary classification is correct and the constraint is Snare with false coordination framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (urban affluent jurisdictions, municipal administration) face low directionality values (d ≈ 0.15–0.25): they are systematically net extractors from the resource distribution mechanism. This derives from their institutional power (can demand continued priority), arbitrage exit options (can shift resources between domains, threaten relocation), and direct benefit from the current allocation. Victims (rural populations, marginalized urban communities) face high directionality values (d ≈ 0.85–0.95): they are systematically net targets of extraction. This derives from their powerless classification, trapped exit options (geographic immobility, economic barriers to relocation), and systematic disadvantage in the allocation mechanism. Rural county government occupies middle ground (d ≈ 0.60): organized power but constrained exit (must comply with state regulations but cannot fund compliance). The sigmoid f(d) function amplifies these differences in the chi computation — beneficiaries with low d experience suppression of effective extraction, while victims with high d experience amplification. At spatial scope local (rural areas), σ(S) = 0.8 dampens chi slightly; at spatial scope global (analyzing the national system), σ(S) = 1.0 applies full extraction metric. The directionality values are not overridden because the structural data (beneficiary/victim + power + exit) produces accurate d estimates.
 *
 * MANDATROPHY ANALYSIS:
 *   The emergency response reach constraint resolves mandatrophy through perspectival decomposition: the constraint is genuinely Snare from victim perspectives, genuinely Tangled Rope from state authority perspectives, and genuinely Rope from beneficiary perspectives. The mandatrophy question ('Is this coordination or extraction?') has a determinate answer: it is both, from different structural positions. A rural patient experiences pure extraction (no coordination benefit, maximum cost). A municipality experiences coordination (efficiently matching resources to call density). A state health department experiences the hybrid (mandating standards it systematically underfunds). The resolution is not to pick one type, but to acknowledge that the constraint's function differs by position: it coordinates for beneficiaries, extracts from victims, and maintains a regulatory theater of coordination while enabling extraction at the state level. The mountain perspective (geography is destiny) is a false summit: the constraint naturalizes what is demonstrably a political choice about allocation. The corrected analysis sees a Snare with state-level institutional extraction (funding formulas that benefit dense urban areas) layered over genuine geometric constraints (serving dispersed populations costs more). Mandatrophy is resolved by showing that the constraint's extractive function is preserved through institutional design, not through inevitable physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_inevitability_vs_political_choice,
    'Is the rural response-time gap an inevitable consequence of geography and physics, or a political choice about resource allocation?',
    'Comparison of rural response time distributions across jurisdictions with equivalent geography but different funding models; analysis of counterfactual rural systems with substantially higher per-capita EMS funding',
    'If inevitable: constraint is mountain (immutable geographic fact). If political: constraint is snare (extraction mechanism masked as natural law). Current data shows variance by funding choice — suggests political component is larger than geometric component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_inevitability_vs_political_choice, empirical, 'Whether response-time gaps reflect geography or resource allocation choices').

omega_variable(
    station_density_optimization_boundary,
    'What station density in rural areas would achieve parity with urban response times, and what would be the marginal cost?',
    'Engineering analysis of optimal rural station placement; cost-benefit modeling of response-time improvement per unit funding in rural vs urban systems; case studies of rural jurisdictions with above-median EMS funding',
    'If cost is modest (< 2% additional public safety budget): current underfunding is choice-based extraction. If cost is severe (> 10%): constraint approaches mountain status — geometry genuinely prevents parity within realistic budgets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(station_density_optimization_boundary, empirical, 'Cost of achieving rural-urban response-time parity').

omega_variable(
    suppression_mechanism_structural_vs_intentional,
    'Is suppression of rural emergency access a structural byproduct of density-weighted funding formulas, or does intentional triage prioritization (implicit or explicit) increase the extraction?',
    'Audit of funding allocation formulas and dispatch protocols; analysis of response-time disparities controlling for call volume and acuity; interviews with dispatch supervisors about prioritization logic',
    'If purely structural: suppression is 0.40–0.50 (geometry-driven). If intentional triage overlaid: suppression is 0.65–0.75 (adds active enforcement to denial). Current assessment (0.65) assumes some intentional component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_intentional, empirical, 'Whether suppression is structural geometry or intentional triage').

omega_variable(
    false_summit_candidate,
    'Is the response-reach constraint fundamentally an immutable natural law (geography), a contingent institutional arrangement (resource allocation), or both layered?',
    'Contrast between physics-based modeling (what geographic/resource optimization requires) and actual system design (what funding and policy actually produce); detection of systematic beneficiary concentration suggesting constructed constraint layered over geographic base',
    'If pure natural law: mountain classification holds. If significant constructed component: false summit — the constraint naturalizes allocation choices. Current data suggests hybrid: geographic base + constructed extraction via allocation mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'Whether response-reach constraint is natural law or constructed extraction').

omega_variable(
    scalar_disparity_empirical_basis,
    'What is the actual scalar disparity in survival outcomes between equivalent patients in rural vs urban systems?',
    'Trauma registry analysis, cardiac arrest outcome studies, stroke outcome analysis comparing equivalent cases in rural and urban systems with risk adjustment',
    'If disparity is > 3:1 in survivability: extraction is severe (snare classification confirmed). If disparity is < 1.5:1: constraint may be rope (coordination) rather than snare (extraction). Current assessment assumes > 2:1 disparity for conditions like STEMI and severe trauma.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scalar_disparity_empirical_basis, empirical, 'Scalar survival-outcome disparity between rural and urban systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_response_reach, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, emergency_response_reach, theater_ratio, 0, 0.32).
narrative_ontology:measurement(emerg_tr_t15, emergency_response_reach, theater_ratio, 15, 0.35).
narrative_ontology:measurement(emerg_tr_t30, emergency_response_reach, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, emergency_response_reach, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(emerg_be_t15, emergency_response_reach, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(emerg_be_t30, emergency_response_reach, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(emerg_su_t0, emergency_response_reach, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(emerg_su_t15, emergency_response_reach, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(emerg_su_t30, emergency_response_reach, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_response_reach, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_response_reach, urban_rural_tax_base_disparity).
narrative_ontology:affects_constraint(emergency_response_reach, paramedic_training_pipeline_concentration).
narrative_ontology:affects_constraint(emergency_response_reach, medical_outcome_inequality_scaling).

% DUAL FORMULATION NOTE:
% The response-reach constraint decomposes into at least three structurally distinct stories: (1) geographic service geometry (ε≈0.35, Tangled Rope: genuine coordination problem with population dispersion), (2) funding allocation mechanism (ε≈0.62, Snare: extraction via formula-based underfunding of rural compliance), (3) outcome inequality scaling (ε≈0.70, Snare: differential mortality outcomes from identical medical conditions). Each has different ε values and different primary mechanisms. The current story focuses on the allocation mechanism; linked stories cover geographic optimization and outcome inequality separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
