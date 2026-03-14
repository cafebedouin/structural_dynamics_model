% ============================================================================
% CONSTRAINT STORY: prefectural_administrative_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prefectural_administrative_power, []).

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
 *   constraint_id: prefectural_administrative_power
 *   human_readable: Prefectural Administrative Power in Multi-Tier Governance
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Prefectural administrative power in multi-tier governance systems
 *   represents a structural tension between vertical integration (central
 *   government's need to ensure uniform policy implementation across diverse
 *   municipalities) and democratic accountability (citizens' right to direct
 *   influence over local priorities). The prefectural layer coordinates
 *   baseline services, prevents regulatory arbitrage, and implements national
 *   policy — genuine coordination functions. Simultaneously, prefectural
 *   authority extracts local autonomy, constrains municipal responsiveness to
 *   local preferences, and creates bureaucratic rent-seeking opportunities
 *   through permitting and resource allocation discretion. The constraint
 *   exhibits all six DR types depending on observer position: pure extraction
 *   from the municipal perspective (snare), mixed coordination-extraction
 *   from the community perspective (tangled rope), pure coordination from the
 *   institutional perspectives (rope), degraded ritual from the assembly
 *   perspective (piton), and balanced tangled rope from the analytical view.
 *   The 40-year measurement trajectory shows drift toward extraction:
 *   extractiveness rising from 0.38 to 0.52, theater rising from 0.48 to
 *   0.64, indicating that administrative procedure is increasingly
 *   performative while discretionary prefectural authority is increasing.
 *   This drift suggests the constraint is moving from genuine Tangled Rope
 *   (balanced trade-off) toward Piton (inertial degradation).
 *
 * KEY AGENTS:
 *   - Municipal Authorities: Primary victims (powerless/trapped) — cities and towns lack genuine autonomy; exit from prefectural system is legally and practically impossible
 *   - Local Communities: Secondary victims (moderate/constrained) — citizens benefit from coordination but lose direct democratic input; identity captured by technocratic efficiency framing
 *   - Prefectural Bureaucracy: Primary beneficiary (institutional/arbitrage) — captures discretionary authority over resource allocation and policy implementation; views constraint as coordination mechanism
 *   - Central Government: Secondary beneficiary (institutional/arbitrage) — benefits from prefectural layer enabling vertical integration without direct administration of thousands of municipalities
 *   - Prefectural Government Assembly: Institutional actor (institutional/arbitrage) — formally represents democratic accountability but functions primarily as legitimacy ritual with atrophied oversight authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as genuine Tangled Rope with both coordination function and extraction asymmetry; notes drift trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prefectural_administrative_power, 0.52).
domain_priors:suppression_score(prefectural_administrative_power, 0.58).
domain_priors:theater_ratio(prefectural_administrative_power, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prefectural_administrative_power, extractiveness, 0.52).
narrative_ontology:constraint_metric(prefectural_administrative_power, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(prefectural_administrative_power, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prefectural_administrative_power, tangled_rope).
narrative_ontology:human_readable(prefectural_administrative_power, "Prefectural Administrative Power in Multi-Tier Governance").
narrative_ontology:topic_domain(prefectural_administrative_power, "political_economy/governance").

domain_priors:requires_active_enforcement(prefectural_administrative_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prefectural_administrative_power, prefectural_bureaucracy).
narrative_ontology:constraint_beneficiary(prefectural_administrative_power, central_government).
narrative_ontology:constraint_victim(prefectural_administrative_power, local_municipalities).
narrative_ontology:constraint_victim(prefectural_administrative_power, citizen_democratic_input).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MUNICIPAL AUTHORITY (SNARE) — Cities and towns operate under prefectural oversight with minimal genuine autonomy. Exit from prefectural governance structure is legally and practically impossible; municipalities bear administrative costs and regulatory burden while prefectural level captures discretionary authority over resource allocation and policy implementation. No coordination benefit perceived; pure extraction of local authority upward.
constraint_indexing:constraint_classification(prefectural_administrative_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL COMMUNITY (TANGLED ROPE) — Citizens experience both coordination benefits (prefectural standards ensure baseline services, infrastructure coordination across municipalities) and extraction (loss of direct influence over local policy, subordination to prefectural priorities that may not align with local interests, cognitive capture by technocratic framing of 'efficient administration'). High exit costs from relocating out of region; some agency through local participation channels but constrained by prefectural override authority.
constraint_indexing:constraint_classification(prefectural_administrative_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PREFECTURAL BUREAUCRACY (ROPE) — Experiences the constraint as pure coordination mechanism. Prefectural administration solves collective action problems: standardizing permits across municipalities, preventing beggar-thy-neighbor regulatory arbitrage, ensuring national policy implementation. Arbitrage options available (central government can replace prefectural structure entirely, as demonstrated by political reorganization episodes). Net beneficiary position.
constraint_indexing:constraint_classification(prefectural_administrative_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL GOVERNMENT (ROPE) — Views prefectural layer as coordination infrastructure for national policy implementation. Prefectures enable vertical integration without requiring direct central administration of thousands of municipalities. Arbitrage options available (can weaken prefectural power, can bypass through direct mandates). Benefits from the system's ability to delegate without losing control; experiences the constraint as a coordination solution rather than extraction mechanism.
constraint_indexing:constraint_classification(prefectural_administrative_power, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PREFECTURAL GOVERNMENT ASSEMBLY (PITON) — Formally represents democratic accountability at the prefectural level, but exhibits substantial theater (0.64): assembly oversight of prefectural administration is largely ceremonial; administrative capacity far exceeds assembly's actual policy influence; budget approval follows technocratic predetermined paths. Assembly persists as constitutional requirement and legitimacy ritual, but its functional verification role has atrophied. Theater indicates degraded coordination mechanism maintained through institutional inertia.
constraint_indexing:constraint_classification(prefectural_administrative_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From structural position outside the governance hierarchy, prefectural power exhibits simultaneous coordination function (vertical integration enabling national policy; horizontal coordination preventing regulatory arbitrage) and extraction mechanism (centralization of authority prevents local democratic responsiveness; bureaucratic capacity enables rent-seeking through permitting, land-use control, business regulation). Effective extraction chi is moderate (0.52) because coordination function is genuine, but extraction is real and asymmetric.
constraint_indexing:constraint_classification(prefectural_administrative_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prefectural_administrative_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prefectural_administrative_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prefectural_administrative_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prefectural_administrative_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prefectural_administrative_power, TR),
    TR >= 0.70.

:- end_tests(prefectural_administrative_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The prefectural layer captures significant discretionary authority over municipal resource allocation, permitting decisions, and policy priorities while shifting regulatory burden downward. However, the value is not higher than 0.52 because genuine coordination functions exist (inter-municipal standards, national policy implementation, prevention of regulatory arbitrage) and municipalities retain limited autonomy within prefectural frameworks. The value reflects a mixed system where extraction is real but not total. Suppression (0.58): Moderate-high. Municipal exit options are substantially constrained by constitutional structure, economic dependencies, and administrative complexity. Citizens face high costs to exit the region entirely (relocation barriers). However, suppression is not total because some prefectural discretion can be appealed, assembly oversight exists as a formal mechanism, and periodic elections provide channels for preference expression. Theater ratio (0.64): Moderate-high and increasing. Prefectural government assemblies exhibit formal democratic structure (elections, budget deliberation, oversight hearings) that occupies significant institutional time while exercising limited actual policy influence. Administrative capacity and bureaucratic expertise far exceed assembly influence over implementation. Theater has increased over 40 years as technocratic administrative capacity has grown while assembly authority has remained formally constant or declined functionally.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. From the municipal perspective, prefectural power is a Snare: no coordination benefit perceived, pure extraction of local autonomy. From the prefectural bureaucracy perspective, the same structure is a Rope: solving collective action problems, enabling vertical integration. From the citizen perspective, it is Tangled Rope: both benefiting from baseline service standardization and losing democratic input. From the assembly perspective, it is Piton: democratic form persists while function atrophies. From the analytical observer, it is Tangled Rope with drift trajectory toward Piton. This perspectival gap reveals that the constraint's classification depends entirely on whether the observer benefits from centralized coordination or suffers from lost autonomy. The beneficiaries (bureaucrats, central government) perceive coordination; the victims (municipalities, citizens) perceive extraction. This is diagnostic of a genuine Tangled Rope moving toward degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   Municipal authorities experience high directionality (d ≈ 0.85-0.95) due to victim status + trapped exit: they bear the administrative burden and loss of autonomy while benefiting marginally from coordination. The prefectural bureaucracy experiences low directionality (d ≈ 0.10-0.20) due to beneficiary status + arbitrage exit: they capture discretionary authority and can redeploy to different structures if needed. Central government experiences low directionality (d ≈ 0.05-0.15) due to beneficiary status + institutional arbitrage: the system serves their integration needs and can be restructured. Citizens experience moderate directionality (d ≈ 0.60-0.70) due to mixed victim-beneficiary status (benefit from service standardization, bear cost of lost input) + constrained exit (high cost to relocate, some local channels exist). These directionality values are not overridden because the structural derivation from beneficiary/victim + exit options produces the correct ranking.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope classification is correct: prefectural power simultaneously coordinates (prevents regulatory arbitrage, enables vertical integration, standardizes baseline services) and extracts (concentrates authority, diminishes local democracy, creates bureaucratic rent-seeking). The coordination function prevents classification as pure Snare; the extraction asymmetry prevents classification as pure Rope. The 40-year measurement trajectory (extractiveness 0.38→0.52, theater 0.48→0.64) suggests mandatrophy may be irresolving — the constraint is drifting toward Piton (degradation via inertia) rather than toward Rope (pure coordination) or Snare (pure extraction). This drift indicates the constraint's coordination function may be eroding: theater is increasing faster than extractiveness, suggesting the administrative procedure is becoming increasingly performative while the bureaucratic discretion remains. The analytical observer should monitor whether this constraint transitions from Tangled Rope to Piton, which would indicate institutional degradation rather than trade-off equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_capture_distinction,
    'Are prefectural administrative standards genuine coordination solving legitimate collective action problems, or is the ''coordination'' narrative a cover story for centralized extraction of local authority?',
    'Comparative analysis of regulatory outcomes: prefectures that delegate more autonomy to municipalities vs those that centralize; measurement of service quality, citizen satisfaction, and policy responsiveness under different autonomy levels. Historical counterfactuals (do municipalities self-coordinate successfully without prefectural oversight in specific domains?).',
    'If genuine coordination: prefectural power should be classified as Rope or Scaffold rather than Tangled Rope or Snare. If cover story: classification as Snare is confirmed. Impacts policy recommendation entirely — the constraint either requires optimization (coordination) or disruption (extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_distinction, empirical, 'Whether prefectural standards are genuine coordination or extraction cover story').

omega_variable(
    identity_lock_in_bureaucratic_capture,
    'Have prefectural bureaucrats internalized the constraint such that they experience their authority as natural and necessary rather than as a choice amenable to decentralization?',
    'Discourse analysis of administrative justifications; interviews with prefectural officials about perceived alternatives; comparison with systems featuring genuine subsidiarity (Switzerland cantons, Australian states) to identify whether different institutional frames produce different self-concepts among administrators.',
    'If identity-locked: central government could reduce prefectural power but bureaucratic resistance would be cognitive rather than structural. If not: bureaucrats would accept decentralization as a coordination mechanism change. Affects feasibility of reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_bureaucratic_capture, empirical, 'Whether prefectural bureaucrats are identity-locked to centralized authority').

omega_variable(
    temporal_drift_toward_extraction,
    'Has prefectural administrative power drifted from coordination mechanism toward pure extraction over the post-war period? Is theater_ratio increasing as function atrophies while structure persists?',
    'Historical time-series analysis: measurement of actual prefectural discretion exercised over successive decades; growth in regulatory burdens on municipalities; administrative permitting timelines; central government directives replacing local deliberation. Statistical decomposition of theater vs function growth.',
    'If drift confirmed: constraint is Piton trajectory (degradation via inertia); policy response is sunset or restructuring. If stable: constraint remains Tangled Rope with stable trade-off between coordination and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_drift_toward_extraction, empirical, 'Whether prefectural power exhibits drift toward extraction and degradation').

omega_variable(
    exit_option_reality_for_municipalities,
    'Are municipal ''constrained'' exit options actually real, or is the constraint effectively trapping municipalities through economic and legal dependencies disguised as administrative coordination?',
    'Case studies of municipalities attempting to exit prefectural oversight (merger, dissolution, functional independence in specific domains); measurement of economic penalties, administrative barriers, and legal obstacles; comparison with actual municipality capacity when prefectural oversight is reduced (pandemic response periods, disaster response showing municipality capability).',
    'If exit is genuinely constrained: classification as Tangled Rope from municipal perspective is accurate. If exit is trapped: reclassify as Snare. If exit barriers are removable: constraint is closer to Scaffold than Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_reality_for_municipalities, empirical, 'Whether municipal exit options are genuinely constrained or effectively trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prefectural_administrative_power, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prefadm_tr_t0, prefectural_administrative_power, theater_ratio, 0, 0.48).
narrative_ontology:measurement(prefadm_tr_t20, prefectural_administrative_power, theater_ratio, 20, 0.58).
narrative_ontology:measurement(prefadm_tr_t40, prefectural_administrative_power, theater_ratio, 40, 0.64).
narrative_ontology:measurement(prefadm_tr_t60, prefectural_administrative_power, theater_ratio, 60, 0.66).

% Extraction over time
narrative_ontology:measurement(prefadm_be_t0, prefectural_administrative_power, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prefadm_be_t20, prefectural_administrative_power, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(prefadm_be_t40, prefectural_administrative_power, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(prefadm_be_t60, prefectural_administrative_power, base_extractiveness, 60, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prefectural_administrative_power, enforcement_mechanism).
narrative_ontology:affects_constraint(prefectural_administrative_power, municipal_fiscal_autonomy).
narrative_ontology:affects_constraint(prefectural_administrative_power, local_democratic_responsiveness).

% DUAL FORMULATION NOTE:
% Prefectural administrative power is downstream of centralization logic (the decision to implement multi-tier governance) but represents a distinct structural constraint with its own extractiveness trajectory. Related constraints include municipal fiscal autonomy (which depends on prefectural resource allocation discretion) and local democratic responsiveness (which is directly suppressed by prefectural override authority). All three constraints share the underlying coordination-extraction tension but operate at different levels of aggregation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
