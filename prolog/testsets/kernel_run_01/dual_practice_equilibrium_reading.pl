% ============================================================================
% CONSTRAINT STORY: dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_practice_equilibrium_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium: Bifurcation of Authority Domains
 *   domain: political_history/institutional_authority/modernization
 *
 * SUMMARY:
 *   This constraint models one specific reading of how practice legitimacy
 *   becomes partitioned across state and traditional authority domains during
 *   modernization: the dual equilibrium reading. In this reading, the
 *   public/administrative domain (calendar, law, dress codes, language)
 *   becomes governed by state authority grounded in rational uniformity and
 *   centralized coordination, while the private/ritual domain (festivals,
 *   marriage, household practice, religious observance) remains governed by
 *   traditional authority grounded in continuity and community consent.
 *   Neither authority system displaces the other; instead, a stable boundary
 *   emerges. This reading presupposes that the boundary is stable, mutually
 *   reinforcing, and sustainable — that both state and traditional
 *   authorities benefit from the partition and have incentive to maintain it.
 *   The bifurcation is neither complete state absorption (the exogenous
 *   override reading) nor the endogenous displacement of traditional
 *   authority by gradual state encroachment (the endogenous displacement
 *   reading). It is a negotiated equilibrium. The extractiveness value (0.38)
 *   reflects that the constraint imposes real costs on caught practitioners
 *   (dual competence, code-switching, moral ambiguity about which norms
 *   apply) while providing genuine coordination benefits to both state and
 *   traditional authorities. Theater ratio (0.52) reflects moderate
 *   performative content: some boundary maintenance is theater (both sides
 *   invoke 'respect for tradition' or 'administrative necessity'
 *   rhetorically), but significant portions of the bifurcation reflect
 *   genuine functional differentiation.
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus (institutional/arbitrage): Primary beneficiary — extracts administrative coordination (unified calendar, law, identity systems) without bearing full cost of eliminating alternative systems
 *   - Traditional Authority Holders (institutional/arbitrage): Primary beneficiary — preserve jurisdictional domain over ritual and household practice; maintain legitimacy and power in private sphere
 *   - Caught Practitioners (powerless/trapped): Primary victim — merchants, farmers, families bearing cognitive and material cost of code-switching between domains; no exit option that avoids severe cost
 *   - Marginalized Groups (powerless/trapped): Secondary victim — actors excluded from both state and traditional domains (e.g., religious minorities, outsider castes); face suppression from both authorities
 *   - Reform Coalition (powerful/mobile): Secondary actor — intellectuals and modernizers who view bifurcation as temporary barrier to unified state system; experience constraint as frustrating impediment rather than extraction
 *   - Regional Administrators (moderate/constrained): Mediating actor — officials managing contradiction between state law and local tradition; experience highest subjective extraction due to role conflict
 *   - Analytical Observer (analytical/analytical): External analyst — identifies bifurcation as either natural structural necessity (mountain view) or contingent institutional equilibrium (piton/tangled rope views)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_practice_equilibrium_reading, 0.38).
domain_priors:suppression_score(dual_practice_equilibrium_reading, 0.48).
domain_priors:theater_ratio(dual_practice_equilibrium_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_practice_equilibrium_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_practice_equilibrium_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dual_practice_equilibrium_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(dual_practice_equilibrium_reading, "Dual Practice Equilibrium: Bifurcation of Authority Domains").
narrative_ontology:topic_domain(dual_practice_equilibrium_reading, "political_history/institutional_authority/modernization").

domain_priors:requires_active_enforcement(dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(dual_practice_equilibrium_reading, distributed).
narrative_ontology:cs_authority_grounding(dual_practice_equilibrium_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(dual_practice_equilibrium_reading).
narrative_ontology:cs_kernel_id(dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).
narrative_ontology:cs_reading_relation(dual_practice_equilibrium_reading, endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation(dual_practice_equilibrium_reading, exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom(dual_practice_equilibrium_reading, foundational, bifurcated_authority_stable_equilibrium).
narrative_ontology:cs_axiom_status(bifurcated_authority_stable_equilibrium, holdable).
narrative_ontology:cs_axiom(dual_practice_equilibrium_reading, foundational, mutual_benefit_partition_principle).
narrative_ontology:cs_axiom_status(mutual_benefit_partition_principle, holdable).
narrative_ontology:cs_reference_frame(dual_practice_equilibrium_reading, negotiated_domain_partition).
narrative_ontology:cs_drift_state(dual_practice_equilibrium_reading, contemporary_state_capacity_expansion, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(dual_practice_equilibrium_reading, traditional_authority_holders).
narrative_ontology:constraint_victim(dual_practice_equilibrium_reading, unified_normative_order).
narrative_ontology:constraint_victim(dual_practice_equilibrium_reading, marginalized_groups_caught_between_domains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAUGHT PRACTITIONER (SNARE) — Local actors (merchants, farmers, families) face incompatible normative demands: state calendar for taxation and bureaucratic compliance, lunar calendar for planting and festivals; Western dress codes for official business, traditional dress for family and community. No single framework governs, yet both are enforced. Exit is trapped — cannot abandon either domain without severe material and social cost. Experiences maximum extraction: must maintain dual competence, code-switch continuously, and bear the cognitive/resource burden of contradiction.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STATE ADMINISTRATIVE ACTOR (ROPE) — Gregorian calendar, metric system, standardized dress codes, and written law coordinate the state's essential function: tax collection, census, military mobilization. The state experiences the bifurcation as a pure coordination mechanism — it has extracted what it needs (administrative synchronization) without eliminating traditional practice. Strategic extraction, but the state sees genuine coordination benefit: traditional domains are left intact, reducing resistance and administrative overhead. Arbitrage exit option: the state can shift the administrative/ritual boundary if beneficial.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TRADITIONAL AUTHORITY HOLDER (ROPE) — Temple keepers, village elders, family heads maintain jurisdiction over ritual, festival, marriage, and agrarian calendars. The bifurcation preserves their domain: the state does not directly govern household ritual or religious practice, only administrative compliance. They experience the constraint as protective coordination — the boundary between public (state) and private (traditional) legitimizes their continued authority. Arbitrage exit: can negotiate boundary shifts with state actors through political channels.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: REFORM COALITION (TANGLED ROPE) — Intellectuals, bureaucratic modernizers, and nationalist elites see the bifurcation as a temporary compromise. They want unified law, unified calendar, unified dress codes — the absorption of traditional practice into a rationalized state system. They perceive the dual equilibrium as extraction because it perpetuates a 'backward' parallel system and fragments national identity. But they also benefit from the gradual drift: the equilibrium position is unstable, and drift favors state standardization over time. They have mobile exit options — they can choose to align with state or traditional domain — but they experience the constraint as frustrating because it prevents their preferred endpoint (full state absorption). Moderate extraction experienced due to institutional friction and partial achievement of goals.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGIONAL ADMINISTRATOR (TANGLED ROPE) — Local officials (magistrates, tax collectors, police) must enforce state law and calendar while respecting regional traditional authorities. They experience the constraint as both coordination (they need legitimacy from both state and local communities) and extraction (they bear the burden of managing contradictions, fielding complaints from both sides, and covering for the system's inconsistencies). Constrained exit: career advancement depends on balancing both domains without creating either state-level or community-level crises. Moderate extraction due to role conflict and impossible mediating position.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PITON VIEW (CIVILIZATIONAL) — From a civilizational scope, the bifurcation appears as a degraded institutional form: the equilibrium is maintained by explicit boundary maintenance (repeated enforcement of the public/private distinction, suppression of convergence pressure, institutional nostalgia for 'traditional' practice), not by genuine functional necessity. The state's administrative needs have long since separated from tradition; what persists is theater — symbolic gesture toward 'respecting' tradition while actually marginalizing it. Theater ratio reflects that much of the traditional domain is now performative, maintained for legitimacy rather than function. The constraint persists through inertia and narrative maintenance ('this is how we've always done it'), not because the bifurcation solves a real coordination problem at scale. Piton classification derives from the theater gate: suppression and extractiveness are moderate precisely because the bifurcation no longer functions as designed.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: UNIVERSAL NATURAL LAW READING (MOUNTAIN) — From an anthropological/universal perspective, practice bifurcation appears as a natural structural limit: all societies maintain some distinction between public and private domains; all societies manage multiple authority systems. This perspective sees the dual equilibrium as reflecting an irreducible cognitive and social fact — humans cannot operate under a single unified normative system because different contexts demand different logics (ceremony vs commerce, kinship vs bureaucracy). The bifurcation is not contingent institutional choice but a structural necessity of human social coordination. However, this reading risks naturalizing what is actually a particular historical negotiation — many societies have unified their normative orders, and the 'irreducible' quality reflects the reading's own framework, not external necessity. Engine will identify this as a false-summit candidate.
constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_practice_equilibrium_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dual_practice_equilibrium_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(dual_practice_equilibrium_reading, TR),
    TR >= 0.70.

:- end_tests(dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint imposes measurable costs on caught practitioners — dual competence maintenance, code-switching overhead, moral friction from incompatible normative frames — but does not approach the severe extraction of a pure snare (ε ≥ 0.46) because both state and traditional authorities provide genuine services and coordination value. The state delivers predictable law and census; traditional authorities deliver ritual meaning and community legitimacy. Neither is pure rent-seeking. The value reflects that the bifurcation is a mixed coordination-extraction equilibrium, not pure extraction. Suppression (0.48): Moderate-high. Caught practitioners face real barriers to exit — they cannot simply abandon state compliance (legal penalties, economic isolation) or traditional compliance (social ostracism, loss of community standing). Yet suppression is not total: some actors successfully navigate single-domain lives (cosmopolitan elites in major cities may minimize traditional compliance; remote rural populations may minimize state compliance). The measurement reflects the average suppression experienced across the population. Theater ratio (0.52): Moderate. The boundary between domains is partly theater (both authorities invoke legitimating narratives about the partition) and partly functional (genuine differences in coordination logic between administrative and ritual domains). The value reflects that some bifurcation appears necessary for different functional contexts, but the boundary has also become ritualized and symbolic — both sides perform 'respect' for the other domain even when actual practice is drifting toward convergence. The rising trajectory (0.38 → 0.52 over 20 time units) reflects Goodhart drift: as state authority becomes more dominant and capable, maintaining the traditional domain becomes increasingly performative (state actors preserve it as theater, not function).
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a fundamental asymmetry in how the bifurcation is experienced. Beneficiary actors (state, traditional authority) perceive coordination, protection of domain, and stable equilibrium (Rope). Caught practitioners perceive maximum extraction and structural trap (Snare). Reform coalition perceives frustration and partial achievement (Tangled Rope). Regional administrators perceive impossible mediation role (Tangled Rope, more extraction-heavy). Analytical observer at civilization scale risks perceiving natural law (Mountain) — the bifurcation as inherent to human social organization — but structural data indicates this is a false summit: the 'inherent' quality reflects the observer's framework, not external necessity. The critical gap is between the beneficiary and powerless perspectives: the state sees coordination; the caught practitioner sees extraction. The equilibrium holds because the state has enough power to sustain it and the caught practitioner has no exit, not because the partition is genuinely balanced or efficient.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural relationship to the bifurcation. The state and traditional authorities occupy beneficiary positions with arbitrage exit options (they can renegotiate the boundary if advantageous) — they experience low or negative d values, making effective extraction χ low relative to ε. Caught practitioners occupy victim positions with trapped exit options — high d values, high f(d), high experienced extraction. Regional administrators occupy intermediate positions with constrained exit and role conflict — moderate d values producing moderate extraction. Reform coalition actors have mobile exit (they can choose state or traditional alignment) and see themselves as resisting the bifurcation rather than benefiting from it — their d values reflect antagonism to the equilibrium rather than extraction by it. The analytical observer at civilization scale risks d = 0.72 (canonical analytical), producing moderate-to-high f(d) that could inflate χ — but the mountain reading reveals this as a perspectival artifact (naturalizing contingency). The tangled rope classification at the analytical institutional level reflects that the constraint both coordinates (state-traditional boundary serves genuine functional purposes) and extracts (caught practitioners bear costs; traditional authority is being gradually displaced). The piton classification at civilization scale reflects that the bifurcation is increasingly theater: the boundary persists through narrative and institutional nostalgia, not because the functional differentiation justifies it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the dual equilibrium reading is analytically stable as long as the bifurcation is maintained. The mandatrophy between Rope and Tangled Rope is resolved by attending to power distribution: the beneficiary actors (state, traditional authority) experience genuine coordination (Rope); the powerless and caught actors experience extraction (Snare or Tangled Rope depending on degree of agency). The piton classification at civilization scale reflects that the bifurcation's functional necessity is degrading over time — what was once a coordination mechanism (different domains genuinely required different norms) is becoming theater (the boundary persists through narrative, not because it solves a coordination problem). The mountain classification risks naturalizing the bifurcation as inevitable, but the structural data indicates it is a contingent institutional arrangement sustained by actors' choices and power asymmetries. The false-summit test is critical here: if traditional authority truly requires partition from state law, the mountain reading is justified; if partition is maintained by state actors who could absorb it but choose not to, the false-summit reading applies and the constraint is actually a Tangled Rope (state extraction masked by rhetoric of respecting tradition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_stability_mechanism,
    'Is the public/private boundary maintained by structural necessity (different coordination logics genuinely require different norms) or by active institutional enforcement (the boundary persists only because actors choose to maintain it)?',
    'Historical analysis of boundary drift: does the boundary hold steady when enforcement pressure is absent? Do regions with weaker state capacity show convergence or divergence of norms? Comparative examination of societies with unified vs bifurcated systems to identify whether the bifurcation solves a real coordination problem or is performative maintenance.',
    'If structural necessity: bifurcation is an immutable feature (Mountain). If institutional enforcement: bifurcation is a contingent equilibrium subject to collapse or drift (Tangled Rope or Snare depending on distribution of costs). This omega resolves whether the natural law reading is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_stability_mechanism, empirical, 'Whether boundary stability is structural or enforced').

omega_variable(
    extraction_vs_coordination_balance,
    'Does the bifurcation extract more value than it coordinates? For the state: do the administrative gains from unified calendar/law exceed the governance overhead of maintaining dual domains? For traditional authorities: does boundary protection outweigh the cost of marginalization?',
    'Accounting of state administrative efficiency gains from standardization vs overhead of dual-system management. Measurement of traditional authority power/legitimacy retention before and after bifurcation. Comparison of taxation/census/conscription efficiency in bifurcated vs unified systems.',
    'High extraction relative to coordination: Snare. Balanced: Tangled Rope. High coordination relative to extraction: Rope. This omega determines whether the constraint is a fair equilibrium or an asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_balance, empirical, 'Net balance of extraction vs coordination value').

omega_variable(
    convergence_trajectory,
    'Is the bifurcation stable over multi-generational timescales, or does it show directional drift toward either state absorption or traditional reassertion?',
    'Long-term measurement: are younger generations still code-switching between domains, or has compliance with one domain (usually state) become default? Do traditional practices show persistence as authentic practice or degradation to theater/tourism? Measurement of state enforcement intensity over time.',
    'If stable: bifurcation is a genuine equilibrium (Rope or Tangled Rope). If drifting toward state absorption: bifurcation is a transition constraint (Scaffold reading may be more accurate). If drifting toward traditional reassertion: bifurcation is unstable and may collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convergence_trajectory, empirical, 'Directional drift of bifurcation over generational timescales').

omega_variable(
    contested_kernel_reading_status,
    'Is the dual practice equilibrium a descriptive reading of what actually exists (how authority is actually partitioned) or a normative reading of what should exist (how authority ought to be partitioned)?',
    'Textual analysis: do state and traditional authority actors rhetorically invoke the dual equilibrium as justification (normative commitment) or merely as description of practice? Do they resist convergence pressure by affirming the bifurcation''s legitimacy (normative) or by claiming structural inability to coordinate across domains (descriptive)? Historical tracing of whether the bifurcation was explicitly negotiated or emerged as unstated accommodation.',
    'If descriptive: the reading models observed institutional practice; compare extractiveness across perspectives as empirical variation. If normative: the reading models a commitment system grounded in a legitimacy claim about proper authority partition; the engine''s CS detection flags this as kernel-reading material; the false-summit test becomes more likely (the normative claim may naturalize a contingent partition). This impacts whether the constraint should have a full cs_structure block.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_reading_status, conceptual, 'Descriptive vs normative status of the dual equilibrium reading').

omega_variable(
    caught_practitioner_internalization,
    'Do the actors caught between domains (merchants, families, farmers) internalize the bifurcation as legitimate (identity_locked) or do they perceive it as external constraint (trapped)?',
    'Qualitative analysis of actor narratives: do they express the dual calendar/dress system as ''how things are done'' (internalized) or as an imposed burden they resent (external)? Do they teach code-switching to children as cultural practice or as survival tactic? Measurement of resistance rates when enforcement weakens.',
    'If internalized: the powerless agent shifts from trapped to identity_locked exit option, changing the perspectival classification at biographical time (from Mountain to Rope per the identity_locked immutability profile). If external: trapped exit persists. This omega resolves whether the bifurcation has achieved legitimacy or remains an enforced imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caught_practitioner_internalization, empirical, 'Whether bifurcation is internalized by caught actors or remains external constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_practice_equilibrium_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_practice_equilibrium_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dual_tr_t10, dual_practice_equilibrium_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(dual_tr_t20, dual_practice_equilibrium_reading, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_practice_equilibrium_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dual_be_t10, dual_practice_equilibrium_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(dual_be_t20, dual_practice_equilibrium_reading, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:affects_constraint(dual_practice_equilibrium_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(dual_practice_equilibrium_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% DUAL FORMULATION NOTE:
% The dual_practice_equilibrium_reading is one constraint in a three-constraint family organized around the kernel legitimacy_of_practice_standardization. The family represents competing readings of how practice authority becomes distributed during modernization. The bifurcation equilibrium (this constraint) assumes stability and mutual benefit; the displacement reading assumes unidirectional state encroachment; the override reading assumes contested boundary with state force prevailing. All three share the same empirical domain but specify different structures and trajectories. Network links indicate that this reading influences the others by establishing an equilibrium baseline — the displacement and override readings are understood as deviations from or ruptures of the equilibrium.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_practice_equilibrium_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
