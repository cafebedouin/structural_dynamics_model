% ============================================================================
% CONSTRAINT STORY: thai_senate_veto_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_senate_veto_2026, []).

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
 *   constraint_id: thai_senate_veto_2026
 *   human_readable: The Senate Constitutional Veto (Post-Transitory Residual)
 *   domain: political/constitutional_governance
 *
 * SUMMARY:
 *   Thailand's Senate constitutional veto under Article 256 is a residual
 *   constraint from the junta-era transitory framework. The original
 *   five-year provision allowing the appointed Senate to vote for Prime
 *   Minister expired in 2024, but the one-third veto power over
 *   constitutional amendments persists. This constraint exemplifies the
 *   structural problem of post-authoritarian institutional legacies: formal
 *   mechanisms designed for transitory military oversight continue to
 *   function as extraction devices long after their original justification
 *   has been superseded. The veto operates as a tangled rope at the
 *   structural level — it provides genuine coordination benefits (prevents
 *   constitutional churn, maintains institutional checks) but extracts
 *   undemocratic control via asymmetric beneficiaries (appointed faction
 *   gains power without electoral mandate) and asymmetric suppression
 *   (alternative reform pathways are blocked). The theater ratio has
 *   increased from 0.42 to 0.58 over five years, reflecting growing
 *   disconnection between the veto's original security rationale (now
 *   irrelevant post-transitory period) and its continued deployment
 *   (increasingly performative institutional legitimacy claim).
 *   Extractiveness has risen from 0.38 to 0.52, driven by the appointed
 *   faction's use of the veto threat to block democratic constitutional
 *   reforms. The constraint presents a mandate-for-purity problem: the system
 *   cannot simultaneously (a) retain the Senate veto as a coordination
 *   mechanism for institutional stability and (b) honor the electoral mandate
 *   for democratic constitutional reform. Resolution requires either (i)
 *   removing or significantly weakening the veto (scaffold pathway), (ii)
 *   accepting the constraint as permanent institutional asymmetry (piton
 *   acceptance), or (iii) coalition-building that secures Senate
 *   supermajority through persuasion (tangled rope equilibrium).
 *
 * KEY AGENTS:
 *   - Appointed Senate Faction: Primary beneficiary (institutional/arbitrage) — retains unelected constitutional veto power; experiences constraint as stabilizing coordination
 *   - Elected House Coalition: Primary victim (organized/constrained) — unable to secure constitutional reforms without Senate approval; constrained by veto threat
 *   - Democratic Reformers: Secondary victim (powerless/trapped) — civil society actors demanding constitutional democratization; face maximal extraction through closed reform pathways
 *   - Junta Legacy Residue: Institutional anchor (institutional/arbitrage) — original authoritarian framework persists as constitutional theater despite transitory period expiration
 *   - Constitutional Reform Movement: Organized agent (organized/constrained) — sees veto as temporary obstacle with sunset pathway through civil mobilization and demographic change
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies tangled rope structure with measurable extraction subordinating coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_senate_veto_2026, 0.52).
domain_priors:suppression_score(thai_senate_veto_2026, 0.65).
domain_priors:theater_ratio(thai_senate_veto_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_senate_veto_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(thai_senate_veto_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(thai_senate_veto_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_senate_veto_2026, tangled_rope).
narrative_ontology:human_readable(thai_senate_veto_2026, "The Senate Constitutional Veto (Post-Transitory Residual)").
narrative_ontology:topic_domain(thai_senate_veto_2026, "political/constitutional_governance").

domain_priors:requires_active_enforcement(thai_senate_veto_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, appointed_senate_faction).
narrative_ontology:constraint_beneficiary(thai_senate_veto_2026, military_institutional_interests).
narrative_ontology:constraint_victim(thai_senate_veto_2026, democratic_constitutional_reform).
narrative_ontology:constraint_victim(thai_senate_veto_2026, electoral_mandate_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC REFORMER (SNARE) — Trapped within the constitutional amendment process. Cannot exit without conceding all reform objectives. Bears full extraction cost: any amendment requires Senate supermajority, which is structurally controlled by the junta-appointed faction. No alternative exit; career and legitimacy depend on reform attempt. Experiences maximum coercion with suppression of alternatives.
constraint_indexing:constraint_classification(thai_senate_veto_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ELECTED HOUSE COALITION (TANGLED ROPE) — Can theoretically govern through ordinary legislation but constrained by veto threat on constitutional changes. Benefits from coordination with Senate on routine legislative matters; coordination function exists (both chambers do pass non-constitutional bills). But extraction occurs via the veto asymmetry: the elected chamber cannot secure fundamental reforms without Senate approval. Active enforcement required — the veto must be deployed as explicit threat to suppress reform attempts. Mixed coordination (lower chamber needs Senate stability) and extraction (Senate extracts legitimacy tax).
constraint_indexing:constraint_classification(thai_senate_veto_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: APPOINTED SENATE FACTION (ROPE) — Experiences the veto as pure coordination mechanism. Coordination function: prevents destabilizing constitutional churn, provides institutional continuity, stabilizes the political system post-junta transition. Arbitrage exit: can walk away from specific amendments while maintaining institutional position. Net beneficiary — the veto grants structural power to this agent without perceivable cost. From their structural position, this is coordination.
constraint_indexing:constraint_classification(thai_senate_veto_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUNTA LEGACY RESIDUE (PITON) — The Senate veto persists as institutional theater: performative constitutional balance. The original rationale (military oversight to prevent democratic excess) has decayed post-transitory period expiration. The constraint is now sustained by inertia rather than functional legitimacy. Theater ratio of 0.58 reflects that the veto is occasionally deployed (not purely symbolic) but increasingly disconnected from its original justification. The institution maintains itself through historical claim rather than demonstrated necessity.
constraint_indexing:constraint_classification(thai_senate_veto_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized civil society sees the veto as a temporary coordination failure with an implicit sunset. The constraint can be overcome through: (a) coalition-building that secures Senate supermajority through persuasion rather than coercion, (b) civil mobilization that raises political cost of veto use, or (c) structural reform of the Senate itself (reducing junta appointee proportion). The scaffold perceives a path through the constraint, not around it. Suppression declines as democratic norms strengthen and junta institutional legitimacy fades. Estimated sunset: constitutional reform pathway opens within 5-10 year horizon as demographic and institutional change reduces junta faction coherence.
constraint_indexing:constraint_classification(thai_senate_veto_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the Senate veto is a genuine hybrid: it does provide coordination (prevents constitutional instability, maintains institutional checks), but coordination is subordinate to extraction (junta faction preserves undemocratic control). The constraint requires active enforcement (explicit veto threats), possesses asymmetric beneficiaries (appointed faction gains unelected power), and imposes asymmetric costs (democratic legitimacy is extracted from electoral majority). Suppression is real (alternatives are structurally blocked) but not totalizing (coalition-building and civil mobilization remain viable). This is the structural definition of tangled rope: coordination with extractive asymmetry.
constraint_indexing:constraint_classification(thai_senate_veto_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_senate_veto_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_senate_veto_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_senate_veto_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_senate_veto_2026, TR),
    TR >= 0.70.

:- end_tests(thai_senate_veto_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The appointed Senate faction leverages the veto threat to extract undemocratic control over constitutional change. The extraction is not totalizing because (a) non-constitutional legislation proceeds normally, (b) coalition-building can theoretically secure Senate supermajority, and (c) the veto is a one-time tool per amendment, not an ongoing extraction mechanism. But the extraction is real: democratic constituencies cannot implement their constitutional agenda without Senate approval controlled by unelected appointees. The 0.52 value reflects actionable veto power without complete suppression of alternatives. Suppression (0.65): Moderately high. Significant barriers to constitutional reform include the explicit supermajority requirement, the ideological coherence of the appointed faction around military-continuity objectives, and the high political cost of challenging the institutional framework (risks destabilizing post-transition period). But suppression is not totalizing — coalition-building within the Senate remains possible, civil mobilization can raise veto costs, and the junta faction's institutional coherence is decaying over time. Theater ratio (0.58): Moderate-high. The veto is not purely symbolic — it has been deployed to block or delay amendments (e.g., restrictions on emergency decrees). But the theater has increased because the original security rationale (military oversight preventing democratic instability) has been superseded by political reality: democratic institutions have proven stable, and the veto now functions primarily to preserve unelected control, not to prevent chaos. The performative aspect has grown as the functional justification has faded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence on the same structural mechanism. The appointed Senate faction sees coordination and stability preservation (Rope perspective) — the veto prevents destabilizing constitutional churn and maintains institutional checks. The elected house sees mixed coordination and extraction (Tangled Rope) — they need Senate stability but are blocked from constitutional reform. The democratic reformer sees pure extraction and coercion (Snare) — the veto closes off the reform pathway entirely. The constitutional reform movement sees a temporary obstacle with a sunset (Scaffold) — the veto can be overcome through coalition-building and institutional change. The junta legacy residue sees its own institutional theater (Piton) — the veto persists through historical inertia despite its original rationale becoming irrelevant. The analytical observer identifies the structural reality (Tangled Rope) — genuine coordination benefits exist but are subordinate to asymmetric extraction. The gap reflects that the veto's legitimacy depends entirely on whether one believes institutional stability requires undemocratic control.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position. The appointed Senate faction (beneficiary + arbitrage exit) derives low d ≈ 0.15 — they experience negative extraction (the veto grants them power). The elected house (organized victim + constrained exit) derives higher d ≈ 0.55 — they cannot easily exit the constitutional system but are constrained by the veto. The democratic reformer (powerless victim + trapped exit) derives maximum d ≈ 0.95 — they have no exit from the constraint and bear its full cost. The analytical observer (analytical perspective) derives d ≈ 0.72 — neutral analytical distance, neither beneficiary nor victim. The beneficiary/victim declarations are critical: the appointed faction is beneficiary (gains undemocratic power), while democratic constitutional reform is victim (extracted from), not the House itself. This precision enables the engine to differentiate the Rope from the Snare: the Rope derives from beneficiary status + arbitrage; the Snare derives from victim status + trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint presents an unresolved mandatrophy: it cannot simultaneously be Rope (pure coordination for institutional stability) and Snare (pure extraction of democratic control). The Tangled Rope classification bridges this mandatrophy by showing that both coordination and extraction are genuine. The veto does provide institutional stability (coordination function), but that stability is purchased through undemocratic control (extraction asymmetry). The mandatrophy is resolved not by choosing a single type but by recognizing that (a) beneficiary and victim perspectives see genuinely different constraints, (b) the beneficiary's Rope is the victim's Snare viewed from different structural positions, and (c) the system cannot be reformed toward pure Rope without reducing undemocratic extraction, nor can it remain as Snare without destabilizing democratic institutions. The Tangled Rope is the structural equilibrium: coordination with embedded extraction. Mandatrophy resolution would require either (i) acceptance of the constraint as permanent tangled rope (institutional power-sharing rather than democratic purity), (ii) removal of the veto (pure democratization at cost of coordination), or (iii) restructuring of the Senate itself (reducing appointed faction proportion, restoring coordination function without extraction asymmetry). Current trajectory suggests drift toward either piton (veto becomes toothless theater as junta faction decays) or constitutional crisis (if veto is deployed too aggressively, triggering coalition pressure or civil mobilization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    junta_faction_coherence_decay,
    'At what rate will the appointed Senate faction''s institutional coherence decay as original junta appointees retire and are replaced by younger, less-committed figures?',
    'Longitudinal tracking of voting bloc cohesion on constitutional amendments; analysis of appointee demographic turnover and replacement patterns; exit interviews with retiring appointees regarding institutional commitment',
    'If coherence decays rapidly (within 5 years): veto becomes toothless, scaffold sunset accelerates, constraint shifts to piton. If decay is slow or arrested: veto remains functional extraction mechanism for 10+ years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(junta_faction_coherence_decay, empirical, 'Rate of institutional decay in junta-appointed faction').

omega_variable(
    civil_mobilization_veto_cost_threshold,
    'At what level of civil mobilization against the veto does the political cost to Senate implementers outweigh institutional benefit preservation?',
    'Cross-national analysis of constitutional veto overrides in post-authoritarian democracies; measurement of protest scale, elite political support defection, and veto utilization cessation as consequence of mobilization campaigns',
    'If threshold is low (modest sustained protest suffices): scaffold perspective correct, veto becomes functionally obsolete within 3-5 years despite formal retention. If threshold is high: extraction mechanism remains credible even under pressure; constraint persists in tangled-rope form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_mobilization_veto_cost_threshold, empirical, 'Civil mobilization threshold for veto cost parity').

omega_variable(
    democratic_legitimacy_extraction_measurement,
    'What observable measures would quantify the ''legitimacy tax'' extracted from democratic procedures by the veto threat?',
    'Measurement of legislative productivity on constitutional items; analysis of abandoned reform attempts attributable to veto threat vs. other causes; polling on public perception of Senate legitimacy; comparative analysis with other bicameral systems lacking veto asymmetry',
    'If legitimacy tax is substantial and measurable: tangled-rope classification is robust. If tax is diffuse and hard to quantify: constraint may be piton (performative) rather than tangled rope (functionally extractive). Affects entire mandatrophy resolution pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_legitimacy_extraction_measurement, empirical, 'Quantification of legitimacy extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_senate_veto_2026, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsv_tr_t0, thai_senate_veto_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tsv_tr_t2, thai_senate_veto_2026, theater_ratio, 2, 0.5).
narrative_ontology:measurement(tsv_tr_t5, thai_senate_veto_2026, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(tsv_be_t0, thai_senate_veto_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tsv_be_t2, thai_senate_veto_2026, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(tsv_be_t5, thai_senate_veto_2026, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_senate_veto_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_senate_veto_2026, thai_emergency_decree_framework).
narrative_ontology:affects_constraint(thai_senate_veto_2026, thai_electoral_mandate_legitimacy).

% DUAL FORMULATION NOTE:
% The Senate veto is structurally linked to the emergency decree framework (the veto can block amendments that constrain emergency power) and to electoral mandate legitimacy (the veto nullifies voting bloc mandates on constitutional matters). These constraints form a family: the veto is downstream of the broader junta legacy institutional design. If the veto is decomposed into separate empirical and normative claims, the empirical claim (the veto functionally blocks amendments via supermajority requirement) is Mountain-like (ε ≈ 0.08), while the normative claim (the veto extracts democratic legitimacy) is Tangled Rope (ε ≈ 0.52). This story models the normative-structural constraint; empirical-mechanical constraint should be written separately if analysis requires that decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
