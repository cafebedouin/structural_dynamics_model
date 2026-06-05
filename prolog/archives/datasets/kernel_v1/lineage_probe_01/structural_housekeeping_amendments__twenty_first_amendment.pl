% ============================================================================
% CONSTRAINT STORY: structural_housekeeping_amendments__twenty_first_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_housekeeping_amendments__twenty_first_amendment, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_housekeeping_amendments__twenty_first_amendment
 *   human_readable: Twenty-First Amendment: Repeal of Prohibition and Return to State Control
 *   domain: constitutional/regulatory
 *
 * SUMMARY:
 *   The Twenty-First Amendment (ratified December 5, 1933) repealed the
 *   Eighteenth Amendment and the Volstead Act, ending the national
 *   prohibition of alcohol. It is the only amendment to repeal another,
 *   making it structurally unique in constitutional history. This constraint
 *   instantiates one reading of the contested kernel of 'structural
 *   housekeeping amendments' — constitutional amendments that repair failed
 *   previous experiments in national governance. The Twenty-First Amendment
 *   reading presents repeal as the necessary correction of a failed
 *   regulatory experiment: prohibition created organized crime, black
 *   markets, and federal enforcement costs without achieving temperance
 *   goals. The amendment re-channels alcohol regulation to state authorities,
 *   permitting diverse state-level approaches (some states retained
 *   prohibition; others licensed retail). This constraint exhibits
 *   characteristics of a Tangled Rope at the analytical and state-regulator
 *   perspectives (genuine coordination mixed with enforcement burden) and
 *   Snare at the bootlegger and temperance-movement perspectives (extraction
 *   mechanisms operating through market elimination and political defeat).
 *   The extractiveness value (0.38) reflects that the repeal's primary
 *   mechanism is redistribution of market control — from bootleggers and
 *   federal enforcers to licensed traders and state regulators — rather than
 *   pure extraction. The suppression metric (0.65) captures the high coercive
 *   cost of eliminating the bootleg supply network and enforcing new
 *   state-licensing regimes.
 *
 * KEY AGENTS:
 *   - Bootleggers and Organized Crime: Primary victims (powerless/trapped) — market eliminated by legalization and state enforcement
 *   - Temperance Movement: Primary victims (powerless/trapped) — constitutional settlement reversed; moral authority dismantled
 *   - Licensed Liquor Trade: Primary beneficiary (institutional/arbitrage) — gains state-protected market with federal legitimacy
 *   - State Regulators and Legislatures: Mixed actor (moderate/constrained) — gain regulatory authority but face enforcement costs and lobbying pressure
 *   - Drinkers and Working-Class Communities: Mixed actor (organized/constrained) — gain legal access but face new restrictions and regulatory discipline
 *   - Federal Prohibition Bureaucracy: Institutional actor (institutional/arbitrage) — functions degrade through repeal but persist residually
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the repeal as inevitable correction of natural economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_housekeeping_amendments__twenty_first_amendment, 0.38).
domain_priors:suppression_score(structural_housekeeping_amendments__twenty_first_amendment, 0.65).
domain_priors:theater_ratio(structural_housekeeping_amendments__twenty_first_amendment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_housekeeping_amendments__twenty_first_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(structural_housekeeping_amendments__twenty_first_amendment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(structural_housekeeping_amendments__twenty_first_amendment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_housekeeping_amendments__twenty_first_amendment, tangled_rope).
narrative_ontology:human_readable(structural_housekeeping_amendments__twenty_first_amendment, "Twenty-First Amendment: Repeal of Prohibition and Return to State Control").
narrative_ontology:topic_domain(structural_housekeeping_amendments__twenty_first_amendment, "constitutional/regulatory").

domain_priors:requires_active_enforcement(structural_housekeeping_amendments__twenty_first_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_housekeeping_amendments__twenty_first_amendment, 'f57bb4aa-a8e6-4d74-af41-d71f7feea5d8').
narrative_ontology:cs_kernel_codification('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', formalized).
narrative_ontology:cs_authority_grounding('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', lineage).
narrative_ontology:cs_reading_relation('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', structural_housekeeping_amendments__twelfth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', structural_housekeeping_amendments__twentieth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', structural_housekeeping_amendments__twenty_second_amendment, coexists_with).
narrative_ontology:cs_reading_relation('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', structural_housekeeping_amendments__twenty_seventh_amendment, coexists_with).
narrative_ontology:cs_axiom('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', foundational, failed_national_experiments_are_reversible).
narrative_ontology:cs_axiom_status(failed_national_experiments_are_reversible, holdable).
narrative_ontology:cs_axiom_grounding('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', failed_national_experiments_are_reversible, empirically_contingent).
narrative_ontology:cs_axiom('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', foundational, federalism_restores_legitimate_governance).
narrative_ontology:cs_axiom_status(federalism_restores_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', federalism_restores_legitimate_governance, conventional).
narrative_ontology:cs_reference_frame('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', national_alcohol_prohibition_experiment).
narrative_ontology:cs_drift_state('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', post_repeal_stabilization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f57bb4aa-a8e6-4d74-af41-d71f7feea5d8', '').
narrative_ontology:cs_kernel_id(structural_housekeeping_amendments__twenty_first_amendment, structural_housekeeping_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_housekeeping_amendments__twenty_first_amendment, licensed_liquor_trade).
narrative_ontology:constraint_beneficiary(structural_housekeeping_amendments__twenty_first_amendment, state_regulators).
narrative_ontology:constraint_victim(structural_housekeeping_amendments__twenty_first_amendment, temperance_movement_legitimacy).
narrative_ontology:constraint_victim(structural_housekeeping_amendments__twenty_first_amendment, bootleggers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOOTLEGGERS/ORGANIZED CRIME (SNARE) — The repeal eliminates their primary source of extractive income, but the mechanism operates as a snare: they were trapped in criminality by prohibition's suppressive enforcement, and repeal re-traps them by legitimizing the competition they cannot survive. No exit from the subordinate position; full extraction of their market share by licensed competitors backed by state authority.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TEMPERANCE MOVEMENT (SNARE) — Trapped by the reversal of their constitutional victory. The amendment destroys the legitimacy of the 18th Amendment's moral settlement. The temperance faction has no institutional power to resist; their enforcement machinery (federal agents, state prohibition boards) is dismantled. Trapped in the loss of their authoritative claim that alcohol prohibition is constitutional law.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORS/LEGISLATURES (TANGLED ROPE) — Experience genuine coordination benefit (return of sovereignty over alcohol policy) alongside significant extraction (federal mandate to choose: permit or continue prohibition, with either path yielding regulatory burden and enforcement costs). States face high costs of establishing new regulatory infrastructure or maintaining enforcement. Also benefit from licensing fees and tax revenue, creating asymmetric gain mixed with sustained burden.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LICENSED LIQUOR TRADE (ROPE) — Primary institutional beneficiary. The repeal creates a legitimate, state-protected market with zero extraction from their perspective — they experience it as pure coordination (state provides legal framework, licensing, market protection). Extraction flows toward them, not away. Exit option is arbitrage: they can lobby for favorable state regulations and shift between state regimes.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DRINKERS/WORKING-CLASS (TANGLED ROPE) — Gain access to legal consumption (coordination benefit) but face new regulatory restrictions, licensing requirements, and state-imposed discipline replacing bootlegger supply chains. The constraint presents as liberation but operates as re-channeling: from black-market supply (unregulated, accessible) to licensed supply (regulated, controlled, taxed). Constrained by regulatory requirements and geographic access (not all states permit; some maintain local prohibition).
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL PROHIBITION BUREAUCRACY (PITON) — The institutional apparatus (Treasury agents, federal courts administering prohibition law) persists in degraded form: some agents retrain for other enforcement; courts continue processing remaining prohibition cases; the administrative structure atrophies but is not deleted. The repeal is performed by the amendment but enforcement decays through institutional inertia. Theater dominates — the formal machinery continues residually long after its primary function is eliminated.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — At maximum temporal and spatial distance, the repeal appears to instantiate an immutable law of constitutional governance: failed regulatory experiments must be reversed; supply-demand economics cannot be suppressed by law; organized crime fills prohibition gaps. This perspective risks naturalizing a contingent political outcome (the repeal coalition achieved constitutional supermajority in 1933) as an inevitable law of governance. False summit candidate: the structural data reveals repeal as a contested political achievement, not a natural necessity.
constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_housekeeping_amendments__twenty_first_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_housekeeping_amendments__twenty_first_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_housekeeping_amendments__twenty_first_amendment, TR),
    TR >= 0.70.

:- end_tests(structural_housekeeping_amendments__twenty_first_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The repeal's primary mechanism is market redistribution (bootleggers → licensed trade) rather than extraction in the sense of resource capture from powerless agents. However, the redistribution operates along power lines — those with capital and state backing (licensed traders) capture the gains; those without (bootleggers, remaining temperance enforcers) bear the losses. The declining trajectory (0.72 → 0.38 over 10 years) reflects the transition period: immediate post-repeal extractiveness is high (uncertainty, rapid institutional change, suppression machinery in flux), declining as new licensing regimes stabilize and informal supply chains are fully displaced. Suppression (0.65): High. The repeal requires sustained coercive enforcement of new state-licensing regimes against residual bootleggers, alcohol cartels in dry states, and enforcement-resistant populations. Prohibition's suppressive machinery (federal agents, state enforcement boards) continues in reconfigured form under state alcohol control boards. The mechanism persists: illegal supply is actively suppressed; possession in dry states remains criminal; movement across state lines (dry-to-wet) is regulated. Theater ratio (0.55): Moderate. State-level regulatory theater exists (licensing boards, inspection rituals, enforcement performance metrics) but is less purely performative than federal prohibition. The actual regulatory function — licensing, tax collection, supply-chain oversight — exists alongside the theater, making the constraint genuinely coordination-mixed-with-extraction rather than pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The repeal exhibits a classic perspectival gap between those who experience it as liberation (licensed traders, some drinkers) and those who experience it as defeat (bootleggers, temperance movement). The state regulators occupy a middle position: they gain constitutional authority (rope-like experience) but are constrained by federal leverage, licensing infrastructure costs, and trade-lobby pressure (tangled-rope experience). The piton perspective reveals the degradation of federal prohibition machinery — it persists residually as institutional inertia, performing diminished functions long after the primary enforcement rationale is eliminated. The analytical observer's mountain risks naturalizing a contingent political outcome (the repeal coalition's constitutional supermajority) as an inevitable law of governance — that supply-demand economics cannot be suppressed by law and failed regulatory experiments must be reversed. This is a false-summit risk: the repeal was a real political contest, not a natural correction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the repeal mechanism. Bootleggers and temperance factions are victims of market elimination — their power and exit options are destroyed by the amendment, placing them at d ≈ 1.0 (maximum target status). Licensed traders are beneficiaries with arbitrage options — they can lobby for favorable state regulation, shift between jurisdictions, relocate supply chains — placing them at d ≈ 0.05 (minimal target status). State regulators are mixed: they gain regulatory authority (low d) but face enforcement costs and implicit pressure to favor licensed trade (high d), netting to d ≈ 0.50 (symmetric position). The analytical observer's mountain classification derives from civilizational distance and the risk of naturalizing a historical political outcome as inevitable law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'repeal of failed policy' is not a single uniform classification but a multiplicity of perspectival readings. From the beneficiary's view (licensed trade), repeal is rope: pure coordination of alcohol supply under legal framework. From the victim's view (bootleggers, temperance movement), it is snare: market elimination mechanism with no exit. From the state regulator's view, it is tangled rope: genuine federalism restoration mixed with sustained enforcement burden and trade-lobby constraint. From the analytical-civilizational observer's view, it risks mountain: natural correction of failed policy. The mandatrophy resolves by recognizing that all six types are legitimate perspectival readings — the presheaf of observations over different structural positions yields the full classification spectrum. No single type is 'correct'; the constraint's identity is the presheaf itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bootlegger_survival_mechanism,
    'Did bootleggers and organized crime fully exit the alcohol market after repeal, or did they shift to other contraband (narcotics, gambling, racketeering)?',
    'Historical analysis of criminal organization revenue streams pre- and post-repeal; correlation between alcohol-trade collapse and rise of narcotics trafficking in bootlegger-controlled regions.',
    'If complete market exit: classification as snare is correct — extraction mechanism was purely alcohol-dependent. If survival through economic reconfiguration: the snare aspect was real but the mechanism shifts — extraction continues in different channels, constraining the ''liberation'' narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bootlegger_survival_mechanism, empirical, 'Whether bootleggers fully exited alcohol trade or reconfigured to other contraband').

omega_variable(
    state_regulatory_capture_risk,
    'Did return of alcohol control to the states enable state legislatures to capture regulatory authority, or did the liquor trade capture the regulators?',
    'Comparative analysis of state alcohol regulations across the post-repeal period; tracking of lobbying influence, campaign contributions, and regulatory decisions favoring or constraining licensed trade.',
    'If genuine state capture of trade: tangled rope classification holds; state regulators experienced constraint alongside autonomy. If trade capture of regulators: the constraint re-shapes into a snare for state institutions — they nominally regain sovereignty but exercise it under liquor-trade constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_regulatory_capture_risk, empirical, 'Whether states captured alcohol regulation or were captured by the liquor trade').

omega_variable(
    repeal_as_kernel_reversal,
    'Is the Twenty-First Amendment a reversal of failed constitutional policy (the reading this constraint instantiates), or is it a capitulation to corruption and failure of moral governance?',
    'Ideological stance on prohibition and constitutional amendment. No empirical resolution — this is a committer-axis question about how to frame the same historical event.',
    'If repeal is justified reversal: beneficiaries are legitimate, victims deserve loss. If repeal is capitulation: beneficiaries are corrupt, victims are unjustly displaced. Classification remains structurally identical (tangled rope + snare perspectives) but evaluative framing flips.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(repeal_as_kernel_reversal, conceptual, 'Ideological framing of repeal as justified policy correction vs. moral capitulation').

omega_variable(
    federalism_restoration_scope,
    'Did repeal genuinely restore federalism (state authority), or did it create a new federal-state extraction mechanism where states regulate liquor trade but federal revenue interests remain embedded?',
    'Analysis of federal alcohol tax structures, federal agency oversight of state alcohol boards, and federal grant dependencies. Comparison with true federal withdrawal (e.g., pre-prohibition era alcohol federalism).',
    'If genuine federalism restoration: the state regulator perspective is rope (receives autonomy). If federal-state hybrid: the state regulator perspective remains tangled rope (autonomy constrained by federal leverage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_restoration_scope, empirical, 'Whether repeal restored genuine federalism or created federal-state hybrid extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_housekeeping_amendments__twenty_first_amendment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tfam_tr_t0, structural_housekeeping_amendments__twenty_first_amendment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tfam_tr_t5, structural_housekeeping_amendments__twenty_first_amendment, theater_ratio, 5, 0.5).
narrative_ontology:measurement(tfam_tr_t10, structural_housekeeping_amendments__twenty_first_amendment, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(tfam_be_t0, structural_housekeeping_amendments__twenty_first_amendment, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(tfam_be_t2, structural_housekeeping_amendments__twenty_first_amendment, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(tfam_be_t5, structural_housekeeping_amendments__twenty_first_amendment, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tfam_be_t10, structural_housekeeping_amendments__twenty_first_amendment, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(tfam_su_t0, structural_housekeeping_amendments__twenty_first_amendment, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(tfam_su_t2, structural_housekeeping_amendments__twenty_first_amendment, suppression_requirement, 2, 0.75).
narrative_ontology:measurement(tfam_su_t5, structural_housekeeping_amendments__twenty_first_amendment, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(tfam_su_t10, structural_housekeeping_amendments__twenty_first_amendment, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_housekeeping_amendments__twenty_first_amendment, resource_allocation).
narrative_ontology:affects_constraint(structural_housekeeping_amendments__twenty_first_amendment, prohibition_enforcement_extraction).
narrative_ontology:affects_constraint(structural_housekeeping_amendments__twenty_first_amendment, alcohol_federalism_post_repeal).

% DUAL FORMULATION NOTE:
% The Twenty-First Amendment is one constraint in a family of housekeeping amendments. The Eighteenth Amendment (prohibition mechanism itself) is a separate constraint (ε=0.78, snare) that the Twenty-First repeal targets. The post-repeal alcohol-federalism settlement is a third constraint (ε varies by state from 0.15 in wet-states to 0.45 in dry-states). The Twenty-First reading describes the repeal mechanism itself; the post-repeal state-level constraints are downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_housekeeping_amendments__twenty_first_amendment, institutional, 0.08).
constraint_indexing:directionality_override(structural_housekeeping_amendments__twenty_first_amendment, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
