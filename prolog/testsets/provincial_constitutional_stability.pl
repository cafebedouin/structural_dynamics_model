% ============================================================================
% CONSTRAINT STORY: provincial_constitutional_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_constitutional_stability, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_constitutional_stability
 *   human_readable: Provincial Constitutional Stability
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Provincial constitutional stability represents a foundational constraint
 *   in multi-level democratic systems. The structure creates a tension
 *   between two legitimate functions: (1) constitutional frameworks must be
 *   stable enough to enable long-term institutional planning and protect
 *   against caprice, and (2) constitutional frameworks must be responsive
 *   enough to incorporate changing popular preferences and remedy systemic
 *   injustices. This constraint exhibits the tangled_rope signature because
 *   it simultaneously coordinates essential institutional continuity AND
 *   extracts from populations seeking reform, creating asymmetric costs
 *   distributed across generational timescales. The constraint appears
 *   immutable (mountain) to observers focused on abstract political theory,
 *   but reveals itself as contingent institutional arrangement (snare,
 *   tangled_rope, scaffold) to agents whose life prospects depend on
 *   constitutional responsiveness. The rising extractiveness trajectory (0.38
 *   → 0.52) and rising theater ratio (0.42 → 0.58) over the 50-year
 *   measurement interval suggests that the constraint's extractive function
 *   is accumulating: initial stabilizing function degrades into protection of
 *   incumbent interests, while constitutional amendment processes become
 *   increasingly ritualistic and unresponsive.
 *
 * KEY AGENTS:
 *   - Provincial Political Elites: Primary beneficiary (institutional/arbitrage) — capture long-term planning security, protected veto positions, low risk of sudden power loss; can use stability to consolidate dominance
 *   - Dissident Provincial Populations: Primary victim (powerless/trapped) — bear costs of constitutional rigidity without exit option; cannot leave jurisdiction without abandoning roots; trapped by geography and legal boundaries
 *   - Governance Reformers: Secondary victim (moderate/constrained) — face high barriers to organizing constitutional change; some career benefits through reform movements; significant personal/reputation costs for challenging stability doctrine
 *   - Constitutional Judiciary: Institutional actor maintaining performative rituals (institutional/arbitrage) — deploys legal reasoning selectively to protect elite interests while claiming neutral principle; sees own role as degraded
 *   - Decentralization & Subsidiary Movements: Organized agents with exit vision (organized/constrained) — benefit from stable framework while organizing toward transition to distributed authority; see current model as temporary scaffold
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent elite preference for rigidity as inherent political necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_constitutional_stability, 0.52).
domain_priors:suppression_score(provincial_constitutional_stability, 0.48).
domain_priors:theater_ratio(provincial_constitutional_stability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_constitutional_stability, extractiveness, 0.52).
narrative_ontology:constraint_metric(provincial_constitutional_stability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(provincial_constitutional_stability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_constitutional_stability, tangled_rope).
narrative_ontology:human_readable(provincial_constitutional_stability, "Provincial Constitutional Stability").
narrative_ontology:topic_domain(provincial_constitutional_stability, "political/constitutional").

domain_priors:requires_active_enforcement(provincial_constitutional_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_constitutional_stability, provincial_political_elites).
narrative_ontology:constraint_beneficiary(provincial_constitutional_stability, historical_power_structures).
narrative_ontology:constraint_victim(provincial_constitutional_stability, dissident_provincial_populations).
narrative_ontology:constraint_victim(provincial_constitutional_stability, aspirational_governance_reformers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSIDENT PROVINCIAL POPULATIONS (SNARE) — Trapped by geographic boundary and legal jurisdiction. Cannot exit the constitutional framework without abandoning livelihood, family, cultural roots. Bears full cost of elite preference for stability over responsive governance. No meaningful exit option; experiences the stability constraint as coercive immobility masquerading as civic duty.
constraint_indexing:constraint_classification(provincial_constitutional_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROVINCIAL POLITICAL ELITES (ROPE) — Experiences the constitutional stability framework as coordination mechanism. Stability enables long-term planning, institutional continuity, predictable power transfers, and protection against sudden redistricting or charter amendment. Benefits from the constraint's enforcement. Arbitrage option: can leverage stability to consolidate power or redirect reform pressure into performative concessions.
constraint_indexing:constraint_classification(provincial_constitutional_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: ASPIRING GOVERNANCE REFORMERS (TANGLED ROPE) — Face high barriers to exit or reform: organizing new political movements risks career/reputation damage; challenging constitutional grounds requires multi-generational organizing; some benefits accrue through established channels (reform movements build capacity, generate policy ideas, create career paths). Extraction is significant but not maximal — real coordination function exists (representing diverse constituencies requires stable institutional framework) alongside asymmetric costs of reform work.
constraint_indexing:constraint_classification(provincial_constitutional_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CONSTITUTIONAL JUDICIARY (PITON) — Maintains interpretive rituals around constitutional stability that are substantially performative. Courts employ precedent, originalism, or living-constitution doctrines to defend the status quo, but these methodologies are deployed selectively — they generate the appearance of neutral legal reasoning while protecting elite interests. Theater ratio high: judicial review consumes time and legitimacy but rarely produces structural change. The judiciary sees itself as degraded — constrained by precedent and political pressure, yet unable to abandon the role without loss of institutional standing.
constraint_indexing:constraint_classification(provincial_constitutional_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: DECENTRALIZATION & SUBSIDIARY MOVEMENTS (SCAFFOLD) — Organized agents (sub-provincial councils, municipal associations, indigenous governance bodies) see provincial constitutional stability as temporary scaffolding for a transition to distributed authority. The movement benefits from the framework (stable institutional base for organizing) while working toward a sunset: deeper subsidiarity, consociational power-sharing, or confederal restructuring would replace the top-down provincial model. Low effective extraction because the constraint is seen as time-bounded and the organized agents have agency and exit pathways.
constraint_indexing:constraint_classification(provincial_constitutional_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of constitutional stability is structurally inherent to governance itself: without legal continuity and predictable boundaries, coordination becomes impossible. The observer may frame this as an immutable law of political order. However, the structural data (extractiveness 0.52, suppression 0.48, victims and beneficiaries clearly identified) contradicts the mountain classification — this represents a false summit where contingent institutional arrangements (elite preference for rigidity) are naturalized as unavoidable political necessity.
constraint_indexing:constraint_classification(provincial_constitutional_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_constitutional_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(provincial_constitutional_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(provincial_constitutional_stability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_constitutional_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(provincial_constitutional_stability, TR),
    TR >= 0.70.

:- end_tests(provincial_constitutional_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The provincial constitutional framework extracts asymmetrically from reform-oriented agents and dissident populations while providing coordination benefits to political elites. The extraction is significant — constitutional amendment barriers are high, organizing costs are substantial, retaliation risks are real — but not maximal because some institutional channels for grievance exist (legislatures, interest groups, media) and some elites acknowledge need for incremental reform. The rising trajectory (0.38 → 0.52) reflects accumulation: initial stabilizing function degrades as incumbent interests use constitutional rigidity to block adaptation. Suppression (0.48): Moderate. Multiple barriers constrain exit and reform: legal amendment procedures are deliberately high-threshold; organizing across provincial boundaries faces coordination costs; career risk for public dissent; media capture by establishment narratives. But suppression is not total — covert organizing occurs, underground press functions, alternative governance experiments proceed in gray zones. Theater ratio (0.58): Moderate-high. Constitutional amendment processes are partly theater: they appear neutral and procedurally fair while consistently protecting elite interests; judicial review supplies legitimacy covering for selective doctrine application; reform movements engage in symbolic action with low probability of structural change. The rising trajectory (0.42 → 0.58) indicates that theater is intensifying — more elaborate procedures, more sophisticated legitimacy work, but less actual responsiveness.
 *
 * PERSPECTIVAL GAP:
 *   Why does the provincial elite see Rope while dissident populations see Snare? The elite's arbitrage option (ability to threaten constitutional amendment as pressure tactic, ability to work through formal channels with influence) produces low d and low chi. The dissident's trapped exit (cannot leave jurisdiction meaningfully; cannot organize effective reform through available channels) produces high d and high chi. The perspectival gap is a pure reflection of structural difference in power and exit capacity. The scaffold perspective adds a temporal dimension: the constraint is temporary from the perspective of agents organizing toward decentralization, while it appears permanent from the trapped agent's biographical horizon. This illustrates how time_horizon shapes classification when exit dynamics change over generational timescales.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is derived from their structural relationship to the constraint's extraction flow. Provincial elites as beneficiaries with arbitrage options experience low d (high negative chi) — they see the constraint as beneficial. Dissident populations as trapped victims experience high d (high positive chi) — they experience maximum extraction. Reformers as moderate victims with constrained exit experience moderate-high d. The judiciary as institutional arbitrageurs see moderate-low d. The decentralization movement as organized agents with constrained exit plus exit vision experience moderate d. The analytical observer's canonical d (0.73) reveals the risk of naturalizing: by failing to differentiate beneficiaries from victims, the 'universal' observer risks endorsing the elite's framing. Explicit directionality overrides are not required because the structural data (beneficiaries and victims clearly identified) drives the derivation chain correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination from extraction along structural lines. The coordination function is real: constitutional stability does enable long-term planning, institutional continuity, and protection against caprice. But the extraction function is also real: the stability mechanism is calibrated to protect incumbent interests at the cost of reform responsiveness, and the costs are borne disproportionately by dissident populations with no exit option. The tangled_rope classification holds both simultaneously: genuine coordination for some agents (elites, judiciary, even some reformers who benefit from stable institutional base) AND genuine extraction from others (trapped dissident populations, reform-blocked constituencies). The rising extractiveness trajectory suggests that the balance is shifting — initial coordination function degrading into pure extraction as elites use constitutional rigidity to entrench against change. If the trajectory continues, the classification may evolve toward snare. If decentralization movements succeed, the structure may transition toward scaffold with genuine sunset. The mandatrophy prevents both the false mountain ('stability is inevitable') and the false snare ('coordination doesn't exist').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_necessity_threshold,
    'What degree of constitutional flexibility preserves functional governance while enabling responsive change? Where is the threshold between necessary stability and extractive rigidity?',
    'Comparative historical analysis of constitutional amendment rates in successful federations (Canada, Switzerland, Australia) vs. dysfunctional ones (Lebanon, Zimbabwe). Correlation between amendment accessibility and governance legitimacy over 50-year horizons.',
    'If threshold is high (amendment rare): current provincial model appears necessary, mountain classification gains support. If threshold is low (amendment common): provincial stability reveals itself as discretionary elite preference, snare and tangled_rope classifications confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_necessity_threshold, empirical, 'Threshold between necessary and extractive constitutional rigidity').

omega_variable(
    reform_exit_capacity,
    'Are barriers to constitutional reform structural (inherent coordination costs) or institutional (deliberately maintained by beneficiaries)? Can organizing capacity overcome them?',
    'Process tracing of recent constitutional reform attempts: cost of organizing, duration, required coalition size, institutional gatekeeping points. Comparison with reform movements in jurisdictions with lower amendment barriers.',
    'If structural: constrains at ''constrained'' exit level justified. If institutional: exit barriers are artificially maintained, reclassifies as ''trapped'' for most agents, elevating snare classification confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_exit_capacity, empirical, 'Whether reform barriers are structural or deliberately maintained').

omega_variable(
    identity_lock_provincial_citizenship,
    'Is provincial constitutional stability binding through internal identity (provinciality is constitutive of selfhood) or external barriers (legal/economic costs to crossing jurisdictional boundaries)?',
    'Analysis of provincial dissident movements: do exit narratives emphasize ''cannot leave'' (trapped) or ''cannot imagine leaving'' (identity_locked)? Ethnographic work on how provincial citizens frame membership and exit possibilities.',
    'If identity-locked: classification remains snare/tangled_rope but binding mechanism is cognitive. If trapped: binding is material (borders, employment, family). Different resolution strategies for each.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_provincial_citizenship, empirical, 'Whether provincial identity creates identity lock or material trapping').

omega_variable(
    decentralization_sunset_viability,
    'Is the decentralization movement''s claimed sunset (transition to subsidiary governance) structurally viable or aspirational theater?',
    'Feasibility analysis of subsidiary governance models at current scale. Track decentralization movement outcomes in comparable jurisdictions. Assess whether provincial elites would accept genuine power devolution.',
    'If viable: scaffold classification holds, genuine exit path exists. If aspirational: movement supplies legitimacy theater, should reclassify as piton or constrained tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_sunset_viability, empirical, 'Whether decentralization represents viable sunset or aspirational theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_constitutional_stability, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcs_tr_t0, provincial_constitutional_stability, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pcs_tr_t25, provincial_constitutional_stability, theater_ratio, 25, 0.51).
narrative_ontology:measurement(pcs_tr_t50, provincial_constitutional_stability, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(pcs_be_t0, provincial_constitutional_stability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pcs_be_t25, provincial_constitutional_stability, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(pcs_be_t50, provincial_constitutional_stability, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_constitutional_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_constitutional_stability, 0.12).
narrative_ontology:affects_constraint(provincial_constitutional_stability, electoral_system_entrenchment).
narrative_ontology:affects_constraint(provincial_constitutional_stability, jurisdictional_boundary_rigidity).
narrative_ontology:affects_constraint(provincial_constitutional_stability, constitutional_amendment_threshold).

% DUAL FORMULATION NOTE:
% Provincial constitutional stability is the parent constraint in a family with electoral system entrenchment, jurisdictional boundary rigidity, and constitutional amendment thresholds. Each has its own ε value reflecting its specific mechanism: the amendment threshold (ε≈0.65, pure barrier) is more extractive than the broader constitutional stability frame (ε≈0.52, mixed coordination). The parent-child relationship captures that stability doctrine justifies high amendment thresholds, creating causal coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_constitutional_stability, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
