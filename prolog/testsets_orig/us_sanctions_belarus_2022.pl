% ============================================================================
% CONSTRAINT STORY: us_sanctions_belarus_2022
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_belarus_2022, []).

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
 *   constraint_id: us_sanctions_belarus_2022
 *   human_readable: U.S. Sanctions Regime Against Belarus (2022)
 *   domain: geopolitical/economic_coercion
 *
 * SUMMARY:
 *   The U.S. sanctions regime against Belarus, escalated in 2022 following
 *   the disputed 2020 presidential election and human rights abuses,
 *   represents a complex hybrid constraint that operates simultaneously as
 *   geopolitical leverage (for the U.S.), economic strangulation (for
 *   Belarusian civilians), organized opposition enablement (for exiled
 *   activists), and regime-stability mechanism (paradoxically, for the
 *   Lukashenko regime by tightening Russian alliance). The constraint
 *   exhibits snare characteristics from the victim perspective (civilians
 *   trapped without exit options), tangled rope characteristics from the
 *   regime perspective (mixed extraction and forced coordination), rope
 *   characteristics from the U.S. perspective (policy lever with arbitrage
 *   capacity), and performative piton characteristics from the international
 *   sanctions architecture perspective. Theater ratio (0.55) reflects that
 *   sanctions enforcement is substantial and material (not pure theater), but
 *   functional outcomes (regime change, human rights improvement) are
 *   marginal relative to enforcement intensity. The constraint is best
 *   classified as a snare because the primary structural consequence — severe
 *   economic and humanitarian extraction from the Belarusian population —
 *   persists regardless of geopolitical intent, and victims have no
 *   legitimate exit mechanism.
 *
 * KEY AGENTS:
 *   - Belarusian Civilian Population: Primary victim (powerless/trapped) — bears direct costs through economic collapse, medicine/fuel shortages, currency depreciation; no exit mechanism except emigration or capitulation
 *   - Belarusian Economy: Primary victim (powerless/trapped) — export markets closed, financing severed, supply chains disrupted; structurally unable to exit sanctions perimeter
 *   - Lukashenko Regime: Secondary actor (moderate/constrained) — benefits from rallying effect and Russian alliance, but loses tax revenue and asset access; exit options exist only through geopolitical capitulation
 *   - U.S. Foreign Policy Apparatus: Primary beneficiary (institutional/arbitrage) — uses sanctions as coordination signal to NATO allies, demonstrates resolve on human rights, maintains leverage without military commitment
 *   - Non-Aligned Trading Partners: Secondary victim (powerful/constrained) — face secondary sanctions pressure; benefit from access to Belarusian commodities at depressed prices but constrained by U.S. enforcement mechanisms
 *   - International Sanctions Architecture: Institutional observer (institutional/arbitrage) — maintains performative enforcement ritual; has arbitrage capacity to lift sanctions for political concessions
 *   - Western Political Opposition in Exile: Secondary beneficiary (organized/mobile) — sanctions legitimize and materially support exiled opposition; have exit and arbitrage capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_belarus_2022, 0.68).
domain_priors:suppression_score(us_sanctions_belarus_2022, 0.72).
domain_priors:theater_ratio(us_sanctions_belarus_2022, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_belarus_2022, snare).
narrative_ontology:human_readable(us_sanctions_belarus_2022, "U.S. Sanctions Regime Against Belarus (2022)").
narrative_ontology:topic_domain(us_sanctions_belarus_2022, "geopolitical/economic_coercion").

domain_priors:requires_active_enforcement(us_sanctions_belarus_2022).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_belarus_2022, us_foreign_policy_leverage).
narrative_ontology:constraint_beneficiary(us_sanctions_belarus_2022, western_political_opposition_in_exile).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarusian_civilian_population).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarusian_economy).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, non_aligned_trading_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BELARUSIAN CIVILIAN POPULATION (SNARE) — Cannot exit the sanctions regime; bears direct costs through currency collapse, medicine shortages, fuel rationing, and economic contraction. No organized exit mechanism. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BELARUSIAN ECONOMY (SNARE) — Trapped within sanctions perimeter. Export markets closed, foreign currency reserves frozen, access to international financing severed, supply chains disrupted. No mechanism to exit except through geopolitical capitulation. Pure extraction with suppression of alternative economic pathways.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LUKASHENKO REGIME (TANGLED ROPE) — Constrained by sanctions but retains substantial coercive capacity over domestic population and maintains alliance with Russia. Experiences sanctions as both extraction (lost revenue, frozen assets) and coordination signal (forced into Russian orbit). Can partially exit through capitulation on human rights or regime change, but exit is politically suicidal. Mixed structure with active enforcement burden.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: U.S. FOREIGN POLICY APPARATUS (ROPE) — Benefits from sanctions as a coordination and signaling mechanism. Uses sanctions to demonstrate resolve to NATO allies, signal disapproval of human rights abuses, and maintain pressure without direct military engagement. Has exit options (lift sanctions for political concessions) and experiences the regime primarily as a policy lever rather than as extraction. Beneficiary with arbitrage capacity.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-ALIGNED TRADING PARTNERS (TANGLED ROPE) — Face pressure to comply with secondary sanctions and enforcement mechanisms. Benefit from access to Belarusian commodities at below-market rates due to sanctions-induced desperation. Extraction takes the form of forced choice between market opportunity and U.S. regulatory risk. Constrained exit — cannot freely trade with Belarus without secondary sanctions consequences. Active enforcement maintains the extraction.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL SANCTIONS ARCHITECTURE (PITON) — Sanctions represent a performative demonstration of institutional coordination through OFAC enforcement, EU parallel sanctions, and multilateral rhetoric. The actual functional impact (regime change, policy modification) is marginal — Belarus remains stable despite sanctions, aligns closer with Russia, and human rights record worsens. The sanction ritual persists through institutional inertia despite low functional success. Theater ratio reflects the gap between enforcement intensity and actual behavioral outcomes.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT MOUNTAIN) — From a civilizational view, the sanctions regime appears to reflect an immutable constraint: the incompatibility between U.S. human rights standards and Belarusian governance represents an irreducible structural conflict with no accommodation. This framing naturalizes what is actually a contingent geopolitical choice — the U.S. could recognize Belarus within existing sovereignty norms, but chooses not to. The mountain classification is a false summit: sanctions are institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_belarus_2022_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_belarus_2022, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_belarus_2022, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_belarus_2022_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sanctions regime extracts significant economic value from the Belarusian population through currency collapse (belarus ruble lost >40% of value), GDP contraction (estimated 3-5% annually), medicine shortages (pharmaceutical imports collapsed), and energy price spikes. This extraction is structural to the regime's design — sanctions target the entire economy, not specific sectors or actors. The value reflects that extraction is comprehensive and unavoidable for trapped populations. Suppression (0.72): Very high. Suppression mechanisms include: (1) no legitimate negotiation pathway (regime change is unstated but obvious prerequisite), (2) inability to exit Belarus (border fortification, visa restrictions into neighboring countries), (3) currency controls preventing capital flight, (4) lack of alternative trading partners at scale, (5) international coordination making secondary sanctions near-universal. The regime benefits from suppression because it drives Russian alliance; civilians bear the cost. Theater ratio (0.55): Moderate. The sanctions regime is not purely theatrical — actual enforcement through OFAC, EU, and UN mechanisms is material and consequential. However, the theater reflects the gap between intensity of enforcement and actual functional outcomes. Sanctions have not changed Belarusian policy, improved human rights, achieved regime change, or broken Russian alliance. The ritual persists despite failure, suggesting some performative maintenance for domestic political audiences.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. From the Belarusian civilian perspective, this is a snare — pure extraction with no coordination benefit and no exit. From the U.S. perspective, this is a rope — a coordination mechanism signaling resolve to allies with minimal U.S. cost. From the regime perspective, this is a tangled rope — extraction through lost revenue and asset freezes, but coordination benefit through forced Russian alliance tightening and rally-around-the-flag effect on domestic legitimacy. From the international sanctions architecture perspective, this is a piton — institutional ritual maintained despite marginal functional success. The regime's constrained exit (can capitulate on human rights but faces domestic collapse) creates a stable snare structure: the regime has no mechanism to lift sanctions without losing power, and civilians have no mechanism to exit without regime capitulation. This structural lock explains why sanctions persist: they are politically painless for the U.S. (arbitrage exit available), politically useful for the regime (Russian alliance justification), and politically impossible to escape for civilians.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position and exit capacity. Belarusian civilians have no exit (trapped) and are victims of extraction (d → 0.95, approaching 1.0 / full target). The regime has constrained exit (capitulation possible but politically suicidal) and is both victim and beneficiary (d → 0.55-0.65, mixed). The U.S. has full arbitrage exit (lift sanctions for concessions, impose for pressure) and is beneficiary (d → 0.10-0.15, close to full beneficiary). Non-aligned partners have arbitrage options (can defy secondary sanctions at regulatory cost) and benefit from trade opportunity while bearing enforcement risk (d → 0.50-0.55, symmetric). The derivation chain maps these structural positions to effective extractiveness chi through the sigmoid f(d), showing that civilians experience the constraint most intensely while the U.S. experiences minimal extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the snare vs tangled-rope tension by disambiguating the beneficiary structure. The snare classification is correct for the comprehensive victim population (civilians, economy), but the regime perspective (tangled rope) reveals that the constraint has a genuine coordination function — it coordinates the regime's alliance with Russia and signals strength domestically. However, the snare classification dominates because: (1) the primary structural outcome (civilian suffering) is driven by the regime's incentive to tighten Russian ties rather than by genuine bilateral coordination, (2) the regime's cooperation is extracted through coercion (sanctions pain), not volunteered as coordination benefit, (3) the U.S. cannot credibly negotiate an alternative (regime change is implicit prerequisite), and (4) the constraint shows no mechanism for voluntary exit or negotiated settlement. The mandatrophy is resolved by showing that what appears as tangled-rope coordination from the regime perspective (Russian alliance) is actually snare-driven extraction from the civilian perspective — the regime's 'benefit' comes from transfer of civilian suffering into alliance dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_change_objective_clarity,
    'Is the sanctions regime designed to achieve regime change in Belarus or merely to signal disapproval and constrain specific sectors?',
    'Analysis of official policy statements vs actual sanction design; assessment of whether sanctions architecture enables or prevents negotiated settlement; timeline correlation between sanctions escalation and public regime-change rhetoric',
    'If regime-change objective: sanctions-induced civilian suffering is instrumentally justified (snare by design). If constraint objective: civilian suffering is externality (snare by effect). Classification remains snare from victim perspective either way, but mandatrophy diagnosis changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_change_objective_clarity, empirical, 'Whether sanctions aim at regime change or behavioral constraint').

omega_variable(
    secondary_sanctions_enforcement_reality,
    'Do secondary sanctions against non-aligned trading partners actually enforce the primary sanctions regime or merely shift trade to non-Western networks?',
    'Quantitative tracking of Belarus export flows; comparison of trade volumes pre-sanctions vs during sanctions to Western vs non-Western partners; measurement of actual enforcement vs threat of enforcement',
    'If enforcement effective: non-aligned partners experience real extraction (constrained choices). If ineffective: extraction is minimal and snare classification for non-aligned partners downgrades to rope. The regime''s actual economic lifeline (Russia, China, Central Asia) remains open.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_sanctions_enforcement_reality, empirical, 'Whether secondary sanctions materially constrain third-party trade with Belarus').

omega_variable(
    humanitarian_externality_vs_design,
    'Is civilian economic suffering a predicted consequence of sanctions design or an unintended externality?',
    'Policy analysis of sanctions targeting specificity; comparison to alternative mechanisms (targeted individual sanctions, travel bans, asset freezes) that could achieve same policy goals with lower civilian impact; assessment of whether humanitarian impact modeling informed design',
    'If predicted design: snare structure is ethically transparent — civilians are intentional targets. If externality: snare structure remains but represents failure of precision targeting. Victim classification does not change; moral-political interpretation does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_externality_vs_design, conceptual, 'Whether civilian suffering reflects design or unintended consequence').

omega_variable(
    russian_alliance_causality,
    'Do sanctions push Belarus closer to Russia or does Belarus already seek Russian alignment for structural reasons independent of sanctions pressure?',
    'Counterfactual analysis using pre-2020 Belarusian foreign policy; timeline analysis of Russian integration steps vs sanctions escalation; comparison to other post-Soviet states without sanctions (Kazakhstan, Kyrgyzstan)',
    'If sanctions-driven: snare classification is accurate — sanctions cause the outcome they ostensibly oppose. If structural alignment: snare remains but causal attribution errors. The regime''s constrained exit options (perspective 3) drive behavior, not external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_alliance_causality, empirical, 'Whether sanctions causally drive Russian alignment or accelerate existing trend').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_belarus_2022, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usbe_tr_t0, us_sanctions_belarus_2022, theater_ratio, 0, 0.38).
narrative_ontology:measurement(usbe_tr_t6, us_sanctions_belarus_2022, theater_ratio, 6, 0.48).
narrative_ontology:measurement(usbe_tr_t12, us_sanctions_belarus_2022, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(usbe_be_t0, us_sanctions_belarus_2022, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usbe_be_t6, us_sanctions_belarus_2022, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(usbe_be_t12, us_sanctions_belarus_2022, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_belarus_2022, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sanctions_belarus_2022, russia_belarus_integration).
narrative_ontology:affects_constraint(us_sanctions_belarus_2022, ukrainian_refugee_corridor).
narrative_ontology:affects_constraint(us_sanctions_belarus_2022, eastern_european_sanctions_contagion).

% DUAL FORMULATION NOTE:
% The Belarus sanctions regime is downstream of the 2020 Belarusian election dispute and the 2022 Russian invasion of Ukraine, but represents a distinct structural constraint with its own extractiveness and enforcement mechanisms. Sister constraints (Russian-Belarusian integration, Ukrainian refugee effects) interact through shared victim populations and geopolitical interdependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sanctions_belarus_2022, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
