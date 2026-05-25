% ============================================================================
% CONSTRAINT STORY: russian_economic_war_footing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russian_economic_war_footing, []).

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
 *   constraint_id: russian_economic_war_footing
 *   human_readable: Russian Economic War Footing: Domestic Extraction Under Military Mobilization
 *   domain: geopolitical_economy/state_mobilization
 *
 * SUMMARY:
 *   Russia's shift to an 'economic war footing' represents a structural
 *   constraint on its domestic population—a systematic extraction of labor,
 *   capital, and consumption to support military and state security apparatus
 *   expansion. The constraint operates through legal conscription, capital
 *   controls, wage suppression, currency manipulation, and confiscatory
 *   taxation justified by military necessity. Unlike coordinate constraints
 *   that solve collective action problems, the economic war footing is a pure
 *   extraction mechanism backed by state coercion. The constraint exhibits
 *   high suppression (0.72) because the population cannot exit without facing
 *   legal penalties, asset seizure, or military punishment. Extractiveness
 *   has risen from 0.45 to 0.68 over an 18-month period as the state has
 *   intensified wage controls and shifted more resources toward military
 *   production. Theater ratio (0.58) reflects a hybrid of genuine military
 *   mobilization and propaganda theater: state media celebrates production
 *   targets and economic sacrifice, but actual execution is fragmented by
 *   corruption and market dynamics that undermine central planning. The
 *   constraint is a snare from most perspectives because it traps the
 *   population through suppression mechanisms rather than coordination
 *   benefits.
 *
 * KEY AGENTS:
 *   - Conscripted/Mobilized Workers: Primary victims (powerless/trapped) — face wage controls, extended hours, legal conscription threat; cannot exit; experience maximum extraction
 *   - Pensioner Households: Secondary victims (powerless/trapped) — fixed nominal income eroded by inflation; cannot work or migrate; suppressed through rationing and price controls
 *   - Small Business Owners: Secondary victims (moderate/constrained) — nominally mobile but face confiscatory exit taxation (capital controls, asset seizure); functionally trapped
 *   - Regional Labor Federations: Tertiary stakeholders (organized/constrained) — perceive mixed coordination and extraction; can negotiate but cannot prevent fundamental suppression
 *   - Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — receives state contracts, labor supply, material allocation, price supports; perceives constraint as enabling coordination
 *   - State Security Apparatus: Secondary beneficiary (institutional/arbitrage) — gains institutional power, budget expansion, enforcement role; identity increasingly fused with wartime mobilization
 *   - International Migration Corridors: Tertiary actor (organized/mobile) — from generational view, represent temporary scaffolding as skilled population exits; suppression is high but declining as networks mature
 *   - Analytical Observer: Global structural view (analytical/analytical) — sees clear snare structure operating via suppression, with long-term sustainability dependent on preventing capital flight and labor shortage feedback
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russian_economic_war_footing, 0.68).
domain_priors:suppression_score(russian_economic_war_footing, 0.72).
domain_priors:theater_ratio(russian_economic_war_footing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russian_economic_war_footing, extractiveness, 0.68).
narrative_ontology:constraint_metric(russian_economic_war_footing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(russian_economic_war_footing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russian_economic_war_footing, snare).
narrative_ontology:human_readable(russian_economic_war_footing, "Russian Economic War Footing: Domestic Extraction Under Military Mobilization").
narrative_ontology:topic_domain(russian_economic_war_footing, "geopolitical_economy/state_mobilization").

domain_priors:requires_active_enforcement(russian_economic_war_footing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russian_economic_war_footing, military_industrial_complex).
narrative_ontology:constraint_beneficiary(russian_economic_war_footing, state_security_apparatus).
narrative_ontology:constraint_beneficiary(russian_economic_war_footing, regional_defense_contractors).
narrative_ontology:constraint_victim(russian_economic_war_footing, civilian_workforce).
narrative_ontology:constraint_victim(russian_economic_war_footing, consumer_economy).
narrative_ontology:constraint_victim(russian_economic_war_footing, small_business_sector).
narrative_ontology:constraint_victim(russian_economic_war_footing, pensioner_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILIZED WORKER (SNARE) — Trapped within national borders by passport/capital controls and legal conscription risk. Faces wage suppression, extended work hours, mandatory production quotas, and confiscatory taxation for 'defense contributions.' No exit option short of emigration (illegal under wartime law). Experiences maximum extraction with zero negotiating power. State can enforce production demands through military discipline and imprisonment.
constraint_indexing:constraint_classification(russian_economic_war_footing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PENSIONER HOUSEHOLD (SNARE) — Fixed income denominated in depreciating rubles; inflation-protected by formula but protection lags actual price increases. Cannot work or migrate. Faces systematic erosion of purchasing power as resources redirect to military. Suppression mechanisms: price controls on essential goods create shortages; alternative markets illegal; state monopoly on distribution. Extraction compounds quarterly as rubles weaken.
constraint_indexing:constraint_classification(russian_economic_war_footing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNER (SNARE) — Can theoretically relocate but faces massive costs: asset seizure, capital controls, exit visa restrictions, family visa complications. Nominally 'mobile' but constrained by confiscatory effective exit taxation. Supply chain disruptions, labor conscription, and forced military contracting extract value. Can exit only by abandoning 80-90% of accumulated capital. Functionally trapped despite legal mobility option.
constraint_indexing:constraint_classification(russian_economic_war_footing, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL LABOR FEDERATION (TANGLED ROPE) — Organized workers perceive some coordination function (state guarantees employment, healthcare, housing allocation during mobilization). Also perceive significant extraction: wage controls, unpaid overtime, blacklisting of dissent, production prioritization over safety. Active enforcement through state security presence on shopfloors. Mixed: confederation gains state negotiating access but cannot translate into meaningful wage gains or working condition improvements. Constrained by legal prohibition on strike action during wartime.
constraint_indexing:constraint_classification(russian_economic_war_footing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. Experiences the constraint as coordination mechanism solving the collective action problem of wartime production scaling. State guarantees contracts, labor supply (conscription), raw materials (state allocation), and price supports. Complex has arbitrage optionality: state contracts can shift between firms; some contractors gain export opportunities through state support. Net extraction flows TOWARD this agent. Perceives constraint as enabling, not restricting — coordination function dominates.
constraint_indexing:constraint_classification(russian_economic_war_footing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL MIGRATION CORRIDOR (SCAFFOLD) — From a generational view, the economic war footing creates temporary but intense extraction that is pushing skilled workers, businesses, and families out of Russia toward EU/US/Central Asia. This migration represents a temporary—but potentially multi-decadal—scaffolding period. The exit corridor is real: thousands are leaving monthly. Suppression is high initially but declines as migration networks mature. From the global observer view, this is a scaffold: high theater (state controls on exit, visa restrictions) that is being worn away by the practical reality of migration becoming cheaper/easier than remaining. Sunset is generational: as the next generation distributes globally, the re-centralization cost becomes prohibitive.
constraint_indexing:constraint_classification(russian_economic_war_footing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: SOVIET-ERA COMMAND ECONOMY RITUAL (PITON) — From a civilizational view, the 'economic war footing' invokes 1940s-1980s command economy language and structure (production quotas, central allocation, labor discipline) but operates within a 21st-century hybrid market economy where most prices are set by markets, not planning boards. Theater ratio is high: planning committees produce targets, state media celebrates overfulfillment, official statistics report success—but actual execution is fragmented, corruption-ridden, and often theatrical. The constraint persists through institutional inertia and security apparatus ritual, not because command planning actually works at scale in modern Russia.
constraint_indexing:constraint_classification(russian_economic_war_footing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a global/civilizational view, the Russian economic war footing is a snare: it extracts resources from a trapped population with suppression mechanisms (capital controls, migration restrictions, surveillance, conscription threat) that operate at civilizational scale. The state apparatus has structural capacity to maintain this extraction for extended periods because: (1) the suppression infrastructure (FSB, military, bureaucracy) is large enough to police the population; (2) the exit costs are sufficiently high that even skilled workers face significant barriers; (3) the constraint is enforced by military coercion ultimately backing economic punishment. Unlike coordinate constraints that degrade from disuse, snares can persist for decades via continuous enforcement—as the Soviet Union demonstrated. The analytical view sees the snare structure clearly.
constraint_indexing:constraint_classification(russian_economic_war_footing, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_economic_war_footing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russian_economic_war_footing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russian_economic_war_footing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russian_economic_war_footing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russian_economic_war_footing, TR),
    TR >= 0.70.

:- end_tests(russian_economic_war_footing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The state extracts through: wage controls (nominal wages frozen or increased below inflation), mandatory taxation for 'defense contributions,' currency depreciation (rubles weakened ~30% vs USD in period), conscription (unpaid labor), and enterprise requisition. The 18-month trajectory from 0.45 to 0.68 reflects progressive tightening of controls. This is not moderate extraction but severe—the civilian population is systematically deprived of purchasing power and labor autonomy. Suppression (0.72): High. Capital controls prevent asset flight; exit visas are restricted; conscription laws threaten male population; military discipline is imposed on factories; state monopolies control critical goods distribution; dissent is criminalized. The suppression infrastructure (FSB, military, border guards) has sufficient capacity to enforce these mechanisms for extended periods. Theater ratio (0.58): Moderate-high. Planning committees produce production targets and celebrate overfulfillment; state media reports economic success; official statistics show growth—but actual execution is fragmented by black markets, corruption, and the reality that modern Russia cannot replicate 1940s command economy efficiency. The theater is real (state invests resources in propaganda) but not dominant; genuine extraction mechanisms underpin it.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap exists between the military-industrial complex (Rope) and the conscripted worker (Snare). From the complex's view, the constraint solves production coordination: state guarantees labor, supplies, contracts, and prices. The 'extraction' is minor compared to the coordination benefit. From the worker's view, there is no coordination—only coercion. The state does not negotiate with workers; it conscripts them. Wages are controlled, not negotiated. The gap is unbridgeable because the two agents have opposite d values (low for complex, high for worker) and opposite experienced extraction directions (toward complex, away from worker). The analytical observer recognizes this as a structurally extractive constraint: coordination requires voluntary participation; conscription does not permit voluntary exit; therefore no genuine coordination exists. The state's framing of the constraint as 'collective defense' is a cover story—the actual mechanism is suppression-backed extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows toward the state apparatus and military-industrial complex (d ≈ 0.05 for institutional beneficiaries with arbitrage options, negative effective extraction chi) and away from the population (d ≈ 0.92 for trapped workers with no exit, maximum effective extraction chi). The beneficiary (military-industrial complex) derives d from: institutional power + arbitrage exit (can shift contracts between firms, access export opportunities, relocate within state system) = low d = low chi = negative or near-zero experienced extraction. The victim (conscripted worker) derives d from: powerless power + trapped exit (cannot leave Russia legally, faces conscription threat, has no arbitrage options) = high d = high chi = maximum experienced extraction. The gradient is sharp because the constraint has institutional enforcement (military law backing conscription) and no private exit alternatives. Suppression is not scaled by d or scope—it is a raw structural property (0.72)—meaning all agents face identical suppression mechanisms, but victims experience it at high chi because their d is high.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that perspectives 1-7 (worker, pensioner, business owner, labor federation, military-industrial complex, international migration, Soviet ritual) are all legitimate readings from their respective structural positions, but perspective 8 (analytical) reveals a critical inversion: the state's framing of 'economic war footing' as a coordination problem is false. True coordination would permit exit and negotiation; this constraint permits neither. The state uses military coercion to back extraction, not coordination. The mandatrophy is not 'which type is correct?' but 'who is benefiting and who is bearing costs?' The answer is unambiguous: the military-industrial complex and state security apparatus benefit (Rope or net-zero extraction from their perspective); the civilian population bears costs (Snare at maximum extraction). The constraint does not resolve to rope because it lacks the voluntary participation necessary for coordination. It is a snare maintained by suppression infrastructure, with theatre (planning committees, propaganda) providing legitimation but not function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_flight_sustainability,
    'Can capital controls and asset seizure mechanisms prevent sufficient wealth transfer abroad to degrade the extraction constraint?',
    'Tracking actual capital flight volume vs state seizure rates; monitoring parallel market exchange rates (divergence indicates control failure); measuring state revenues from asset confiscation vs economic contraction costs',
    'If capital flight succeeds: extractiveness declines as elites relocate assets (d decreases, f(d) decreases). If state controls hold: extraction deepens (d increases). Determines whether the constraint is sustainable long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flight_sustainability, empirical, 'Whether capital controls can prevent wealth transfer and maintain extraction').

omega_variable(
    labor_productivity_collapse_threshold,
    'At what conscription/wage suppression level does labor productivity collapse sufficiently to make the extraction economically counter-productive?',
    'Factory output metrics, quality defect rates, worker sabotage indicators, replacement equipment costs; comparison to USSR wartime labor productivity patterns',
    'If threshold exceeded early: state forced to reduce extraction pressure (suppression decreases). If threshold never reached: extraction can be sustained indefinitely. Determines upper bound of viable suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_productivity_collapse_threshold, empirical, 'Productivity threshold for counter-productive extraction').

omega_variable(
    demographic_replacement_failure,
    'Will generational outflow through migration (scaffold perspective) eventually create labor shortages that force the state to reduce extraction pressure to retain workers?',
    'Demographic cohort analysis; emigration rates by age group; state policy responses to labor shortages; wage/benefit increases offered to retain workers',
    'If yes: scaffold sunset becomes real—generational replacement failure forces renegotiation (constraint degrades to rope). If no: state can maintain snare indefinitely through forced immigration/conscription. Determines temporal horizon of constraint viability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_replacement_failure, empirical, 'Whether migration will eventually force state to reduce extraction').

omega_variable(
    elite_identity_lock_durability,
    'Is the state security apparatus''s identity fused with wartime extraction such that peacetime renegotiation becomes cognitively/institutionally impossible?',
    'Institutional analysis of FSB/military integration into economic system; documentation of stated ideologies about permanent mobilization; measurement of career incentives and promotion pathways within security apparatus',
    'If yes: constraint persists even after external military threat declines (institutional inertia prevents renegotiation). If no: state can shift policy when incentives change. Determines whether snare converts to rope upon threat reduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_identity_lock_durability, conceptual, 'Whether security apparatus identity depends on wartime extraction').

omega_variable(
    international_sanctions_feedback,
    'Do international sanctions increase state dependence on internal extraction, or do sanctions provide cover story for extraction that would occur anyway?',
    'Counterfactual analysis: compare extraction rates under sanctions vs historical extraction without sanctions; measure state propaganda framing of sanctions-driven vs foreign-aggression-driven necessity',
    'If sanctions-driven: external shock sustains snare (but creates external exit pressure via sanctions logic). If cover story: extraction is endogenous (snare is independent constraint, not response to external coercion). Determines whether constraint resolves through sanctions relief or structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_sanctions_feedback, empirical, 'Whether sanctions amplify or justify economic war footing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_economic_war_footing, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rewf_tr_t0, russian_economic_war_footing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rewf_tr_t6, russian_economic_war_footing, theater_ratio, 6, 0.5).
narrative_ontology:measurement(rewf_tr_t12, russian_economic_war_footing, theater_ratio, 12, 0.58).
narrative_ontology:measurement(rewf_tr_t18, russian_economic_war_footing, theater_ratio, 18, 0.62).

% Extraction over time
narrative_ontology:measurement(rewf_be_t0, russian_economic_war_footing, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rewf_be_t6, russian_economic_war_footing, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(rewf_be_t12, russian_economic_war_footing, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(rewf_be_t18, russian_economic_war_footing, base_extractiveness, 18, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russian_economic_war_footing, enforcement_mechanism).
narrative_ontology:affects_constraint(russian_economic_war_footing, russian_capital_controls).
narrative_ontology:affects_constraint(russian_economic_war_footing, conscription_labor_mobilization).
narrative_ontology:affects_constraint(russian_economic_war_footing, ruble_currency_depreciation).
narrative_ontology:affects_constraint(russian_economic_war_footing, international_sanctions_regime).

% DUAL FORMULATION NOTE:
% The economic war footing constraint is downstream of geopolitical conflict (Ukraine war, NATO expansion threat narrative) but represents a distinct structural constraint on Russian domestic population. The upstream constraints (sanctions regime, military losses) create state incentives for extraction; the economic war footing constraint is how those incentives are implemented domestically. The upstream and downstream constraints reinforce each other: sanctions justify extraction; extraction justifies conscription; conscription justifies suppression. Decomposition separates the mechanisms while preserving causal relationships through affects_constraints network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russian_economic_war_footing, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
