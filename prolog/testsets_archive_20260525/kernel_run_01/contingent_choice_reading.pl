% ============================================================================
% CONSTRAINT STORY: contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_contingent_choice_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: contingent_choice_reading
 *   human_readable: Bretton Woods Transition as Contingent Policy Choice
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The Bretton Woods collapse of 1971 is the primary observable. The
 *   contingent choice reading frames Nixon's decision to end dollar-gold
 *   convertibility and float the dollar as a deliberate policy choice that
 *   COULD have been avoided with different policy paths — sustained gold
 *   discipline, a reformed reserve system (expanded SDR), or coordinated
 *   revaluation. From this reading, the transition was not overdetermined by
 *   structural forces but rather was a causal decision node where U.S.
 *   policymakers deliberately chose to exit a constraining regime and shift
 *   adjustment costs onto dollar-holding economies. The extractiveness arises
 *   from the asymmetric distribution of transition costs: developing
 *   economies that held dollars (as reserves and debt denominations)
 *   experienced uncompensated currency devaluation, while the U.S. gained
 *   monetary autonomy and captured seigniorage. The suppression is moderate —
 *   not total constraint, but significant barriers to exit or counter-action
 *   by affected parties (no coordination mechanism to demand compensation; no
 *   alternative currency system). This reading is one of three competing
 *   framings of the same historical event: the overdetermined-collapse
 *   reading emphasizes structural inevitability (Triffin dilemma, impossible
 *   math); the hybrid-trigger reading describes Nixon's choice as endogenous
 *   response to exogenous triggers (Bretton Woods was doomed, but policy
 *   choice shaped how the doom unfolded). All three readings are live in
 *   academic monetary-economics debate.
 *
 * KEY AGENTS:
 *   - U.S. Monetary Authority / Nixon Administration (institutional/arbitrage): primary decision-maker and beneficiary. Initiated the shock; gained monetary autonomy and seigniorage.
 *   - Developing Economy Dollar Creditors (powerless/trapped): primary victims. Held dollars for reserves; experienced uncompensated devaluation. No exit option; no coordination mechanism to demand compensation.
 *   - Allied Industrial Economies / G-10 (institutional/constrained): secondary actors. Could demand gold conversion or coordinate response; chose instead to negotiate new regime terms. Constrained by need to maintain alliance and manage their own economies.
 *   - International Financial Institutions (organized/constrained): managed shock absorption and transition to new regime. IMF and World Bank became coordinators of post-transition adjustment.
 *   - Analytical Observer (analytical/analytical): civilizational view. Risk of naturalizing deliberate policy choice as structural inevitability — the Triffin dilemma was real, but solutions existed beyond the path taken.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(contingent_choice_reading, 0.52).
domain_priors:suppression_score(contingent_choice_reading, 0.48).
domain_priors:theater_ratio(contingent_choice_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(contingent_choice_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(contingent_choice_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(contingent_choice_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(contingent_choice_reading, "Bretton Woods Transition as Contingent Policy Choice").
narrative_ontology:topic_domain(contingent_choice_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(contingent_choice_reading, distributed).
narrative_ontology:cs_authority_grounding(contingent_choice_reading, expertise).
narrative_ontology:cs_kernel_id(contingent_choice_reading, transition_causality).
narrative_ontology:cs_reading_relation(contingent_choice_reading, overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation(contingent_choice_reading, hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom(contingent_choice_reading, foundational, policy_autonomy_in_transition).
narrative_ontology:cs_axiom_status(policy_autonomy_in_transition, holdable).
narrative_ontology:cs_axiom(contingent_choice_reading, foundational, alternative_policy_viability).
narrative_ontology:cs_axiom_status(alternative_policy_viability, holdable).
narrative_ontology:cs_reference_frame(contingent_choice_reading, constrained_u_s_monetary_autonomy).
narrative_ontology:cs_drift_state(contingent_choice_reading, post_1971_floating_regime, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(contingent_choice_reading, us_monetary_autonomy).
narrative_ontology:constraint_beneficiary(contingent_choice_reading, us_financial_sector).
narrative_ontology:constraint_victim(contingent_choice_reading, fixed_exchange_regime_members).
narrative_ontology:constraint_victim(contingent_choice_reading, developing_economy_creditors).
narrative_ontology:constraint_victim(contingent_choice_reading, global_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMY CREDITORS (SNARE) — Locked into dollar holdings through Bretton Woods architecture; cannot exit without total currency loss. Transition imposes maximum extraction: dollar devaluation wipes purchasing power while the US escapes constraints. Trapped, powerless, and bearing uncompensated cost of U.S. policy choice.
constraint_indexing:constraint_classification(contingent_choice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED INDUSTRIAL ECONOMIES (TANGLED ROPE) — Germany, France, Japan face constrained exits: can demand gold conversion (which U.S. blocks anyway) or accept new regime terms. The transition both coordinates new trade-settlement rules AND extracts from their gold reserves through inflation. Mixed experience: coordination benefit from clearer exchange-rate regime eventually, extraction cost in the immediate shock and reserve erosion.
constraint_indexing:constraint_classification(contingent_choice_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. MONETARY AUTHORITY (ROPE) — Escapes gold-standard constraint through deliberate choice; gains monetary autonomy and seigniorage. Experiences the transition as problem-solving coordination: establishing new Nixon Shock regime, floating rates, exorbitant privilege. Beneficiary with arbitrage options — can impose regime change and shift to favorable terms. Pure coordination from this vantage: the constraint-breaking IS the solution they were seeking.
constraint_indexing:constraint_classification(contingent_choice_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL FINANCIAL INSTITUTIONS (SCAFFOLD) — IMF, World Bank, and successor regimes (Plaza Accord, etc.) manage the transition from fixed to floating rates with sunset logic. Designed to be temporary shock absorbers; contain contagion; build new coordination rules (SDR, managed float frameworks). Organized actors with constrained but strategic agency — they negotiate terms rather than absorb costs unilaterally. Theater here is LOW: institutional rebuilding is functional necessity, not ritual.
constraint_indexing:constraint_classification(contingent_choice_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED CENTRAL BANKS (TANGLED ROPE) — Coordinate management of transition shock alongside U.S. Federal Reserve and Treasury; constrained by need to maintain allied relationships while defending their own monetary autonomy. Coordination function: stabilizing markets through coordinated intervention (G-10 agreements post-1971). Extraction component: U.S. influences exchange-rate outcomes despite nominally floating regime; gets first-mover advantage in new currency hierarchy. Active enforcement: repeated interventions, policy coordination meetings, bilateral agreements.
constraint_indexing:constraint_classification(contingent_choice_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — STRUCTURAL INEVITABILITY (MOUNTAIN) — From civilizational timescale, Bretton Woods collapse was inevitable: the Triffin dilemma (dollar supply growth ≠ gold supply growth) made the regime mathematically unsustainable. This perspective sees the transition not as policy choice but as structural law — the constraint of fixed-ratio commodity backing could not survive modern finance. However, this reading naturalizes what is actually a contingent institutional choice about how to resolve the dilemma (gold standard, SDR-based reserve system, free-floating, or various hybrids). The false-summit detector will flag this natural-law framing as obscuring deliberate choices about who bears the adjustment costs.
constraint_indexing:constraint_classification(contingent_choice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(contingent_choice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(contingent_choice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(contingent_choice_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The transition imposes real costs on dollar-holding economies and benefits the U.S. substantially through monetary autonomy and seigniorage gains. This is not maximum extraction because the transfer was neither total nor coercively enforced on unwilling participants — other economies eventually negotiated new trade and settlement rules that constrained pure extraction. The value reflects the initial shock magnitude (uncompensated devaluation for dollar holders) moderated by subsequent negotiation and adaptation. Base extractiveness grew from 0.32 (pre-transition, when Bretton Woods was merely constraining but not yet extracting via transition mechanism) to 0.55 (post-transition aftermath, as distributional consequences became clear and uncompensated costs accumulated). Suppression (0.48): Moderate. Victims face significant barriers to counter-action — no alternative reserve currency, no coordination mechanism to demand compensation, no exit from dollar exposure. But suppression is not total — some countries held gold reserves, some could demand negotiated terms post-shock, some eventually exited dollar dependence through regional currency arrangements. Theater ratio (0.38): LOW. This is critical for distinguishing this reading. The contingent choice framing sees the transition as real functional problem-solving (U.S. needed to escape monetary constraint; world needed a new settlement system), not as ritualized performance. The low theater reflects that the policy decision was functionally driven and analytically transparent — economists and policymakers openly discussed the Triffin dilemma, the need for monetary autonomy, and the costs of the transition. Compare to the overdetermined-collapse reading, which might have higher theater (naturalizing the collapse as inevitable) or the hybrid-trigger reading (which might show theater in the rituals built around presenting the transition as necessary).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The U.S. monetary authority (institutional/arbitrage) sees pure coordination and beneficial autonomy (Rope from their view). Dollar-holding developing economies (powerless/trapped) see pure extraction with no escape (Snare). Allied central banks (institutional/constrained) see mixed coordination-extraction (Tangled Rope) — they negotiate new rules while bearing adjustment costs. International institutions (organized/constrained) see a temporary shock to be managed toward new equilibrium (Scaffold). The civilizational analytical observer risks seeing structural necessity (Mountain — Triffin dilemma made it inevitable) but this reading naturalize what was actually a contingent choice about WHO bears the adjustment costs. The perspectival gap reveals the core disagreement: is the transition's distributional outcome (U.S. gain, dollar-holder loss) a structural necessity or a deliberate policy choice?
 *
 * DIRECTIONALITY LOGIC:
 *   The contingent choice reading frames the transition as benefiting the U.S. and imposing costs on dollar-holding economies. This beneficiary/victim structure drives directionality: U.S. monetary authority (beneficiary with arbitrage options) experiences low d; developing economies (victims with trapped status) experience high d; allied economies (partial victims with constrained options) experience moderate d. The engine's directionality derivation from beneficiary/victim + exit_options produces the perspectival gap: beneficiaries see low/negative χ (coordination), victims see high χ (extraction). No directionality overrides are needed — the structural data (beneficiary = U.S. autonomy; victims = dollar-holder losses) directly produces the required d values through the sigmoid function.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the kernel framing. The six constraint types DO NOT represent six competing correct answers to 'what type is Bretton Woods?' Rather, they represent six perspectival readings of a single constraint FROM DIFFERENT POSITIONS. The mandatrophy — the apparent confusion about whether this is coordination, extraction, inevitability, or transition — disappears when you recognize that it IS all of these FROM DIFFERENT POSITIONS. The contingent_choice reading does not claim to be the unique correct classification. It claims to be ONE READING that emphasizes policy autonomy and distributional choice. The overdetermined-collapse reading will produce similar types FROM THE SAME POSITIONS (e.g., Snare from developing-economy view) but with different ε and different rationale (structural inevitability vs. deliberate extraction). The engine's job is to model both readings and flag the disagreement as a kernel-reading dispute, not as a classification error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_policy_viability,
    'Could the Bretton Woods regime have persisted with different policy choices (sustained gold discipline, expanded SDR, collective revaluation)?',
    'Counterfactual economic modeling of alternative policy paths (1968–1971); analysis of technical barriers vs. political barriers to each alternative; comparison with post-2008 crisis policy responses (showing political choice under similar structural pressure)',
    'If viability HIGH: contingent choice reading confirmed — transition was avoidable; reveals policy autonomy and distributional choices. If viability LOW: transition was overdetermined structural collapse; contingent choice reading weakened; shifts credibility toward overdetermined-collapse sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_policy_viability, conceptual, 'Whether Bretton Woods had viable policy alternatives').

omega_variable(
    causal_priority_for_collapse,
    'Was the Nixon decision the PRIMARY causal driver (contingent choice reading), a symptom of structural forces (overdetermined collapse reading), or a hybrid with endogenous policy response to exogenous trigger (hybrid trigger reading)?',
    'Historical sequencing analysis: identify decision points where actors could have chosen differently; compare counterfactual scenarios to actual events; assess whether ''structural inevitability'' narratives are post-hoc rationalization of specific policy choices; review Nixon administration internal memos on alternatives considered and rejected.',
    'PRIMARY DRIVER framing: contingent choice reading is correct; Nixon decision was genuine causal node. SYMPTOM framing: overdetermined collapse reading is correct. HYBRID framing: hybrid trigger reading is correct — validates all three readings as capturing different dimensions of same event.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_priority_for_collapse, conceptual, 'Causal priority of policy choice vs. structural determination').

omega_variable(
    distributional_intent_vs_necessity,
    'Was the Nixon Shock''s distributional outcome (U.S. gain, dollar-holder loss) a necessary consequence of any exit from fixed exchange, or did it result from specific policy choices about adjustment mechanisms (floating vs. coordinated revaluation)?',
    'Analysis of 1968–1971 policy debates; comparison of U.S. policy outcomes to outcomes under alternative exit mechanisms (e.g., French proposal for gold-based system, German revaluation proposal); assessment of whether U.S. chose floating rates specifically to maximize its extraction capacity rather than to minimize global adjustment cost.',
    'If necessary consequence: distributional outcome could not have been avoided; contingent choice framing is technically correct (choice to exit) but extraction distributional pattern was structural. If result of specific choices: strongly validates contingent choice reading — multiple policy paths existed with different distributional outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_intent_vs_necessity, empirical, 'Whether distributional pattern was structural necessity or chosen outcome').

omega_variable(
    kernel_reading_classification_ambiguity,
    'Is this constraint best understood as ONE reading of a contested kernel (transition_causality), or does each sibling reading describe a distinct structural constraint with its own ε value?',
    'Compare ε values across all three readings: if ε values differ by >0.20, then readings describe different constraints (ε-invariance principle); if ε values cluster, then readings are genuinely perspectival reframings of same constraint. Apply DP-001 test: does measuring the constraint differently produce different ε? If yes, you have constraint family, not single kernel.',
    'If separate constraints: should decompose into three distinct JSON files linked by network.affects_constraints, each with its own ε, perspectives, and measurements. If single kernel: maintain as one story with three sibling readings. Current authoring assumption: SINGLE KERNEL (contingent_choice, overdetermined_collapse, hybrid_trigger are reading frames on transition_causality, not separate constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_classification_ambiguity, conceptual, 'Whether contingent_choice is a reading or a distinct constraint').

omega_variable(
    beneficiary_identification_ambiguity,
    'Is U.S. monetary autonomy a genuine beneficiary (extraction target), or is it an emergent outcome of policy choice with no prior structure to extract from?',
    'Clarify whether the Bretton Woods regime imposed extractive constraints on U.S. autonomy (beneficiary = those freed from constraint) or merely imposed coordination costs (beneficiary = those avoiding coordination cost). Historical analysis: did Triffin dilemma force U.S. to choose between gold stability and monetary autonomy, making autonomy a benefit from constraint-breaking?',
    'If genuine beneficiary: extraction reading is correct; U.S. monetary autonomy is constraint-relative benefit. If outcome of choice: beneficiary framing may be misleading; the choice freed autonomy but didn''t extract it FROM anyone (rather, imposed adjustment cost ON dollar-holders). Current authoring assumes: genuine beneficiary — Bretton Woods constrained U.S. policy autonomy; exit released that constraint and shifted cost to others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, conceptual, 'Whether U.S. autonomy gain is extraction from prior constraint or mere policy outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(contingent_choice_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccr_theater_t0_1968, contingent_choice_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ccr_theater_t1_1971_shock, contingent_choice_reading, theater_ratio, 1, 0.28).
narrative_ontology:measurement(ccr_theater_t3_1974_aftermath, contingent_choice_reading, theater_ratio, 3, 0.38).

% Extraction over time
narrative_ontology:measurement(ccr_extractiveness_t0_1968, contingent_choice_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ccr_extractiveness_t1_1971_shock, contingent_choice_reading, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(ccr_extractiveness_t3_1974_aftermath, contingent_choice_reading, base_extractiveness, 3, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(contingent_choice_reading, resource_allocation).
narrative_ontology:affects_constraint(contingent_choice_reading, triffin_dilemma_structural_inevitability).
narrative_ontology:affects_constraint(contingent_choice_reading, bretton_woods_reserve_adequacy).

% DUAL FORMULATION NOTE:
% The contingent_choice reading is ONE OF THREE competing framings of the Bretton Woods transition (transition_causality kernel). The overdetermined_collapse reading and hybrid_trigger reading are separate constraint stories in the family. All three share the same historical observable (1971 Nixon Shock) but differ in causal attribution and policy autonomy assessment. The network relationships indicate that this constraint is downstream of structural debates about whether the transition was determined or chosen.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
