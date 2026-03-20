% ============================================================================
% CONSTRAINT STORY: hyperstition_snare
% ============================================================================
% Version: 7.0-json (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-03-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hyperstition_snare, []).

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
 *   constraint_id: hyperstition_snare
 *   human_readable: Hyperstition as Terminal Snare (Propagation Consumes the Verification Mechanism)
 *   domain: epistemology/political_economy/media
 *
 * SUMMARY:
 *   Ideas that propagate successfully enough begin producing the conditions
 *   that confirm them. The feedback loop that would catch the error is
 *   absorbed into the self-confirming system. This is the terminal synthesis
 *   of the constraint family: the mountain (selection pressure selects for
 *   accessibility over truth) creates the conditions under which feedback
 *   suppression (the tangled rope) operates, and when feedback suppression is
 *   deep enough, the uncorrected ideas close the hyperstition loop —
 *   propagation generates political power or social reality that
 *   retroactively validates the propagated claim, and the verification
 *   mechanism is consumed. Historical examples span scales: divine right of
 *   kings produced the political theology that confirmed it; market
 *   fundamentalism produced the deregulation that confirmed its predictions
 *   about market efficiency; platform inevitabilism produced the investment
 *   and regulatory deference that confirmed its growth projections. In each
 *   case, the claim's truth-value became a function of its spread rather than
 *   its accuracy. The mechanism is self-reinforcing: successful propagation →
 *   political/social reality → confirming evidence → more propagation.
 *   Counter-claims face structural disadvantage: they must compete against a
 *   system that generates its own confirming data while undermining the
 *   institutions that would evaluate competing claims.
 *
 * KEY AGENTS:
 *   - Epistemic subject (powerless/trapped): individual embedded in a reshaped information environment where counter-evidence is structurally disadvantaged; exit requires abandoning participation in the discourse
 *   - Propagation beneficiary (institutional/arbitrage): political movement or platform whose institutional existence is constituted through the propagated claim; experiences the constraint as pure coordination
 *   - True believer (moderate/identity_locked): actor whose identity has fused with the propagated claim; structurally mobile but cognitively captured; exit requires identity dissolution
 *   - Verification institution (organized/constrained): fact-checking or scientific institution whose corrective output enters the propagation environment and gets selected for accessibility; engagement with the claim feeds the loop
 *   - Structural analyst (analytical/analytical): sees the full propagation-confirmation-propagation loop across historical cases; classifies as snare because the coordination function has been consumed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hyperstition_snare, 0.72).
domain_priors:suppression_score(hyperstition_snare, 0.72).
domain_priors:theater_ratio(hyperstition_snare, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hyperstition_snare, extractiveness, 0.72).
narrative_ontology:constraint_metric(hyperstition_snare, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hyperstition_snare, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hyperstition_snare, snare).
narrative_ontology:human_readable(hyperstition_snare, "Hyperstition as Terminal Snare (Propagation Consumes the Verification Mechanism)").
narrative_ontology:topic_domain(hyperstition_snare, "epistemology/political_economy/media").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hyperstition_snare, propagators_of_successful_ideas).
narrative_ontology:constraint_beneficiary(hyperstition_snare, political_movements_riding_propagation).
narrative_ontology:constraint_victim(hyperstition_snare, verification_institutions).
narrative_ontology:constraint_victim(hyperstition_snare, counter_claim_advocates).
narrative_ontology:constraint_victim(hyperstition_snare, public_epistemics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual embedded in an information environment where a hyperstition loop has closed. The claim has produced the conditions that confirm it — the evidence environment itself has been reshaped. Counter-evidence is not suppressed by force but by structural disadvantage: it must compete against a self-confirming system that generates its own confirming data. Exit requires abandoning the information environment entirely, which means abandoning participation in the discourse. Trapped: the cost of epistemic exit is social isolation.
constraint_indexing:constraint_classification(hyperstition_snare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Political movement, media platform, or ideological network whose power is constituted through the propagated claim. From this position the constraint is pure coordination: the idea organizes collective action, allocates social status, and sustains institutional coherence. The extraction — the consumption of verification capacity — is invisible because verification would threaten the institutional basis. The beneficiary does not experience the snare because the snare is the source of their institutional existence.
constraint_indexing:constraint_classification(hyperstition_snare, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Actor whose identity has fused with the propagated claim. Structurally mobile — could access counter-evidence, could engage alternative frameworks — but identity-locked because the claim is constitutive of their self-concept, community membership, and social world. The hyperstition loop is invisible from inside because the self-confirming evidence IS the evidence environment. Exit requires not just changing a belief but dissolving the identity built around it. Classifies snare because the extraction (epistemic capture) is total and the coordination function (community belonging) is parasitic on the capture.
constraint_indexing:constraint_classification(hyperstition_snare, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% Fact-checking organization, scientific institution, or independent press attempting to maintain verification capacity against the hyperstition loop. Constrained exit: the institution's mandate requires engaging the propagated claim, but engagement feeds the loop (attention is propagation fuel). Sees tangled rope: there IS a coordination function (the propagated claim does organize social behavior, which has value), but the extraction (consumption of verification capacity) is asymmetric and accelerating. The institution's own corrective output enters the propagation environment and gets selected for accessibility rather than accuracy.
constraint_indexing:constraint_classification(hyperstition_snare, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Observer who sees the full loop: propagation → political/social reality → retroactive confirmation → more propagation. At civilizational time horizon, the pattern is recognizable across historical cases (divine right, manifest destiny, market fundamentalism, platform inevitabilism). The analytical classification is snare, not tangled rope, because at scale the coordination function is consumed by the extraction: whatever genuine social coordination the propagated idea initially provided has been replaced by self-confirming circularity. The coordination claim survives as theater.
constraint_indexing:constraint_classification(hyperstition_snare, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hyperstition_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hyperstition_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hyperstition_snare, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hyperstition_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hyperstition_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.72 reflects that the hyperstition loop, once closed, extracts from verification capacity almost totally. The claim does not merely coexist with verification — it consumes it, redirecting the institutions and cognitive resources that would evaluate the claim into either propagation or futile opposition that feeds the loop. This is above the 0.70 mandatrophy threshold, requiring mandatrophy_resolved: true. Suppression at 0.72: counter-claims face not overt prohibition but structural impossibility — the evidence environment has been reshaped to confirm the propagated claim, so counter-evidence appears fringe, outdated, or hostile to the community's self-understanding. Theater ratio at 0.45: the hyperstition loop produces genuine social reality (political power, institutional structures, economic arrangements), so it is not purely theatrical. But roughly half of its confirmatory evidence is circular — produced by the propagation itself rather than by independent verification. The measurement trajectory shows monotonic accumulation across both metrics, reflecting that hyperstition loops deepen over time as the confirming infrastructure builds. The initial state (ε=0.25, theater=0.10) represents an idea with genuine empirical content that has not yet closed the self-confirming loop; the terminal state represents full epistemic capture.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the Propagation Beneficiary (rope) and everyone else (snare or tangled_rope). The beneficiary sees pure coordination because their institutional existence depends on the claim — verification would be self-dissolution. The True Believer sees snare but cannot act on the classification because their identity is constituted through the claim. The Verification Institution sees tangled_rope — acknowledging that the propagated idea does coordinate social behavior (real function) while consuming verification capacity (extraction) — but this is the most generous defensible reading. The Structural Analyst resolves the gap as snare: at civilizational scale, the coordination function has been replaced by self-confirming circularity, and whatever genuine social coordination persists is parasitic on the identity lock rather than on the idea's epistemic content. The perspectival structure itself enacts the essay's thesis: the closer you are to the propagation's social benefits, the less extraction you perceive.
 *
 * DIRECTIONALITY LOGIC:
 *   Propagators of successful ideas and political movements riding propagation are primary beneficiaries with arbitrage exit — they can deploy the hyperstition mechanism across domains and withdraw institutional capital if one instance fails. Derived d ≈ 0.05, f(d) ≈ -0.12. Verification institutions are victims with constrained exit — their mandate requires engagement, but engagement feeds the loop. Derived d ≈ 0.75, f(d) ≈ 1.08. Counter-claim advocates are victims with constrained-to-trapped exit depending on how completely the information environment has been reshaped. Public epistemics is a diffuse victim — the general capacity for truth-tracking in the information environment. The True Believer perspective uses identity_locked exit: structurally mobile (moderate power, could access counter-evidence) but identity-fused with the propagated claim. Derived d ≈ 0.89, f(d) ≈ 1.28 — higher effective extraction than the constrained verification institution because the binding mechanism is internal.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved: the snare classification is correct and the coordination-function claim is addressed. The propagated idea does organize social behavior — communities cohere around shared beliefs, collective action becomes possible, institutional stability is maintained. This is real coordination. But the mandatrophy resolution is that at the terminal stage of the hyperstition loop, this coordination is parasitic on the identity lock rather than on the idea's epistemic content. The community would cohere around ANY identity-constituting claim; the specific content is interchangeable. Evidence: communities that replace one hyperstition with another (political realignment, religious conversion) maintain their coordination capacity while swapping the content entirely. This means the coordination function attributed to the specific idea is theatrical — the real coordinator is the identity lock, and the idea is the vessel. Snare classification is therefore correct: the claimed coordination function (social organization through shared truth) has been replaced by actual extraction (epistemic capture through self-confirming circularity), with a theatrical coordination residual (community cohesion attributed to the idea but actually produced by identity fusion). The tangled_rope reading (from the Verification Institution's perspective) is the most generous defensible alternative, and it is captured by omega_coordination_residual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_hyperstition_reversibility,
    'At what point does successful propagation become irreversible? Is there a critical threshold after which the self-confirming loop cannot be broken from within, or can exogenous shocks (economic crisis, technological disruption, generational turnover) reliably break the cycle?',
    'Historical case analysis: identify cases where established hyperstition loops were broken (divine right of kings, phlogiston theory, pre-germ-theory miasma). Measure whether the break was endogenous (internal verification reasserting) or exogenous (external shock making the claim''s predictions fail catastrophically). If always exogenous, the snare classification is confirmed — internal verification cannot break the loop once closed.',
    'If reversible endogenously: reclassify as tangled_rope with high extraction — the verification mechanism is suppressed but not consumed. If irreversible without exogenous shock: snare classification confirmed, and the essay''s implicit corrective hope (legibility via AI enables correction) requires an exogenous mechanism it has not specified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_hyperstition_reversibility, empirical, 'Reversibility threshold for self-confirming propagation loops').

omega_variable(
    omega_legibility_conversion,
    'Does making the failure mode visible (via AI legibility, structural analysis, or media criticism) produce correction, or does the legibility itself enter the propagation environment and get selected for accessibility rather than accuracy?',
    'Track the propagation trajectory of structural critiques of hyperstition: do they produce institutional reform, or do they become content that circulates for engagement value without producing corrective action? If the critique of the loop becomes another node in the loop, the essay is subject to the mechanism it describes.',
    'If legibility converts to correction: the deferred legibility_scaffold axis should be promoted from omega to constraint story. If legibility is captured by propagation: the essay''s corrective thesis fails, and the snare is deeper than argued — even the analytical position is consumed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_legibility_conversion, empirical, 'Whether visibility of the failure mode converts to correction or becomes propagation content').

omega_variable(
    omega_coordination_residual,
    'Does the propagated idea retain any genuine coordination function after the hyperstition loop closes, or is the remaining coordination purely parasitic on the epistemic capture?',
    'Functional decomposition: for established hyperstition cases, measure whether the social coordination attributed to the propagated idea (community cohesion, collective action capacity, institutional stability) persists when the epistemic content is removed or replaced. If coordination persists with different content, the coordination function was independent of the specific claim — the idea was a vessel, not a coordinator.',
    'If genuine residual coordination: reclassify as tangled_rope. The mandatrophy analysis must then account for a real coordination function that would be lost if the snare were dismantled. If parasitic: snare classification confirmed. The coordination claim is theater (the community coheres because of the identity lock, not because of the idea''s coordination value).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_coordination_residual, conceptual, 'Residual coordination function after hyperstition loop closure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hyperstition_snare, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hype_tr_t0, hyperstition_snare, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hype_tr_t20, hyperstition_snare, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hype_tr_t40, hyperstition_snare, theater_ratio, 40, 0.28).
narrative_ontology:measurement(hype_tr_t60, hyperstition_snare, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hype_tr_t80, hyperstition_snare, theater_ratio, 80, 0.4).
narrative_ontology:measurement(hype_tr_t100, hyperstition_snare, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(hype_be_t0, hyperstition_snare, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hype_be_t20, hyperstition_snare, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(hype_be_t40, hyperstition_snare, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(hype_be_t60, hyperstition_snare, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(hype_be_t80, hyperstition_snare, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(hype_be_t100, hyperstition_snare, base_extractiveness, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hyperstition_snare, identity_coordination).

% DUAL FORMULATION NOTE:
% Terminal node of the three-axis constraint family. Downstream of both selection_pressure_architecture (mountain: optimization selects for accessibility) and feedback_suppression_tangled_rope (success ecology dismantles error-correction). The mountain creates the selection environment; the tangled rope suppresses the corrective signals; the snare is what happens when suppression is deep enough that propagation closes the self-confirming loop. No downstream constraints declared — this is the terminal state. The deferred legibility_scaffold axis represents the potential exit from this terminal, but its mechanism is unspecified (omega_legibility_conversion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hyperstition_snare, moderate, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
