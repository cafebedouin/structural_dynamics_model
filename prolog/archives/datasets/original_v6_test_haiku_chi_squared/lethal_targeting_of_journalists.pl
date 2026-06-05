% ============================================================================
% CONSTRAINT STORY: lethal_targeting_of_journalists
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lethal_targeting_of_journalists, []).

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
 *   constraint_id: lethal_targeting_of_journalists
 *   human_readable: De Facto Policy of Lethal Targeting of Journalists in Conflict Zones
 *   domain: political/military/human_rights
 *
 * SUMMARY:
 *   The lethal targeting of journalists in conflict zones represents a de
 *   facto extraction mechanism operating through direct violence rather than
 *   institutional coercion. Based on documented cases of disproportionate
 *   journalist casualties in specific theaters of conflict, this constraint
 *   models how state military actors suppress independent observation of
 *   battlefield decisions and civilian impacts. The constraint is
 *   characterized by high extractiveness (0.78) — eliminating journalists
 *   removes real-time accountability mechanisms — and extreme suppression
 *   (0.92) — journalists face lethal threat with no formal protection, legal
 *   recourse, or negotiated exit. The low theater ratio (0.35) reflects that
 *   this extraction mechanism is not disguised as something else; state
 *   actors either deny targeting explicitly or justify it as military
 *   necessity, making the suppression structurally naked rather than
 *   performative. The constraint escalated in extractiveness over the
 *   interval (0.42 → 0.78) as conflicts intensified and targeting became more
 *   systematic. Theater ratio remained low and stable, indicating that the
 *   extraction mechanism relies on direct threat rather than on mis-framing
 *   or institutional performativity.
 *
 * KEY AGENTS:
 *   - Targeted Journalists: Powerless/trapped primary victims (immediate horizon) — face direct lethal threat with no legal protection or exit option
 *   - Independent Media Outlets: Moderate/trapped secondary victims (biographical horizon) — cannot cease operations without abandoning mission; face systematic targeting of staff and assets
 *   - Civilian Information Ecosystem: Powerless/trapped abstract victim (generational horizon) — population dependent on independent reporting for accountability and information asymmetry reversal; no exit option
 *   - State Military Command: Institutional/arbitrage primary beneficiary (immediate horizon) — benefits from suppressed real-time reporting; experiences constraint as coordination solution for operational security
 *   - Information Control Apparatus: Institutional beneficiary — state actors managing narrative control and limiting accountability mechanisms
 *   - International Governance Framework: Institutional/constrained secondary actor (civilizational horizon) — formal commitments to journalist protection lack enforcement; constraint persists through institutional inertia despite low functional governance
 *   - Analytical Observer: Civilizational analyst (global scope) — sees pure extraction mechanism with no coordination function; constraint is pure information suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lethal_targeting_of_journalists, 0.78).
domain_priors:suppression_score(lethal_targeting_of_journalists, 0.92).
domain_priors:theater_ratio(lethal_targeting_of_journalists, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lethal_targeting_of_journalists, extractiveness, 0.78).
narrative_ontology:constraint_metric(lethal_targeting_of_journalists, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(lethal_targeting_of_journalists, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lethal_targeting_of_journalists, snare).
narrative_ontology:human_readable(lethal_targeting_of_journalists, "De Facto Policy of Lethal Targeting of Journalists in Conflict Zones").
narrative_ontology:topic_domain(lethal_targeting_of_journalists, "political/military/human_rights").

domain_priors:requires_active_enforcement(lethal_targeting_of_journalists).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lethal_targeting_of_journalists, state_military_command).
narrative_ontology:constraint_beneficiary(lethal_targeting_of_journalists, information_control_apparatus).
narrative_ontology:constraint_victim(lethal_targeting_of_journalists, journalists).
narrative_ontology:constraint_victim(lethal_targeting_of_journalists, independent_media_ecosystem).
narrative_ontology:constraint_victim(lethal_targeting_of_journalists, civilian_information_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED JOURNALIST (SNARE) — Operating in an active conflict zone with no exit option short of fleeing the region permanently. Faces direct lethal threat with minimal formal protection mechanisms or accountability. Cannot negotiate exit from constraint. d≈0.98, f(d)≈1.45, σ=0.9 → χ≈1.02.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDEPENDENT MEDIA OUTLET (SNARE) — Trapped within operational theater. Cannot cease reporting without abandoning mission; cannot relocate staffing without losing source networks. Faces systematic targeting. d≈0.92, f(d)≈1.30, σ=1.0 → χ≈1.02.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVILIAN INFORMATION ECOSYSTEM (SNARE) — Abstract collective of citizens dependent on independent reporting. No exit option. Trapped in information scarcity. Cannot organize collective defense. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.05.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE MILITARY COMMAND (ROPE) — Experiences targeting of journalists as a coordination problem solution: suppressing real-time battlefield reporting improves operational security and reduces political friction. Views constraint as necessary military information control. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.07.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL GOVERNANCE FRAMEWORK (PITON) — Geneva Conventions, UN protocols, press freedom resolutions exist as formal commitments but lack enforcement mechanisms in active conflicts. Journalistic protection is performative — symbolic condemnation without sanctions sufficient to deter targeting. theater_ratio=0.35 but constraint persists through institutional inertia despite low functional governance. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.32.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a global/civilizational perspective, lethal targeting of journalists is pure extraction: eliminating independent observers from military decision-making zones to prevent accountability and information asymmetry reversal. No coordination benefit. Pure suppression of alternative information pathways. ε=0.78, suppression=0.92 confirm snare across all observation dimensions. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.02.
constraint_indexing:constraint_classification(lethal_targeting_of_journalists, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lethal_targeting_of_journalists_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lethal_targeting_of_journalists, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lethal_targeting_of_journalists, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lethal_targeting_of_journalists, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lethal_targeting_of_journalists, TR),
    TR >= 0.70.

:- end_tests(lethal_targeting_of_journalists_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and increasing. The constraint extracts significant value by eliminating independent reporting, preventing accountability mechanisms, and enabling undocumented military decision-making. The escalation from 0.42 to 0.78 over the interval reflects systematic intensification of targeting as conflicts deepened. Unlike 'propaganda' (which involves positive mis-framing), this constraint uses direct lethal force to prevent information flow entirely. Suppression (0.92): Extreme. Journalists face lethal threat with: (1) no formal legal protection mechanisms, (2) no international enforcement of Geneva Convention protections, (3) no meaningful military distinction or protective marking, (4) no negotiated safe passage or non-combatant status recognition, (5) no accessible grievance mechanisms. Exit options are binary: flee the theater entirely or accept lethal risk. Theater ratio (0.35): Low and stable. The constraint does not disguise itself as something beneficial. State actors either deny targeting (explicit denial is low-theater) or justify it as military necessity (framing as required operational security is mid-theater). The stability across the interval indicates the extraction mechanism does not require increasing performativity — naked threat is sufficient when victims cannot escape.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives except the military beneficiary classify as Snare. The targeted journalist, media outlet, civilian information ecosystem, and analytical observer all see pure extraction: elimination of independent observation with no coordination benefit. The international governance framework sees a degraded piton (formal protections that don't function). Only the state military command sees rope (coordination benefit: operational security). This represents a stark perspectival gap where the beneficiary sees coordination and the victims see pure extraction. The gap exists because the 'coordination' from the military perspective (suppressing real-time reporting) is inherently extraction from the civilian perspective (eliminating information access). The two perspectives are not measuring the same constraint from different angles — they are structurally incompatible. Military operational security gained = civilian information loss. There is no mutual benefit frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted journalists: Victim + trapped → d≈0.98, f(d)≈1.45. Near-maximum extraction. No alternative but to accept lethal risk or flee. State military command: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary position. Can exit constraint at will (cease targeting) without material cost. Independent media outlets: Victim + trapped → d≈0.92, f(d)≈1.30. High extraction. Cannot exit without abandoning mission. Civilian information ecosystem: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Abstract collective with no agency. International governance: Constrained institutional actor → d≈0.55, f(d)≈0.75. Moderate directionality. Has formal commitments (constraints) but lacks enforcement power. The directionality derivation is unambiguous: all victims show high d (trapped exit, no alternatives); beneficiary shows low d (arbitrage exit, can reverse targeting at will).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED WITH MANDATROPHY RESOLVED. Extractiveness > 0.70 requires mandatrophy resolution (ε=0.78). The classification satisfies all snare gates: (1) ε ≥ 0.46 (✓ 0.78), (2) suppression ≥ 0.60 (✓ 0.92), (3) χ ≥ 0.66 (computed χ ≈ 1.02 from all victim perspectives, ✓). The constraint is pure extraction, not mislabeled coordination. No coordination function exists that would justify the military targeting. Claimed coordination value ('operational security') is not mutual gain — it is information suppression that harms civilians. The mandatrophy is resolved by demonstrating: (a) the beneficiary (military command) experiences rope only because they define 'coordination' as unilateral information elimination, which is extraction by definition; (b) from every victim perspective, the constraint is pure snare; (c) the international governance framework cannot reframe this as coordination because the 'suppressed reporting' would provide accountability, not mutual optimization; (d) the analytical observer sees no legitimate coordination role — the constraint is structural violence. Therefore, Snare is the correct classification, and no reframing into Tangled Rope is justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_threshold,
    'Is disproportionate journalist casualty rate evidence of de facto targeting policy, or collateral consequence of conflict geography and access patterns?',
    'Forensic analysis of strike patterns relative to civilian density; comparison of journalist casualty rates to baseline military casualty rates in same zones; examination of pre-strike intelligence collection on media locations',
    'If intentional policy: snare classification confirmed across all perspectives. If collateral: constraint reclassifies to tangled_rope (defensive military necessity + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_threshold, empirical, 'Whether targeting is deliberate policy or collateral outcome').

omega_variable(
    actor_attribution_clarity,
    'Which institutional actor within the state apparatus owns the targeting policy? Does clarity of command responsibility affect classification?',
    'Documentary evidence (orders, targeting guidelines); forensic accountability chains; interviews with military/security personnel; comparison to documented protocols',
    'If centralized policy: institutional beneficiary is unified (military command). If distributed/tacit: beneficiary becomes diffuse (information control apparatus generally), potentially increasing theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actor_attribution_clarity, empirical, 'Attribution of targeting authority within state apparatus').

omega_variable(
    counterfactual_information_effect,
    'What information would journalists report if they could operate safely? Would it change military decision-making or only political accountability?',
    'Comparison of operational outcomes in theaters with vs without independent reporting; analysis of civilian casualty patterns; correlation between information access and political response magnitude',
    'If information would change military decisions: constraint prevents optimization (extraction is significant). If only political response: extraction is primarily audience information control, not military necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_information_effect, empirical, 'Whether suppressed information would alter military or only political outcomes').

omega_variable(
    state_definition_boundary,
    'Does the de facto targeting policy reflect state-level institutional decision, rogue unit autonomy, or private security actor behavior? Where does institutional responsibility lie?',
    'Accountability investigations; comparison of patterns across multiple conflicts; organizational accountability chain analysis; state denial/acknowledgment patterns',
    'Clear institutional responsibility: snare classification stable. Ambiguity: constraint potentially bifurcates into separate stories (state_policy_targeting vs private_security_targeting) with different ε values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_definition_boundary, conceptual, 'Whether targeting reflects institutional state policy or non-state actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lethal_targeting_of_journalists, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ltoj_tr_t0, lethal_targeting_of_journalists, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ltoj_tr_t3, lethal_targeting_of_journalists, theater_ratio, 3, 0.31).
narrative_ontology:measurement(ltoj_tr_t6, lethal_targeting_of_journalists, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(ltoj_be_t0, lethal_targeting_of_journalists, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ltoj_be_t3, lethal_targeting_of_journalists, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(ltoj_be_t6, lethal_targeting_of_journalists, base_extractiveness, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lethal_targeting_of_journalists, enforcement_mechanism).
narrative_ontology:affects_constraint(lethal_targeting_of_journalists, state_information_monopoly).
narrative_ontology:affects_constraint(lethal_targeting_of_journalists, civilian_casualty_accountability).
narrative_ontology:affects_constraint(lethal_targeting_of_journalists, press_freedom_suppression).

% DUAL FORMULATION NOTE:
% Lethal targeting of journalists is part of a constraint family around state information control. The upstream constraint is state_information_monopoly (the institutional drive to control narrative); lethal targeting is the enforcement mechanism that makes monopoly effective. The downstream constraint is press_freedom_suppression (the broader structural suppression of independent media). These three constraints have different ε values reflecting different aspects of the extraction: monopoly (ε≈0.55, more institutional), targeting (ε≈0.78, more violent), suppression (ε≈0.68, more structural). All three are linked through the beneficiary (state information control apparatus).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lethal_targeting_of_journalists, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
