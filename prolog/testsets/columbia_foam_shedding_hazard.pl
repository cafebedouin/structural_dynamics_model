% ============================================================================
% CONSTRAINT STORY: columbia_foam_shedding_hazard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_columbia_foam_shedding_hazard, []).

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
 *   constraint_id: columbia_foam_shedding_hazard
 *   human_readable: Columbia Foam Shedding Hazard: Institutional Suppression of Known Launch Risk
 *   domain: aerospace_safety/institutional_failure
 *
 * SUMMARY:
 *   The Columbia Space Shuttle foam shedding hazard represents a structural
 *   constraint where institutional actors (NASA management) imposed
 *   existential risk on powerless agents (crew) while suppressing awareness
 *   of the hazard through information compartmentalization and authority
 *   enforcement. The constraint operated across multiple launch cycles; foam
 *   shedding was documented in previous flights (STS-112 in 2002), observed
 *   in foam impacts during ascent, and addressed through risk acceptance
 *   procedures rather than hazard mitigation. The decision to launch despite
 *   known foam shedding represents pure extraction: crew members bore the
 *   catastrophic risk (65% probability of undetected tile damage leading to
 *   structural failure) while management captured the mission success
 *   benefit. The constraint's suppression mechanism operated through
 *   institutional hierarchy (crew cannot unilaterally refuse launch),
 *   information compartmentalization (hazard data not clearly communicated to
 *   decision-makers in a unified risk narrative), and the theater of formal
 *   Flight Readiness Reviews that documented 'acceptable risk' without
 *   functional hazard mitigation. This is a paradigmatic Snare: high
 *   extraction (78%), high suppression (82%), asymmetric power, and no crew
 *   exit option. The theater ratio (65%) reflects that certification
 *   procedures and risk acceptance documents performed legitimacy without
 *   corresponding hazard control.
 *
 * KEY AGENTS:
 *   - Columbia Crew: Primary victims (powerless/trapped) — bore existential risk with zero exit option; had no authority to unilaterally cancel mission despite hazard awareness
 *   - Structural Engineers and Safety Personnel: Secondary victims (moderate/constrained) — possessed hazard data and safety concerns but faced career risk for escalation; internal dissent was suppressed through organizational hierarchy
 *   - NASA Flight Readiness Review Authority: Primary beneficiary (institutional/arbitrage) — captured mission success benefits; possessed decision authority to cancel or delay; used formal procedures to rationalize risk acceptance
 *   - Thermal Protection System Program Management: Primary beneficiary (institutional/arbitrage) — benefits from launch completion without full hazard mitigation investment; possessed knowledge of previous foam shedding incidents
 *   - Institutional Safety Certification Apparatus: Performer of theater (institutional/arbitrage) — maintains legitimacy through procedures (Flight Readiness Reviews, hazard assessments) without functional oversight; creates the appearance of rigorous hazard control
 *   - Analytical Observer: Structural gap revealer — identifies the extraction mechanism that institutional procedures conceal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(columbia_foam_shedding_hazard, 0.78).
domain_priors:suppression_score(columbia_foam_shedding_hazard, 0.82).
domain_priors:theater_ratio(columbia_foam_shedding_hazard, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(columbia_foam_shedding_hazard, extractiveness, 0.78).
narrative_ontology:constraint_metric(columbia_foam_shedding_hazard, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(columbia_foam_shedding_hazard, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(columbia_foam_shedding_hazard, snare).
narrative_ontology:human_readable(columbia_foam_shedding_hazard, "Columbia Foam Shedding Hazard: Institutional Suppression of Known Launch Risk").
narrative_ontology:topic_domain(columbia_foam_shedding_hazard, "aerospace_safety/institutional_failure").

domain_priors:requires_active_enforcement(columbia_foam_shedding_hazard).
% --- Structural relationships ---
narrative_ontology:constraint_victim(columbia_foam_shedding_hazard, crew_members).
narrative_ontology:constraint_victim(columbia_foam_shedding_hazard, mission_safety).
narrative_ontology:constraint_victim(columbia_foam_shedding_hazard, institutional_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASTRONAUT CREW (SNARE) — Trapped by institutional hierarchy and mission assignment protocols. Cannot refuse launch despite documented foam shedding hazard without career-ending consequences. Bears maximum existential risk with zero exit option. Suppression enforced through chain-of-command authority and the impossibility of unilateral mission abort without organizational permission.
constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD ENGINEERS AND SAFETY ADVOCATES (TANGLED ROPE) — Face career risk for dissent but possess some institutional voice through engineering channels. Benefit from the mission's existence (employment, research opportunities) while bearing costs of suppressed safety concerns. Constrained exit: whistleblowing carries career damage; internal escalation is partially effective but inefficient and career-costly.
constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NASA MANAGEMENT (ROPE) — Experiences the constraint as coordination: maintaining launch schedules requires managing safety assessments, cost pressures, and institutional reputations. Arbitrage available through regulatory capture, cost-benefit framing, and risk acceptance procedures. Net beneficiary from the constraint's enforcement — extraction runs toward this agent.
constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REVIEW APPARATUS (PITON) — Flight Readiness Review, Safety Assessment documents, and sign-off procedures perform certification without genuine verification. The process maintains legitimacy through ritual (checklists, formal meetings, documented concurrences) rather than functional oversight. Theater ratio elevated by the gap between procedural rigor and actual risk management. Institutional inertia: the certification apparatus persists because alternatives would require organizational restructuring.
constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NATURALIZED ORGANIZATIONAL CONSTRAINT (FALSE SUMMIT) — From a distance, the foam shedding hazard might appear as an immutable law of engineering ('thermal protection systems inherently shed material') or organizational necessity ('spaceflight requires accepting calculated risks'). This perspective naturalizes what is structurally a contingent institutional choice: the decision to launch despite known hazard, enforced through hierarchy and information suppression. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (STRUCTURAL REALITY) — From the structural data, the foam hazard is pure extraction: known risk imposed on powerless agents by institutional actors with exit options. High suppression (82%) reflects the enforcement of silence through hierarchy, information compartmentalization, and career risk. High extractiveness (78%) reflects the asymmetry: crew bears existential risk, management captures mission success benefits. The constraint persists not because the hazard is inevitable but because the institutional enforcement of launch-readiness overrides crew safety.
constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(columbia_foam_shedding_hazard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(columbia_foam_shedding_hazard, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(columbia_foam_shedding_hazard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(columbia_foam_shedding_hazard, TR),
    TR >= 0.70.

:- end_tests(columbia_foam_shedding_hazard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. Asymmetric distribution of costs and benefits: crew bears 65% catastrophic risk; management bears only reputational/career risk of mission failure or accident. The extraction is not negotiated — it is imposed through authority and information control. The measurement trajectory (0.55 → 0.68 → 0.75) reflects accumulation: as launch date approached, hazard signals intensified (impact damage observed on cameras), but the risk acceptance framework was reinforced rather than abandoned, indicating active enforcement of the extraction mechanism. Suppression (0.82): Very high. Multiple suppression mechanisms: (1) Authority-based — crew cannot refuse launch without organizational permission; (2) Information-based — hazard data compartmentalized; crew not briefed on previous foam shedding incidents or their severity; (3) Organizational norms — expressing safety dissent carries career costs; escalation through safety channels is slow and unreliable. Theater ratio (0.65): Moderate-high. Flight Readiness Reviews, formal risk assessments, and sign-off procedures create institutional appearance of rigorous hazard evaluation. However, the procedures do not include functional hazard mitigation (no design change to reduce foam shedding, no contingency plan for tile damage, no real-time inspection capability post-launch). The theater masks the underlying constraint: launch readiness was declared acceptable despite persistent hazard.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. NASA Management (rope perspective) sees the constraint as coordination: managing mission scheduling, cost efficiency, and risk acceptance procedures is a complex coordination problem requiring institutional judgment. Flight crews and safety personnel (tangled_rope and snare perspectives) see the same constraint as pure extraction: hazard-bearing responsibilities are non-negotiable, and disagreement carries career consequences. The institutional review apparatus (piton perspective) performs certification without genuine verification — the rituals maintain legitimacy despite functional hazard gaps. The analytical observer identifies a false summit in the mountain perspective: naturalizing the foam hazard as 'inherent to thermal protection' masks the institutional choice to launch with known defect. The perspectival gap is maximal here: one agent's coordination problem is another agent's existential hazard.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for crew: 0.95 (trapped, victim of asymmetric extraction). D for management: 0.10 (institutional, beneficiary with arbitrage). The sigmoid f(d) produces: f(0.95) ≈ 1.42 for crew (maximum experienced extractiveness), f(0.10) ≈ -0.05 for management (negative experienced extraction — they benefit). Scope modifier σ(global) = 1.2 applies to institutional scope of space program. Final chi: crew experience ~1.35 (normalized), management experience ~0.0 (negative). This is the maximum perspectival divergence in the chi calculation — one agent experiences the constraint as catastrophic; the other experiences it as manageable coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint resolves through the recognition that the foam hazard was NOT a genuine coordination problem (which would classify as Rope or Tangled Rope) but pure extraction enforced through institutional authority. The mandatrophy risk — mislabeling extraction as coordination — is defeated by the structural evidence: (1) No genuine coordination benefit to crew. The hazard imposes risk; crew gain nothing except the privilege of remaining employed. (2) Suppression mechanism explicitly prevents crew from exercising consent. True coordination requires voluntary participation; forced participation is extraction. (3) Information asymmetry. Management possessed hazard data unavailable to crew; they used this asymmetry to enforce acceptance. (4) Alternative exists. The mission could have been delayed or cancelled without technical impossibility — only institutional cost prevented this. The snare classification is confirmed: extraction enforced through suppression, not voluntary participation. The extraction persisted because institutional authority made the alternative (mission cancellation) organizationally costly, not because the hazard was genuinely unavoidable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    acceptable_risk_framing,
    'Was the foam shedding risk genuinely ''acceptable'' by engineering standards, or was it reclassified as acceptable to preserve schedule?',
    'Historical analysis of risk assessments pre-launch vs post-accident; comparison with hazard severity thresholds applied to other shuttle flights; deposition analysis of decision-making rationale',
    'If genuinely acceptable: constraint reclassifies toward Rope (risk coordination). If reclassified for schedule: constraint remains Snare (institutional extraction of risk onto crew).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(acceptable_risk_framing, empirical, 'Whether foam risk acceptance was genuine engineering judgment or schedule-driven rationalization').

omega_variable(
    information_compartmentalization,
    'Was crew awareness of foam hazard suppression deliberate institutional policy or emergent organizational silence?',
    'Analysis of communication logs, meeting transcripts, and pre-flight briefing documentation; interviews with mission planners and flight directors regarding explicit suppression orders vs implicit information barriers',
    'If deliberate: suppression mechanism is enforcement (high institutional intentionality). If emergent: suppression is systemic (high institutional inertia). Both yield the same classification but different remediation strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_compartmentalization, empirical, 'Whether information suppression was deliberate policy or systemic organizational failure').

omega_variable(
    counterfactual_mission_cancellation,
    'What institutional costs prevented mission cancellation as a response to the foam hazard?',
    'Cost-benefit analysis reconstruction: funding commitments, political timeline pressures, reputational factors, workforce continuity; comparison with other shuttle missions that were delayed or cancelled for hazard concerns',
    'If costs are primarily financial/political: the constraint is structurally choice-enabled (could have been rejected). If costs are systemic/irreversible: constraint may reflect genuine organizational lock-in. Either way, it remains high-extraction from crew perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_mission_cancellation, empirical, 'Institutional costs that locked in the launch decision despite foam hazard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(columbia_foam_shedding_hazard, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfsh_tr_t0, columbia_foam_shedding_hazard, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cfsh_tr_t3, columbia_foam_shedding_hazard, theater_ratio, 3, 0.58).
narrative_ontology:measurement(cfsh_tr_t6, columbia_foam_shedding_hazard, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(cfsh_be_t0, columbia_foam_shedding_hazard, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cfsh_be_t3, columbia_foam_shedding_hazard, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(cfsh_be_t6, columbia_foam_shedding_hazard, base_extractiveness, 6, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(columbia_foam_shedding_hazard, enforcement_mechanism).
narrative_ontology:affects_constraint(columbia_foam_shedding_hazard, institutional_safety_theater).
narrative_ontology:affects_constraint(columbia_foam_shedding_hazard, aerospace_schedule_pressure).

% DUAL FORMULATION NOTE:
% The foam shedding hazard itself is a physical/engineering constraint (material degradation during ascent); this story concerns the institutional constraint of suppressing awareness and enforcement of that hazard. The two constraints have different epsilon values: the physical hazard is a technical property (ε ≈ 0.30, rope/tangled_rope depending on mitigation); the suppression constraint is institutional extraction (ε ≈ 0.78, snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
