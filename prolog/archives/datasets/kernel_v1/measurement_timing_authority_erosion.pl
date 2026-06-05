% ============================================================================
% CONSTRAINT STORY: measurement_timing_authority_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_measurement_timing_authority_erosion, []).

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
 *   constraint_id: measurement_timing_authority_erosion
 *   human_readable: Measurement Timing Authority Erosion in Organizational Control Systems
 *   domain: organizational_psychology/systems_theory/epistemology_of_control
 *
 * SUMMARY:
 *   The measurement timing authority erosion constraint arises in
 *   organizations where governance requires measurement-based authorization
 *   but operational states change faster than authorization cycles can
 *   complete. The structural tension is acute: measurement-based authority is
 *   intended to prevent arbitrary action and distribute accountability by
 *   grounding decisions in objective data. However, when measurements become
 *   stale before authorization concludes, the authority mechanism
 *   simultaneously erodes (the decision is based on outdated information) and
 *   strengthens (the stale measurement provides formal cover). This
 *   constraint produces the full range of DR classifications across
 *   perspectives because different organizational actors experience the same
 *   timing gap as bearing fundamentally different structural meanings: it is
 *   an immutable feature of complex systems (mountain/analytical), a
 *   temporary coordination failure with technological solutions (scaffold), a
 *   degraded ritual maintained by inertia (piton), a genuine coordination
 *   mechanism with asymmetric extraction (tangled rope), a trap for real-time
 *   operators (snare), and a benefit for decision authorities (rope). The
 *   constraint's theater ratio (0.68) reflects that classical authorization
 *   procedures increasingly perform legitimacy rather than function:
 *   quarterly reviews of data that has changed monthly, annual audits
 *   assessing conditions that shift weekly, committee approvals based on
 *   dashboards updated since the meeting agenda was printed. Real-time
 *   domains (hospitals, emergency response, manufacturing control) experience
 *   the gap most acutely; slow-changing domains (strategic planning, policy
 *   development) experience it as negligible. The measurement-timing
 *   authority erosion is thus not a uniform constraint but a presheaf
 *   structure — different organizational contexts instantiate it with
 *   different extractiveness, suppression, and theater ratios, all present
 *   simultaneously in the same formal system.
 *
 * KEY AGENTS:
 *   - Decision Authorities (executives, compliance officers, committee chairs): institutional/arbitrage — primary beneficiaries capturing deniability and coordination function; experience constraint as enabling (Rope/Tangled Rope)
 *   - Real-Time Stakeholders (hospital staff, safety personnel, operational managers): moderate/constrained — experience constraint as a trap; must execute actions authorized on stale measurements with full liability (Snare)
 *   - Accountability Framework (governance structures, audit functions, regulatory requirements): powerless/trapped — abstract institutional structure bearing responsibility for authorization decisions made on stale data (Snare)
 *   - Real-Time Systems (feedback control systems, monitoring infrastructure, decision-support platforms): organized/constrained — provide coordination function but suppressed by slow authorization cycles (Tangled Rope)
 *   - Adaptive Governance Reformers (CIOs, organizational development consultants, real-time compliance practitioners): powerful/mobile — see timing mismatch as temporary problem solvable through technology and process redesign (Scaffold)
 *   - Classical Authorization Rituals (established review procedures, committee structures, audit cycles): institutional/arbitrage — persist through institutional inertia; maintain formal legitimacy despite functional obsolescence (Piton)
 *   - Analytical Observer: analytical/analytical — risks naturalizing contingent institutional timing constraints as inherent features of authorization itself (False Summit Mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(measurement_timing_authority_erosion, 0.52).
domain_priors:suppression_score(measurement_timing_authority_erosion, 0.58).
domain_priors:theater_ratio(measurement_timing_authority_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(measurement_timing_authority_erosion, extractiveness, 0.52).
narrative_ontology:constraint_metric(measurement_timing_authority_erosion, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(measurement_timing_authority_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(measurement_timing_authority_erosion, tangled_rope).
narrative_ontology:human_readable(measurement_timing_authority_erosion, "Measurement Timing Authority Erosion in Organizational Control Systems").
narrative_ontology:topic_domain(measurement_timing_authority_erosion, "organizational_psychology/systems_theory/epistemology_of_control").

domain_priors:requires_active_enforcement(measurement_timing_authority_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(measurement_timing_authority_erosion, decision_authorities).
narrative_ontology:constraint_beneficiary(measurement_timing_authority_erosion, operational_actors).
narrative_ontology:constraint_victim(measurement_timing_authority_erosion, accountability_framework).
narrative_ontology:constraint_victim(measurement_timing_authority_erosion, real_time_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCOUNTABILITY FRAMEWORK (SNARE) — Measurement-based accountability cannot exit the timing mismatch. Formal responsibility is assigned to measured states that no longer reflect current reality. The framework bears full structural cost: it certifies actions taken on stale data while bearing reputational liability for outcomes. No exit, maximum experienced extraction.
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REAL-TIME STAKEHOLDERS (SNARE) — Operators in fast-changing domains (hospital units, industrial control rooms, emergency response) see measurement delay as a trap. They must execute authorized action on measurements known to be stale. Career risk and liability attach to them despite acting on formally authorized data. High suppression: they cannot refuse authorization based on real-time knowledge without violating formal governance. Exit is costly — whistleblowing, job loss.
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DECISION AUTHORITIES (TANGLED ROPE) — Executives benefit from the measurement-timing gap because it provides both authorization (measurement provides legitimacy) and deniability (stale data provides cover). They coordinate a genuine function: establishing formal authorization trails that distribute accountability downstream. Simultaneously, the timing lag enables extraction: decisions can be rationalized through measurements known to be outdated. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REAL-TIME SYSTEMS (TANGLED ROPE) — Organized agents building closed-loop feedback systems (advanced manufacturing controls, medical device protocols, autonomous systems) see measurement-timing erosion as a hybrid: they provide genuine coordination function (real-time monitoring) but face suppression from authorization requirements that enforce slow cycles. Constrained: they can install faster sensors and feedback, but authorization architecture (compliance reviews, governance sign-offs) still operates on slow timescales. The faster monitoring shows what has changed; formal authorization still lags.
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ADAPTIVE GOVERNANCE REFORMERS (SCAFFOLD) — Organizations implementing real-time compliance dashboards, situational authority protocols, and dynamic authorization renewal see the timing mismatch as a temporary coordination failure with a sunset. Sunset clause: as monitoring infrastructure matures (sensor arrays, data pipelines, decision-support systems), authorization cycles can be decoupled from measurement cycles and re-synchronized. Governance transforms from 'measure once, authorize broadly' to 'continuous measurement, continuous re-authorization.' Estimated sunset: 5-10 years for organizational norms to mature.
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CLASSICAL AUTHORIZATION RITUALS (PITON) — Established authorization procedures (quarterly reviews, annual audits, committee approvals, formal sign-offs) persist through institutional inertia despite becoming functionally obsolete for fast-changing domains. The ritual is maintained because it distributes formal responsibility and creates audit trails, not because it provides real-time accountability. Theater ratio is high: the procedures perform legitimacy and governance rather than enabling actual oversight. The constraint persists because the alternative (admitting formal authorization is not checking the measured state that actors will rely on) is organizationally unthinkable.
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INHERENT LAG VIEW (MOUNTAIN) — From a universal civilizational perspective, measurement-to-authorization lag appears as a natural limit: complex systems always have measurement latency, analysis time, and decision lag. Authority cannot act on unmeasured or unanalyzed states. This perspective sees the timing mismatch as inherent to how authorization works — measurement, analysis, authorization, execution form an inescapable sequence. However, the structural data reveals this as a false summit: the lag is not inherent to authorization but contingent on institutional architecture choices (synchronous vs asynchronous authorization, batch vs streaming authorization, implicit vs explicit real-time revision).
constraint_indexing:constraint_classification(measurement_timing_authority_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(measurement_timing_authority_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(measurement_timing_authority_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(measurement_timing_authority_erosion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(measurement_timing_authority_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(measurement_timing_authority_erosion, TR),
    TR >= 0.70.

:- end_tests(measurement_timing_authority_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting asymmetric benefit to decision authorities while bearing costs externalize to real-time stakeholders and accountability frameworks. The constraint is not pure extraction (no beneficiary has full immunity from consequences) but genuine mixed coordination-extraction. The measurement framework provides a real coordination function — it distributes accountability and prevents completely arbitrary decisions — while simultaneously enabling extraction through deniability: decisions rationalized by measurements known to be outdated at execution time. Suppression (0.58): Moderate-high. Real-time stakeholders cannot refuse to execute formally authorized decisions without violating governance structures; whistleblowing and deviation are suppressed through hierarchical authority, liability exposure, and job security. Alternative verification pathways (real-time data, intuitive operator knowledge) exist but carry organizational risk to voice. The suppression is not total because some organizations allow operational override under extreme conditions, but baseline suppression is substantial. Theater ratio (0.68): High and rising over the measured interval. Classical authorization procedures (quarterly reviews, annual compliance audits, committee approvals, sign-off ceremonies) increasingly perform legitimacy rather than function. The procedures create the appearance of control and accountability while actual decision-making increasingly relies on real-time data (dashboards, continuous monitoring) that arrives after authorization has concluded. The theater ratio rises as operational systems become faster (manufacturing cycle times compress, medical interventions accelerate, emergency response demands shorten) while authorization procedures remain fixed on historical timescales. The 0.48 → 0.68 trajectory reflects this widening gap.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The decision authority sees a Rope — a coordination mechanism distributing accountability. The real-time stakeholder sees a Snare — a trap where they execute stale-data decisions with full liability. The reformer sees a Scaffold — a temporary coordination failure with a sunset path through technology and process redesign. The classical authority ritual is a Piton — performing legitimacy while functionally obsolete. The real-time systems are Tangled Rope — providing genuine monitoring while suppressed by slow authorization. The accountability framework is Snare — no exit, maximum extraction. The analytical observer risks Mountain — seeing timing lag as inherent to authorization — but the structural data reveals false summit: the lag is contingent on institutional timing choices. The perspectival gap reflects that different organizational actors occupy radically different structural positions relative to the same formal mechanism. No single classification is 'correct' — the constraint is a presheaf, and the set of classifications IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural relationship each agent has to the timing gap. Decision authorities benefit from the gap (arbitrage): they gain both the coordination benefit of measurement-based authority and the extraction benefit of deniability. Their d ≈ 0.15 (net beneficiary with high exit optionality) produces negative χ (constraint subsidizes them). Real-time stakeholders bear the full cost (trapped): they execute authorized decisions on stale measurements and bear liability for outcomes. Their d ≈ 0.92 produces high χ (maximum experienced extraction). Real-time systems are mixed beneficiaries constrained by slow authorization (constrained): they provide genuine monitoring function but face suppression from governance requirements. Their d ≈ 0.55 produces moderate χ. The accountability framework is abstract and cannot exit; it is trapped bearing responsibility for a mechanism it does not control. Its d ≈ 0.98 produces near-maximum χ. The analytical observer at civilizational scope (d ≈ 0.72) risks seeing the timing gap as natural law, but the presence of beneficiaries and identifiable suppression mechanisms reveals structural contingency rather than inherent necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that measurement-timing authority erosion is not a single constraint viewed from multiple angles but a constraint family decomposed by timescale. At immediate timescale (real-time operations): Snare or Tangled Rope (stale measurements trap real-time actors). At biographical timescale (individual organizational careers): Tangled Rope (mixed coordination and extraction for moderate actors; Rope for beneficiaries; Snare for stakeholders). At generational timescale (organizational norm evolution): Scaffold (the timing mismatch is solvable through adaptive governance and real-time authorization redesign). At civilizational timescale (epistemology of control): Mountain false summit (risks naturalizing institutional timing as inherent limit). The true structure is a temporal presheaf: different time horizons instantiate the constraint differently. Additionally, the constraint decomposes by organizational form: hierarchical organizations experience higher extractiveness (decisions flow top-down from stale measurements); federated organizations with distributed authority experience lower extractiveness (decision-making can incorporate local real-time knowledge). Both decompositions are valid — they reveal different aspects of the same structural phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_staleness_threshold,
    'What measurement age threshold converts legitimate authorization lag into extractive authority erosion?',
    'Domain-specific analysis: compare decision outcomes when measurement age is below threshold vs above; correlation analysis between measurement staleness and decision quality/safety outcomes',
    'If threshold is < 1 cycle: most measurement-based authority is extractive (Snare classification dominates). If threshold is > 5 cycles: measurement-based authority retains legitimacy despite staleness (Rope classification plausible). Threshold determines whether timing gap is coordination problem or extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_staleness_threshold, empirical, 'Measurement staleness threshold distinguishing authorization lag from extraction').

omega_variable(
    real_time_alternative_feasibility,
    'Is real-time authorization (continuous measurement + continuous re-authorization) technically and organizationally feasible in governance-heavy domains, or does it structurally require deference to human judgment in ways that undermine the measurement-based authority premise?',
    'Case studies of organizations implementing real-time compliance dashboards; assessment of whether operators actually use continuous re-authorization or revert to batch authorization under pressure; measurement of decision-support system adoption rates and reliance patterns',
    'If feasible: scaffold sunset is real and measurement-timing erosion is a temporary coordination problem (supports Scaffold classification). If infeasible: the timing gap is structural to governance, and authority erosion is permanent (supports Snare/Tangled Rope as terminal states). The feasibility assessment determines whether the constraint is self-resolving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_time_alternative_feasibility, empirical, 'Whether real-time authorization is organizationally feasible').

omega_variable(
    deniability_extraction_mechanism,
    'To what degree does the measurement-timing gap enable deniability extraction — decision-makers using stale measurements as post-hoc justification for predetermined decisions?',
    'Analysis of authorization records before and after decision: compare whether measurements were actually causative of decisions or whether decisions preceded and shaped measurement interpretation; comparison of organizations with different measurement-to-authorization timing ratios and decision-reversal rates; interviews with decision-makers about measurement reliance',
    'If deniability is primary mechanism: extractiveness is higher than current 0.52 (should be 0.65+) and the constraint is primarily extraction (Snare/Tangled Rope dominate). If deniability is incidental: extractiveness estimate is accurate and coordination function is genuine. Affects whether beneficiaries are extracting from authority or using authority functionally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deniability_extraction_mechanism, empirical, 'Degree to which measurement timing enables deniability-based extraction').

omega_variable(
    false_summit_natural_law_candidate,
    'Is measurement-to-authorization lag a natural law of complex systems or a contingent institutional choice?',
    'Historical analysis: identify organizational periods where measurement-authorization synchrony was attempted; analysis of technical vs organizational barriers to real-time authorization; comparison across organizational forms (hierarchical vs distributed, centralized vs federated)',
    'If natural law: the mountain perspective is justified (authorization inherently lags measurement). If contingent: the mountain perspective naturalizes an institutional arrangement, and the constraint is a false summit (Tangled Rope or Snare is the underlying type).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether measurement-authorization lag is natural law or institutional contingency').

omega_variable(
    accountability_distribution_artifact,
    'Does the measurement-timing gap exist to solve a genuine coordination problem (distributing accountability) or to enable extraction (creating deniability while maintaining the appearance of control)?',
    'Structural analysis: identify whether measurement-based authorization emerged from coordination pressures (multiple decision-makers needing shared reference point) or from extraction pressures (actors needing cover for decisions made on other grounds); compare organizations with measurement-based authorization to those with alternative accountability distributions; assess whether removing the timing gap would improve or degrade accountability',
    'If coordination-originated: the Rope/Tangled Rope perspective is primary. If extraction-originated: the Snare perspective is primary and the constraint''s legitimacy is questioned. Affects whether the constraint is redeemable through better synchronization or requires structural replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_distribution_artifact, conceptual, 'Whether measurement-timing authority exists for coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(measurement_timing_authority_erosion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mtae_tr_t0, measurement_timing_authority_erosion, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mtae_tr_t3, measurement_timing_authority_erosion, theater_ratio, 3, 0.58).
narrative_ontology:measurement(mtae_tr_t6, measurement_timing_authority_erosion, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(mtae_be_t0, measurement_timing_authority_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mtae_be_t3, measurement_timing_authority_erosion, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mtae_be_t6, measurement_timing_authority_erosion, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mtae_su_t0, measurement_timing_authority_erosion, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(mtae_su_t3, measurement_timing_authority_erosion, suppression_requirement, 3, 0.53).
narrative_ontology:measurement(mtae_su_t6, measurement_timing_authority_erosion, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(measurement_timing_authority_erosion, enforcement_mechanism).
narrative_ontology:affects_constraint(measurement_timing_authority_erosion, regulatory_lag_extraction).
narrative_ontology:affects_constraint(measurement_timing_authority_erosion, governance_theater_inertia).
narrative_ontology:affects_constraint(measurement_timing_authority_erosion, real_time_stakeholder_accountability_trap).

% DUAL FORMULATION NOTE:
% Measurement-timing authority erosion decomposes into multiple constraint families. (1) TIMING CONSTRAINT: measurement cycle time vs authorization cycle time — technical/organizational. (2) EPISTEMOLOGY CONSTRAINT: whether measurement legitimacy requires temporal synchrony with measured state — philosophical/governance. (3) EXTRACTION CONSTRAINT: whether timing gap enables deniability extraction — structural. Each family member has distinct ε; network edges link them as structural dependents. The timing constraint (ε ≈ 0.35) is a coordination problem; the epistemology constraint (ε ≈ 0.58) is a mixed constraint; the extraction constraint (ε ≈ 0.72) is primarily extraction. This story addresses the mixed constraint and its temporal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(measurement_timing_authority_erosion, institutional, 0.18).
constraint_indexing:directionality_override(measurement_timing_authority_erosion, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
