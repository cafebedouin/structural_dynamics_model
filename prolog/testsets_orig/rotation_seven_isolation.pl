% ============================================================================
% CONSTRAINT STORY: rotation_seven_isolation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotation_seven_isolation, []).

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
 *   constraint_id: rotation_seven_isolation
 *   human_readable: R7 Medical Isolation Protocol
 *   domain: political/technological/social
 *
 * SUMMARY:
 *   The R7 Medical Isolation Protocol represents a structural constraint that
 *   enforces indefinite detention through medical justification. Individuals
 *   flagged under R7 criteria are subject to immediate removal and isolation,
 *   with minimal due process, opaque flagging criteria, and no clear exit
 *   mechanism. The constraint operates by deploying medical authority as
 *   cover for extraction: removal is framed as medical necessity
 *   (unchangeable, universal, science-based) when the actual mechanism is
 *   political discretion dressed in epidemiological language. The
 *   extractiveness has increased over the interval (0.52 → 0.78) as
 *   application scope has widened and appeal processes have become more
 *   perfunctory. Theater ratio has also increased (0.48 → 0.65), reflecting
 *   increasing reliance on medical legitimation narratives rather than actual
 *   medical necessity. This constraint exemplifies how false mountains are
 *   created: by naturalizing contingent political choices as immutable
 *   scientific limits.
 *
 * KEY AGENTS:
 *   - Flagged Individuals: Primary victims (powerless/trapped) — subject to indefinite isolation with no meaningful exit or appeal mechanism; bear full cost of extraction
 *   - At-Risk Populations: Secondary victims (moderate/constrained) — broader demographic categories subject to potential flagging; suppressed through uncertainty and threat of isolation
 *   - Protocol Administrators: Primary beneficiaries (institutional/arbitrage) — gain authority, resource allocation, enforcement power; can redefine criteria to serve institutional interests
 *   - State Health Apparatus: Primary beneficiary (institutional/arbitrage) — expands operational control, budget, regulatory domain through emergency framing; coordinates enforcement
 *   - Implementing Medical Professionals: Mixed actors (moderate/constrained) — forced to execute isolation orders, constrained by employment; benefit from institutional clarity but pay ethical cost
 *   - International Medical Community: Performative overseer (organized/constrained) — maintains ethical framework in declarations but lacks enforcement mechanisms; theater mechanism preserves legitimacy while enabling extraction
 *   - Analytical Observer: Civilian context (analytical/analytical) — risks naturalizing contingent political choice as scientific necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_isolation, 0.78).
domain_priors:suppression_score(rotation_seven_isolation, 0.88).
domain_priors:theater_ratio(rotation_seven_isolation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_isolation, extractiveness, 0.78).
narrative_ontology:constraint_metric(rotation_seven_isolation, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(rotation_seven_isolation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_isolation, snare).
narrative_ontology:human_readable(rotation_seven_isolation, "R7 Medical Isolation Protocol").
narrative_ontology:topic_domain(rotation_seven_isolation, "political/technological/social").

domain_priors:requires_active_enforcement(rotation_seven_isolation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotation_seven_isolation, protocol_administrators).
narrative_ontology:constraint_beneficiary(rotation_seven_isolation, state_health_apparatus).
narrative_ontology:constraint_victim(rotation_seven_isolation, flagged_individuals).
narrative_ontology:constraint_victim(rotation_seven_isolation, due_process_rights).
narrative_ontology:constraint_victim(rotation_seven_isolation, medical_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLAGGED INDIVIDUAL (SNARE) — No meaningful exit option. Subject to immediate removal, indefinite detention, and isolation justified as medical necessity. The flagging criterion is opaque, review is perfunctory, and the cost of non-compliance is loss of liberty. Maximum experienced extraction — stripped of agency, due process, and bodily autonomy.
constraint_indexing:constraint_classification(rotation_seven_isolation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AT-RISK POPULATION (SNARE) — Broader population categories subject to flagging (political dissidents, minorities, medical non-conformists) face chronic uncertainty and constrained options. Cannot exit the threat of flagging. Extraction occurs through suppression of speech, behavior, and mobility to avoid isolation. Theater mechanism: safety justification legitimates preventive detention.
constraint_indexing:constraint_classification(rotation_seven_isolation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTOCOL ADMINISTRATOR (ROPE) — Benefits from protocol authority, resource allocation, and expanded enforcement powers. Experiences the constraint as a coordination mechanism: clear procedures, defined authority, operational clarity. Low effective extraction because administrators have full agency and control. Arbitrage option: can redefine flagging criteria to serve institutional interests without legal constraint.
constraint_indexing:constraint_classification(rotation_seven_isolation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE HEALTH APPARATUS (ROPE) — Gains operational control, budget expansion, and regulatory authority under medical emergency framing. Experiences coordination: standardized isolation protocols, defined chains of command, resource flows. Net beneficiary — the apparatus expands its domain and legitimacy. Arbitrage exit: can adjust threat classification to maintain emergency authority.
constraint_indexing:constraint_classification(rotation_seven_isolation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPLEMENTING MEDICAL PROFESSIONAL (TANGLED ROPE) — Faces conflicting duties: patient care vs. protocol enforcement. Constrained by employment and licensing — refusal to execute isolation orders risks career loss. But also benefits from protocol clarity and institutional backing. Mixed extraction: forced to participate in deprivation while maintaining professional legitimacy through medical framing. Some benefit (job security, clear authority), substantial cost (ethical compromise, complicity).
constraint_indexing:constraint_classification(rotation_seven_isolation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL MEDICAL COMMUNITY (PITON) — Formally maintains medical ethics standards opposing non-consensual detention and medical abuse. But enforcement is theatrical: declarations of concern without enforcement mechanisms; reports issued without consequences; professional sanctions rare. The international apparatus preserves its legitimacy through performative condemnation while the protocol persists. Theater ratio high because oversight bodies exist and issue statements but do not disrupt the underlying extraction. Piton classification: ethical framework exists but has atrophied into ritual.
constraint_indexing:constraint_classification(rotation_seven_isolation, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — The protocol frames isolation as an unchangeable medical necessity (natural law). Disease containment is presented as immutable physics: if flagged, isolation *must* occur. However, the structural data reveals this as a false summit. The 'medical necessity' label masks a contingent political choice — the flagging criteria, isolation duration, and review mechanisms are discretionary policy decisions, not natural law constraints. The analytical observer risks naturalizing what is actually an institutional arrangement.
constraint_indexing:constraint_classification(rotation_seven_isolation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_isolation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotation_seven_isolation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_isolation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotation_seven_isolation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotation_seven_isolation, TR),
    TR >= 0.70.

:- end_tests(rotation_seven_isolation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and increasing. The protocol extracts liberty, bodily autonomy, and due process from flagged individuals with minimal compensation or coordination benefit. The extraction grows over time as: (1) flagging criteria expand to broader populations, (2) isolation durations become indefinite rather than time-limited, (3) appeal mechanisms atrophy from procedural rigor to rubber-stamp approval. Suppression (0.88): Extreme. Flagged individuals have no legal right to refuse isolation, no meaningful appeal process, and no exit option short of proving medical safety (burden impossible to meet). At-risk populations face suppression through fear of flagging. The constraint survives through legal frameworks that authorize removal, institutional capacity to enforce, and state monopoly on legitimacy. Theater ratio (0.65): Moderate-high and increasing. Medical necessity framing provides legitimation narrative, but actual isolation conditions and criteria reveal the underlying political-detention function. Theater increases as the protocol requires more public justification and faces more scrutiny — more medical language deployed to mask political operation. Claimed type: Snare. All structural properties match: pure extraction (no coordination benefit for victims), high suppression (no exit), high effective extraction, and growing institutional reliance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a maximal perspectival gap between beneficiaries and victims. Administrators and state apparatus experience coordination (Rope) — clear procedures, operational clarity, authority consolidation. Victims experience pure extraction (Snare) — indefinite detention, opaque criteria, no exit. Medical professionals experience coercion (Tangled Rope) — mixed benefit and cost. International observers experience performative oversight (Piton) — ethical frameworks maintained through declarations that do not disrupt extraction. The analytical observer at civilizational scope risks a false summit (Mountain) — naturalizing medical necessity when the underlying mechanism is political discretion. The perspectival divergence reflects structural reality: the same constraint delivers authority and resources to beneficiaries while extracting liberty and autonomy from victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position and exit capacity. Flagged individuals (powerless/trapped) have d ≈ 0.98 — full target, maximum extraction experienced. At-risk populations (moderate/constrained) have d ≈ 0.80 — substantial target position through flagging threat. Protocol administrators (institutional/arbitrage) have d ≈ 0.02 — full beneficiary, negative experienced extraction (they gain authority). State health apparatus (institutional/arbitrage) has d ≈ 0.05 — beneficiary position through expanded control. Medical professionals (moderate/constrained) have d ≈ 0.60 — mixed position, forced participation but with job security and institutional backing. International community (organized/constrained) has d ≈ 0.35 — performative actor, constrained by lack of enforcement mechanisms but with some agency to issue declarations. The high d values for victims produce high f(d) values via the sigmoid function, resulting in χ ≥ 0.66 across victim perspectives — Snare classification confirmed from victim views. The low d values for beneficiaries produce negative f(d), confirming Rope classification from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   RESOLVED: The R7 Protocol is unambiguously a Snare (pure extraction) from victim perspectives and Rope (pure coordination) from beneficiary perspectives. The mandatrophy is resolved by recognizing that these are genuinely different experiences of the same structural phenomenon, not contradictory classifications. The false summit at the analytical/civilizational level — where the constraint appears as an immutable medical law — is rejected by the structural data. Medical necessity is claimed but not demonstrated: flagging criteria are opaque, isolation durations are indefinite, appeal mechanisms are perfunctory, and implementation correlates with political opposition rather than epidemiological risk. The engine's false summit detector identifies the mountain classification as naturalization of a contingent political choice. The constraint is structurally a Snare with medical theater, not a Mountain with scientific foundation. Extractiveness > 0.70 triggers mandatrophy resolution: confirmed by the perspectival analysis showing pure extraction for victims (no coordination benefit) and unambiguous beneficiary positions for administrators (no legitimate cost). No ambiguity exists about the constraint's essential nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flagging_criterion_opacity,
    'What makes an individual ''flagged'' under R7, and who decides? Is the criterion medical, political, or hybrid?',
    'Freedom of information requests, leak analysis, or testimony from protocol administrators revealing flagging algorithms and decision criteria',
    'If purely medical: protocol may be classified as Rope or Scaffold (coordination mechanism). If political or hybrid: classification remains Snare (extraction mechanism). Opacity itself is the suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flagging_criterion_opacity, empirical, 'Transparency of flagging criteria and decision authority').

omega_variable(
    isolation_duration_necessity,
    'Are isolation durations medically determined or administratively discretionary? Is there exit criteria or indefinite detention?',
    'Case records analysis showing median duration, release criteria, appeal outcomes; medical literature on containment necessity; comparison to non-R7 medical isolation protocols',
    'If medically determined with clear exit: classification shifts toward Scaffold (temporary support). If indefinite or administratively renewable: classification remains Snare (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(isolation_duration_necessity, empirical, 'Whether isolation duration is medically determined or administratively discretionary').

omega_variable(
    actual_medical_risk_vs_justification,
    'Do flagged individuals actually pose the medical risk claimed by the protocol, or is medical danger a pretext for political/social removal?',
    'Epidemiological analysis of flagged populations; comparison of actual transmission rates between flagged vs. unflagged groups; analysis of flagging criteria correlation with medical vulnerability vs. political status',
    'If medical risk is real and proportional: classification shifts toward Tangled Rope (hybrid with legitimate coordination function). If medical risk is pretext: classification confirms Snare (extraction masked by medical theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_medical_risk_vs_justification, empirical, 'Whether flagged individuals actually pose claimed medical risk or medical danger is pretext').

omega_variable(
    review_mechanism_efficacy,
    'Can individuals meaningfully contest their flagging through review procedures, or are review processes theatrical?',
    'Appeal case analysis showing reversal rates, timeframes, and appeal success barriers; examination of evidence required to contest flagging vs. evidence required to impose it',
    'If reviews are substantive and reversals frequent: suppression metric decreases, possibly shifting toward Tangled Rope. If reviews are perfunctory with rare reversals: theater_ratio increases, suppression confirmed, Snare classification held.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(review_mechanism_efficacy, empirical, 'Whether review mechanisms provide meaningful contestation or are theatrical').

omega_variable(
    political_vs_medical_categorization,
    'Is the protocol operationally a medical isolation system or a political detention system mislabeled as medical?',
    'Comparative analysis: do flagging rates correlate with epidemiological risk or with political opposition/demographic targeting; analysis of isolation conditions against medical vs. carceral standards',
    'If operationally medical: Snare classification with medical coordination component. If operationally political: classification confirmed as pure extraction (Snare) with medical theater masking political detention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_medical_categorization, conceptual, 'Whether R7 functions as medical isolation or political detention with medical framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_isolation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r7iso_tr_t0, rotation_seven_isolation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(r7iso_tr_t3, rotation_seven_isolation, theater_ratio, 3, 0.57).
narrative_ontology:measurement(r7iso_tr_t6, rotation_seven_isolation, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(r7iso_be_t0, rotation_seven_isolation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(r7iso_be_t3, rotation_seven_isolation, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(r7iso_be_t6, rotation_seven_isolation, base_extractiveness, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_isolation, enforcement_mechanism).
narrative_ontology:affects_constraint(rotation_seven_isolation, medical_authority_legitimacy).
narrative_ontology:affects_constraint(rotation_seven_isolation, due_process_erosion).

% DUAL FORMULATION NOTE:
% R7 Medical Isolation Protocol is downstream of broader constraints around state emergency authority and medical gatekeeping. The protocol represents a specific instantiation of how emergency framing can mask extraction. Affects constraints dealing with due process erosion (how emergency mechanisms become permanent) and medical authority legitimacy (how medical language provides cover for political decisions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rotation_seven_isolation, institutional, 0.03).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
