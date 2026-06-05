% ============================================================================
% CONSTRAINT STORY: rotation_seven_isolation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The R7 Medical Isolation Protocol represents a archetypal snare disguised
 *   as medical necessity. Individuals flagged by an opaque authority are
 *   immediately removed from society and isolated indefinitely under the
 *   claim that this is an unchangeable medical requirement. The constraint
 *   exhibits high extractiveness (0.78) and high suppression (0.82),
 *   reflecting the complete removal of due process, medical autonomy, and
 *   exit options. The theater ratio (0.65) indicates substantial performative
 *   justification: the protocol's framing as medical necessity obscures its
 *   function as state power over bodily isolation with no meaningful clinical
 *   criteria for release. The structural gap between the claimed type
 *   (mountain, natural law) and the actual metrics (snare, pure extraction)
 *   exemplifies how foundational constraints in authoritarian systems
 *   weaponize medical and safety language to naturalize coercive power. The
 *   protocol's mandatrophy is resolved by recognizing that the beneficiaries
 *   (administrators, flagging authority) experience rope-like coordination
 *   benefits while the victims (flagged individuals) experience pure snare
 *   extraction. The analytical observer confronts a false summit: the
 *   protocol claims to be an immutable feature of medical science, but the
 *   high extractiveness, absence of clinical release criteria, and indefinite
 *   detention reveal it as a political choice weaponized through medical
 *   framing.
 *
 * KEY AGENTS:
 *   - Flagged Individual: Primary victim (powerless/trapped) — no due process, immediate removal, indefinite isolation, stripped medical autonomy
 *   - Protocol Administrators: Primary beneficiary (institutional/arbitrage) — control authority, legal protection, resource concentration during operation
 *   - Flagging Authority: Secondary beneficiary (institutional/arbitrage) — discretionary power, no accountability, ability to weaponize protocol against political opponents or disfavored populations
 *   - Medical Professionals: Organized actors (organized/constrained) — constrained to participate, benefit from liability protection, experience moral injury and autonomy loss
 *   - Broader Population: Secondary victims (moderate/constrained) — constrained by loss of due process norms, medical autonomy normalization, state power expansion
 *   - Public Health Bureaucracy: Institutional actor (institutional/constrained) — maintains protocol through inertia despite atrophied medical rationale; theater-heavy justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_isolation, 0.78).
domain_priors:suppression_score(rotation_seven_isolation, 0.82).
domain_priors:theater_ratio(rotation_seven_isolation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_isolation, extractiveness, 0.78).
narrative_ontology:constraint_metric(rotation_seven_isolation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(rotation_seven_isolation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_isolation, snare).
narrative_ontology:human_readable(rotation_seven_isolation, "R7 Medical Isolation Protocol").
narrative_ontology:topic_domain(rotation_seven_isolation, "political/technological/social").

domain_priors:requires_active_enforcement(rotation_seven_isolation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotation_seven_isolation, protocol_administrators).
narrative_ontology:constraint_beneficiary(rotation_seven_isolation, flagging_authority).
narrative_ontology:constraint_victim(rotation_seven_isolation, flagged_individuals).
narrative_ontology:constraint_victim(rotation_seven_isolation, due_process_rights).
narrative_ontology:constraint_victim(rotation_seven_isolation, medical_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLAGGED INDIVIDUAL (SNARE) — No due process, no appeal mechanism, no exit from isolation once flagged. Removal is immediate and indefinite. Experiences pure coercive extraction: separation from society, medical autonomy stripped, no meaningful consent. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.84.
constraint_indexing:constraint_classification(rotation_seven_isolation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROTOCOL ADMINISTRATORS (ROPE) — Experience the protocol as a coordination mechanism for population control and risk management. Framed as solving a collective action problem (infection/contamination prevention). Beneficiaries who can exit the constraint by wielding authority. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.09. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(rotation_seven_isolation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CLINICAL SAFETY JUSTIFICATION (FALSE SUMMIT) — The protocol claims to emerge naturally from medical necessity: isolation is an unchangeable law of epidemiology. However, the structural data (ε=0.78, suppression=0.82) contradicts the mountain gate (ε≤0.25, suppression≤0.05). This is a false summit—the 'inherent medical necessity' framing naturalizes what is actually a contingent political choice with high extraction and suppression. The engine will flag this as feigned natural law.
constraint_indexing:constraint_classification(rotation_seven_isolation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MEDICAL PROFESSIONALS (TANGLED ROPE) — Organized actors constrained by protocol mandates but also benefit from legal protection against liability for harm done during isolation. The protocol provides a coordination function (unified treatment standards, centralized decision-making) alongside extractive enforcement (loss of professional autonomy, moral injury from participation in coercive isolation). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(rotation_seven_isolation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH BUREAUCRACY (PITON) — Maintains the isolation protocol long after its claimed medical rationale has been superseded by data. The protocol persists through institutional inertia: bureaucratic entrenchment, sunk costs in infrastructure, career dependence on continuation. Theater ratio = 0.65 reflects that the protocol is mostly performative justification wrapped in medical language, with functional verification atrophied. d≈0.20, f(d)≈0.10, σ=1.0 → χ≈0.07.
constraint_indexing:constraint_classification(rotation_seven_isolation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BROADER POPULATION (SNARE) — Constrained but not fully trapped. Benefits from the framing of isolation as collective safety, but experiences extraction through loss of due process rights, medical autonomy norms, and normalization of state power over bodily isolation. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.76.
constraint_indexing:constraint_classification(rotation_seven_isolation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

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
 *   Extractiveness (0.78): Very high. The protocol extracts bodily autonomy, freedom, due process rights, and medical self-determination from flagged individuals with no compensation or recourse. The extraction is total for those flagged and operates indefinitely. The measurement shows growth from 0.45 to 0.78 over the interval, reflecting creeping expansion of criteria and lengthening of typical isolation durations. Suppression (0.82): Very high. Multiple barriers prevent escape: immediate removal without appeal, indefinite detention, lack of transparent release criteria, asymmetric information (authority determines flagging but not individual), criminalization of resistance, normalization through medical framing. Suppression is structural and sustained. Theater ratio (0.65): Moderate-high and rising. The protocol's justification is substantially performative: medical necessity is claimed but release criteria are administrative rather than clinical. The rise from 0.38 to 0.65 reflects increasing reliance on performative language as the protocol's medical rationale is undermined by data. Claimed type (Snare): Confirmed by metrics. High extractiveness, high suppression, no coordination function for victims, existence depends on suppressing alternatives (legal challenge, due process norms, medical ethics guidelines).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radical perspectival divergence. The protocol administrators see rope—a coordination mechanism for collective safety. The flagged individual sees snare—pure extraction with no escape. The clinical safety justification falsely claims mountain status but has snare metrics. The medical professionals see tangled rope—coordinated decision-making but also forced participation in coercive extraction. The public health bureaucracy sees piton—a ritual maintained by inertia despite atrophied function. The broader population sees snare—constrained but not entirely trapped, benefits from false sense of safety but pays in lost rights and normalization of state power. The false summit (mountain perspective) is the most dangerous reading: it naturalizes the constraint as unchangeable medical law, when the metrics reveal it as a contingent political choice with maximal extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Flagged individual: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction and suppression. Protocol administrators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries with authority to exit constraints. Flagging authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Discretionary power, no exit constraints. Medical professionals: Mixed + constrained → d≈0.55, f(d)≈0.75. Constrained to participate but protected from liability. Broader population: Victim-beneficiary + constrained → d≈0.70, f(d)≈1.08. Constrained by loss of rights but sold on false safety narrative. Public health bureaucracy: Institutional + constrained → d≈0.20, f(d)≈0.10. Maintains protocol but with reduced functional authority; theater-heavy.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH STRUCTURAL CLARITY: The R7 constraint avoids mandatrophy by showing that the false summit (mountain/natural law perspective) contradicts the empirical metrics. The protocol claims to be an unchangeable medical necessity (mountain: ε≤0.25, suppression≤0.05, emerges_naturally=true). The actual metrics (ε=0.78, suppression=0.82, theater=0.65) disconfirm this. The engine's false natural law detector will flag this as a feigned mountain. The true classification is snare (ε≥0.46, suppression≥0.60, χ≥0.66 — all thresholds met). The mandatrophy resolution lies in recognizing that the protocol's beneficiaries (administrators, flagging authority) have strategically framed the constraint as natural law to prevent legitimacy challenge. The analytical observer who accepts the false summit commits to naturalizing what is actually a contingent political choice. The mediated resolution: acknowledge snare classification, recognize false summit as deliberate naturalization, reinstate due process and medical autonomy as non-negotiable constraints that the protocol violates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_threshold,
    'What empirical criteria establish when isolation is a medical necessity vs. a political choice?',
    'Epidemiological data on transmission rates, clinical outcomes, and alternative containment methods; comparison with evidence-based isolation protocols from independent medical bodies',
    'If truly medical-necessary: mountain classification recovers legitimacy. If primarily political: snare classification is confirmed, and the false summit is exposed as deliberate naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_necessity_threshold, empirical, 'Threshold for medical necessity of isolation vs political choice').

omega_variable(
    flagging_authority_legitimacy,
    'What mechanism determines who is ''flagged'' for isolation, and is that mechanism subject to independent review?',
    'Analysis of appeal processes, due process guarantees, transparency of flagging criteria; comparison with jurisdictional standards for involuntary medical detention',
    'If legitimate independent review exists: constraint becomes tangled rope with procedural safeguards. If opaque or non-appealable: snare classification is solidified, and extraction is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flagging_authority_legitimacy, empirical, 'Legitimacy and review mechanisms for flagging decisions').

omega_variable(
    isolation_duration_unboundedness,
    'Is isolation duration bounded by objective clinical criteria or indefinite pending authority discretion?',
    'Review of release criteria, appeal processes, data on median isolation duration, documentation of individuals held beyond claimed medical necessity',
    'If bounded and clinical: constraint approaches scaffold (temporary support). If indefinite or administrative: snare classification is confirmed with maximum suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(isolation_duration_unboundedness, empirical, 'Whether isolation duration is bounded by clinical criteria or indefinite').

omega_variable(
    therapeutic_vs_punitive_intent,
    'Is isolation designed as therapeutic isolation or as detention/punishment framed as medical?',
    'Analysis of protocol documentation, medical oversight, living conditions, access to care, court findings on involuntary detention precedents',
    'If therapeutic: classification may shift toward tangled rope. If primarily detention: snare is confirmed, and the medical framing is revealed as theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_vs_punitive_intent, conceptual, 'Whether isolation intent is therapeutic or punitive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_isolation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r7iso_tr_t0, rotation_seven_isolation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(r7iso_tr_t3, rotation_seven_isolation, theater_ratio, 3, 0.52).
narrative_ontology:measurement(r7iso_tr_t6, rotation_seven_isolation, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(r7iso_be_t0, rotation_seven_isolation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(r7iso_be_t3, rotation_seven_isolation, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(r7iso_be_t6, rotation_seven_isolation, base_extractiveness, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_isolation, enforcement_mechanism).
narrative_ontology:affects_constraint(rotation_seven_isolation, bodily_autonomy_normalization).
narrative_ontology:affects_constraint(rotation_seven_isolation, state_medical_power_expansion).
narrative_ontology:affects_constraint(rotation_seven_isolation, due_process_attenuation).

% DUAL FORMULATION NOTE:
% The R7 isolation protocol is structurally downstream of broader state power expansion mechanisms but represents a distinct snare constraint. The upstream constraints establish the normalization of state authority over medical decisions; the R7 protocol operationalizes that normalized authority through concrete coercive isolation. Decomposition: state_medical_power_expansion (ε≈0.35, tangled rope) is upstream; rotation_seven_isolation (ε≈0.78, snare) is downstream and more extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rotation_seven_isolation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
