% ============================================================================
% CONSTRAINT STORY: institutional_signal_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_signal_degradation, []).

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
 *   constraint_id: institutional_signal_degradation
 *   human_readable: Institutional Signal Degradation and Credibility Collapse
 *   domain: institutional_epistemology/organizational_trust
 *
 * SUMMARY:
 *   Institutional signal degradation describes the structural dynamic where
 *   organizations ostensibly designed to generate and communicate reliable
 *   information become mechanisms for suppressing, distorting, or obscuring
 *   that information. This constraint exhibits a fundamental tension:
 *   institutions provide essential coordination infrastructure (news
 *   organizations, regulatory agencies, scientific bodies, government
 *   communications) that agents depend on, yet this dependence creates
 *   extractive leverage. Gatekeepers can degrade signal quality with impunity
 *   because consumers cannot exit without sacrificing coordination benefits.
 *   The constraint shows both the tangled hybrid structure (genuine
 *   coordination function mixed with extraction) and the piton degradation
 *   (accountability mechanisms becoming theatrical). Suppression (0.62)
 *   reflects multiple barriers: information asymmetry (gatekeepers control
 *   what is knowable), institutional authority (gatekeepers define
 *   credibility), career risk for internal challengers, and captured
 *   oversight (auditors serve gatekeepers). The theater_ratio (0.68) reflects
 *   that accountability rituals (internal reviews, ombudsmen, fact-checking
 *   departments) increasingly perform legitimacy without verifying signal
 *   quality. Extractiveness (0.58) represents the accumulated cost to
 *   information consumers, subordinate agents, and the public epistemic
 *   commons: cognitive labor for signal verification, impaired
 *   decision-making from degraded information, and erosion of institutional
 *   trust that cascades to legitimate institutions.
 *
 * KEY AGENTS:
 *   - Information Consumers: Primary victims (powerless/trapped) — depend on institutions for reliable signals; cannot exit without sacrificing coordination benefits
 *   - Subordinate Institutional Agents: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with organization; forced to participate in signal degradation while internalizing blame
 *   - Institutional Gatekeepers: Primary beneficiaries (institutional/arbitrage) — capture information asymmetry; maintain authority through signal control; can exit to alternative institutions
 *   - Reform-Minded Internal Agents: Secondary actors (moderate/constrained) — attempt coordination repair; face suppression and career risk for challenging gatekeepers
 *   - Alternative Information Networks: Organized coalition (organized/mobile) — building distributed verification channels; constrained by coordination costs and gatekeeper suppression
 *   - Audit and Oversight Systems: Institutional actors (institutional/arbitrage) — maintain performative accountability rituals; serve gatekeeper interests through captured reviews
 *   - Analytical Observer: Civilian perspective (analytical/analytical) — risks naturalizing institutional failure as inherent to scale; misses contingent capture structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_signal_degradation, 0.58).
domain_priors:suppression_score(institutional_signal_degradation, 0.62).
domain_priors:theater_ratio(institutional_signal_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_signal_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_signal_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_signal_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_signal_degradation, tangled_rope).
narrative_ontology:human_readable(institutional_signal_degradation, "Institutional Signal Degradation and Credibility Collapse").
narrative_ontology:topic_domain(institutional_signal_degradation, "institutional_epistemology/organizational_trust").

domain_priors:requires_active_enforcement(institutional_signal_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_signal_degradation, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(institutional_signal_degradation, captured_regulators).
narrative_ontology:constraint_victim(institutional_signal_degradation, information_consumers).
narrative_ontology:constraint_victim(institutional_signal_degradation, subordinate_agents).
narrative_ontology:constraint_victim(institutional_signal_degradation, public_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION CONSUMER (SNARE) — Cannot exit institutional communication channels without sacrificing coordination benefits. Faces degraded signal quality with no alternatives. Trapped by dependence on institutions that have become unreliable while maintaining their authority. Maximum extraction: must process noise, detect hidden agendas, and invest cognitive labor in source verification that institutions once provided.
constraint_indexing:constraint_classification(institutional_signal_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBORDINATE AGENT (SNARE, IDENTITY-LOCKED) — Structurally mobile (could seek employment elsewhere) but identity-fused with the organization's mission and role. Believes in the institution's stated purpose even as they observe its degraded signaling. Cannot exit because exiting would mean abandoning professional identity and the worldview constituted through organizational loyalty. High suppression from internalized commitment. Extraction: forced to participate in signal degradation while shouldering blame for institutional failure.
constraint_indexing:constraint_classification(institutional_signal_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-MINDED INTERNAL AGENT (TANGLED ROPE) — Constrained by institutional hierarchy and career risk, but also benefits from attempting coordination repair. Some capacity to improve signaling; significant cost if they challenge gatekeepers. Mixed experience: genuine coordination function (institutions still mediate necessary information flows) alongside extraction (captured by gatekeepers who suppress reform signals and punish whistleblowers).
constraint_indexing:constraint_classification(institutional_signal_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL GATEKEEPER (ROPE) — Benefits from information asymmetry and reduced scrutiny. Experiences constraint as coordination: controlling what signals reach the public is presented as necessary institutional function. Can arbitrage to competing institutions; maintains power through signal control. Net extraction flows toward this actor; they perceive the mechanism as legitimate coordination of public communication.
constraint_indexing:constraint_classification(institutional_signal_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE INFORMATION NETWORK (TANGLED ROPE) — Organized agents (independent media, decentralized platforms, scientific communities) have genuine alternative channels but also require coordination to reach scale. Experience extraction from institutional gatekeepers' attempts to suppress alternatives (legal pressure, platform deplatforming, reputation attacks) alongside genuine coordination function building distributed verification. Mobile exit options (can migrate platforms, rebuild networks) reduce but don't eliminate effective extraction.
constraint_indexing:constraint_classification(institutional_signal_degradation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: AUDIT AND ACCOUNTABILITY THEATER (PITON) — Institutional mechanisms for signal verification (ombudsmen, fact-checking departments, internal reviews) persist through inertia despite degraded function. Theater ratio is high: the rituals of accountability look legitimate but lack teeth when gatekeepers control the review process. Institution maintains these mechanisms because alternatives haven't fully replaced them and dismantling them would signal loss of credibility. Performative credibility management.
constraint_indexing:constraint_classification(institutional_signal_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW RISK (MOUNTAIN) — From a universal perspective, institutional signal degradation could be framed as an inherent property of large-scale coordination: as organizations grow, internal communication degradation is inevitable due to scale constraints. This perspective risks naturalizing what is actually a contingent outcome of captured governance structures and misaligned incentives. The mountain classification is a false summit — it misidentifies institutional failure as natural law.
constraint_indexing:constraint_classification(institutional_signal_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_signal_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_signal_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_signal_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_signal_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_signal_degradation, TR),
    TR >= 0.70.

:- end_tests(institutional_signal_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint combines genuine coordination infrastructure loss (institutions were once reliable signal sources) with extraction mechanisms (gatekeepers weaponize dependence). The baseline (0.35 at T=0) reflects functional institutions with modest corruption; the endpoint (0.58 at T=10) reflects accumulated gatekeeping advantage as alternative channels remain subcritical. Suppression (0.62): Reflects multiple structural barriers: information asymmetry (gatekeepers control evidence available to consumers), institutional authority (gatekeepers define what counts as credible), legal/reputational tools (suppress internal challenges), and captured oversight (auditors report to gatekeepers). Not total suppression because some escape routes exist (alternative media, internal leaks) but substantial enough to trap powerless agents. Theater_ratio (0.68): Reflects accountability rituals that have lost function. Ombudsmen exist but report to leadership; fact-checking departments exist but their conclusions serve narrative control; audits exist but auditors can be captured. The ratio rises from 0.42 to 0.68 as institutions substitute ritual for repair — a classic piton signature. Claimed_type (tangled_rope): The constraint has real coordination function (institutions do communicate information that enables collective action) alongside extraction (gatekeepers degrade this function for power/rent). Both are structural, not optional.
 *
 * PERSPECTIVAL GAP:
 *   The gap between gatekeeper and consumer perspectives is maximal. Gatekeepers experience rope (coordination mechanism, immediate, arbitrage exit, global scope — rope classification with χ < 0.35). Consumers experience snare (extraction mechanism, biographical, trapped exit, national scope — snare classification with χ > 0.66). Same institutional structure, opposite experienced types, because the structural position determines d-value which scales f(d) which scales χ. This gap is diagnostic of asymmetric extraction: if the constraint were genuinely rope, all perspectives would classify similarly. The spectral spread here (rope → snare → piton → tangled rope across the same mechanism) confirms tangled_rope as the analytical type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position within the constraint. Gatekeepers with arbitrage options and beneficiary status: d ≈ 0.15 (low), f(d) ≈ -0.01 (negative chi, they experience subsidy from extraction). Information consumers trapped with victim status: d ≈ 0.95 (high), f(d) ≈ 1.42 (maximum f(d), experienced extraction approaches chi_max). Subordinate agents identity-locked but victim: d ≈ 0.89 (very high), f(d) ≈ 1.28 (very high extraction, internalized through identity fusion). Reform agents moderate/constrained: d ≈ 0.55 (near symmetric), f(d) ≈ 0.75 (moderate extraction). Coalition mobile: d ≈ 0.60 (slight victim lean), f(d) ≈ 0.85 (moderate). Scope modifier σ(national) = 1.0 (baseline); chi = ε × f(d) × σ(S). For trapped consumers: χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high effective extraction). For beneficiary gatekeepers: χ ≈ 0.58 × (-0.01) × 1.0 ≈ -0.01 (subsidy). This perspectival variation reveals the constraint's true structure: systematic extraction from powerless agents toward institutional gatekeepers, masked by coordination rhetoric.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy — the risk of mislabeling extraction as coordination or vice versa — by declaring both: tangled_rope type requires beneficiaries (coordination function exists; institutions genuinely coordinate information), victims (extraction exists; gatekeepers capture asymmetry), and active enforcement (gatekeepers actively suppress signals). The beneficiary/victim split prevents false-positive rope classification (would require no victims). The high suppression (0.62) and theater_ratio (0.68) prevent false-positive scaffold classification (would require sunset, which this lacks). The piton perspective correctly identifies accountability theater as degraded, not functional. The mountain perspective's natural law framing is correctly flagged as false summit (institutional scale limits are contingent on capture, not inherent). The constraint's genuine coordination function (institutions still enable information flow, even if degraded) alongside extraction (gatekeepers profit from degradation) is exactly tangled rope's definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_structural_degradation,
    'Is signal degradation the result of deliberate gatekeeping strategy or structural incompetence/capacity limits?',
    'Behavioral analysis of gatekeeper responses to signal quality improvements; comparison of effort allocation to cover-up vs repair; correlation between gatekeeping incentives and observed degradation patterns',
    'If intentional: Snare classification holds; extraction is conscious and optimized. If structural/incompetent: tangled rope (failures + genuine coordination gaps) may be more accurate; implies different remediation strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_structural_degradation, empirical, 'Whether signal degradation is strategic gatekeeping or structural incapacity').

omega_variable(
    alternative_channel_viability,
    'Can decentralized and alternative information networks achieve sufficient scale and verification quality to supplant institutional signaling?',
    'Longitudinal tracking of information accuracy rates and reach across institutional vs alternative channels; measurement of public trust migration; cost-benefit analysis of alternative network coordination infrastructure',
    'If viable: coalition perspective''s mobile exit becomes real; constraint transforms to scaffold with sunset. If not viable: powerless agents remain trapped; snare classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_channel_viability, empirical, 'Whether alternative information networks can achieve institutional-scale verification').

omega_variable(
    identity_lock_persistence,
    'Can subordinate agents maintain organizational loyalty while recognizing signal degradation, or does recognition force identity renegotiation?',
    'Longitudinal interviews with internal agents; measurement of cognitive dissonance resolution patterns; tracking of defection rates when signal degradation becomes undeniable',
    'If identity lock persists: suppression remains at 0.62+; subordinates internalize blame. If identity breaks: suppression drops sharply; becomes constrained rather than trapped exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Durability of organizational identity lock under signal degradation stress').

omega_variable(
    theater_ratio_feedback_loop,
    'Does increasing auditing/accountability theater (piton degradation) accelerate signal degradation through credibility erosion?',
    'Analysis of public trust trends correlated with audit/review ritual expansion; measurement of theater_ratio growth vs actual verification improvements; detection of ''audit fatigue'' in stakeholder engagement',
    'If positive feedback: theater_ratio approaching 1.0 signals piton collapse; constraint becomes pure snare. If decoupled: theater can persist longer; enables gatekeeper arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_feedback_loop, empirical, 'Whether accountability theater accelerates signal degradation through credibility erosion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_signal_degradation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isd_tr_t0, institutional_signal_degradation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(isd_tr_t3, institutional_signal_degradation, theater_ratio, 3, 0.55).
narrative_ontology:measurement(isd_tr_t6, institutional_signal_degradation, theater_ratio, 6, 0.64).
narrative_ontology:measurement(isd_tr_t10, institutional_signal_degradation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(isd_be_t0, institutional_signal_degradation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(isd_be_t3, institutional_signal_degradation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(isd_be_t6, institutional_signal_degradation, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(isd_be_t10, institutional_signal_degradation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_signal_degradation, information_standard).
narrative_ontology:boltzmann_floor_override(institutional_signal_degradation, 0.08).
narrative_ontology:affects_constraint(institutional_signal_degradation, regulatory_capture).
narrative_ontology:affects_constraint(institutional_signal_degradation, epistemic_authority_erosion).
narrative_ontology:affects_constraint(institutional_signal_degradation, public_trust_deficit).

% DUAL FORMULATION NOTE:
% Institutional signal degradation is upstream of regulatory capture (gatekeepers use degraded signals to capture regulators) and epistemic authority erosion (degraded signals undermine trust in institutions generally). The network captures dependency: if signal degradation worsens, regulatory capture probability increases; if alternative information networks mature (downstream constraint), signal degradation's extraction potential decreases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_signal_degradation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
