% ============================================================================
% CONSTRAINT STORY: institutional_narrative_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_narrative_control, []).

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
 *   constraint_id: institutional_narrative_control
 *   human_readable: Institutional Narrative Control
 *   domain: institutional_governance/epistemic
 *
 * SUMMARY:
 *   Institutional narrative control is the constraint through which dominant
 *   organizations shape interpretive frames, suppress competing narratives,
 *   and maintain epistemic monopoly over authorized knowledge. The constraint
 *   operates through coordinated gatekeeping — control of publishing
 *   platforms, research funding allocation, credentialing authority, and
 *   media access. It functions as genuine coordination (unified institutional
 *   messaging) combined with asymmetric extraction (suppressed dissent bears
 *   costs). The constraint exhibits all six DR types from different
 *   structural positions. For powerless dissenters, it is a pure snare — exit
 *   requires abandoning professional identity. For institutional leadership,
 *   it is a rope — solving the real problem of maintaining organizational
 *   coherence. For organized knowledge movements, it is a scaffold with
 *   genuine sunset — decentralized information distribution undermines
 *   gatekeeping. For legacy institutions, it is a piton — the control
 *   mechanism persists through theater rather than functional monopoly. The
 *   theatrical component (68% theater ratio at T=6) reflects that
 *   institutional narrative control increasingly operates through performance
 *   rather than structural control: media management, rhetorical
 *   amplification, and delegitimization rituals substitute for actual
 *   information monopoly as technical distribution barriers collapse.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures benefits of narrative monopoly without bearing enforcement costs; can arbitrage between internal disagreement and external messaging
 *   - Dissenting Voices: Primary victim (powerless/trapped) — face career damage, professional isolation, deplatforming; cannot exit without destroying professional identity
 *   - Alternative Framers: Secondary victim (moderate/constrained) — develop competing interpretations but face funding barriers, publication gatekeeping, social risk; constrained agency enables some parallel institution building
 *   - Decentralized Knowledge Movement: Organized alternative (organized/mobile) — blockchain publishing, open-access journals, social media platforms building parallel distribution with genuine exit paths and sunset logic
 *   - Legacy Media Institution: Institutional gatekeeper (institutional/arbitrage) — maintains editorial authority theater but technical information monopoly has collapsed; piton classification from degraded functional control
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional narrative control as inherent requirement for organizational function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_narrative_control, 0.58).
domain_priors:suppression_score(institutional_narrative_control, 0.65).
domain_priors:theater_ratio(institutional_narrative_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_narrative_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_narrative_control, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_narrative_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_narrative_control, tangled_rope).
narrative_ontology:human_readable(institutional_narrative_control, "Institutional Narrative Control").
narrative_ontology:topic_domain(institutional_narrative_control, "institutional_governance/epistemic").

domain_priors:requires_active_enforcement(institutional_narrative_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_narrative_control, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_narrative_control, narrative_gatekeepers).
narrative_ontology:constraint_victim(institutional_narrative_control, alternative_framers).
narrative_ontology:constraint_victim(institutional_narrative_control, dissenting_voices).
narrative_ontology:constraint_victim(institutional_narrative_control, field_epistemic_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING VOICE (SNARE) — Career damage, professional isolation, and deplatforming create structural barriers to contradiction of institutional narrative. No exit without destroying professional identity. Maximum experienced extraction — the powerless agent cannot organize alternatives or challenge the frame without incurring catastrophic costs.
constraint_indexing:constraint_classification(institutional_narrative_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE FRAMER (TANGLED ROPE) — Mid-career researchers or policy analysts who develop competing interpretations face resource barriers (funding concentrated in dominant narrative), publication barriers (peer review gatekeeping), and social risk. But some coordination function exists — the constraint does enable organized dissent communities and alternative frameworks to emerge through parallel institutions. Significant extraction but not maximal — constrained agency exists.
constraint_indexing:constraint_classification(institutional_narrative_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Benefits from narrative monopoly without bearing costs of enforcement. Can arbitrage between internal disagreement and external consistency messaging. Experiences the constraint as coordination: maintaining institutional coherence across stakeholders. Net beneficiary — extraction runs toward this agent through suppressed dissent and unified external messaging.
constraint_indexing:constraint_classification(institutional_narrative_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED KNOWLEDGE MOVEMENT (SCAFFOLD) — Organized agents (independent scholars, blockchain-based publishing, open-access journals, social media platforms) are building alternative narrative distribution channels. These represent genuine sunset mechanisms: as information asymmetry breaks down and gatekeeping loses technical advantage, the control constraint's extraction mechanism loses force. High agency, visible exit path, declining enforcement effectiveness over time.
constraint_indexing:constraint_classification(institutional_narrative_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEDIA INSTITUTION (PITON) — Traditional institutional narrative control through exclusive information access (newspapers, journals, broadcast) is substantially degraded. The institution maintains gatekeeping theater (editorial authority, peer review rituals, institutional prestige) but technical monopoly on information distribution has collapsed. Theater persists through institutional inertia, not through functional control — the constraint exists because alternatives haven't fully replaced it, not because it works.
constraint_indexing:constraint_classification(institutional_narrative_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational/universal perspective, institutional narrative control appears immutable: all institutions necessarily coordinate through shared interpretive frames; some narrative dominance is inherent to organizational survival. This perspective risks naturalizing contingent institutional arrangements as laws of institutional necessity. However, the structural data reveals this as a false summit — narrative control mechanisms are actively enforced, suppression is high, and alternative systems are demonstrably possible.
constraint_indexing:constraint_classification(institutional_narrative_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_narrative_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_narrative_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_narrative_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_narrative_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_narrative_control, TR),
    TR >= 0.70.

:- end_tests(institutional_narrative_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutional narrative control extracts through suppressed dissent, concentrated credentialing, and resource allocation toward dominant frames. But extraction is not total (0.72+) because alternative narratives do emerge and some funding/platforms exist outside institutional control. The reduced value reflects partial constraint — enforcement is weakening as decentralization increases. Suppression (0.65): High. Structural barriers to contradicting institutional narrative include career risk, publication gatekeeping, funding concentration, and social ostracism. These are significant but not absolute — brave or desperate agents still challenge, and organized movements can emerge. Theater ratio (0.68): High and increasing. Institutional narrative control increasingly operates through rhetorical performance (message discipline, media management, delegitimization of critics) rather than structural information monopoly. As technical gatekeeping fails (information distribution is decentralized), institutional theater substitutes — performative consistency replaces actual monopoly. The measurement trajectory shows theater rising faster than base extractiveness, indicating constraint degradation: the constraint is maintained increasingly through performance and less through function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion across institutional positions. The beneficiary (institutional leadership) perceives coordination and necessary institutional coherence. The victim trapped in the system (dissenting voice) perceives pure extraction and insurmountable barriers. The organized alternative movement perceives a temporary problem with a visible sunset — decentralized systems are genuinely emerging. The legacy institution perceives its own degradation — gatekeeping persists through theater, not through functional monopoly. The analytical observer risks the most dangerous misclassification: naturalizing institutional narrative control as immutable law rather than contingent mechanism. The gap widens as technology enables alternatives — what appears as mountain (immutable institutional necessity) from the analytical distance becomes scaffold (temporary coordination failure with sunset) from the organized agent position and snare (extraction mechanism) from the powerless position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the narrative control flow. Institutional leadership sits at d ≈ 0.10 (beneficiary with arbitrage options) — extraction flows toward them, they experience low or negative effective chi. Dissenting voices sit at d ≈ 0.92 (victim trapped without exit) — maximum experienced extraction. Alternative framers sit at d ≈ 0.58 (victim with constrained options) — significant extraction but some agency through parallel institution building. Decentralized knowledge movement sits at d ≈ 0.45 (organized victim with mobile options) — moderate extraction experienced, declining as their alternatives mature. Legacy media sits at d ≈ 0.15 (beneficiary but with declining functional advantage, transitioning to piton). The analytical observer sits at d ≈ 0.72 (observer position but risks naturalizing the constraint) — high epistemic extraction for the observer's own analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   INSTITUTIONAL NARRATIVE CONTROL AS TANGLED ROPE EXEMPLAR: This constraint resolves the mandatrophy by showing how institutional narrative control genuinely coordinates internal messaging while asymmetrically extracting from external dissent. Both functions are real. The coordination function (unified institutional voice) solves a genuine problem — large organizations must communicate coherently. The extraction function (suppressed alternatives) is equally real — benefits accrue to institutional leadership and narrative gatekeepers, costs borne by dissenters. The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is hybrid. The mandatrophy is resolved by recognizing that the same mechanism serves both functions: unified narrative is coordination internally, suppression externally. As decentralized alternatives mature, the coordination function could separate from the extraction — unified messaging could come from voluntary alignment rather than enforced gatekeeping. The scaffold perspective represents this potential future: temporary constraint solving a coordination problem until distributed systems mature enough to provide better solutions. The piton perspective represents the present: constraint persists through theater (media discipline, authority claims) even as functional monopoly degrades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_visibility_threshold,
    'At what point does suppression enforcement become visible enough that alternative narratives gain credibility rather than remaining marginalized?',
    'Longitudinal analysis of narrative adoption rates before and after enforcement becomes public; measurement of credibility gains when suppression tactics are exposed',
    'If visibility rapidly increases credibility: enforcement backfires and accelerates the constraint''s degradation. If visibility has minimal effect: suppression remains stable despite exposure. Controls the scaffold sunset timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_visibility_threshold, empirical, 'Threshold at which visible suppression increases alternative narrative adoption').

omega_variable(
    decentralized_coordination_sufficiency,
    'Can decentralized knowledge systems (blockchains, peer networks, federated publishing) actually coordinate shared interpretation as effectively as institutional gatekeeping, or do they fragment into irreconcilable narratives?',
    'Comparative analysis of interpretation convergence rates across decentralized vs institutional publishing ecosystems; measurement of epistemic common ground in peer networks vs traditional institutions',
    'If sufficient: institutional narrative control becomes functionally unnecessary and the constraint collapses to piton status. If insufficient: decentralized systems become epistemic chaos and traditional control reasserts value. Controls whether scaffold perspective is structural or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_coordination_sufficiency, empirical, 'Whether decentralized systems provide adequate narrative coordination').

omega_variable(
    identity_lock_reversibility,
    'For institutional actors (institutional_leadership), is narrative control internalized as genuine institutional identity or maintained as contingent instrumental strategy?',
    'Comparative case analysis of institutional behavior under external constraints: whether leadership maintains narrative control when external audience disappears; measurement of internal policy changes when external consistency requirements are removed',
    'If identity-locked: leadership cannot perceive alternatives even if external enforcement disappears — constraint persists as internalized institutional culture. If instrumental: constraint collapses immediately when enforcement mechanisms lose force. Determines whether piton classification is terminal or transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether institutional narrative control is identity-fused or instrumentally maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_narrative_control, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_narr_tr_t0, institutional_narrative_control, theater_ratio, 0, 0.52).
narrative_ontology:measurement(inst_narr_tr_t3, institutional_narrative_control, theater_ratio, 3, 0.6).
narrative_ontology:measurement(inst_narr_tr_t6, institutional_narrative_control, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(inst_narr_be_t0, institutional_narrative_control, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(inst_narr_be_t3, institutional_narrative_control, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(inst_narr_be_t6, institutional_narrative_control, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_narrative_control, identity_coordination).
narrative_ontology:affects_constraint(institutional_narrative_control, epistemic_gatekeeping).
narrative_ontology:affects_constraint(institutional_narrative_control, credentialing_monopoly).
narrative_ontology:affects_constraint(institutional_narrative_control, information_asymmetry_extraction).

% DUAL FORMULATION NOTE:
% Institutional narrative control is downstream of specific gatekeeping mechanisms (publishing access, funding allocation, credentialing authority) and upstream of epistemic pluralism constraints. Each upstream mechanism has its own extractiveness value reflecting domain-specific barriers; institutional narrative control represents the coordinated enforcement of these mechanisms across domains. The constraint family should be decomposed if measuring specific gatekeeping domains (academic publishing vs media vs professional licensing) separately — each has different ε values reflecting domain-specific technical barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_narrative_control, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
