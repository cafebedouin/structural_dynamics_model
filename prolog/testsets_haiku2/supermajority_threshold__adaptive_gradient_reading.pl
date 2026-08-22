% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold (Adaptive-Gradient Reading)
 *   domain: constitutional/political_economy
 *
 * SUMMARY:
 *   This story instantiates the adaptive-gradient reading of the
 *   supermajority threshold kernel. The reading holds that the threshold's
 *   legitimacy depends on empirical calibration to actual consensus-formation
 *   rates and reversibility costs in the specific jurisdiction and historical
 *   moment. Under this reading, the threshold is neither a natural
 *   constitutional fixed point (consensus-safeguard reading) nor an
 *   instrument of minority veto (minoritarian-veto reading), but a functional
 *   tool whose justified value varies with context: too low produces
 *   instability (rope-type coordination failure); too high produces
 *   ossification and entrenches status quo against durable majoritarian
 *   preference (snare-type extraction). The reading grounds legitimacy in
 *   measurable performance—whether the threshold in fact prevents churn while
 *   preserving adaptation to persistent consensus—rather than in intrinsic
 *   democratic or protective values. The claim here is tangled_rope: the
 *   threshold genuinely coordinates institutional stability (beneficiaries:
 *   institutional actors, consensus-measurement authorities) while
 *   asymmetrically extracting from rapid-reform constituencies and
 *   majoritarian coalitions (victims) who bear time costs and exclusion from
 *   normal democratic processes.
 *
 * KEY AGENTS:
 *   - institutional_stability_seekers: Benefit from predictability and reduced amendment churn; enjoy mobile exit options (can calibrate enforcement)
 *   - majoritarian_coalitions: Organized political majorities unable to enact constitutional change through regular channels; constrained exit (must negotiate or abandon reform)
 *   - rapid_reform_constituencies: Groups whose survival or identity depends on constitutional reform; identity-locked to the framework they seek to change; experience indefinite postponement
 *   - consensus_measurement_practitioners: Scholars and jurists who interpret and operationalize consensus; benefit from authority to set measurement standards and adjudicate claims
 *   - constitutional_courts: Agenda-setters that administer threshold enforcement and calibrate what counts as consensus; mobile through interpretive discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.62).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.41).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold (Adaptive-Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '711f4795-c71f-458b-8109-064e67121e3a').
narrative_ontology:cs_kernel_codification('711f4795-c71f-458b-8109-064e67121e3a', fixed_text).
narrative_ontology:cs_authority_grounding('711f4795-c71f-458b-8109-064e67121e3a', extraction).
narrative_ontology:cs_interpretation_layer_present('711f4795-c71f-458b-8109-064e67121e3a').
narrative_ontology:cs_reading_relation('711f4795-c71f-458b-8109-064e67121e3a', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('711f4795-c71f-458b-8109-064e67121e3a', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('711f4795-c71f-458b-8109-064e67121e3a', foundational, legitimacy_grounded_in_performance).
narrative_ontology:cs_axiom_status(legitimacy_grounded_in_performance, holdable).
narrative_ontology:cs_axiom_grounding('711f4795-c71f-458b-8109-064e67121e3a', legitimacy_grounded_in_performance, instrumental).
narrative_ontology:cs_axiom('711f4795-c71f-458b-8109-064e67121e3a', foundational, threshold_calibration_empirically_determinable).
narrative_ontology:cs_axiom_status(threshold_calibration_empirically_determinable, holdable).
narrative_ontology:cs_axiom_grounding('711f4795-c71f-458b-8109-064e67121e3a', threshold_calibration_empirically_determinable, empirically_contingent).
narrative_ontology:cs_reference_frame('711f4795-c71f-458b-8109-064e67121e3a', consensus_calibrated_threshold).
narrative_ontology:cs_drift_state('711f4795-c71f-458b-8109-064e67121e3a', contemporary_polarization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('711f4795-c71f-458b-8109-064e67121e3a', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, institutional_stability_seekers).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, consensus_measurement_practitioners).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, majoritarian_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, rapid_reform_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, minority_blocking_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, reformist_legislators).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, performance_based_constitutional_calibration).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, context_dependent_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional actors (courts, legislatures, regulatory bodies) that benefit from constitutional stability and predictability. They commission or endorse high supermajority thresholds to reduce churn and preserve established institutional authority. They benefit from delayed change that requires repeated consensus-formation efforts, which increases their negotiating leverage across cycles.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_stability_seekers, beneficiary,
    institutional, generational, mobile, national).

% Electoral and legislative majorities that seek constitutional change to address perceived injustice or governance failure. They bear the cost of supermajority requirements: their policy preferences cannot be enacted even when they command 51-60% of electoral support, forcing them into prolonged negotiation or constitutional entrenchment of status quo. Exit options include peaceful political organizing (constrained by time and resource scarcity) or non-constitutional channels (illegal or delegitimizing).
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, majoritarian_coalitions, payer,
    organized, biographical, constrained, national).

% Groups whose core identity or material survival depends on constitutional reform: persecuted minorities seeking rights expansion, workers seeking labor protections, colonized peoples seeking self-determination. They experience supermajority requirements as indefinite postponement of remedy. Their identity-lock derives from the constitutional structure itself—they cannot exit the framework without abandoning claims to membership. Time costs of consensus-formation accumulate on their constituencies while status quo harms persist.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, rapid_reform_constituencies, payer,
    moderate, biographical, identity_locked, national).

% Scholars, demographers, political scientists, and judicial actors who interpret and measure social consensus. Under this reading, their expertise becomes determinative: they certify whether a proposed change reflects genuine, persistent consensus or transient passion. They benefit from authority to calibrate the threshold, set measurement standards, and adjudicate consensus claims. They can arbitrage across jurisdictions or epistemic communities by selecting which measurement regime applies.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, consensus_measurement_practitioners, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, consensus_measurement_practitioners, agenda_setter).

% Organized factions whose policy preferences are protected by supermajority requirements: property owners resisting redistribution, religious majorities resisting secularization, regional elites resisting centralization. They benefit because the threshold converts their minority status into veto power. They can costlessly block change proposals unless a supermajority forms against them. Their mobility derives from their ability to credibly threaten non-cooperation if the threshold is lowered.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, minority_blocking_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Legislators or executives commanding clear majoritarian support but unable to enact constitutional change through regular channels. They incur administrative, political, and opportunity costs negotiating supermajority coalitions. They may resort to constitutional workarounds (creative interpretation, executive overreach, plebiscitary appeals) that undermine rule-of-law legitimacy. Their exit option—accepting constitutional stasis—costs them electoral mandate and constituency trust.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reformist_legislators, payer,
    powerful, biographical, constrained, national).

% Judicial bodies that interpret the constitutional threshold and, under this reading, assess whether proposed changes meet the consensus standard. They administer the enforcement machinery: certifying that supermajorities exist, measuring consensus through legislative votes, public opinion analysis, and deliberative processes. They can calibrate the threshold by varying what counts as consensus evidence. Their mobility derives from their ability to shift interpretive standards between reform cycles.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_courts, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, constitutional_courts, observer).

% The doctrine that legitimate constitutional authority derives from majority will expressed through regular electoral processes. This tradition is structurally excluded from the supermajority mechanism: its voice enters only as the opposition threshold (51% is insufficient). It would argue that supermajority requirements contradict fundamental democratic principles and require justification by performance, not appeal to stability. It appears in legal testimony and political rhetoric but the threshold itself is designed to bind its influence.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, democratic_majoritarian_tradition, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__adaptive_gradient_reading, democratic_majoritarian_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, institutional_stability_seekers).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The supermajority threshold solves a collective-action coordination problem: it prevents constitutional amendment driven by narrow electoral swings while preserving the possibility of adaptation to persistent consensus. It coordinates institutional actors (courts, legislatures) around a stability norm and forces coalition-building across factional lines, which surfaces deeply-held preferences and filters transient majorities.
% TRANSFER_FUNCTION: Transfers amendment authority from simple-majority coalitions (51%+) to supermajority coalitions (typically 60-67%), and from majoritarian preferences to institutionalized consensus-measurement authorities (courts, scholars). Transfers time costs and negotiation burden to rapid-reform constituencies, who must invest in repeated consensus-building campaigns. Transfers veto power to blocking minorities, who can indefinitely prevent change through non-cooperation.
% ABSENT_VOICES: Rapid-reform constituencies (persecuted minorities, workers, colonized peoples) whose material survival depends on constitutional change are structurally underrepresented in supermajority formations. They would argue that the threshold privileges indefinite postponement over remedy, and that consensus measurement favors status-quo equilibria because blocking coalitions can afford to wait while reform constituencies incur urgency costs. These voices appear in legislative testimony but the mechanism is designed to limit their direct influence on amendment outcomes.
% DISAPPEARANCE_RATIONALE: If supermajority requirements vanished, amendment rates would increase, rights-expansion and redistribution initiatives would pass on simple majorities, blocking minorities would lose veto power, and institutional stability would depend on alternative mechanisms (legitimacy norms, party polarization, two-party equilibrium). The equilibrium would reorganize substantially: reform constituencies would gain decisive power, institutional actors would face higher amendment risk, and the time cost of coalition-building would shift to status-quo defenders.
% FOUNDING_PROBLEM: Constitutional churn: early constitutions and electoral systems with weak legitimacy norms and high amendment rates experienced instability driven by momentary factional leverage. Simple-majority amendment procedures enabled narrow coalitions to entrench constitutional preferences without durable consensus, leading to rapid reversal and institutional decay.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and Federalist doctrine attest that constitutional churn was a historical problem in unstable democracies (19th-century Latin America, post-colonial constitutions). Consensus-measurement and stability-seeking scholars argue the founding problem remains live in polarized contexts. Majoritarian reform constituencies and rapid-reform advocates attest the founding problem is solved by modern electoral legitimacy and polarization (parties are durable, norms are stable, churn is prevented by institutional factors not amendment procedures). Political science literature is divided on whether supermajorities cause stability (some evidence supports it; some evidence suggests stability comes from other factors). No corroborating source outside the stability-seeking coalition attributes the founding problem as still-live.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the threshold asymmetrically distributes amendment power: stable majorities (51-66%) cannot enact change; blocking minorities gain veto power; reform constituencies bear repeated negotiation costs and time delays. However, extractiveness is not extreme (snare-level) because the threshold does perform a genuine coordination function (preventing churn) and the measurement of consensus introduces a calibration mechanism that could, in principle, adjust the threshold to legitimate levels. Suppression (0.41) is moderate because the threshold operates through formal constitutional rules (not direct coercion), majorities retain the formal right to propose amendments, and the suppression of alternative consensus-measurement methods is partial—competing interpretations of consensus still enter legislative debate. Theater ratio (0.28) is low-moderate: while courts and scholars do ritualize consensus-measurement (invoking deliberative processes, supermajority voting counts as consensus evidence), much of the enforcement activity is functionally necessary to operate the threshold. The measurement series shows extractiveness rising early (t=0 to t=15, as majoritarian pressure for change intensifies and the cost of blocked reforms accumulates) then plateauing (t=15 to t=40, as political actors adapt to the regime and find workarounds or accept entrenchment). This plateau suggests the constraint reaches an equilibrium extractiveness rather than ratcheting toward full snare. The reading's core claim is that this empirical profile (moderate-high extraction, moderate suppression, low theater) is NOT a sign the threshold has failed, but a sign it requires recalibration based on consensus-formation rates: if rapid-reform constituencies represent genuinely persistent 55-65% consensus, the threshold is too high (is extracting rather than protecting); if they represent transient majorities that shift with electoral cycles, the threshold is calibrated correctly. The adaptive reading makes legitimacy depend on this measurement, not on abstract principles.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-stability seat, the threshold functions as genuine coordination (low d, low effective extraction). From the rapid-reform seat, the same mechanism operates as snare-type extraction (high d, high effective extraction). From the consensus-measurement seat, the threshold is a calibration tool—neither pure beneficiary nor pure target, but an agenda-setter that can adjust the arrangement (d near 0.5, high power, mobile exit). The per-seat computation by the engine will show this divergence explicitly: what is coordination for one seat is extraction for another. This divergence is not an error—it is the measurement the story exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional stability seekers are beneficiaries (d near 0.0): they benefit from reduced amendment churn, preserved institutional authority, and increased negotiating leverage across electoral cycles. They have mobile exit options (can credibly threaten non-cooperation if threshold is lowered; can recommend interpretive calibrations). Majoritarian coalitions are full targets (d near 1.0): they bear the cost of excluded preference; they have constrained exit (must negotiate within the supermajority rule or accept entrenchment). Rapid-reform constituencies are the most-targeted (d = 1.0): their exit options are identity-locked (cannot exit the constitutional framework without abandoning membership claims); they bear accumulated time costs and urgency costs (status-quo harms continue while negotiations drag). Consensus-measurement practitioners are secondary beneficiaries (d near 0.2): they benefit from authority to interpret and calibrate; they have arbitrage-grade exit (can select measurement standards across jurisdictions and epistemic communities, can shift interpretive regimes between cycles). The directionality logic is non-uniform across stakeholders, which is the source of the tangled_rope classification: the threshold coordinates institutional stability AND extracts from rapid-reform constituencies through the same mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional churn through momentary factional leverage) was a genuine problem in early constitutions with rapid amendment procedures and weak electoral legitimacy norms. In contemporary constitutions with stable two-party systems, strong electoral legitimacy, and institutional traditions of norm-following, the founding problem has substantially attenuated: churn is prevented by factors other than supermajority requirements (polarization, incumbent advantage, legitimacy norms). However, the supermajority mechanism persists and has accrued new functions: it now protects minority blocking coalitions (property owners, regional elites, religious majorities) against majoritarian redistribution and secularization efforts. The mechanism has not been abandoned—it has been repurposed. This is not quite mandatrophy (death of founding function + persistence through inertia), because the mechanism does retain a residual coordinating function (it does slow change and force coalition-building, which can be adaptive even when churn is not the live threat). Rather, the supermajority threshold exhibits drift: the ratio of coordination to extraction has shifted. Early in its operation (t=0), the threshold primarily prevented churn (coordination-dominant, lower extractiveness). Currently (t=40), the threshold primarily protects minority veto power while the coordination benefit is secondary (extraction-dominant, higher extractiveness). Mandatrophy resolution depends on whether the contemporary ratio is justified by current consensus-formation rates and reversibility costs: if durable majorities (60%+ persistent consensus) are blocked, the threshold is no longer calibrated and has drifted toward extraction. If electoral volatility remains high and actual consensus is narrow and transient, the threshold is still calibrated correctly. The reading makes legitimacy contingent on this measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_measurement_contestation,
    'What observable operations count as evidence of genuine social consensus, and who adjudicates between competing measurement regimes?',
    'Comparative constitutional law: examine which measurement standards different jurisdictions and court systems have adopted and whether those standards correlate with amendment outcomes and reform-constituency satisfaction. Deliberative democracy research on consensus-formation processes.',
    'If consensus measurement is contested and opaque, the threshold devolves into institutional capture of the interpretation machinery—agenda-setters (courts, scholars) can set measurement standards to protect preferred coalitions. If measurement can be standardized and empirically grounded (e.g., persistent 60%+ supermajority in opinion polling + legislative support), the threshold can be calibrated defensibly. The reading''s legitimacy claim depends entirely on whether measurement is contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_measurement_contestation, empirical, 'Whether consensus measurement can be operationalized objectively or remains contested and capture-prone').

omega_variable(
    threshold_reversibility_asymmetry,
    'Is the supermajority threshold itself reversible? Can a simple majority (or lower threshold) eliminate the supermajority requirement, or is the requirement entrenched against its own repeal?',
    'Constitutional-text analysis: check whether supermajority rules apply to amendments to the supermajority rule itself. Empirical observation from constitutional change events.',
    'If the supermajority requirement is self-entrenching (super-majority needed to eliminate it), the threshold converts into a permanent structural veto, and the entire legitimacy claim depends on the threshold being perfectly calibrated at inception—no correction mechanism exists. If simple-majority repeal is possible, the threshold remains a tool that majorities can adjust, and the calibration problem becomes manageable. Self-entrenchment converts the reading from ''functional tool'' to ''structural trap''—a snare rather than tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_reversibility_asymmetry, empirical, 'Whether supermajority requirements are self-entrenching or reversible by simple majority').

omega_variable(
    identity_lock_persistence_under_exclusion,
    'Do rapid-reform constituencies (persecuted minorities, colonized peoples, workers) remain committed to constitutional change through the supermajority process, or do indefinite delays push them toward non-constitutional channels (civil disobedience, constitutional rupture, institutional exit)?',
    'Historical and ethnographic observation of reform constituencies'' strategic choices after repeated supermajority blocking. Polling on satisfaction with constitutional process. Measurement of non-constitutional action (strikes, protests, institutional separation).',
    'If repeated blocking causes identity-locked constituencies to abandon faith in the constitutional process, the constraint''s suppression becomes internalized (actors believe the process is unfair) and latent resistance increases, raising the risk of constitutional breakdown. If constituencies maintain commitment despite delays, the identity-lock persists and suppression remains structural. The measurement trajectories shift: low resistance + persistent identity-lock is a sign the constraint is working as designed (people accept the rule); rising resistance + eroding identity-lock is a sign the constraint has become extraction without sufficient coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_under_exclusion, empirical, 'Whether identity-locked constituencies erode commitment to constitutional process under indefinite supermajority blocking').

omega_variable(
    adaptive_reading_vs_consensus_safeguard_relation,
    'Can the adaptive-gradient reading coexist with the consensus-safeguard reading within a single constitutional framework, or do they have incompatible epistemic foundations?',
    'Constitutional-law analysis: examine whether courts that use safeguard framing (the threshold ensures deep consensus) also use adaptive framing (we calibrate the threshold based on measured consensus). Check for explicit contradiction in judicial reasoning.',
    'If the readings coexist, courts can appeal to safeguard reasoning in some cases and adaptive reasoning in others—this permits flexibility but undermines principled calibration (captures the appearance of consistency without the substance). If the readings foreclose each other, courts must choose: either the threshold is justified by its intrinsic protective value (safeguard) or by its measured performance (adaptive). The choice determines how consensustoration contests are framed and resolved. A court using safeguard reasoning cannot recalibrate without admitting that the founding problem is dead; a court using adaptive reasoning can recalibrate without admitting failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_reading_vs_consensus_safeguard_relation, conceptual, 'Compatibility of adaptive-gradient and consensus-safeguard readings as simultaneous judicial doctrines').

omega_variable(
    consensus_persistence_vs_electoral_volatility,
    'In the specific jurisdiction, what is the empirical rate of genuine consensus formation (60%+ supermajority that persists across two or more election cycles)? How volatile are electoral coalitions?',
    'Political science quantitative analysis: measure supermajority-capable coalitions over time. Examine whether proposed amendments reflect transient coalitions or persistent preferences. Compare to historical baseline of amendment passage rates and coalition durability.',
    'If genuine persistent supermajorities form regularly (60%+ in polls and elections, stable across 2+ cycles) but fail to amend, the threshold is too high (extractive, preventing adaptation to durable preferences). If proposed amendments reflect narrow, transient majorities and the supermajority prevents churn, the threshold is calibrated correctly (coordinating function dominates extraction). The adaptive reading''s legitimacy depends on this empirical finding. If consensus is persistent and blocked, the reading collapses toward minoritarian-veto. If consensus is transient and the threshold prevents churn, the reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_persistence_vs_electoral_volatility, empirical, 'Empirical rate of genuine consensus formation and electoral volatility in the jurisdiction').

omega_variable(
    court_interpretive_capture_risk,
    'Do courts and consensus-measurement authorities (scholars, jurists) have incentives to underestimate consensus for reforms that threaten institutional stability or their own authority?',
    'Meta-analysis of consensus-measurement decisions and amendment outcomes: do courts systematically reject consensus claims for reforms courts oppose? Examine cases where courts endorsed weaker consensus evidence for amendments courts favored.',
    'If courts capture the interpretation machinery, the adaptive reading becomes a framework for legitimating institutional veto under the guise of measurement—the constraint becomes snare-type (pure extraction via captured mechanism). If courts apply standards neutrally, the reading can function as claimed. The capture risk is high because consensus measurement is inherently contestable; courts can rationalize any measurement standard and then claim to follow it neutrally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_interpretive_capture_risk, empirical, 'Whether institutional actors systematically bias consensus measurement to block disfavored reforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t5, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(supe_tr_t5, observed).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(supe_tr_t10, observed).
narrative_ontology:measurement(supe_tr_t15, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(supe_tr_t15, observed).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(supe_tr_t20, observed).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(supe_tr_t25, observed).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(supe_tr_t30, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(supe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t5, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(supe_be_t5, observed).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(supe_be_t10, observed).
narrative_ontology:measurement(supe_be_t15, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(supe_be_t15, observed).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(supe_be_t20, observed).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(supe_be_t25, observed).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(supe_be_t30, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(supe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t5, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(supe_su_t5, observed).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(supe_su_t10, observed).
narrative_ontology:measurement(supe_su_t15, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(supe_su_t15, observed).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(supe_su_t20, observed).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(supe_su_t25, observed).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(supe_su_t30, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(supe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel has three structurally distinct readings, each with different ε values and beneficiary/victim structures. The adaptive-gradient reading treats the threshold as a functional tool whose legitimacy depends on empirical calibration to consensus-formation rates. The consensus-safeguard reading treats it as an intrinsic protective device ensuring deep consensus. The minoritarian-veto reading treats it as a conversion of historical privilege into permanent veto power. These are not different perspectives on the same constraint—they are different constraints (different referents for ε, different mechanisms of operation). The three stories are linked by network edges; sibling relationships are formalized in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, institutional, 0.15).
constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
