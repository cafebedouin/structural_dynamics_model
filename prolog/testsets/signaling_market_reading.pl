% ============================================================================
% CONSTRAINT STORY: signaling_market_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_signaling_market_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: signaling_market_reading
 *   human_readable: Costly-Signal Cooperation Market (Spence Equilibrium Reading)
 *   domain: cooperation_theory/institutional_economics/evolutionary_game_theory
 *
 * SUMMARY:
 *   In a population where cooperative intent cannot be directly observed, a
 *   costly signal (a certification badge, guild membership, bonding deposit,
 *   professional uniform) lets genuine cooperators separate themselves from
 *   defectors: only agents who plan to cooperate repeatedly find the signal's
 *   maintenance cost worthwhile, so its mere presence is informative.
 *   Counterparties rationally use the signal as a screening heuristic rather
 *   than verifying each interaction directly. Over time, however, the
 *   entities that administer the signal (certification bodies, licensing
 *   boards, bonding intermediaries) acquire an independent interest in the
 *   fee stream, and the signal's cost begins to track intermediary
 *   rent-seeking capacity as much as separating power. Established,
 *   well-capitalized cooperators who can amortize the signal cost across many
 *   transactions benefit disproportionately, while cooperators who are
 *   equally trustworthy but new, poor, or embedded in informal reputation
 *   networks the formal market does not recognize are treated as
 *   indistinguishable from defectors.
 *
 * KEY AGENTS:
 *   - established_high_reputation_cooperators: primary beneficiary (powerful/arbitrage) — amortizes signal cost across volume
 *   - signal_certification_intermediaries: primary beneficiary and agenda_setter (institutional/arbitrage) — administers and prices the signal
 *   - novice_cooperators: primary target (powerless/constrained) — genuinely cooperative but priced out
 *   - low_capital_cooperators: primary target (powerless/trapped) — cannot front signal cost
 *   - informal_sector_reciprocators: excluded/target (powerless/trapped) — real reputation unrecognized by formal market
 *   - signal_relying_counterparties: secondary beneficiary/payer (organized/mobile) — uses signal as cheap heuristic, pays rent-inflated premium to signal holders
 *   - game_theoretic_analysts: analytical observer (analytical) — evaluates separating power vs rent drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(signaling_market_reading, 0.52).
domain_priors:suppression_score(signaling_market_reading, 0.38).
domain_priors:theater_ratio(signaling_market_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(signaling_market_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(signaling_market_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(signaling_market_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(signaling_market_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(signaling_market_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(signaling_market_reading, tangled_rope).
narrative_ontology:human_readable(signaling_market_reading, "Costly-Signal Cooperation Market (Spence Equilibrium Reading)").
narrative_ontology:topic_domain(signaling_market_reading, "cooperation_theory/institutional_economics/evolutionary_game_theory").

domain_priors:requires_active_enforcement(signaling_market_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(signaling_market_reading, '5aa887fb-a8b6-471d-9165-46826b303f39').
narrative_ontology:cs_kernel_codification('5aa887fb-a8b6-471d-9165-46826b303f39', distributed).
narrative_ontology:cs_authority_grounding('5aa887fb-a8b6-471d-9165-46826b303f39', practice).
narrative_ontology:cs_interpretation_layer_present('5aa887fb-a8b6-471d-9165-46826b303f39').
narrative_ontology:cs_reading_relation('5aa887fb-a8b6-471d-9165-46826b303f39', credible_cooperator_kernel__audit_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aa887fb-a8b6-471d-9165-46826b303f39', credible_cooperator_kernel__commitment_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aa887fb-a8b6-471d-9165-46826b303f39', credible_cooperator_kernel__exit_option_reading, influences).
narrative_ontology:cs_axiom('5aa887fb-a8b6-471d-9165-46826b303f39', foundational, trust_emerges_from_signal_cost_not_verification).
narrative_ontology:cs_axiom_status(trust_emerges_from_signal_cost_not_verification, holdable).
narrative_ontology:cs_axiom_grounding('5aa887fb-a8b6-471d-9165-46826b303f39', trust_emerges_from_signal_cost_not_verification, empirically_contingent).
narrative_ontology:cs_axiom('5aa887fb-a8b6-471d-9165-46826b303f39', secondary, signal_affordability_stratifies_trustworthiness_recognition).
narrative_ontology:cs_axiom_status(signal_affordability_stratifies_trustworthiness_recognition, holdable).
narrative_ontology:cs_axiom_grounding('5aa887fb-a8b6-471d-9165-46826b303f39', signal_affordability_stratifies_trustworthiness_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('5aa887fb-a8b6-471d-9165-46826b303f39', spence_separating_equilibrium_baseline).
narrative_ontology:cs_drift_state('5aa887fb-a8b6-471d-9165-46826b303f39', contemporary_credentialing_inflation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5aa887fb-a8b6-471d-9165-46826b303f39', '').
narrative_ontology:cs_kernel_id(signaling_market_reading, credible_cooperator_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(signaling_market_reading, established_high_reputation_cooperators).
narrative_ontology:constraint_beneficiary(signaling_market_reading, signal_certification_intermediaries).
narrative_ontology:constraint_victim(signaling_market_reading, novice_cooperators).
narrative_ontology:constraint_victim(signaling_market_reading, low_capital_cooperators).
narrative_ontology:constraint_victim(signaling_market_reading, informal_sector_reciprocators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(signaling_market_reading, signal_relying_counterparties).
narrative_ontology:constraint_victim(signaling_market_reading, signal_relying_counterparties).
narrative_ontology:constraint_vindicates(signaling_market_reading, spence_signaling_equilibrium_holds_for_cooperation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already possess accumulated reputation capital and repeat-interaction history, so the marginal cost of maintaining the costly signal (certifications, bonding, visible uniforms, membership dues) is small relative to the volume of cooperative exchange it unlocks. They amortize the fixed cost across many transactions and effectively price out newer entrants who cannot spread the cost the same way.
narrative_ontology:constraint_stakeholder(signaling_market_reading, established_high_reputation_cooperators, beneficiary,
    powerful, generational, arbitrage, regional).

% Operate the certification, bonding, or credentialing apparatus that manufactures the costly signal (guild seals, professional licenses, escrow bonds, verified badges). They set the price and difficulty of acquiring the signal and collect fees regardless of whether the signal's separating function is still doing real epistemic work.
narrative_ontology:constraint_stakeholder(signaling_market_reading, signal_certification_intermediaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(signaling_market_reading, signal_certification_intermediaries, agenda_setter).

% Are genuinely willing to cooperate honestly but lack the capital, track record, or standing to afford the signal at a price that makes sense for a single early transaction. They are pooled with defectors by counterparties not because their intentions differ, but because the signal-market prices them out of the separating equilibrium before they can build the reputation that would let them afford it.
narrative_ontology:constraint_stakeholder(signaling_market_reading, novice_cooperators, payer,
    powerless, biographical, constrained, local).

% Are structurally identical in cooperative intent to signal-holders but cannot front the bonding capital, membership fee, or credentialing cost. They either accept worse terms as unsignaled counterparties, exit the market for cooperative exchange entirely, or pay predatory intermediaries for partial signals that offer weak separation.
narrative_ontology:constraint_stakeholder(signaling_market_reading, low_capital_cooperators, payer,
    powerless, biographical, trapped, local).

% Operate reputation and reciprocity through dense local networks that do the same epistemic work as formal signals, but the formal signal market does not recognize local reputation as a substitute. They are treated as unsignaled defectors by any counterparty outside their network, regardless of actual cooperative history.
narrative_ontology:constraint_stakeholder(signaling_market_reading, informal_sector_reciprocators, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(signaling_market_reading, informal_sector_reciprocators, excluded).

% Use the presence or absence of the costly signal as a cheap screening device to avoid the cost of individually verifying every counterparty. They benefit from the separating equilibrium's information value but also pay a premium to signaled counterparties that partly reflects rent rather than pure risk-adjustment.
narrative_ontology:constraint_stakeholder(signaling_market_reading, signal_relying_counterparties, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(signaling_market_reading, signal_relying_counterparties, payer).

% Model the signaling market as a Spence-style equilibrium, evaluating whether the signal cost genuinely separates cooperator types or whether it has drifted into a rent-extraction toll layered on top of a once-functional separating mechanism.
narrative_ontology:constraint_stakeholder(signaling_market_reading, game_theoretic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(signaling_market_reading, signal_certification_intermediaries).
narrative_ontology:fixing_cost_class(signaling_market_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine adverse-selection problem: without some costly, hard-to-fake signal, cooperators and defectors are indistinguishable ex ante, so counterparties cannot tell who to trust and cooperative exchange collapses to the pooling equilibrium (mutual defection or no trade). The signal lets genuine cooperators separate themselves because only they find the ongoing cost worth bearing.
% TRANSFER_FUNCTION: Moves certification fees, bonding capital, and membership dues from cooperators (concentrated on those least able to amortize the cost) to signal-issuing intermediaries and, indirectly, transfers favorable trading terms from unsignaled to signaled cooperators regardless of their actual reliability.
% ABSENT_VOICES: Novice and low-capital cooperators who are functionally trustworthy but priced out of the signal are not represented in the market's clearing price for signals — the equilibrium is set by intermediaries and established signal-holders, not by the excluded tier who would argue for cheaper or reputation-portable alternative signals.
% DISAPPEARANCE_RATIONALE: If the signaling market vanished overnight, counterparties would lose their cheap screening heuristic and would have to fall back on costlier direct verification, local reputation networks, or accept a higher-defection pooling equilibrium in the short run; established signal-holders would lose their competitive advantage over unsignaled entrants, and certification intermediaries would lose their fee stream entirely.
% FOUNDING_PROBLEM: Cooperators and defectors are observationally indistinguishable before interaction, so absent some mechanism only genuine cooperators find worth sustaining, rational counterparties should refuse to cooperate with anyone, collapsing mutually beneficial exchange to the pooling equilibrium.
% FOUNDING_PROBLEM_CORROBORATION: Game-theoretic analysts and independent market-design researchers attest the separating function remains partially live — signal cost still correlates with lower defection rates in most studied markets. But labor economists studying credentialing inflation and access-to-market researchers (outside the certification-intermediary interest) document that signal cost has risen faster than any measurable increase in separating power, consistent with the mechanism having partly decoupled from its founding function into toll collection.
narrative_ontology:disappearance_verdict(signaling_market_reading, world_rearranges).
narrative_ontology:founding_problem_status(signaling_market_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(signaling_market_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(signaling_market_reading, 'none', 1).
narrative_ontology:epsilon_provenance(signaling_market_reading, 0.52, 'claude-sonnet-5', 'conditional_vs_unconditional_cooperation_2026_20260725_131209', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(signaling_market_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(signaling_market_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(signaling_market_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and theater_ratio (0.41) are both mid-range and both authored as rising over the interval: the signal began as a functioning separating device (low extraction, low theater at t=0) and has drifted toward a toll as certification intermediaries gained an independent stake in the fee stream disconnected from actual separating accuracy — theater rising alongside extraction is the signature of Goodhart drift, where possessing the badge substitutes for the underlying cooperative disposition it was meant to certify. Suppression (0.38) is moderate: there is no direct coercion forcing anyone to seek the signal, but market participants who lack it face a de facto exclusion from favorable terms that functions as suppression of an alternative equally-valid trust-building path (informal reputation). Accessibility_collapse (0.44) reflects that alternatives to the formal signal (direct verification, informal reputation networks) still exist but are increasingly discounted by signal-reliant counterparties, partially collapsing the field of workable substitutes. Resistance (0.55) is comparatively high because excluded cooperators and access-to-market researchers actively contest the rising cost and declining marginal separating power of the signal.
 *
 * PERSPECTIVAL GAP:
 *   From the established cooperator's or certification intermediary's seat, the signal market looks like clean coordination solving a genuine information problem — this is the tangled rope's coordination face, and it is not fabricated; the adverse-selection problem is real and the signal genuinely reduces it in aggregate. From the novice or low-capital cooperator's seat, the identical structure looks like an entry toll that has nothing to do with whether they would actually cooperate — the same mechanism, read from the other side of the capital-stratification line, is where the extraction lives. Both readings are structurally correct simultaneously; that is precisely what makes this a tangled rope rather than a pure rope or pure snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Established high-reputation cooperators and certification intermediaries sit near the beneficiary end of directionality: the former because volume lets them amortize a fixed cost that functions as a toll booth against new entrants, the latter because they administer and price the very apparatus that manufactures scarcity of the signal. Novice cooperators, low-capital cooperators, and informal-sector reciprocators sit near the target end: they are structurally identical in cooperative intent to the signal-holders but are trapped or constrained by capital or recognition barriers that have nothing to do with actual trustworthiness. Signal-relying counterparties are genuinely dual-positioned — real beneficiaries of the screening heuristic's information value, but also payers of a premium partly inflated by rent rather than risk-adjustment, which is why they carry a secondary payer role rather than a pure beneficiary designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adverse selection under unobservable cooperative type) has not disappeared — mutual strangers still cannot verify intent directly, so the coordination function documented in coordination_function remains partially live. But the founding_problem_status is authored as contested rather than dead precisely because independent researchers outside the certification-intermediary interest document that signal cost has decoupled from separating accuracy over the measured interval (rising theater_ratio alongside rising extractiveness), which is the diagnostic signature of a constraint whose original coordination function is being used as cover for a toll that has outgrown it. Classifying this as tangled_rope rather than snare or rope prevents both mislabeling errors: calling it a pure snare would deny the real information-economics function it still performs for signal-relying counterparties; calling it a pure rope would erase the documented stratification of cooperators by capital rather than by trustworthiness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signaling_reading_vs_sibling_readings,
    'Is ''credible cooperation'' best modeled as a costly-signal separating market (this reading), a monitoring/audit relationship (audit_reading), an enforceable-commitment device (commitment_reading), or an exit-option equilibrium (exit_option_reading) — and does the correct model vary by domain (guild certification vs. escrow bonding vs. repeated-game reputation)?',
    'Cross-domain empirical comparison: measure whether trust levels track signal cost (supporting this reading), monitoring intensity (supporting audit_reading), contract enforceability (supporting commitment_reading), or counterparty switching cost (supporting exit_option_reading) across multiple cooperation markets.',
    'If the signaling-market mechanism dominates in a given domain, the victim set is capital-stratified cooperators rather than all cooperators uniformly, and remedies should target signal-cost accessibility rather than monitoring capacity or contract enforcement infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signaling_reading_vs_sibling_readings, conceptual, 'Which structural model of credible cooperation applies in a given empirical domain — the four sibling readings are not fully commensurable and may each be locally correct.').

omega_variable(
    signal_cost_separating_power_decoupling,
    'Has the signal''s cost genuinely decoupled from its separating power (i.e., is the rising extraction measured over the interval rent-seeking by intermediaries) or has the cost risen because the population of would-be defectors has grown more sophisticated, requiring a genuinely costlier signal to maintain the same separating accuracy?',
    'Compare defection rates among signal-holders over the interval against signal cost: if defection rates among signal-holders are stable or falling while cost rises, decoupling (rent-seeking) is supported; if defection rates among signal-holders are also rising, cost increases may be tracking a genuine arms race rather than pure extraction.',
    'Decoupling supports the tangled_rope classification with intermediaries as the concentrated extractive beneficiary; a genuine arms race would suggest the constraint remains closer to a functional rope whose cost increase is defensible coordination expense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(signal_cost_separating_power_decoupling, empirical, 'Whether rising signal cost reflects rent extraction or a genuine increase in the difficulty of separating types.').

omega_variable(
    informal_reputation_substitutability,
    'Is informal-network reputation (used by informal_sector_reciprocators) a functionally equivalent substitute for the formal costly signal, or does it carry genuinely lower information value to outside counterparties who cannot verify it?',
    'Compare cooperation and defection outcomes between formally-signaled and informally-reputed cooperators when both interact with the same class of outside counterparties, controlling for actual prior defection history.',
    'If informal reputation carries equivalent information value, its non-recognition by the formal signal market is pure exclusionary rent-protection for certification intermediaries; if it carries genuinely lower verifiable information value to strangers, some of the exclusion reflects a real (if harsh) information asymmetry rather than pure gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_reputation_substitutability, empirical, 'Whether excluding informal reputation networks from the formal signal market is justified by real information limits or is pure rent protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(signaling_market_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sign_tr_t0, signaling_market_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sign_tr_t8, signaling_market_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(sign_tr_t16, signaling_market_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(sign_tr_t24, signaling_market_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(sign_tr_t32, signaling_market_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(sign_tr_t40, signaling_market_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(sign_be_t0, signaling_market_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sign_be_t8, signaling_market_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(sign_be_t16, signaling_market_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(sign_be_t24, signaling_market_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(sign_be_t32, signaling_market_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(sign_be_t40, signaling_market_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sign_su_t0, signaling_market_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sign_su_t8, signaling_market_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(sign_su_t16, signaling_market_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(sign_su_t24, signaling_market_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(sign_su_t32, signaling_market_reading, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(sign_su_t40, signaling_market_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(signaling_market_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(signaling_market_reading, 0.12).
narrative_ontology:affects_constraint(signaling_market_reading, audit_reading).
narrative_ontology:affects_constraint(signaling_market_reading, commitment_reading).
narrative_ontology:affects_constraint(signaling_market_reading, exit_option_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of credible_cooperator_kernel, each instantiating a structurally distinct mechanism by which cooperation becomes credible: signaling_market_reading (this story, a Spence-style costly-signal separating equilibrium stratifying cooperators by capital), audit_reading (monitoring/verification), commitment_reading (binding/enforceable promises), and exit_option_reading (repeated-game reputation via switching cost). Each carries its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than merged because the underlying mechanisms — and therefore who bears the cost and who captures the gain — differ structurally across readings even though all four are colloquially described as 'trust' or 'credible cooperation.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
