% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Path Dependency Reading)
 *   domain: technology_history/economics
 *
 * SUMMARY:
 *   This story instantiates the path-dependency reading of the
 *   QWERTY-persistence kernel: the layout survived not because any coalition
 *   of manufacturers, trainers, or institutions engineered its lock-in for
 *   profit, but because it solved a real (now-obsolete) mechanical problem
 *   early, and the resulting installed base of trained typists and
 *   manufactured hardware created a self-reinforcing coordination equilibrium
 *   with no actor able to unilaterally profit from disrupting it. This is a
 *   distinct constraint from the sibling strategic_lock_in_reading, which
 *   holds that manufacturer cartels and training-institution partnerships
 *   actively manufactured and defend the lock-in for rent. The two readings
 *   share the observable (QWERTY's persistence) but diverge sharply on
 *   epsilon: this reading holds extraction near zero and diffuse; the sibling
 *   holds it substantial and captured. Per the epsilon-invariance principle,
 *   these are authored as separate constraint files linked structurally, not
 *   as one constraint with two measurements.
 *
 * KEY AGENTS:
 *   - typists_and_general_users: bear whatever diffuse efficiency cost exists, individually, with no coordinated relief available
 *   - incumbent_keyboard_manufacturers: respond to existing demand rather than manufacturing it; they are beneficiaries of an accident, not architects of a scheme
 *   - alternative_layout_designers: excluded by the coordination problem itself, not by suppression
 *   - economic_historians: analytical observers assessing whether the persistence reflects genuine market failure or an overstated claim of inefficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Layout Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/economics").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '3eeae620-6147-4460-a6bb-7db4a2a2ff93').
narrative_ontology:cs_kernel_codification('3eeae620-6147-4460-a6bb-7db4a2a2ff93', distributed).
narrative_ontology:cs_authority_grounding('3eeae620-6147-4460-a6bb-7db4a2a2ff93', distributed).
narrative_ontology:cs_reading_relation('3eeae620-6147-4460-a6bb-7db4a2a2ff93', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('3eeae620-6147-4460-a6bb-7db4a2a2ff93', foundational, persistence_is_uncaptured_coordination_equilibrium).
narrative_ontology:cs_axiom_status(persistence_is_uncaptured_coordination_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('3eeae620-6147-4460-a6bb-7db4a2a2ff93', persistence_is_uncaptured_coordination_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('3eeae620-6147-4460-a6bb-7db4a2a2ff93', secondary, manufacturer_response_is_demand_following_not_demand_shaping).
narrative_ontology:cs_axiom_status(manufacturer_response_is_demand_following_not_demand_shaping, holdable).
narrative_ontology:cs_axiom_grounding('3eeae620-6147-4460-a6bb-7db4a2a2ff93', manufacturer_response_is_demand_following_not_demand_shaping, empirically_contingent).
narrative_ontology:cs_reference_frame('3eeae620-6147-4460-a6bb-7db4a2a2ff93', mechanical_typebar_era_original_design_rationale).
narrative_ontology:cs_drift_state('3eeae620-6147-4460-a6bb-7db4a2a2ff93', contemporary_digital_keyboard_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3eeae620-6147-4460-a6bb-7db4a2a2ff93', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, incumbent_keyboard_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, typists_and_general_users).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, network_effects_theory_of_technology_lock_in).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, path_dependency_economics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn QWERTY because everyone else uses QWERTY and every keyboard sold uses QWERTY. Bear a small, diffuse efficiency cost relative to alternative layouts (disputed magnitude), but this cost is spread across billions of individually tiny typing-speed differentials rather than concentrated on any identifiable group. Switching layouts individually is possible but costly in retraining time, and offers no benefit unless others switch too.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typists_and_general_users, payer,
    powerless, biographical, constrained, global).

% Manufacture what the installed base of typists demands, which is QWERTY, because that is what training, muscle memory, and existing hardware inventories already assume. They did not design this outcome; they respond to a demand curve shaped by decades of prior individual choices. They would readily manufacture an alternative layout at equal cost if the market coordinated on one, and several manufacturers have in fact sold Dvorak-compatible hardware without commercial success.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, incumbent_keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Designed and promoted layouts (Dvorak, Colemak) claiming ergonomic and speed advantages. Their proposals are structurally locked out not by suppression but by the coordination problem itself: no individual has an incentive to switch first, since the benefit of a superior layout depends on training availability, software support, and peer adoption all moving together. They are excluded from the conversation by the collective-action problem, not by anyone's design.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_designers, excluded,
    moderate, biographical, constrained, global).

% Study whether QWERTY's persistence demonstrates market failure via network-effect lock-in (David 1985) or whether the efficiency gap itself is overstated (Liebowitz & Margolis 1990). This reading sides with the latter camp's structural claim: even granting some inefficiency, no actor engineered or profits disproportionately from the lock-in — it is the predictable mathematical result of a coordination problem with no natural re-coordination mechanism, not a captured rent.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: QWERTY solves the problem of universal typing compatibility: a single shared standard lets any typist use any keyboard, any manufacturer serve any market, and any typing-instruction material apply everywhere, without per-device or per-person negotiation.
% TRANSFER_FUNCTION: No identifiable transfer occurs. Whatever marginal typing-speed cost exists relative to an alternative layout is paid by each typist individually, in proportion to how much they type, and does not flow to any collecting party — it dissipates as diffuse, uncaptured inefficiency rather than moving from a payer class to a beneficiary class.
% ABSENT_VOICES: Alternative-layout designers and ergonomics researchers would object that the standard is suboptimal and that manufacturers underinvest in transition support. They are not silenced by any actor; they are absent because the coordination problem gives no single party the incentive to bear the switching cost alone.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight with no alternative standard emerging in its place, typists would simply re-coordinate on some layout through the same accidental process that produced QWERTY originally — likely re-converging on QWERTY itself given the surviving installed base of trained typists, muscle memory, and legacy hardware. Nothing is extracted by QWERTY's existence for anyone to lose; the arrangement is closer to a natural equilibrium of a coordination game than a maintained institution.
% FOUNDING_PROBLEM: Early typewriter mechanisms jammed when adjacent typebars were struck in quick succession; QWERTY's letter arrangement was selected substantially to slow rapid alternating strikes and separate common letter pairs, solving a mechanical-jamming problem that no longer exists in modern keyboards.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (David 1985; Diamond 1997, popularizing account) attest the original mechanical rationale is defunct. Liebowitz and Margolis (1990), writing from outside any keyboard-manufacturing interest, corroborate the status from the opposite direction: they argue the claimed inefficiency of the surviving standard was never rigorously established either, so there is neither a live founding problem nor a demonstrated ongoing cost that would motivate deliberate maintenance by any beneficiary.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_unchanged).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.08) and rising only marginally over the century-scale interval, reflecting that whatever cost exists is a byproduct of coordination failure rather than a maintained extraction channel — there is no mechanism by which the marginal inefficiency accrues disproportionately to any party. Suppression is authored very low (0.05): no active enforcement prevents individuals or firms from adopting alternative layouts; Dvorak keyboards are commercially available and always have been. Accessibility collapse is authored high (0.88) for a different reason than coercive suppression would produce: the collapse is a mathematical artifact of network effects — once the vast majority of typists and hardware are QWERTY-trained, the option set for any individual effectively collapses to 'use QWERTY' even though no one is blocking the alternative. Resistance is authored low (0.10): the small alternative-layout movement exists but does not constitute organized resistance to an extractive structure, because there is no extractive structure to resist, only a coordination trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent manufacturers) are declared not because they extract rents but to trigger honest FSM (false-summit) evaluation: a mountain claim with a declared beneficiary group must be checked against the possibility that 'technological inevitability' language is being used to launder an actually-engineered lock-in. This reading's position is that the check should clear the mountain reading, because the manufacturers' benefit is incidental (they'd happily sell an alternative layout if demand existed) rather than constructed (they did not create the demand structure). No victims are declared because the efficiency loss, if any, is diffuse and uncaptured rather than transferred to an identifiable payer class — this is the key structural delta from the strategic_lock_in_reading, which does declare victims (workers trained at cost into an inferior standard for manufacturer benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy to resolve in this reading: the founding problem (mechanical typebar jamming) is dead, but the arrangement did not need active maintenance to persist — it persists because re-coordination costs exceed the diffuse and contested benefit of switching, not because any institution defends a defunct mandate. This is structurally different from a piton (which requires an administrator with the power to change it but insufficient motive) — no single administrator holds that power here; the equilibrium is distributed across billions of independent typing and manufacturing decisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_natural_vs_constructed,
    'Is QWERTY''s persistence a genuine natural-law-like consequence of network effects and switching costs (mountain), or does it require ongoing institutional maintenance by manufacturers and typing-instruction bodies to prevent re-coordination on a superior standard (constructed lock-in benefiting identifiable parties)?',
    'Examine whether keyboard manufacturers, standards bodies, or typing-certification institutions have taken affirmative actions (lobbying, exclusive contracts, suppression of alternative-layout hardware, coordinated refusal to support alternatives) beyond simply meeting existing demand. Absence of such affirmative maintenance actions across the historical record supports the path-dependency reading; presence of them supports the sibling strategic_lock_in_reading.',
    'If affirmative maintenance actions are found, this reading''s mountain classification is undermined and the constraint should be reclassified toward tangled_rope or snare with the manufacturers as active beneficiaries rather than incidental ones — which is precisely the sibling reading''s claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_natural_vs_constructed, empirical, 'Whether QWERTY persistence requires active institutional maintenance or is a pure coordination-game equilibrium.').

omega_variable(
    efficiency_gap_magnitude_contested,
    'Is there in fact a meaningful typing-speed or ergonomic efficiency gap between QWERTY and alternative layouts (Dvorak, Colemak), or was the claimed gap overstated or fabricated by early Dvorak-layout advocates and patent holders (Liebowitz & Margolis''s central historiographical claim)?',
    'Independent, non-industry-funded controlled studies comparing typing speed and repetitive strain outcomes across large matched populations trained from scratch on each layout.',
    'If the gap is negligible or nonexistent, the extractiveness score authored here (0.08) is itself an overstatement and should be revised toward zero, strengthening the mountain classification. If the gap is substantial and well-documented, extractiveness should rise and the diffuse-externality framing becomes harder to sustain, though it would still not by itself establish a beneficiary who captures the loss.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_gap_magnitude_contested, empirical, 'Whether the QWERTY/Dvorak efficiency gap that motivates lock-in critiques is empirically real or a historiographical artifact.').

omega_variable(
    beneficiary_incidental_vs_captured,
    'Does declaring incumbent_keyboard_manufacturers as a beneficiary on a mountain-claimed constraint correctly capture ''incidental beneficiary of an accident'' rather than ''party actively capturing rent,'' given that FSM logic gates on beneficiary presence alone regardless of intent?',
    'Compare manufacturer profit margins and market concentration in the keyboard-hardware market against markets for comparable standardized peripherals with no lock-in history, to see whether QWERTY incumbency confers supernormal returns beyond ordinary competitive returns.',
    'If margins are ordinary (competitive), the incidental-beneficiary framing holds and the mountain classification, once FSM-checked, should survive. If margins are supernormal and correlate with QWERTY exclusivity contracts, the beneficiary declaration under-describes actual capture and the constraint should be reconsidered as closer to the sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_incidental_vs_captured, conceptual, 'Whether the declared beneficiary genuinely benefits incidentally or captures concentrated rent, which the FSM gate cannot itself distinguish.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 20, 0.02).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 40, 0.03).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 60, 0.03).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 80, 0.04).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% This constraint and qwerty_persistence_inevitability__strategic_lock_in_reading are the two readings of a single contested kernel: the historical fact of QWERTY's persistence despite alleged ergonomic inferiority. This reading (path_dependency) authors epsilon near 0.08, no victim set, and a mountain claim, treating persistence as an emergent coordination-game equilibrium. The sibling reading authors substantially higher epsilon, a declared victim set (retrained workers, locked-out innovators), and a tangled_rope or snare claim, treating persistence as manufacturer-and-institution-engineered lock-in. The two files must never be merged or averaged; they represent structurally incompatible causal claims about the same observable, linked here for contamination-propagation and comparative analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
