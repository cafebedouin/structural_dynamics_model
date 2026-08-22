% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Weapons as Rational-Choice Dropout Constraint
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the rational-dropout reading of the nuclear
 *   impossibility kernel. The reading asserts that nuclear weapons created a
 *   rational-choice problem: war between nuclear-armed states remains
 *   structurally possible (victory could theoretically be achieved through
 *   superior force application) but its costs (mutual destruction,
 *   civilization-ending harm) exceed any conceivable benefit, making war
 *   rational to avoid. This reading differs from the
 *   structural_contraction_reading (which asserts victory is physically
 *   impossible) and the credibility_paradox_reading (which asserts the
 *   deterrent is inherently non-credible). The rational-dropout reading keeps
 *   victory in the reachable set but drops it from the active choice set
 *   through cost-benefit reasoning. Strategic behavior reflects this
 *   constraint: peer nuclear powers refrain from direct large-scale conflict
 *   not because war is impossible but because it is rationally dominated. The
 *   constraint persists through active maintenance: doctrine refinement,
 *   verification regimes, strategic signaling, and institutional enforcement
 *   that keeps the cost-benefit framing stable.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: institutional power; maintain the strategic framework; trapped by mutual vulnerability
 *   - deterrence_doctrine_custodians: institutional power; provide legitimacy and professional authority for the rational-choice framing; beneficiaries of the constraint's persistence
 *   - non_nuclear_states: organized power; excluded from doctrine-setting; pay the cost through strategic dependence
 *   - civilian_populations: powerless; trapped; bear existential risk and psychological burden
 *   - rising_peer_competitors: powerful but excluded; locked out of doctrine negotiation; would contest the framing's applicability to their situation
 *   - disarmament_advocates: moderate power; excluded; challenge the cost-benefit framing itself rather than the numbers
 *   - strategic_analysts: analytical seat; measure whether the constraint actually guides behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.68).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.72).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Weapons as Rational-Choice Dropout Constraint").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'fb8ac645-b03a-4b1f-829a-52fcfc356b95').
narrative_ontology:cs_kernel_codification('fb8ac645-b03a-4b1f-829a-52fcfc356b95', distributed).
narrative_ontology:cs_authority_grounding('fb8ac645-b03a-4b1f-829a-52fcfc356b95', extraction).
narrative_ontology:cs_interpretation_layer_present('fb8ac645-b03a-4b1f-829a-52fcfc356b95').
narrative_ontology:cs_reading_relation('fb8ac645-b03a-4b1f-829a-52fcfc356b95', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb8ac645-b03a-4b1f-829a-52fcfc356b95', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('fb8ac645-b03a-4b1f-829a-52fcfc356b95', foundational, victory_rationally_dominated_by_mutual_destruction).
narrative_ontology:cs_axiom_status(victory_rationally_dominated_by_mutual_destruction, holdable).
narrative_ontology:cs_axiom_grounding('fb8ac645-b03a-4b1f-829a-52fcfc356b95', victory_rationally_dominated_by_mutual_destruction, empirically_contingent).
narrative_ontology:cs_axiom('fb8ac645-b03a-4b1f-829a-52fcfc356b95', foundational, rational_actor_deterrence_model).
narrative_ontology:cs_axiom_status(rational_actor_deterrence_model, holdable).
narrative_ontology:cs_axiom_grounding('fb8ac645-b03a-4b1f-829a-52fcfc356b95', rational_actor_deterrence_model, empirically_contingent).
narrative_ontology:cs_reference_frame('fb8ac645-b03a-4b1f-829a-52fcfc356b95', rational_cost_benefit_deterrence).
narrative_ontology:cs_drift_state('fb8ac645-b03a-4b1f-829a-52fcfc356b95', contemporary_proliferation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb8ac645-b03a-4b1f-829a-52fcfc356b95', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_doctrine_custodians).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_nuclear_umbrella).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_nuclear_umbrella).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, strategic_stability_through_mutual_vulnerability).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_model_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and the deterrence doctrine that justifies them. They set the strategic framework by controlling verification regimes, doctrine development, and institutional legitimacy. They are trapped by mutual vulnerability: abandoning the arsenal would expose them to existential risk from peer competitors' arsenals. Their strategic authority depends on the rational-dropout framing — if victory became possible again, their deterrence narrative would collapse.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapons_states, agenda_setter,
    institutional, generational, trapped, global).

% Strategic analysts, military planners, policy intellectuals, and academic researchers whose professional authority and career advancement depend on the rational-choice framing. They produce doctrine, author strategy papers, brief policymakers, and provide intellectual legitimacy for nuclear postures. The constraint vindicates their framework; departure from the rational-dropout reading would require fundamental retraining and reputation recalibration.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_doctrine_custodians, beneficiary,
    institutional, generational, constrained, global).

% Live within the security architecture established by the nuclear deterrent. They depend on extended deterrence from nuclear-armed allies or exist in mutual-deterrence relationships with peer non-nuclear states while nuclear powers overshadow the region. They pay through strategic dependence (cannot pursue independent security policies), resource transfer (military aid, security commitments), and reduced conventional deterrence (because nuclear powers allocate military resources to arsenals).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, payer,
    organized, biographical, constrained, global).

% Benefit from the rational-dropout constraint's prevention of large-scale conventional war between nuclear powers (which would devastate their territories and societies). They pay through psychological burden (knowledge of existential risk), resource opportunity cost (military spending at the expense of public goods), and vulnerability to any scenario where the deterrent fails or is perceived as non-credible. They have no voice in the strategic framework's maintenance or revision.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_nuclear_umbrella, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_nuclear_umbrella, beneficiary).

% States with developing or emerging nuclear arsenals (e.g., India, Pakistan, Iran as aspirant) face the same rational-dropout logic but are excluded from doctrine-setting institutions. They would contest whether the rational-choice threshold applies to their security situation (arguing their arsenals are existential necessities, not deterrence luxuries). Their exclusion from strategic dialogue enforces the constraint by preventing alternative cost-benefit framings from gaining institutional voice.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, rising_peer_competitors, excluded,
    powerful, generational, trapped, global).

% Non-governmental organizations, academics, and activists arguing that nuclear weapons are categorically impermissible and that disarmament is the only moral and rational course. They are excluded from doctrine-setting institutions and treated as outside the strategic rationality frame. Their position challenges not just the cost-benefit numbers but the framing itself — they argue war with nuclear weapons is not a rational choice but a categorical wrong.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% External observers measuring whether the rational-choice framework actually predicts state behavior and whether alternative framings (structural contraction, credibility paradox) better fit the evidence. They claim epistemic authority but lack institutional power to set doctrine. Their role is to track the constraint's mechanism and test whether the rational-dropout reading is causally accurate.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic behavior of nuclear-armed states by establishing a shared cost-benefit framework: war between peer nuclear powers is structurally rational to avoid because its costs (mutual annihilation or unacceptable harm) exceed any benefit. Enables a stable equilibrium where states refrain from large-scale direct conflict without needing explicit agreements or trust.
% TRANSFER_FUNCTION: Transfers strategic vulnerability from nuclear-armed states (who possess destructive capability but face mutual destruction) to non-nuclear states and populations (who depend on the deterrent holding and bear the burden of the doctrine's maintenance costs). Nuclear weapons states collect the benefit of perceived security; non-nuclear states pay through strategic dependence, military expenditure, and psychological exposure.
% ABSENT_VOICES: Rising peer competitors with developing arsenals are excluded; they would contest whether the rational-dropout framework applies to their situation and argue for different cost-benefit accounting. Disarmament advocates are excluded; they challenge the cost-benefit framing itself and argue categorical impermissibility transcends calculation. Their absence from doctrine-setting institutions is enforced and maintained by the same powers whose security the constraint protects.
% DISAPPEARANCE_RATIONALE: If nuclear weapons vanished or were rendered non-functional, the rational-dropout constraint would evaporate immediately. Large-scale conventional war between former nuclear powers would become structurally possible again (cost-benefit would favor it or make it a live option). Military postures would shift, alliances would reorganize, and regional powers would recalculate security strategies without the nuclear overhang. Strategic doctrine would require wholesale reinterpretation.
% FOUNDING_PROBLEM: After nuclear weapons emerged and made mutual destruction possible, strategists faced the problem: how do you maintain deterrence and strategic stability when the cost of failure is civilization-ending? The rational-choice response: establish a framework where war itself becomes rational to avoid, independent of deterrent credibility or intentions.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear powers attest the founding problem is live and the rational-dropout framing is the solution that has kept them from major-power war for 80 years. Disarmament analysts attest the founding problem is mis-stated: the actual problem is that rationality cannot be trusted to prevent use, so categorical prohibition is necessary. Historians and military analysts outside the benefiting states attest the constraint's contribution to post-1945 peace is real but contested — alternative explanations (bipolarity itself, conventional deterrence, economic interdependence, reduced great-power contact) compete for credit.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects that nuclear-armed states extract strategic advantage and institutional authority from the rational-dropout framing. Non-nuclear states pay through dependence and constrained choice. Suppression (0.72) is high because the constraint requires active enforcement: doctrine custodians must suppress alternative framings (structural contraction, credibility paradox), rising powers must be kept from contesting the threshold, and disarmament advocates must be excluded from legitimacy. Theater_ratio (0.44 and rising) indicates that institutional maintenance activity (arms control negotiations, doctrine refinement, strategic signaling) increasingly performs an identity/legitimacy function alongside its real strategic role. Accessibility_collapse increases from 0.42 (structural, 1945) to 0.78 (structural, 2025) as the constraint becomes more deeply embedded in institutional practice — alternatives become harder to conceive within the dominant strategic vocabulary. The coercion grid shows that at the structural level, the constraint operates through hard institutions (deterrence doctrine, arsenal maintenance, verification); at the individual level, the suppression is more diffuse (psychological acceptance, narrative normalization). Rising resistance at the individual and class levels (0.45 and 0.58 in 2025) reflects disarmament movements and non-aligned-state skepticism, though resistance remains lower than at the structural level where it is actively suppressed by institutional machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapons-state seat, the constraint is a solution to an existential coordination problem: it enables peaceful coexistence with peer competitors despite mutual destructive capability. From the non-nuclear-state seat, the same constraint is a structure of dependence and vulnerability: their security depends on a framework they did not author and cannot exit. From the strategic-analyst seat, the constraint's necessity is itself contested — whether rational-choice dropout, structural impossibility, or credibility paradox better explains observed non-use is an open empirical question. The engine computes these divergences from the structural data (power, exit_options, beneficiary/victim status); the claim remains independent: this constraint is CLAIMED as tangled_rope (coordination function is real, asymmetric extraction is real, enforcement is active), and the metrics support that claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states are agenda-setters (d near 1.0 on the target side, reversed: they set the terms but are also trapped by mutual vulnerability — d closer to 0.5 symmetric due to identity-lock). Deterrence custodians are beneficiaries (d near 0.0 beneficiary side). Non-nuclear states are payers (d near 1.0 target side, constrained exit). Civilian populations are dual-positioned: they benefit from the dropout logic (prevented from large-scale conventional war) but pay the cost of the arrangement (psychological burden, resource opportunity cost, existential risk). The identity-lock on nuclear weapons states is profound: their national security identity and institutional structures have fused with the deterrent logic; exit from the rational-dropout framework would require rethinking their entire security posture. This identity-fusion is documented in an omega variable below.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain deterrence and stability given mutual destruction capability) is contested in its status: nuclear powers attest it is live and being solved; disarmament advocates attest it is mis-stated (the real problem is that rationality cannot prevent use). The disappearance_verdict is world_rearranges: if the constraint vanished, strategic behavior would reorganize immediately. This is consistent with a tangled_rope: the coordination function (rational avoidance of mutually catastrophic war) is real, and the asymmetric extraction (nuclear powers collect strategic advantage, non-nuclear states pay through dependence) is also real. The constraint is not mandatropic: it serves both an active coordination function and an extractive function simultaneously, and the coordination function would not persist without enforcement (the asymmetric benefit to the nuclear powers). If the constraint became purely performative (coordination ceased, enforcement-only remained), it would transition to a snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_actor_assumption,
    'Do state actors in nuclear crises actually reason according to cost-benefit rational-choice logic, or do cognitive biases, signaling imperatives, and institutional inertia dominate decision-making?',
    'Behavioral analysis of historical nuclear crises (Cuban Missile Crisis, Kargil, Taiwan Strait standoffs): do state decisions track the cost-benefit calculus or deviate systematically? Experimental and archival evidence on decision-maker cognition.',
    'If states reason rationally as the constraint assumes, the rational-dropout framing is accurate and the constraint operates as described. If rational-choice reasoning is systematically displaced by bias and institutional factors, the constraint''s causal mechanism is misidentified — the actual mechanism might be structural contraction, path dependence, or other non-rational factors. Classification would shift from tangled_rope toward snare (institutional actors captured by doctrine) or piton (inertial non-use without rational foundation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption, empirical, 'Whether nuclear-armed states actually use rational-choice reasoning in strategic decisions.').

omega_variable(
    structural_vs_rational_mechanism,
    'Does non-use between nuclear powers result from the rational-dropout mechanism (cost-benefit reasoning) or from structural contraction (physical impossibility making war irrational at the margins)?',
    'Hypothetical contingency: if military technology reduced the risk of mutual assured destruction (e.g., perfect missile defense), would states begin planning for large-scale nuclear war, or would they persist in non-use because of the rational-dropout frame? Alternatively: if a state acquired a first-strike-disarming capability, would the rational-dropout constraint fail?',
    'If the constraint depends on rational-choice dropout, it is vulnerable to technological change that alters the cost-benefit calculus or to strategic innovations that reduce mutual vulnerability. If the constraint actually depends on structural impossibility (victory is categorically unavailable), it would persist even if the rational-choice framing failed. Classification implications: rational-dropout alone is extractive (asymmetric benefit to nuclear powers); combined with structural contraction, the constraint becomes more durable and less extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_vs_rational_mechanism, conceptual, 'Whether the mechanism of non-use is rational-choice dropout or structural impossibility.').

omega_variable(
    identity_lock_on_deterrence_doctrine,
    'To what degree is the deterrence doctrine institutionally entrenched versus conceptually open to revision by the strategic community?',
    'Post-signaling from disarmament advocates and alternative strategic frameworks (e.g., no-first-use, graduated deterrence, resilience-based security): are these treated as genuinely alternative framings or as categorically outside serious strategy? Institutional evolution in strategic studies: do new frameworks gain institutional foothold or remain marginalized?',
    'High identity-lock means the constraint is increasingly performative and resistant to contestation (higher theater_ratio, suppression approaching 1.0). The suppression of alternative readings would escalate, and the constraint would trend toward snare or piton (power maintained by exclusion rather than active coordination). Low identity-lock would mean the rational-dropout framing is genuinely revisable if evidence shifts or new strategic circumstances emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_on_deterrence_doctrine, empirical, 'Degree of institutional and cognitive entrenchment of the rational-dropout framing among strategic elites.').

omega_variable(
    kernel_alternative_framings,
    'This reading instantiates rational-choice dropout; what if the actual mechanism of non-use between nuclear powers is one of the sibling readings — structural contraction (physical impossibility) or credibility paradox (the deterrent is non-credible)?',
    'Empirical differentiation via strategic discourse analysis: which reading do policymakers and strategists invoke to justify non-use? Via behavioral analysis: do states behave as if victory is cost-prohibitive (rational-dropout) or as if it is impossible (structural-contraction)? Via institutional analysis: does doctrine treat nuclear war as strategically unthinkable (supports rational-dropout) or as categorically impermissible (supports credibility-paradox or structural-contraction readings)?',
    'If structural-contraction is the actual mechanism, the constraint operates from a different causal foundation and is more robust to cost-benefit shifts. If credibility-paradox is operative, the constraint is fragile and depends on the perpetual non-credibility of threats. Classification shifts accordingly: rational-dropout tangled_rope → structural-contraction rope (genuine coordination, minimal extraction) or structural-contraction mountain (physical law, zero extraction); rational-dropout → credibility-paradox snare (extraction without real coordination, theater-dominant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_alternative_framings, conceptual, 'Which kernel reading actually explains non-use: rational-dropout, structural-contraction, or credibility-paradox.').

omega_variable(
    suppression_mechanism,
    'Is the suppression of alternative readings (structural contraction, credibility paradox, disarmament advocacy) structural (institutional exclusion, power imbalance, resource control) or internalized (strategic elites believe rational-dropout is the only coherent framework)?',
    'Post-exit suppression trajectory: if institutional barriers to alternative framings were removed (e.g., funding diverted to disarmament research, doctrine-setting opened to non-aligned voices), would the rational-dropout reading persist in strategic discourse? If yes, suppression is partly internalized; if no, suppression is structural.',
    'High structural suppression means the constraint''s persistence depends on active enforcement and power asymmetries; it would be categorized as snare-like or piton-like (maintained by exclusion). High internalized suppression means the constraint has absorbed into strategic cognition; exit from the framing is psychologically difficult even absent external barriers. This would make the constraint more extractive and more resistant to contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Whether suppression of alternative readings is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(nucl_tr_t1945, projected).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.38).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1979, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1979, 0.41).
narrative_ontology:measurement_basis(nucl_tr_t1979, observed).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.42).
narrative_ontology:measurement_basis(nucl_tr_t1991, observed).
narrative_ontology:measurement(nucl_tr_t2008, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2008, 0.43).
narrative_ontology:measurement_basis(nucl_tr_t2008, observed).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(nucl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(nucl_be_t1945, projected).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.58).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1979, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1979, 0.64).
narrative_ontology:measurement_basis(nucl_be_t1979, observed).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.61).
narrative_ontology:measurement_basis(nucl_be_t1991, observed).
narrative_ontology:measurement(nucl_be_t2008, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement_basis(nucl_be_t2008, observed).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(nucl_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(nucl_su_t1945, projected).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement_basis(nucl_su_t1962, observed).
narrative_ontology:measurement(nucl_su_t1979, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1979, 0.71).
narrative_ontology:measurement_basis(nucl_su_t1979, observed).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.69).
narrative_ontology:measurement_basis(nucl_su_t1991, observed).
narrative_ontology:measurement(nucl_su_t2008, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement_basis(nucl_su_t2008, observed).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(nucl_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2025
narrative_ontology:measurement(nucl_grid_01, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(class), 1945, 0.28).
narrative_ontology:measurement(nucl_grid_02, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(class), 2025, 0.68).
narrative_ontology:measurement(nucl_grid_03, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(individual), 1945, 0.15).
narrative_ontology:measurement(nucl_grid_04, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(individual), 2025, 0.62).
narrative_ontology:measurement(nucl_grid_05, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(organizational), 1945, 0.38).
narrative_ontology:measurement(nucl_grid_06, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(organizational), 2025, 0.72).
narrative_ontology:measurement(nucl_grid_07, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(structural), 1945, 0.42).
narrative_ontology:measurement(nucl_grid_08, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(structural), 2025, 0.78).
narrative_ontology:measurement(nucl_grid_09, nuclear_impossibility_kernel__rational_dropout_reading, resistance(class), 1945, 0.38).
narrative_ontology:measurement(nucl_grid_10, nuclear_impossibility_kernel__rational_dropout_reading, resistance(class), 2025, 0.58).
narrative_ontology:measurement(nucl_grid_11, nuclear_impossibility_kernel__rational_dropout_reading, resistance(individual), 1945, 0.22).
narrative_ontology:measurement(nucl_grid_12, nuclear_impossibility_kernel__rational_dropout_reading, resistance(individual), 2025, 0.45).
narrative_ontology:measurement(nucl_grid_13, nuclear_impossibility_kernel__rational_dropout_reading, resistance(organizational), 1945, 0.48).
narrative_ontology:measurement(nucl_grid_14, nuclear_impossibility_kernel__rational_dropout_reading, resistance(organizational), 2025, 0.62).
narrative_ontology:measurement(nucl_grid_15, nuclear_impossibility_kernel__rational_dropout_reading, resistance(structural), 1945, 0.42).
narrative_ontology:measurement(nucl_grid_16, nuclear_impossibility_kernel__rational_dropout_reading, resistance(structural), 2025, 0.55).
narrative_ontology:measurement(nucl_grid_17, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(class), 1945, 0.35).
narrative_ontology:measurement(nucl_grid_18, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(class), 2025, 0.74).
narrative_ontology:measurement(nucl_grid_19, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(individual), 1945, 0.22).
narrative_ontology:measurement(nucl_grid_20, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(individual), 2025, 0.68).
narrative_ontology:measurement(nucl_grid_21, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(organizational), 1945, 0.52).
narrative_ontology:measurement(nucl_grid_22, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(organizational), 2025, 0.79).
narrative_ontology:measurement(nucl_grid_23, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(structural), 1945, 0.48).
narrative_ontology:measurement(nucl_grid_24, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(structural), 2025, 0.81).
narrative_ontology:measurement(nucl_grid_25, nuclear_impossibility_kernel__rational_dropout_reading, suppression(class), 1945, 0.28).
narrative_ontology:measurement(nucl_grid_26, nuclear_impossibility_kernel__rational_dropout_reading, suppression(class), 2025, 0.68).
narrative_ontology:measurement(nucl_grid_27, nuclear_impossibility_kernel__rational_dropout_reading, suppression(individual), 1945, 0.18).
narrative_ontology:measurement(nucl_grid_28, nuclear_impossibility_kernel__rational_dropout_reading, suppression(individual), 2025, 0.62).
narrative_ontology:measurement(nucl_grid_29, nuclear_impossibility_kernel__rational_dropout_reading, suppression(organizational), 1945, 0.35).
narrative_ontology:measurement(nucl_grid_30, nuclear_impossibility_kernel__rational_dropout_reading, suppression(organizational), 2025, 0.71).
narrative_ontology:measurement(nucl_grid_31, nuclear_impossibility_kernel__rational_dropout_reading, suppression(structural), 1945, 0.38).
narrative_ontology:measurement(nucl_grid_32, nuclear_impossibility_kernel__rational_dropout_reading, suppression(structural), 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_stability_coordination).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, arms_control_verification_regime).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel is a contested kernel with three readings, each structurally distinct in their causal mechanisms and classifications. This story instantiates the rational_dropout_reading: victory remains possible but is rational to avoid due to cost-benefit reasoning. Sibling readings (structural_contraction and credibility_paradox) instantiate alternative mechanisms with different ε values and classification outcomes. The three readings coexist in strategic discourse and are held by different factions of the strategic community. Decomposition was necessary because ε varies across readings: rational-dropout has moderate-high extractiveness (0.68) due to asymmetric benefit to nuclear powers; structural-contraction would have lower ε (physical law, not institutional arrangement); credibility-paradox would have higher ε (pure extraction without real coordination). Each reading forms its own constraint story; they are linked via network.affects_constraints to show the kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
