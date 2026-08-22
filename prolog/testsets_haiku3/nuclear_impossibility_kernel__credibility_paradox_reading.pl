% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Credibility Paradox: Deterrence Threat Stability
 *   domain: strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   Nuclear deterrence grounded itself in a logical paradox: for the threat
 *   of nuclear retaliation to deter adversaries, it must be credible (the
 *   threatened party must believe the threat would be carried out). But the
 *   threat is to use weapons that guarantee mutual annihilation, which is
 *   irrational—no rational actor would execute such a threat, so the threat
 *   is not credible. This reading of the nuclear kernel focuses on the
 *   paradox as a manageable logical problem: strategic doctrines layer
 *   escalation ladders, 'flexible response,' and extended deterrence
 *   commitments to try to restore credibility by creating intermediate rungs
 *   where small nuclear use might seem rational. But the paradox remains
 *   unresolved: no doctrine has successfully explained why any nuclear use
 *   would be rational once the escalation ladder is climbed. The constraint
 *   functions as a snare because great powers benefit from the freeze on
 *   great-power conventional war (the paradox makes nuclear use so incredible
 *   that conventional conflicts are capped at sub-threshold levels), while
 *   non-nuclear states, threshold states, and post-conflict polities pay
 *   through prolonged conventional vulnerability and inability to credibly
 *   threaten nuclear retaliation against conventionally superior
 *   nuclear-armed rivals. The extraction is maintained through active
 *   enforcement: regular crisis demonstrations (near-use incidents, force
 *   posture escalations, declaratory policy reminders) that keep the paradox
 *   performatively credible despite its logical instability.
 *
 * KEY AGENTS:
 *   - Status Quo Great Powers (US, Russia, China, UK, France): institutional power, civilizational horizon, trapped exit — set the paradox-management doctrine and extract strategic dominance through the freeze on great-power conventional war.
 *   - Non-Nuclear Allies (NATO, Japan, South Korea): powerful, generational horizon, constrained exit — receive security guarantees but pay through targeting risk and constrained independent policy choices.
 *   - Nuclear Threshold States (Iran, North Korea, India/Pakistan, others): moderate power, biographical/generational horizon, identity-locked exit — face credible conventional threat without credible nuclear deterrent; nationalism binds them to nuclear aspiration but international enforcement prevents acquisition.
 *   - Post-Conflict Reconstruction Polities (Syria, Crimea, Ukraine): powerless, immediate horizon, trapped exit — absorb conventional destruction for years while the paradox keeps great-power nuclear escalation frozen.
 *   - Strategic Planners / Nuclear Command: institutional power, biographical horizon, identity-locked exit — maintain the paradox operationally while aware of its logical instability; professional identity constituted through nuclear expertise makes questioning the paradox career-ending.
 *   - Disarmament Advocates: moderate power, civilizational horizon, constrained exit — excluded from deterrence decision-making; would eliminate the paradox by eliminating nuclear weapons; their exclusion is structural to deterrence management.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.68).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.72).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, snare).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Credibility Paradox: Deterrence Threat Stability").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, 'ceafc1e4-6599-40c3-bbb2-0b987007d30e').
narrative_ontology:cs_kernel_codification('ceafc1e4-6599-40c3-bbb2-0b987007d30e', formalized).
narrative_ontology:cs_authority_grounding('ceafc1e4-6599-40c3-bbb2-0b987007d30e', extraction).
narrative_ontology:cs_interpretation_layer_present('ceafc1e4-6599-40c3-bbb2-0b987007d30e').
narrative_ontology:cs_reading_relation('ceafc1e4-6599-40c3-bbb2-0b987007d30e', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('ceafc1e4-6599-40c3-bbb2-0b987007d30e', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('ceafc1e4-6599-40c3-bbb2-0b987007d30e', foundational, deterrence_logically_manageable_through_escalation_gradation).
narrative_ontology:cs_axiom_status(deterrence_logically_manageable_through_escalation_gradation, holdable).
narrative_ontology:cs_axiom_grounding('ceafc1e4-6599-40c3-bbb2-0b987007d30e', deterrence_logically_manageable_through_escalation_gradation, instrumental).
narrative_ontology:cs_axiom('ceafc1e4-6599-40c3-bbb2-0b987007d30e', secondary, intermediate_nuclear_use_can_be_made_rational_through_damage_limitation).
narrative_ontology:cs_axiom_status(intermediate_nuclear_use_can_be_made_rational_through_damage_limitation, holdable).
narrative_ontology:cs_axiom_grounding('ceafc1e4-6599-40c3-bbb2-0b987007d30e', intermediate_nuclear_use_can_be_made_rational_through_damage_limitation, empirically_contingent).
narrative_ontology:cs_reference_frame('ceafc1e4-6599-40c3-bbb2-0b987007d30e', credible_deterrent_posture).
narrative_ontology:cs_drift_state('ceafc1e4-6599-40c3-bbb2-0b987007d30e', contemporary_escalation_instability_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ceafc1e4-6599-40c3-bbb2-0b987007d30e', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, status_quo_great_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_threshold_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, post_conflict_reconstruction_polities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_allies).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, peer_great_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_allies).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_threshold_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, peer_great_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The nuclear armed permanent powers (US, Russia, China, UK, France) set and maintain the deterrence doctrine. They claim the credibility paradox is managed through 'extended deterrence' (alliance commitments to defend non-nuclear allies), 'flexible response' (graduated escalation postures), and 'second-strike stability' (survivable retaliatory capacity). They extract strategic dominance: the paradox itself becomes a tool—it freezes conventional conflicts at threshold levels, prevents rival great powers from conventional victory, and deters non-aligned actors from challenging their position. Their exit is trapped because abandoning nuclear deterrence signals weakness and triggers regional destabilization; they maintain the paradox performatively through force posture, declaratory policy, and periodic crises that re-demonstrate the threat's 'credibility' via near-escalation.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, status_quo_great_powers, agenda_setter,
    institutional, civilizational, trapped, global).

% NATO members, Japan, South Korea, and other extended-deterrence partners receive security guarantees backed by US nuclear commitment. They claim to benefit from the umbrella: it deters conventional attack without forcing them to develop nuclear weapons. But they also pay: they host nuclear bases, face targeting in any escalation, cannot credibly pursue independent security policies (the deterrent only works if they cannot easily exit), and live under threat of uncontrolled escalation if the paradox breaks. Their exit is constrained—leaving the alliance means losing security guarantees in a high-threat environment; staying means accepting both protection and the implicit risk of being drawn into great-power nuclear conflict.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_allies, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_allies, beneficiary).

% States like Iran, North Korea, and historically India/Pakistan occupy the threshold: technically capable of nuclear weapons, geopolitically pressured not to acquire them or to abandon them. They face credible conventional threat from nuclear-armed rivals (Iran from US/Israel, North Korea from US/South Korea) but cannot credibly use nuclear options in response because the paradox's enforcement machinery treats their use as civilizational violation, triggering international isolation, sanctions escalation, and possible preemptive strikes. Their identity-lock is profound: nationalism binds them to nuclear ambition as a prestige/sovereignty claim, but international deterrence structure treats nuclear acquisition as a destabilizing violation rather than a legitimate security choice. They bear costs of the paradox without its protective benefits.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_threshold_states, payer,
    moderate, biographical, identity_locked, regional).

% Polities emerging from conventional wars where nuclear-armed powers abstained from use (e.g., Crimea 2014, Syria 2011–, Ukraine 2022–) pay through prolonged conventional conflict at sub-nuclear threshold. The paradox's credibility problem creates space for nuclear-armed powers to conduct conventional wars against non-nuclear adversaries precisely because the nuclear threat is so incredible against equals that it cannot be deployed against inferiors without appearing absurdly disproportionate or utterly unhinged. These polities absorb conventional destruction for years while the paradox keeps great-power nuclear escalation frozen. Their exit is trapped: they cannot threaten nuclear retaliation (they have no nuclear weapons, and any acquisition attempt triggers the same enforcement that constrains threshold states), cannot appeal to international intervention (great powers prefer the paradox's stability to humanitarian intervention), and cannot unilaterally exit the conflict (military defeat or surrender is the only exit, both catastrophic).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, post_conflict_reconstruction_polities, payer,
    powerless, immediate, trapped, local).

% Military strategists, defense intellectuals, and nuclear command authorities who maintain the paradox operationally. They author doctrine, conduct exercises, and make go/no-go decisions during crises. Their structural position is paradoxical: they must maintain the belief that nuclear use is possible (credibility) while operating under the constraint that use is impossible (mutual destruction). Their identity-lock is acute: professional identity is constituted through nuclear expertise and command authority; questioning the paradox's core architecture is career-ending. They observe the system because their calculations cannot resolve its contradiction—they can only perform it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_planners_and_nuclear_command, observer,
    institutional, biographical, identity_locked, global).

% Non-governmental organizations, scientists, and some state parties to disarmament treaties (NPT, CTBT frameworks) argue the paradox proves nuclear weapons should be eliminated entirely. They would fundamentally alter the constraint by removing nuclear arsenals; their exclusion from deterrence decision-making is structural—deterrence is managed exclusively by the armed powers and their allies, while disarmament advocates are consulted only on the margins or shut out entirely from operational decisions. They are excluded because inclusion would require abandoning the paradox-management strategy and adopting vulnerability, which the armed powers will not accept.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, disarmament_advocates, excluded,
    moderate, civilizational, constrained, global).

% Russia and China occupy a specific position: they benefit from the paradox's freeze on conventional great-power war (it is as incredible for the US to use nuclear weapons against them as for them to use against the US), but they also pay through uncertainty about US declaratory policy (extended deterrence commitments to allies create multiple tripwires where escalation could occur), through the cost of maintaining credible second-strike capability in the face of US missile defense efforts, and through the constant shadow of the paradox breaking. Their position is symmetric with the US on the paradox's core structure but asymmetric on alliance networks and technological capability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, peer_great_powers, beneficiary,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, peer_great_powers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, status_quo_great_powers).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes great-power competition by creating a shared understanding that any nuclear use escalates to mutual annihilation, thereby making all parties prefer to contest via conventional means and diplomatic negotiation below the nuclear threshold. Nuclear forces coordinate all major powers on an implicit non-use bargain: maintain deployments, maintain credible second-strike, but agree (through demonstrated restraint) not to cross into actual use.
% TRANSFER_FUNCTION: Transfers strategic dominance from direct contest to freeze. Nuclear-armed great powers extract the ability to pursue regional objectives, suppress regional nuclear proliferation, and maintain military superiority in conventional conflicts, because non-nuclear and threshold-state adversaries cannot credibly threaten nuclear retaliation (the paradox makes such threats unbelievable). Non-nuclear states pay through prolonged conventional vulnerability and inability to match great-power security guarantees to their allies.
% ABSENT_VOICES: Disarmament advocates, non-aligned states arguing for elimination of nuclear weapons, and threshold states claiming the right to nuclear deterrence are excluded from deterrence doctrine-setting. They would argue the paradox proves nuclear weapons are an illegitimate strategic asset masquerading as deterrent, and that the constraint should be abolished entirely. Their exclusion is structural: deterrence doctrine is set exclusively by nuclear-armed powers and their allies, with other parties consulted only on marginal matters.
% DISAPPEARANCE_RATIONALE: If the credibility paradox as a stabilizing mechanism disappeared overnight, great powers would immediately face a choice: either resolve the paradox by accepting nuclear use as rational (triggering re-armament, force-posture escalation, lowered use thresholds, and proliferation), or adopt a different stabilization mechanism (verified disarmament, conventional force balancing, or unilateral hegemony). The current freeze on great-power conventional war would evaporate within years; regional conflicts would no longer be implicitly capped at sub-nuclear threshold; proliferation would accelerate. The international security order depends on the paradox remaining unresolved and performatively managed.
% FOUNDING_PROBLEM: The discovery of nuclear mutual annihilation created a logical paradox: deterrence requires credible threat of use, but use guarantees mutual destruction, making the threat incredible. Early Cold War strategists responded by proposing escalation ladders and flexible response—the idea that credibility could be restored by making small nuclear use seem rational at some intermediate threshold. The founding problem was: can we make the incredible credible through strategic doctrine?
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners across all nuclear powers continue to operate as if solving the credibility problem is the core deterrence challenge. Recent strategic reviews (U.S. Nuclear Posture Review, Russian military doctrine, Chinese strategic writings) continue to propose escalation-control mechanisms and flexible response doctrine as means to restore credibility. The fact that no solution has been found after 80 years—despite enormous resources and the best strategic minds—is itself the corroboration: every power maintains the same paradox-managing doctrine, suggesting the founding problem persists as unsolved and unresolvable. Historical scholarship from outside the benefiting state apparatus (Sagan, Jervis, Accinelli, Payne) documents that no resolution has been achieved; the paradox is managed through performance and doctrine layering, not through logical solution. This corroboration comes from strategic analysts, historians, and theorists who are not primary beneficiaries of the current deterrent structure.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (high) because the constraint produces asymmetric strategic dominance: nuclear-armed powers extract the ability to conduct conventional wars and pursue regional objectives while preventing nuclear retaliation and capping escalation. Non-nuclear and threshold states cannot use nuclear deterrence against conventionally superior nuclear-armed neighbors, facing either prolonged conventional vulnerability or the catastrophic institutional/legal cost of breaking the non-proliferation taboo. The measurement series (0.48→0.68 over 80 years) shows rising extractiveness, matching the historical pattern: early Cold War saw more-plausible escalation pathways and greater symmetry in uncertainty; as doctrine matured and both sides demonstrated restraint despite crises, the paradox became entrenched and the freeze more complete, allowing status-quo powers to exercise conventional dominance with less risk of threshold-crossing. Suppression is high (0.72) because the constraint persists through active enforcement: force-posture changes, declaratory policy, crisis exercises, and near-use incidents that maintain the implicit bargain on non-use. Theater ratio is high (0.58) and rising, indicating that increasing shares of deterrence activity are performative rather than functional—the paradox itself has not been resolved, so doctrine layering has become more theatrical and less explanatory. Accessibility collapse is relatively low (0.42) because alternatives exist at the margins (disarmament, verified decoupling, conventional deterrence), even though they are politically blocked; the paradox is not a natural law that forecloses all alternatives, but a constructed freeze maintained through enforcement. Resistance is high (0.71) because threshold states and disarmament advocates actively resist the constraint's legitimacy, and even great powers experience internal tension between the paradox's logical incoherence and the operational necessity of maintaining it.
 *
 * PERSPECTIVAL GAP:
 *   From the status-quo-great-powers seat, the constraint appears as genuine coordination: nuclear deterrence solved the problem of great-power war (no peer war has occurred since 1945) and stabilized the international system through mutual vulnerability and credible second-strike capability. The paradox is managed through layered doctrine and flexible response, which they believe makes intermediate nuclear use rational at some threshold (limited counterforce strikes, escalation control). From the non-nuclear-ally seat, the constraint appears as protection with hidden costs: extended deterrence is beneficial (no Soviet invasion of Western Europe occurred), but the beneficiary is aware of the paradox's incompleteness—if the US is unwilling to actually use nuclear weapons (because it is irrational), then the extended deterrence guarantee is less credible than stated. From the threshold-state and victim seats, the constraint appears as pure extraction: they are denied nuclear deterrence (through NPT enforcement, sanctions, and international delegitimization) while remaining vulnerable to nuclear-armed neighbors' conventional superiority, and they observe great powers benefiting from the freeze on peer conflict while experiencing prolonged conventional vulnerability themselves. The engine should compute these differences in directionality: status-quo powers sit near d=0.0 (beneficiaries), non-nuclear allies near d=0.3-0.5 (mixed), threshold and victim states near d=0.8-1.0 (targets). These divergences arise from the structural data—beneficiary/victim declarations, power atoms, and exit options—not from authored classification of the constraint's type, which is computed by the engine from the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Status quo great powers are the primary beneficiaries (d ≈ 0.0-0.2): they extract strategic dominance, set the rules, and maintain the apparatus. Their exit is nominally 'trapped' (cannot abandon nuclear deterrence without triggering destabilization and losing strategic advantage), but this is a trapped exit among powerful institutional actors with resources and alliances to absorb disruption—it is trapped by choice and sunk cost, not by powerlessness. Non-nuclear allies are secondary beneficiaries-payers (d ≈ 0.3-0.5): they receive security guarantees (benefit) but cannot independently pursue security policy and face targeting risk in any escalation (cost). Their exit is constrained: leaving the alliance means losing security guarantees, but staying means accepting risk and dependence. Threshold states are primary targets (d ≈ 0.75-0.9): they are denied nuclear deterrence through international enforcement while facing conventional threat, and their identity-lock (nationalism binds them to nuclear aspiration) makes exit impossible without psychological/political revolution. Their suppression includes both structural (legal/treaty barriers, sanctions threat) and internalized (belief that nuclear acquisition would trigger international punishment greater than the benefit of deterrence). Post-conflict polities are the most severely targeted (d ≈ 0.95): they are powerless, have immediate time horizons where conventional destruction is catastrophic, and their exit is trapped—they cannot escalate to nuclear level (not nuclear-armed, and escalation would trigger great-power intervention), cannot exit the conflict through military victory (conventionally outmatched), and cannot appeal for intervention (great powers prefer the paradox's stability). Disarmament advocates are excluded rather than targeted (they are not playing the game, so directionality does not apply in the same way), but if they were included in the directionality calculus, they would be targets of suppression for challenging the foundation of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was: how do we maintain strategic deterrence when use is irrational? The status quo great powers claim the problem is still live—they maintain full nuclear forces, declaratory policies of extended deterrence, and crisis-response postures as evidence that the paradox remains the binding strategic constraint. But the rising theater ratio (0.42 to 0.58 over 80 years) and the persistence of the paradox despite 80 years of strategic research suggests the founding problem may be shifting toward 'dead' status: the paradox was never resolved, only performatively managed through doctrine layering and institutional habit. If the paradox cannot be resolved (as the credibility reading asserts), and if doctrine layering has failed to make nuclear use credible (as the data suggests), then the founding problem of 'how to restore credibility' is unsolvable and therefore effectively dead—the constraint persists not because the problem is live, but because abandoning the constraint is politically and institutionally costly. This creates a mandatrophy risk: the constraint extracts strategic dominance for status-quo powers (who would lose that advantage if nuclear deterrence were abandoned or if nuclear weapons proliferated to match their levels), but the mechanism for extracting that dominance (credible deterrent threat) is logically unstable and performatively maintained at rising cost. If the theater ratio continues to rise (approaching 0.70-0.80), the constraint reclassifies toward piton: a doctrine maintained by institutional inertia, sunk costs, and identity-fusion (strategic planners whose professional identity is constituted through nuclear expertise) rather than by functional deterrent effect. The mandatrophy verdict depends on whether the founding problem is truly dead or merely contested—if dead, the constraint becomes a case study in institutional persistence of logically incoherent arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_paradox_vs_structural_contraction_kernel_reading,
    'Is nuclear deterrence held together by a manageable logical paradox (credibility reading: this story) or an unmovable physical fact (structural_contraction reading)?',
    'The readings differ on what makes war incredible. Credibility reading: doctrine layering and escalation ladders can make small nuclear use rational, thereby making the threat credible, thereby maintaining deterrence. Structural reading: no doctrine makes nuclear use rational because mutual annihilation is guaranteed; the threat is incredible by physics, not logic, and deterrence persists despite logical incoherence. Test: does any strategic innovation restore credibility (missile defense, counterforce accuracy, damage limitation)? If yes, credibility reading is more accurate. If innovations are pursued but fail to restore credibility, structural reading gains force.',
    'If structural_contraction_reading displaces this reading, the constraint reclassifies from snare (manageable through enforced credible threat) to piton (maintained by institutional inertia while the functional mechanism has failed). Beneficiaries would shift: status-quo powers would be revealed as captive administrators of a failed system rather than strategic beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credibility_paradox_vs_structural_contraction_kernel_reading, conceptual, 'Whether the paradox is a resolvable logical problem or an unsolvable physical fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(nucl_tr_t40, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement(nucl_tr_t60, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 60, 0.57).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 80, 0.58).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(nucl_be_t40, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(nucl_be_t60, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(nucl_su_t40, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(nucl_su_t60, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(nucl_su_t80, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__credibility_paradox_reading, 0.18).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, non_proliferation_treaty_enforcement).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_alliance_commitment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nuclear_impossibility_kernel kernel. The kernel constrains all three readings (credibility_paradox_reading, structural_contraction_reading, rational_dropout_reading); they share the same artifact (nuclear weapons deterrence) but attribute different structural causation to the same observable (great-power war has not occurred since 1945 despite multiple crises). The credibility_paradox_reading views the freeze as dependent on continuous institutional maintenance of an incredible threat through doctrine layering. The structural_contraction_reading views it as dependent on brute physical fact (mutual annihilation). The rational_dropout_reading views it as dependent on cost-benefit calculation independent of threat credibility. Each reading has distinct ε (extractiveness), beneficiary structure, and stability properties. See commentary.kernel_context for the full reading taxonomy and sibling relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, institutional, 0.15).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
