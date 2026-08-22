% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb: Legitimacy Gained by Fringe Practices Ascending Through Demonstrated Superiority
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the endogenous-climb reading of the
 *   state-commitment-installation kernel: a marginal practice — a bookkeeping
 *   method, a dispute procedure, a logistics innovation — originates at an
 *   institutional fringe with no formal authority behind it, is adopted
 *   piecemeal by intermediate institutions betting on relative advantage, and
 *   climbs toward the apex as its comparative superiority becomes visible
 *   across successive comparisons. Legitimacy accrues from the bottom up,
 *   contingent on demonstrated performance rather than declared authority.
 *   The apex initially resists (its own incumbent practices and personnel are
 *   displaced) but eventually absorbs the climbing practice, at which point
 *   it is re-issued as an apex commitment and its fringe origins are often
 *   minimized in the official record. This is one of three readings of the
 *   same kernel: the exogenous_imposition_reading holds that legitimacy is
 *   installed top-down by an authority with transformation mandate, and the
 *   hybrid_cascade_reading holds that commitments are installed at the apex
 *   first but require fringe validation to stabilize. Each reading names a
 *   structurally distinct mechanism with its own beneficiary/victim set and
 *   its own epsilon; they are not three measurements of one mechanism but
 *   three different mechanisms competing for the same historical episodes.
 *
 * KEY AGENTS:
 *   - fringe_practice_originators: primary beneficiary if the climb succeeds (powerless/constrained) — bears the risk of total non-credit if it fails
 *   - early_adopting_intermediate_institutions: secondary beneficiary (moderate/mobile) — conduits carrying the practice upward, exposed if the bet fails
 *   - apex_incumbents: eventual payer and eventual re-beneficiary (institutional/constrained) — resists then absorbs
 *   - displaced_incumbent_practitioners: primary victim (moderate/trapped) — skills depreciate as the practice climbs past them
 *   - rival_fringe_traditions_that_lost_the_climb: secondary victim (powerless/trapped) — foreclosed once a competing variant wins
 *   - populations_subject_to_premature_standardization: diffuse victim (powerless/trapped) — bear transition costs of a still-unproven practice
 *   - historians_of_institutional_diffusion: analytical observer — reconstructs whether the winning practice was genuinely superior or merely selected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.55).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb: Legitimacy Gained by Fringe Practices Ascending Through Demonstrated Superiority").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '0890bb09-3feb-4ce8-8a99-e340574c30a8').
narrative_ontology:cs_kernel_codification('0890bb09-3feb-4ce8-8a99-e340574c30a8', distributed).
narrative_ontology:cs_authority_grounding('0890bb09-3feb-4ce8-8a99-e340574c30a8', practice).
narrative_ontology:cs_interpretation_layer_present('0890bb09-3feb-4ce8-8a99-e340574c30a8').
narrative_ontology:cs_reading_relation('0890bb09-3feb-4ce8-8a99-e340574c30a8', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('0890bb09-3feb-4ce8-8a99-e340574c30a8', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('0890bb09-3feb-4ce8-8a99-e340574c30a8', foundational, legitimacy_accrues_through_demonstrated_comparative_performance).
narrative_ontology:cs_axiom_status(legitimacy_accrues_through_demonstrated_comparative_performance, holdable).
narrative_ontology:cs_axiom_grounding('0890bb09-3feb-4ce8-8a99-e340574c30a8', legitimacy_accrues_through_demonstrated_comparative_performance, empirically_contingent).
narrative_ontology:cs_axiom('0890bb09-3feb-4ce8-8a99-e340574c30a8', secondary, fringe_origin_does_not_disqualify_eventual_institutional_authority).
narrative_ontology:cs_axiom_status(fringe_origin_does_not_disqualify_eventual_institutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('0890bb09-3feb-4ce8-8a99-e340574c30a8', fringe_origin_does_not_disqualify_eventual_institutional_authority, conventional).
narrative_ontology:cs_reference_frame('0890bb09-3feb-4ce8-8a99-e340574c30a8', distributed_comparative_testing_framework).
narrative_ontology:cs_drift_state('0890bb09-3feb-4ce8-8a99-e340574c30a8', post_absorption_apex_retelling, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0890bb09-3feb-4ce8-8a99-e340574c30a8', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_practice_originators).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopting_intermediate_institutions).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, eventual_apex_incumbents_after_absorption).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, displaced_incumbent_practitioners).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, rival_fringe_traditions_that_lost_the_climb).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, populations_subject_to_premature_standardization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop a novel administrative, legal, or fiscal practice at the margins of the state apparatus — a merchant guild's bookkeeping method, a provincial court's dispute procedure, a frontier garrison's logistics innovation. They have no formal standing to install anything; their only lever is that the practice visibly outperforms incumbent methods under comparison. If the practice climbs, they become retroactively credited as founders; if it stalls at the periphery, the work is absorbed without credit or abandoned.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_practice_originators, beneficiary,
    powerless, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_practice_originators, agenda_setter).

% Regional courts, provincial administrations, or mid-tier guilds that adopt the fringe practice before the center does, betting that early adoption yields a competitive advantage over peer institutions. They gain relative standing among peers and become conduits carrying the practice upward, but they bear the risk of having backed a practice that never climbs and now marks them as deviant from the still-dominant apex norm.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopting_intermediate_institutions, beneficiary,
    moderate, biographical, mobile, regional).

% The central authority whose existing commitments (legal codes, fiscal procedures, ritual forms) are the incumbent standard being displaced. They did not choose the challenger practice, cannot simply suppress it once its comparative superiority becomes visible to their own subordinate institutions, and pay in credibility and control each time they must concede ground to a fringe-originated alternative — while eventually capturing the practice's legitimacy once absorbed into the center.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_incumbents, payer,
    institutional, generational, constrained, national).

% Officials, scribes, and specialists whose expertise was built around the incumbent practice now being outcompeted. Their skills depreciate as the climbing practice is adopted at higher and higher tiers; they cannot easily retrain into the new practice without conceding that their prior authority was misplaced, and their institutional position offers no exit from the comparison being run against them.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, displaced_incumbent_practitioners, payer,
    moderate, biographical, trapped, national).

% Other marginal practices competing for the same institutional niche as the eventually-successful practice. Because legitimacy accrues to whichever fringe practice wins the comparative climb, competing traditions are foreclosed once the winner is validated upward — their variant becomes retroactively cast as inferior or provincial, regardless of the actual merits of the comparison that decided the outcome.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, rival_fringe_traditions_that_lost_the_climb, payer,
    powerless, generational, trapped, local).

% Communities living under whichever institution is mid-climb when the practice is applied to them — subject to a legal or administrative form still being contested and refined, absorbing the costs of an unstable transitional practice before it has actually proven itself at scale, with no say in whether their region serves as a proving ground.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, populations_subject_to_premature_standardization, payer,
    powerless, biographical, trapped, regional).

% Study the diffusion pattern after the fact, reconstructing which practices climbed, which stalled, and whether the comparative superiority claimed for the winner was genuine or was itself a product of the climb (a practice looks superior partly because institutions selected it, not only because it was independently better).
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, historians_of_institutional_diffusion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the state apparatus to upgrade its administrative, legal, and fiscal technologies by running a live comparative test across many semi-autonomous institutional sites simultaneously, rather than mandating untested reform from the center — a distributed search process that surfaces genuinely superior practices before they are locked in at scale.
% TRANSFER_FUNCTION: Moves institutional legitimacy, credit, and eventually formal authority from marginal originators and early-adopting intermediate institutions toward whichever practice wins the climb — and once won, transfers residual legitimacy to the apex that absorbs and re-issues the practice as its own, while transferring displacement costs onto incumbent practitioners and losing fringe traditions.
% ABSENT_VOICES: Rival fringe traditions that lost the climb are rarely recorded at all — their variant is folded into 'what preceded the reform' and their advocates left no institutional trace once absorbed or discredited. Populations used as proving grounds for a still-unsettled practice are almost never consulted on whether they wished to bear transitional instability.
% DISAPPEARANCE_RATIONALE: If the endogenous-climb mechanism vanished, institutional reform would have to occur either by pure central mandate (removing the comparative-test function entirely) or by permanent fragmentation (no path to legitimacy for any fringe practice, however successful). Existing careers, credited founders, and the retrospective legitimacy narratives built around 'proven superior' practices all depend on the climb having happened.
% FOUNDING_PROBLEM: Centralized authorities cannot single-handedly generate, test, and validate every administrative or legal innovation the state eventually needs; a mechanism was needed to let genuine improvements surface from practice before being formally adopted, without requiring the center to bear all the risk of untested reform.
% FOUNDING_PROBLEM_CORROBORATION: Institutional historians outside the fringe-originator and apex-incumbent camps attest that some climbs do reflect genuine comparative advantage (e.g., double-entry bookkeeping's adoption by merchant states), corroborating the founding problem as still partly live. But comparative institutionalists studying failed or stalled climbs note that apex incumbents frequently claim the climb narrative retroactively to legitimate what was actually a politically brokered adoption — meaning the 'demonstrated superiority' framing is sometimes asserted only by the eventual winners themselves, with no corroboration from the losing fringe traditions or the displaced practitioners who bore the transition costs.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.42 at interval end) and rising: the coordination function (a genuine distributed test of institutional practices) is real and substantial at the outset, but as the climb consolidates, the winning practice's advocates and the apex that eventually absorbs it begin capturing legitimacy gains disproportionate to the ongoing coordination value — later adopters and losing rivals bear increasing relative cost as the field narrows. Suppression rises in parallel (0.30 to 0.55) as the climb approaches the apex: early-stage competition among fringe variants is comparatively open, but once a practice nears institutional capture it actively forecloses rival traditions and displaces incumbent practitioners, requiring increasing active suppression of alternatives to consolidate the win. Theater ratio rises modestly (0.10 to 0.28) as retrospective founding narratives are constructed to legitimate the climb after the fact — the 'demonstrated superiority' story becomes partly performative once the outcome is settled and origin myths are retold to naturalize what was, in part, a contingent institutional contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe originators and early intermediate adopters sit near the beneficiary end when the climb succeeds — low directionality, they gain credit and standing disproportionate to their formal power. Apex incumbents occupy a dual position: payer during the resistance phase (losing control, credibility) but partial beneficiary after absorption (recapturing legitimacy by re-issuing the practice as their own) — this is why apex_incumbents is declared payer here rather than split into two seats: the story is about the climb itself, and the apex's net position across the full climb is extractive-from in the sense that it did not choose the outcome and bears real transition cost even where it later benefits from absorption. Displaced practitioners, losing rival traditions, and proving-ground populations sit at the target end — trapped exit options, generational or biographical time horizons with no meaningful ability to arbitrage the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — decentralized generation and testing of institutional innovation without requiring the center to bear all risk — remains partly live wherever genuine comparative advantage drives the climb (double-entry bookkeeping, standardized weights and measures). But the mechanism is vulnerable to mandatrophy: once a fringe practice climbs and is absorbed, the apex has an incentive to retell the story as pure demonstrated-superiority even where the actual adoption was politically brokered, path-dependent, or driven by network effects among early adopters rather than intrinsic merit. Classifying this as tangled_rope rather than pure rope preserves the distinction: the coordination function (distributed testing) is genuine and should not be mislabeled as pure extraction, but the asymmetric outcome — credit capture by winners, foreclosure of rival traditions, transition costs dumped on proving-ground populations and displaced practitioners — means it cannot be classified as pure coordination either. The active enforcement requirement (rising suppression as the climb nears its apex) is what prevents this from resting as a simple rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_superiority_vs_selection_artifact,
    'Was the climbing practice actually superior on independent grounds, or does it only appear superior because institutions selected it — making ''demonstrated superiority'' partly circular?',
    'Comparative case analysis of practices that climbed versus contemporaneous rival practices with plausible claims to equal or superior performance, controlling for adoption-network effects (which practices had well-connected early adopters versus which had genuinely better outcomes independent of adoption pattern).',
    'If largely genuine, this reading''s coordination-function claim is well-grounded and the tangled_rope classification''s coordination half is robust. If largely selection artifact, the extraction component is understated here and the constraint moves closer to snare — the ''climb'' narrative would be closer to retrospective legitimation of a contingent or politically brokered outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_superiority_vs_selection_artifact, empirical, 'Whether climbing practices were genuinely superior or retrospectively narrated as such.').

omega_variable(
    kernel_reading_disambiguation,
    'For any given historical episode of institutional change, is the endogenous_climb_reading the correct structural account, or does the exogenous_imposition_reading or hybrid_cascade_reading better fit the actual sequence of events?',
    'Archival reconstruction of the actual adoption sequence: did the practice appear first at the fringe and climb (this reading), was it mandated first at the center and then diffused downward (exogenous_imposition_reading), or was it installed at the apex but require subsequent fringe validation to hold (hybrid_cascade_reading)? Different episodes may fit different readings; no single reading is claimed to be universally correct.',
    'Which reading applies changes the beneficiary/victim structure entirely: this reading names fringe originators as primary beneficiaries and apex incumbents as payers; the exogenous_imposition_reading would invert this, naming central authority as primary beneficiary/agenda-setter and diffusion-resistant local actors as payers. Mislabeling a top-down imposition as an endogenous climb would obscure the actual power structure driving the change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Which kernel reading structurally fits a given historical episode of institutional installation, and where the disagreement between readings is located.').

omega_variable(
    apex_dual_position_resolution,
    'Should apex_incumbents be treated as a single payer seat across the full climb, or does the post-absorption beneficiary position warrant a separate stakeholder seat (splitting pre-absorption resistance from post-absorption re-legitimation capture)?',
    'Track whether the specific historical apex institution that resisted a given climbing practice is the same institutional continuity that later claims credit for absorbing it, or whether personnel/regime turnover means the ''payer'' and ''beneficiary'' apex are effectively different agents.',
    'If institutional continuity holds, the current single-seat payer treatment (net-extractive-from across the full climb) is correct. If turnover means a different apex regime captures the absorption benefit than the one that paid the resistance cost, this should be split into two seats with different directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_dual_position_resolution, conceptual, 'Whether the apex''s dual position (resister then absorber) should be one seat or two.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(stat_tr_t60, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(stat_tr_t80, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(stat_tr_t100, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(stat_be_t60, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(stat_be_t80, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(stat_be_t100, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(stat_su_t60, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement(stat_su_t80, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 80, 0.51).
narrative_ontology:measurement(stat_su_t100, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, resource_allocation).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'how new state commitments gain legitimacy.' The exogenous_imposition_reading and hybrid_cascade_reading name structurally distinct mechanisms (top-down mandate; apex-first-with-fringe-stabilization respectively) with different beneficiary/victim sets and different epsilon values. This reading (endogenous_climb) is authored with beneficiaries concentrated among fringe originators and moderate-rising extraction reflecting late-stage legitimacy capture by climb winners; the sibling readings should not be expected to share this epsilon or this beneficiary structure — per the epsilon-invariance principle, each reading is a separate constraint, not an alternative measurement of one mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
