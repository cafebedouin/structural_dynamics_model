% ============================================================================
% CONSTRAINT STORY: commitment_cost_location_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commitment_cost_location_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commitment_cost_location_flat_control
 *   human_readable: Pre-Committed Kill Condition as Discourse Norm
 *   domain: epistemology/discourse_norms
 *
 * SUMMARY:
 *   This story treats, as a single flat constraint, the shared discourse
 *   commitment that stating a falsifiable 'kill condition' alongside a
 *   forecast or confident claim carries a genuine epistemic cost even before
 *   that condition is ever tested — the norm that pre-commitment itself is a
 *   real, discharge-able obligation, not merely rhetorical decoration. Every
 *   participant in this practice agrees that the commitment has a cost; they
 *   disagree, structurally rather than merely opinion-wise, about where that
 *   cost is located (in the author's future reputation, in the reader's trust
 *   budget, in the world's eventual verdict, in a third party's adjudication)
 *   and what actually discharges it (public acknowledgment of failure,
 *   retraction, silent non-renewal, external fact-check). This story does not
 *   decompose that disagreement into separate readings; it authors the
 *   disagreement as perspectival divergence across stakeholder seats on one
 *   constraint, plus omega variables naming what remains genuinely open.
 *
 * KEY AGENTS:
 *   - forecast_issuing_authors: sets and can reinterpret the kill condition, banks credibility at commitment time (moderate/constrained) — primary beneficiary of ambiguity about where the cost sits
 *   - discourse_platform_moderators: sustains the norm's reputational value without funding verification (organized/mobile) — secondary beneficiary
 *   - downstream_readers: extends trust based on the stated condition, cannot verify enforcement (powerless/trapped) — bears the cost of unlocated discharge
 *   - cited_forecast_subjects: real-world subjects of the forecast who suffer consequences if falsification never actually fires (powerless/trapped)
 *   - independent_adjudicators: structurally excluded third parties who could resolve firing/non-firing but usually aren't funded or invited
 *   - epistemic_community_observers: analytical seat studying the aggregate pattern across many instances of this norm (analytical/global)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commitment_cost_location_flat_control, 0.42).
domain_priors:suppression_score(commitment_cost_location_flat_control, 0.31).
domain_priors:theater_ratio(commitment_cost_location_flat_control, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commitment_cost_location_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(commitment_cost_location_flat_control, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(commitment_cost_location_flat_control, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commitment_cost_location_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commitment_cost_location_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commitment_cost_location_flat_control, tangled_rope).
narrative_ontology:human_readable(commitment_cost_location_flat_control, "Pre-Committed Kill Condition as Discourse Norm").
narrative_ontology:topic_domain(commitment_cost_location_flat_control, "epistemology/discourse_norms").

domain_priors:requires_active_enforcement(commitment_cost_location_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(commitment_cost_location_flat_control, commitment_cost_location).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commitment_cost_location_flat_control, forecast_issuing_authors).
narrative_ontology:constraint_beneficiary(commitment_cost_location_flat_control, discourse_platform_moderators).
narrative_ontology:constraint_victim(commitment_cost_location_flat_control, downstream_readers).
narrative_ontology:constraint_victim(commitment_cost_location_flat_control, cited_forecast_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes a pre-committed falsifiable claim (a kill condition) attached to a forecast or model, and controls how strictly that condition is specified and later interpreted. Gains credibility for having 'put a condition on the line' whether or not the condition is ever actually tested against outcomes, since the mere act of pre-commitment reads as epistemic virtue to an audience. Can retroactively narrow, contextualize, or reinterpret the condition if the world does not cooperate, and bears little enforceable cost for doing so beyond reputational friction that is itself hard to verify.
narrative_ontology:constraint_stakeholder(commitment_cost_location_flat_control, forecast_issuing_authors, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commitment_cost_location_flat_control, forecast_issuing_authors, beneficiary).

% Maintains the norm that kill conditions are a marker of intellectual seriousness, since the norm generates engagement and distinguishes 'good faith' from 'bad faith' participants at low administrative cost. Benefits from the appearance of accountability infrastructure without having to build or fund the verification apparatus that would actually adjudicate whether a condition fired.
narrative_ontology:constraint_stakeholder(commitment_cost_location_flat_control, discourse_platform_moderators, beneficiary,
    organized, biographical, mobile, national).

% Relies on the presence of a stated kill condition as a proxy for the trustworthiness of the underlying claim, without the time, expertise, or standing to independently verify whether the condition was ever actually triggered or how it was later reinterpreted. Pays the cost of the norm's laxity by being misled into updating on claims whose falsification machinery quietly failed to fire when it should have.
narrative_ontology:constraint_stakeholder(commitment_cost_location_flat_control, downstream_readers, payer,
    powerless, immediate, trapped, national).

% The people, institutions, or policies that the forecast is actually about bear real consequences when a poorly-adjudicated kill condition allows a wrong prediction to keep circulating as if vindicated. They have no seat in the discourse and no mechanism to force the falsification to be honored.
narrative_ontology:constraint_stakeholder(commitment_cost_location_flat_control, cited_forecast_subjects, payer,
    powerless, biographical, trapped, national).

% Third-party fact-checkers, replication teams, or prediction-market resolvers who could in principle determine whether a kill condition fired, but are rarely invited into the original commitment and often lack funding, mandate, or timely access to do so. Their absence is precisely what allows the cost of the commitment to remain unlocated.
narrative_ontology:constraint_stakeholder(commitment_cost_location_flat_control, independent_adjudicators, excluded,
    moderate, generational, constrained, national).

% Philosophers of science, forecasting researchers, and meta-analysts who study the practice of falsifiable pre-commitment across many instances, without a stake in any single forecast's outcome. They can see the aggregate pattern of condition-drift and selective enforcement that no single participant is positioned to observe.
narrative_ontology:constraint_stakeholder(commitment_cost_location_flat_control, epistemic_community_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pre-committing to a falsifiable kill condition genuinely solves a real coordination problem: it lets an audience distinguish claims that risk being wrong from claims insulated against ever being tested, and it lets the author signal seriousness before the outcome is known, when signaling is cheapest to fake and hardest to verify.
% TRANSFER_FUNCTION: The arrangement moves credibility from the audience (who extend provisional trust on the strength of the stated condition) to the author (who banks reputational capital immediately, at commitment time) — but the cost of an unenforced or reinterpreted condition is deferred and diffused onto readers and forecast subjects who cannot easily trace, later, whether the bill was ever actually paid.
% ABSENT_VOICES: Independent adjudicators who could resolve whether a condition fired are structurally absent from the original commitment; they would insist on pre-registered, third-party-verifiable trigger definitions, which most informal discourse commitments do not have the infrastructure to support.
% DISAPPEARANCE_RATIONALE: Authors and moderators would say the world barely changes — forecasts would simply drop the pretense of falsifiability and be judged on vibes, which is roughly what already happens once a condition is reinterpreted away. Epistemic-community observers and independent adjudicators would say the world rearranges substantially: without even the nominal discipline of a stated kill condition, the last remaining lever forcing authors to specify what would count as being wrong disappears, and readers lose the only artifact they currently have to demand accountability against, however imperfectly enforced.
% FOUNDING_PROBLEM: Public forecasting and confident claim-making had no mechanism to distinguish genuine risk-taking from unfalsifiable hedging; the kill-condition norm was built so that a claim could be checked against reality rather than endlessly reinterpreted after the fact.
% FOUNDING_PROBLEM_CORROBORATION: Forecasting researchers and prediction-market practitioners (Tetlock-adjacent literature, replication-focused meta-analysts) attest from outside the benefiting author/moderator seats that the founding problem — distinguishing real risk from unfalsifiable hedging — remains live, but that the informal kill-condition norm as practiced in ordinary discourse has drifted from solving it toward merely performing it, since verification infrastructure was never built to match the rhetorical commitment.
narrative_ontology:disappearance_verdict(commitment_cost_location_flat_control, contested).
narrative_ontology:founding_problem_status(commitment_cost_location_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commitment_cost_location_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(commitment_cost_location_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(commitment_cost_location_flat_control, 0.42, 'claude-sonnet-5', 'omega_production_confession_kernel_20260814_211528', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commitment_cost_location_flat_control_tests).
:- end_tests(commitment_cost_location_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.42 at interval end) rather than severe: the coordination function is real and not fake from the outset — pre-committing to falsifiability genuinely differs from refusing to, and readers do extract real signal value from the practice. But extraction rises over the interval because as the norm becomes established and prestige-conferring, the incentive to let conditions drift or be silently reinterpreted grows faster than the verification infrastructure that would hold authors to them. Theater ratio starts moderate (0.28) and rises to just under half (0.48) — reflecting that a rising share of 'I have a kill condition' framing becomes performative signaling of seriousness rather than functioning falsification machinery, without ever becoming pure theater, since some conditions genuinely do get checked. Suppression is authored low-to-moderate and roughly flat (0.20→0.31): there is no active coercive mechanism forcing anyone to accept a given author's framing of whether their condition fired — the mechanism is closer to information asymmetry and absent adjudication infrastructure than to coercion, which is why suppression stays well below extractiveness and theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (forecast_issuing_authors), this looks like Rope: they see themselves taking on a real, costly commitment that most confident claim-makers refuse to take on, and experience the norm as genuine epistemic discipline. From the payer seats (downstream_readers, cited_forecast_subjects), the same structure often computes as Tangled Rope or worse: the coordination function is real in principle but the actual discharge mechanism is absent often enough, and unverifiable often enough, that the stated cost frequently never lands anywhere at all — it is claimed, banked, and never actually paid. The engine should be expected to compute these seats differently given the sharp asymmetry in exit_options (constrained vs. trapped) and power (moderate vs. powerless) even though every seat is looking at the identical constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   forecast_issuing_authors sits near the beneficiary end: they collect reputational credit at commitment time and bear only soft, hard-to-verify costs if the condition is reinterpreted later, so directionality should derive low. discourse_platform_moderators similarly derive toward the beneficiary end, collecting engagement and a veneer of rigor without funding enforcement. downstream_readers and cited_forecast_subjects derive toward the target end: they extend trust or bear real-world consequences without the standing or resources to force the cost to be located and discharged. independent_adjudicators are excluded rather than benefiting or paying directly, which is itself the structural fact worth noting: their absence is what keeps the cost unlocated in the first place.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric mislabelings. Calling this pure Rope would erase the real, measured divergence between the reputational benefit an author books immediately and the diffuse, frequently-undischarged cost borne by readers and forecast subjects — treating a partially-performed obligation as a fully honored one. Calling this pure Snare would erase the genuine coordination value the practice provides when conditions ARE honestly specified and later checked — collapsing every instance of the norm to bad faith would make the framework unable to distinguish careful forecasters from unfalsifiable hedgers, which is exactly the distinction the norm exists to preserve. Tangled Rope holds both facts: real coordination function, real asymmetric extraction riding on the same structure, requiring active vigilance (moderators, adjudicators, or reader skepticism) to keep the extraction from crowding out the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_location_ambiguity,
    'Where does the cost of a stated-but-unenforced kill condition actually land — in the author''s future reputation, in the reader''s trust calibration, in the discourse community''s collective epistemic hygiene, or nowhere at all?',
    'Longitudinal tracking of specific forecasters across many stated kill conditions, cross-referenced against whether their subsequent claims are treated with more or less credibility by the same audience when a condition demonstrably fails to fire and is not honestly acknowledged.',
    'If the cost reliably lands on author reputation over enough instances, the arrangement functions closer to Rope with real accountability; if it demonstrably lands nowhere (audiences do not update, or update too slowly to matter), the arrangement functions closer to Snare with a coordination-shaped cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_location_ambiguity, empirical, 'Whether the discharge mechanism for kill-condition costs actually exists in practice or is purely notional.').

omega_variable(
    discharge_mechanism_specification,
    'What, structurally, would count as ''discharging'' the cost of a kill condition — is it public acknowledgment of failure, formal retraction, silent non-renewal of the claim, or third-party adjudication — and does the discourse community actually agree on this, or only agree that SOME discharge is owed?',
    'Survey or discourse analysis of stated expectations across forecasting communities (e.g. prediction-market participants, public intellectuals, academic forecasters) to determine whether a shared operational definition of ''discharge'' exists or whether each party privately assumes a different one.',
    'If no shared discharge definition exists, then the appearance of consensus that the kill condition ''has a cost'' is itself doing extractive work — it lets every party feel the obligation is respected while no single mechanism is ever actually triggered, which would push the classification further toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discharge_mechanism_specification, conceptual, 'Whether disagreement over what discharges the cost is itself the extraction mechanism.').

omega_variable(
    verification_infrastructure_feasibility,
    'Could a genuinely low-cost, low-suppression verification infrastructure (e.g. neutral third-party registries, automated tracking of stated conditions against outcomes) close the gap this constraint currently leaves open, or does the informality of most discourse settings make such infrastructure structurally infeasible?',
    'Pilot comparison between discourse communities with formal registries (e.g. prediction markets, pre-registered replication studies) and those without, measuring whether theater_ratio and extractiveness are structurally lower where registries exist.',
    'If feasible and effective, this constraint is closer to a fixable Tangled Rope (add the missing enforcement layer, convert toward Rope); if infeasible at the scale of ordinary discourse, the extraction may be closer to structurally inherent to the practice as currently constituted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_infrastructure_feasibility, empirical, 'Whether the missing adjudication layer is a solvable engineering gap or a structural feature of informal discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commitment_cost_location_flat_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commitment_cost_location_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comm_tr_t4, commitment_cost_location_flat_control, theater_ratio, 4, 0.33).
narrative_ontology:measurement(comm_tr_t8, commitment_cost_location_flat_control, theater_ratio, 8, 0.37).
narrative_ontology:measurement(comm_tr_t12, commitment_cost_location_flat_control, theater_ratio, 12, 0.4).
narrative_ontology:measurement(comm_tr_t16, commitment_cost_location_flat_control, theater_ratio, 16, 0.43).
narrative_ontology:measurement(comm_tr_t20, commitment_cost_location_flat_control, theater_ratio, 20, 0.46).
narrative_ontology:measurement(comm_tr_t24, commitment_cost_location_flat_control, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commitment_cost_location_flat_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comm_be_t4, commitment_cost_location_flat_control, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(comm_be_t8, commitment_cost_location_flat_control, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(comm_be_t12, commitment_cost_location_flat_control, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(comm_be_t16, commitment_cost_location_flat_control, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(comm_be_t20, commitment_cost_location_flat_control, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(comm_be_t24, commitment_cost_location_flat_control, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commitment_cost_location_flat_control, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comm_su_t4, commitment_cost_location_flat_control, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(comm_su_t8, commitment_cost_location_flat_control, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(comm_su_t12, commitment_cost_location_flat_control, suppression_requirement, 12, 0.26).
narrative_ontology:measurement(comm_su_t16, commitment_cost_location_flat_control, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(comm_su_t20, commitment_cost_location_flat_control, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(comm_su_t24, commitment_cost_location_flat_control, suppression_requirement, 24, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commitment_cost_location_flat_control, information_standard).
narrative_ontology:boltzmann_floor_override(commitment_cost_location_flat_control, 0.05).

% DUAL FORMULATION NOTE:
% This is the flat (undecomposed) construction of the commitment-cost-location substrate, authored as a single tangled_rope constraint with perspectival divergence carried in stakeholder seats and unresolved locations carried in omegas, rather than as separate reading-indexed constraint stories. It is a construction-perturbation control: a companion decomposition of the same substrate into distinct readings (e.g. an author-discharge reading, a reader-trust reading, a third-party-adjudication reading) would be linked here via affects_constraints if and when authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
