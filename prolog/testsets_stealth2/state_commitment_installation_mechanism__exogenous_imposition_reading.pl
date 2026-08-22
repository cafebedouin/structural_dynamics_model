% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down Commitment Installation by Transformation-Mandate Authority (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   In centralizing polities, this reading holds, new commitments — a unified
 *   legal code, an official creed, a standardized calendar or language —
 *   acquire legitimacy at the moment of top-down installation by an authority
 *   holding a transformation mandate. The decree, backed by administrative
 *   machinery and penal sanction, is the legitimacy event; demonstrated
 *   superiority at the institutional fringe confers nothing. This file
 *   instantiates ONE reading of the kernel
 *   state_commitment_installation_mechanism; the sibling readings
 *   (endogenous_climb_reading, hybrid_cascade_reading) are separate
 *   constraint stories linked through network.affects_constraints, and
 *   nothing about them is averaged into this file. The epsilon referent is
 *   the standing installation arrangement as this reading sees it — not the
 *   endogenous arrangement the siblings endorse. The expected structural
 *   delta is honored throughout: the state is the beneficiary, no grassroots
 *   advocacy channel exists, adoption is abrupt and by decree, and resistance
 *   concentrates at the base. Claim and metrics are independent authored
 *   facts: the type is claimed from structure (a real coordination function
 *   joined to asymmetric transfer and active enforcement), while the metrics
 *   describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - - centralizing_state_authority: Agenda-setting beneficiary (institutional/arbitrage) — issues installation decrees, commands the enforcement machinery, collects compliance, uniformity, and cultural authority
 *   - - administrative_implementing_class: Secondary beneficiary (organized/constrained) — staffs the schools, courts, and registries that carry decrees outward; gains offices, absorbs friction
 *   - - mandate_vindicating_intellectuals: Doctrinal beneficiary (organized/mobile) — supplies the progress narratives and civilizational comparisons that justify the mandate
 *   - - local_practice_communities: Primary target (powerless/trapped) — hold the overridden customary practices; comply under penalty, evade, or migrate; bear enforcement
 *   - - fringe_institutional_innovators: Foreclosed challenger (moderate/constrained) — hold working demonstrations of alternative commitments that the decree route bypasses and never credits
 *   - - comparative_historical_sociologists: Analytical observer (analytical/analytical) — codes initiation, compliance, resistance, and persistence across cases; sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.66).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.58).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down Commitment Installation by Transformation-Mandate Authority (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '2e3ab09c-2b18-4732-ad4e-a980c27726e4').
narrative_ontology:cs_kernel_codification('2e3ab09c-2b18-4732-ad4e-a980c27726e4', distributed).
narrative_ontology:cs_authority_grounding('2e3ab09c-2b18-4732-ad4e-a980c27726e4', expertise).
narrative_ontology:cs_interpretation_layer_present('2e3ab09c-2b18-4732-ad4e-a980c27726e4').
narrative_ontology:cs_reading_relation('2e3ab09c-2b18-4732-ad4e-a980c27726e4', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e3ab09c-2b18-4732-ad4e-a980c27726e4', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('2e3ab09c-2b18-4732-ad4e-a980c27726e4', foundational, installation_by_mandate_authority_grants_legitimacy).
narrative_ontology:cs_axiom_status(installation_by_mandate_authority_grants_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2e3ab09c-2b18-4732-ad4e-a980c27726e4', installation_by_mandate_authority_grants_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('2e3ab09c-2b18-4732-ad4e-a980c27726e4', secondary, endogenous_demonstration_insufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(endogenous_demonstration_insufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2e3ab09c-2b18-4732-ad4e-a980c27726e4', endogenous_demonstration_insufficient_for_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('2e3ab09c-2b18-4732-ad4e-a980c27726e4', apex_mandate_installation_norm).
narrative_ontology:cs_drift_state('2e3ab09c-2b18-4732-ad4e-a980c27726e4', post_everyday_resistance_literature, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2e3ab09c-2b18-4732-ad4e-a980c27726e4', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, centralizing_state_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, administrative_implementing_class).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_practice_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, fringe_institutional_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_vindicating_intellectuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a transformation mandate and issues the decrees that install new commitments — a unified legal code, an official creed, a standardized calendar or language — across the governed territory. Backs each installation with administrative machinery and penal sanctions, appoints the commissions and inspectorates that carry the decree outward, and receives compliance, administrative uniformity, and the standing that comes from having defined what counts as legitimate. Its exit is arbitrage: it can retime, repackage, or quietly shelve a failing installation and reissue it under a new doctrine without surrendering the mandate itself.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, centralizing_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, centralizing_state_authority, beneficiary).

% Staffs the schools, courts, registries, and survey operations through which decrees reach localities. Gains offices, promotion ladders, and jurisdiction that exist only so long as the installation project runs; also absorbs the friction of enforcement — evaded registrations, hostile receptions, quota pressure from the center, and career risk if the policy reverses. Leaving the project means leaving the career it built.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, administrative_implementing_class, beneficiary,
    organized, biographical, constrained, national).

% Supply the doctrinal case that the mandate-holder is entitled, indeed obliged, to install commitments from above: progress narratives, civilizational comparisons, histories of backwardness overcome. Collect advisory posts, lecture halls, and publication markets keyed to the project, and stake their public credibility on its success. If the current regime falls, they can reposition into a successor's intellectual corps.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, mandate_vindicating_intellectuals, beneficiary,
    organized, generational, mobile, national).

% Hold the customary practices, loyalties, and calendars the decrees override. Comply under penalty, evade where supervision is thin, or migrate; bear the costs of dismantling inherited practice, of fines and requisitions, and of raising children inside an imposed commitment they did not choose. Individually weak, but occasionally able to act together in open refusal — withholding, sanctuary, revolt — at the price of reprisal.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_practice_communities, payer,
    powerless, generational, trapped, local).

% Hold working demonstrations of alternative commitments at the institutional margins — reformed liturgies, tested legal procedures, improved cultivation rules — but the installation route bypasses them: legitimacy attaches at the center's decree, not at their demonstrated results. They are neither consulted in the drafting nor credited when their variants are later adopted wholesale. Realistic exits are patience, publication abroad, or service inside the implementing apparatus.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, fringe_institutional_innovators, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, fringe_institutional_innovators, excluded).

% Analyze installation episodes across cases and periods, coding who initiated, who complied, who resisted, and what persisted. Hold no stake in any particular installation; their leverage is archival access and comparative method, and their findings feed forward into how later mandate-holders justify themselves.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, comparative_historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, centralizing_state_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delivers polity-wide commitment on the center's timetable: where persuasion, demonstration, and piecemeal adoption would take generations and leave enclaves, a decree plus administrative machinery places every locality under the same legal code, creed, calendar, or language within months, giving fiscal, military, and diplomatic projects a reliable substrate.
% TRANSFER_FUNCTION: Moves practice-authority and compliance upward and inward: local communities surrender customary practice and the right to decide what they are committed to; the center receives uniformity, legibility, enforcement revenue and labor, and the cultural authority of having defined legitimacy; implementing officials receive offices and jurisdiction carved from the transferred space.
% ABSENT_VOICES: The decree chamber seats the mandate-holder, his ministers, and his doctrinal apologists; it does not seat the communities whose practices will be overridden, nor the fringe innovators whose demonstrated variants the decree route renders irrelevant. Their objections enter the record only as resistance reports and evasion statistics — data the center reads as implementation problems, not as votes.
% DISAPPEARANCE_RATIONALE: If the installation mechanism vanished overnight, every polity mid-transformation would lose its steering instrument: codes, creeds, and calendars would fragment back along local lines until endogenous agreement emerged or never did; implementing administrations would dissolve for lack of a project; and the tempo of state formation — with the wars, migrations, and conversions paced by it — would change. Arrangements across the polity rest on commitments that arrived by decree.
% FOUNDING_PROBLEM: Centralizing authorities confronted a coordination deadlock: their transformation projects required every locality to hold the same commitment faster than persuasion could deliver, while fragmented customary practice kept populations illegible, taxes unreliable, and frontiers unstable. Installation by decree was built to break that deadlock on the center's schedule.
% FOUNDING_PROBLEM_CORROBORATION: The deadlock itself is corroborated from outside the benefiting parties: fiscal and military archives, inspectors' correspondence, and contemporary foreign observers independently attest illegible populations and unreliable compliance preceding the great installation waves. What no one outside the beneficiary set attests is the stronger premise this reading requires — that installation, rather than patient endogenous convergence, was necessary rather than merely expedient; comparative historians outside the state-centered school explicitly dispute it, and that absence of corroboration is itself signal.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: the arrangement transfers practice-authority, compliance, and decision-rights upward on a large scale, discounted by the genuine polity-wide coordination it delivers. Suppression is authored at 0.58 as a STRUCTURAL property — the standing closure of alternatives (old practices penalized, the fringe-demonstration route unrecognized) — and deliberately does not equal the final suppression_requirement measurement (0.47), because the series tracks ACTIVE ENFORCEMENT EFFORT, which follows a ratchet-and-decay arc: initial decree (0.58), intensification against organized base refusal (0.76 at T6), then progressive decay as compliance habituates (0.47 at T30). The scalar persists after the enforcement recedes because the alternatives stay closed even when fewer patrols are needed. Theater_ratio rises 0.10 to 0.36 across the interval: promulgation begins as pure function, and commemorative ceremony, loyalty rituals, and compliance performance grow as enforcement recedes — the signature of a mechanism aging toward ceremonial maintenance, though the functional share remains the majority at interval end. Accessibility_collapse at 0.55: within the official framework the endogenous route is foreclosed outright, but evasion, quiet persistence of custom, and migration keep alternatives partly alive at the margins. Resistance at 0.68 is definitional to this reading — base resistance is the delta the reading itself predicts. Coalition note: the base communities are individually powerless but periodically coordinate open refusal (withholding, sanctuary, revolt); this latent coalition capacity is precisely why suppression must be actively maintained rather than assumed, and why the enforcement ratchet appears at T6.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent classifications from identical structural data. From the center's seat the arrangement is a coordination achievement: the mandate fulfilled, uniformity delivered on schedule, resistance a manageable implementation cost. From the base seat the same arrangement is dispossession: inherited practice overridden without consent, penalties for continuity, children raised inside an imposed commitment. The implementing class occupies the hinge — office gains and career dependence on one side, enforcement friction and reversal risk on the other — and the vindicating intellectuals collect status while staking their credibility on the project's success. The engine derives these per-seat experiences from the authored power, exit, and role data; the divergence between the center's experience and the base's experience is the measurable content of this story.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The centralizing state sits near the full-beneficiary end (declared beneficiary, arbitrage-grade exit — it can retime or rebrand a failing installation without surrendering the mandate). Local practice communities sit near the full-target end (declared victims, trapped exit — practice is fused with land, kinship, and liturgy, so exit means migration or clandestinity, which amplifies their effective burden). Fringe institutional innovators sit high as well: their route to legitimacy is foreclosed by the very machinery, and their exit is constrained. The organized set (implementing class, vindicating intellectuals) is declared beneficiary but demonstrably bears costs — enforcement friction, quota pressure, reputational stakes — so a directionality override lifts the organized power atom from the near-pure-beneficiary value the derivation would produce (~0.1) to 0.28. National spatial scope makes verification of compliance imperfect, which scales effective burden at the target seats modestly upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — breaking a coordination deadlock on the center's schedule — is authored as CONTESTED rather than dead: transformation waves recur, but whether installation is necessary (versus patient endogenous convergence) is exactly what the sibling readings dispute, so no mandatrophy_resolved declaration is authored. The classification guards against both mislabelings: naming the coordination function prevents reading the whole arrangement as pure extraction with a decoration of doctrine; naming the victims, the enforcement requirement, and the foreclosed challengers prevents reading it as pure coordination. The temporal series feeds the drift detector the aging signature: falling suppression_requirement alongside rising theater_ratio is the trajectory a successful installation follows as enforcement gives way to habituation and then to performance. If the kernel contest resolves toward the endogenous reading, the installation machinery is positioned to persist as ceremonial maintenance — the measurements give that hypothesis its test curve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_legitimacy_production,
    'This constraint is one reading of the kernel state_commitment_installation_mechanism (reading: exogenous_imposition_reading); the disagreement with sibling readings (endogenous_climb_reading, hybrid_cascade_reading) is located in WHERE legitimacy is produced — apex decree versus fringe demonstration versus apex installation plus fringe validation. Which location does the historical record support?',
    'Comparative coding of installation episodes for the timing of legitimacy signals: whether acceptance precedes, accompanies, or follows the decree independently of enforcement intensity; the sibling stories carry the same coding protocol so the three readings are scored on one instrument.',
    'If legitimacy reliably precedes installation, the endogenous reading absorbs the cases, this reading''s victim set contracts (fringe innovators were the true carriers), and its measured burden on the base drops; if installation alone stabilizes commitments, this reading stands; if stabilization requires fringe validation, the hybrid reading takes the middle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_of_legitimacy_production, conceptual, 'Committer-frame omega: this file instantiates one of three readings of a shared kernel; the contest is located at the site of legitimacy production.').

omega_variable(
    survivorship_contamination_of_success_cases,
    'Do surviving installation regimes evidence that installation confers legitimacy, or does the archive over-sample regimes whose installed commitments happened to find uptake, with reversed edicts and dead-letter decrees leaving thinner records?',
    'Reconstruct the denominator: catalog attempted installations including aborted ones, and test whether post-decree uptake correlates with enforcement intensity or with pre-decree local readiness.',
    'If survivorship drives the record, the reading''s core axiom loses its evidential base: the machinery was purchasing compliance, not legitimacy, and the arrangement''s effective burden at the base seat rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_contamination_of_success_cases, empirical, 'Whether the reading''s supporting cases are selected on dependent-variable success.').

omega_variable(
    resistance_as_disconfirmation,
    'Does base resistance count against the reading''s own claim (if installation conferred legitimacy, resistance should be rare), or is resistance mere transition friction that subsequent legitimation dissolves?',
    'Track resistance trajectories after installation: resistance that decays as enforcement recedes supports the reading (friction, then legitimation); resistance that persists or resurfaces at each enforcement relaxation indicates the commitment never took and the machinery is holding it by force alone.',
    'If resistance is disconfirming, the mechanism reads as coercion wearing a legitimacy narrative — the coordination-function gate weakens and the base seat''s computed classification hardens toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_as_disconfirmation, conceptual, 'Whether the reading''s signature evidence (decree followed by eventual compliance) is compatible with its own counter-evidence (persistent base resistance).').

omega_variable(
    implementing_class_net_position,
    'Is the administrative implementing class a net beneficiary, or do enforcement friction, quota pressure from the center, and career risk under policy reversal make it a covert payer?',
    'Career-cohort analysis: attrition, promotion rates, and post-reversal fates of implementing officials compared against comparable non-implementing cadres.',
    'If net payer, the beneficiary structure narrows to the center alone, sharpening the asymmetry the engine computes and concentrating the arrangement''s gains in a single seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementing_class_net_position, empirical, 'Net position of the implementing class between office gains and enforcement friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t6, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(stat_tr_t12, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(stat_tr_t18, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 30, 0.36).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(stat_be_t6, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(stat_be_t12, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(stat_be_t18, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 30, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stat_su_t6, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(stat_su_t12, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(stat_su_t18, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 18, 0.61).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 30, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how new commitments gain legitimacy' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that cannot share one story: endogenous climb (upstream — its cases are cited as evidence that apparent impositions merely ratify prior fringe development), exogenous imposition (this file — installation as the legitimacy event itself), and hybrid cascade (mediator — installation necessary but insufficient without fringe validation). Each member of the family carries its own epsilon, beneficiary/victim structure, and type. This file's epsilon is authored for the standing installation arrangement as the exogenous reading sees it; the sibling files author epsilon for their own arrangements over the same referent. Upstream influences downstream: endogenous-climb evidence is routinely deployed against this reading, which is why the family edges run in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
