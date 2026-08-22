% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Catastrophe-Memory Mourning Cycle as Survival-Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A persecuted minority community maintains an annual cycle of fasts,
 *   lament recitations, and household retellings of catastrophe and escape
 *   (the pattern exemplified by Tisha B'Av-style communal laments and
 *   Passover-seder-style domestic rehearsal). The tradition-holders present
 *   the cycle as survival training: each observance drills a piece of
 *   catastrophe-response — portability of assets, trusted-contact discipline,
 *   rapid mutual-aid mobilization — and the calendar-timed contributions
 *   finance the rescue institutions the drills presuppose. The costs are real
 *   and unevenly placed: households fund the apparatus and defer
 *   assimilation; the young bear the discipline and the closed doors before
 *   any payoff arrives. This story is ONE READING of the
 *   catastrophe_memory_kernel (Rule 1): the survival-competence reading,
 *   generated as a clean epsilon-invariant constraint. Epsilon's referent is
 *   the standing arrangement under contest — the observance regime as it
 *   actually operates — assessed by this reading's own lights, which yields
 *   MODERATE extraction (0.48): a genuine transmission function wearing real,
 *   asymmetrically distributed costs. The claimed type (tangled_rope) and the
 *   metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - ritual_calendar_authorities: Agenda-setting seat ([institutional]/[identity_locked]) — fixes the canon, rules on observance, collects the standing the administration confers; cannot treat exit as an option without dissolving their own office
 *   - threatened_households: Primary beneficiary seat ([moderate]/[constrained]) — receives transmitted competence and network shelter; pays time, contributions, and deferred assimilation (dual-positioned: beneficiary with a heavy payer side)
 *   - second_generation_youth: Primary cost-bearing seat ([powerless]/[identity_locked]) — bears discipline and foreclosed options now; the competence reaches them later, contingently
 *   - communal_mutual_aid_funds: Institutional collector ([organized]/[trapped]) — receives the calendar-timed contributions; exists only inside the observance rhythm
 *   - host_society_majority: Excluded counterparty ([powerful]/[mobile]) — source of both the intermittent threat and the forgone alternatives; holds no seat in the councils
 *   - comparative_ritual_scholars: Analytical observer ([analytical]/[analytical]) — tests the training claim against outcome data from outside the community's control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Catastrophe-Memory Mourning Cycle as Survival-Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '94d6633c-9310-4734-a72f-ae05a4a7a64d').
narrative_ontology:cs_kernel_codification('94d6633c-9310-4734-a72f-ae05a4a7a64d', fixed_text).
narrative_ontology:cs_authority_grounding('94d6633c-9310-4734-a72f-ae05a4a7a64d', lineage).
narrative_ontology:cs_interpretation_layer_present('94d6633c-9310-4734-a72f-ae05a4a7a64d').
narrative_ontology:cs_reading_relation('94d6633c-9310-4734-a72f-ae05a4a7a64d', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('94d6633c-9310-4734-a72f-ae05a4a7a64d', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('94d6633c-9310-4734-a72f-ae05a4a7a64d', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('94d6633c-9310-4734-a72f-ae05a4a7a64d', foundational, ritual_rehearsal_confers_survival_advantage).
narrative_ontology:cs_axiom_status(ritual_rehearsal_confers_survival_advantage, holdable).
narrative_ontology:cs_axiom_grounding('94d6633c-9310-4734-a72f-ae05a4a7a64d', ritual_rehearsal_confers_survival_advantage, empirically_contingent).
narrative_ontology:cs_axiom('94d6633c-9310-4734-a72f-ae05a4a7a64d', secondary, readiness_obligation_justified_by_threat_persistence).
narrative_ontology:cs_axiom_status(readiness_obligation_justified_by_threat_persistence, holdable).
narrative_ontology:cs_axiom_grounding('94d6633c-9310-4734-a72f-ae05a4a7a64d', readiness_obligation_justified_by_threat_persistence, instrumental).
narrative_ontology:cs_reference_frame('94d6633c-9310-4734-a72f-ae05a4a7a64d', catastrophe_response_training_cycle).
narrative_ontology:cs_drift_state('94d6633c-9310-4734-a72f-ae05a4a7a64d', emancipation_era_security, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('94d6633c-9310-4734-a72f-ae05a4a7a64d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, communal_mutual_aid_funds).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, second_generation_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, threatened_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fix the yearly round of fasts, lament recitations, and memorial observances; rule on the texts and on exemptions; levy and time the communal contributions that flow through the calendar. Their standing rests on administering the transmission — they inherited the canon from their teachers and owe their office to keeping it unbroken. Stepping outside the cycle would dissolve the basis of their own authority, so they do not treat that as an option.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_calendar_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% Keep the observances in the home: retell the catastrophe-and-escape narratives at the table, keep the communal fasts, host the memorial gatherings, and pay the calendar-timed contributions to the burial societies and loan funds. What comes back is practical instruction — where to flee, whom to trust, how to keep papers and savings portable — plus a ready-made network when trouble arrives. Staying costs hours, money, and opportunities in the wider society passed up; leaving means walking away from the network that would shelter them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_households, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, threatened_households, payer).

% Grow up inside the obligations before choosing them: memorize the laments, keep the fasts, accept courtship and marriage expectations that keep them within the group, and sit through rehearsals of disasters they have never witnessed. The competence the cycle carries reaches them mostly later in life, if trouble comes; the discipline and the closed doors are theirs now. Walking out means losing family and the only community they have ever had.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, second_generation_youth, payer,
    powerless, biographical, identity_locked, national).

% Burial societies, dowry and loan funds, and rescue committees financed by the calendar-timed contributions; they pay ransoms, passages, and rebuilding costs after each catastrophe. They exist only inside the observance rhythm that fills them — no contributions, no funds — and they disburse back to the same households that pay in.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, communal_mutual_aid_funds, beneficiary,
    organized, generational, trapped, continental).

% Neighbors, employers, officials, and interfaith partners of the minority. They alternate between long tolerant periods and episodes of persecution; in the tolerant periods they offer the paths — guilds, universities, mixed neighborhoods, intermarriage — that the observances ask members to decline or defer. They have no seat in the councils that set the calendar, and the cycle's texts cast them as the recurring threat.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, host_society_majority, excluded,
    powerful, biographical, mobile, national).

% Historians and anthropologists of minority survival who compare communities with dense catastrophe-liturgies against similarly situated communities without them, tracking outcomes across expulsions and massacres. They take testimony from every seat, publish outside the community's control, and can confirm or deflate the training claim with evidence neither the authorities nor the households command.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, comparative_ritual_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, communal_mutual_aid_funds).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of transmitting non-declarative survival knowledge — how to respond to expulsion, confiscation, and violence; whom to trust; how to keep assets liquid and documents portable; how to reconstitute a dispersed community — across generations under conditions where written instruction is unsafe and firsthand experience is lethal to acquire. Secondarily, it times and pools the mutual-aid contributions that finance rescue and rebuilding.
% TRANSFER_FUNCTION: Moves time, labor, and money from member households, and deferred assimilation opportunities from the young, into the communal reserve institutions and the authority structure that administers the calendar; moves rehearsed response patterns, warnings, and network access back down to the households.
% ABSENT_VOICES: The host-society majority is structurally outside the conversation — cast in the texts as the threat, never consulted as counterparty — along with employers and would-be integrators whose offers the observances quietly refuse. Departed members who bore the costs without staying for the payoff are also absent. Youth sit inside the room but without agenda power.
% DISAPPEARANCE_RATIONALE: If the cycle vanished overnight, the calendar-timed contribution streams stop and the mutual-aid funds deplete within a generation; the rehearsed response patterns stop being drilled and decay to anecdote; intermarriage and economic integration accelerate; the distinct institutions dissolve within two to three generations. In the next threat episode the community would reconstitute poorly — the arrangements demonstrably depend on the practice continuing.
% FOUNDING_PROBLEM: Recurrent catastrophic persecution — expulsions, confiscations, massacres — repeatedly destroyed minority communities that lacked rehearsed response patterns, portable resources, and pre-committed mutual aid.
% FOUNDING_PROBLEM_CORROBORATION: The historical reality of the founding problem is corroborated from outside the benefiting parties: host-state archival records document the expulsions and massacres, and comparative historians of minorities attest the recurrence pattern. But corroboration of the problem's PRESENT liveness comes almost entirely from the community's own authorities, who cite each new catastrophe as proof; external scholars are divided, with a substantial literature holding that long emancipation-era security stretches have made the founding problem dormant. Partial external corroboration, openly disputed on liveness.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48) because the arrangement does deliver what it claims — drilled response patterns and funded rescue — while transferring real resources and foreclosing real options; it is neither negligible (a pure rope) nor dominant (a snare). Suppression (0.52) is a raw structural property, unscaled by power or scope: enforcement runs through communal censure, contribution-linked access to burial and credit, and family obligation rather than state coercion. Theater ratio (0.25) is low-to-moderate: the liturgy genuinely transmits content, but a growing share of commemorative activity functions as identity display rather than drill — visible in the slow theater rise across the series. Accessibility collapse (0.42): alternatives to ritual transmission (formal schooling, secular insurance, citizenship strategies) persist in tolerant periods and collapse under acute threat, so the constraint closes options partially and conditionally. Resistance (0.45): youth drift, exemption-seeking, and periodic reform movements are chronic but rarely decisive. The three measurement series share ONE time grid (points 0–150 at 25-year steps) so every metric is authored at every examined point. The trajectories are monotone with step-shaped underlying dynamics: each catastrophe shock permanently elevates the baseline (a new memorial obligation is added and never removed), producing a rectified-ratchet appearance smoothed into the sampled series. The suppression_requirement series traces enforcement-capacity maturation — informal censure early, codified communal ordinance later — which is why it is tracked here rather than left to the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter's position the cycle is an insurance system it stewards: premiums, reserves, drills, payouts. From the youth seat the same structure is conscription without consent — obligations front-loaded, benefits deferred and contingent. From the host society's position it is clannish closure that refuses integration offers. From the analyst's position it is a testable hypothesis about ritual and survival with mixed evidence. The engine computes these divergent per-seat classifications from power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation: threatened_households (beneficiary, constrained exit) derive low-directionality with a real payer drag; communal_mutual_aid_funds (beneficiary, trapped) sit near the full-beneficiary end — they receive the flows and cannot exist elsewhere; second_generation_youth (victim, identity_locked, powerless) derive near the full-target end, amplified by identity lock. Two overrides are declared because the derivation chain would misplace two seats. (1) moderate -> 0.35: threatened_households are the only moderate-power agent; deriving from beneficiary-plus-constrained-exit alone would land them near 0.15, understating that they fund the entire apparatus and carry the assimilation deferral — 0.35 places them net-beneficiary but materially burdened. (2) institutional -> 0.15: ritual_calendar_authorities have no beneficiary/victim declaration, so they would fall to the generic institutional fallback near symmetric; in fact they collect standing, deference, and livelihood from administering the cycle and bear almost none of its costs — 0.15 records their beneficiary-side position.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope keeps both faces of the arrangement visible and prevents the two standard mislabels. Reading it as a pure rope would erase the identifiable cost-bearing seat: the young pay before they choose, and the enforcement machinery exists precisely because voluntary uptake would lag. Reading it as a snare would erase the coordination function: the transmitted competence and funded rescue are real, participants are net beneficiaries over a life course that includes a threat episode, and the arrangement does not depend on suppressing exits so much as on making exit expensive. The mandate question — whether the founding problem is still live — is deliberately NOT resolved here; it is routed to the threat_recurrence_status omega and the R5 interview (status: contested). If the dormancy reading wins, the expected trajectory is slow drift toward theatrical maintenance (the theater_ratio series already creeps upward) and eventual piton character; if recurrence wins, the obligation load tracks a live hazard and the coordination function stays genuine. The classification apparatus holds that question open instead of letting either the authorities' revival narrative or the assimilationist critique settle it by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is the operative function of the mourning cycle the transmission of survival competence (this reading), or symbolic continuity, trauma warning, or boundary enforcement (the sibling readings of catastrophe_memory_kernel)?',
    'Process-trace what the rites actually rehearse and what practitioners can do afterward: if liturgical content maps onto concrete response behaviors (portable assets, trusted contacts, flight decisions) that practitioners demonstrably execute in crises, the competence reading stands; if the content is symbolic without behavioral yield, a sibling reading captures the structure.',
    'Each sibling instantiates a different constraint with a different epsilon: boundary-maintenance would author higher extraction (enforcement of closure for its own sake), symbol-continuity lower (continuity as its own end). This story''s epsilon of 0.48 holds only under the competence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the catastrophe-memory kernel this arrangement instantiates.').

omega_variable(
    rehearsal_efficacy_unverified,
    'Does ritual rehearsal of catastrophe-response actually improve survival outcomes, or is observed survival attributable to mutual-aid networks, geography, and host-regime variation that would operate with or without the liturgy?',
    'Matched comparison of comparable minority communities with and without dense catastrophe-liturgy across recorded expulsions and massacres, controlling for wealth, urbanization, and host policy.',
    'If no efficacy differential survives controls, the coordination function collapses and the arrangement reclassifies toward inertial obligation maintained by authority and habit alone; if a differential survives, the moderate extraction is partly the price of the training itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rehearsal_efficacy_unverified, empirical, 'Whether the transmitted survival competence is real or a cover story.').

omega_variable(
    threat_recurrence_status,
    'Does the persecution threat that justifies perpetual readiness still recur at intervals that justify the full obligation load, or have long emancipation-era security stretches made the founding problem dormant?',
    'Host-state incident series and persecution-event chronologies correlated with the calendar''s own expansion history (each new memorial marks a new catastrophe); asylum and displacement data for the community''s host regions.',
    'If the threat is dormant, the arrangement persists by inertia and the mandate question resolves toward obsolescence with slow theatrical drift; if recurrent, the obligation load tracks a live hazard and the coordination function stays genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_recurrence_status, empirical, 'Whether the founding threat justifying the cycle is live or dormant.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the enforcement that keeps members inside the observance structural (dependence on communal credit, burial, dowry, and rescue networks that only contributors may draw on) or internalized (filial duty and shame that persist after economic independence makes exit possible)?',
    'Post-exit trajectories of members who leave during tolerant periods: if observance pressure and guilt persist socially after financial independence, the internalized share dominates; if departures are clean, the structural share dominates.',
    'If internalized, effective suppression exceeds the structural measure and the cost-bearing seat is more tightly held than its exit-option atom suggests; the omega feeds the suppression-mechanism ambiguity directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of observance enforcement.').

omega_variable(
    cost_timing_asymmetry,
    'Are the boundary-maintenance costs borne by the young a fair insurance premium for deferred protection, or a transfer from the seat without agenda power to the seats with it?',
    'Life-course accounting comparing lifetime net flows of members who stayed versus comparable members who exited before adulthood, evaluated separately under threat and security regimes.',
    'If the premium framing holds, the households'' moderately beneficiary-side directionality is right; if a transfer, the youth seat''s target-side position generalizes across the community and measured extraction reads higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_timing_asymmetry, preference, 'Whether the intergenerational cost timing is premium or transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement_basis(cata_tr_t75, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(cata_tr_t100, observed).
narrative_ontology:measurement(cata_tr_t125, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 125, 0.24).
narrative_ontology:measurement_basis(cata_tr_t125, observed).
narrative_ontology:measurement(cata_tr_t150, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement_basis(cata_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 75, 0.46).
narrative_ontology:measurement_basis(cata_be_t75, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.47).
narrative_ontology:measurement_basis(cata_be_t100, observed).
narrative_ontology:measurement(cata_be_t125, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 125, 0.48).
narrative_ontology:measurement_basis(cata_be_t125, observed).
narrative_ontology:measurement(cata_be_t150, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement_basis(cata_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(cata_su_t50, observed).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 75, 0.47).
narrative_ontology:measurement_basis(cata_su_t75, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement_basis(cata_su_t100, observed).
narrative_ontology:measurement(cata_su_t125, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 125, 0.51).
narrative_ontology:measurement_basis(cata_su_t125, observed).
narrative_ontology:measurement(cata_su_t150, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 150, 0.52).
narrative_ontology:measurement_basis(cata_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'catastrophe memory ritual' conflates four structurally distinct claims about what the mourning cycle does. This file instantiates ONLY the survival-competence reading; its epsilon (0.48) is authored for the standing observance regime as this reading sees it — a functioning training system whose costs fall unevenly — and is not invariant across readings (the boundary-maintenance sibling would author higher epsilon, the symbol-continuity sibling lower). Each sibling is a separate story with its own epsilon, stakeholders, and classification; the edges here are reading-level relations, not shared metrics. The upstream/downstream gradient runs from this reading INTO the boundary-maintenance reading, because the competence claim is the warrant boundary-enforcers cite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, moderate, 0.35).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
