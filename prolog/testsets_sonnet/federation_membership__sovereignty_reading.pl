% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty reading of the
 *   federation-membership kernel: membership is a conditional treaty from
 *   which national authority retains legitimate border control, and free
 *   movement is a negotiable policy rather than a constitutional right. Under
 *   this reading, member states can invoke retained sovereignty to ration or
 *   suspend cross-border labor mobility in response to domestic political or
 *   economic pressure. This is structurally distinct from the integration
 *   reading (a separate constraint,
 *   `federation_membership__integration_reading`), where free movement is a
 *   constitutional entitlement enforced supranationally and border-invocation
 *   is treated as a treaty breach rather than a legitimate exercise of
 *   authority. The two readings are not two measurements of one constraint —
 *   they have different beneficiary/victim sets, different enforcement
 *   authorities, and different epsilon: this reading's ε is driven by
 *   mobility restriction falling on mobile populations; the integration
 *   reading's ε would be driven by supranational override of domestic
 *   labor-market protection. They are linked structurally, not averaged.
 *
 * KEY AGENTS:
 *   - national_border_agencies: primary agenda-setter, retains and exercises border authority under the sovereignty clause
 *   - local_labor_markets and domestic_incumbent_workers: primary beneficiaries of mobility restriction
 *   - mobile_citizens, cross_border_workers, binational_families: primary targets bearing the cost of unpredictable mobility restriction
 *   - federation_secretariat: structurally excluded institutional voice for the rival integration reading
 *   - treaty_arbitration_panel: analytical observer adjudicating disputed invocations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '01a15821-4a45-46b3-8a4b-6e59f6428c07').
narrative_ontology:cs_kernel_codification('01a15821-4a45-46b3-8a4b-6e59f6428c07', formalized).
narrative_ontology:cs_authority_grounding('01a15821-4a45-46b3-8a4b-6e59f6428c07', lineage).
narrative_ontology:cs_interpretation_layer_present('01a15821-4a45-46b3-8a4b-6e59f6428c07').
narrative_ontology:cs_reading_relation('01a15821-4a45-46b3-8a4b-6e59f6428c07', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('01a15821-4a45-46b3-8a4b-6e59f6428c07', foundational, border_authority_is_retained_not_delegated).
narrative_ontology:cs_axiom_status(border_authority_is_retained_not_delegated, holdable).
narrative_ontology:cs_axiom_grounding('01a15821-4a45-46b3-8a4b-6e59f6428c07', border_authority_is_retained_not_delegated, conventional).
narrative_ontology:cs_axiom('01a15821-4a45-46b3-8a4b-6e59f6428c07', foundational, free_movement_is_revocable_treaty_policy).
narrative_ontology:cs_axiom_status(free_movement_is_revocable_treaty_policy, holdable).
narrative_ontology:cs_axiom_grounding('01a15821-4a45-46b3-8a4b-6e59f6428c07', free_movement_is_revocable_treaty_policy, conventional).
narrative_ontology:cs_reference_frame('01a15821-4a45-46b3-8a4b-6e59f6428c07', westphalian_treaty_reservation).
narrative_ontology:cs_drift_state('01a15821-4a45-46b3-8a4b-6e59f6428c07', post_enlargement_labor_mobility_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('01a15821-4a45-46b3-8a4b-6e59f6428c07', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_border_agencies).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, domestic_incumbent_workers).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, binational_families).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, conditional_treaty_membership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers entry permits, work authorization quotas, and emergency border-control clauses that member states retain under the treaty's sovereignty carve-outs. Sets the terms under which free movement is suspended or renegotiated, framing this as the state's non-negotiable prerogative rather than a federation grant.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_border_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Domestic employers, unions, and sectoral bodies that benefit when mobility restrictions cap the inflow of competing labor, stabilizing wages and hiring pipelines for incumbents. They lobby to keep free movement classified as a revocable policy rather than a fixed right.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Workers whose bargaining position and job security benefit from reduced cross-border competition when quotas tighten. Their gain is real but diffuse and politically invoked more than individually collected.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, domestic_incumbent_workers, beneficiary,
    moderate, biographical, constrained, national).

% Federation citizens who planned relocation, work, or residence across borders under the expectation of free movement, now facing quotas, permit delays, or outright suspension invoked under the sovereignty clause. Their plans and investments (housing, employment contracts, family arrangements) are stranded when a member state re-asserts border authority.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, continental).

% Workers who commute or seasonally migrate across the border for employment now dependent on renewable permits and quota allocations set unilaterally by the receiving state. A permit non-renewal can end their livelihood overnight with no federation-level appeal mechanism that binds the state.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    powerless, immediate, trapped, regional).

% Households spanning two member states whose residence, custody, and reunification arrangements depend on continued mutual recognition of movement rights. A sovereignty-invoked border tightening can separate family members or force relocation with no recourse beyond the state's own domestic courts.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, binational_families, payer,
    powerless, biographical, trapped, regional).

% The supranational administrative body that would, under the rival integration reading, treat free movement as a constitutional guarantee it enforces. Under this sovereignty reading it is structurally sidelined: its objections are advisory only, and member states are not bound to accept its interpretation of the treaty's movement clauses.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_secretariat, excluded,
    institutional, generational, analytical, continental).

% Adjudicates disputes over whether a member state's invocation of the border-authority clause is a legitimate exercise of retained sovereignty or a treaty breach. Its rulings are persuasive but not self-executing against a resistant member state.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, treaty_arbitration_panel, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to pool trade, regulatory, and diplomatic benefits of federation membership while each retains the ability to unilaterally reassert border control and gate labor mobility when domestic political or economic conditions demand it — coordination with an opt-out valve preserved by design.
% TRANSFER_FUNCTION: Moves the stability of wages and labor-market position toward domestic incumbents and local employers, and moves the cost of unpredictability — stranded relocation plans, lost work authorization, disrupted family reunification — onto mobile citizens, cross-border workers, and binational families whenever a member state invokes its retained authority.
% ABSENT_VOICES: The federation secretariat and pan-federation civil-society groups representing mobile citizens would argue free movement should be a constitutional entitlement immune to unilateral suspension; under this reading their voice is advisory and non-binding, structurally outside the decision that actually gates entry.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading's border-authority clause vanished overnight, member states could no longer unilaterally suspend or ration free movement; mobile citizens, cross-border workers, and binational families would gain durable, judicially enforceable mobility rights, while local labor markets would lose their principal lever for capping inflows — labor markets, permit bureaucracies, and family-reunification practice would all reorganize.
% FOUNDING_PROBLEM: Founding member states needed a mechanism to join a federation for trade and regulatory coordination benefits without permanently surrendering control over who crosses their borders, particularly during asymmetric shocks (regional unemployment spikes, security crises) that a fixed integration commitment could not accommodate.
% FOUNDING_PROBLEM_CORROBORATION: Member-state border agencies and domestic labor organizations attest the founding problem (need for crisis-responsive border flexibility) remains live, citing periodic labor-market shocks. The federation secretariat and independent migration-policy researchers outside the beneficiary set attest the clause is invoked disproportionately for domestic political signaling rather than genuine emergency response, and that the harms to mobile populations now exceed the coordination benefit it was designed to preserve.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 by interval end: the constraint transfers labor-market stability to domestic incumbents at direct cost to mobile citizens and cross-border workers whose relocation and employment plans are stranded by unilateral suspension. Suppression (0.62) reflects that exit is genuinely constrained for cross-border workers and binational families — there is no federation-level appeal that binds a resisting member state, so the suppression is structural rather than merely bureaucratic friction. Theater ratio (0.40) captures that a meaningful share of border-authority invocations now function as domestic political signaling (demonstrating sovereignty to a domestic audience) rather than addressing a genuine labor-market emergency — this share has grown over the interval as the clause has been invoked more frequently for reasons unrelated to acute shock.
 *
 * PERSPECTIVAL GAP:
 *   From the national_border_agencies seat, this is a legitimate retained sovereign function exercised sparingly and defensibly. From the cross_border_workers and binational_families seats, the same mechanism computes as high-suppression extraction with no meaningful appeal. The engine computes this divergence from the differing power/exit/scope data authored per seat; this story does not adjudicate which seat is 'correct' — it authors the structural facts and lets per-seat computation surface the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Local labor markets and domestic incumbent workers sit near the beneficiary end: they gain a durable structural advantage (reduced labor competition) each time the sovereignty clause is invoked, at low direct cost to themselves. Mobile citizens sit nearer the target end because their relocation and career plans, made under a federation-membership expectation, can be nullified unilaterally. Cross-border workers and binational families sit at the extreme target end: trapped exit options (no alternative labor market or family-reunification path exists that does not depend on the contested border), immediate time horizon (permit renewal is often annual or shorter), and no binding appeal mechanism. National border agencies are the agenda-setters rather than beneficiaries in the narrow sense — they administer and can change the arrangement, but the concentrated gain (wage and hiring stability) accrues to local labor markets, not to the agency itself, which is why this reads as tangled_rope (coordination for federation-membership benefits + concentrated extraction from mobile populations via the same border-authority mechanism) rather than a piton.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — crisis-responsive border flexibility for asymmetric shocks — is contested as still live: the coordination function (retaining ability to respond to genuine regional unemployment shocks) has a real basis, but the founding_problem_corroboration shows independent migration researchers assessing that the clause is invoked disproportionately for domestic political signaling now, well beyond genuine emergency response. This divergence between the sunset-worthy justification (temporary crisis response) and the now-routine invocation pattern (rising theater_ratio, rising suppression_requirement) is exactly the signature classification should surface as tangled_rope with a hardening enforcement trend rather than either a clean rope (if invocation were rare and crisis-limited) or a pure snare (if there were no genuine underlying coordination function at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_vs_integration_reading,
    'Is the treaty''s movement clause correctly read as a revocable national prerogative (sovereignty_reading) or an irreversible constitutional commitment (integration_reading)? The two readings are not measurement variants of one constraint — they instantiate structurally distinct constraints with different authority seats and different victim/beneficiary sets.',
    'A binding ruling by the treaty_arbitration_panel or a constitutional-court-equivalent body settling which reading has final interpretive authority would resolve which constraint actually governs; absent that, both readings remain live and are authored as separate linked constraint stories per the ε-invariance principle.',
    'If the integration_reading''s supranational-authority premise prevails as binding, member states lose the ability to unilaterally invoke the border-authority clause, and this constraint (sovereignty_reading) would dissolve into the mechanism the integration_reading already describes — mobile citizens'' victim status would end and local_labor_markets would lose their principal lever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_reading_vs_integration_reading, conceptual, 'Committer-level ambiguity between the two kernel readings; routed to omega rather than folded into this constraint''s classification per Kernel Rule 2.').

omega_variable(
    genuine_emergency_vs_political_signaling,
    'What proportion of actual border-authority invocations under this reading respond to a genuine, verifiable labor-market or security shock, versus function as domestic political signaling with no underlying emergency?',
    'Independent audit of invocation triggers against contemporaneous labor-market and security data, cross-referenced against the treaty_arbitration_panel''s case record and academic migration-policy analysis outside the beneficiary set.',
    'A high genuine-emergency share would support the coordination-function reading (rope-leaning); a low share, consistent with the rising theater_ratio trend authored here, would support reclassifying the constraint''s trajectory toward snare as the coordination justification erodes into pure political extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_emergency_vs_political_signaling, empirical, 'Whether border-authority invocation tracks genuine crisis or has drifted into routine political signaling.').

omega_variable(
    national_sovereignty_natural_or_constructed,
    'Is retained national border authority within a federation a natural/default state of sovereign statehood, or is it itself a constructed carve-out negotiated into the treaty that could equally have been negotiated away (as the integration_reading asserts happened in other federations)?',
    'Comparative federalism analysis: examine federations (e.g. fully integrated internal labor markets) where equivalent border-authority carve-outs were negotiated away entirely, to test whether retained sovereignty is a structural necessity or a contingent, reversible bargaining outcome.',
    'If retained sovereignty is shown to be a contingent, reversible bargain rather than a necessary feature of federation, the vindicated_propositions (national_sovereignty_doctrine, conditional_treaty_membership_doctrine) lose force as natural-law-adjacent justifications and the constraint''s coordination claim weakens further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_sovereignty_natural_or_constructed, conceptual, 'Whether the sovereignty premise is naturalized or a negotiated, reversible construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fede_tr_t4, federation_membership__sovereignty_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(fede_tr_t8, federation_membership__sovereignty_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(fede_tr_t12, federation_membership__sovereignty_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(fede_tr_t16, federation_membership__sovereignty_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(fede_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t4, federation_membership__sovereignty_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(fede_be_t8, federation_membership__sovereignty_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(fede_be_t12, federation_membership__sovereignty_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(fede_be_t16, federation_membership__sovereignty_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(fede_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t4, federation_membership__sovereignty_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(fede_su_t8, federation_membership__sovereignty_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(fede_su_t12, federation_membership__sovereignty_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(fede_su_t16, federation_membership__sovereignty_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(fede_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This story and federation_membership__integration_reading form a two-member constraint family decomposing the colloquial concept 'federation membership and free movement' per the ε-invariance principle. This reading (sovereignty_reading) authors ε=0.68 driven by mobility restriction extracted from mobile populations via retained national border authority; the sibling (integration_reading) would author a different ε driven by supranational override of domestic labor-market protections. They share a kernel (the founding treaty's movement/sovereignty clauses) but are structurally distinct constraints with different agenda-setters, beneficiaries, and victims — not two measurements of the same thing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
