% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Militia-Conditioned Individual Right to Arms (Civic Right Reading)
 *   domain: constitutional law/political theory/rights jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   second_amendment_scope: the civic right reading, under which the Second
 *   Amendment protects an individual right to arms that is constituted
 *   through — and conditioned on — civic militia participation. The
 *   arrangement the story is about is the militia-conditioned arms regime as
 *   this reading holds it: enrollment, training obligation, eligibility
 *   administration, and protected status for those who serve. Per the
 *   epsilon-invariance principle, the sibling readings
 *   (individual_right_reading, collective_right_reading) are separate
 *   constraints in separate files with their own beneficiary/victim
 *   structures and their own epsilon values; nothing about them is averaged
 *   into this file. The epsilon referent is the standing militia-conditioned
 *   arrangement itself, assessed by this reading's own lights — not the
 *   unconditioned regime the sibling reading would endorse. KEY AGENTS (by
 *   structural relationship): see key_agents; the load-bearing asymmetry is
 *   between the administering authorities, who define the gate and collect
 *   compliance, and the three payer seats, who bear the condition's costs
 *   from structurally different positions.
 *
 * KEY AGENTS:
 *   - serving_militia_members: participant-beneficiary (organized/constrained) — holds protected arms-bearing status, owes service, training, and risk of death; exit from burden equals exit from benefit
 *   - state_militia_authorities: agenda-setting administrator-beneficiary (institutional/arbitrage) — defines eligibility, calibrates enforcement, collects compliance and gatekeeping discretion
 *   - federal_government: secondary agenda-setter and beneficiary (institutional/arbitrage) — receives distributed defense capacity without a standing army, holds organize/arm/discipline powers
 *   - nonparticipating_citizens: primary target (moderate/constrained) — stands outside the protected class unless compliant; no third position inside the arrangement
 *   - conscientious_objectors: identity-locked target (powerless/identity_locked) — the purchased path violates conscience; the exit is who they are
 *   - militia_ineligible_groups: gate-defined outsider (powerless/trapped) — excluded from both burden and benefit; disarmed when the gate tightens
 *   - personal_defense_only_owners: excluded constituency (organized/constrained) — wants the right unconditioned; unseated in the civic bargain
 *   - judiciary: analytical observer (institutional/analytical) — adjudicates which reading governs; re-weights the condition without administering it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.22).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.74).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Militia-Conditioned Individual Right to Arms (Civic Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional law/political theory/rights jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '33be8e69-0219-42bc-9355-6a2649afe7a6').
narrative_ontology:cs_kernel_codification('33be8e69-0219-42bc-9355-6a2649afe7a6', fixed_text).
narrative_ontology:cs_authority_grounding('33be8e69-0219-42bc-9355-6a2649afe7a6', lineage).
narrative_ontology:cs_interpretation_layer_present('33be8e69-0219-42bc-9355-6a2649afe7a6').
narrative_ontology:cs_reading_relation('33be8e69-0219-42bc-9355-6a2649afe7a6', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('33be8e69-0219-42bc-9355-6a2649afe7a6', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('33be8e69-0219-42bc-9355-6a2649afe7a6', foundational, arms_right_constituted_by_civic_obligation).
narrative_ontology:cs_axiom_status(arms_right_constituted_by_civic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('33be8e69-0219-42bc-9355-6a2649afe7a6', arms_right_constituted_by_civic_obligation, deontological).
narrative_ontology:cs_axiom('33be8e69-0219-42bc-9355-6a2649afe7a6', secondary, regulated_militia_preferred_to_standing_army).
narrative_ontology:cs_axiom_status(regulated_militia_preferred_to_standing_army, holdable).
narrative_ontology:cs_axiom_grounding('33be8e69-0219-42bc-9355-6a2649afe7a6', regulated_militia_preferred_to_standing_army, instrumental).
narrative_ontology:cs_reference_frame('33be8e69-0219-42bc-9355-6a2649afe7a6', founders_militia_fusion_settlement).
narrative_ontology:cs_drift_state('33be8e69-0219-42bc-9355-6a2649afe7a6', contemporary_post_guard_transition, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('33be8e69-0219-42bc-9355-6a2649afe7a6', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, serving_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_militia_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, nonparticipating_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, conscientious_objectors).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, militia_ineligible_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, serving_militia_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enrolled citizens on the militia rolls. They hold constitutional protection for their arms and the civic standing that attaches to service, and in exchange they owe musters, training days, equipment readiness, and service when called, with the attendant risk of death. Leaving the rolls forfeits the protected status the condition confers, so exit from the burden is simultaneously exit from the benefit.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, serving_militia_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, serving_militia_members, payer).

% Governors, adjutants general, and enrollment officers who administer the condition. They set training standards, determine who counts as militia-eligible, decide how vigorously muster laws are prosecuted, and collect compliance, service labor, and gatekeeping discretion from running the arrangement. They can tighten or loosen the gate within statutory bounds without surrendering the gate itself.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, state_militia_authorities, beneficiary).

% Holds the Article I powers to organize, arm, and discipline the militia and receives a defense capacity distributed across the citizenry without maintaining a large standing professional army. Bears the costs of arming the force and of federal-state friction over command and standardization, and can reshape the system through Congress without touching the constitutional condition.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, federal_government, agenda_setter).

% Militia-eligible adults who decline service or lapse from the rolls. Under the conditioning they stand outside the protected class: keeping arms becomes legally precarious, subject to the discretion of the authorities who administer eligibility. Their path back into protection runs through compliance with the service condition; there is no third position inside the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, nonparticipating_citizens, payer,
    moderate, biographical, constrained, national).

% Persons whose conscience forbids military service in any form. The condition offers them a binary: violate conscience by mustering, or forfeit constitutional protection for their arms. The objection is constitutive of who they are, so the available exit — abandoning the objection — is not an exit they can take without ceasing to be themselves.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, conscientious_objectors, payer,
    powerless, biographical, identity_locked, national).

% Populations the eligibility gate places outside the arrangement entirely — by race, loyalty screening, or property qualification, depending on the period. Excluded from both the service burden and the protection it purchases, they hold neither the right nor the civic standing service confers. Historically the gate was tightened precisely to disarm them, as in the post-Reconstruction period, and eligibility is defined by the very authorities they would have to persuade.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_ineligible_groups, payer,
    powerless, generational, trapped, national).

% Owners whose interest in arms is private self-protection rather than civic service. The conditioning subordinates their use-case to militia duty and treats their ownership as legitimate only to the extent it feeds the civic function. They have no seat in the bargain the reading constructs; their objections surface only through litigation and politics aimed at an unconditioned right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, personal_defense_only_owners, excluded,
    organized, biographical, constrained, national).

% Adjudicates which reading of the amendment governs. Weighs founding-era militia practice, textual argument, and contemporary conditions; its rulings re-weight the force of the conditioning without administering it, and successive rulings have moved the operative constraint between this reading and its siblings.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, state_militia_authorities).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the polity's common defense through citizen-soldiers: distributes arms-bearing together with military obligation, producing a defense force without a large standing professional army, and binds private armament to public accountability through enrollment, training, and command structure.
% TRANSFER_FUNCTION: Moves military service obligation, training time, and risk of death from militia-eligible individuals to the public defense effort; moves regulatory discretion over who may bear arms to the authorities administering eligibility; moves protected arms-bearing status and civic standing back to those who serve.
% ABSENT_VOICES: Personal-defense-only owners, conscientious objectors, and the populations barred from militia eligibility had no seat in the founding settlement that fixed the condition; their objections enter only through later amendment, statute, and litigation. The founding bargain was struck among civic-republican elites, and the unanimity of the founding generation on militia-fusion partly reflects who was in the room.
% DISAPPEARANCE_RATIONALE: If the conditioning vanished overnight, civic republicans expect the civic infrastructure of accountable arms-bearing — the fusion of right and duty — to collapse, with arms-bearing drifting to a purely private entitlement; pragmatists observe that material defense arrangements long ago migrated to statutory channels and the professionalized Guard, so little would rearrange in practice. The parties dispute which world we are in.
% FOUNDING_PROBLEM: Securing the new republic against invasion and insurrection without a large standing professional army, which founding-generation republicans regarded as an inherent instrument of tyranny; the militia-conditioned right fused private arms to public defense duty so that the means of resistance and the means of defense were the same institution.
% FOUNDING_PROBLEM_CORROBORATION: Military historians document the collapse of the compulsory muster system through absenteeism and desuetude across the nineteenth century, and the National Defense Act of 1903 is itself a congressional finding that the volunteer militia system was superseded by an organized, federally standardized force — attestation from outside the benefiting parties. No serious external source attests that the no-standing-army problem remains live in its original form.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim tangled_rope because the arrangement, over the arc of its life, genuinely possessed both faces: a real coordination function (common defense without a standing army, arms bound to accountability) AND asymmetric extraction (service obligation and risk taken from participants, protection withheld from non-participants, eligibility discretion captured by administrators, and a gate repeatedly tightened to disarm the disfavored). The metrics describe actual operation on one shared time grid (T0 approx. 1791 ratification through T120 approx. 1911, after the National Defense Act reorganization). Base extractiveness rises monotonically (0.40 to 0.68) as the coordination return decays while obligations, exclusions, and administrative discretion persist — extraction accumulating on top of a shrinking coordination base. Theater ratio rises steeply (0.10 to 0.74) tracking the well-documented collapse of compulsory muster into farce and then into purely rhetorical invocation; by interval end the conditioning is mostly performance, which is why the end-state profile approaches a piton signature even though the life-of-arrangement claim is tangled_rope — that divergence is the drift signal, not an inconsistency. Suppression_requirement is deliberately NON-monotonic: high at T0 (compulsory musters backed by fines and law), decaying through mid-century desuetude (0.62 down to 0.36), spiking at T80 (approx. 1871, when militia-adjacent statutes were actively enforced to disarm newly enfranchised populations), then collapsing (0.22) as enforcement machinery died with the system it served. Accessibility_collapse is low (0.35) because the alternatives — an unconditioned individual right, a states-only collective right, a professional army — never collapsed; they remain the live sibling readings and rival institutions. Resistance (0.55) is a life-of-arrangement value: substantial wherever enforcement actually bit (anti-muster resistance, objections by the excluded), declining to near-zero as the constraint became inert. Base-property scalars describe the standing arrangement at interval end except where noted.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the state_militia_authorities seat the arrangement is stewardship: a civic infrastructure they administer, with the gate as a legitimate instrument of accountability — low experienced extraction. From the serving_militia_members seat it is a fair trade only so long as the militia musters, trains, and delivers protection; as delivery decays, the same obligation reads increasingly as uncompensated taking. From the nonparticipating_citizens and militia_ineligible_groups seats the identical structure operates as a condition converting a right into a privilege contingent on obedience or gatekeeper favor — high experienced extraction with no offsetting delivery. The judiciary seat sees a contest among readings rather than any of these lived positions. The engine computes these divergences from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. state_militia_authorities and federal_government sit near the beneficiary pole (they collect compliance, discretion, and defense capacity; their exit is arbitrage-grade since they define the terms). nonparticipating_citizens, conscientious_objectors, and militia_ineligible_groups sit near the full-target pole, with identity_lock and trap statuses pushing them further toward full target than their raw power levels suggest. One override is authored: serving_militia_members (power atom 'organized', d 0.4). The structural derivation from their beneficiary declaration alone would place them near the beneficiary end, but that misreads their position: they simultaneously bear the service obligation, training burden, and mortality risk the condition extracts, and their exit (leaving the rolls) forfeits the benefit — a genuinely dual position best modeled near symmetric. No other override is needed; the derivation handles the remaining seats correctly from declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — republican common defense without a standing army — is dead: the standing army won, the Guard was federalized and professionalized, and compulsory universal service is extinct. The conditioning nonetheless persisted as doctrine and rhetoric long after its function atrophied, which is the classic mandatrophy shape, and the R5 mismatch (status dead x disappearance verdict contested) flags the zombie tendency for the engine to cross-check against the computed theater path. The tangled_rope classification prevents two opposite mislabelings: reading the arrangement as pure snare would erase decades of genuine coordination (the militia actually was the republic's defense instrument through the early national period, and participants really did receive what the condition promised); reading it as pure rope would erase the gate's extractive career (politicized eligibility, protection withheld from non-compliers, and the gate's repeated use as a disarmament instrument against the excluded). Holding both faces in one classification lets the temporal series carry the degradation: rising theater and rising extraction-on-declining-function are exactly the signature that distinguishes a tangled rope drifting toward piton from either a stable hybrid or a live scaffold. Whether the end-state is best read as degraded piton or as dormant-and-revivable is left open as an omega rather than forced here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus,
    'This story is one reading (civic_right_reading) of the kernel second_amendment_scope; the sibling readings individual_right_reading and collective_right_reading instantiate structurally different constraints — where exactly does the contest bind, and what would adopting a sibling change?',
    'Adopting a sibling reading changes the bearer of the right and the conditioning element: individual_right_reading removes the service gate (beneficiary class becomes all owners, regulatory authority contracts, epsilon falls); collective_right_reading removes the individual bearer entirely (states become the protected party, individual payers vanish from the accounting). Cross-reading comparison must run through the linked family, not this file alone.',
    'Classification is reading-relative over a shared text; treating any single file''s verdict as the verdict on ''the Second Amendment'' conflates three constraints with different victim sets and different epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement is located.').

omega_variable(
    eligibility_gate_manipulability,
    'Who counts as militia-eligible is administratively determined — how far can the gate move, in practice, without formal constitutional revision?',
    'Comparative statutory and enrollment-record history across jurisdictions and periods: property qualifications, race-based exclusions, loyalty screenings, and their reversal or tightening by the administering authorities.',
    'The victim-set size and measured epsilon swing widely with gate breadth; sustained discretionary contraction of eligibility pushes the arrangement''s operation toward snare-flavored extraction through nominally civic machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_gate_manipulability, empirical, 'How much of the constraint''s extraction profile is gate-administration discretion rather than fixed structure.').

omega_variable(
    service_reciprocity_fairness,
    'Is the service obligation a fair price for protected status — does the militia actually muster, train, and deliver defense — or does the condition take obligation while the coordinated good decays?',
    'Compare mustering and training rates, actual defensive employment of the force, and protection delivered against obligations imposed, period by period across the interval.',
    'High delivery sustains the rope face and keeps the arrangement a genuine trade; delivery collapse with obligations retained converts the same structure into extraction without coordination, driving the piton-drift reading of the end state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_reciprocity_fairness, empirical, 'Whether the conditioning''s extraction is reciprocated by the coordination good it purchases.').

omega_variable(
    end_state_piton_or_hibernation,
    'Is the late-interval arrangement a degraded piton — function dead, persistence carried by inertia and civic-republican rhetoric — or a dormant arrangement awaiting revival by civic-republican movements proposing renewed universal service?',
    'Whether revival efforts achieve statutory re-fusion of service obligation and arms-bearing (universal service legislation, Guard restructuring toward civic-militia form), versus continued rhetorical invocation without institutional uptake.',
    'Piton reading resolves mandatrophy as terminal and points to decommission; hibernation reading treats the interval-end theater as transitional and expects re-coordination, restoring the tangled_rope reading with falling theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_state_piton_or_hibernation, conceptual, 'Whether end-state persistence is inertia or suspended function.').

omega_variable(
    reconstruction_suppression_spike_attribution,
    'Was the suppression spike near T80 (approximately 1871) an intrinsic enforcement ratchet of this constraint, or an exogenous racial-control project routed through the militia gate?',
    'Comparative statutory history separating enforcement of militia-conditioning proper (muster law, enrollment obligation) from standalone disarmament codes that borrowed militia vocabulary and eligibility machinery.',
    'Intrinsic attribution keeps the spike inside this constraint''s suppression account; exogenous attribution moves it to a separate disarmament constraint connected by network edge, lowering this story''s suppression attribution and sharpening the end-state decay picture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_suppression_spike_attribution, empirical, 'Attribution of the mid-series enforcement spike to this constraint versus a neighboring one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(seco_tr_t60, second_amendment_scope__civic_right_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(seco_tr_t80, second_amendment_scope__civic_right_reading, theater_ratio, 80, 0.6).
narrative_ontology:measurement(seco_tr_t100, second_amendment_scope__civic_right_reading, theater_ratio, 100, 0.68).
narrative_ontology:measurement(seco_tr_t120, second_amendment_scope__civic_right_reading, theater_ratio, 120, 0.74).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(seco_be_t60, second_amendment_scope__civic_right_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement(seco_be_t80, second_amendment_scope__civic_right_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(seco_be_t100, second_amendment_scope__civic_right_reading, base_extractiveness, 100, 0.66).
narrative_ontology:measurement(seco_be_t120, second_amendment_scope__civic_right_reading, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(seco_su_t60, second_amendment_scope__civic_right_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(seco_su_t80, second_amendment_scope__civic_right_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(seco_su_t100, second_amendment_scope__civic_right_reading, suppression_requirement, 100, 0.34).
narrative_ontology:measurement(seco_su_t120, second_amendment_scope__civic_right_reading, suppression_requirement, 120, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, resource_allocation).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Second Amendment' covers three structurally distinct claims that differ in the bearer of the right and in whether militia participation conditions it. This file is the civic_right_reading member (service-gated individual right; moderate regulatory authority; moderate epsilon with the gate as the extraction surface). The individual_right_reading member has a broader beneficiary class, weaker regulatory authority, and lower epsilon; the collective_right_reading member has state governments as the protected party and individual owners outside the accounting entirely. Because epsilon differs across the members, they are modeled as separate stories linked by affects_constraints rather than one story with a measurement parameter; the upstream founding-era settlement cited by each reading is the shared ancestry the family links encode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__civic_right_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
