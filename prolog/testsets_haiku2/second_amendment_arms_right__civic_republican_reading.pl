% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment: Armed Citizenship as Civic Republican Duty-Right
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The civic-republican reading of the Second Amendment frames the right to
 *   bear arms as grounding armed citizenship — a dual relationship where
 *   citizens possess the RIGHT to maintain arms in organized militia capacity
 *   AND the DUTY to remain trained and accountable to civic standards. This
 *   reading positions the right neither as pure individual liberty
 *   (libertarian reading) nor as state militia monopoly (collective reading),
 *   but as a constitutional balance: regulatory authority can require
 *   training, organization, and qualification WITHOUT violating the right;
 *   conversely, regulatory authority cannot functionally disarm the organized
 *   citizenry or render civic defense structurally impossible. The constraint
 *   is CLAIMED as rope (genuine coordination solving the tyranny-prevention
 *   problem) and the metrics describe moderate extraction relative to
 *   individual-right readings but lower extraction than regulatory regimes
 *   that lack civic-participation grounding. The claim and metrics are
 *   independent authored facts — the engine computes divergence.
 *
 * KEY AGENTS:
 *   - armed_citizens_organized_militia: civic participants who benefit from the right but also bear training duty
 *   - regulatory_authority_civic_constrained: federal/state authorities that can regulate arms within civic boundaries
 *   - courts_interpreting_authority: adjudicate whether regulations preserve civic capacity
 *   - libertarian_individualist_reading_constituency: excluded — their individual-ownership frame is outside THIS reading
 *   - collective_militia_authority_constituency: excluded — their state-monopoly frame is outside THIS reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.29).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment: Armed Citizenship as Civic Republican Duty-Right").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '4cd8e3fc-2368-4c06-8c88-c4410388d073').
narrative_ontology:cs_kernel_codification('4cd8e3fc-2368-4c06-8c88-c4410388d073', fixed_text).
narrative_ontology:cs_authority_grounding('4cd8e3fc-2368-4c06-8c88-c4410388d073', lineage).
narrative_ontology:cs_interpretation_layer_present('4cd8e3fc-2368-4c06-8c88-c4410388d073').
narrative_ontology:cs_reading_relation('4cd8e3fc-2368-4c06-8c88-c4410388d073', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cd8e3fc-2368-4c06-8c88-c4410388d073', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('4cd8e3fc-2368-4c06-8c88-c4410388d073', foundational, armed_citizenship_civic_participation_binding).
narrative_ontology:cs_axiom_status(armed_citizenship_civic_participation_binding, holdable).
narrative_ontology:cs_axiom_grounding('4cd8e3fc-2368-4c06-8c88-c4410388d073', armed_citizenship_civic_participation_binding, deontological).
narrative_ontology:cs_axiom('4cd8e3fc-2368-4c06-8c88-c4410388d073', foundational, regulatory_authority_constrained_by_civic_capacity).
narrative_ontology:cs_axiom_status(regulatory_authority_constrained_by_civic_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4cd8e3fc-2368-4c06-8c88-c4410388d073', regulatory_authority_constrained_by_civic_capacity, deontological).
narrative_ontology:cs_reference_frame('4cd8e3fc-2368-4c06-8c88-c4410388d073', republic_armed_citizenry_tyranny_check).
narrative_ontology:cs_drift_state('4cd8e3fc-2368-4c06-8c88-c4410388d073', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cd8e3fc-2368-4c06-8c88-c4410388d073', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, armed_citizens_organized_militia).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_structure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, armed_citizens_organized_militia).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, armed_populace_tyranny_prevention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who organize in trained militia or civic-participation structures derive both the right to bear arms AND the duty to train and remain accountable to civic standards. They benefit from constitutional protection against federal disarmament and from the normative status that comes with participation in self-defense structures. They also bear the obligation to meet qualification and training standards, distinguishing them from purely individualistic arms ownership.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, armed_citizens_organized_militia, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, armed_citizens_organized_militia, payer).

% States and federal government regulate arms access and militia organization subject to the constraint: regulation must not render the armed citizenry structurally unable to perform its republican function. Authorities can impose training requirements, background checks, and participation criteria WITHOUT running afoul of the right — the right protects capability for organized civic defense, not unregulated individual ownership. Enforcement machinery distinguishes civic-organized arms bearing from purely commercial or libertarian individualism.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, regulatory_authority_civic_constrained, agenda_setter,
    institutional, generational, analytical, national).

% The abstract institutional structure that derives legitimacy from the claim that an armed citizenry prevents tyranny. This is a vindicated proposition — the constraint's operation vindicates this doctrine — not a collecting agent.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, tyranny_prevention_institution, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, tyranny_prevention_institution).

% Exercises primary regulatory authority over arms policy under the constraint: may regulate type, training, background checks, and militia standards but cannot functionally disarm the organized citizenry or substitute standing military for civic-defense capacity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_congress, agenda_setter,
    institutional, generational, analytical, national).

% Exercise concurrent regulatory authority, defining militia organization, training standards, and local arms regulations within the bounds set by the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_legislatures, agenda_setter,
    institutional, generational, analytical, regional).

% Advocates for reading the right as protecting purely individual ownership unconstrained by militia duty or civic participation requirements. This reading is structurally excluded by the civic-republican framing: the right in THIS constraint IS the duty, not an individual liberty separable from civic obligation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, libertarian_individualist_reading_constituency, excluded,
    organized, biographical, mobile, national).

% Advocates for reading the right as protecting only state-organized militia authority, not individual participation. This reading is excluded by the civic-republican framing: the right protects ARMED CITIZENS as organized militia members, not state militia exclusively.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, collective_militia_authority_constituency, excluded,
    organized, biographical, mobile, national).

% Federal courts adjudicate disputes over what regulatory schemes conform to the constraint — whether a given regulation preserves the armed citizenry's capacity for civic self-defense or impermissibly disarms it. The courts read the framing and its boundary (trained/organized vs. purely individual; civic duty + right vs. unconstrained ownership).
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, courts_interpreting_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates defense of the republic against tyranny through an armed citizenry organized at the civic level — neither centralized standing military alone nor purely private individuals, but trained citizen-defenders accountable to civic standards and bound by republican duty. Solves the founding problem: how to prevent a standing military from becoming an instrument of tyranny while maintaining defense capacity.
% TRANSFER_FUNCTION: Transfers authority and responsibility: individual citizens receive the RIGHT to bear arms (protection against federal disarmament); the state/federal authority receives the regulatory power to require TRAINING, ORGANIZATION, and ACCOUNTABILITY in exchange for the right. The constraint moves civic burden upward (citizens must participate in militia structure, meet standards) and legal permission downward (protection against confiscation, unilateral federal disarmament).
% ABSENT_VOICES: Libertarian individualists and collective-militia-authority readers are excluded from THIS reading's framework. They would argue: libertarians that the right protects personal choice independent of civic duty; collectivists that state militia interests override individual arms possession. The civic-republican reading brackets both by making the right INHERENTLY tied to organized civic participation.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, regulatory authority would face no constitutional boundary on federal or state disarmament of citizens. The civic-republican doctrine that motivates armed citizenry as tyranny-prevention would lose constitutional grounding. The balance between state regulatory authority and citizen right to organize would collapse toward either unconstrained federal authority or unconstrained individual ownership (depending on which reading replaced it), and the republican self-governance structure would reorganize around centralized or purely individual armed force.
% FOUNDING_PROBLEM: The founders sought to prevent a standing military from becoming an instrument of tyranny by constitutionally protecting an armed citizenry capable of civic defense. Armed citizens in organized militia form — trained, accountable, and performing the civic duty of self-defense — serve as a constitutional check on military monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Federalist Papers (especially No. 29, Hamilton on militia) and founding-era militia records attest the founding problem and the civic-republican framing. Modern historians (Garry Wills, Jack Rakove) and militia scholars corroborate the civic-duty reading from outside the gun-rights advocacy community. The competing readings (individual-right and collective-militia) are also corroborated by other historical voices — the contest is live across the evidence base.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 end-state) because the constraint does impose regulatory burden on citizens (training, organizational participation, accountability) but grounds that burden in civic duty rather than arbitrary state authority. The trajectory rises from 0.22 to 0.38 as case law and regulatory practice accumulate — the constraint becomes more extractive as the state uses its authority to tighten qualification and training requirements, and as legal interpretation elaborates the boundary between protected civic arms bearing and unprotected individual accumulation. Theater ratio is low (0.22) because the civic-duty framing makes genuine enforcement and participation requirements, though theater does rise over time as compliance becomes more rhetorical and less substantive. Suppression is low (0.29) because the constraint protects citizens against disarmament; suppression rises modestly as the state interprets its regulatory authority more expansively. The shared time grid ensures every metric is authored at every examined point (7 points spanning 50 years).
 *
 * PERSPECTIVAL GAP:
 *   From the armed citizen's seat: the right is a protection against disarmament, and the training/duty requirement is legitimate because it constitutes the civic act the right protects. From the regulatory authority's seat: the right is a constraint limiting how far they can regulate, but the civic-participation norm lets them impose substantial requirements (training, militia membership, background checks) without violating it. From a pure libertarian's seat: the requirement to join civic structures to access the right IS extraction (not a true right but a privilege conditional on state-approved participation). From a state-power seat: the civic-republican constraint LIMITS state authority by protecting armed citizens and forbidding disarmament. The engine computes per-seat type from the power/exit atoms; these four seats should diverge visibly.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens (organized militia members) sit as BENEFICIARIES on the dimension of rights protection — they are protected from federal disarmament — but also as partial PAYERS on the dimension of civic duty and regulatory requirement. Their directionality is asymmetric: d near 0.4 rather than pure 0.0 (beneficiary) or 1.0 (target), reflecting the dual benefit-and-burden structure. Regulatory authority also sits asymmetrically: as agenda-setter (power to regulate) but constrained by the civic-participation norm (cannot functionally disarm). The libertarian and collectivist reading constituencies are EXCLUDED from this framing — not coordinated, not targeted, just outside the frame entirely. The courts are pure observers (analytical seat). This distributes directionality across the seats differentially: the civic-republican reading produces a fundamentally different d-distribution than the individual-right reading (which would make armed citizens pure beneficiaries with d near 0.0) or the collective reading (which would make regulatory authority the beneficiary with d near 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The civic-republican constraint avoids mandatrophy by tying the right directly to the founding problem it solves — tyranny prevention through armed civic capacity. The constraint would degrade into piton (performance without function) only if: (1) training requirements became ornamental rather than substantive, (2) militia organization became theatrical rather than functional, or (3) regulatory authority ignored the civic-participation norm and treated it as mere performance cover while pursuing unilateral disarmament. The measurement trajectory shows slight theater-ratio increase (0.08 to 0.22) — suggesting modest drift toward ornamental compliance — but theater remains well under piton threshold (0.5). The constraint remains alive: substantive training and civic participation structures persist, and courts continue to enforce the boundary. Mandatrophy would require a deeper institutional collapse of the civic-militia structure itself, not yet evidenced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_participation_vs_state_monopoly,
    'Does the civic-republican framing constrain state regulatory authority, or does it provide a mask for state authority to impose requirements on arms bearing by embedding them in ''civic duty'' language?',
    'Historical analysis of militia-regulation practices: do states systematically use civic-duty language to justify disarmament, or do courts enforce actual limits on state authority grounded in the civic-participation norm?',
    'If states use the frame as cover, the constraint becomes snare (extraction dressed as coordination). If courts enforce the boundary, it remains rope (genuine coordination with enforceable limits). The constraint''s type hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_participation_vs_state_monopoly, empirical, 'Whether civic-republican framing constrains or masks state regulatory power').

omega_variable(
    organized_militia_vs_individual_arms,
    'Can an armed citizen maintain the constitutional right outside organized militia structure, or does the civic-republican reading require formal militia participation to vindicate the right?',
    'Court interpretation of the boundary between protected civic-organized arms bearing and unprotected individual possession. Comparative analysis of militia-participation rates and legal enforcement of the participation requirement.',
    'If participation is required, the constraint is more extractive (duty binds all beneficiaries) and more selective in who qualifies as a protected ''armed citizen.'' If individuals can maintain the right outside militia, the constraint is less extractive and less participatory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organized_militia_vs_individual_arms, conceptual, 'Whether the civic-republican right binds to organized militia participation or protects individual arms bearing').

omega_variable(
    tyranny_prevention_assumption,
    'Does an armed citizenry actually prevent tyranny, or is this a foundational myth that justifies arms distribution?',
    'Comparative historical analysis of regime stability, tyranny-prevention capacity, and role of armed citizenry across democracies and autocracies. Empirical evaluation of whether arms-bearing populations systematically resist tyranny.',
    'If the assumption is false, the ''vindicated proposition'' (armed_populace_tyranny_prevention) is not vindicated by the constraint''s operation — the constraint becomes harder to justify as coordination and more vulnerable to reclassification as theater. If true, the constraint''s coordination function is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tyranny_prevention_assumption, empirical, 'Whether armed citizenry actually serves the tyranny-prevention function the constraint posits').

omega_variable(
    civic_reading_identity_fusion,
    'To what extent does the civic-republican reading fuse the reader''s own political identity (republican self-governance, anti-tyranny sentiment) with the constraint''s mechanism, making the reading harder to question?',
    'Measure reader response to counterfactuals (scenarios where armed citizenry fails or democracy survives disarmed) and alternative readings that disagree on the beneficiary structure while agreeing on the founding problem.',
    'If identity fusion is high, the constraint may rely on suppression (internalized rather than structural) — the civic-republican reader believes the frame so deeply that questioning it feels like questioning their own republican commitment. This would raise effective suppression and lower accessibility of alternatives for readers who adopt the framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_reading_identity_fusion, conceptual, 'Identity fusion between civic-republican reading and reader''s political identity').

omega_variable(
    kernel_reading_contest_relationship,
    'What is the structural relationship between THIS civic-republican reading and its sibling readings (individual_right and collective_right)? Do they coexist, or does one foreclose another?',
    'Formal analysis of the axioms and core premises: if two readings hold different core premises about what the right protects (individual liberty vs. civic duty vs. state militia), can both be true in the same legal framework?',
    'The cs_structure.reading_relations field declares coexists_with for both siblings — asserting that all three readings remain live positions. If this is wrong and one reading actually forecloses another, the classification of the kernel contest changes and the constraint''s role in the broader system shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_relationship, conceptual, 'Whether civic-republican reading structurally forecloses sibling readings or coexists with them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t8, second_amendment_arms_right__civic_republican_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(seco_tr_t8, observed).
narrative_ontology:measurement(seco_tr_t16, second_amendment_arms_right__civic_republican_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(seco_tr_t16, observed).
narrative_ontology:measurement(seco_tr_t24, second_amendment_arms_right__civic_republican_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(seco_tr_t24, observed).
narrative_ontology:measurement(seco_tr_t32, second_amendment_arms_right__civic_republican_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement_basis(seco_tr_t32, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__civic_republican_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__civic_republican_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t8, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement_basis(seco_be_t8, observed).
narrative_ontology:measurement(seco_be_t16, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 16, 0.32).
narrative_ontology:measurement_basis(seco_be_t16, observed).
narrative_ontology:measurement(seco_be_t24, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement_basis(seco_be_t24, observed).
narrative_ontology:measurement(seco_be_t32, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 32, 0.37).
narrative_ontology:measurement_basis(seco_be_t32, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t8, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement_basis(seco_su_t8, observed).
narrative_ontology:measurement(seco_su_t16, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement_basis(seco_su_t16, observed).
narrative_ontology:measurement(seco_su_t24, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement_basis(seco_su_t24, observed).
narrative_ontology:measurement(seco_su_t32, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 32, 0.29).
narrative_ontology:measurement_basis(seco_su_t32, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 50, 0.29).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, federal_militia_regulation_authority).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, standing_military_tyranny_prevention_doctrine).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel (second_amendment_arms_right) decomposes into three constraint stories, one per live reading: civic_republican_reading (this file), individual_right_reading, and collective_right_reading. Each instantiates a different ε, different beneficiary structure, and different regulatory boundary. They are linked via network.affects_constraints because each reading's success affects the others' credibility and legal standing. The three stories are NOT merged into one; each is ε-invariant and independent per OQ-26 (ε-invariance). The contest among readings is routed through omega variables (kernel_reading_contest_relationship) and cs_structure axioms, not through metric manipulation or claim-hedging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
