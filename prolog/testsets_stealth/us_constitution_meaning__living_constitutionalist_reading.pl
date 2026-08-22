% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living Constitutionalism: Enduring Principles with Evolving Application
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   The arrangement under contest is the living-constitutionalist settlement
 *   of judicial authority: federal judges are bound by enduring
 *   constitutional principles and empowered to adapt those principles'
 *   application to social attitudes and circumstances as they change.
 *   Assessed by the reading's own lights, the arrangement solves a real
 *   coordination problem — an eighteenth-century charter governing a changing
 *   polity without perpetual amendment — while imposing a real
 *   counter-majoritarian cost: policy choices made by electorates and
 *   legislatures can be overridden by an unaccountable bench whose compliance
 *   is compulsory. The epsilon referent is this standing arrangement itself,
 *   never any alternative arrangement this reading would prefer. Claim and
 *   metrics are independent authored facts: the reading is claimed as
 *   tangled_rope from the authoring seat — genuine coordination plus
 *   asymmetric extraction held together by active enforcement — while the
 *   metric values describe the arrangement's observed operation across the
 *   interval (t=0 approximates the 1937 New Deal settlement; t=90 the
 *   present).
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter (institutional/constrained) — administers the adaptation; evolved rulings bind all other actors
 *   - rights_claimants_evolving_contexts: primary beneficiary (powerless/trapped) — claims vindicated as application evolves
 *   - democratic_majorities: primary payer (organized/constrained) — enactments subject to invalidation under evolved application
 *   - state_governments: secondary payer (institutional/constrained) — bound by federalized rights application
 *   - legal_academy: analytical observer — supplies the doctrinal vocabulary adaptation runs on
 *   - disenfranchised_residents: excluded — governed by application but absent from the attitude-channels adaptation consults
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.48).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.5).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living Constitutionalism: Enduring Principles with Evolving Application").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '4845d327-4160-4ad4-a93c-527b5cb6ef80').
narrative_ontology:cs_kernel_codification('4845d327-4160-4ad4-a93c-527b5cb6ef80', fixed_text).
narrative_ontology:cs_authority_grounding('4845d327-4160-4ad4-a93c-527b5cb6ef80', lineage).
narrative_ontology:cs_interpretation_layer_present('4845d327-4160-4ad4-a93c-527b5cb6ef80').
narrative_ontology:cs_reading_relation('4845d327-4160-4ad4-a93c-527b5cb6ef80', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4845d327-4160-4ad4-a93c-527b5cb6ef80', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('4845d327-4160-4ad4-a93c-527b5cb6ef80', foundational, enduring_principles_bind_application_evolution).
narrative_ontology:cs_axiom_status(enduring_principles_bind_application_evolution, holdable).
narrative_ontology:cs_axiom_grounding('4845d327-4160-4ad4-a93c-527b5cb6ef80', enduring_principles_bind_application_evolution, deontological).
narrative_ontology:cs_axiom('4845d327-4160-4ad4-a93c-527b5cb6ef80', secondary, contemporary_consensus_informs_application).
narrative_ontology:cs_axiom_status(contemporary_consensus_informs_application, holdable).
narrative_ontology:cs_axiom_grounding('4845d327-4160-4ad4-a93c-527b5cb6ef80', contemporary_consensus_informs_application, instrumental).
narrative_ontology:cs_reference_frame('4845d327-4160-4ad4-a93c-527b5cb6ef80', enduring_principles_adaptive_application).
narrative_ontology:cs_drift_state('4845d327-4160-4ad4-a93c-527b5cb6ef80', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4845d327-4160-4ad4-a93c-527b5cb6ef80', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_evolving_contexts).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, democratic_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, state_governments).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, evolving_standards_of_decency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured federal judges administer the arrangement: they identify enduring principles in the constitutional text and decide when application must evolve with social attitudes and circumstances. Their evolved rulings bind every other actor, and compliance is compulsory. Exit is resignation or retirement; the interpretive authority accrues to the office across appointments rather than to any individual.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Minorities and marginalized groups whose claims gain recognition as application evolves — school desegregation litigants, criminal defendants, same-sex couples. They typically cannot win through ordinary politics, which is why they litigate, and they cannot exit the legal system whose application determines their rights; their access runs through the courts' willingness to read enduring principles at the level of present circumstances.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_evolving_contexts, beneficiary,
    powerless, biographical, trapped, national).

% Electoral majorities and the coalitions that enact policy. When application evolves against their enactments, courts invalidate them and the majority must comply, amend the constitution under supermajority thresholds rarely met, or wait to reshape the bench through appointments — a channel measured in decades. They bear the counter-majoritarian cost: policy choices overridden without a legislative forum.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, democratic_majorities, payer,
    organized, biographical, constrained, national).

% State legislatures, governors, and courts whose law is bound by federal constitutional application as it evolves — incorporation of the Bill of Rights, equal protection mandates, one-person-one-vote. They cannot leave the arrangement; their remedies are litigation, compliance, or political mobilization to change the bench. They retain their own police powers only within the boundaries evolved application draws.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Constitutional scholars and law teachers who supply the doctrinal vocabulary through which adaptation is justified or attacked. Their scholarship feeds opinions and shapes which evolutions read as principled. They hold no enforcement power and collect no compliance; their stake is the coherence of the interpretive tradition itself.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% People governed by constitutional application who hold no vote — children, non-citizens, disenfranchised felons, territorial residents. The arrangement consults social attitudes and circumstances as registered in democratic channels they cannot access; their interests enter only when courts independently attend to them. They cannot exit the jurisdiction's application and cannot vote on the adaptation question.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, disenfranchised_residents, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single constitutional order coherent across generations: a fixed eighteenth-century charter would either ossify, losing legitimacy as circumstances change, or require amendment so onerous that it could not respond at all. The arrangement lets enduring principles govern new circumstances without formal revision, preserving continuity of the legal order while it updates.
% TRANSFER_FUNCTION: Moves interpretive authority over the charter's application away from ratification-era understandings and current legislative majorities and toward the federal bench; moves concrete policy outcomes to litigants who prevail under evolved application; moves compliance costs to the losing governments and majorities.
% ABSENT_VOICES: The disenfranchised — children, non-citizens, disenfranchised felons, territorial residents — whose circumstances application governs but who register no signal in the democratic channels whose attitudes adaptation consults; also the losers of adapted rulings, whose policy preferences are overridden in a forum where they had no vote.
% DISAPPEARANCE_RATIONALE: If the adaptation arrangement vanished overnight, the charter would govern as fixed historical meaning: every domain settled by evolved application — desegregation, incorporation, reapportionment, recognition of new rights claims — reopens, and the entire update burden shifts to an amendment process that has succeeded only seventeen times since 1791. The legal order would rearrange around either ossification or amendment crisis.
% FOUNDING_PROBLEM: How a short, abstract charter written for an eighteenth-century republic can bind a continental industrial democracy across centuries without either losing authority or being perpetually rewritten — the problem Marbury's review power and the New Deal settlement each addressed in different registers.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians of the New Deal settlement and comparative constitutionalists studying every long-lived written constitution attest that the founding problem is live and structural; the originalist seat attests the problem exists while disputing the proposed solution. Corroboration does not rest on rights claimants, the arrangement's principal beneficiaries.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48): adaptation transfers real policy authority from majoritarian institutions to the bench, but the transfer is bounded by the enduring-principles half of the frame and proceeds case by case rather than by program. Suppression is moderate (0.50) and structural, not interpersonal: compliance with evolved rulings is compulsory, and the formal alternative (Article V amendment) has succeeded only seventeen times since 1791, so losing governments' realistic options are litigation, appointments politics, and waiting. Accessibility collapse is 0.50 — alternatives exist but are slow and costly. Resistance is 0.60: court-curbing proposals, appointment battles, and periodic open noncompliance episodes are a standing feature. Theater is low (0.22): the interpretive work is functional; ceremonial constitutionalism is a minor fraction. The three measurement series share one seven-point grid. base_extractiveness tracks the counter-majoritarian transfer: it climbs through the Warren and early Burger courts (0.36 to 0.58), then partially recedes as the Dobbs-era Court reasserted the enduring-principles constraint (0.48). suppression_requirement is tracked because the enforcement machinery itself matured over the interval — incorporation extended federal review into state criminal justice, justiciability doctrines widened the docket, and compliance enforcement hardened — rising 0.42 to 0.57 before plateauing. theater_ratio drifts up slowly as interpretive output grows faster than the functional core.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently. From the bench, adaptation is fidelity: principles endure precisely so they can govern circumstances their framers did not face, and the counter-majoritarian cost is the price of a charter that does not ossify. From the majoritarian seats, the same structure is unaccountable policy revision — a transfer of lawmaking to a body no one can vote out. Rights claimants experience the arrangement as the only channel their exclusion from ordinary politics leaves open, which is why the beneficiary seat is powerless and trapped rather than powerful. The engine computes these per-seat classifications from the structural data; this story's claimed type does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. rights_claimants_evolving_contexts (beneficiary, powerless, trapped) sits near the full-beneficiary end: the arrangement subsidizes claims they cannot win politically. democratic_majorities and state_governments (payers, constrained exit) sit near the full-target end, and their extraction is amplified by the near-closed Article V alternative. federal_judiciary is the agenda-setter and the standing receipt: each successful adaptation accrues interpretive authority to the bench, so its directionality sits low despite its enforcement role — it is the seat gain_flow names. legal_academy is analytical and collects nothing. disenfranchised_residents are excluded: structurally absent from the consensus channels, neither collecting nor paying through a seat of their own. No directionality overrides are used because the beneficiary/victim plus exit data already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a durable charter for an evolving polity — is live, so no mandatrophy declaration: the arrangement's function has not outlived itself. The tangled-rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure rope would erase the real counter-majoritarian extraction: majorities pay through the same structure that coordinates. Reading it as a snare would erase the genuine coordination: constitutional continuity and rights protection that no alternative channel currently provides. The extraction and the coordination run through the same interpretive structure, which is the tangled-rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is one reading of kernel us_constitution_meaning (living_constitutionalist_reading). What structural differences would the sibling readings — originalist_reading and positivist_reading — introduce, and where is the disagreement located?',
    'Compare the three sibling stories'' beneficiary/victim structures and epsilon values; locate the disagreement in the declared source of constitutional authority (interpreted enduring principles versus ratification-era public meaning versus enactment procedure).',
    'Under originalist_reading the adaptation channel closes: rights claimants lose the beneficiary seat and those burdened by original application become the victims. Under positivist_reading the enduring-principles coordination function disappears and validity rests on enactment procedure alone. Beneficiary/victim structure and classification all shift with the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in the source of constitutional authority.').

omega_variable(
    adaptation_principle_boundary,
    'Is the enduring-principles half of the frame a genuine limit that distinguishes legitimate adaptation from judicial preference, or a rhetorical license that constrains nothing?',
    'Predictive tests on adapted rulings: do outcomes follow from stated principle plus documented changes in social circumstances, or do they track judges'' policy preferences (doctrinal coherence analysis, ideology-score studies)?',
    'If license, the arrangement''s extraction is substantially higher and it trends toward snare (coordination story as cover); if a genuine limit, the coordination function dominates and the tangled-rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_principle_boundary, empirical, 'Whether the principles-endure half of the frame actually binds the adaptation it authorizes.').

omega_variable(
    consensus_ascertainment,
    'Whose contemporary consensus does evolving application track — the courts'' reading of social attitudes or actual public attitudes?',
    'Compare adapted rulings against contemporaneous polling, election returns, and state legislative trends at decision time.',
    'Systematic divergence means adaptation is elite projection: extraction from majoritarian governance rises and the beneficiary structure narrows to litigants; close alignment supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_ascertainment, empirical, 'Whether ''social attitudes'' in the frame means measured public attitudes or judicial perception of them.').

omega_variable(
    overreach_cost_incidence,
    'Does the counter-majoritarian cost fall on discrete legislative losers or on the diffuse legitimacy of the order itself?',
    'Trace invalidated enactments and post-ruling compliance patterns: who bears identifiable losses, and how often do losing governments comply without further resistance?',
    'Concentrated incidence strengthens the victim seats and pushes the arrangement toward snare-flavored extraction; diffuse incidence makes the victim the counter-majoritarian constraint itself — a structural good eroded rather than a group harmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overreach_cost_incidence, empirical, 'Distribution of the overreach risk the reading itself names as its cost.').

omega_variable(
    amendment_channel_viability,
    'Is Article V a live alternative to judicial adaptation, or effectively closed — and does closure change the arrangement''s suppression profile?',
    'Count successful amendments and serious application campaigns over the interval; assess whether any contemporary policy reversal has actually moved through Article V.',
    'If Article V is effectively closed, judicial adaptation is the only update channel: alternatives to compliance collapse further, suppression is higher than authored, and the coordination justification strengthens (the bench becomes the only exit from ossification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_viability, empirical, 'Whether the formal alternative to adaptation is real or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 45, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t45, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t75, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t75, observed).
narrative_ontology:measurement(us_c_tr_t90, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t45, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 45, 0.56).
narrative_ontology:measurement_basis(us_c_be_t45, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t75, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 75, 0.54).
narrative_ontology:measurement_basis(us_c_be_t75, observed).
narrative_ontology:measurement(us_c_be_t90, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 90, 0.48).
narrative_ontology:measurement_basis(us_c_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t45, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement_basis(us_c_su_t45, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t75, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 75, 0.56).
narrative_ontology:measurement_basis(us_c_su_t75, observed).
narrative_ontology:measurement(us_c_su_t90, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 90, 0.5).
narrative_ontology:measurement_basis(us_c_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: us_constitution_meaning decomposes into three reading-stories — originalist_reading, positivist_reading, and this one. Each is epsilon-invariant: this story authors epsilon for the living-constitutionalist arrangement of judicial authority itself (coordination of stability and adaptation with counter-majoritarian extraction), not for the originalist or positivist arrangements, which are authored separately with their own beneficiary/victim structures. The edges record that the readings compete over the same kernel text; neither sibling's values are averaged into this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
