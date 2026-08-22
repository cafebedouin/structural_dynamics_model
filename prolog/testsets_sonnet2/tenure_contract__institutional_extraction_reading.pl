% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Claim on Departmental Resources (Institutional Extraction Reading)
 *   domain: higher_education_governance/labor_economics
 *
 * SUMMARY:
 *   This story instantiates the institutional-extraction reading of the
 *   tenure-contract kernel: tenure viewed not as an academic-freedom shield
 *   but as a permanent, seniority-locked claim on departmental budget and
 *   headcount, held by an early cohort of winners and enforced by
 *   faculty-governance votes that same cohort controls. Over the last several
 *   decades, as public funding per student declined and enrollment shifted
 *   across fields, the fixed stock of tenured lines has become increasingly
 *   disconnected from current instructional need, and the resulting
 *   flexibility gap has been closed almost entirely by growth in contingent,
 *   non-tenure-track hiring. The claimed type (tangled_rope) and the authored
 *   metrics are independent: tenure genuinely once solved (and at the point
 *   of initial hire still partially solves) a real screening and commitment
 *   problem, but the same mechanism now runs a resource claim that persists
 *   regardless of current departmental need, funded by cost-shifting onto
 *   contingent labor and students. This is a sibling reading, not a rebuttal,
 *   to academic_freedom_reading (which locates tenure's function in
 *   truth-seeking insulation) and demographic_reproduction_reading (which
 *   locates its function in demographic gatekeeping via peer review). All
 *   three readings share the same kernel — the tenure contract as a
 *   stabilized commitment — but author different ε, different
 *   beneficiary/victim structures, and different classifications, because
 *   they are different constraints reading the same text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.58).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Claim on Departmental Resources (Institutional Extraction Reading)").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '7ef02d51-97e8-40fc-8248-b544c75e56b8').
narrative_ontology:cs_kernel_codification('7ef02d51-97e8-40fc-8248-b544c75e56b8', formalized).
narrative_ontology:cs_authority_grounding('7ef02d51-97e8-40fc-8248-b544c75e56b8', practice).
narrative_ontology:cs_interpretation_layer_present('7ef02d51-97e8-40fc-8248-b544c75e56b8').
narrative_ontology:cs_reading_relation('7ef02d51-97e8-40fc-8248-b544c75e56b8', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ef02d51-97e8-40fc-8248-b544c75e56b8', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('7ef02d51-97e8-40fc-8248-b544c75e56b8', foundational, permanence_of_claim_exceeds_justifying_risk).
narrative_ontology:cs_axiom_status(permanence_of_claim_exceeds_justifying_risk, holdable).
narrative_ontology:cs_axiom_grounding('7ef02d51-97e8-40fc-8248-b544c75e56b8', permanence_of_claim_exceeds_justifying_risk, empirically_contingent).
narrative_ontology:cs_axiom('7ef02d51-97e8-40fc-8248-b544c75e56b8', secondary, seniority_locked_allocation_is_illegitimate_absent_ongoing_review).
narrative_ontology:cs_axiom_status(seniority_locked_allocation_is_illegitimate_absent_ongoing_review, holdable).
narrative_ontology:cs_axiom_grounding('7ef02d51-97e8-40fc-8248-b544c75e56b8', seniority_locked_allocation_is_illegitimate_absent_ongoing_review, instrumental).
narrative_ontology:cs_reference_frame('7ef02d51-97e8-40fc-8248-b544c75e56b8', lifetime_appointment_as_scarcity_insurance).
narrative_ontology:cs_drift_state('7ef02d51-97e8-40fc-8248-b544c75e56b8', contemporary_contingent_majority_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ef02d51-97e8-40fc-8248-b544c75e56b8', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_adjunct_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, untenured_junior_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, undergraduate_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, untenured_junior_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, university_administration).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, seniority_based_resource_allocation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a permanent claim on a departmental line, teaching load, and salary floor that is essentially immune to enrollment shifts, budget contraction, or performance review below the misconduct threshold. Vote on hiring, curriculum, and departmental resource allocation, effectively setting the terms under which the line they occupy could ever be reallocated. Can move institutions if dissatisfied (portable reputation, marketable research record) even though the position itself is not revocable.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents, agenda_setter).

% Teach a large and growing share of instructional hours, often the same courses as tenured colleagues, at a fraction of the pay, with no job security, no vote in governance, and semester-to-semester renewal. Absorb the flexibility the institution needs because tenured lines cannot be shed; their contracts are the shock absorber for enrollment and budget volatility. Geographic and family constraints frequently trap them in a single labor market, foreclosing exit to other regions or industries.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_adjunct_faculty, payer,
    powerless, immediate, trapped, regional).

% Compete for a shrinking number of new tenure lines against a fixed stock already occupied by incumbents; must clear a probationary review controlled by the very incumbents whose resource claims junior hires would eventually rival. If they clear tenure they join the beneficiary class; until then they bear the risk, the up-front productivity demands, and the geographic mobility costs of chasing scarce lines.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, untenured_junior_faculty, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, untenured_junior_faculty, beneficiary).

% Pay tuition that funds salaries for a permanent faculty stock regardless of whether that stock matches current student demand or program relevance; increasingly taught by underpaid, overloaded adjuncts because tenured lines cannot be redirected toward high-demand fields without new money. Their choice is largely confined to which institution to attend, not how instructional resources are allocated within it.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, undergraduate_students, payer,
    powerless, immediate, constrained, national).

% Manages the budget around a large, fixed tenured-salary base it cannot easily shrink, and increasingly relies on contingent hiring to retain any operational flexibility. Sets tuition and adjunct pay partly to cover legacy tenure commitments in declining-enrollment departments, while publicly defending tenure as necessary for institutional prestige and accreditation.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administration, payer).

% Trained for academic careers in fields where existing tenured lines have frozen the department's capacity; have no seat in governance and are not consulted when institutions decide to preserve legacy lines over opening new ones matched to current demand. Would argue for reallocating tenure slots toward current need, but their interest is structurally unrepresented in the faculty senates that control the lines.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, prospective_faculty_in_shrinking_fields, excluded,
    powerless, biographical, trapped, national).

% Study the tenure system's effects on labor market segmentation, wage compression among contingent faculty, and misallocation of instructional resources; produce the empirical record that the extraction and rigidity effects rest on, without a stake in the outcome themselves.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure review does coordinate something real at hire: it screens for a baseline of scholarly competence at the point of entry and creates a credible long-horizon commitment that lets an institution invest in a scholar's development. But once granted, the same mechanism converts a screening decision into a permanent, unreviewable claim on departmental headcount and budget share.
% TRANSFER_FUNCTION: Moves instructional flexibility and compensation from contingent and junior faculty (and, via tuition and reduced program responsiveness, from students) to the fixed stock of tenured incumbents, who retain salary, workload protections, and governance votes irrespective of current departmental need or teaching output.
% ABSENT_VOICES: Prospective faculty in shrinking fields and students in under-resourced high-demand programs have no vote in the faculty governance bodies that control line allocation; contingent faculty who teach the marginal courses are also excluded from the tenure-line votes that determine whether their teaching converts into a permanent position.
% DISAPPEARANCE_RATIONALE: If permanent tenure claims vanished overnight and all faculty employment reverted to renewable contracts, departments would immediately reallocate lines toward current enrollment and research demand, contingent faculty could compete on equal contractual footing for now-open positions, and administrations would lose their primary argument for relying on adjunct labor as the sole flexible margin — the current two-tier labor structure depends entirely on tenure's permanence.
% FOUNDING_PROBLEM: Early-20th-century faculty could be dismissed for unpopular research findings, political views, or administrative displeasure with no due process, chilling inquiry and giving administrators and trustees direct leverage over scholarly output.
% FOUNDING_PROBLEM_CORROBORATION: Faculty unions and academic freedom scholars attest the dismissal-without-cause problem remains live in adjacent contexts (contingent faculty report exactly this vulnerability). Independent labor economists and university budget officers, outside the tenured-beneficiary class, attest that the permanence mechanism has decoupled from the free-inquiry problem it was built for and now functions primarily as a seniority-based resource lock — supported by data showing contingent faculty, who lack any tenure protection, perform the bulk of teaching with no corresponding academic-freedom crisis distinguishable from their tenured peers'.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.72 over the interval as the ratio of contingent to tenure-track instructional hours has grown nationally; the tenured stock's claim on resources has not shrunk even as enrollment and funding patterns shifted, so a larger share of institutional cost is loaded onto contingent faculty and reflected in tuition. Theater ratio rises in parallel (0.20 to 0.48) as institutions increasingly frame contingent hiring as 'flexibility' or 'market responsiveness' rather than naming it as the direct consequence of tenure's rigidity — the justificatory language performs a coordination story that increasingly diverges from the resource-allocation reality. Suppression is moderate (0.58 at interval end): the mechanism does not rely on force but on faculty-governance votes controlled by incumbents, tenure-track scarcity that disciplines junior faculty into compliance, and adjunct labor markets with few alternative employers in a given region.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured incumbents sit near the full-beneficiary end: they hold the permanent claim, vote on its continuation, and face essentially no downside from departmental resource pressure. Contingent faculty sit near the full-target end: trapped exit options (regional labor markets, sunk specialization costs), no governance voice, and directly absorb the flexibility costs tenure's rigidity generates. Junior untenured faculty are dual-positioned — payers during probation, prospective beneficiaries if they clear review — which is why they carry both payer and beneficiary roles; the override risk here is treating them purely as victims, which would erase the real incentive tenure offers them to accept short-term extraction for a long-term claim of their own. Students are diffuse payers: they do not vote and cannot easily perceive the link between tenure rigidity and their tuition or instructor mix, but the transfer function routes through them structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting scholars from arbitrary dismissal for unpopular findings) is contested rather than flatly dead, because a live academic-freedom function still exists at the margin for some tenured research. But this reading isolates the resource-allocation mechanism specifically, and on that narrower question the founding problem's connection to permanence has weakened: contingent faculty, who have zero tenure protection, do not show a correspondingly elevated rate of academic-freedom violations relative to tenured peers in the available record, which is the mismatch (status=contested tilting toward dead, verdict=world_rearranges) that flags this as a candidate for mandatrophy rather than a settled case — the arrangement's resource-claim function has partially detached from the risk it was built to insure against, while the claim itself, once made permanent, resists any renegotiation of scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_freedom_function_separability,
    'Is the resource-allocation rigidity this reading identifies separable from the academic-freedom insulation the sibling reading identifies, or are they the same mechanism viewed from different seats?',
    'Compare institutions or systems that have decoupled permanence from resource-claim scope (e.g., post-tenure review regimes that periodically re-justify line allocation without revoking academic-freedom protections) against traditional lifetime-tenure systems, measuring both academic-freedom incident rates and resource-reallocation flexibility.',
    'If separable, tenure''s extraction function could in principle be reformed (e.g., periodic line reallocation) without touching the academic-freedom function, meaning this reading''s classification concerns the permanence-of-claim design choice, not tenure per se. If inseparable, any reform aimed at extraction necessarily degrades the academic-freedom coordination function, and the two readings describe a genuine tradeoff rather than two independent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_freedom_function_separability, conceptual, 'Whether the extraction mechanism and the academic-freedom mechanism are the same structure or genuinely separable design features.').

omega_variable(
    tenure_track_scarcity_causal_attribution,
    'How much of the growth in contingent faculty share is caused by tenure''s rigidity specifically, versus general higher-education funding decline that would have produced contingent hiring growth even under a fully at-will faculty labor market?',
    'Cross-national or cross-system comparison of contingent-faculty growth rates in systems with strong tenure protection versus systems with weak or no tenure protection, controlling for public funding trends over the same period.',
    'If contingent growth tracks funding decline independent of tenure regime, this reading''s extraction claim is overstated and much of the cost-shifting onto contingent faculty is a funding-policy effect, not a tenure-structure effect, which would lower the justified ε for this reading. If contingent growth tracks tenure-regime strength independent of funding, the extraction attribution to tenure rigidity is strongly corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tenure_track_scarcity_causal_attribution, empirical, 'Whether contingent-faculty growth is attributable to tenure rigidity or to independent funding trends.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and the academic_freedom_reading sit — is it a factual dispute about tenure''s current effects, or a normative dispute about which effect should count as tenure''s ''real'' function?',
    'Distinguish empirically testable sub-claims (does tenure reduce dismissal-for-unpopular-findings incidents; does tenure correlate with contingent-share growth) from normative sub-claims (which function, if either, justifies the permanence design). Route the empirical sub-claims to the causal-attribution omega above; leave the normative sub-claim as an irreducible framing choice.',
    'If the disagreement is purely factual, further evidence could in principle converge the readings on a shared characterization even while ε values remain reading-indexed. If normative, the readings will remain distinct constraints regardless of evidence, and the kernel''s contest is a genuine multi-reading structure rather than a temporary empirical gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating whether the extraction/freedom reading split is empirical or normative in character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__institutional_extraction_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__institutional_extraction_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__institutional_extraction_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__institutional_extraction_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__institutional_extraction_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__institutional_extraction_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__institutional_extraction_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__institutional_extraction_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__institutional_extraction_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__institutional_extraction_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__institutional_extraction_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__institutional_extraction_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, contingent_faculty_labor_market).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the tenure_contract kernel, decomposed per the ε-invariance principle because the same textual/legal instrument (tenure) produces structurally distinct claims depending on which mechanism is foregrounded: academic-freedom insulation (low ε, mountain-adjacent coordination), demographic gatekeeping via peer review (distinct victim set: excluded demographic groups), and institutional resource extraction (this story: high ε, tangled_rope, victim set of contingent/junior faculty and students). Each reading carries its own beneficiary/victim structure and its own stable ε; they are linked here rather than merged because averaging or hedging across them would violate the ε-invariance principle. This story also links to contingent_faculty_labor_market as a downstream constraint whose extractive terms are structurally dependent on the resource rigidity this reading identifies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
