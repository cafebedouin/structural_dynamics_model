% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Separation of Powers — Functionalist Reading (Overlapping Authority / Intelligible Principle)
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the functionalist reading of the
 *   separation-of-powers kernel: the constitutional text is treated as
 *   establishing a flexible framework tolerant of overlapping institutional
 *   authority, provided Congress supplies an 'intelligible principle' to
 *   guide agency action. This reading legitimates the modern administrative
 *   state — independent agencies exercising combined rulemaking, enforcement,
 *   and adjudicative power — and coordinates governance of technically
 *   complex domains that Congress cannot practically legislate in full
 *   detail. It is a distinct constraint from the formalist reading (which
 *   treats the same text as establishing impermeable boundaries and would
 *   find most delegations unconstitutional) and the unitary executive reading
 *   (which treats the same text as vesting all executive power exclusively in
 *   the President and would find independent-agency insulation from removal
 *   unconstitutional). Each reading has its own ε, its own beneficiary/victim
 *   structure, and its own classification; they are linked here only through
 *   network.affects_constraints and the omega variables documenting the
 *   kernel contest, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - administrative_agencies: primary beneficiary/agenda_setter (institutional/arbitrage) — exercises delegated combined functions
 *   - congress: beneficiary/agenda_setter (institutional/arbitrage) — delegates broadly, avoids political cost
 *   - executive_branch: beneficiary (institutional/arbitrage) — directs agency policymaking
 *   - regulated_parties_facing_agency_overreach: primary payer (moderate/constrained) — bears cost of rules made under vague standards
 *   - nondelegation_challengers: payer/excluded (moderate/trapped) — structurally near-foreclosed constitutional claim
 *   - reviewing_courts: agenda_setter/analytical observer — sets practical boundary via intelligible principle doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.32).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.28).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers — Functionalist Reading (Overlapping Authority / Intelligible Principle)").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'b287b065-057b-42d1-9ad1-ecb88548a7f1').
narrative_ontology:cs_kernel_codification('b287b065-057b-42d1-9ad1-ecb88548a7f1', fixed_text).
narrative_ontology:cs_authority_grounding('b287b065-057b-42d1-9ad1-ecb88548a7f1', lineage).
narrative_ontology:cs_interpretation_layer_present('b287b065-057b-42d1-9ad1-ecb88548a7f1').
narrative_ontology:cs_reading_relation('b287b065-057b-42d1-9ad1-ecb88548a7f1', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b287b065-057b-42d1-9ad1-ecb88548a7f1', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('b287b065-057b-42d1-9ad1-ecb88548a7f1', foundational, functional_overlap_permissible_if_intelligible_principle_supplied).
narrative_ontology:cs_axiom_status(functional_overlap_permissible_if_intelligible_principle_supplied, holdable).
narrative_ontology:cs_axiom_grounding('b287b065-057b-42d1-9ad1-ecb88548a7f1', functional_overlap_permissible_if_intelligible_principle_supplied, conventional).
narrative_ontology:cs_axiom('b287b065-057b-42d1-9ad1-ecb88548a7f1', secondary, administrative_expertise_justifies_combined_agency_functions).
narrative_ontology:cs_axiom_status(administrative_expertise_justifies_combined_agency_functions, holdable).
narrative_ontology:cs_axiom_grounding('b287b065-057b-42d1-9ad1-ecb88548a7f1', administrative_expertise_justifies_combined_agency_functions, instrumental).
narrative_ontology:cs_reference_frame('b287b065-057b-42d1-9ad1-ecb88548a7f1', post_new_deal_functional_accommodation).
narrative_ontology:cs_drift_state('b287b065-057b-42d1-9ad1-ecb88548a7f1', contemporary_nondelegation_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b287b065-057b-42d1-9ad1-ecb88548a7f1', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulated_industries_seeking_stability).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_parties_facing_agency_overreach).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, nondelegation_challengers).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, administrative_state_legitimacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, chevron_style_deference_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise combined rulemaking (legislative-like), enforcement (executive-like), and adjudication (judicial-like) functions under statutory delegations that name only an 'intelligible principle' rather than detailed rules. Their legitimacy and operating budget depend on courts continuing to read separation of powers as permitting this functional blending.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, administrative_agencies, agenda_setter).

% Delegates broad rulemaking authority to agencies via loosely bounded statutory standards, avoiding the political cost of resolving technical or contested policy questions itself. Retains oversight and appropriations leverage without bearing day-to-day implementation risk.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, agenda_setter).

% Directs and staffs the agencies that receive delegated authority, gaining substantial policymaking power through appointment and removal influence, executive orders, and guidance documents that agencies implement under the delegated standard.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, executive_branch, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from a stable, expert, and predictable administrative apparatus that can adapt technical rules over time without requiring fresh legislation for every change; lobby agencies directly rather than needing to move Congress on every issue.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_industries_seeking_stability, beneficiary,
    organized, biographical, constrained, national).

% Small businesses, individual permit applicants, and less-organized regulated actors face agency rules promulgated under vague statutory standards with limited practical avenue to contest the delegation itself; judicial review of the underlying delegation is nearly foreclosed once an intelligible principle is found, leaving only arbitrary-and-capricious review of the agency's exercise.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_parties_facing_agency_overreach, payer,
    moderate, biographical, constrained, national).

% Litigants who argue a specific delegation is unconstitutionally broad face a doctrine (intelligible principle) that has invalidated a statute only twice in the nation's history; their constitutional claim is structurally very difficult to win regardless of the delegation's actual breadth.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, nondelegation_challengers, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, nondelegation_challengers, excluded).

% Apply the intelligible principle standard and deference doctrines to agency action, effectively setting the practical boundary of the delegation. Their interpretive choices determine how much functional overlap the framework tolerates in practice.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, reviewing_courts, agenda_setter,
    institutional, generational, analytical, national).

% Argue from competing readings of the same constitutional text that this framework's tolerance for overlapping authority is itself the constitutional violation; their objection is a live minority position in courts and legal scholarship but does not control current doctrine.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_and_unitary_executive_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Congress to set broad policy goals and delegate technical implementation to expert agencies, and allows agencies to combine rulemaking, enforcement, and adjudicative functions needed to administer complex regulatory programs (environmental, financial, health and safety) without requiring Congress to legislate every technical detail or requiring a single branch to perform every governmental function in isolation.
% TRANSFER_FUNCTION: Moves practical lawmaking and adjudicative authority from Congress and courts to agencies and the executive branch that direct them; moves the burden of contesting the scope of that authority onto regulated parties, who must litigate agency action under deferential review rather than challenge the underlying delegation.
% ABSENT_VOICES: Nondelegation challengers and formalist/unitary-executive advocates raise the objection that functional overlap dissolves the separation the Constitution requires, but the intelligible principle doctrine's near-total judicial deference means their structural challenge is rarely reachable in practice; individual regulated parties harmed by a specific agency action typically cannot separately litigate the delegation's constitutionality.
% DISAPPEARANCE_RATIONALE: If the functionalist reading were displaced by a strict formalist or unitary-executive reading, the modern regulatory state as currently structured (independent agencies, broad delegated rulemaking, combined agency functions) would need to be substantially rebuilt or curtailed: Congress would need to legislate at a level of specificity it rarely does, and independent agencies with removal protections would face restructuring.
% FOUNDING_PROBLEM: The problem was that a rigid, formalist separation of powers could not accommodate a modern administrative state needing to regulate complex, fast-changing technical domains (securities, environment, telecommunications, public health) without Congress legislating every detail or without agencies possessing the combined capacity to write rules, enforce them, and adjudicate disputes.
% FOUNDING_PROBLEM_CORROBORATION: Administrative law scholars and several generations of Supreme Court majorities (from the New Deal era through Chevron-era deference cases) attest the functionalist accommodation remains necessary to modern governance. Formalist and unitary-executive scholars, along with a growing bloc within the current Supreme Court (nondelegation revival advocates, major questions doctrine proponents), attest from outside the administrative-agency beneficiary set that the functionalist reading has drifted from a narrow accommodation into an unbounded license for unaccountable lawmaking — this is a genuine live dispute, not a settled genealogy.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.32 at interval end) because the functionalist reading's primary operation is coordination of complex regulatory governance, not rent extraction — but it is not negligible because delegated authority under vague standards does shift real costs onto regulated parties who cannot meaningfully contest the delegation itself. Suppression is authored low-moderate (0.28) reflecting that alternatives (formalist or unitary-executive readings) are not suppressed by force but are structurally disadvantaged by nearly a century of deference precedent that makes them very hard to win on. Theater ratio is modest (0.22) — the intelligible principle test is not pure performance, but its extremely low invalidation rate (two statutes in US history) suggests a meaningful gap between the doctrine's stated rigor and its practical bite. Accessibility collapse is moderate (0.35): alternative constitutional readings remain live in scholarship and on the current Court, so collapse is far from complete. Resistance is meaningfully high (0.55) because formalist and unitary-executive advocates, including a growing bloc on the Supreme Court, actively contest this reading in ongoing litigation (nondelegation revival, major questions doctrine, removal-power cases).
 *
 * PERSPECTIVAL GAP:
 *   From the administrative agency and Congress seats, this reading computes as legitimate coordination solving a real governance problem — expert, adaptable regulation of technical domains. From the nondelegation challenger seat, the identical doctrine computes as a near-unfalsifiable license: the intelligible principle standard is satisfied by almost any statutory language, making the constitutional constraint largely rhetorical from that vantage. The engine should register this divergence directly from the structural data (beneficiary concentration in institutional seats, victim concentration in moderate-power constrained seats) rather than from either party's rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative agencies, Congress, and the executive branch sit near the beneficiary end: they gain practical governing capacity and political cover from the functionalist framework and have institutional arbitrage-grade positioning relative to any single adverse ruling. Regulated parties facing agency overreach and nondelegation challengers sit nearer the target end: they bear compliance costs and litigation burdens under a doctrine that gives them very little structural purchase to challenge the delegation itself, and their exit options are constrained or trapped because opting out of federal regulatory jurisdiction is rarely practical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accommodating technical governance the formalist framework could not administer) remains partly live — regulatory domains have only grown more technically complex since the New Deal — but the founding_problem_status is authored as contested because critics argue the accommodation has expanded well past its original justification into essentially unbounded delegation, while defenders argue the complexity that justified the original accommodation has only intensified. This is not a case of straightforward mandatrophy (the coordination function has not simply evaporated) nor of clean vindication (the scope of what counts as 'intelligible' has been read so loosely that the limiting principle does little independent work) — hence the mismatch is left open rather than resolved in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_functionalist,
    'Is the functionalist reading of separation of powers (tolerant of overlapping authority and broad intelligible-principle delegation) the structurally correct reading of the constitutional kernel, or is it a drift from a formalist original meaning that has been sustained primarily because it serves the institutional interests of Congress, the executive branch, and the administrative agencies that operate under it?',
    'There is no empirical resolution mechanism for a contested constitutional interpretive question; resolution (if any) comes through Supreme Court doctrine shifts (e.g., a revived nondelegation doctrine gaining a majority) or through constitutional amendment. Track doctrinal drift via case outcomes (intelligible principle invalidations, major questions doctrine invocations, removal-power cases).',
    'If the formalist or unitary-executive reading displaces the functionalist reading as controlling doctrine, this constraint''s beneficiary structure inverts: agencies and Congress lose the delegation latitude that currently benefits them, and the current victims (regulated parties, nondelegation challengers) would see their structural position improve substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_functionalist, conceptual, 'Whether the functionalist reading is the structurally correct reading of the kernel or an institutionally self-serving drift from formalist original meaning.').

omega_variable(
    intelligible_principle_toothlessness,
    'Is the intelligible principle standard a genuine, operative constitutional limit on delegation, or has it become toothless in practice (invalidating only two statutes in the nation''s history) such that it functions mainly as legitimating theater for what is effectively unbounded delegation?',
    'Comparative analysis of the breadth of delegating statutory language actually upheld under the standard versus statutes that would plausibly fail a more rigorous test; tracking whether the current Supreme Court''s nondelegation-revival signals (e.g., concurrences inviting future challenges) translate into actual invalidations.',
    'If the standard is genuinely toothless, the theater_ratio and effective suppression of alternative interpretive readings should be revised upward, and the functionalist reading would look more like a tangled_rope (real coordination function plus a captured legitimating doctrine) than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_toothlessness, empirical, 'Whether the intelligible principle doctrine functions as a real limit or as legitimating theater for unbounded delegation.').

omega_variable(
    agency_beneficiary_vs_public_interest,
    'When agencies benefit from the functionalist reading''s grant of combined rulemaking/enforcement/adjudicative power, is that benefit incidental to serving the public interest the agency is charged with protecting, or does it constitute institutional self-interest (budget, jurisdiction, deference-seeking) independent of the underlying regulatory mission?',
    'Case studies of agency behavior under budget or jurisdictional threat versus under genuine public-interest tradeoffs; examine whether agencies defend the functionalist reading even in cases where a narrower reading would better serve their nominal mission.',
    'If agency self-interest substantially explains defense of the functionalist reading independent of public-interest justification, the beneficiary declaration for administrative_agencies is strengthened as a directionality input (agencies as structural beneficiaries, not merely instrumental actors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_beneficiary_vs_public_interest, empirical, 'Whether agency support for the functionalist reading reflects institutional self-interest or genuine public-interest service.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__functionalist_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(sepa_tr_t45, separation_of_powers_text__functionalist_reading, theater_ratio, 45, 0.17).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__functionalist_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(sepa_tr_t75, separation_of_powers_text__functionalist_reading, theater_ratio, 75, 0.21).
narrative_ontology:measurement(sepa_tr_t90, separation_of_powers_text__functionalist_reading, theater_ratio, 90, 0.22).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__functionalist_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(sepa_be_t45, separation_of_powers_text__functionalist_reading, base_extractiveness, 45, 0.26).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__functionalist_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(sepa_be_t75, separation_of_powers_text__functionalist_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement(sepa_be_t90, separation_of_powers_text__functionalist_reading, base_extractiveness, 90, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__functionalist_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(sepa_su_t45, separation_of_powers_text__functionalist_reading, suppression_requirement, 45, 0.21).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__functionalist_reading, suppression_requirement, 60, 0.23).
narrative_ontology:measurement(sepa_su_t75, separation_of_powers_text__functionalist_reading, suppression_requirement, 75, 0.26).
narrative_ontology:measurement(sepa_su_t90, separation_of_powers_text__functionalist_reading, suppression_requirement, 90, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three members of the separation_of_powers_text kernel family. The formalist_reading (strict boundaries, delegation largely impermissible) and unitary_executive_reading (all executive power vests exclusively in the President, independent agencies unconstitutional) are separate constraint stories with their own ε, beneficiary/victim structures, and classifications. This functionalist_reading story is authored with substantially lower ε (0.32) than would be expected for either sibling under contested-delegation scrutiny, because its coordination function (enabling technical regulatory governance) is genuine and its beneficiary/victim asymmetry is comparatively modest. The formalist reading would be expected to show different victim sets (agencies and delegating Congress recast as overreaching, regulated parties recast as protected) and the unitary executive reading would recast independent agencies themselves as a victim/target category (their removal-protected status becoming the contested extraction). Each story documents this kernel relationship independently per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
