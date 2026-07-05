% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause — Narrow Originalist Reading (Trade-Crossing-Lines / Facilitation-Only)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the narrow originalist reading of the Commerce
 *   Clause kernel: 'commerce' denotes trade physically crossing state lines,
 *   'regulate' means to make regular or facilitate rather than to prohibit or
 *   comprehensively control, and federal power is confined to dismantling
 *   state-imposed barriers to interstate trade and securing uniform
 *   commercial rules. From 1937 to roughly 1995 this reading was largely
 *   dormant as a controlling doctrine — the Court's post-New Deal
 *   jurisprudence (Jones & Laughlin, Wickard) operated on a functionally
 *   broader premise — but it persisted as a minority scholarly and judicial
 *   position, resurfacing with real doctrinal force in Lopez (1995) and
 *   Morrison (2000), which struck federal statutes reaching gun possession
 *   near schools and gender-motivated violence as exceeding the commerce
 *   power. The reading has not displaced its rivals; it coexists with and
 *   periodically constrains the intermediate-channels reading that currently
 *   governs. This is ONE reading among three siblings (broad_effects_test,
 *   intermediate_channels, narrow_originalist); the other two are separate
 *   constraint stories with their own epsilon values and stakeholder sets,
 *   linked here via network.affects_constraints, not folded into this file's
 *   classification.
 *
 * KEY AGENTS:
 *   - state_governments: institutional beneficiary retaining police-power autonomy
 *   - local_businesses_exempt_from_federal_reach: moderate-power beneficiary of narrowed jurisdiction
 *   - federal_regulatory_agencies: institutional payer losing jurisdictional reach
 *   - national_regulatory_uniformity_interests: organized payer bearing patchwork compliance costs
 *   - civil_rights_claimants_in_recalcitrant_states: powerless, trapped payer lacking federal recourse
 *   - workers_in_intrastate_industries_seeking_federal_labor_protection: powerless, trapped payer
 *   - constitutional_courts: institutional agenda-setter enforcing the boundary
 *   - legal_academics_and_originalist_scholars: analytical observer supplying doctrinal content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause — Narrow Originalist Reading (Trade-Crossing-Lines / Facilitation-Only)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '17190f43-a4f1-43b2-bfca-39c248e8a67b').
narrative_ontology:cs_kernel_codification('17190f43-a4f1-43b2-bfca-39c248e8a67b', fixed_text).
narrative_ontology:cs_authority_grounding('17190f43-a4f1-43b2-bfca-39c248e8a67b', lineage).
narrative_ontology:cs_interpretation_layer_present('17190f43-a4f1-43b2-bfca-39c248e8a67b').
narrative_ontology:cs_reading_relation('17190f43-a4f1-43b2-bfca-39c248e8a67b', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('17190f43-a4f1-43b2-bfca-39c248e8a67b', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('17190f43-a4f1-43b2-bfca-39c248e8a67b', foundational, commerce_denotes_trade_not_all_economic_activity).
narrative_ontology:cs_axiom_status(commerce_denotes_trade_not_all_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('17190f43-a4f1-43b2-bfca-39c248e8a67b', commerce_denotes_trade_not_all_economic_activity, conventional).
narrative_ontology:cs_axiom('17190f43-a4f1-43b2-bfca-39c248e8a67b', foundational, regulate_means_facilitate_not_prohibit).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('17190f43-a4f1-43b2-bfca-39c248e8a67b', regulate_means_facilitate_not_prohibit, conventional).
narrative_ontology:cs_axiom('17190f43-a4f1-43b2-bfca-39c248e8a67b', secondary, federal_power_confined_to_enumerated_grants).
narrative_ontology:cs_axiom_status(federal_power_confined_to_enumerated_grants, holdable).
narrative_ontology:cs_axiom_grounding('17190f43-a4f1-43b2-bfca-39c248e8a67b', federal_power_confined_to_enumerated_grants, deontological).
narrative_ontology:cs_reference_frame('17190f43-a4f1-43b2-bfca-39c248e8a67b', founding_era_trade_facilitation_framework).
narrative_ontology:cs_drift_state('17190f43-a4f1-43b2-bfca-39c248e8a67b', post_new_deal_expansion_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('17190f43-a4f1-43b2-bfca-39c248e8a67b', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses_exempt_from_federal_reach).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_regulatory_experimentation_advocates).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity_interests).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, workers_in_intrastate_industries_seeking_federal_labor_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, dual_sovereignty_federalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain plenary police-power authority over labor, environmental, and civil-rights matters that occur wholly within their borders. Under this reading, federal statutes reaching purely intrastate economic or social activity are struck as beyond the enumerated commerce power, so states set their own floor (or no floor) for these protections. States favoring lax regulation gain a durable jurisdictional shield; states favoring strong protection can still legislate, so the reading costs them nothing and frees them from federal preemption fights.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, state_governments, agenda_setter).

% Firms whose production, labor practices, or environmental footprint stay wholly within one state's borders are read out of federal reach entirely — no minimum wage floor, no federal environmental standard, no federal antidiscrimination statute applies to purely intrastate operations. They benefit directly from the narrower jurisdictional line and can relocate operations to stay inside it if federal reach threatens to widen.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses_exempt_from_federal_reach, beneficiary,
    moderate, biographical, mobile, regional).

% Agencies built to administer nationwide labor, environmental, and civil-rights statutes (NLRB, EPA, EEOC-adjacent enforcement) lose jurisdiction over any activity a court finds insufficiently 'crossing state lines' in the literal, physical sense. Their statutory mandates are unchanged by Congress but judicially narrowed; they cannot litigate their way back to the broader reach without a constitutional amendment or a different bench.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Multi-state businesses, national labor unions, and consumer-protection coalitions that rely on a single federal standard instead of fifty divergent state regimes absorb the cost of patchwork compliance. A firm operating across state lines faces fifty different labor and environmental regimes for activity a broader reading would have unified; there is no exit from this fragmentation short of relitigating the doctrine itself.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity_interests, payer,
    organized, generational, trapped, national).

% Individuals facing discrimination in employment, housing, or public accommodation within a state whose government declines to legislate protection have no federal recourse where the discriminating conduct is judged purely intrastate and non-commercial in this reading's terms. Historically this is the reading that would have left the 1964 Civil Rights Act's public-accommodations provisions without their strongest commerce-power foundation. Exit requires either interstate migration or a change in the constitutional doctrine itself — neither is available to most claimants.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states, payer,
    powerless, biographical, trapped, regional).

% Workers employed by firms whose production and sale occur wholly within one state cannot invoke federal minimum-wage, overtime, or collective-bargaining protections under this reading, because their employer's activity is not itself interstate trade. Their only protection is whatever the state legislature chooses to enact, and changing that requires state-level political power they may not have.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, workers_in_intrastate_industries_seeking_federal_labor_protection, payer,
    powerless, biographical, trapped, regional).

% Federal courts adjudicate the line between interstate trade and everything else, actively striking down statutes that reach intrastate conduct. This is the enforcement mechanism that makes the reading operative: without judicial willingness to invalidate broader federal statutes, the narrow reading is merely a law-review position. Courts bear no direct cost or benefit but determine which seat's version of the kernel governs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Debate the founding-era semantic content of 'commerce' and 'regulate,' producing the historical and textual arguments this reading relies on. They neither collect from nor pay into the arrangement directly, but their scholarship supplies the doctrinal ammunition courts and litigants use to fight over which reading prevails.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, legal_academics_and_originalist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves the problem the Commerce Clause's text was written to address: preventing states from erecting tariff walls, discriminatory taxes, and protectionist barriers against each other's goods, and providing a single federal rule for the physical crossing of goods and persons over state lines. This narrow function is real and uncontested even by rival readings.
% TRANSFER_FUNCTION: Moves regulatory authority away from federal agencies and toward state legislatures for any activity judged intrastate — shifting the cost of protection (or its absence) from a uniform federal floor onto whatever a given state chooses to provide, and shifting the burden of multi-state compliance onto businesses and workers who cannot rely on a single national standard.
% ABSENT_VOICES: Civil rights claimants and low-wage intrastate workers who would benefit from a federal floor are not parties to the doctrinal debate over the constitutional text — that debate occurs among judges, litigators, and scholars, not among the people whose protections turn on its outcome. National consumer and labor coalitions are present as institutional litigants but individual claimants in recalcitrant states are not represented in the interpretive contest itself.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned entirely in favor of one of its siblings, federal environmental, labor, and civil-rights statutes currently vulnerable to narrow-reading challenges would gain secure constitutional footing, states currently shielded from federal preemption would lose that shield, and a substantial body of state-level regulatory autonomy over purely local economic activity would be displaced by uniform federal rules. Because major federal statutes and their enforcement infrastructure are contingent on which reading a court applies, the doctrinal choice has immediate, concrete downstream effects.
% FOUNDING_PROBLEM: The Constitution's framers needed to solve the Articles of Confederation-era problem of states erecting tariffs and discriminatory trade barriers against each other, and needed a mechanism to ensure uniform commercial rules (weights, measures, currency, navigation) for goods actually crossing state lines.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and state-sovereignty advocates (the reading's principal beneficiaries) attest the founding problem is still the correct and complete frame for the clause's scope. Outside that camp, legal historians studying the broader ratification debates and the post-New Deal economic integration of the United States — including scholars who are not aligned with either the states'-rights or federal-power beneficiary sets — argue the founding problem as originally conceived (interstate tariff wars) was a narrower slice of a broader concern about national economic disintegration, and that the modern economy's interdependence has made the founding-era boundary largely non-administrable without either massive state-shopping by firms or a hollowed-out federal regulatory capacity. No fully disinterested corroborating source exists; the historical record itself is contested terrain in the same dispute the reading is meant to resolve.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at 2025) because the reading's core function — preventing state trade wars and ensuring uniform commercial rules for goods crossing state lines — is a genuine, low-cost coordination function; the extraction that exists is the diffuse cost imposed on national uniformity interests and on individuals in states that decline to legislate protections, not a concentrated rent captured by an administrator. Suppression rises sharply after 1995 (0.05 to 0.42) because enforcing the narrow line increasingly requires courts to actively invalidate federal statutes that had operated unchallenged for six decades — the suppression is the judicial machinery required to hold this reading against a much broader status quo, not a stable structural feature. Theater ratio is modest (0.22) — the doctrine is enforced in substantive holdings (Lopez, Morrison), not merely performed, though NFIB v. Sebelius-era commerce clause dicta suggests an increasing rhetorical component decoupled from the case outcomes it accompanies. Accessibility collapse (0.58) and resistance (0.71) are both elevated because this reading, once you accept its textual premises, forecloses a wide swath of federal regulatory action that has operated for decades — but it meets substantial ongoing resistance precisely because it displaces settled expectations built on the broader reading, unlike a genuine mountain which would meet almost none.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local intrastate businesses sit near the beneficiary end: the reading directly expands their autonomy and reduces federal exposure, with essentially arbitrage-grade exit (states can legislate as they choose; businesses can structure operations to stay intrastate). Federal agencies and national uniformity interests sit toward the target end: their statutory reach and operational uniformity depend on a broader reading, and they cannot exit the doctrinal environment by relocating — their remedy is confined to relitigating the underlying constitutional question. Civil rights claimants and intrastate workers sit at the extreme target end: trapped, powerless, and bearing the sharpest cost — the absence of a federal floor when the state floor is inadequate or hostile. This maps precisely onto the expected structural delta: narrow victim set for federal COMMERCIAL regulation, but a real and severe victim set for federal SOCIAL regulation (civil rights, labor) that had relied on commerce-power authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interstate tariff wars and the absence of uniform commercial rules — is substantially resolved; no state currently erects the kind of protectionist trade barriers the framers were responding to. Under this reading's own logic, that means the commerce power's legitimate scope should be narrow, and the reading is coherent, not a mandatrophy case on its own terms. But when the same reading is invoked to invalidate federal civil-rights and labor statutes whose target problem (discrimination, exploitative labor conditions in a national economy) plainly remains live, the founding-problem-status mismatch (dead for tariffs / live for civil rights) becomes visible only because those two policy domains are yoked to the same textual hook. This story documents the tariff-focused founding problem faithfully; the civil-rights mismatch is the sibling readings' argument against confining the clause to this scope, not a defect internal to this reading's own coordination claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_era_semantic_meaning_of_commerce,
    'Did the founding generation understand ''commerce'' to mean only trade/exchange (the narrow reading) or did it also encompass manufacturing, agriculture, and other economic activity with interstate effects (as some corpus-linguistics and founding-era dictionary studies suggest)?',
    'Systematic corpus analysis of founding-era usage across legal, commercial, and popular texts (already partially undertaken by scholars on multiple sides); resolution remains contested because founding-era usage itself may have been genuinely heterogeneous rather than uniformly narrow.',
    'If founding-era usage was broader than pure trade, this reading''s textual foundation weakens substantially and its claim to originalist fidelity is undermined, strengthening the intermediate_channels or broad_effects_test siblings instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_era_semantic_meaning_of_commerce, empirical, 'Whether the narrow trade-only definition of ''commerce'' accurately reflects founding-era semantic usage.').

omega_variable(
    coordination_function_scope_boundary,
    'Is the narrow reading''s coordination function (preventing interstate trade wars, ensuring uniform commercial rules) separable from the broader economic interdependence that developed after 1787, such that a text-bound 1787 scope can still solve a 2025-scale coordination problem?',
    'Comparative institutional analysis of whether states, absent federal civil-rights and labor floors, actually engage in a race-to-the-bottom dynamic comparable to the tariff wars the clause was written to prevent — this is testable via observed state regulatory competition patterns.',
    'If a genuine race-to-the-bottom dynamic exists in labor/environmental standards analogous to 1780s tariff wars, the narrow reading''s own coordination logic would argue for extending federal reach further than this reading permits — an internal tension within the reading''s own premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_scope_boundary, conceptual, 'Whether the narrow reading''s coordination rationale, applied consistently, would require broader federal reach than the reading itself permits.').

omega_variable(
    civil_rights_statute_vulnerability,
    'Would consistent application of this reading actually invalidate the Civil Rights Act of 1964''s public accommodations provisions (as Heart of Atlanta Motel''s commerce-power rationale would be threatened), or would a different constitutional hook (Fourteenth Amendment enforcement power) preserve the same statutory outcome under this reading?',
    'Doctrinal analysis of whether Fourteenth Amendment Section 5 enforcement power, unconnected to the commerce power, provides an independent and sufficient constitutional basis for the civil rights statutes currently justified via Commerce Clause reasoning.',
    'If Section 5 provides an adequate independent basis, the civil-rights victim set in this story''s beneficiary/victim declaration is overstated — the true victim set under a consistently-applied narrow reading would be smaller than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_statute_vulnerability, conceptual, 'Whether civil rights enforcement genuinely depends on the commerce power under this reading or has an independent constitutional foundation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__narrow_originalist, theater_ratio, 1937, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_scope__narrow_originalist, theater_ratio, 1955, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1955, observed).
narrative_ontology:measurement(comm_tr_t1975, commerce_clause_scope__narrow_originalist, theater_ratio, 1975, 0.09).
narrative_ontology:measurement_basis(comm_tr_t1975, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__narrow_originalist, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_scope__narrow_originalist, theater_ratio, 2010, 0.2).
narrative_ontology:measurement_basis(comm_tr_t2010, observed).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__narrow_originalist, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(comm_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__narrow_originalist, base_extractiveness, 1937, 0.15).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_scope__narrow_originalist, base_extractiveness, 1955, 0.12).
narrative_ontology:measurement_basis(comm_be_t1955, observed).
narrative_ontology:measurement(comm_be_t1975, commerce_clause_scope__narrow_originalist, base_extractiveness, 1975, 0.1).
narrative_ontology:measurement_basis(comm_be_t1975, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__narrow_originalist, base_extractiveness, 1995, 0.2).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_scope__narrow_originalist, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement_basis(comm_be_t2010, observed).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__narrow_originalist, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement_basis(comm_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__narrow_originalist, suppression_requirement, 1937, 0.05).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_scope__narrow_originalist, suppression_requirement, 1955, 0.05).
narrative_ontology:measurement_basis(comm_su_t1955, observed).
narrative_ontology:measurement(comm_su_t1975, commerce_clause_scope__narrow_originalist, suppression_requirement, 1975, 0.08).
narrative_ontology:measurement_basis(comm_su_t1975, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__narrow_originalist, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_scope__narrow_originalist, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement_basis(comm_su_t2010, observed).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_scope__narrow_originalist, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(comm_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.08).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the Commerce Clause' into structurally distinct constitutional readings, per the epsilon-invariance principle. Each reading has its own epsilon, its own beneficiary/victim structure, and its own classification. narrow_originalist (this file) shows the lowest extractiveness and narrowest victim set of the three because it confines federal reach to trade-crossing-lines and facilitation-only regulation. broad_effects_test would show substantially higher extractiveness from state sovereignty (aggregation doctrine reaching purely local activity). intermediate_channels sits between, preserving categorical federal reach over channels/instrumentalities while requiring a jurisdictional element for non-economic activity. All three share the identical constitutional text as their kernel; they differ in the committer's reading of 'commerce,' 'regulate,' and the scope of federal power, not in any observable fact about the world.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
