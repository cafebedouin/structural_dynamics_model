% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Three-Category Framework with Categorical Limiting Principles
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the intermediate reading of the commerce clause
 *   scope kernel: federal power reaches three enumerated categories
 *   (channels, instrumentalities, and substantially-affecting economic
 *   activity) but is checked by categorical limiting principles — a
 *   jurisdictional-element requirement for non-economic conduct, restriction
 *   of aggregation to economic activity, and a bar on regulation through
 *   attenuated causal chains. This is the doctrine as articulated across
 *   Lopez, Morrison, and Raich, distinct from the narrow_originalist reading
 *   (commerce power limited to facilitating interstate trade) and the
 *   broad_effects_test reading (any activity with cumulative economic effect,
 *   non-economic or not). Under this reading, ε sits at a medium level: the
 *   standing arrangement lets federal power reach nearly the entire economic
 *   sphere while the limiting principles are real but administratively
 *   unstable, producing genuine but incompletely reliable victim exclusion.
 *
 * KEY AGENTS:
 *   - federal_regulatory_authority: institutional beneficiary/agenda_setter — drafts statutes to fit the three categories
 *   - state_family_and_criminal_law_authority: institutional beneficiary — retains police power carve-out
 *   - national_market_participants: organized beneficiary — benefits from uniform economic regulation
 *   - local_noneconomic_conduct_regulators: moderate payer — loses federal hooks for non-economic local harms
 *   - civil_rights_plaintiffs_relying_on_aggregation: powerless payer — barred from aggregation theory for non-economic harms
 *   - doctrinal_coherence: analytical non-agent payer — the economic/non-economic line is manipulable
 *   - reviewing_courts: institutional agenda_setter — administers the categorical tests case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.48).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Three-Category Framework with Categorical Limiting Principles").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '91c866d3-8558-4345-9c7e-519dcc2c086a').
narrative_ontology:cs_kernel_codification('91c866d3-8558-4345-9c7e-519dcc2c086a', fixed_text).
narrative_ontology:cs_authority_grounding('91c866d3-8558-4345-9c7e-519dcc2c086a', lineage).
narrative_ontology:cs_interpretation_layer_present('91c866d3-8558-4345-9c7e-519dcc2c086a').
narrative_ontology:cs_reading_relation('91c866d3-8558-4345-9c7e-519dcc2c086a', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('91c866d3-8558-4345-9c7e-519dcc2c086a', commerce_clause_scope__broad_effects_test, influences).
narrative_ontology:cs_axiom('91c866d3-8558-4345-9c7e-519dcc2c086a', foundational, categorical_limits_are_judicially_enforceable).
narrative_ontology:cs_axiom_status(categorical_limits_are_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('91c866d3-8558-4345-9c7e-519dcc2c086a', categorical_limits_are_judicially_enforceable, conventional).
narrative_ontology:cs_axiom('91c866d3-8558-4345-9c7e-519dcc2c086a', foundational, economic_activity_is_a_coherent_natural_kind_for_aggregation_purposes).
narrative_ontology:cs_axiom_status(economic_activity_is_a_coherent_natural_kind_for_aggregation_purposes, holdable).
narrative_ontology:cs_axiom_grounding('91c866d3-8558-4345-9c7e-519dcc2c086a', economic_activity_is_a_coherent_natural_kind_for_aggregation_purposes, empirically_contingent).
narrative_ontology:cs_reference_frame('91c866d3-8558-4345-9c7e-519dcc2c086a', post_lopez_categorical_synthesis).
narrative_ontology:cs_drift_state('91c866d3-8558-4345-9c7e-519dcc2c086a', contemporary_post_nfib_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('91c866d3-8558-4345-9c7e-519dcc2c086a', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_regulatory_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_family_and_criminal_law_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_market_participants).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_noneconomic_conduct_regulators).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, civil_rights_plaintiffs_relying_on_aggregation).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, doctrinal_coherence).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, dual_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and federal agencies retain broad authority to regulate channels, instrumentalities, and economic activity substantially affecting interstate commerce. They draft statutes with express jurisdictional hooks or economic findings to fit within the three categories, and litigate to defend that fit when challenged. The framework lets them reach nearly all economic regulation while requiring extra drafting care for anything touching traditionally local or non-economic conduct.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_regulatory_authority, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, federal_regulatory_authority, agenda_setter).

% State legislatures and courts retain primary authority over family law, general criminal law, and education because the framework excludes non-economic, traditionally local activity absent a jurisdictional element or channel/instrumentality nexus. This is a genuine carve-out that preserves state police power, though it depends entirely on courts continuing to hold the economic/non-economic line.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_family_and_criminal_law_authority, beneficiary,
    institutional, generational, constrained, national).

% Firms operating across state lines benefit from a uniform federal regulatory floor for genuinely economic activity — antitrust, labor standards, environmental rules — administered under a single coherent doctrinal category rather than fragmented state-by-state regimes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_market_participants, beneficiary,
    organized, biographical, mobile, national).

% Local governments and advocates who want federal involvement in matters framed as non-economic (e.g., gun-free school zones, gender-motivated violence remedies) find their preferred federal hooks foreclosed unless they can locate a jurisdictional element or recharacterize the conduct as economic. They bear the cost of the categorical line even when the underlying harm has real interstate dimensions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_noneconomic_conduct_regulators, payer,
    moderate, biographical, constrained, regional).

% Plaintiffs seeking federal civil remedies for non-economic harms (e.g., violence against women) cannot use aggregation to establish substantial effects because aggregation is reserved for economic activity under this reading. They are pushed back to state remedies regardless of whether those remedies are adequate, and cannot access the federal forum the broad_effects_test reading would have provided them.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, civil_rights_plaintiffs_relying_on_aggregation, payer,
    powerless, biographical, trapped, national).

% The economic/non-economic distinction and the jurisdictional-element requirement are manipulable in application — courts and litigants can often characterize the same conduct as economic or non-economic depending on desired outcome, and the line between substantial effects and attenuated causal chains resists principled application across cases. The framework's stability depends on judicial discipline that is not structurally guaranteed.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, doctrinal_coherence, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, doctrinal_coherence).

% Federal courts, particularly the Supreme Court, administer the three-category framework and its limiting principles, deciding in each case whether conduct is economic or non-economic, whether a jurisdictional element cures an otherwise excluded statute, and whether a causal chain is too attenuated. Their discretion in applying these categorical tests is itself a form of policymaking power.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, reviewing_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable middle doctrine that lets the federal government regulate the national economy coherently through three enumerated categories while preserving a genuine, judicially enforceable zone of state authority over traditionally local and non-economic matters — solving the problem of unbounded federal power without returning to the pre-New-Deal near-total exclusion of federal economic regulation.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction and remedial access: federal authority captures nearly all economic activity with interstate effects (including aggregated conduct), while local governments and individual plaintiffs seeking federal remedies for non-economic harms are redirected to state forums, regardless of the practical adequacy of state remedies or the real interstate dimensions of the underlying harm.
% ABSENT_VOICES: Individuals harmed by non-economic conduct with genuine interstate dimensions (e.g., interstate stalking treated as insufficiently 'economic,' trafficking survivors whose cases don't fit cleanly into a channel-of-commerce theory) are not parties to the doctrinal debate about where the economic/non-economic line sits; the line is drawn in appellate litigation between governments and organized interests, not by the people whose access to a federal forum depends on it.
% DISAPPEARANCE_RATIONALE: If this three-category framework with its limiting principles vanished, either commerce power would collapse toward the narrow_originalist reading (eliminating most modern federal economic regulation — labor law, environmental law, much of antitrust) or expand toward the broad_effects_test reading (eliminating the state police-power carve-outs for family law, education, and general criminal law). Either direction would trigger a substantial reallocation of which government regulates what, and would reopen access to federal remedies currently foreclosed by the economic/non-economic distinction.
% FOUNDING_PROBLEM: The doctrine was built to resolve a specific institutional crisis: the pre-1937 Court's overly narrow reading of commerce power had produced repeated invalidation of New Deal economic legislation, provoking a constitutional confrontation with the political branches, while the unbounded 1937-1995 reading (upholding commerce power reaching a single farmer's home-grown wheat) threatened to erase any meaningful limit on federal power and any distinct zone of state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Federal government litigators and academic defenders of the framework attest the categorical limits are a live, principled check still doing real work (citing invalidations of federal statutes lacking jurisdictional elements). Independent constitutional scholars outside both the federal-power and states'-rights advocacy camps — including textualist and living-constitutionalist commentators who otherwise disagree on nearly everything — corroborate that the economic/non-economic line has proven difficult to administer consistently across cases and functions partly as a discretionary lever for courts rather than a fully determinate rule; several sitting and former federal judges have written separately questioning the line's coherence even while applying it.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects that the framework transfers real jurisdiction and remedial access away from a moderate victim set (local governments and individual plaintiffs seeking federal non-economic remedies) toward federal and state institutional beneficiaries — but the transfer is bounded by genuine categorical limits, unlike the broad_effects_test reading where extraction would be higher (no non-economic carve-out) or the narrow_originalist reading where extraction would be near zero (federal power barely reaches beyond trade facilitation). Suppression (0.42) is moderate: the doctrine does not suppress alternative regulatory routes so much as require litigants and legislators to characterize conduct carefully to fit the categories — a drafting and litigation cost, not an outright bar. Theater ratio rises modestly over the interval (0.25 to 0.38) as courts and litigants increasingly perform careful jurisdictional-element and economic-characterization arguments whose actual constraining force is contested — the ritual of finding or drafting a jurisdictional hook sometimes substitutes for a genuine substantive limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory authority and national market participants sit near the beneficiary end: the three-category framework preserves nearly all of their post-1937 regulatory reach. State family/criminal/education authority is also a beneficiary within its carved-out sphere, though its exit options are constrained by dependence on courts continuing to police the line. Local non-economic conduct regulators and civil rights plaintiffs relying on aggregation sit near the target end: they bear the cost of exclusion from federal remedies whenever their preferred conduct is characterized as non-economic, with the powerless plaintiff seat carrying the least ability to relitigate that characterization. Doctrinal coherence is authored as a non-agent payer — it collects no rents and cannot act, but the framework's operation degrades it, which is why it belongs in victims rather than beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unbounded post-1937 commerce power on one side and a paralyzing pre-1937 restriction on the other — is genuinely contested rather than dead: the doctrine continues to do real invalidating work (Lopez, Morrison), which distinguishes it from a pure zombie mandate. But the manipulability of the economic/non-economic line means the doctrine's actual constraining force in any given case depends heavily on how courts characterize conduct, which is exactly the disappearance_verdict='world_rearranges' + founding_problem_status='contested' combination that should NOT be read as capture — this is a live, contested doctrine, not a hollowed-out one performing function it no longer serves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic/non-economic distinction a principled, judicially administrable line, or is it manipulable enough that outcomes depend on which category a court chooses to apply rather than on a determinate rule?',
    'Empirical coding of circuit and Supreme Court commerce clause decisions post-Lopez to measure inter-judge and inter-circuit consistency in economic/non-economic characterization of similar fact patterns.',
    'If the line proves highly manipulable, the categorical limiting principles function more as discretionary judicial levers than as genuine constraints on federal power, which would push this reading''s effective operation closer to the broad_effects_test reading in practice despite differing in doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, empirical, 'Whether the economic/non-economic distinction is a stable, principled line or a manipulable discretionary lever.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the intermediate_channels reading a stable equilibrium among the three sibling readings, or is it a transitional compromise that will drift toward either the narrow_originalist or broad_effects_test pole as Court composition changes?',
    'Track citation patterns and doctrinal drift across subsequent commerce clause cases; a reading that persists across multiple changes in Court composition without erosion toward either pole is more plausibly a stable equilibrium than a transitional artifact.',
    'If the reading is transitional rather than stable, ε and the victim set authored here describe only a temporary state, and the true long-run constraint may be one of the sibling readings rather than this intermediate one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether this reading is a stable doctrinal equilibrium or a transitional compromise between the sibling readings.').

omega_variable(
    jurisdictional_element_as_formalism,
    'Does the jurisdictional-element requirement (e.g., requiring proof the specific firearm traveled in interstate commerce) provide a genuine substantive check on federal overreach, or is it a formality that legislatures can satisfy with boilerplate language while achieving the same practical reach as an unlimited commerce power?',
    'Compare federal statutes drafted with jurisdictional elements post-Lopez against their practical enforcement scope — if enforcement reaches conduct functionally identical to what would have been reached absent the element, the requirement is largely formal.',
    'If largely formal, the framework''s claimed state-autonomy-preserving function is substantially theatrical, which would push the classification toward tangled_rope with a higher effective extractiveness than the base score suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_element_as_formalism, empirical, 'Whether the jurisdictional-element cure is a substantive limit or a drafting formality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__intermediate_channels, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__intermediate_channels, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__intermediate_channels, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__intermediate_channels, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(comm_tr_t2018, commerce_clause_scope__intermediate_channels, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__intermediate_channels, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__intermediate_channels, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__intermediate_channels, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__intermediate_channels, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__intermediate_channels, base_extractiveness, 2012, 0.47).
narrative_ontology:measurement(comm_be_t2018, commerce_clause_scope__intermediate_channels, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__intermediate_channels, base_extractiveness, 2025, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(commerce_clause_scope__intermediate_channels, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
