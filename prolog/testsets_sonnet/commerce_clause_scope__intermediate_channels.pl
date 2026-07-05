% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope — Three-Category Test with Limiting Principles (Lopez/Morrison Framework)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'intermediate channels' reading of the
 *   Commerce Clause kernel — the three-category framework articulated in
 *   Lopez (1995) and Morrison (2000): federal power reaches channels of
 *   interstate commerce, instrumentalities/persons/things in interstate
 *   commerce, and activities substantially affecting interstate commerce, but
 *   is checked by express limiting principles (non-economic activity needs a
 *   jurisdictional element, aggregation applies only to economic activity,
 *   and attenuated causal-chain reasoning is disallowed). This reading
 *   occupies a structural middle ground between the narrow originalist
 *   reading (commerce power limited to facilitating interstate trade) and the
 *   broad effects test (any activity with cumulative national economic impact
 *   is reachable). Unlike its siblings, this reading generates a genuinely
 *   contested doctrinal category — the economic/non-economic distinction —
 *   whose administrability is itself disputed among the justices and the
 *   scholarship. The ε value here (0.52) reflects moderate extraction:
 *   substantial federal reach persists within the economic sphere, but a
 *   real, judicially enforced category of exclusively state authority
 *   survives, distinguishing this reading sharply from the broad effects
 *   test's near-total erosion of limits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.52).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.48).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope — Three-Category Test with Limiting Principles (Lopez/Morrison Framework)").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'ff445067-eb5b-48b3-a62f-174d32fa98c5').
narrative_ontology:cs_kernel_codification('ff445067-eb5b-48b3-a62f-174d32fa98c5', fixed_text).
narrative_ontology:cs_authority_grounding('ff445067-eb5b-48b3-a62f-174d32fa98c5', lineage).
narrative_ontology:cs_interpretation_layer_present('ff445067-eb5b-48b3-a62f-174d32fa98c5').
narrative_ontology:cs_reading_relation('ff445067-eb5b-48b3-a62f-174d32fa98c5', commerce_clause_scope__narrow_originalist, influences).
narrative_ontology:cs_reading_relation('ff445067-eb5b-48b3-a62f-174d32fa98c5', commerce_clause_scope__broad_effects_test, influences).
narrative_ontology:cs_axiom('ff445067-eb5b-48b3-a62f-174d32fa98c5', foundational, categorical_limiting_principles_required).
narrative_ontology:cs_axiom_status(categorical_limiting_principles_required, holdable).
narrative_ontology:cs_axiom_grounding('ff445067-eb5b-48b3-a62f-174d32fa98c5', categorical_limiting_principles_required, conventional).
narrative_ontology:cs_axiom('ff445067-eb5b-48b3-a62f-174d32fa98c5', foundational, economic_noneconomic_distinction_is_workable).
narrative_ontology:cs_axiom_status(economic_noneconomic_distinction_is_workable, holdable).
narrative_ontology:cs_axiom_grounding('ff445067-eb5b-48b3-a62f-174d32fa98c5', economic_noneconomic_distinction_is_workable, empirically_contingent).
narrative_ontology:cs_reference_frame('ff445067-eb5b-48b3-a62f-174d32fa98c5', post_new_deal_unbounded_commerce_power).
narrative_ontology:cs_drift_state('ff445067-eb5b-48b3-a62f-174d32fa98c5', contemporary_post_raich_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff445067-eb5b-48b3-a62f-174d32fa98c5', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments_on_traditional_matters).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_economic_actors_seeking_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, gun_free_school_zone_defendants).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, gender_violence_civil_plaintiffs).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_experimentation_on_borderline_economic_activity).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence_of_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, congress).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, dual_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations across economic sectors — labor, environment, finance, health care markets — relying on the substantial-effects and aggregation prongs to reach conduct that never crosses a state line. They litigate the economic/non-economic line in their favor whenever a statute's survival depends on it, and benefit from the doctrine's elasticity within the economic category even as they lose specific cases (Lopez, Morrison) at its edges.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies, agenda_setter).

% Administers the three-category test and its limiting principles case by case, deciding whether a statute's jurisdictional hook, its economic character, and its causal chain to interstate commerce satisfy the framework. The Court authored the limiting principles as a check on its own earlier expansive readings but retains full discretion over where the economic/non-economic line falls in any given case.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Retain primary authority over family law, general criminal law, and education because the doctrine categorically excludes non-economic intrastate conduct lacking a jurisdictional element from federal reach. This exclusion is conditional — Congress can often re-draft a statute to add a jurisdictional hook (an interstate commerce nexus requirement) and recapture the field, so the protection is doctrinal rather than absolute.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments_on_traditional_matters, beneficiary,
    institutional, generational, constrained, national).

% Prosecuted or not prosecuted federally depending on which side of the economic/non-economic line their conduct falls — gun possession near a school was held non-economic and outside federal reach in Lopez, but Congress amended the statute to add a jurisdictional element and largely restored federal prosecutorial reach for the same underlying conduct. Individual defendants have no ability to litigate the doctrinal boundary; they experience it as an accident of statutory drafting.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, gun_free_school_zone_defendants, payer,
    powerless, biographical, trapped, local).

% Sought a federal civil remedy under the Violence Against Women Act's civil-remedy provision, framed by Congress as addressing an activity substantially affecting interstate commerce (aggregate economic impact of gender-motivated violence on national productivity and travel). The Court held this reasoning would erase any principled limit on federal power and struck the provision, leaving plaintiffs to state tort and criminal remedies only — remedies that vary widely in adequacy across states.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, gender_violence_civil_plaintiffs, payer,
    powerless, biographical, trapped, local).

% Study the doctrine's internal coherence: whether the economic/non-economic distinction is administrable, whether the jurisdictional-element and aggregation-limitation rules actually constrain outcomes or merely relocate the discretion into statutory drafting. Many conclude the limiting principles are manipulable — Congress can often achieve near-equivalent regulatory reach by adding a jurisdictional hook or recharacterizing conduct as economic — without endorsing either the narrow or broad alternative reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% Drafts statutes to satisfy the doctrine's requirements after the fact — adding case-specific jurisdictional elements, generating extensive legislative findings characterizing regulated conduct as economic or substantially affecting commerce. Congress's drafting sophistication under this reading functions as a workaround that narrows the practical bite of the limiting principles for well-resourced legislative drafting but not for one-off statutes that lack such findings.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, congress, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, congress, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory jurisdiction between federal and state governments by sorting conduct into three defined categories (channels, instrumentalities, substantial-effects) and applying express limiting principles (jurisdictional element requirement, economic-activity-only aggregation, no attenuated causal chains) — solving the genuine coordination problem of a national economy that needs uniform rules while preserving a zone of exclusively state authority.
% TRANSFER_FUNCTION: Moves regulatory authority and litigation risk: federal agencies and Congress gain reach over conduct characterized as economic, even where locally situated, while individuals whose conduct is categorized non-economic (and lacking a jurisdictional hook) are shifted to state law remedies that vary in strength. The framework also transfers interpretive power to the judiciary, which controls the line-drawing in individual cases.
% ABSENT_VOICES: Individual criminal defendants and civil plaintiffs whose cases turn on the economic/non-economic characterization have no voice in setting the doctrinal test itself — the line is drawn in appellate litigation driven by institutional litigants (DOJ, state attorneys general, advocacy organizations) whose interests diverge from any single defendant's or plaintiff's stake in a particular outcome.
% DISAPPEARANCE_RATIONALE: If the three-category framework and its limiting principles disappeared, federal power would either collapse toward the narrow originalist reading (removing federal reach from vast swaths of current economic regulation, e.g., labor, environmental, and consumer-protection statutes reaching intrastate activity) or expand toward the broad effects test (eliminating the categorical exclusion of non-economic local conduct, e.g., family law and general criminal law would become federally reachable through sufficiently creative aggregation arguments). Either direction would force a wholesale renegotiation of federal-state regulatory boundaries currently settled by this intermediate doctrine.
% FOUNDING_PROBLEM: The New Deal-era broad reading of the commerce power (Wickard v. Filburn and its progeny) had by the 1990s produced no discernible outer limit to federal authority; the Court needed a test that preserved the modern regulatory state's reach over the national economy while re-establishing that some subjects (family law, general criminal law, education) remain categorically outside federal commerce power.
% FOUNDING_PROBLEM_CORROBORATION: The Court itself (in Lopez and Morrison) attests the founding problem — an unbounded commerce power — was live and required correction. Federal agencies and many mainstream constitutional scholars corroborate that some outer limit was doctrinally necessary but dispute whether this specific three-category framework, as opposed to some other limiting device, was the right solution; a substantial minority of scholars and several dissenting Justices argue the founding problem was largely rhetorical and the limiting principles have proven inadministrable in practice, corroborating persistence of the underlying tension rather than its resolution.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) sits at the midpoint of the kernel's range because federal reach is real and growing within the economic category (Raich, 2005, extended aggregation deep into activity many considered non-commercial) while the categorical exclusion for non-economic conduct lacking jurisdictional hooks (Lopez, Morrison) remains judicially enforced and has struck down federal statutes. Suppression (0.48) reflects that the doctrine does foreclose certain regulatory pathways (a bare civil remedy for gender-motivated violence without a jurisdictional element cannot survive), but Congress retains substantial workaround capacity through statutory redrafting, so the foreclosure is real but not airtight. Theater ratio (0.31) captures that a meaningful share of the doctrinal apparatus — extensive legislative findings, elaborate economic-effects narratives attached to statutes — functions to satisfy the test's form rather than to reflect a genuine substantive economic/non-economic distinction; this share has grown modestly over the interval as legislative drafters have become more sophisticated at working the test.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and Congress sit near the beneficiary end: they retain broad reach within the economic category and can often satisfy the limiting principles through careful drafting, converting a nominal constraint into a drafting exercise. State governments benefit conditionally — real authority over family law, general criminal law, and education, but that authority survives only so long as Congress does not add a jurisdictional hook, making it a defeasible rather than an absolute protection. Individual defendants and civil plaintiffs bear the cost of the doctrine's line-drawing without power to influence where the line falls in their own case; their outcomes turn on categorization decisions made in unrelated prior litigation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an effectively unbounded commerce power after Wickard-era expansion — was genuinely live in 1995 and the three-category test with limiting principles was a good-faith judicial response. But the framework's administrability has been contested from the start: critics argue the economic/non-economic distinction has no principled content independent of the outcome a given panel wants to reach, meaning the 'limiting principles' function partly as available doctrinal tools rather than as fixed constraints. This is not classic mandatrophy (the founding problem hasn't disappeared — the tension between national economic regulation and federalism remains live) but it does show characteristics of doctrinal drift: legislative drafting sophistication has partially neutralized the limiting principles' practical bite over the thirty-year interval, without any formal abandonment of the doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_line_administrability,
    'Is the economic/non-economic distinction a principled categorical boundary, or is it a manipulable label that judges apply post hoc to reach preferred outcomes?',
    'Longitudinal analysis of lower-court applications of the Lopez/Morrison framework to determine whether outcomes correlate with independently verifiable features of the regulated conduct (e.g., presence of a market, fungibility, commercial character) or instead correlate with panel composition and case-specific policy stakes.',
    'If the distinction is unprincipled, this reading''s claimed structural coherence collapses toward a disguised discretionary test, functionally converging with either the broad or narrow reading depending on the deciding body''s priors — undermining the reading''s claim to occupy a genuine intermediate position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_administrability, conceptual, 'Whether the reading''s defining categorical distinction is administrable or a manipulable label.').

omega_variable(
    jurisdictional_element_workaround_efficacy,
    'How completely does Congress''s practice of adding case-specific jurisdictional elements neutralize the doctrine''s exclusion of non-economic conduct from federal reach?',
    'Empirical survey of post-Lopez/Morrison federal statutes regulating conduct plausibly non-economic, comparing the rate of jurisdictional-element amendment and subsequent judicial acceptance against the rate of successful facial or as-applied challenges.',
    'High workaround efficacy would mean the state-autonomy protection this reading claims to provide is largely nominal for well-resourced federal legislative drafting, shifting the effective classification of this reading''s protective function toward theater rather than substantive limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_element_workaround_efficacy, empirical, 'Whether the jurisdictional-element requirement is a real constraint or a drafting formality Congress routinely satisfies.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the intermediate_channels reading the doctrinally dominant and currently controlling reading, or is it itself unstable and subject to displacement toward either sibling reading depending on future Court composition?',
    'Track citation patterns, doctrinal commentary, and subsequent case outcomes (e.g., whether courts increasingly cite Raich''s aggregation reasoning expansively, trending toward broad_effects_test, or increasingly cite Lopez/Morrison''s limiting language restrictively, trending toward narrow_originalist).',
    'If the doctrine is drifting toward either sibling, this reading''s authored ε and victim set describe a transitional rather than stable state, and future re-authoring of this story (or a successor) would be warranted to reflect the drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether this reading is the stable controlling doctrine or a way-station between the narrower and broader readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__intermediate_channels, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__intermediate_channels, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__intermediate_channels, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__intermediate_channels, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(comm_tr_t2018, commerce_clause_scope__intermediate_channels, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__intermediate_channels, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__intermediate_channels, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__intermediate_channels, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__intermediate_channels, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__intermediate_channels, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(comm_be_t2018, commerce_clause_scope__intermediate_channels, base_extractiveness, 2018, 0.51).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__intermediate_channels, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__intermediate_channels, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__intermediate_channels, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__intermediate_channels, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_scope__intermediate_channels, suppression_requirement, 2012, 0.46).
narrative_ontology:measurement(comm_su_t2018, commerce_clause_scope__intermediate_channels, suppression_requirement, 2018, 0.47).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_scope__intermediate_channels, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the Commerce Clause scope.' Each reading of the commerce_clause_scope kernel is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle: narrow_originalist confines federal power to facilitating interstate trade (low ε, minimal federal reach, maximal state autonomy); intermediate_channels (this story) applies the three-category test with limiting principles (medium ε, categorical exclusions coexisting with substantial federal reach within the economic sphere); broad_effects_test extends federal power to any activity with cumulative national economic effect (high ε, minimal categorical limits). All three are linked bidirectionally via affects_constraints to represent the kernel contest; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
