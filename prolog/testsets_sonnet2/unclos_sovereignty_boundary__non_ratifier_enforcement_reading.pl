% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary Law Enforced by Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   unclos_sovereignty_boundary kernel: the non-ratifier enforcement reading,
 *   under which freedom-of-navigation principles are treated as customary
 *   international law that exists independently of UNCLOS ratification and is
 *   enforced through naval presence rather than treaty mechanisms. Under this
 *   reading, a major naval power that never ratified UNCLOS nonetheless
 *   invokes its navigation provisions as binding custom, conducts
 *   freedom-of-navigation operations through contested straits and EEZs, and
 *   thereby decouples the operative maritime-access regime from the treaty
 *   text itself. This is structurally distinct from the strict_eez_reading
 *   (which holds Article 57's 200nm boundaries exclusive and enforceable,
 *   full stop) and the historical_rights_reading (which grounds sovereign
 *   claims in occupation and usage predating UNCLOS). Each reading is
 *   authored as its own constraint story with its own epsilon; this file does
 *   not average across them or describe the underlying contest — it authors
 *   only this reading's structure. Under this reading, naval powers become
 *   beneficiaries (they enforce rules they never contractually accepted) and
 *   coastal states attempting EEZ exclusivity become victims (their
 *   treaty-based claims are contested by a party outside the treaty's
 *   obligations).
 *
 * KEY AGENTS:
 *   - major_naval_powers: primary beneficiary/agenda_setter (institutional/arbitrage) — asserts and enforces customary-law navigation rights without treaty obligations
 *   - global_shipping_and_trade_interests: secondary beneficiary (organized/mobile) — benefits from open sea lanes without bearing enforcement cost
 *   - coastal_states_asserting_eez_exclusivity: primary target (moderate/constrained) — treaty-based claims contested by non-ratifier naval presence
 *   - small_littoral_states_without_naval_capacity: powerless payer (trapped) — absorbs whatever settlement larger actors negotiate
 *   - unclos_treaty_body_and_tribunals: excluded institutional voice — sidelined by a reading that locates law in custom rather than treaty
 *   - international_law_scholars: analytical observer — contests whether the customary-law crystallization claim is doctrinally sound
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.71).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary Law Enforced by Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '8cafb809-01b4-4d61-8626-27f6a2da0ab1').
narrative_ontology:cs_kernel_codification('8cafb809-01b4-4d61-8626-27f6a2da0ab1', distributed).
narrative_ontology:cs_authority_grounding('8cafb809-01b4-4d61-8626-27f6a2da0ab1', distributed).
narrative_ontology:cs_reading_relation('8cafb809-01b4-4d61-8626-27f6a2da0ab1', unclos_sovereignty_boundary__strict_eez_reading, influences).
narrative_ontology:cs_reading_relation('8cafb809-01b4-4d61-8626-27f6a2da0ab1', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('8cafb809-01b4-4d61-8626-27f6a2da0ab1', foundational, custom_binds_independent_of_treaty_ratification).
narrative_ontology:cs_axiom_status(custom_binds_independent_of_treaty_ratification, holdable).
narrative_ontology:cs_axiom_grounding('8cafb809-01b4-4d61-8626-27f6a2da0ab1', custom_binds_independent_of_treaty_ratification, conventional).
narrative_ontology:cs_axiom('8cafb809-01b4-4d61-8626-27f6a2da0ab1', secondary, naval_presence_constitutes_valid_enforcement_of_customary_navigation_rights).
narrative_ontology:cs_axiom_status(naval_presence_constitutes_valid_enforcement_of_customary_navigation_rights, holdable).
narrative_ontology:cs_axiom_grounding('8cafb809-01b4-4d61-8626-27f6a2da0ab1', naval_presence_constitutes_valid_enforcement_of_customary_navigation_rights, instrumental).
narrative_ontology:cs_reference_frame('8cafb809-01b4-4d61-8626-27f6a2da0ab1', pre_unclos_customary_navigation_practice).
narrative_ontology:cs_drift_state('8cafb809-01b4-4d61-8626-27f6a2da0ab1', contemporary_multipolar_naval_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8cafb809-01b4-4d61-8626-27f6a2da0ab1', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_and_trade_interests).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_littoral_states_without_naval_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct routine 'freedom of navigation operations' asserting that transit and overflight rights exist as customary international law binding on all states regardless of UNCLOS ratification. Deploys naval vessels through contested straits and claimed EEZs to contest what it calls excessive maritime claims. Never ratified UNCLOS itself, yet invokes its navigation provisions as customary law while declining the treaty's dispute-resolution and seabed-authority obligations. Bears essentially no cost from operating this way; its fleet provides the enforcement machinery no international body possesses.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, beneficiary).

% Commercial shipping, insurers, and trading states rely on predictable open sea lanes. They benefit directly when naval presence keeps chokepoints and contested waters open, lowering insurance premiums and transit risk, without themselves bearing any enforcement cost or diplomatic exposure.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_and_trade_interests, beneficiary,
    organized, generational, mobile, global).

% Ratified UNCLOS and rely on its Article 57 EEZ provisions to exclude foreign military activity and resource extraction within 200 nautical miles. Find their claimed exclusivity repeatedly contested by naval transits from a state that never accepted the treaty's obligations while claiming its navigation rights. Their formal legal recourse (UNCLOS tribunals) has no enforcement mechanism against a non-ratifier's naval presence; their practical options are diplomatic protest, regional coalition-building, or acquiescence.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    moderate, biographical, constrained, regional).

% Lack any naval or diplomatic capacity to contest either the naval power's transits or larger coastal states' overlapping claims. Whichever reading of the kernel prevails is decided over their heads by fleets and treaty texts alike; they absorb whatever settlement other actors negotiate, with no seat at the table shaping it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_littoral_states_without_naval_capacity, payer,
    powerless, biographical, trapped, regional).

% The formal adjudicatory apparatus created by the treaty this reading treats as merely codifying pre-existing custom. Its rulings on EEZ disputes carry no binding force over the non-ratifier, whose participation in the customary-law regime is asserted unilaterally rather than through the tribunal system. Its voice is structurally sidelined by a reading that locates the operative law outside the treaty it administers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_treaty_body_and_tribunals, excluded,
    institutional, generational, analytical, global).

% Debate whether freedom-of-navigation norms genuinely crystallized as customary international law prior to and independent of UNCLOS, or whether the customary-law claim is a legal fiction constructed to let a non-ratifier selectively invoke favorable provisions while rejecting binding obligations.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps international straits, high-seas corridors, and contested EEZ waters navigable for military and commercial transit, preventing a patchwork of unilateral coastal-state closures from fragmenting global sea lanes.
% TRANSFER_FUNCTION: Moves practical control over contested maritime access from coastal states asserting exclusivity to the naval power whose presence determines which claims are contested and which are tacitly allowed to stand; shifts enforcement cost from a multilateral treaty body to a single state's fleet, and shifts diplomatic and legal risk onto coastal states who must contest transits without a forum that binds the transiting power.
% ABSENT_VOICES: The UNCLOS treaty body and its tribunals are the natural forum for adjudicating these disputes but are structurally sidelined by a reading that locates the operative law in custom rather than treaty; smaller littoral states with claims in contested waters have no naval capacity to contest transits and are rarely party to the bilateral protests that do occur.
% DISAPPEARANCE_RATIONALE: If the customary-law claim and its naval enforcement vanished overnight, coastal states' EEZ claims would face no unilateral contestation from non-ratifiers; UNCLOS tribunals would become the sole authoritative forum; some chokepoints could see genuine attempts at exclusive coastal control, and global shipping would face new negotiation costs at multiple straits absent a standing naval guarantor.
% FOUNDING_PROBLEM: Coastal states unilaterally expanding maritime claims (territorial seas, EEZs, straight baselines) threatened to fragment historically open sea lanes essential to global trade and naval mobility, and no treaty existed yet, or a state's non-ratification left it formally unbound, that could compel restraint.
% FOUNDING_PROBLEM_CORROBORATION: The naval power's own defense and state department documents assert that navigation freedoms are settled customary law predating the treaty. Coastal states and multiple international law scholars, publishing independently of the naval power's position, dispute that the custom crystallized as claimed and argue the doctrine functions to let the non-ratifier enjoy treaty benefits without treaty obligations; UNCLOS tribunal opinions and ITLOS commentary, produced by the excluded adjudicatory body itself, treat the disputed claims as properly justiciable under the treaty rather than settled by custom.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects that the naval power captures the practical benefit of navigation-rights enforcement — keeping sea lanes open on its terms — while bearing none of the treaty's reciprocal obligations (seabed authority payments, dispute submission, ratified boundary commitments) that ratifying coastal states accepted as the price of the same regime. Suppression (0.71) is high because contesting the naval power's transits has no binding forum: UNCLOS tribunals cannot compel a non-ratifier, so coastal states' only levers are diplomatic protest or regional coalition, both of which the naval power's power asymmetry renders largely ineffective. Theater ratio (0.4) is moderate: freedom-of-navigation operations are substantively real (they do open contested waters) but are also partly performative demonstrations of resolve, calibrated for domestic and allied audiences as much as for the specific contested claim. Accessibility collapse (0.5) is mid-range — coastal states retain the theoretical alternative of building naval capacity or regional coalitions, so the alternative is not fully foreclosed, merely very costly. Resistance (0.68) is substantial: coastal states protest routinely and some (particularly in Southeast Asia and Latin America) have pursued tribunal rulings against overlapping claims, even though those rulings cannot bind the non-ratifier directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Major naval powers sit at the beneficiary end: they set the operational tempo of enforcement, bear negligible cost, and gain the practical benefit of an open-navigation regime without treaty-based reciprocal obligations — d is low despite institutional power because the constraint subsidizes rather than constrains them. Global shipping interests are similarly beneficiaries with mobile exit (they can reroute around specific hotspots) and no enforcement burden. Coastal states asserting EEZ exclusivity are targets: their treaty-conferred claims are the object being contested, and their exit options are constrained (regional coalition-building, limited naval buildup) rather than mobile. Small littoral states without naval capacity sit nearest the full-target end — trapped exit, no leverage over either the naval power or larger coastal-state negotiations that determine outcomes over their heads.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unilateral coastal-state overreach threatening open sea lanes — has genuine ongoing elements (some claims are extravagant) but the reading's own enforcement mechanism has outgrown a narrowly corrective function: it now operates as a general-purpose tool for contesting treaty-conferred boundaries the naval power finds inconvenient, regardless of whether the specific claim is genuinely excessive under the treaty text it declines to ratify. Classifying this as tangled_rope rather than pure rope (coordination) or pure snare (extraction) captures that a genuine coordination function exists (keeping global sea lanes navigable serves nearly everyone) alongside asymmetric extraction (the enforcing power captures the coordination benefit while externalizing the treaty's reciprocal costs onto the states that did ratify).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_crystallization_question,
    'Did freedom-of-navigation norms actually crystallize as binding customary international law prior to and independently of UNCLOS, satisfying the traditional test of consistent state practice plus opinio juris — or is the customary-law claim a post-hoc legal construction that lets a non-ratifier selectively invoke favorable treaty provisions while rejecting binding ones?',
    'Comprehensive historical review of pre-UNCLOS state practice and diplomatic correspondence across a representative sample of maritime states, assessed against the ICJ''s customary-law formation criteria (North Sea Continental Shelf standard); a genuine crystallization finding would require near-uniform pre-1982 state practice recognizing these specific navigation rights as legally binding rather than merely customary courtesy.',
    'If the customary-law claim does not survive scrutiny, this reading collapses into pure extraction dressed as coordination — a snare rather than a tangled_rope, since the coordination justification would be fabricated. If the claim holds, the tangled_rope classification is more defensible as genuine (if asymmetric) coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_crystallization_question, empirical, 'Whether the customary-law premise underlying this reading is doctrinally sound or a constructed justification.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does this reading (rather than strict_eez_reading or historical_rights_reading) dominate in practice for the naval power specifically, while other actors invoke different readings for their own contested claims?',
    'Comparative analysis of which reading each major maritime actor invokes across different disputes — testing whether reading selection correlates with which reading favors that actor''s specific interest in each case, which would indicate readings are chosen opportunistically rather than held as consistent legal principle.',
    'If reading selection correlates strongly with self-interest across cases, it supports treating all three readings as strategically deployed rather than genuinely held legal positions, which would elevate suppression and extractiveness scores across the whole kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether kernel-reading selection tracks genuine doctrinal commitment or case-by-case strategic interest.').

omega_variable(
    enforcement_capacity_asymmetry_permanence,
    'Is the naval enforcement asymmetry that sustains this reading a permanent structural feature of the international system, or could multipolar naval capacity growth (regional coalitions, rising powers) eventually produce competing enforcement claims that neutralize the current reading''s dominance?',
    'Track naval capacity distribution and freedom-of-navigation operation frequency by multiple state actors over a multi-decade horizon; a shift toward multiple states conducting competing FONOPs under this same doctrine would indicate the asymmetry is eroding.',
    'A durable unipolar enforcement capacity supports treating this as stable tangled_rope; emerging multipolar enforcement competition would push toward reclassifying the whole kernel as an unstable equilibrium approaching great-power conflict risk rather than a settled constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry_permanence, empirical, 'Whether the naval enforcement asymmetry underlying this reading is a durable or transitional structural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, historical_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the UNCLOS sovereignty boundary dispute.' Each sibling instantiates a structurally distinct claim with its own beneficiary/victim set and its own epsilon: strict_eez_reading treats Article 57 boundaries as exclusive treaty law (coastal-state-favoring); historical_rights_reading grounds sovereignty in pre-treaty occupation and usage (favors states with long-standing physical presence claims, e.g. in disputed archipelagic waters); non_ratifier_enforcement_reading (this file) treats navigation freedoms as binding custom enforced by naval presence independent of ratification (favors naval powers with global reach). The three are linked via affects_constraints because a shift in dominance among readings in practice (e.g., a major tribunal ruling, a shift in naval posture, an accession to the treaty by the non-ratifier) would propagate pressure across all three siblings' legitimacy and resource conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
