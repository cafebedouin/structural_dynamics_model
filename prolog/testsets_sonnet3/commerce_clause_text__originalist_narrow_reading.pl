% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Only)
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   This story instantiates the originalist narrow reading of the Commerce
 *   Clause kernel: federal power reaches only trade that physically crosses a
 *   state border or uses an instrumentality of interstate movement, with all
 *   other economic activity presumptively reserved to state police power.
 *   From 1937 to the mid-1990s this reading was largely dormant as the
 *   doctrinal minority position while the substantial-effects reading
 *   governed; since United States v. Lopez (1995) and United States v.
 *   Morrison (2000), it has re-entered live jurisprudence as an active
 *   constraint on federal reach, most visibly in Commerce Clause challenges
 *   to federal labor, environmental, and gun-free-zone statutes. The metrics
 *   track this reading's own operation as a live legal constraint from 1937
 *   (when it existed mainly as a losing dissent position, hence lower
 *   measured extraction and higher relative theater) through its post-1995
 *   revival as a binding limit with real doctrinal teeth.
 *
 * KEY AGENTS:
 *   - state_governments: primary beneficiary — retain police power shielded from federal reach
 *   - anti_federal_consolidation_advocates: ideological beneficiary — the reading is the doctrinal mechanism for their founding commitment
 *   - intrastate_incumbent_industries: economic beneficiary — structure operations to stay within the shelter
 *   - cross_border_pollution_sufferers, low_wage_intrastate_workers, interstate_migrant_laborers: primary targets — bear externalized costs of activity shielded by intrastate characterization
 *   - federal_judiciary_originalist_wing: agenda-setter — actively polices and enforces the border-crossing line
 *   - congress: excluded — legislative judgment about needed uniformity is foreclosed by the doctrinal test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.38).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Only)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '250d9578-4d84-493c-a759-2aa7279d570d').
narrative_ontology:cs_kernel_codification('250d9578-4d84-493c-a759-2aa7279d570d', fixed_text).
narrative_ontology:cs_authority_grounding('250d9578-4d84-493c-a759-2aa7279d570d', lineage).
narrative_ontology:cs_interpretation_layer_present('250d9578-4d84-493c-a759-2aa7279d570d').
narrative_ontology:cs_reading_relation('250d9578-4d84-493c-a759-2aa7279d570d', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('250d9578-4d84-493c-a759-2aa7279d570d', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('250d9578-4d84-493c-a759-2aa7279d570d', foundational, enumerated_powers_require_textual_border_crossing_nexus).
narrative_ontology:cs_axiom_status(enumerated_powers_require_textual_border_crossing_nexus, holdable).
narrative_ontology:cs_axiom_grounding('250d9578-4d84-493c-a759-2aa7279d570d', enumerated_powers_require_textual_border_crossing_nexus, conventional).
narrative_ontology:cs_axiom('250d9578-4d84-493c-a759-2aa7279d570d', foundational, state_police_power_presumptively_retained_absent_clear_federal_textual_warrant).
narrative_ontology:cs_axiom_status(state_police_power_presumptively_retained_absent_clear_federal_textual_warrant, holdable).
narrative_ontology:cs_axiom_grounding('250d9578-4d84-493c-a759-2aa7279d570d', state_police_power_presumptively_retained_absent_clear_federal_textual_warrant, deontological).
narrative_ontology:cs_reference_frame('250d9578-4d84-493c-a759-2aa7279d570d', founding_era_enumerated_powers_framework).
narrative_ontology:cs_drift_state('250d9578-4d84-493c-a759-2aa7279d570d', post_new_deal_administrative_state_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('250d9578-4d84-493c-a759-2aa7279d570d', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, intrastate_incumbent_industries).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, cross_border_pollution_sufferers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, low_wage_intrastate_workers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_market_uniformity_seekers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_migrant_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain regulatory police power over labor, environment, and commerce within their borders because federal reach stops at the state line except for goods and persons actually crossing it. Can set local standards, including weaker ones, without federal preemption. Litigate aggressively to keep the border-crossing line narrow whenever Congress tries to regulate an intrastate activity by pointing to its national ripple effects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, state_governments, agenda_setter).

% Political and legal movement whose founding commitment is that federal power must remain enumerated and limited. The narrow reading is not incidental to their goals but is the central mechanism by which they prevent what they see as unchecked national regulatory expansion. They fund litigation and judicial appointments to entrench this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, civilizational, mobile, national).

% Firms whose production, labor practices, or pollution stays formally within one state's borders benefit from being outside federal reach even when their economic effects (price competition, wage suppression, downstream pollution) cross state lines in substance. They structure operations to avoid the appearance of border-crossing activity specifically to stay under this reading's shelter.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, intrastate_incumbent_industries, beneficiary,
    powerful, biographical, constrained, regional).

% Residents of downstream or downwind states who bear the costs of an upstream state's under-regulated intrastate industry. Because the pollution's SOURCE is intrastate under this reading, federal environmental regulation of the source is contestable even though the harm is interstate in fact. They cannot vote in the polluting state and have no direct legal lever against the source.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, cross_border_pollution_sufferers, payer,
    powerless, generational, trapped, regional).

% Workers in industries a state has classified as purely intrastate (e.g., certain manufacturing or agricultural labor not formally shipped across lines at the point of labor) lose access to federal minimum labor standards that would apply if their activity were read as commerce. Their remedy is confined to whatever protection their home state chooses to legislate, and interstate wage competition pressures states to legislate less.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, low_wage_intrastate_workers, payer,
    powerless, biographical, trapped, regional).

% Businesses and consumers who would benefit from a single national regulatory standard (e.g., one food-safety rule, one securities disclosure regime) instead of 50 different state regimes triggered whenever an activity is read as intrastate. Compliance costs and market fragmentation are the price of the narrow reading's insistence on a border-crossing trigger.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_market_uniformity_seekers, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__originalist_narrow_reading, national_market_uniformity_seekers).

% Workers who move across state lines for work but whose labor conditions, once they arrive, are treated as purely intrastate matters beyond the reach of federal labor law under this reading. The border-crossing act of migration is federally cognizable; the conditions of the work they migrated for are not.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_migrant_laborers, payer,
    powerless, biographical, trapped, national).

% Judges committed to this reading actively police the border-crossing/instrumentalities line in commerce clause litigation, striking down federal statutes that regulate activity characterized as intrastate no matter how substantial its aggregate economic effect. Their continued adherence to the reading is what keeps it operative rather than merely rhetorical.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_judiciary_originalist_wing, agenda_setter,
    institutional, civilizational, analytical, national).

% Would prefer, in many enacted statutes, to reach intrastate activity with substantial interstate effects (labor standards, environmental spillovers, market fragmentation) but is foreclosed from doing so under this reading unless it can characterize the activity as literally border-crossing or as use of an interstate instrumentality. Its considered legislative judgment about what needs national uniformity is not part of the test this reading applies.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, congress, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the vertical division of regulatory authority between federal and state governments by giving both sides a bright, administrable line — trade that physically crosses a state border or uses an interstate instrumentality is federal; everything else is presumptively state. This reduces jurisdictional litigation cost relative to an open-ended effects test and lets states experiment with different regulatory regimes.
% TRANSFER_FUNCTION: Moves regulatory authority and the associated rents from the federal government and would-be uniform national standard beneficiaries to state governments and in-state incumbent industries; moves the cost of unregulated interstate spillovers (pollution, wage suppression, market fragmentation) onto diffuse, often powerless, cross-border populations who have no vote or standing in the state whose intrastate characterization shields the activity.
% ABSENT_VOICES: Cross-border pollution sufferers, low-wage intrastate workers, and interstate migrant laborers would object that the border-crossing line is a formal fiction that lets substantively interstate harms escape federal reach by characterizing their source as local. They are not party to the litigation that entrenches this reading — that litigation is conducted by states and industries with standing and resources, not by the diffuse victims of the externalities the line permits.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight in favor of a broader federal reach, state governments would lose exclusive regulatory authority over reclassified intrastate activity, incumbent industries currently shielded by intrastate characterization would face new federal labor, environmental, or market regulation, and the anti-consolidation movement would lose its principal doctrinal lever against federal expansion. The border-crossing test is actively load-bearing for who gets to regulate what.
% FOUNDING_PROBLEM: At the founding, the commerce clause was meant to solve the collective-action failure of state-level trade wars and tariff barriers under the Articles of Confederation — states erecting protectionist barriers against each other's goods, with no federal power to compel a national free-trade zone.
% FOUNDING_PROBLEM_CORROBORATION: State governments and anti-consolidation advocates attest the founding problem was narrowly about interstate trade barriers and that the narrow reading is faithful to that original scope. Economic historians and several sitting federal judges outside this reading's tradition attest that the founding-era economy and the modern integrated national economy are structurally different, and that the narrow reading's fidelity to 1787 trade patterns produces regulatory gaps the founders could not have anticipated — a reading of the founding problem offered by parties who do not benefit from the narrow reading's persistence.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) rather than high because the coordination function is genuine — a bright-line jurisdictional test does reduce litigation cost and does permit real state regulatory experimentation with real benefits to federalism values. But it is not zero, because the same bright line is used strategically by incumbent industries and by states to externalize costs (pollution, wage suppression) onto parties who have no standing to contest the intrastate characterization. Suppression is lower than extraction because compliance is not coerced by direct force — it operates through judicial doctrine and the structural inability of excluded parties (Congress, diffuse victims) to alter the test through ordinary political channels, which is a real but indirect form of suppression, hence the rising suppression_requirement trajectory as the reading hardened post-1995. Theater is moderate and dipped mid-century when the reading was dormant, since a losing minority position generates rhetorical performance without much operative bite; it rises again after the reading regains doctrinal force because contemporary applications increasingly require formalistic characterization exercises (is this activity 'in commerce' or merely 'affecting commerce') to preserve the line's coherence against an integrated national economy.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and anti-consolidation advocates sit near the full-beneficiary end: the reading directly grants them the authority and doctrinal victory they seek, with essentially no cost borne. Intrastate incumbent industries are also beneficiaries but with moderate exit constraints — they must actually structure operations around the intrastate/interstate line, which is itself a real (if lesser) cost. Cross-border pollution sufferers, low-wage intrastate workers, and interstate migrant laborers sit near the full-target end: they are powerless, trapped (no exit — they cannot relocate the pollution source or bargain around wage suppression), and bear costs directly caused by the doctrinal line's placement. National market uniformity seekers are marked as a non-agent payer class (an economic interest, not an organized actor) whose costs are diffuse compliance burden rather than personal harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interstate trade wars under the Articles of Confederation — is genuinely dead as a live crisis; no state currently erects tariff walls against another's goods. What keeps this reading load-bearing is not the original problem but a live, ongoing federalism contest over which level of government should regulate an integrated national economy. Classifying this as tangled_rope rather than snare or piton is deliberate: it retains genuine coordination value (administrable line, real state regulatory autonomy, litigation cost reduction) even as it produces asymmetric extraction (externality victims with no standing). A pure piton reading would be wrong because concentrated beneficiaries (states, incumbent industries, an organized ideological movement) actively maintain and profit from the line — this is not mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    border_crossing_formalism_vs_substance,
    'Is the border-crossing/instrumentality line a principled originalist constraint tracking the founders'' actual understanding of ''commerce,'' or is it a constructed formalism whose practical effect is to shelter specific beneficiary groups (states, incumbent industries, anti-consolidation advocates) from federal regulation regardless of founding intent?',
    'Historical linguistic analysis of founding-era usage of ''commerce'' in state and federal legislative records, correspondence, and ratification debates, cross-checked against how consistently the doctrine is actually applied versus how often exceptions are carved out for politically favored activities.',
    'If the line tracks genuine founding-era meaning consistently applied, the reading is better characterized as principled interpretation with incidental distributive effects. If the line is applied inconsistently to favor specific beneficiaries, the reading is better characterized as motivated doctrine using originalism as post-hoc justification — pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_crossing_formalism_vs_substance, conceptual, 'Whether the narrow reading is principled originalism or motivated formalism serving concentrated beneficiaries.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Given that the commerce_clause_text kernel supports at least three structurally distinct readings (this narrow reading, the expansive reading, and the substantial-effects-limited reading), what determines which reading a given judicial panel or era adopts — is it genuine interpretive method, or is it downstream of the appointing political coalition''s substantive policy preferences?',
    'Empirical study of voting patterns: do judges apply this reading consistently across ideologically cross-cutting cases (e.g., striking down both liberal and conservative federal statutes equally), or does application correlate with the political valence of the regulated activity?',
    'Consistent cross-ideological application would support the reading''s claim to principled method; correlation with political valence of outcomes would support treating the choice of reading itself as an extractive lever operated by whichever coalition controls judicial appointments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether reading-selection tracks method or political outcome preference.').

omega_variable(
    externality_victim_representation,
    'Do cross-border pollution sufferers, low-wage intrastate workers, and interstate migrant laborers have any effective indirect representation in the doctrinal contest over this reading (e.g., through federal agencies, advocacy litigation, or downstream state political pressure), or are they genuinely absent from the process that maintains the reading?',
    'Survey of amicus participation, agency intervention, and legislative advocacy records in the major cases (Lopez, Morrison, and successors) to determine whether excluded-victim interests were represented by any party, even imperfectly.',
    'If effectively represented despite lacking direct standing, the excluded classification and absent_voices severity should be moderated. If genuinely unrepresented, the tangled_rope classification''s asymmetric-extraction element is more severe than the base metrics currently capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_victim_representation, empirical, 'Whether externality-bearing populations have any indirect voice in the doctrinal process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.35).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement_basis(comm_tr_t1955, observed).
narrative_ontology:measurement(comm_tr_t1975, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement_basis(comm_tr_t1975, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(comm_tr_t2010, observed).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.2).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1955, 0.22).
narrative_ontology:measurement_basis(comm_be_t1955, observed).
narrative_ontology:measurement(comm_be_t1975, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement_basis(comm_be_t1975, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement_basis(comm_be_t2010, observed).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(comm_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.15).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1955, 0.18).
narrative_ontology:measurement_basis(comm_su_t1955, observed).
narrative_ontology:measurement(comm_su_t1975, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement_basis(comm_su_t1975, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2010, 0.34).
narrative_ontology:measurement_basis(comm_su_t2010, observed).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(comm_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the commerce_clause_text kernel. expansive_federal_reading claims federal power reaches all economic activity with substantial aggregate national effects — its beneficiary/victim structure is nearly inverted relative to this story's (federal government and uniform-standard seekers benefit; states and localized industries pay). substantial_effects_limited_reading occupies a middle position requiring jurisdictional nexus and non-pretextual regulation, producing a smaller and more contested beneficiary/victim set than either pole. Each story's ε is authored independently for that reading's own operation; do not average or blend across the three when reasoning about 'the Commerce Clause' as a single constraint — that label covers three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
