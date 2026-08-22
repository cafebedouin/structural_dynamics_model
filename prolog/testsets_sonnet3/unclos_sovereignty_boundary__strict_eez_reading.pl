% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: Strict UNCLOS Article 57 EEZ Exclusivity Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the strict-textualist reading of the UNCLOS
 *   sovereignty kernel: EEZ rights extend exactly 200 nautical miles from an
 *   undisputed baseline per Article 57, and no overlay claim — historical,
 *   customary, or occupation-based — can override the treaty measurement.
 *   Under this reading, coastal states with strong baseline positions and the
 *   tribunal system that administers the Convention gain a durable,
 *   litigable, exclusive resource right; states and communities whose claims
 *   rest on history or presence rather than distance lose standing entirely
 *   wherever the arcs conflict. The reading is coherent and internally
 *   consistent, but it is one of three live readings of the same underlying
 *   kernel — the historical-rights reading and the non-ratifier
 *   customary-navigation reading are separate constraints with their own
 *   beneficiary/victim structures and their own epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.71).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "Strict UNCLOS Article 57 EEZ Exclusivity Reading").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '091ea78f-c089-4e4f-9c44-5c8689a6c0f1').
narrative_ontology:cs_kernel_codification('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', formalized).
narrative_ontology:cs_authority_grounding('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', lineage).
narrative_ontology:cs_interpretation_layer_present('091ea78f-c089-4e4f-9c44-5c8689a6c0f1').
narrative_ontology:cs_reading_relation('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', foundational, treaty_text_exclusively_dispositive).
narrative_ontology:cs_axiom_status(treaty_text_exclusively_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', treaty_text_exclusively_dispositive, conventional).
narrative_ontology:cs_axiom('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', secondary, distance_measurement_supersedes_occupation_history).
narrative_ontology:cs_axiom_status(distance_measurement_supersedes_occupation_history, holdable).
narrative_ontology:cs_axiom_grounding('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', distance_measurement_supersedes_occupation_history, conventional).
narrative_ontology:cs_reference_frame('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', unclos_1982_baseline_regime).
narrative_ontology:cs_drift_state('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', contemporary_south_china_sea_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('091ea78f-c089-4e4f-9c44-5c8689a6c0f1', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_undisputed_baselines).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, unclos_secretariat_and_tribunal_system).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, large_fishing_and_energy_concessionaires).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_historical_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, small_island_neighbor_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, artisanal_fishers_in_contested_waters).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, landlocked_and_geographically_disadvantaged_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draw a 200nm EEZ from an uncontested baseline and obtain exclusive rights to fisheries, hydrocarbons, and seabed minerals within it, backed by UNCLOS's dispute-resolution machinery. They invoke Article 57 to reject overlay claims from neighbors and non-ratifiers alike, and they staff the delegations that interpret and enforce the boundary in tribunals and at sea.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_undisputed_baselines, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_undisputed_baselines, agenda_setter).

% Administers the Convention, the Commission on the Limits of the Continental Shelf, and the arbitral tribunals that adjudicate boundary disputes strictly by the treaty text. Its authority depends on treating the 200nm rule as the sole valid metric; entertaining historical-rights or non-ratifier customary claims as co-equal would undercut the institution's reason to exist.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_secretariat_and_tribunal_system, agenda_setter,
    institutional, civilizational, analytical, global).

% Obtain licenses from coastal states to extract fish stocks and hydrocarbons inside clearly bounded EEZs, which gives them legal certainty and insurable title they could not get in contested waters. They can relocate capital to whichever coastal state offers favorable concession terms.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, large_fishing_and_energy_concessionaires, beneficiary,
    organized, biographical, mobile, global).

% Assert sovereignty over waters and features based on centuries of usage, historic maps, or prior occupation that predate UNCLOS's 1982 baseline regime. Under the strict reading their claims are simply void wherever they overlap a rival's 200nm arc, regardless of historical depth; their only paths are litigation they are likely to lose on the text, or unilateral enforcement that draws sanctions and naval confrontation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_historical_claimants, payer,
    powerful, generational, constrained, regional).

% Have EEZs that mathematically overlap with larger neighbors' 200nm arcs from disputed or newly-built features. The strict reading resolves overlaps by treaty-text priority rules that frequently favor whichever state's baseline claim survives tribunal review, leaving smaller states with reduced fishing and seabed access and no realistic capacity to enforce a contrary claim militarily.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, small_island_neighbor_states, payer,
    moderate, biographical, trapped, regional).

% Fish grounds their communities have used for generations, now inside a rival state's exclusive zone under the strict boundary reading. They have no standing before UNCLOS tribunals, no capital to relocate operations, and face coast-guard seizure of vessels and catch if they continue fishing traditional grounds that fall on the wrong side of the line.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, artisanal_fishers_in_contested_waters, payer,
    powerless, immediate, trapped, local).

% Have no coastline to draw an EEZ from at all, or a coastline too short or enclosed to generate a meaningful zone. UNCLOS grants them limited negotiated access to neighboring surplus resources, but the strict exclusivity reading means their bargaining position depends entirely on the goodwill of coastal neighbors who have no obligation to share exclusive rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, landlocked_and_geographically_disadvantaged_states, excluded,
    powerless, generational, trapped, regional).

% Patrol and assert freedom-of-navigation rights through EEZs, generally accepting the exclusivity of resource rights while contesting attempts to treat the EEZ as territorial water for transit purposes. Their conduct partly determines whether the strict reading holds in practice or is quietly eroded by tolerated overlay claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_naval_powers, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, international_naval_powers, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually fixed method (distance from baseline) for allocating exclusive rights to fisheries, hydrocarbons, and seabed resources among coastal states, avoiding a maritime commons where every state's navy would need to enforce its own competing claim by force.
% TRANSFER_FUNCTION: Moves resource access and enforcement legitimacy from claimants whose title rests on history, occupation, or customary practice to claimants whose title rests on measurable distance from an undisputed baseline — concentrating exclusive rights in states with strong baseline positions and treaty-literate legal teams, and away from historically-rooted or geographically disadvantaged claimants.
% ABSENT_VOICES: Overlapping historical claimants and artisanal fishing communities whose usage predates the 1982 baseline regime are not parties to tribunal proceedings in any meaningful sense — states litigate on their behalf or not at all; landlocked states have no coastline to generate standing under the very framework being applied to exclude them.
% DISAPPEARANCE_RATIONALE: If the strict 200nm exclusivity rule vanished overnight, boundary disputes would revert to negotiated or contested claims based on history, occupation, and naval presence; concession licenses issued under EEZ certainty would lose their legal foundation, and states currently excluded by the arc-based measurement would immediately reassert historical or equitable claims to the same waters.
% FOUNDING_PROBLEM: Pre-UNCLOS ocean law left resource rights and boundary claims to unilateral proclamation, historic usage assertions, and naval enforcement, producing unpredictable and frequently violent disputes over fisheries and, later, offshore hydrocarbons.
% FOUNDING_PROBLEM_CORROBORATION: Coastal states and the UNCLOS tribunal system attest the founding problem remains live and the 200nm rule is the settled, functioning solution. Overlapping historical claimants, several regional legal scholars outside any state's foreign ministry, and non-ratifying naval powers attest that the strict reading has become a tool coastal states use to foreclose legitimate historical and customary claims rather than merely to prevent unregulated resource grabs — the underlying coordination problem persists, but the strict textual solution has hardened into an instrument that itself generates new disputes it cannot resolve on its own terms.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects that the strict reading transfers real, monetizable resource access away from claimants whose title rests on history or occupation, concentrated where EEZ arcs overlap. Suppression (0.71) is high because the reading's coherence depends on treating any competing sovereignty framework — historical usage, customary navigation, occupation — as categorically inadmissible before the tribunal system, not merely as weaker evidence. Theater ratio (0.28) is moderate-low: the coordination function (predictable resource allocation, reduced naval friction) is real and substantial, but a growing share of tribunal and diplomatic activity is spent defending the boundary framework itself against contestation rather than adjudicating novel allocation questions.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states with clean, undisputed baselines and the institutions that administer UNCLOS sit at the beneficiary end: they collect exclusive resource rights and the legitimacy of an entire adjudicatory apparatus built to enforce exactly their claim. Overlapping historical claimants, small island states boxed in by a larger neighbor's arc, and artisanal fishers with no tribunal standing sit at the target end — trapped exit options, generations of usage voided by a distance calculation. Large concessionaires benefit derivatively from the certainty the boundary creates, and can relocate capital if a given jurisdiction's title becomes contested, unlike the fishers who cannot relocate their grounds.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unregulated, force-backed unilateral ocean claims — was genuinely solved by a distance-based allocation rule; that coordination function has not disappeared. But the mandatrophy signal here is that the same rule, having solved the general problem, is now used to foreclose a subset of claims (historical, occupation-based) that the treaty regime never actually adjudicated on the merits — it simply declared them void by omission. The tangled_rope reading captures this: real coordination (avoiding a naval free-for-all) persists alongside a genuine extraction from historically-rooted claimants whose access is lost not because their claim was examined and rejected, but because the metric chosen to resolve the general problem structurally cannot register their kind of claim at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_versus_customary_primacy,
    'Does a ratified treaty''s explicit distance formula categorically supersede customary international law claims (historical usage, prior occupation) that predate the treaty, or do the two sources of law coexist as parallel, weighable claims?',
    'Track how international tribunals actually rule when historical-usage evidence is presented in EEZ boundary disputes: consistent textual-priority rulings would corroborate this reading; a body of rulings that weighs historical evidence against the baseline arc would undermine the strict reading''s claimed exclusivity.',
    'If tribunals routinely admit historical evidence as a genuine counterweight rather than dismissing it as inadmissible, the strict reading''s suppression score is understated and its classification moves further toward extraction with a thinner coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_versus_customary_primacy, conceptual, 'Whether treaty text categorically forecloses customary/historical claims or merely creates a rebuttable presumption.').

omega_variable(
    non_ratifier_binding_status,
    'Is the strict 200nm exclusivity rule binding on states that never ratified UNCLOS, given that some UNCLOS provisions are argued to reflect customary international law while others (arguably including the specific exclusivity mechanics) are treaty-created?',
    'State practice and opinio juris analysis: do non-ratifying naval powers'' actual patrol and enforcement patterns treat the 200nm exclusivity rule as binding customary law, or only the narrower freedom-of-navigation principle?',
    'If non-ratifiers'' practice shows they accept freedom-of-navigation as customary but not resource exclusivity as customary, this reading''s claim to universal (not merely treaty-party) applicability is overstated, and the constraint''s effective scope should be narrowed to ratifying states only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_binding_status, empirical, 'Whether the strict exclusivity mechanic binds non-ratifiers as customary law or only treaty parties.').

omega_variable(
    coordination_extraction_separability_eez,
    'Is the exclusivity mechanic (categorical exclusion of overlay claims) structurally necessary to achieve the coordination benefit (predictable, low-conflict resource allocation), or could the same coordination benefit be achieved by a framework that weighs historical claims within the distance-based system rather than voiding them?',
    'Comparative analysis of maritime boundary regimes that DO incorporate historical-usage weighting (e.g., some bilateral delimitation agreements) versus pure-distance regimes, measured for dispute frequency and resolution stability.',
    'If hybrid regimes achieve comparable stability, the categorical exclusion is not required by the coordination function and the extraction from historical claimants is closer to pure surplus than to a necessary cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability_eez, conceptual, 'Whether the strict reading''s total exclusion of historical claims is coordination-necessary or extractive surplus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.1).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unclos_sovereignty_boundary kernel. strict_eez_reading treats Article 57's distance formula as exclusive and dispositive (this file). historical_rights_reading treats historical usage/occupation as capable of overriding the distance formula. non_ratifier_enforcement_reading treats freedom-of-navigation as customary law independent of treaty ratification, enforced by naval presence rather than tribunal adjudication. Each reading has its own epsilon and its own beneficiary/victim structure — the strict reading's beneficiaries (clean-baseline coastal states, the tribunal apparatus) are frequently the historical-rights reading's targets, and vice versa. Do not average across the three; each is a distinct constraint linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
