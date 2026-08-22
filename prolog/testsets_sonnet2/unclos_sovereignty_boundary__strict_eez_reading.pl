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
 *   human_readable: UNCLOS Article 57 Strict EEZ Boundary Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint is the strict-textualist reading of the UNCLOS Article 57
 *   EEZ boundary kernel: the 200-nautical-mile limit is exclusive,
 *   enforceable, and forecloses overlapping sovereignty claims not grounded
 *   in the treaty's own baseline geometry. It is one of three readings of a
 *   single contested kernel (unclos_sovereignty_boundary). This reading is
 *   authored on its own terms — it does not describe or average over the
 *   historical-rights reading (which grounds sovereignty in occupation/usage
 *   predating 1982) or the non-ratifier enforcement reading (which grounds
 *   freedom-of-navigation claims in customary law independent of treaty
 *   ratification). Each of those is a separate constraint with its own ε,
 *   beneficiary/victim structure, and classification, linked here via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Article 57 Strict EEZ Boundary Reading").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e').
narrative_ontology:cs_kernel_codification('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', fixed_text).
narrative_ontology:cs_authority_grounding('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', lineage).
narrative_ontology:cs_interpretation_layer_present('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e').
narrative_ontology:cs_reading_relation('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', foundational, treaty_ratification_is_exclusive_sovereignty_source).
narrative_ontology:cs_axiom_status(treaty_ratification_is_exclusive_sovereignty_source, holdable).
narrative_ontology:cs_axiom_grounding('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', treaty_ratification_is_exclusive_sovereignty_source, conventional).
narrative_ontology:cs_axiom('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', foundational, geometric_bright_line_supersedes_prior_occupation_title).
narrative_ontology:cs_axiom_status(geometric_bright_line_supersedes_prior_occupation_title, holdable).
narrative_ontology:cs_axiom_grounding('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', geometric_bright_line_supersedes_prior_occupation_title, conventional).
narrative_ontology:cs_reference_frame('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', unclos_1982_treaty_ratification_baseline).
narrative_ontology:cs_drift_state('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', contemporary_south_china_sea_arbitration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1641b6dd-bbb2-41aa-95eb-46ddcd80ee1e', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_ratified_claims).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, eez_resource_licensees).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_historical_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, artisanal_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, landlocked_and_geographically_disadvantaged_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive rights to fish stocks, hydrocarbons, and seabed minerals within 200nm of its baseline under Article 57, and can license, tax, and exclude foreign vessels within that zone. Uses UNCLOS tribunal rulings and naval patrols to enforce exclusivity against overlapping claimants. Can invoke the treaty text as a bright-line defense against competing historical or occupation-based claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_ratified_claims, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_ratified_claims, agenda_setter).

% Multinational fishing fleets and energy companies purchase exclusive extraction licenses from the coastal state, relying on the EEZ boundary being treated as legally settled and enforceable. Their commercial certainty depends entirely on the exclusivity holding against rival claimants.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, eez_resource_licensees, beneficiary,
    powerful, biographical, mobile, regional).

% States or communities whose fishing grounds, navigation routes, or occupation predate the 200nm rule find their traditional claims voided by the strict reading regardless of centuries of use. Their only recourse is litigation before a forum (ITLOS, arbitral tribunals) that itself applies the strict Article 57 text, foreclosing the argument before it is heard.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_historical_claimants, payer,
    moderate, generational, trapped, regional).

% Small-scale fishers who worked waters now allocated by treaty-line cartography to a neighboring state's exclusive zone lose access overnight when the boundary is drawn and enforced, with no compensation mechanism and no standing to contest the line themselves — only their state can litigate on their behalf, and often does not.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, artisanal_fishing_communities, payer,
    powerless, biographical, trapped, local).

% States with no coastline, or a short/enclosed one, receive no EEZ of comparable value and depend on Part X access arrangements that coastal states are not obligated to grant generously. The strict reading locks in a distributional outcome determined entirely by accident of geography.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, landlocked_and_geographically_disadvantaged_states, payer,
    powerless, civilizational, trapped, regional).

% Adjudicates boundary disputes strictly by reference to ratified treaty text and baseline geometry, treating historical usage and non-ratifier customary claims as legally subordinate or irrelevant. Its rulings are the enforcement mechanism that makes the strict reading operative rather than aspirational.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_tribunals_and_itlos, agenda_setter,
    institutional, generational, analytical, global).

% States that have not ratified UNCLOS are not parties to this reading's legal framework at all; they operate under a separate customary-law claim (freedom of navigation) that this reading treats as either compatible innocent passage or an unauthorized incursion depending on transit character, but their own legal argument is not adjudicated within this constraint.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifying_naval_powers, excluded,
    powerful, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, geometrically determinate rule (200nm from baseline) that lets coastal states, resource companies, and neighboring states know in advance which waters are whose, avoiding case-by-case negotiation over every stretch of ocean and enabling predictable investment in fisheries and offshore energy.
% TRANSFER_FUNCTION: Moves exclusive control over fish stocks, seabed minerals, and hydrocarbon rights from whoever previously used or claimed those waters under customary, historical, or occupation-based title to whichever state's baseline geometry places the water within 200nm — regardless of who used it first or longest.
% ABSENT_VOICES: Overlapping historical claimants and artisanal fishing communities would argue that centuries of use should carry legal weight, but they have no standing to appear before UNCLOS tribunals except through their state, and non-ratifying naval powers who reject treaty supremacy entirely are not represented in this framework's adjudicative forums at all.
% DISAPPEARANCE_RATIONALE: If the strict 200nm bright-line rule vanished, resource licensing built on it would become legally uncertain overnight, dozens of active boundary disputes (South China Sea, Eastern Mediterranean, Arctic shelf claims) would reopen to competing legal theories, and coastal states would lose the enforceable exclusivity that currently underwrites billions in extraction revenue.
% FOUNDING_PROBLEM: Pre-UNCLOS ocean governance left vast expanses of resource-rich water under contested or no clear jurisdiction, producing recurring naval standoffs (Cod Wars, competing continental shelf grabs) and no stable basis for investment in offshore resources.
% FOUNDING_PROBLEM_CORROBORATION: Coastal states and resource licensees attest the bright-line rule remains necessary to prevent renewed jurisdictional conflict. Independent maritime law scholars and representatives of landlocked/geographically disadvantaged states — outside the beneficiary set — attest that the founding problem of jurisdictional chaos has been solved but the strict reading now functions primarily to lock in a geography-determined distributional outcome rather than to prevent conflict, citing continued disputes in the South China Sea and Eastern Mediterranean as evidence the rule displaces rather than resolves contestation.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the bright-line rule performs genuine coordination (predictable resource jurisdiction, reduced naval standoffs) while simultaneously transferring exclusive resource control to whichever state's baseline geometry happens to capture the water — a transfer with no relationship to prior use, need, or equity. Suppression (0.72) is high and structural: the rule is enforced by naval patrol, coast guard interdiction, and tribunal precedent that treats historical-usage and non-ratifier arguments as categorically subordinate, not merely weaker. Theater ratio (0.28) is moderate-low — most enforcement activity (patrols, licensing, tribunal rulings) performs real gatekeeping function, though a growing share of tribunal activity increasingly defends the bright-line itself as doctrine rather than adjudicating genuine boundary ambiguity. All three metrics are authored on one shared time grid from 1982 (UNCLOS opening for signature) to 2024.
 *
 * PERSPECTIVAL GAP:
 *   From the coastal-state seat, this is a rope: a genuine, hard-won coordination solution to decades of jurisdictional chaos (Cod Wars, unilateral shelf grabs). From the overlapping-claimant and artisanal-fisher seats, the identical rule is tangled-rope-trending-snare: real coordination benefit exists, but it operates through a geometric transfer with no equity correction and a suppression apparatus (naval interdiction, tribunal foreclosure) that requires active maintenance. The engine's per-seat computation is expected to diverge sharply here — that divergence is the intended structural signal for a bright-line rule that reallocates value by fixed geometry.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states with ratified claims and their resource licensees sit near the beneficiary end: they collect exclusive extraction rights and licensing revenue and can invoke treaty text defensively. Overlapping historical claimants, artisanal fishing communities, and landlocked/geographically-disadvantaged states sit near the target end: their prior use, proximity, or need carries no legal weight against the geometric rule, and their exit options are trapped (a fishing community cannot relocate its traditional grounds; a landlocked state cannot acquire a coastline). UNCLOS tribunals are the enforcement/agenda-setting seat — they do not benefit materially but their interpretive discretion is what makes the boundary operative rather than aspirational.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (jurisdictional chaos over ocean resources) is contested as live vs. dead: coastal states argue continued disputes prove the rule is still needed; independent scholars and disadvantaged-state representatives argue the underlying chaos problem was solved decades ago and what remains is a distributional lock-in dressed as conflict-prevention. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (predictability, reduced naval conflict) that a pure-extraction label would erase, while the required beneficiary/victim/enforcement triad prevents the equally false move of treating the boundary as costless natural fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_supremacy_vs_customary_law_priority,
    'Does ratified treaty text (UNCLOS Article 57) categorically override prior historical usage and customary international law claims, or do those sources retain independent legal force that the strict reading wrongly forecloses?',
    'Longitudinal tracking of ITLOS/PCA arbitral outcomes where historical-rights or non-ratifier arguments are raised: if tribunals consistently subordinate them to treaty geometry, the strict reading''s foreclosure claim is empirically vindicated within the adjudicative system even if contested outside it.',
    'If customary/historical claims retain genuine independent force (as the sibling readings assert), the strict reading''s high suppression score reflects active doctrinal foreclosure of a legitimate rival framework rather than mere enforcement of settled law — sharpening its tangled_rope-to-snare drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_supremacy_vs_customary_law_priority, conceptual, 'Whether treaty supremacy genuinely displaces or merely outcompetes customary/historical sovereignty claims.').

omega_variable(
    geometric_allocation_equity_ambiguity,
    'Is the 200nm bright-line rule a neutral coordination mechanism (arbitrary but fair because uniformly applied) or a distributionally biased rule that systematically favors states with long coastlines and disadvantages landlocked/enclosed/historically-dispossessed populations?',
    'Comparative analysis of EEZ resource value distribution by coastline length and geographic configuration versus population and prior-use baselines; a strong correlation between coastline length and resource capture with no equity correction would support the bias reading.',
    'If the rule is shown to be systematically distributionally biased rather than neutral, the coordination-function claim weakens relative to the extraction-transfer claim, pushing the classification toward snare from the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geometric_allocation_equity_ambiguity, empirical, 'Whether the geometric bright-line rule is distributionally neutral or systematically biased by coastline geography.').

omega_variable(
    kernel_framing_under_determination,
    'Is the relevant kernel ''what source of law determines maritime sovereignty'' (treaty vs. custom vs. occupation), or is there a deeper kernel about ''who has standing to adjudicate ocean governance at all'' (state-centric tribunal system vs. affected-community standing)?',
    'Compare classification outcomes if the kernel were reframed around adjudicative standing rather than substantive legal source — would artisanal fishing communities and landlocked states enter as victims under either framing, or does the standing framing surface a different victim set entirely?',
    'Under the substantive-source framing (adopted here), artisanal communities are indirect victims mediated through their state''s litigation choices. Under a standing framing, their exclusion from direct adjudicative voice would itself be the primary structural finding, potentially producing a different classification (snare, on grounds of systematic voice denial) rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative kernel framing (substantive legal source vs. adjudicative standing) that could change which classification the strict reading produces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement(uncl_tr_t1994, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(uncl_tr_t2004, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2004, 0.19).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2012, 0.23).
narrative_ontology:measurement(uncl_tr_t2018, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.34).
narrative_ontology:measurement(uncl_be_t1994, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1994, 0.41).
narrative_ontology:measurement(uncl_be_t2004, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2012, 0.53).
narrative_ontology:measurement(uncl_be_t2018, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(uncl_su_t1994, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1994, 0.53).
narrative_ontology:measurement(uncl_su_t2004, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2004, 0.61).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2012, 0.66).
narrative_ontology:measurement(uncl_su_t2018, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unclos_sovereignty_boundary kernel. strict_eez_reading (this file) treats Article 57's 200nm geometric limit as treaty-supreme and exclusive. historical_rights_reading treats prior occupation/usage as an independent, potentially overriding source of sovereign title. non_ratifier_enforcement_reading treats freedom-of-navigation as customary law binding regardless of UNCLOS ratification, enforced by naval presence rather than treaty text. Each reading has a distinct beneficiary/victim structure and a distinct ε: this reading's ε (0.58) reflects transfer-via-geometry to ratified coastal states; the historical_rights reading's ε would reflect transfer away from long-settled occupants toward newer treaty-geometry claimants; the non_ratifier reading's ε would reflect the cost imposed on coastal states by external naval enforcement of a legal theory they may not accept. The three are not the same constraint measured three ways — they are three constraints sharing a contested kernel, per the ε-invariance principle (DP-001).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
