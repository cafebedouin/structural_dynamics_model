% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Strict UNCLOS Article 57 EEZ Boundary Reading (200nm Exclusive, No Overlay Claims)
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   This story instantiates the strict textual reading of the UNCLOS
 *   sovereignty kernel: Article 57's 200-nautical-mile exclusive economic
 *   zone is treated as the exhaustive and self-sufficient basis for maritime
 *   resource allocation, with no overlay claim — historical usage, prior
 *   occupation, or customary-law navigation rights asserted independent of
 *   ratification — recognized as legally cognizable against it. As a
 *   bright-line coordination rule it genuinely replaced ad hoc unilateral
 *   claims and naval confrontation with a computable geometry every ratifying
 *   state can apply. But the same exclusivity that solved the coordination
 *   problem also became the exclusive vehicle for allocating resources,
 *   producing identifiable winners (long-coastline states, the treaty
 *   administrative apparatus, distant-water fleets seeking clean bilateral
 *   counterparties) and identifiable losers (overlapping claimants
 *   disadvantaged by baseline geometry, historically-present communities with
 *   no standing, landlocked states structurally excluded from the allocation
 *   entirely). The extraction is not incidental to the coordination function
 *   — it rides on the same textual exclusivity that makes the rule
 *   administrable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.71).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "Strict UNCLOS Article 57 EEZ Boundary Reading (200nm Exclusive, No Overlay Claims)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9').
narrative_ontology:cs_kernel_codification('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', formalized).
narrative_ontology:cs_authority_grounding('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', lineage).
narrative_ontology:cs_interpretation_layer_present('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9').
narrative_ontology:cs_reading_relation('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', foundational, treaty_text_exhausts_sovereignty_claims).
narrative_ontology:cs_axiom_status(treaty_text_exhausts_sovereignty_claims, holdable).
narrative_ontology:cs_axiom_grounding('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', treaty_text_exhausts_sovereignty_claims, conventional).
narrative_ontology:cs_axiom('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', secondary, bright_line_geometry_preferred_over_equitable_balancing).
narrative_ontology:cs_axiom_status(bright_line_geometry_preferred_over_equitable_balancing, holdable).
narrative_ontology:cs_axiom_grounding('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', bright_line_geometry_preferred_over_equitable_balancing, instrumental).
narrative_ontology:cs_reference_frame('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', treaty_textual_supremacy).
narrative_ontology:cs_drift_state('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', post_south_china_sea_arbitration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c1227a1-8fbe-4ec9-a6d4-93cbbe35ebe9', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_clean_200nm_zones).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, unclos_treaty_secretariat).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations_via_bilateral_access_deals).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, indigenous_maritime_communities_with_historical_usage).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, landlocked_and_geographically_disadvantaged_states).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, unclos_textual_supremacy_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, bright_line_boundary_efficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold uncontested 200nm zones under the strict reading, gaining exclusive fishing, seabed mining, and energy rights with no competing claim to litigate against. They invoke Article 57's bright-line rule whenever a neighbor raises a historical or equity-based counterclaim, and they staff the diplomatic and legal apparatus that defends the textual reading in tribunals.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_clean_200nm_zones, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_clean_200nm_zones, agenda_setter).

% Administers the treaty framework and depends on the 200nm rule's clean administrability to keep the arbitration system tractable. A world of contested overlay claims (historical rights, non-ratifier customary law) would multiply disputes beyond what ITLOS and annex VII tribunals can process, so the secretariat's institutional relevance is bound to the strict reading holding.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_treaty_secretariat, agenda_setter,
    institutional, civilizational, analytical, global).

% Cannot fish inside a coastal state's EEZ without permission, but the strict reading's clarity lets them negotiate clean bilateral access agreements with a single recognized sovereign rather than navigating multiple overlapping claimants. They benefit from the boundary's exclusivity because it tells them exactly whom to pay.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations_via_bilateral_access_deals, beneficiary,
    powerful, biographical, mobile, global).

% Assert EEZ claims that overlap a neighbor's under the strict 200nm measurement rule (e.g. opposite or adjacent coasts less than 400nm apart, or disputed baseline islands). The strict reading forces a binary resolution — median line, tribunal award, or unresolved friction — foreclosing claims grounded in historical presence, prior administration, or equity. Their exit is constrained to litigation within the same framework that produced the unfavorable geometry.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states, payer,
    moderate, generational, constrained, regional).

% Have fished, navigated, and occupied waters for generations under customary practice that predates UNCLOS by centuries, but the strict reading recognizes only state-held 200nm zones measured from baselines, not community usage. Their historical claim has no standing in the treaty text and no seat at the state-to-state tribunals that adjudicate boundaries; if their state's EEZ line is drawn against them, they simply lose the water.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, indigenous_maritime_communities_with_historical_usage, excluded,
    powerless, civilizational, trapped, local).

% Have no coastline (or a short, enclosed one) from which to project a 200nm zone, so the strict reading's uniform rule structurally allocates most maritime resources to states with long coastlines regardless of population or need. Article 62 and 69 give them theoretical rights to negotiate surplus-catch access, but this depends entirely on the coastal state's discretion.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, landlocked_and_geographically_disadvantaged_states, payer,
    powerless, generational, trapped, regional).

% Apply the strict textual reading when adjudicating boundary disputes (e.g. the South China Sea arbitration), producing rulings that reinforce the 200nm rule's exclusivity even where historical or customary claims are extensive, because the treaty text — not equitable balancing of historical use — is their controlling instrument.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, regional_courts_and_arbitral_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_clean_200nm_zones).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, geometrically computable rule (distance from declared baselines) for allocating exclusive maritime resource rights among coastal states, replacing the pre-1982 patchwork of unilateral claims, gunboat enforcement, and unresolved historical assertions with a rule every ratifying state can calculate and every tribunal can apply without adjudicating centuries of contested history.
% TRANSFER_FUNCTION: Moves exclusive fishing, seabed mineral, and energy-exploration rights to whichever state's baseline geometry produces the larger or cleaner 200nm projection, transferring resource access away from historically-present communities, overlapping neighbors on the losing side of median-line calculations, and states with no coastline at all.
% ABSENT_VOICES: Indigenous and local maritime communities whose historical usage predates the treaty have no standing before the state-to-state tribunals that draw the lines; landlocked states get a nominal surplus-access provision but no seat in the boundary-drawing process itself. Both would argue the 200nm rule replaces lived and historical relationships to the sea with an abstraction that happens to favor whoever has the longest unbroken coastline.
% DISAPPEARANCE_RATIONALE: If the strict 200nm exclusivity rule vanished overnight, dozens of pending boundary disputes (South China Sea, Eastern Mediterranean, Gulf of Guinea) would revert to unresolved unilateral claims, historical-usage arguments would regain practical force, distant-water fishing fleets would lose their clean bilateral-access counterparties, and naval powers would likely fill the resulting ambiguity with presence-based enforcement — the entire post-1982 maritime resource allocation regime depends on this rule holding.
% FOUNDING_PROBLEM: Pre-UNCLOS, coastal states unilaterally extended territorial claims (3nm, 12nm, 200nm fishing zones asserted ad hoc) leading to escalating 'cod wars' and naval confrontations over fishing and resource access, with no shared measure of where one state's rights ended and international waters began.
% FOUNDING_PROBLEM_CORROBORATION: Ratifying coastal states and the treaty secretariat attest the founding problem (unilateral, conflict-prone claims) remains solved by the bright-line rule. Independent maritime law scholars and non-ratifying naval powers (notably the United States, which observes the EEZ regime as customary law while rejecting the treaty's dispute-resolution and seabed provisions) attest that the rule's clean administrability has become a vehicle for excluding equitable historical claims rather than merely preventing conflict — corroboration exists on both sides of the status question, which is why it is authored contested rather than resolved in either direction.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58) and suppression (0.71) diverge deliberately: suppression is high because the strict reading's entire operating logic is the exclusion of alternative sovereignty frameworks (historical rights, non-ratifier customary enforcement) — that exclusion is the mechanism, not a side effect. Extraction is moderate-high rather than extreme because many coastal states genuinely need the coordination function and are not primarily extracting from anyone; the extraction is concentrated on the specific victim groups named, not diffused across all parties equally. Theater ratio stays low (0.22) because tribunal enforcement is functionally real, not performative — ITLOS rulings materially reallocate resource access. The suppression_requirement series rises across the interval as more states ratify, as the arbitration apparatus matures, and as the 2016 South China Sea arbitration demonstrates the rule's willingness to override extensive historical-usage claims outright — enforcement infrastructure hardened rather than merely persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the coastal-state agenda-setter seat, the 200nm rule reads as elegant, necessary coordination that ended decades of naval brinkmanship. From the overlapping-claimant or indigenous-community seat, the identical rule reads as an arbitrary geometric formula that happened to be adopted by the international community that most benefits from it, foreclosing older and arguably more legitimate claims to the same water. The engine should compute divergent per-seat classifications from the same structural facts — this divergence is exactly what the tangled_rope claim predicts and the mountain-reading (favored by beneficiary states) would obscure.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states with unambiguous 200nm geometry sit near the beneficiary end: the rule subsidizes them by converting geographic accident into exclusive legal entitlement with no adjudication cost. The treaty secretariat is institutionally beneficiary-aligned because its relevance depends on the rule's administrability holding. Overlapping claimants and landlocked states sit near the target end: the same rule that clarifies boundaries for the fortunate forecloses equitable claims for the disadvantaged, and their exit options are constrained to litigating within the framework that produced their disadvantage. Indigenous maritime communities are the most extreme target case — trapped exit, powerless, and structurally absent from the state-to-state adjudication process entirely, so their directionality is not merely high-d but functionally outside the bargaining frame altogether.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unilateral claims causing naval conflict) is genuinely partially live — boundary disputes still generate military tension (South China Sea, Eastern Mediterranean) — which is why founding_problem_status is authored contested rather than dead. This prevents mislabeling the constraint as pure extraction: the coordination function has not vanished, so a snare classification would be too strong. But the concentration of benefit on long-coastline incumbents and the treaty apparatus, combined with the categorical exclusion of historical and customary claims, means classification as pure rope would launder the extraction. Tangled rope holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_exclusivity_vs_customary_accretion,
    'Does Article 57''s textual silence on historical and customary claims mean those claims are legally void, or does customary international law continue to operate alongside the treaty text regardless of the treaty''s own exclusivity claim?',
    'Track whether international tribunals (ICJ, ITLOS, annex VII panels) ever grant weight to historical-usage evidence in boundary disputes despite Article 57''s text, versus uniformly excluding it. A consistent exclusionary pattern across multiple tribunals over decades would support the strict reading''s self-sufficiency claim; any granted weight to historical evidence would undermine it.',
    'If tribunals never actually apply pure textual exclusivity in practice, the strict reading as authored here is a normative claim about how the rule SHOULD work rather than a description of how it DOES work, which would lower confidence in the high suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_exclusivity_vs_customary_accretion, conceptual, 'Whether the strict reading is doctrinally pure or only aspirationally so in actual tribunal practice.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the coastal-state administrative apparatus (baseline declaration, tribunal enforcement, diplomatic defense of the 200nm line) the correct locus of analysis, or is the deeper kernel the treaty''s own claim to supersede pre-existing customary and historical sovereignty frameworks entirely?',
    'Compare classification outcomes under both framings: (a) treating UNCLOS ratification and tribunal enforcement as the operative constraint (as authored here) versus (b) treating the prior, unwritten claim that a multilateral treaty text can retroactively extinguish standing historical claims as the deeper kernel. Framing (b) would likely surface higher suppression and a starker beneficiary/victim asymmetry since it exposes the foundational supersession claim rather than its administrative machinery.',
    'Framing (a) — chosen here — produces a tangled_rope classification because real coordination benefit (ending naval conflict) is visible alongside extraction. Framing (b) would likely classify closer to snare, since the deeper claim (treaty text extinguishes prior sovereignty) has no coordination function of its own — it is purely a supersession claim. This story adopts framing (a) because it matches the SCOPE manifest''s stated hypothesis and keeps the story''s ε tied to observable administrative and enforcement behavior rather than to an unmeasurable jurisprudential abstraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framing of the kernel''s locus (administrative apparatus vs. underlying supersession claim) would change the computed type.').

omega_variable(
    coastal_state_beneficiary_naturalness,
    'Is the 200nm rule''s benefit distribution to long-coastline states a natural consequence of geography (unavoidable given any distance-based rule) or a constructed choice among alternative allocation principles (population-based, historical-use-based, equidistant-regardless-of-coastline-length) that happened to be adopted by states already possessing long coastlines?',
    'Review the UNCLOS III negotiating history (1973-1982) for evidence of alternative allocation proposals that were rejected, and by whom. If landlocked and geographically disadvantaged states proposed population-based or equity-based alternatives that were voted down by coastal-state blocs, that supports the constructed-choice reading.',
    'If constructed, the beneficiary concentration on long-coastline states is not incidental to a neutral geometric rule but a designed outcome of the negotiating process, strengthening the tangled_rope reading. If the distance-based principle was the only administratively tractable option available at the time, the beneficiary concentration is closer to an unavoidable side effect of solving the coordination problem at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coastal_state_beneficiary_naturalness, empirical, 'Whether coastal-state benefit concentration is geographically inevitable or a negotiated design choice among rejected alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1994, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1994, 0.13).
narrative_ontology:measurement(uncl_tr_t2006, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2006, 0.16).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement(uncl_tr_t2018, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.34).
narrative_ontology:measurement(uncl_be_t1994, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1994, 0.41).
narrative_ontology:measurement(uncl_be_t2006, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2006, 0.49).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2012, 0.53).
narrative_ontology:measurement(uncl_be_t2018, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(uncl_su_t1994, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1994, 0.53).
narrative_ontology:measurement(uncl_su_t2006, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2012, 0.65).
narrative_ontology:measurement(uncl_su_t2018, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unclos_sovereignty_boundary kernel. strict_eez_reading (this file) treats Article 57's 200nm rule as exclusive and self-sufficient. historical_rights_reading treats historical usage and occupation as creating sovereign rights that predate and override UNCLOS, directly contradicting this reading's 'no overlay claims valid' premise — the two cannot coexist within a single adjudicating framework, hence forecloses in cs_structure.reading_relations. non_ratifier_enforcement_reading treats freedom-of-navigation as customary law independent of ratification, enforceable by naval presence — this creates downstream pressure on the strict reading's legitimacy (by asserting an enforcement basis outside the treaty regime) without directly foreclosing its resource-allocation logic, hence influences. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here for contamination-propagation analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
