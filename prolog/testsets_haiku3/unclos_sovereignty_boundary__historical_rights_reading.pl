% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Usage and Occupation Override UNCLOS EEZ Boundaries
 *   domain: international law / maritime governance
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested UNCLOS
 *   sovereignty boundary kernel. The reading asserts that historical usage
 *   and occupation create sovereign rights that predate and override UNCLOS
 *   Exclusive Economic Zone (EEZ) provisions. Under this reading, a state
 *   with centuries of maritime navigation, resource extraction, or occupation
 *   in a zone can invoke those historical rights to contest a neighboring
 *   coastal state's exclusive control under UNCLOS Article 57
 *   (200-nautical-mile EEZ boundary). This reading benefits expansive
 *   claimant states (particularly major naval powers and states asserting
 *   historic claims in Southeast Asia and the Arctic) by allowing them to
 *   override bright-line EEZ boundaries. It imposes costs on UNCLOS-dependent
 *   coastal states who expected exclusive zone control and on non-claimant
 *   maritime actors navigating contested waters. The constraint is CLAIMED as
 *   tangled rope because it coordinates (historical rights as a legitimate
 *   organizing principle) while extracting (beneficiary states capture zone
 *   access; victim states lose exclusive control); the metrics reflect the
 *   measurement of that mixed operation and its enforcement burden.
 *
 * KEY AGENTS:
 *   - Expansive claimant states (China, Russia, major naval powers): assert historical rights; benefit from boundary contestation; push the reading forward through naval operations and diplomatic claims.
 *   - Strict EEZ coastal states (especially Global South): rely on UNCLOS Article 57; lose exclusive zone control when historical rights are asserted; bear enforcement uncertainty.
 *   - Non-ratifier states (United States): outside formal UNCLOS obligation; benefit from freedom to invoke customary law and historical rights as needed; arbitrate the reading's acceptability through naval presence.
 *   - Smaller coastal nations: trapped; depend on UNCLOS bright-line as only defense; lack naval power to enforce against historical-rights claims.
 *   - International adjudicators (ITLOS, ICJ, arbitration tribunals): set standards for what 'counts' as valid historical rights; their rulings shape the reading's scope.
 *   - Maritime commerce and fishing interests: pay through operational uncertainty and lost market exclusivity when zones become contested.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Usage and Occupation Override UNCLOS EEZ Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international law / maritime governance").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'ab7f5f72-e2ee-496b-b778-6341c9649533').
narrative_ontology:cs_kernel_codification('ab7f5f72-e2ee-496b-b778-6341c9649533', fixed_text).
narrative_ontology:cs_authority_grounding('ab7f5f72-e2ee-496b-b778-6341c9649533', extraction).
narrative_ontology:cs_interpretation_layer_present('ab7f5f72-e2ee-496b-b778-6341c9649533').
narrative_ontology:cs_reading_relation('ab7f5f72-e2ee-496b-b778-6341c9649533', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('ab7f5f72-e2ee-496b-b778-6341c9649533', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('ab7f5f72-e2ee-496b-b778-6341c9649533', foundational, historical_rights_persist_across_regime_change).
narrative_ontology:cs_axiom_status(historical_rights_persist_across_regime_change, holdable).
narrative_ontology:cs_axiom_grounding('ab7f5f72-e2ee-496b-b778-6341c9649533', historical_rights_persist_across_regime_change, deontological).
narrative_ontology:cs_axiom('ab7f5f72-e2ee-496b-b778-6341c9649533', foundational, customary_law_continuity_unbroken_by_unclos).
narrative_ontology:cs_axiom_status(customary_law_continuity_unbroken_by_unclos, holdable).
narrative_ontology:cs_axiom_grounding('ab7f5f72-e2ee-496b-b778-6341c9649533', customary_law_continuity_unbroken_by_unclos, empirically_contingent).
narrative_ontology:cs_reference_frame('ab7f5f72-e2ee-496b-b778-6341c9649533', pre_unclos_customary_maritime_law).
narrative_ontology:cs_drift_state('ab7f5f72-e2ee-496b-b778-6341c9649533', post_unclos_bright_line_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab7f5f72-e2ee-496b-b778-6341c9649533', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, historical_occupancy_defenders).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_freedom_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, navigational_freedom_advocates).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, smaller_coastal_nations).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, fisheries_and_resource_extractors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with historical navigation rights, resource extraction, or occupation claims in waters now claimed as EEZ by other coastal states under UNCLOS. They assert that customary international law predating UNCLOS recognizes their historical rights and that these rights override the 200-nautical-mile EEZ boundary. They benefit from the ability to assert overlapping claims and resist the erosion of their historical position to a coastal state's exclusive zone. Examples: major naval powers conducting freedom of navigation operations, states with centuries-old trading routes or fishing rights, China's historic nine-dash-line claim in Southeast Asia.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, beneficiary,
    powerful, generational, mobile, global).

% Coastal states that ratified UNCLOS and rely on Article 57's 200-nautical-mile EEZ as their exclusive economic and resource control boundary. They lose the ability to enforce their EEZ monopoly when expansive claimants invoke historical rights to operate within the zone without permission. They bear the cost in lost resource control, diminished enforcement jurisdiction, and political subordination of their legal claims to the historical rights narrative. Their exit consists of accepting the overlap (constrained) or challenging it through international dispute resolution (constrained by the costs and unpredictability of litigation).
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_coastal_states, payer,
    powerful, generational, constrained, global).

% International maritime commerce, naval operations of non-claimant states, and freedom-of-navigation coalitions face a constraint that legitimizes restrictive interpretations of the high seas. When historical rights are invoked to override EEZ boundaries, the result is a patchwork of overlapping claims and unpredictable enforcement zones. They pay the cost in navigational uncertainty and restricted passage; they benefit theoretically from the precedent that historical rights can override formal boundaries (if they can establish their own historical navigation claims), but most lack the power to do so.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, navigational_freedom_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, navigational_freedom_advocates, beneficiary).

% Nations with limited naval enforcement capacity and smaller economic stakes, who depend on UNCLOS's bright-line EEZ boundary as their primary defense against more powerful states' historical rights claims. When powerful neighbors invoke historical rights, these smaller nations lose the ability to protect their exclusive economic zone. They are trapped because their exit options (naval enforcement, challenging great-power claims through litigation) are economically and militarily infeasible. Examples: Southeast Asian nations facing China's historical claims, island nations facing overlapping great-power historical rights assertions.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, smaller_coastal_nations, payer,
    moderate, biographical, trapped, regional).

% States that did not ratify UNCLOS (notably the United States, which remains the world's largest naval power) can invoke customary international law and historical rights without UNCLOS obligations constraining them. They benefit from the reading that historical rights override UNCLOS because it preserves their freedom to assert customary-law claims regardless of UNCLOS text. They have the highest exit optionality: they can claim UNCLOS rules where convenient and invoke historical/customary law where UNCLOS constrains them.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_states, beneficiary,
    institutional, generational, arbitrage, global).

% Commercial fishing fleets and resource companies licensed by coastal states to operate exclusively in EEZs. When historical rights claims override EEZ boundaries, their exclusive licenses become contested and enforcement becomes uncertain. They pay through lost market exclusivity and operational uncertainty; their exit is limited because alternative fishing/extraction zones are either equally contested or in deeper decline.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, fisheries_and_resource_extractors, payer,
    moderate, biographical, constrained, regional).

% International courts, dispute resolution bodies, and legal scholarship that must rule on whether a historical claim is valid. They set the terms for which historical rights 'count' (occupation how long, usage by whom, continuous or interrupted, etc.). Their agenda-setting power is substantial but constrained by the need to maintain legitimacy across competing parties. They benefit from the continuation of the dispute (their role exists because the boundary is contested); they carry costs when rendering one verdict that invalidates the other reading.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, historical_rights_adjudicators, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically-established maritime access and resource-extraction rights; recognition of long-standing patterns of navigation, fishing, and occupation that predate formal boundary-drawing. The reading solves the coordination problem by anchoring rights to observable historical facts rather than to treaty text alone.
% TRANSFER_FUNCTION: Moves enforcement jurisdiction and exclusive resource control from UNCLOS-ratifying coastal states (who expect to control their 200-nautical-mile zone) to claimant states with historical usage or occupation patterns, and to their associated naval and commercial operators. The constraint transfers legitimacy from treaty obligations to historical fact.
% ABSENT_VOICES: Coastal states of the Global South with weak naval enforcement capacity and limited historical great-power engagement in their waters are largely excluded from articulating whether historical rights should override their UNCLOS protections. They would argue that historical rights claims typically benefit powerful states and harm vulnerable ones, but they lack the power to enforce that position. Non-ratifier states' refusal to attend UNCLOS creates an institutional asymmetry: they can invoke the reading without having formally bound themselves to UNCLOS constraints.
% DISAPPEARANCE_RATIONALE: If the constraint (historical rights override UNCLOS EEZ) disappeared, coastal states' exclusive economic zones would be unambiguously enforced, expansion of great-power maritime access into neighboring zones would require explicit consent or dispute resolution, and maritime commerce routing would stabilize around bright-line boundaries. The geopolitical map of maritime control would shift substantially; multiple ongoing regional disputes (South China Sea, Arctic, Eastern Mediterranean) would move from contestation toward either settlement or formal militarization under clearer legal rules.
% FOUNDING_PROBLEM: Before UNCLOS (pre-1982), maritime rights were governed by customary international law and historical occupation/usage. Powerful maritime states had centuries of established navigation and resource-extraction patterns. UNCLOS attempted to formalize maritime boundaries with bright-line EEZ limits, but it did not explicitly cancel pre-existing historical rights. The founding problem is how to allocate maritime space when a new formal regime (UNCLOS EEZ) overlays older customary claims.
% FOUNDING_PROBLEM_CORROBORATION: Expansive claimant states (China, Russia, and major naval powers) assert the founding problem persists: historical rights predate and override UNCLOS. UNCLOS-dependent coastal states (particularly Global South nations) assert the founding problem is SOLVED by UNCLOS: the bright-line boundary was the solution, and historical-rights overlays undermine it. International law scholarship is divided; some scholars emphasize customary international law continuity (supporting the reading), others emphasize UNCLOS as a replacement regime (opposing it). No consensus exists outside the benefiting parties; arbitration verdicts (e.g., 2016 South China Sea ruling against the nine-dash-line) have sided with the strict EEZ reading, but they are not universally accepted as binding on non-parties.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end because the reading transfers exclusive control from UNCLOS-reliant coastal states to claimant states with historical arguments. This is high-extractiveness coordination because a real problem (how to recognize pre-UNCLOS rights) is solved by giving claimant states veto power over EEZ enforcement. Suppression is 0.72 because the reading persists through active naval enforcement operations (freedom-of-navigation transits, resource extraction presence, diplomatic assertion) and because rival readings are kept marginal through institutional power imbalance. Theater is moderate (0.41) because the reading is not purely performative — historical-rights claims do change material maritime access — but a growing share of enforcement activity is defensive theater (proving the right through repeated transit/occupation) rather than solving the coordination problem itself. Accessibility collapse is 0.62: alternatives (strict EEZ boundaries, or customary-law-only without historical overlay) are partially but not completely closed off; they remain live in legal scholarship and in the institutional resistance of coastal states. Resistance is 0.71: substantial; the reading meets active resistance from UNCLOS-dependent states, smaller coastal nations with no historical-rights counterclaims, and maritime commerce coalitions seeking predictability. Measurement trajectory shows increasing extractiveness and suppression over the interval (time 0 to 40) as great-power naval presence in contested zones intensifies and adjudicatory precedents (arbitrations, ICJ cases) fail to settle the reading's scope. Theater is rising (0.28 → 0.41) because enforcement increasingly consists of repeated transits and operations designed to establish or assert precedent, characteristic of constraints whose underlying coordination problem is solved but whose distribution of benefits is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the expansive claimant seat (e.g., China, Russia), the reading is genuine coordination: historical rights are a legitimate principle, and invoking them solves the problem of how pre-UNCLOS claims coexist with post-UNCLOS boundaries. From the strict EEZ coastal seat (Vietnam, Philippines, Indonesia), the same constraint is pure extraction: a great power using historical claims as a cover for geopolitical expansion. From the non-ratifier seat (US), the reading is a tool: useful when asserting freedom of navigation (invoking customary law against others' EEZ claims) and useful when defending allies' EEZ boundaries (the 2016 arbitration verdict against China's nine-dash line was couched in strict UNCLOS reading). The engine computes these divergences from the structural data (power, exit, beneficiary/victim assignments); the authored claim does not reconcile them. The perspectival gap is the measurement this story exists to make.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states sit at beneficiary end of directionality (d ≈ 0.1–0.2): the constraint transfers exclusive control to them; they have institutional and naval power to assert the reading; their exit is mobile (they can claim historical rights or invoke UNCLOS as needed). Strict EEZ coastal states sit at target end (d ≈ 0.8–0.9): the constraint strips exclusive zone control; smaller nations have trapped exit (they cannot enforce against great powers); even powerful coastal states like Vietnam or the Philippines face constrained exit (litigation is long and uncertain). Non-ratifier states (US) occupy an asymmetric middle (d ≈ 0.3–0.4): they benefit from freedom to invoke the reading but also benefit from strict EEZ where it prevents other great powers' expansion. Smaller coastal nations have the highest effective extraction because they lack the power to assert counterclaims and depend entirely on UNCLOS protection. Navigational-freedom advocates face increased constraint (d ≈ 0.6): passage becomes uncertain; they cannot invoke historical rights (they lack the power) but are not named beneficiaries of the coordination either.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified tangled rope, not snare, because a real coordination function exists: pre-UNCLOS maritime rights do pose a genuine problem for any formal boundary regime, and historical occupation/usage is a defensible principle for allocating rights. However, the constraint is near the snare boundary because enforcement is heavily asymmetric (great powers assert and defend historical rights; weaker states cannot) and because the beneficiary set is concentrated (major naval powers collect the access). Mandatrophy is not triggered because the constraint's founding problem (recognizing historical rights alongside new formal boundaries) remains live and contested; the reading is not a zombie arrangement. However, the rising theater ratio and the concentration of beneficiaries are red flags: as enforcement activity shifts from 'solving maritime coordination' to 'proving the claim through repeated operations,' the constraint risks drifting toward snare classification. The omega variables document the ambiguity in how 'historical rights' are defined and who can claim them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_definition,
    'What constitutes valid ''historical rights'' for purposes of overriding EEZ boundaries? How long must usage persist, how continuous must it be, and who is authorized to claim it?',
    'International court or tribunal ruling that establishes objective criteria for historical-rights claims (e.g., continuous occupation for X years, specific documented usage patterns, uncontested exercise of rights). Arbitration precedents (South China Sea, Arctic disputes) will shape this incrementally.',
    'A narrow definition (continuous formal occupation for 200+ years) would severely restrict which states can claim historical rights and would shift the constraint back toward strict EEZ reading. A broad definition (any documented historical usage, including fishing by nationals) would expand beneficiaries substantially and entrench the historical_rights_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_definition, empirical, 'The empirical and legal boundary between valid and invalid historical-rights claims.').

omega_variable(
    customary_law_continuity_vs_unclos_displacement,
    'Did UNCLOS formally displace pre-existing customary international law regarding maritime rights, or did UNCLOS merely codify customary law while leaving pre-existing claims intact?',
    'Comparative analysis of UNCLOS travaux préparatoires (negotiation records), scholarly consensus on law-of-treaties interpretation, and adjudicatory rulings on whether UNCLOS is a complete replacement regime or a partial codification overlaid on customary law.',
    'If UNCLOS displaced customary law, the strict_eez_reading wins and historical rights are superseded. If UNCLOS merely codified part of customary law without displacing pre-existing claims, the historical_rights_reading persists and overlapping claims remain legitimate. This is a foundational conceptual question that cannot be fully resolved by evidence alone; it depends on law-of-treaties principles that themselves are contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_continuity_vs_unclos_displacement, conceptual, 'Whether UNCLOS replaced or merely modified customary maritime law.').

omega_variable(
    beneficiary_power_asymmetry,
    'To what extent does the reading''s persistence depend on the military and naval power of claimant states (particularly China, Russia, and the US) rather than on the legitimacy of the historical-rights principle itself?',
    'Counterfactual analysis: if a weak state asserted the same historical-rights claim, would it receive institutional recognition and enforcement, or would only great-power claims be treated as valid? Observational data: comparing how historical claims from major powers are treated versus claims from smaller states in adjudication.',
    'If the reading''s persistence depends primarily on beneficiary power rather than principle, it is structurally snare-adjacent and vulnerable to reclassification if power distributions shift. If the principle is genuinely accepted as legitimate across power levels, the tangled_rope classification holds and the constraint has durability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_power_asymmetry, empirical, 'The degree to which the reading relies on beneficiary-state power for its persistence versus normative legitimacy.').

omega_variable(
    non_ratifier_advantage_structural,
    'Is the non-ratifier advantage (ability to invoke customary law without UNCLOS constraint) a feature of the reading, or does it rest on the non-ratifier''s power to resist adjudication?',
    'Test case: if a non-ratifier state were sued in an international tribunal for violating another state''s rights, and the tribunal ruled against the non-ratifier''s customary-law argument, would the non-ratifier accept the ruling? Observational data: US behavior in UNCLOS-related disputes and whether the US accepts tribunal authority.',
    'If the non-ratifier advantage depends on power to resist adjudication (not on legal principle), then the reading is structurally contingent on great-power privilege. If the advantage rests on valid legal theory (customary law truly is independent source of obligation), then non-ratifier behavior is a normal consequence of law, not extraction. This affects whether the constraint should be classified as snare or tangled rope at the non-ratifier seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_advantage_structural, empirical, 'Whether the non-ratifier advantage is a legitimate legal principle or a product of power asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(uncl_tr_t8, observed).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(uncl_tr_t16, observed).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(uncl_tr_t24, observed).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t32, observed).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(uncl_be_t8, observed).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(uncl_be_t16, observed).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(uncl_be_t24, observed).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(uncl_be_t32, observed).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(uncl_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(uncl_su_t8, observed).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(uncl_su_t16, observed).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(uncl_su_t24, observed).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(uncl_su_t32, observed).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(uncl_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__historical_rights_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% The unclos_sovereignty_boundary kernel decomposes into three structurally distinct constraints: (1) strict_eez_reading: EEZ boundaries are exclusive and override historical claims; (2) historical_rights_reading (this one): historical usage overrides EEZ; (3) non_ratifier_enforcement_reading: customary law and naval presence are enforceable independent of UNCLOS ratification. Each reading has different beneficiary sets, victim sets, and extraction profiles. They are linked by the kernel ambiguity: UNCLOS text does not explicitly forbid historical-rights overlays or non-ratifier assertion of customary law. Beneficiary power and institutional backing (not empirical facts about history) determine which reading is dominant in any given dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
