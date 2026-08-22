% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary â Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint is the embedded_liberalism_reading of the
 *   nafta_jurisdictional_boundary kernel: the trade agreement text as a
 *   framework that balances market access against legitimate domestic policy
 *   space. Under this reading, environmental and labor standards are
 *   compatible with trade obligations when non-discriminatory, regulatory
 *   agencies retain defensive authority within a 'legitimate objectives'
 *   boundary, and extraction is moderateâoperating primarily through
 *   litigation costs and regulatory chill rather than outright preemption.
 *   The constraint coordinates genuine cross-border commerce while
 *   asymmetrically extracting regulatory autonomy from domestic institutions.
 *
 * KEY AGENTS:
 *   - state_parties (agenda_setter/institutional/constrained): Negotiated and maintain the treaty framework; can amend or withdraw but face prohibitive diplomatic and economic costs.
 *   - treaty_dispute_panels (agenda_setter/institutional/constrained): Interpret and enforce the jurisdictional boundary through case law.
 *   - multinational_exporters (beneficiary/powerful/arbitrage): Gain market access and dispute leverage.
 *   - cross_border_investors (beneficiary/powerful/arbitrage): Benefit from investment protections and ISDS.
 *   - domestic_regulatory_agencies (payer/institutional/constrained): Bear litigation risk and regulatory chill within legitimate objectives boundaries.
 *   - affected_domestic_publics (payer/powerless/trapped): Experience weakened or delayed standards due to trade-compatibility pressure.
 *   - public_interest_ngos (excluded/moderate/constrained): Absent from formal dispute settlement despite representing labor and environmental interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.6).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.68).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary â Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'd9523f85-916b-46b1-bbd9-883628e6ae33').
narrative_ontology:cs_kernel_codification('d9523f85-916b-46b1-bbd9-883628e6ae33', formalized).
narrative_ontology:cs_authority_grounding('d9523f85-916b-46b1-bbd9-883628e6ae33', lineage).
narrative_ontology:cs_interpretation_layer_present('d9523f85-916b-46b1-bbd9-883628e6ae33').
narrative_ontology:cs_reading_relation('d9523f85-916b-46b1-bbd9-883628e6ae33', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9523f85-916b-46b1-bbd9-883628e6ae33', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('d9523f85-916b-46b1-bbd9-883628e6ae33', foundational, legitimate_objectives_defensive_authority).
narrative_ontology:cs_axiom_status(legitimate_objectives_defensive_authority, holdable).
narrative_ontology:cs_axiom_grounding('d9523f85-916b-46b1-bbd9-883628e6ae33', legitimate_objectives_defensive_authority, conventional).
narrative_ontology:cs_axiom('d9523f85-916b-46b1-bbd9-883628e6ae33', foundational, non_discrimination_compatibility).
narrative_ontology:cs_axiom_status(non_discrimination_compatibility, holdable).
narrative_ontology:cs_axiom_grounding('d9523f85-916b-46b1-bbd9-883628e6ae33', non_discrimination_compatibility, conventional).
narrative_ontology:cs_reference_frame('d9523f85-916b-46b1-bbd9-883628e6ae33', embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('d9523f85-916b-46b1-bbd9-883628e6ae33', post_neoliberal_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9523f85-916b-46b1-bbd9-883628e6ae33', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_exporters).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, cross_border_investors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, affected_domestic_publics).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_liberalization_with_regulatory_preservation).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and maintain the treaty framework; can amend provisions or withdraw, but face high diplomatic and economic costs for doing so; act as both defendants and complainants in dispute settlement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, state_parties, agenda_setter,
    institutional, generational, constrained, global).

% Adjudicate jurisdictional boundaries between trade obligations and domestic regulatory space through case-by-case interpretation of legitimate objectives, necessity tests, and proportionality analysis.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, treaty_dispute_panels, agenda_setter,
    institutional, generational, constrained, global).

% Gain preferential market access and can invoke state-state or investor-state dispute settlement when foreign regulations affect their operations; benefit from harmonized rules that reduce transaction costs across borders.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_exporters, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit from investment protections and the ability to challenge domestic regulations through dispute mechanisms; enjoy reduced regulatory variance across signatory jurisdictions.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, cross_border_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Must design environmental and labor regulations within the legitimate objectives boundary to avoid litigation costs and adverse dispute rulings; experience regulatory chill and defensive drafting to preempt challenges.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Bear the downstream costs when regulatory agencies delay or weaken standards due to trade-compatibility concerns and litigation risk; cannot exit the domestic regulatory framework.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, affected_domestic_publics, payer,
    powerless, biographical, trapped, local).

% Advocate for labor and environmental standards but are structurally excluded from formal dispute settlement proceedings and treaty amendment processes; their objections appear in amicus briefs that panels may disregard.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, public_interest_ngos, excluded,
    moderate, biographical, constrained, national).

narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes reciprocal market access commitments across signatory states while preserving bounded domestic regulatory autonomy for legitimate public policy objectives such as health, safety, and environmental protection.
% TRANSFER_FUNCTION: Moves litigation risk and regulatory compliance burden from international exporters to domestic regulatory agencies; moves market access benefits and dispute-settlement leverage to cross-border capital.
% ABSENT_VOICES: Public interest NGOs and affected domestic publics are formally excluded from investor-state and state-state dispute settlement proceedings; sovereignty_primacy advocates are structurally marginalized in treaty text interpretation.
% DISAPPEARANCE_RATIONALE: If the treaty framework disappeared, tariff and non-tariff barriers would proliferate, market access predictability would collapse, and domestic regulatory agencies would regain unilateral standard-setting authority without necessity testing or litigation exposure.
% FOUNDING_PROBLEM: Post-war fragmentation of international trade, protectionist spirals, and the need to liberalize cross-border commerce without dismantling the domestic regulatory and welfare state.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and embedded-liberalism scholars corroborate the founding problem from outside the beneficiary set; investor-state disputants and neoliberal trade economists contest that the current arrangement still serves that original balance.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60 at interval end) because the constraint genuinely coordinates market access but asymmetrically extracts through litigation costs and regulatory chill. Suppression is moderate-high (0.68): the threat of dispute settlement and sanctions suppresses protectionist alternatives and discourages ambitious domestic regulation. Theater ratio (0.42) captures the growing gap between the textual promise of 'legitimate objectives' and actual dispute practice that narrows defensive authority. Accessibility collapse (0.55) reflects that pure sovereignty and pure capital supremacy are structurally excluded from the treaty architecture, though they persist in political discourse. Resistance (0.55) is moderate, coming from regulatory agencies and civil society defending policy space. The measurement series run on one shared time grid to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (domestic regulatory agencies and affected publics) experience the constraint as encroachment on democratic regulatory autonomy, while the beneficiary seats (multinational exporters and cross-border investors) experience it as predictable market access coordination. State parties occupy an intermediate position: they authored the framework and can invoke it, but they also defend against disputes and bear political costs of withdrawal. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational exporters and cross-border investors are declared beneficiaries with arbitrage-grade exit options, placing them at the low-d end of the spectrum. Domestic regulatory agencies and affected domestic publics are declared victims with constrained or trapped exit, placing them at the high-d end. State parties and treaty dispute panels are neither beneficiaries nor victims in the base properties; their directionality reverts to the canonical fallback for institutional actors with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpost-war trade fragmentation and the need to preserve the domestic welfare stateâis contested. Capital supremacy readings argue the mandate has atrophied into capital-serving extraction, while sovereignty primacy readings argue the coordination was never necessary. The embedded liberalism reading holds that the mandate remains live but is under pressure: the constraint still coordinates market access, yet dispute practice has drifted toward narrower readings of regulatory autonomy. The metrics (rising extractiveness and theater ratio over the interval) model that drift without prejudging whether the mandate is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'How would the classification change if this constraint were instantiated under the capital_supremacy_reading or sovereignty_primacy_reading instead of the embedded_liberalism_reading?',
    'Comparative reading analysis across the kernel''s constraint family; examine how each reading assigns beneficiaries, victims, and extractiveness to the same treaty text.',
    'Capital supremacy would increase extractiveness and reduce regulatory agency defensive authority, likely classifying as snare or heavily extractive tangled rope; sovereignty primacy would reduce extractiveness and coordination function, potentially classifying as rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Sibling reading structural delta for this kernel instance.').

omega_variable(
    legitimate_objectives_boundary_ambiguity,
    'Does the ''legitimate objectives'' boundary in trade agreement text genuinely preserve domestic policy space, or does it theatricalize regulatory autonomy while extraction operates through litigation cost and chill?',
    'Track the rate of successful domestic regulatory defenses in dispute settlement and the volume of pre-emptive regulatory withdrawal or weakening.',
    'If defensive success is high and chill is low, the embedded liberalism reading is structurally accurate; if chill is high despite formal defenses, the constraint is more extractive than its textual framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_ambiguity, empirical, 'Whether the legitimate objectives boundary is functional or theatrical.').

omega_variable(
    regulatory_chill_mechanism,
    'Is the suppression of domestic regulatory alternatives structural (treaty-enforced) or internalized (agencies self-censoring to avoid litigation)?',
    'Compare regulatory output in treaty-bound versus non-bound jurisdictions controlling for other factors; interview regulatory staff about litigation-risk impact on standard-setting.',
    'Internalized chill would indicate higher effective suppression than structural measures suggest, pushing classification toward snare; structural suppression alone fits tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_mechanism, empirical, 'Structural versus internalized suppression of regulatory alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(naft_tr_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(naft_tr_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(naft_tr_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(naft_be_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(naft_be_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(naft_be_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(naft_su_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(naft_su_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(naft_su_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(naft_su_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel decomposes into three structurally distinct readings: embedded_liberalism (this file), capital_supremacy, and sovereignty_primacy. Each reading assigns a different epsilon to the same treaty text depending on how it frames the domestic regulatory boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
