% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Capital Supremacy Reading: Trade Agreement as Overriding Domestic Regulatory Authority
 *   domain: international_trade_law/regulatory_federalism/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel: the NAFTA
 *   jurisdictional boundary. The reading authored here is the CAPITAL
 *   SUPREMACY READING: trade agreement text stands as supreme law overriding
 *   domestic regulatory authority; capital mobility and regulatory
 *   harmonization are mandatory treaty obligations that supersede domestic
 *   labor and environmental standards. This reading is one stable constraint
 *   with one ε-invariance: trade obligations are interpreted as supremacy and
 *   extracted from domestic regulatory agencies, workers, and environmental
 *   constituencies. Alternative readings of the same kernel
 *   (embedded_liberalism_reading: trade as framework compatible with domestic
 *   policy space; sovereignty_primacy_reading: domestic law as supreme over
 *   trade agreements) are structurally different constraints with different ε
 *   values and different victim sets — each is authored separately and linked
 *   via network.affects_constraints. The kernel contest is real: the three
 *   readings correspond to three genuinely held institutional positions
 *   across NAFTA parties and within them over the 35-year interval. This
 *   story is NOT an attempt to describe the 'true' reading — it instantiates
 *   one and documents the structural implications of that reading's
 *   enforcement.
 *
 * KEY AGENTS:
 *   - multinational_capital: institutional beneficiary, arbitrage exit, collects extraction directly
 *   - trade_treaty_dispute_resolution_bodies: institutional agenda-setter and enforcement machinery, analytical exit, interprets constraint and renders binding decisions
 *   - domestic_labor_regulators: institutional payer, constrained/identity-locked exit, loses authority to set standards
 *   - domestic_environmental_agencies: institutional payer, identity-locked exit, mandate conflicts with authority
 *   - workers_in_harmonized_jurisdictions: powerless payer, trapped exit, absorbs wage and standard erosion
 *   - environmental_protection_constituencies: organized payer, constrained exit, loses regulatory victories to trade challenge
 *   - developing_country_governments: dual position (beneficiary/payer), moderate power, constrained exit
 *   - trade_policy_negotiators: institutional observer, analytical exit, can see divergence but politically costs to reverse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.82).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Capital Supremacy Reading: Trade Agreement as Overriding Domestic Regulatory Authority").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/regulatory_federalism/political_economy").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, 'f52291f1-b700-4402-a5da-24ca18983676').
narrative_ontology:cs_kernel_codification('f52291f1-b700-4402-a5da-24ca18983676', fixed_text).
narrative_ontology:cs_authority_grounding('f52291f1-b700-4402-a5da-24ca18983676', extraction).
narrative_ontology:cs_interpretation_layer_present('f52291f1-b700-4402-a5da-24ca18983676').
narrative_ontology:cs_reading_relation('f52291f1-b700-4402-a5da-24ca18983676', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('f52291f1-b700-4402-a5da-24ca18983676', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('f52291f1-b700-4402-a5da-24ca18983676', foundational, capital_mobility_fundamental_right).
narrative_ontology:cs_axiom_status(capital_mobility_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('f52291f1-b700-4402-a5da-24ca18983676', capital_mobility_fundamental_right, instrumental).
narrative_ontology:cs_axiom('f52291f1-b700-4402-a5da-24ca18983676', foundational, regulatory_harmonization_mandatory).
narrative_ontology:cs_axiom_status(regulatory_harmonization_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('f52291f1-b700-4402-a5da-24ca18983676', regulatory_harmonization_mandatory, conventional).
narrative_ontology:cs_reference_frame('f52291f1-b700-4402-a5da-24ca18983676', free_capital_allocation_framework).
narrative_ontology:cs_drift_state('f52291f1-b700-4402-a5da-24ca18983676', contemporary_post_labor_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f52291f1-b700-4402-a5da-24ca18983676', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_treaty_dispute_resolution_bodies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, workers_in_harmonized_jurisdictions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_protection_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, developing_country_governments).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_exporters_in_low_standard_jurisdictions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, developing_country_governments).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, capital_mobility_as_fundamental_right).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, regulatory_harmonization_as_contractual_obligation).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_dispute_settlement_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Multinational corporations structured across NAFTA jurisdictions gain the right to locate production in the lowest-cost regulatory environment without penalty; they can challenge domestic labor or environmental standards as trade barriers under investor-state dispute settlement (ISDS) mechanisms. The constraint guarantees capital mobility by subordinating domestic regulatory authority to treaty obligations. They benefit from regulatory arbitrage without corresponding exit cost.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital, beneficiary,
    institutional, generational, arbitrage, global).

% International arbitration panels and trade courts interpret the constraint and enforce it against domestic regulators. They author the reading of the treaty text and determine whether domestic standards 'unnecessarily' restrict trade. They are the enforcement machinery and the interpretive authority that sustains the capital-supremacy reading. Their power is delegated by the treaty text itself but exercises real sovereignty over domestic policy space.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_treaty_dispute_resolution_bodies, agenda_setter,
    institutional, generational, analytical, global).

% National labor departments and unions lose de facto authority to set minimum wage, working conditions, and organizing rights without risk of trade challenge. They bear the cost of regulatory weakening (labor standards erode to match lowest-cost jurisdictions) but are excluded from treaty-level negotiations on remedy. Their exit is captured: staying in the trade regime means accepting lower standards; leaving means economic isolation. The constraint forces regulatory harmonization downward.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_regulators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_regulators, excluded).

% National environmental ministries and agencies lose authority to impose standards stricter than the lowest harmonized level without triggering ISDS claims. They are institutionally committed to environmental protection but structurally bound by trade obligations; their mandate conflicts with their authority. They bear the cost of regulatory collapse in their domain. They are excluded from trade-law interpretation despite being the domain experts.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_agencies, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_agencies, excluded).

% Workers in high-wage, high-standard jurisdictions compete for manufacturing employment against workers in low-standard jurisdictions. Capital mobility forces regulatory harmonization downward — wages, benefits, and safety standards in high-standard countries erode to match lower-cost production sites. They cannot exit: moving labor across borders is restricted while capital is free; they absorb the extraction as wages compress. The constraint traps them in a race to the bottom.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, workers_in_harmonized_jurisdictions, payer,
    powerless, biographical, trapped, national).

% Environmental organizations and citizens groups in high-standard jurisdictions bear the cost of regulatory roll-back driven by capital mobility and harmonization pressure. They organized to achieve domestic environmental standards; the constraint renders those victories vulnerable to trade challenge. Their exit is costly (environmental damage is irreversible in many cases, and relocation is not an option). The constraint suppresses their power by subordinating environmental law to trade law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_protection_constituencies, payer,
    organized, generational, constrained, national).

% Developing-country governments are promised job creation and capital inflow from locating multinational production; they benefit from the constraint's guarantee of capital mobility. They simultaneously bear the cost of regulatory harmonization — they are pressured to lower standards to remain attractive. The benefit is concentrated (jobs, foreign exchange) while the cost is diffuse (environmental damage, labor standards erosion). Their structural position is dual: beneficiary from capital, payer of regulatory cost.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, developing_country_governments, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, developing_country_governments, payer).

% Domestic firms in lower-cost jurisdictions benefit from the constraint's guarantee that high-standard competitors will face pressure to harmonize downward or lose market share. They gain comparative advantage not from innovation or efficiency but from the constraint's protection of their lower regulatory burden. Their exit is constrained by trade dependence; their benefit is contingent on the constraint's persistence.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_exporters_in_low_standard_jurisdictions, beneficiary,
    moderate, biographical, constrained, national).

% Government trade negotiators authored the constraint but are now analysts of its operation. They can observe whether the capital-supremacy reading is being enforced, but reversing it would require renegotiating the treaty, which is politically costly. They sit outside the enforcement but inside the authority that could change it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_policy_negotiators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single unified regulatory space for capital investment and production allocation across NAFTA jurisdictions: multinational firms can optimize plant location without facing contradictory or shifting labor/environmental rules; harmonized standards reduce compliance cost; dispute resolution provides a predictable mechanism for resolving trade-regulatory conflicts.
% TRANSFER_FUNCTION: Moves regulatory authority from domestic labor and environmental agencies to trade-treaty interpretation bodies; moves production location decisions from domestic policy makers to multinational capital; moves labor standards, environmental standards, and working conditions from higher-standard to lower-standard levels through harmonization pressure. The direct extraction is regulatory capture and downward harmonization; the beneficiaries are multinational capital and dispute-resolution bodies; the payers are domestic regulators, workers, and environmental constituencies.
% ABSENT_VOICES: Labor unions across jurisdictions, environmental organizations, and worker representatives are excluded from trade-law interpretation and ISDS proceedings, though those proceedings directly affect their constituencies. Indigenous communities and subsistence communities whose environmental and labor rights depend on stricter standards are absent from the negotiating table. Democratic legislatures that enacted domestic labor and environmental protections are excluded from the appeal process when those protections are challenged as trade barriers.
% DISAPPEARANCE_RATIONALE: If the capital-supremacy reading disappeared and were replaced by a sovereignty-primacy reading (domestic law supreme), governments would immediately restore labor and environmental standards that had been eroded; multinational investment location decisions would shift to account for genuine regulatory diversity rather than harmonized baselines; labor and environmental constituencies would regain institutional authority they lost; trade disputes would be resolved through negotiation rather than binding arbitration. The regulatory landscape would reorganize rapidly.
% FOUNDING_PROBLEM: Post-WWII trade regime fragmented into competing regional blocs with incompatible regulatory standards; firms faced contradictory compliance requirements; capital could not flow freely to optimal production locations due to patchwork national regulations; transaction costs of operating across multiple regulatory regimes were high.
% FOUNDING_PROBLEM_CORROBORATION: Trade negotiators and multinational corporations attest the founding problem is live and ongoing — regulatory patchwork still exists and creates compliance friction. Economists in the neoliberal consensus support this account. Labor economists and environmental scientists outside the benefiting parties attest the founding problem is partially solved BUT that the remedy (capital-supremacy reading) created a larger problem — the loss of domestic regulatory authority and the race to the bottom. Legislative testimony from high-standard jurisdictions disputes both that the founding problem justified this remedy and that the remedy actually solves it; they point to evidence that standards divergence persists and that the constraint's benefit accrues narrowly to multinational capital rather than broadly to trade participants.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.54 to 0.78 over the interval because the capital-supremacy reading is incrementally enforced: early disputes are few and ambiguous; by year 35, ISDS precedent is thick and labor/environmental regulators internalize the constraints without formal challenge — the extraction rate stabilizes as compliance becomes automatic. Theater ratio rises from 0.18 to 0.41 because the coordination function (unified regulatory space) is real and valuable early; over time, the enforcement machinery increasingly defends capital mobility against legitimate regulatory resistance, and rhetoric about 'necessary harmonization' increasingly covers pure extraction. Suppression is high throughout (0.68–0.82) because the constraint's persistence depends on actively preventing domestic regulatory authority from reasserting itself — labor regulators and environmental agencies are continuously suppressed from restoring standards even when public demand exists. The coercion grid shows class-level and individual-level suppression running consistently above structural and organizational suppression: the constraint operates through diffuse pressure on workers and communities rather than overt institutional violence. One shared time grid on all three metrics ensures temporal alignment and prevents measurement artifacts from dating type transitions early.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (labor regulators, environmental agencies, workers) should compute as snare or tangled_rope depending on how visible the coordination function is to them; the beneficiary seat (multinational capital) should compute as rope or tangled_rope reflecting genuine coordination benefit. The agenda-setter seat (ISDS bodies) should compute as tangled_rope or snare depending on whether their administrative function is read as coordination or as pure extraction machinery. The engine will surface this divergence; the authored claim does not settle it.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational capital: d ≈ 0.05 (full beneficiary, arbitrage exit, directly collects extraction). Trade-dispute-resolution bodies: d ≈ 0.10 (beneficiary from institutional power-concentration, analytical exit). Domestic labor regulators: d ≈ 0.88 (full target, constrained/identity-locked exit, bear cost of regulatory harmonization downward). Domestic environmental agencies: d ≈ 0.92 (full target, identity-locked exit, mandate explicitly conflicts with constraint). Workers: d ≈ 0.95 (full target, trapped exit, no alternatives, absorb wage erosion). Environmental constituencies: d ≈ 0.82 (high target, constrained exit, victories are reversible). Developing-country governments: d ≈ 0.55 (symmetric — genuine benefit from capital inflow balanced by cost of regulatory erosion and loss of policy space). No directionality overrides are needed; the structural derivation from beneficiary/victim + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory fragmentation, transaction costs of operating across incompatible standards) WAS live and real at t0. The stated remedy is capital-supremacy interpretation: uniform regulatory baseline achieved by treaty supremacy and mandatory harmonization. At t35, the founding problem is substantially solved (harmonization exists) BUT by displacement rather than by coordination: high-standard jurisdictions have not lifted low-standard ones; instead, the constraint has achieved de facto harmonization by suppressing upward regulatory movement. The coordination function (unified legal space for capital) is genuine; the extraction is real (regulatory authority transferred, standards eroded). This is the tangled_rope signature: genuine coordination + asymmetric extraction + active enforcement. Mandatrophy is NOT present (the founding problem is not dead — fragmentation would re-emerge if the constraint were removed); instead, what is present is EXTRACTION ACCUMULATION: the coordination function is maintained at t0 levels while enforcement machinery is built up to suppress regulatory reassertion. The theater ratio rise (0.18→0.41) indicates increasing performativity — more enforcement language about 'necessary harmonization' and 'preventing protectionism' per unit of actual coordination value delivered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_harmonization_necessity,
    'Is regulatory harmonization actually necessary to solve the founding problem (transaction costs from divergent standards), or is it imposed by capital to maximize extraction opportunities?',
    'Comparative analysis of trade flows and capital allocation under embedded liberalism vs. capital supremacy readings — do capital flows increase more than can be explained by efficiency gains, suggesting extraction? Study of jurisdictions that maintained higher standards while remaining trade-open.',
    'If harmonization is incidental and capital-driven extraction is primary, the constraint is purely extractive snare, not tangled_rope; if harmonization genuinely solves a coordination problem, the tangled_rope classification holds. This determines whether the constraint''s persistence is functional or inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_harmonization_necessity, empirical, 'Whether regulatory harmonization is coordination cost or extraction mechanism.').

omega_variable(
    reading_institutional_grounding,
    'Which reading (capital supremacy, embedded liberalism, sovereignty primacy) is institutionally entrenched across NAFTA dispute-resolution bodies, and does that entrenchment reflect the text or does it reflect captured interpretation?',
    'Analysis of ISDS case law and tribunal composition: do tribunals systematically favor capital-supremacy interpretation? Do tribunal members have financial or career ties to multinational capital? What proportion of cases rule against domestic regulations vs. in favor? Comparison to alternative texts or regional agreements using embedded liberalism interpretation.',
    'If capital supremacy is entrenched through institutional capture rather than textual necessity, the constraint is a false-natural-law candidate (mountain-fraud). If the reading is textually defensible but only one among live alternatives, the constraint is a genuine kernel reading and remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_institutional_grounding, empirical, 'Whether the capital-supremacy reading is institutionally captured or textually justified.').

omega_variable(
    labor_standard_suppression_internalization,
    'To what extent is labor-standard suppression structural (external barriers: trade sanctions, capital flight, ISDS threat) vs. internalized (labor movements accept the logic of capital supremacy; regulatory agencies internalize the constraint without external enforcement)?',
    'Post-constraint removal experiment: if a jurisdiction unilaterally adopted sovereignty-primacy reading and restored labor standards, would investment rapidly flee (structural suppression) or would capital remain despite higher regulatory burden (internalization)? Study of union organizing and labor-movement framing shifts pre/post treaty ratification.',
    'If suppression is primarily internalized, the effective suppression is higher than authored (0.82) — the target carries the constraint internally and exports it; it is stickier and harder to overturn. If primarily structural, the constraint would relax if enforcement threat were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_standard_suppression_internalization, empirical, 'Structural vs. internalized labor standard suppression.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the capital-supremacy and sovereignty-primacy readings logically foreclose each other, or do they coexist as simultaneously held but opposed institutional positions?',
    'Textual analysis of NAFTA provisions: if capital supremacy is the only defensible textual reading, it forecloses the alternatives. If multiple defensible readings exist, they coexist. Political-economy analysis: do capital-supremacy proponents assert foreclosure (the text REQUIRES capital supremacy and forbids sovereignty primacy) or do they defend it as superior policy (it CAN coexist with other readings but is institutionally dominant)?',
    'If foreclosure: this reading is a hard constraint that cannot be coexist with an alternative without fundamental institutional reconfiguration. If coexistence: the constraint is contingent on institutional choice and could be superseded by explicit renegotiation. Foreclosure strengthens the constraint''s persistence; coexistence makes it more vulnerable to political change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether readings logically foreclose each other or coexist.').

omega_variable(
    multinational_capital_heterogeneity,
    'Do ALL multinational firms uniformly benefit from capital-supremacy reading, or does benefit concentrate narrowly (large firms, specific sectors), with smaller firms and domestic-focused capital bearing some costs?',
    'Sectoral analysis of trade dispute caseload: which firms sue under ISDS? Analysis of wage and employment changes by firm size and nationality pre/post treaty ratification. Do domestic firms lose market share to multinationals that can arbitrage standards?',
    'If benefit is broadly distributed across multinational capital, the beneficiary set is correct and stable. If benefit concentrates narrowly (large pharmaceutical, energy, tech firms), the constraint''s stability is more fragile because other capital may form coalitions with labor/environmental constituencies against it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multinational_capital_heterogeneity, empirical, 'Whether capital-supremacy benefit is broadly distributed or concentrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(naft_tr_t5, observed).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(naft_tr_t10, observed).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(naft_tr_t15, observed).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(naft_tr_t20, observed).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(naft_tr_t25, observed).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(naft_tr_t30, observed).
narrative_ontology:measurement(naft_tr_t35, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(naft_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(naft_be_t5, observed).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(naft_be_t10, observed).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(naft_be_t15, observed).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(naft_be_t20, observed).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(naft_be_t25, observed).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(naft_be_t30, observed).
narrative_ontology:measurement(naft_be_t35, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(naft_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(naft_su_t0, observed).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(naft_su_t5, observed).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(naft_su_t10, observed).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(naft_su_t15, observed).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(naft_su_t20, observed).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement_basis(naft_su_t25, observed).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement_basis(naft_su_t30, observed).
narrative_ontology:measurement(naft_su_t35, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 35, 0.82).
narrative_ontology:measurement_basis(naft_su_t35, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(naft_grid_01, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(naft_grid_02, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(class), 35, 0.66).
narrative_ontology:measurement(naft_grid_03, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(individual), 0, 0.42).
narrative_ontology:measurement(naft_grid_04, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(individual), 35, 0.64).
narrative_ontology:measurement(naft_grid_05, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(naft_grid_06, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(organizational), 35, 0.68).
narrative_ontology:measurement(naft_grid_07, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(naft_grid_08, nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse(structural), 35, 0.72).
narrative_ontology:measurement(naft_grid_09, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(naft_grid_10, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(class), 35, 0.78).
narrative_ontology:measurement(naft_grid_11, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(individual), 0, 0.51).
narrative_ontology:measurement(naft_grid_12, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(individual), 35, 0.58).
narrative_ontology:measurement(naft_grid_13, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(naft_grid_14, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(organizational), 35, 0.71).
narrative_ontology:measurement(naft_grid_15, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(structural), 0, 0.45).
narrative_ontology:measurement(naft_grid_16, nafta_jurisdictional_boundary__capital_supremacy_reading, resistance(structural), 35, 0.52).
narrative_ontology:measurement(naft_grid_17, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(class), 0, 0.51).
narrative_ontology:measurement(naft_grid_18, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(class), 35, 0.74).
narrative_ontology:measurement(naft_grid_19, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(naft_grid_20, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(individual), 35, 0.71).
narrative_ontology:measurement(naft_grid_21, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(naft_grid_22, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(organizational), 35, 0.81).
narrative_ontology:measurement(naft_grid_23, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(naft_grid_24, nafta_jurisdictional_boundary__capital_supremacy_reading, stakes_inflation(structural), 35, 0.79).
narrative_ontology:measurement(naft_grid_25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(class), 0, 0.74).
narrative_ontology:measurement(naft_grid_26, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(class), 35, 0.86).
narrative_ontology:measurement(naft_grid_27, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement(naft_grid_28, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(individual), 35, 0.85).
narrative_ontology:measurement(naft_grid_29, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(naft_grid_30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(organizational), 35, 0.84).
narrative_ontology:measurement(naft_grid_31, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(structural), 0, 0.61).
narrative_ontology:measurement(naft_grid_32, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression(structural), 35, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel carries three live readings: capital_supremacy (this story, highest extraction), embedded_liberalism (moderate extraction, broader benefit distribution), sovereignty_primacy (near-zero extraction, mountain or pure rope). Each reading is authored as a separate constraint story with separate ε values and victim sets. The kernel contest determines which reading is institutionally entrenched in treaty interpretation; the engine does not adjudicate the contest. This story documents extraction under the capital-supremacy reading. Sibling stories document extraction/coordination under alternative readings. All three are linked via network.affects_constraints because they are institutional competitors for the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__capital_supremacy_reading, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
