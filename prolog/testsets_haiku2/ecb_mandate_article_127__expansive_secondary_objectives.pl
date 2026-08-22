% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127 — Expansive Secondary Objectives Reading
 *   domain: economic/constitutional
 *
 * SUMMARY:
 *   Article 127 of the Treaty on the Functioning of the European Union
 *   defines the ECB's mandate as primary: maintain price stability;
 *   secondary: support the EU's economic policies, including employment. The
 *   expansive secondary objectives reading interprets the 'without prejudice'
 *   clause as authorizing the ECB's Governing Council to operationally weight
 *   employment and growth objectives whenever price stability is not under
 *   threat — a discretionary balancing rather than a strict hierarchy. This
 *   reading has animated the ECB's actual practice since the 2012 Draghi era,
 *   justifying quantitative easing, negative rates, and accommodative
 *   guidance. The rival orthodox price-stability reading contends the
 *   secondary objectives are aspirational but non-operational; the
 *   climate-incorporation reading adds environmental integration obligations.
 *   This story instantiates only the expansive secondary-objectives reading
 *   and its structural consequences for workers, debtors, savers, and
 *   member-state coordination.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: agenda setter, institutional power, interprets mandate and sets discretionary secondary-objective weight
 *   - Employed workers and indebted households: beneficiaries, powerless, gain employment support and low real debt service
 *   - Growth-dependent member states (peripheral eurozone): beneficiary, moderate power, substitute for fiscal stimulus capacity
 *   - Savers and fixed-income recipients: victims, moderate power, bear cost of low real interest rates
 *   - Price-stability-prioritizing member states (Germany, Netherlands): victims, powerful, bear inflation and credibility cost
 *   - European Court of Justice: observer, institutional power, adjudicates mandate boundaries
 *   - Orthodox inflation-targeting coalition: excluded from discretionary authority, would dispute the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.42).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127 — Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "economic/constitutional").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'a0ae78ae-919c-4b80-865d-80f3bd06aba4').
narrative_ontology:cs_kernel_codification('a0ae78ae-919c-4b80-865d-80f3bd06aba4', fixed_text).
narrative_ontology:cs_authority_grounding('a0ae78ae-919c-4b80-865d-80f3bd06aba4', lineage).
narrative_ontology:cs_interpretation_layer_present('a0ae78ae-919c-4b80-865d-80f3bd06aba4').
narrative_ontology:cs_reading_relation('a0ae78ae-919c-4b80-865d-80f3bd06aba4', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('a0ae78ae-919c-4b80-865d-80f3bd06aba4', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('a0ae78ae-919c-4b80-865d-80f3bd06aba4', foundational, secondary_objectives_operationally_weighted).
narrative_ontology:cs_axiom_status(secondary_objectives_operationally_weighted, holdable).
narrative_ontology:cs_axiom_grounding('a0ae78ae-919c-4b80-865d-80f3bd06aba4', secondary_objectives_operationally_weighted, conventional).
narrative_ontology:cs_axiom('a0ae78ae-919c-4b80-865d-80f3bd06aba4', foundational, ecb_discretionary_mandate_interpretation).
narrative_ontology:cs_axiom_status(ecb_discretionary_mandate_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a0ae78ae-919c-4b80-865d-80f3bd06aba4', ecb_discretionary_mandate_interpretation, deontological).
narrative_ontology:cs_reference_frame('a0ae78ae-919c-4b80-865d-80f3bd06aba4', secondary_objectives_operational_discretion).
narrative_ontology:cs_drift_state('a0ae78ae-919c-4b80-865d-80f3bd06aba4', post_2021_inflation_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0ae78ae-919c-4b80-865d-80f3bd06aba4', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, employed_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, growth_dependent_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, savers_fixed_income_recipients).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_prioritizing_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the treaty mandate. Under this reading, possesses discretionary authority to weight employment and growth objectives operationally when price stability is not threatened. Sets monetary policy rates, asset purchase programs, and collateral frameworks. Adjudicates what 'not threatened' means in real time, which determines the scope of secondary-objective weight.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from accommodative monetary policy that supports employment levels above the natural rate when inflation remains stable. Experience lower unemployment and tighter labor markets as the ECB operationally weights employment. Exit from the eurozone labor market is constrained by visa/citizenship and language barriers; exit from eurozone participation is individual powerlessness.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, employed_workers, beneficiary,
    powerless, biographical, trapped, continental).

% Benefit from sustained low interest rates justified by secondary employment objectives; service existing debts at lower real cost. Exit requires exiting the eurozone currency and debt denominations, which is structurally constrained (refinancing, currency conversion risk, legal lock-in of mortgage/loan contracts).
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households, beneficiary,
    powerless, biographical, constrained, continental).

% Southern and peripheral eurozone member states with high unemployment and public debt service burdens. Benefit from ECB monetary accommodation that supports growth and tax revenue without requiring politically costly domestic fiscal consolidation. Their exit option (eurozone departure) carries existential costs for currency stability and debt servicing.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, growth_dependent_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Bear the cost of sustained low or negative real interest rates justified by secondary employment objectives. Fixed-income pension savings and savings accounts lose purchasing power; real wealth transfers from savers to borrowers as the secondary-objectives reading permits accommodative rates. Exit includes eurozone asset reallocation (currency diversification, real estate, equities outside euro zone) and geographic migration.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, savers_fixed_income_recipients, payer,
    moderate, biographical, mobile, continental).

% Germany, Netherlands, northern EU periphery. Prioritize price stability and view secondary employment objectives as inflationary policy that erodes the purchasing power anchor. Pay in the form of tolerance for inflation above the 2% target and loss of monetary credibility. Exit (eurozone departure) is constrained by economic interdependence; voice (treaty amendment to subordinate secondary objectives) requires consensus of all member states, which the beneficiary coalition can block.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_prioritizing_member_states, payer,
    powerful, generational, constrained, continental).

% Interprets the treaty and adjudicates whether the ECB's secondary-objective weight stays within the mandate or exceeds it. Reviews whether 'without prejudice' permits the ECB's actual discretionary practice or forecloses it. Seat for judicial constraint on the reading's operational scope.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% Economists, central bankers, and policymakers in northern member states and ECB executive board members who hold the orthodox price-stability reading. Would argue that secondary objectives corrupt the primary mandate and that the ECB should operate under an explicit inflation-only constraint. Excluded from the discretionary balancing authority under this reading's framework (their interpretation is overridden by the Governing Council majority).
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, inflation_targeting_coalition, excluded,
    powerful, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the eurozone's political coordination problem: member states with high unemployment and debt cannot unilaterally adjust monetary policy; the ECB's discretionary application of secondary objectives coordinates accommodative policy across the union without requiring fiscal transfers or treaty amendment, preserving monetary union cohesion.
% TRANSFER_FUNCTION: Moves the benefit of accommodative monetary policy (lower unemployment, growth support, lower debt service costs) to workers, debtors, and growth-dependent member states; moves the cost (lower real savings returns, inflation above target, monetary credibility loss) to savers and price-stability-prioritizing member states.
% ABSENT_VOICES: Orthodox price-stability advocates are structurally excluded from the Governing Council's discretionary authority (their reading is overridden); inflation-targeting economists outside the ECB are advisory only; fiscal policymakers in debtor states could claim co-determination if secondary objectives implied ECB obligation to fiscal dominance (they are excluded from that reading's version of the mandate).
% DISAPPEARANCE_RATIONALE: If this reading and its discretionary secondary-objective weight vanished, the ECB would enforce strict price-stability targeting, interest rates would rise across the eurozone, unemployment would increase in peripheral member states, and debt servicing burdens would sharpen — forcing political choice between fiscal austerity, eurozone departure, or treaty amendment. Member states cannot simply restore employment and growth without monetary accommodation or fiscal space.
% FOUNDING_PROBLEM: Eurozone design flaw: monetary policy is centralized (ECB) but fiscal policy is national (member states cannot issue their own currency for fiscal adjustment). When the 2008 crisis struck and eurozone member states were unable to use independent monetary or fiscal stimulus, unemployment in peripheral states soared. The ECB's operational secondary-objectives reading emerged as a workaround: using discretionary monetary accommodation to support growth and employment when price stability constraint permitted, without requiring the political consensus for treaty amendment that would formally rebalance the mandate.
% FOUNDING_PROBLEM_CORROBORATION: The Governing Council attests the founding problem is live, citing persistent unemployment differentials across member states. Peripheral member states' finance ministers and social partners attest that secondary-objective accommodation is the only available substitute for fiscal stimulus capacity. Northern member states and the Bundesbank attest the problem is overstated and the solution is creeping mandate erosion. Independent economic analysis (IMF, OECD) documents the eurozone's incomplete union design and the ECB's de facto fiscal stabilizer role, corroborating the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the secondary-objectives reading generates real coordination benefits (employment support without fiscal transfers) but also redistributes from savers to borrowers, from northern to southern members, and from price-stability to growth priorities. This extraction grows from 2012 (0.38, when the reading emerged as emergency practice) to 2021 (0.61, peak expansionary response post-COVID) before declining slightly to 2024 (0.58, as inflation pressures forced rate increases despite secondary-objective support). Suppression is moderate (0.42) because the discretionary authority is formally within the treaty (the reading must suppress the orthodox alternative, but does not require coercive exit from the eurozone). Theater is low-to-moderate (0.28) because the employment objectives are genuinely pursued, but the constraint's persistence depends partly on keeping the orthodox reading suppressed through interpretive authority rather than falsifying its actual costs (a piton risk if secondary objectives became purely performative). Accessibility of alternatives is moderate (0.65) because ECB membership is locked in (eurozone exit is structurally constrained) but voice mechanisms and treaty amendment exist formally.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB Governing Council seat, this reading is pure coordination: the discretion it grants solves the union's design flaw by permitting employment support without fiscal transfers or treaty renegotiation. From the beneficiary seats (workers, debtors), it is genuine coordination benefit without perceivable extraction (unemployment declines, debt service eases). From the saver and price-stability seats, it is extractive redistribution and mandate creep — the secondary objectives become cover for inflation tolerance and real-wealth transfer. The schema requires us to author one reading (the expansive secondary objectives), but the measurement metrics (extractiveness, suppression, theater) describe the structural asymmetry: the reading coordinates across member states AND extracts from certain distributional groups. The engine computes per-seat classifications; this commentary explains why the Governing Council and northern members will experience different types from the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Employed workers (powerless, trapped) sit at high d (near full target) because they depend on the secondary-objectives accommodation and cannot exit; savers (moderate power, mobile) sit at moderate-to-high d because they can reallocation assets across currencies and geographies but exit from the eurozone itself is slow. Price-stability member states (powerful, constrained) sit at moderate-high d because they have formal voice but cannot block the Governing Council majority and eurozone exit is economically catastrophic. The ECB Governing Council (institutional, analytical) derives d from its beneficiary role and discretionary authority — it collects no direct rents (it is not a for-profit seat) but its power and freedom to interpret the mandate mean it is insulated from the costs it imposes, placing it near the beneficiary end (low d). The engine derives d from beneficiary/victim declarations and exit options; we have declared the structural positions honestly (workers benefit and are trapped; savers pay and have modest exit; the ECB coordinates and sets policy).
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive secondary-objectives reading sits at risk of mandatrophy: if secondary objectives were to fade from operational weight and become purely rhetorical (theater_ratio → 1.0), the constraint would degrade to a piton — the ECB would maintain the secondary-objectives language to preserve the reading's formal legitimacy while returning to price-stability-only practice. The measurement series show theater_ratio rising from 0.15 (2012, genuine secondary-objective pursuit post-crisis) to 0.31 (2021, peak accommodation), then declining to 0.28 (2024, as rate increases constrained accommodation). The peak-then-decline pattern suggests the reading's operational force is tied to the inflation regime: when inflation is below target, secondary objectives are operational and coordination is visible; when inflation threatens the target, the primary objective reasserts and secondary objectives fade into rhetoric. This is the mandatrophy trajectory — the founding problem (eurozone design flaw) remains live, but the reading's solution (discretionary secondary-objective weight) becomes intermittently performative rather than consistently operational. An omega variable addresses this under-determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_objectives_operational_weight,
    'Does the ''without prejudice'' clause authorize operational discretionary weight on secondary objectives, or is it merely a non-prejudicial statement that secondary objectives may be pursued if price stability is not threatened?',
    'European Court of Justice textual and purposive interpretation of the treaty; evidence of the negotiating intent at Maastricht and Amsterdam treaty revisions; comparative analysis of how the phrase is used elsewhere in EU law.',
    'If ''without prejudice'' grants discretion, the expansive secondary-objectives reading holds; if it is merely permissive (secondary objectives may be pursued but cannot shape policy choices), the orthodox price-stability reading holds. This determines whether the ECB has authority to trade off primary and secondary objectives or must treat secondary objectives as constraints to optimize within, not variables to operationally weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_objectives_operational_weight, conceptual, 'The legal meaning of ''without prejudice to'' in the treaty text.').

omega_variable(
    price_stability_not_threatened_boundary,
    'What level of actual or expected inflation constitutes ''price stability not threatened'' for purposes of secondary-objective discretion? Is it below the 2% target? Within a tolerance band (say, 1.5–2.5%)? A forward-looking expectation rather than realized inflation?',
    'ECB Governing Council practice over successive policy cycles; contrast with actual rate-setting decisions in periods of rising and falling inflation; explicit guideline statements or policy rules from the ECB.',
    'A narrow interpretation (inflation below 2% exactly) restricts secondary-objective weight; a broad interpretation (inflation expected to stay below 2% in the medium term despite current elevation) expands discretion. The boundary is where the reading''s operational scope is determined in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_stability_not_threatened_boundary, empirical, 'The operational definition of ''not threatened'' that the ECB actually uses.').

omega_variable(
    mandatrophy_path_inflation_regime_dependence,
    'Is the secondary-objectives reading''s operational force permanently dependent on an inflation regime below the target, such that sustained inflation above target dissolves the reading into a piton (secondary objectives remain formal but operationally subordinate)?',
    'Post-2021 inflation dynamics and ECB rate-path decisions. If inflation remains elevated and the ECB maintains accommodative secondary-objective weight despite breaching the 2% target, the reading survives the inflation test; if the ECB abandons secondary-objective accommodation and re-centers on price-stability targeting, the reading will have degraded to rhetoric.',
    'If the reading is inflation-regime-dependent, its structural status is contingent rather than durable; it becomes a scaffold-like constraint that emerges under specific conditions (low inflation) rather than a stable coordinate of the mandate. This affects whether the reading should be classified as rope (stable coordination) or something more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_path_inflation_regime_dependence, empirical, 'Whether secondary-objective discretion survives an inflationary environment.').

omega_variable(
    eurozone_design_flaw_permanence,
    'Is the ECB''s secondary-objectives accommodation a permanent structural feature of eurozone governance, or a temporary patch that will eventually be formalized (via fiscal union, eurozone treasury, or treaty amendment) or abandoned (via orthodox price-stability re-entrenchment)?',
    'Trajectory of eurozone institutional reform; evidence of political will to formalize fiscal integration or to mandate price-stability primacy via treaty; alternative institutional developments (e.g., European Fiscal Board gaining operational authority, national fiscal buffers replacing ECB accommodation).',
    'If temporary patch, the reading''s mandate is scaffolding — it solves a real problem (design flaw) but is meant to be replaced by proper institutional design. If permanent, it is a durable coordinate and the reading is a true rope. This affects classification and the interpretation of mandatrophy signals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eurozone_design_flaw_permanence, preference, 'Whether secondary-objective accommodation is a permanent eurozone feature or a contingent institutional patch.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 2012, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2012, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(ecb__tr_t2018, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2021, 0.31).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2012, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(ecb__be_t2018, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2018, 0.54).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2012, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(ecb__su_t2018, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2021, 0.46).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, resource_allocation).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.18).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% Article 127 of the Treaty on the Functioning of the European Union is a contested kernel instantiated in three constraint stories: orthodox_price_stability (strict hierarchy of primary and secondary objectives; secondary objectives are non-operational); expansive_secondary_objectives (this story; discretionary weight on secondary when primary not threatened); climate_incorporation (environmental integration as a treaty obligation). The three readings share a common kernel text but diverge in how they interpret its legal meaning, the scope of ECB discretion, and the beneficiary/victim structure. Each story is independent and ε-invariant; the network links document the kernel family. The expansive reading influences both siblings: it expands what constitutes permissible ECB authority (affects orthodox, which claims the expansive reading overreaches) and constrains where climate incorporation can be added (affects climate, which must negotiate within the secondary-objectives framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
