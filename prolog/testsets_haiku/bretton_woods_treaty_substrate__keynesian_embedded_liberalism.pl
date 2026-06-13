% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls Coordination (Keynesian Embedded Liberalism Reading)
 *   domain: economic/political/institutional
 *
 * SUMMARY:
 *   Bretton Woods (1944) is a treaty creating a system of fixed exchange
 *   rates and capital controls to protect nations' ability to pursue
 *   full-employment policy and social welfare expansion without triggering
 *   capital flight. Under the Keynesian embedded-liberalism reading
 *   instantiated here, the treaty's core achievement is a CONSTRAINT ON
 *   INTERNATIONAL CAPITAL—one that benefits national governments and labor
 *   movements by removing capital's veto over macroeconomic policy.
 *   International finance capital (banks, investors, corporations) bears the
 *   cost of immobilized capital and foreclosed arbitrage. The constraint is
 *   enforced through Article VI (capital-control authorization) and the IMF's
 *   conditional support. This reading contests the neoliberal reading (which
 *   frames capital controls as inefficient restrictions on markets) and the
 *   sovereignty-defense reading (which frames them as temporary insulation
 *   against external discipline). The ε-invariance principle: this reading
 *   has a single, stable extraction measure because it consistently defines
 *   the constraint as 'capital controls protecting domestic policy autonomy.'
 *   An alternative reading would change what counts as extraction; this one
 *   does not.
 *
 * KEY AGENTS:
 *   - National governments: beneficiaries and co-agenda-setters; use capital controls to pursue full employment and welfare expansion without destabilization.
 *   - International finance capital: victims; cannot move capital freely, faces repatriation restrictions, arbitrage opportunities foreclosed.
 *   - Labor movements: beneficiaries; protected from capital strikes and capital flight in response to wage demands.
 *   - IMF: agenda-setter and administrator; coordinates consent to capital controls, provides liquidity support to nations using them.
 *   - Speculative investors: victims; prevented from profiting via currency speculation or sudden capital reallocation.
 *   - Welfare-state constituencies: beneficiaries; gain from policy space for social expansion without currency instability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.38).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.38).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls Coordination (Keynesian Embedded Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "economic/political/institutional").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e0221a88-46e2-4e1f-a260-c5236cbb1b06').
narrative_ontology:cs_kernel_codification('e0221a88-46e2-4e1f-a260-c5236cbb1b06', fixed_text).
narrative_ontology:cs_authority_grounding('e0221a88-46e2-4e1f-a260-c5236cbb1b06', lineage).
narrative_ontology:cs_interpretation_layer_present('e0221a88-46e2-4e1f-a260-c5236cbb1b06').
narrative_ontology:cs_reading_relation('e0221a88-46e2-4e1f-a260-c5236cbb1b06', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('e0221a88-46e2-4e1f-a260-c5236cbb1b06', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('e0221a88-46e2-4e1f-a260-c5236cbb1b06', foundational, capital_controls_are_legitimate_macroeconomic_tools).
narrative_ontology:cs_axiom_status(capital_controls_are_legitimate_macroeconomic_tools, holdable).
narrative_ontology:cs_axiom_grounding('e0221a88-46e2-4e1f-a260-c5236cbb1b06', capital_controls_are_legitimate_macroeconomic_tools, conventional).
narrative_ontology:cs_axiom('e0221a88-46e2-4e1f-a260-c5236cbb1b06', foundational, policy_autonomy_justifies_market_restrictions).
narrative_ontology:cs_axiom_status(policy_autonomy_justifies_market_restrictions, holdable).
narrative_ontology:cs_axiom_grounding('e0221a88-46e2-4e1f-a260-c5236cbb1b06', policy_autonomy_justifies_market_restrictions, instrumental).
narrative_ontology:cs_reference_frame('e0221a88-46e2-4e1f-a260-c5236cbb1b06', macroeconomic_policy_autonomy_via_capital_controls).
narrative_ontology:cs_drift_state('e0221a88-46e2-4e1f-a260-c5236cbb1b06', post_1968_gold_pool_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0221a88-46e2-4e1f-a260-c5236cbb1b06', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_movements).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations_seeking_unrestricted_capital_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bretton Woods grants them explicit authority to maintain capital controls—restrictions on the movement of money across borders—in order to insulate domestic monetary and fiscal policy from external speculation and capital flight. They use this protection to pursue full-employment policies, welfare expansion, and redistributive taxation without destabilizing the exchange rate. The constraint allows governments to coordinate monetary policy with the IMF while retaining the power to regulate capital flows.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter).

% Cannot freely move capital across borders without state permission; faces mandatory repatriation windows, currency convertibility restrictions, and portfolio limitations. They bear the cost of capital immobility—returns on investments abroad are delayed or blocked entirely, and arbitrage opportunities are foreclosed. Their exit is constrained by the treaty structure itself, which explicitly permits (even mandates) these restrictions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital, payer,
    powerful, biographical, trapped, global).

% Benefit from the constraint because it protects against capital strikes and capital flight in response to wage demands or labor legislation. With capital controls in place, employers cannot credibly threaten to move production overseas or repatriate profits to escape labor agreements. This constraint enables the post-war labor compact in Europe and North America, where full employment and collective bargaining became sustainable policy goals.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_movements, beneficiary,
    organized, generational, mobile, national).

% Benefit from domestic policy space to expand social provision—education, healthcare, pensions—without triggering capital flight or currency crises. The constraint prevents speculative attacks on the currency in response to expansionary social spending. Vulnerable populations (low-income workers, unemployed, elderly) gain from the stability the constraint enables, though they do not directly participate in setting policy.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_constituencies, beneficiary,
    moderate, biographical, constrained, national).

% Cannot profit from currency speculation or sudden capital reallocation across national borders. The constraint directly prevents the speculative attacks that would generate outsized returns. They are trapped: their preferred strategy (moving capital to exploit interest-rate differentials or bet against currencies) is institutionally foreclosed by the treaty framework.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors, payer,
    powerful, immediate, trapped, global).

% Face restrictions on repatriating profits, accessing foreign exchange markets freely, or relocating production in response to labor or tax policy. They must negotiate with national governments for permission to move capital across borders. While they retain operational capability within nations, their global financial flexibility is reduced.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations, payer,
    powerful, biographical, constrained, global).

% Administers and enforces the capital control regime through Article VI of the Bretton Woods charter, which explicitly permits member states to restrict capital movements. The IMF coordinates consent and provides liquidity support to governments using capital controls. It holds enforcement authority by conditioning access to IMF resources on appropriate use of controls.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf, agenda_setter,
    institutional, generational, arbitrage, global).

% Are formally included in Bretton Woods but face pressure to open their capital accounts as a condition of IMF support, even though the charter permits controls. They would argue that capital controls are essential for their own monetary autonomy and development, but this argument becomes contested in the reading sibling constraints.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, developing_economies, excluded,
    moderate, generational, constrained, global).

% Measure the constraint's distributional effects and debate whether capital controls are efficient coordination (this reading) or distortion (the neoliberal reading). They document how long-term investment flows, unemployment rates, and welfare provision changed as capital controls tightened or were dismantled.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, academic_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of capital flight: without coordinated capital controls, any one nation opening its borders to unrestricted capital movement would invite speculative inflows and outflows that destabilize its currency and make domestic monetary policy ineffective. Bretton Woods allows nations to commit together to a regime of controlled capital mobility, so each can pursue full-employment and redistributive policies without triggering a race-to-the-bottom in capital regulation.
% TRANSFER_FUNCTION: Transfers the ability to pursue macroeconomic policy autonomy FROM international financial markets TO national governments. It moves the constraint on policy choice from 'what global capital markets will tolerate' to 'what the treaty permits.' Capital that would otherwise flee in response to expansionary spending is immobilized; that immobility is redistributed as policy freedom to labor and welfare constituencies.
% ABSENT_VOICES: Financial capital's preferred representatives—bond traders, currency speculators, multinational corporations optimizing global cash flows—are structurally excluded from the treaty-making process; the constraint is imposed on them, not negotiated with them. They do not sit at the table; their objection is registered only through economic pressure (capital strikes if the regime weakens), never through voice.
% DISAPPEARANCE_RATIONALE: If capital controls and the Bretton Woods framework vanished overnight, financial capital would immediately redeploy across borders seeking returns; currencies would face speculative pressure; governments would lose the ability to run independent monetary policies without inviting capital flight; the post-war welfare state would face immediate pressure to scale back social spending (as capital would threaten exit). The entire architecture of full-employment policy and labor bargaining power that capital controls enabled would collapse within months.
% FOUNDING_PROBLEM: The 1930s and World War II demonstrated that unconstrained capital mobility enables speculative attacks and forces governments into self-defeating austerity during crises. Maynard Keynes and the architects of Bretton Woods sought to protect nations' ability to pursue expansionary fiscal policy and full-employment objectives without being punished by capital flight. The founding problem is: how can nations coordinate to prevent capital from disciplining macroeconomic policy?
% FOUNDING_PROBLEM_CORROBORATION: Keynes and White (US Treasury) explicitly documented this problem in the design negotiations (1944). Labor movements and welfare-state constituencies across the OECD attested to the problem through political coalition-building for the system's maintenance into the 1970s. Post-war economic historians (Ruggie, Helleiner, Polanyi's legacy) corroborate that the problem was real and the constraint addressed it. The constraint's eventual dismantling (1970s–1980s) was driven by financial-capital interests, which only confirms that the original problem (capital's power to discipline policy) persisted as capital controls were removed.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint does impose real costs on international capital, but it is justified by a genuine coordination problem (preventing speculative attacks and enabling collective commitment to macroeconomic stability). Suppression is moderate (0.42) because enforcement relies on treaty legitimacy and IMF conditionality rather than on coercive exclusion of dissenting parties; capital is coerced but not excluded from sitting at the negotiating table (it simply lost the vote). Theater is low-to-moderate (0.28) because the coordination function is substantive: capital controls genuinely enable full-employment policy, the IMF genuinely provides liquidity, and governments genuinely use the treaty authority. The rising theater ratio (1944→1973) reflects increasing strain: by the late 1960s, the Bretton Woods constraint became theatricalized as the enforcement machinery (the gold standard backing) eroded; governments performed compliance while the underlying mechanism weakened. Accessibility collapse is high (0.72) because once nations accept the treaty framework, alternatives (unilateral capital controls without coordination, or unrestricted capital flows) collapse as viable middle-ground options; the binary is stark: coordinate within Bretton Woods or face the discipline of unconstrained capital markets. Resistance is moderate (0.55) because financial capital resists throughout (through capital strikes, pressure on central banks, currency speculation at the system's boundaries), but organized labor and welfare constituencies actively defend the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (national governments + IMF), Bretton Woods is genuine coordination to solve a collective-action problem; from the victim seat (international financial capital), it is coercive extraction sustained by political coalition. The engine computes both: a government beneficiary sees low d (benefits without bearing costs) while a financial-capital victim sees high d (bears suppression without benefit). This divergence is structural, not a measurement error—it is exactly the point of a tangled-rope constraint where one party is coordinated and another pays.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are beneficiaries (d near 0.0): they collect policy autonomy and extract this from financial capital through coercive capital controls. Labor and welfare constituencies are beneficiaries (d near 0.1–0.2): they benefit from full-employment policy and social spending without direct enforcement responsibility. International finance capital is a victim (d near 0.95): it bears suppression (capital immobility, arbitrage foreclosure) without compensation or negotiating power within the treaty. Speculative investors are victims (d=1.0): the constraint exists to exclude them specifically. The IMF holds an administrative position (d ≈ 0.5): it enforces the constraint on capital in exchange for being the coordinator of liquidity support; it benefits from institutional authority but also must defend a system that constrains its own stakeholders (multinational financial interests often influence IMF governance).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap because the founding problem (capital's disciplinary power over macroeconomic policy) remained manifestly live through the entire interval and beyond. The constraint is genuinely enforced (the IMF uses conditionality; governments coordinate capital-control policy; financial capital experiences real suppression). However, the rising theater ratio (1944→1973) signals warning: the gold-standard mechanism that backed the entire system was eroding throughout the 1960s; by 1968 (the 'London Gold Pool' crisis), the coordination fiction became transparent—governments were performing commitment to gold convertibility while the underlying machinery failed. The constraint's terminal collapse (1973) was not mandatrophy (it did not persist without function) but rather the point at which suppression costs exceeded coordination benefits—financial capital's resistance accumulated, and the beneficiaries (labor and welfare states) lacked the power to maintain the enforcement against the now-dominant neoliberal coalition. Mandatrophy would have been if Bretton Woods persisted into the 1990s–2000s as theatrical compliance while the coordination function had already disappeared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_control_legitimacy_contestation,
    'Are capital controls a legitimate tool of national policy (as this reading asserts) or are they market distortions that reduce efficiency (as the neoliberal reading asserts)?',
    'Empirical comparison: measure long-term growth, unemployment, and wealth inequality in economies with and without capital controls, holding development level and institutional quality constant. Compare post-1973 outcomes (capital liberalization) to pre-1973 outcomes (capital controls) within the same nations.',
    'If post-liberalization growth and employment are better, the neoliberal reading gains structural credibility. If pre-liberalization welfare outcomes were superior or post-liberalization inequality increased, this reading''s legitimacy claim strengthens. The Piketty/IMF research of the 2010s (showing inequality rose post-liberalization, growth did not) partially resolves this omega toward this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_legitimacy_contestation, empirical, 'Whether capital controls are legitimate policy or distortion.').

omega_variable(
    beneficiary_stability_over_interval,
    'Did the beneficiary coalition (national governments + labor + welfare constituencies) remain stable in support for capital controls throughout 1944–1973, or did support fragment as the constraint aged?',
    'Political-economy history: examine labor-movement positions, welfare-state expansion timelines, and government statements across decades. Did labor defend the constraint or shift toward capital liberalization? Did welfare constituencies fight for capital controls or accept their erosion?',
    'If support fragmented (labor or welfare constituencies defected to neoliberal positions), the constraint''s beneficiary structure was not as stable as this reading assumes, and the constraint may have been less a stable rope than a destabilized tangled_rope nearing collapse. Historical record shows labor generally supported capital controls through the 1960s but began to fragment in the 1970s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_stability_over_interval, empirical, 'Coalition stability for the beneficiary-imposed constraint.').

omega_variable(
    reading_versus_material_interest,
    'Is this reading a genuine interpretation of Bretton Woods'' design intent, or does it project Keynesian ideology onto a historically contingent outcome?',
    'Textual analysis of the Bretton Woods Conference records, Keynes and White papers, and IMF founding documents. Did the architects explicitly frame capital controls as protecting macroeconomic autonomy, or did that framing emerge later from advocates seeking to defend the system against neoliberal critique?',
    'If the reading accurately reflects design intent, it carries stronger committer legitimacy. If the reading is a retroactive projection, the kernel''s indeterminacy is higher—the sibling readings have equal claim to the text. Historical scholarship (Helleiner, Ruggie, Steliglitz) suggests the reading is partially ex-post (post-1960s scholars emphasized autonomy protection more than contemporary designers), but Keynes''s own writings do foreground capital-control authorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_material_interest, conceptual, 'Whether the reading represents original design intent or retroactive reinterpretation.').

omega_variable(
    measurement_basis_contested,
    'Is extractiveness the right dimension to measure Bretton Woods'' constraint on capital, or does the core issue lie in sovereignty, legitimacy, or power distribution?',
    'Philosophical analysis: define what extractiveness MEANS in the context of international agreements where one party (capital) is not a signatory but a subject of the regime. Does suppression of capital count as ''extraction'' (this reading''s assumption) or as ''regulation of a non-party actor'' (which might not count as extraction in the constraint-theory sense)?',
    'If the regime is re-measured as ''regulation of non-parties rather than extraction from stakeholders,'' the classification could shift from rope/tangled_rope to ''enforcement mechanism'' or global infrastructure, changing the type significantly. This omega documents that the choice to measure capital-control suppression as extractiveness is a reading choice, not a neutral measurement choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_basis_contested, preference, 'Whether capital-control suppression should be measured as extractiveness or as legitimate regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.12).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(bret_tr_t1950, observed).
narrative_ontology:measurement(bret_tr_t1960, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(bret_tr_t1960, observed).
narrative_ontology:measurement(bret_tr_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1968, 0.28).
narrative_ontology:measurement_basis(bret_tr_t1968, observed).
narrative_ontology:measurement(bret_tr_t1970, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1970, 0.3).
narrative_ontology:measurement_basis(bret_tr_t1970, observed).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.28).
narrative_ontology:measurement_basis(bret_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement_basis(bret_be_t1950, observed).
narrative_ontology:measurement(bret_be_t1960, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement_basis(bret_be_t1960, observed).
narrative_ontology:measurement(bret_be_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1968, 0.4).
narrative_ontology:measurement_basis(bret_be_t1968, observed).
narrative_ontology:measurement(bret_be_t1970, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1970, 0.39).
narrative_ontology:measurement_basis(bret_be_t1970, observed).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.38).
narrative_ontology:measurement_basis(bret_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement_basis(bret_su_t1950, observed).
narrative_ontology:measurement(bret_su_t1960, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1960, 0.38).
narrative_ontology:measurement_basis(bret_su_t1960, observed).
narrative_ontology:measurement(bret_su_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1968, 0.42).
narrative_ontology:measurement_basis(bret_su_t1968, observed).
narrative_ontology:measurement(bret_su_t1970, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1970, 0.44).
narrative_ontology:measurement_basis(bret_su_t1970, observed).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement_basis(bret_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, post_war_labor_bargain_social_democracy).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cold_war_geopolitical_alignment_dollar_hegemony).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family decomposing the Bretton Woods treaty kernel. The three readings (keynesian_embedded_liberalism, neoliberal_convertibility, sovereignty_defense) instantiate the same treaty text with different structural interpretations. This reading measures capital-control suppression as extraction justified by coordination function; the neoliberal reading measures the same controls as market distortion; the sovereignty reading measures external discipline as extraction. Each has its own ε, beneficiary/victim set, and classification. All three are linked via network.affects_constraints because they are sibling interpretations of the same institutional commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
