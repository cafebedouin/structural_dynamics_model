% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This is the FREEDOM-FLOOR READING of unconditional income support as a
 *   policy constraint. In this reading, UBI operates as a removal of labor
 *   market coercion: by decoupling subsistence from employment, it enables
 *   workers, caregivers, and creators to participate voluntarily rather than
 *   under desperation-driven constraint. This reading frames the constraint
 *   as coordination that enables autonomy, not as redistribution that crowds
 *   out work incentives (the dependency-trap reading) or as a theoretically
 *   incoherent cross-partisan compromise (the universality-paradox reading).
 *   The three readings contest a shared kernel—what unconditional income
 *   support IS—but instantiate incompatible structural analyses and empirical
 *   predictions. This JSON instantiates ONLY the freedom-floor reading as a
 *   coherent, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - precarious_workers: laborers dependent on employment for subsistence, experiencing employer bargaining leverage from desperation
 *   - unpaid_caregivers: those providing socially necessary care (children, elders, disabled people) outside the labor market, currently trapped between unpaid obligation and wage-labor requirement
 *   - artists_creators: those whose work is intermittent or non-market, forced into unrelated wage labor
 *   - abuse_survivors: those economically trapped in abusive relationships by lack of independent income
 *   - labor_market_employers: those who currently wield bargaining leverage deriving from workers' material desperation
 *   - general_taxpayers: those bearing the fiscal cost of UBI through expanded taxation
 *   - policy_advocates__freedom_reading: organized advocates framing UBI as autonomy-enabling, removing coercion
 *   - labor_organizing_movements: strategic ally seeing UBI floor as complementary to worker power
 *   - dependency-trap advocates: excluded (contest empirical premises about labor supply effects)
 *   - universality-paradox theorists: analytical observers documenting theoretical tensions across implementation paths
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.32).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'de744592-ff6a-429f-8245-e85e9f49e275').
narrative_ontology:cs_kernel_codification('de744592-ff6a-429f-8245-e85e9f49e275', distributed).
narrative_ontology:cs_authority_grounding('de744592-ff6a-429f-8245-e85e9f49e275', distributed).
narrative_ontology:cs_reading_relation('de744592-ff6a-429f-8245-e85e9f49e275', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('de744592-ff6a-429f-8245-e85e9f49e275', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('de744592-ff6a-429f-8245-e85e9f49e275', foundational, material_autonomy_enables_choice).
narrative_ontology:cs_axiom_status(material_autonomy_enables_choice, holdable).
narrative_ontology:cs_axiom_grounding('de744592-ff6a-429f-8245-e85e9f49e275', material_autonomy_enables_choice, deontological).
narrative_ontology:cs_axiom('de744592-ff6a-429f-8245-e85e9f49e275', foundational, labor_desperation_suppresses_bargaining).
narrative_ontology:cs_axiom_status(labor_desperation_suppresses_bargaining, holdable).
narrative_ontology:cs_axiom_grounding('de744592-ff6a-429f-8245-e85e9f49e275', labor_desperation_suppresses_bargaining, empirically_contingent).
narrative_ontology:cs_reference_frame('de744592-ff6a-429f-8245-e85e9f49e275', labor_market_coercion_framework).
narrative_ontology:cs_drift_state('de744592-ff6a-429f-8245-e85e9f49e275', contemporary_welfare_state_contestation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('de744592-ff6a-429f-8245-e85e9f49e275', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists_creators).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, labor_organizing_movements).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, labor_market_employers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Low-wage, irregular work, no benefits, facing employer leverage that suppresses bargaining power. UBI floor means they can refuse unacceptable terms without immediate destitution. Income volatility is absorbed by the floor, making labor market participation a choice rather than survival necessity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% Provide childcare, eldercare, or disability support outside the market. Currently trapped between unpaid family obligation and labor market exclusion. UBI enables them to sustain socially necessary care without forced wage labor or destitution, recognizing care as legitimate economic contribution.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    moderate, biographical, identity_locked, local).

% Depend on intermittent, uncertain income from creative work. Often forced into wage labor that displaces creative time. UBI floor removes the subsistence crisis that forces them into unrelated work, enabling full-time creative practice without institutional gatekeepers' permission.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists_creators, beneficiary,
    moderate, biographical, constrained, global).

% Face economic entrapment in abusive relationships because leaving means destitution. UBI floor provides material ground for exit that did not require proving abuse, meeting institutional conditions, or finding new employment immediately—autonomy from economic coercion.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, biographical, trapped, local).

% Face higher labor costs if wage floors rise to offset UBI inflation, or alternatively face reduced bargaining power as workers gain exit alternatives. From this reading, the shift in power is the point—employers lose the leverage that comes from workers' desperation. Some sectors experience selective labor tightening; labor-intensive industries restructure toward higher productivity or automation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, labor_market_employers, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, labor_market_employers, observer).

% Bear the fiscal cost through expanded taxation. The transfer is distributed across the tax base; incidence depends on the tax structure (progressive vs. flat). This reading frames the transfer as enabling autonomy for those constrained by market coercion, not as subsidy to idleness—a different moral framing than dependency-trap reading suggests.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, general_taxpayers, payer,
    organized, biographical, constrained, national).

% Advocates for UBI on autonomy grounds: removal of coercion, dignity, recognition of unpaid work. Sets policy agenda by framing the problem as labor market coercion rather than individual deficiency. Builds coalition across ideological lines by emphasizing freedom (conservative libertarian strand) and justice (progressive strand), though the sibling readings contest whether this framing obscures incompatible implementation paths.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, policy_advocates__freedom_reading, agenda_setter,
    organized, generational, mobile, global).

% View UBI floor as a wedge that shifts power from employers to workers—workers can refuse bad terms because the floor removes desperation. This reading aligns UBI with union power: both remove coercive leverage from the employer side. Some labor traditions contest this (see dependency_trap_reading), fearing UBI crowds out labor organizing itself.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, labor_organizing_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, labor_organizing_movements, agenda_setter).

% Administers conditional welfare (asset tests, behavioral conditions, ongoing verification). UBI replaces their gatekeeping role with automatic universality. They would argue UBI is wasteful (gives to non-needy) and loses targeting precision; from this reading their objection reflects institutional interest in maintaining the discretionary power that conditional programs grant.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, means_tested_welfare_administrators, excluded,
    institutional, biographical, constrained, national).

% Advocates who read UBI as incentive-distorting subsidy. They hold that unconditional transfer weakens labor supply incentives and crowds out targeted aid. Not in conversation with this reading's proponents at the empirical level (see omegas on labor supply effects and data interpretation). Their exclusion reflects disagreement on both values (what counts as autonomy) and facts (what labor supply effects empirically occur).
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, dependency_trap_advocates, excluded,
    organized, generational, mobile, global).

% Observes that UBI's cross-ideological appeal masks conflicting implementation paths: libertarian UBI (replace all welfare, low flat rate) vs. social-democratic UBI (add to existing programs, higher rate) converge on similar fiscal outcomes while instantiating incompatible theories of justice. From this reading's seat, the paradox observers see either empirical vindication (the freedom floor works across multiple implementation frames) or a sign of theoretical incoherence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, universality_paradox_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal material floor decoupled from employment status or institutional gatekeeping, enabling voluntary labor market participation and unpaid social contribution (caregiving, creation) without destitution—solves the coordination problem of how to remove labor market coercion while maintaining economic participation.
% TRANSFER_FUNCTION: Transfers purchasing power from general taxation (incidence determined by tax structure) to all residents unconditionally. The transfer is distributed universally, not concentrated on the worst-off; the freedom-floor reading argues this universality is the point—removal of stigma, discretion, and conditionality.
% ABSENT_VOICES: Means-tested welfare administrators are structurally excluded (their gatekeeping role dissolves under universality). Dependency-trap advocates are excluded (they contest the empirical premise that labor supply effects are minimal). Those whose labor market power depends on worker desperation are present but have incentives to downplay the power-shift effect.
% DISAPPEARANCE_RATIONALE: If UBI disappeared, precarious workers would lose the exit option that weakens employer coercion; unpaid caregivers would return to forced trade-off between care and subsistence; abuse survivors would lose the material ground for exit without institutional gatekeeping. Labor supply would not collapse but labor market power distribution would shift back toward employers. The freedom-floor reading predicts labor-intensive sectors would face reduced willingness to accept bad terms; disappearance of UBI would rearrange that power dynamic.
% FOUNDING_PROBLEM: Labor market coercion: workers constrained by desperation accept unacceptable terms (wage, hours, conditions, dignity) that they would refuse if material survival were unconditioned. Unpaid work (caregiving, creation) is rendered invisible and undervalued. Welfare conditionality adds institutional coercion and stigma on top of market coercion. The founding problem is not poverty per se but the removal of choice from those constrained by it.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying wage-productivity gaps, bargaining theory literature on desperation as leverage, qualitative research on worker decision-making in low-wage sectors, testimonies from abuse survivors, and data on welfare conditionality burden all attest the founding problem is live. Dependency-trap reading contests the significance (argues incentive effects dominate), not the existence of coercive labor market dynamics; universality-paradox reading contests implementation coherence, not the coercion claim. The founding problem is corroborated across the contested readings despite disagreement on policy response.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32 at interval end) because the transfer removes resources from those who currently benefit from labor market coercion (employers gaining desperation-leverage, means-tested welfare administrators maintaining gatekeeping power), but the constraint does not extract FROM beneficiaries—it removes extraction pressure on them. The transfer is a cost to general taxpayers, but this reading frames it as enabling choice, not subsidizing idleness, so the extractiveness ε is assessed as the coercion removed, not the fiscal cost. Suppression is very low (0.15) because the mechanism operates by removing suppression (the coercive pressure of desperation), not by imposing it. The constraint succeeds by removing barriers, not enforcing compliance. Theater is minimal (0.08) because the functional content (material support enabling voluntary participation) is the entire point; there is no performative layer masking different activity. Accessibility of alternatives is low (0.22) not because alternatives are unavailable but because the constraint itself is the alternative to desperation-driven labor market participation—it removes the most common coercive exit option. Resistance is substantial (0.58) because the dependency-trap reading challenges the empirical premise (labor supply effects) and the universality-paradox reading contests implementation coherence; employer interests align with suppression of UBI policy.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the freedom-floor reading seat and the dependency-trap reading seat is profound and structural. From the freedom-floor seat, UBI removes coercion and enables autonomy—precarious workers, caregivers, and abuse survivors gain exit options they lacked. From the dependency-trap seat, UBI weakens labor supply incentives and crowds out targeted aid that reaches those truly in need. Both readings reference the same empirical claims (labor supply elasticity, redistribution mechanics) but interpret them through incompatible frameworks: one sees choice-enabling removal of desperation, the other sees incentive-distorting subsidy. The universality-paradox reading observes that libertarian and social-democratic versions of UBI have different justifications but similar fiscal outcomes, suggesting either empirical convergence or theoretical confusion—a different critique entirely. The engine computes each seat's type from its structural position: the freedom-floor reading instantiates a rope (genuine coordination enabling participation); the dependency-trap reading likely computes as tangled_rope or snare (extraction justified by behavior change claims); the universality-paradox reading likely computes as piton or tangled_rope (theoretical incoherence masked by political appeal). These are NOT ONE constraint from multiple angles—they are genuinely different constraints, each with its own ε and beneficiary/victim structure, linked by network relationship.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, unpaid caregivers, artists, and abuse survivors are the structural beneficiaries—the constraint removes coercive pressure on them. Their directionality is low (d near 0.0), indicating subsidy/benefit. General taxpayers are the payers—the fiscal cost lands on them—so their directionality is higher (d near 0.5–0.7 depending on progressivity of the tax structure). Employers lose bargaining leverage deriving from worker desperation, so they bear an opportunity cost; their directionality is positive but they are not listed as victims because the freedom-floor reading does not frame the power-shift as harm—it frames the prior desperation-leverage as coercive and its removal as restoring just terms. Labor organizers are beneficiaries (worker power increases) not payers. The key analytical move: the freedom-floor reading's beneficiary structure includes people who were already income-poor (precarious workers, caregivers) AND people who were not poor but constrained by bad options (abuse survivors, artists with adequate savings but forced into wage labor). This structure is importantly different from a poverty-reduction reading; it is a choice-enabling reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem is 'labor market coercion constraining choice'—a problem that is live in current labor market dynamics. The disappearance_verdict is 'world_rearranges'—if UBI disappeared, worker bargaining power would shift back to employers, unpaid caregivers would face renewed trade-offs, abuse survivors would lose material ground for exit. No mandatrophy signal here. The constraint's function is not orphaned; it directly addresses its stated founding problem. Where the danger of mandatrophy appears is in the CONTESTED EMPIRICAL CLAIMS: if labor supply effects turn out to be substantial (contrary to this reading's empirical hypothesis), the founding problem might be reframed (from 'coercion' to 'incentive distortion'), the beneficiaries might be reclassified (precarious workers no longer benefit if employment declines), and the type might shift to tangled_rope or snare (extraction justified by behavior-change claims that are failing). This is the research frontier captured by the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_contestation,
    'What is the actual labor supply elasticity response to unconditional income support? Does UBI modestly reduce labor supply (as dependency-trap reading predicts) or leave it substantially unchanged (as freedom-floor reading hypothesizes)?',
    'Long-term RCT data from comparable economies (Alaska Permanent Fund dividend, Kenya GiveDirectly, India randomized trials, proposed North American pilots). Measure employment, hours, wage levels, occupational mobility 3–5 years post-implementation across income and demographic groups.',
    'If labor supply reduction is substantial (>5% overall, >15% for low-wage workers), the founding problem (coercion) may be reframed as incentive distortion, beneficiary structure shifts (precarious workers now ''harmed'' by employment reduction), and type likely reverts to tangled_rope or snare. If minimal (<2%), freedom-floor reading is vindicated and the constraint remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_contestation, empirical, 'Labor supply elasticity to unconditional income support is the core empirical crux between freedom-floor and dependency-trap readings.').

omega_variable(
    bargaining_power_measurement,
    'Does UBI floor demonstrably shift labor market bargaining power away from employers toward workers, or does the effect dissolve in aggregate labor supply adjustment?',
    'Measurement of wage growth, benefit adoption, occupational mobility, and employer concessions in low-wage sectors before and after UBI implementation. Compare against counterfactual (similar economy without UBI). Qualitative research on worker decision-making about work acceptance and negotiation.',
    'If bargaining power shift is real and sustained, the freedom-floor reading''s claim that UBI removes coercion is vindicated. If bargaining power shift is temporary (absorbed by labor supply responses), the constraint may be reclassified as tangled-rope (coordination effect is real but ephemeral, requiring active enforcement to sustain). If bargaining power does not shift at all, ε drops sharply (the removal of coercion that defines the reading is not happening) and type reverts to scaffolding or even piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bargaining_power_measurement, empirical, 'Whether UBI''s coercion-removal mechanism actually operates through bargaining power shift or whether the effect is illusory.').

omega_variable(
    welfare_conditionality_elimination,
    'Does removing conditionality and means-testing (administrative gatekeeping) from income support actually reduce stigma and expand effective freedom, or does universality itself create different forms of stigma or administrative burden?',
    'Comparative implementation data: measure stigma (survey self-reports, behavioral proxies like non-uptake), administrative cost, and perceived dignity across UBI (unconditional universal) vs. means-tested welfare systems. Track shifts in how recipients describe their participation.',
    'If stigma is substantially reduced and freedom to exit bad situations is demonstrably enabled, the beneficiary group (abuse survivors, caregivers) experiences genuine autonomy gain, supporting the type as rope. If stigma is redirected (e.g., ''UBI recipient'' becomes a status category) or administrative burden is replaced with different friction, the type may be tangled_rope (coordination achieved but with non-obvious extraction or complexity cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_conditionality_elimination, empirical, 'Whether unconditional universality actually removes welfare stigma or redistributes it.').

omega_variable(
    caregiver_unpaid_work_recognition,
    'Does UBI floor genuinely enable and recognize unpaid caregiving as legitimate economic contribution, or does the universality structure (no distinction between work and non-work) obscure and erode the recognition claim?',
    'Ethnographic and qualitative research on how caregivers describe their labor and social position under UBI. Measure care quality and intensity outcomes. Track whether caregiving remains undervalued or becomes socially recognized as necessary work.',
    'If recognition increases and caregiver autonomy is substantially enabled, the beneficiary structure holds. If caregiving erodes (time spent on care decreases as other income options become available without institutional recognition) or social recognition does not materialize, the constraint may fail its founding problem for this constituency and reclassify.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caregiver_unpaid_work_recognition, conceptual, 'Whether UBI enables caregiver autonomy or erodes unpaid work through invisible reallocation of time and social value.').

omega_variable(
    kernel_reading_coherence,
    'Are the freedom-floor, dependency-trap, and universality-paradox readings genuinely distinct constraints, or do they decompose further into sub-readings with incompatible ε values within each framing?',
    'Structural analysis of axioms, beneficiary claims, and empirical hypotheses within each reading. Test whether a single framing (e.g., ''freedom-floor'') branches into incompatible sub-claims (e.g., ''UBI as worker power'' vs. ''UBI as artist support'' vs. ''UBI as abuse escape'') that would yield different ε values if separated.',
    'If the readings decompose further, the kernel is more complex than three constraints; the network.affects_constraints structure expands. If readings hold as coherent wholes, the three-constraint family is stable. This affects the scope of the freedom-floor constraint—is it only about labor coercion, or does it coherently span caregiver autonomy, artist support, and abuse survival?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Whether the freedom-floor reading is a single coherent constraint or a bundle of distinct constraints that should be decomposed per ε-invariance.').

omega_variable(
    universality_vs_targeting_tradeoff,
    'Is there a genuine empirical tradeoff between universality (the defining feature of UBI) and targeting precision (means-tested welfare''s defining feature), or can one design UBI that achieves both through income-tapering or phase-out structures?',
    'Economic modeling and pilot data comparing pure universal UBI against tapered/phased designs. Measure redistribution efficiency, coverage of most-vulnerable populations, political sustainability, and administrative cost across designs.',
    'If a tapered design can achieve both universality and targeting, the dependency-trap reading''s ''crowd out'' claim loses force (you can have UBI-like freedom with targeted support). If targeting undermines universality (re-introduces conditionality and stigma), the freedom-floor reading''s core claim holds. This affects the boundary between the freedom-floor constraint and possible hybrid designs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_targeting_tradeoff, empirical, 'Whether the universality-paradox reading''s observation of incompatibility between implementation paths is a real structural feature or a design artifact that can be resolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__freedom_floor_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__freedom_floor_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__freedom_floor_reading, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__freedom_floor_reading, base_extractiveness, 25, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.14).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__freedom_floor_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__freedom_floor_reading, suppression_requirement, 25, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'unconditional_income_support'. The freedom-floor reading frames UBI as autonomy-enabling (removal of labor market coercion); the dependency-trap reading frames it as incentive-distorting subsidy; the universality-paradox reading observes that both readings' implementations converge fiscally despite incompatible justifications. The three constraints share a kernel and affect each other through research evidence (labor supply data, bargaining power effects) and political framing (which reading dominates shapes policy design). Each has its own ε, beneficiary/victim structure, and type—they are genuinely distinct constraints, not perspectives on a single constraint. They are linked by network edges capturing structural influence (shared empirical facts, competing framings, implementation paths).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
