% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Rapid Decarbonization as Intergenerational Justice Obligation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation_priority reading of the climate_response_obligation kernel
 *   instantiates rapid decarbonization as a binding intergenerational justice
 *   obligation. It reads the kernel (the UNFCCC/Paris commitment to avoid
 *   dangerous interference) as requiring minimization of peak warming through
 *   aggressive near-term mitigation, with burden-sharing weighted by
 *   historical emissions and capability. This reading competes with
 *   adaptation_priority (accept warming, invest in resilience) and
 *   degrowth_reading (reduce throughput, not just decarbonize). Structurally,
 *   it is a tangled rope: genuine coordination of a planetary collective
 *   action problem (the carbon budget) fused with asymmetric extraction
 *   (current generation pays transition costs; fossil capital absorbs
 *   stranded asset losses; Global North pays disproportionate share). The
 *   constraint requires active enforcement (NDC cycles, carbon pricing,
 *   regulation) and its extraction has risen steadily as the carbon budget
 *   tightens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.55).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Rapid Decarbonization as Intergenerational Justice Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '81c7997b-7974-40c7-a346-6e4ed86622a2').
narrative_ontology:cs_kernel_codification('81c7997b-7974-40c7-a346-6e4ed86622a2', formalized).
narrative_ontology:cs_authority_grounding('81c7997b-7974-40c7-a346-6e4ed86622a2', lineage).
narrative_ontology:cs_interpretation_layer_present('81c7997b-7974-40c7-a346-6e4ed86622a2').
narrative_ontology:cs_reading_relation('81c7997b-7974-40c7-a346-6e4ed86622a2', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('81c7997b-7974-40c7-a346-6e4ed86622a2', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('81c7997b-7974-40c7-a346-6e4ed86622a2', foundational, intergenerational_justice_requires_minimizing_warming).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_minimizing_warming, holdable).
narrative_ontology:cs_axiom_grounding('81c7997b-7974-40c7-a346-6e4ed86622a2', intergenerational_justice_requires_minimizing_warming, deontological).
narrative_ontology:cs_axiom('81c7997b-7974-40c7-a346-6e4ed86622a2', foundational, historical_emissions_create_differential_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_create_differential_obligation, holdable).
narrative_ontology:cs_axiom_grounding('81c7997b-7974-40c7-a346-6e4ed86622a2', historical_emissions_create_differential_obligation, conventional).
narrative_ontology:cs_reference_frame('81c7997b-7974-40c7-a346-6e4ed86622a2', unfccc_1992_stabilization_commitment).
narrative_ontology:cs_drift_state('81c7997b-7974-40c7-a346-6e4ed86622a2', post_paris_2015_implementation_gap, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('81c7997b-7974-40c7-a346-6e4ed86622a2', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_global_workforce).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_taxpayers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital_asset_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_south_vulnerable_populations).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, common_but_differentiated_responsibilities).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, carbon_budget_finitude).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the climatic consequences of today's emissions pathway. They cannot negotiate, litigate, or exit the atmospheric commons. Their welfare is the constraint's stated justification but they have no voice in its enforcement. Every fraction of avoided warming is a direct benefit they cannot claim.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Face disproportionate climate impacts at current warming levels. Benefit directly from minimized warming but also bear transition costs where mitigation policy restricts development pathways. Their exit options are constrained by global finance conditions and technology transfer terms set by Global North institutions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_vulnerable_populations, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_south_vulnerable_populations, payer).

% Bear transition costs through energy price increases, sectoral job displacement, and consumption changes. In carbon-intensive regions, exit means geographic or occupational mobility that is often unavailable. In service economies, costs diffuse through prices. Organized labor in some jurisdictions has negotiated just-transition provisions, but coverage is uneven.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_global_workforce, payer,
    moderate, biographical, constrained, global).

% Finance the disproportionate mitigation burden through public investment, subsidies, and international climate finance commitments. Historical emissions create the justice claim for this burden. Exit options include capital flight, political resistance, and lobbying for weaker targets — but treaty obligations and domestic law constrain defection.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_taxpayers, payer,
    powerful, biographical, mobile, national).

% Face stranded asset risk as the constraint's enforcement renders reserves unburnable. Their exit is constrained by the physical specificity of assets (refineries, pipelines, reserves) and the global scope of the carbon budget. They deploy political influence to delay enforcement, shape transition policy, and secure compensation — making them both targets of extraction and active shapers of the constraint's form.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital_asset_holders, payer,
    institutional, biographical, constrained, global).

% Administer the constraint through UNFCCC processes, national NDCs, carbon pricing, and regulatory standards. They set the mitigation trajectory, monitor compliance, and allocate burdens. Their authority derives from the kernel's legitimacy (intergenerational justice, carbon budget science). They face legitimacy pressure when enforcement gaps widen.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_policy_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Argue that 2-3°C warming is inevitable and resources should flow to resilience rather than costly prevention. They are structurally excluded from the mitigation_priority reading's framework because accepting their premise would dissolve the intergenerational justice claim. They operate in parallel policy tracks (disaster risk reduction, infrastructure hardening) but contest the mitigation obligation's primacy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, adaptation_priority_advocates, excluded,
    organized, biographical, mobile, national).

% Hold that material throughput reduction, not decarbonization alone, is required to stay within planetary boundaries. They observe the mitigation_priority reading from a distinct epistemic position — neither fully excluded nor aligned. Their critique targets the reading's growth-compatible framing; they would reshape the constraint's coordination function rather than reject the obligation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, degrowth_advocates, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational collective action problem: each generation prefers others to mitigate while it free-rides. The constraint coordinates by establishing a normative and legal obligation (via carbon budgets, NDCs, net-zero targets) that binds current emitters to a trajectory that limits cumulative warming.
% TRANSFER_FUNCTION: Moves mitigation costs (capital reallocation, energy transition expenditure, stranded asset losses, consumption adjustment) from current emitters — disproportionately Global North historical emitters and fossil capital — to secure a stable climate for future generations and vulnerable present populations. The transfer is intertemporal (present to future) and geographic (North to South via climate finance).
% ABSENT_VOICES: Future generations are the primary beneficiaries but structurally absent — they cannot consent, object, or enforce. Post-colonial states with minimal historical emissions but high vulnerability are often underrepresented in agenda-setting forums where burden-sharing is negotiated. Indigenous peoples' sovereignty claims over transition mineral extraction and land-use mitigation are frequently overridden.
% DISAPPEARANCE_RATIONALE: If the mitigation obligation vanished overnight, NDCs would lapse, carbon prices would collapse, fossil investment would resume, and the carbon budget would be exhausted within decades. The world would reorganize around unmanaged warming: adaptation becomes the only paradigm, climate finance flows cease, and the intergenerational contract dissolves. The Geological record would show the constraint's absence as a regime shift.
% FOUNDING_PROBLEM: The atmospheric commons has no price and no owner, creating a classic intergenerational tragedy: each generation extracts climate stability as a free input to production while imposing cumulative costs on all successors. The UNFCCC (1992) and Paris Agreement (2015) were built to internalize this externality through a shared, differentiated mitigation obligation grounded in historical responsibility and capability.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group III assessments corroborate the physical externality (cumulative CO2 determines warming). The UNFCCC secretariat and climate-vulnerable forum states corroborate the regime's founding logic. Fossil fuel exporting states and some Global North industry groups contest whether the founding problem requires *this* constraint (rapid decarbonization) versus adaptation or technological carbon removal — they accept the externality but dispute the solution's form and burden distribution.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint now demands near-total energy system transformation within decades — costs are large, concentrated on specific sectors and geographies, and the carbon budget's finitude makes delay extractive toward future generations. Suppression (0.55) is moderate: the constraint operates through law, finance, and norm diffusion rather than direct coercion, but alternatives (unconstrained fossil development) are actively foreclosed by policy. Theater ratio (0.28) is low-moderate: real deployment (renewables, EVs, efficiency) is occurring, but net-zero pledges often exceed credible policy, and offset markets create performative compliance. Accessibility collapse (0.45) reflects that alternatives (adaptation-only, degrowth) exist but are politically marginalized within the dominant framework. Resistance (0.72) is high: fossil capital, carbon-intensive regions, and political movements contest the constraint's legitimacy, pace, and distribution.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (current workforce, Global North taxpayers, fossil capital) experience the constraint as enforced extraction with limited exit — their computed type trends toward snare. The beneficiary seats (future generations, vulnerable populations) experience it as essential coordination — their computed type trends toward rope. The agenda-setter seat (climate institutions) experiences it as scaffold with a moving sunset (net-zero by 2050) — their computed type depends on whether the transition is credible. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the constraint's hybrid nature across all seats simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries (d ≈ 0.0) — they collect the entire avoided warming benefit with zero enforcement power. Global South vulnerable populations are net beneficiaries but with constrained exit, placing them at d ≈ 0.2-0.3 (benefit from avoided warming, pay transition costs). Current global workforce and Global North taxpayers are targets (d ≈ 0.6-0.8) — they bear costs with constrained-to-mobile exit. Fossil capital asset holders are near-full targets (d ≈ 0.85) — concentrated losses, asset-specific exit barriers, but high political power modulates effective extraction. Climate policy institutions are agenda-setters with analytical exit (d ≈ 0.3 — they administer and benefit from institutional maintenance). The derivation chain from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational externality of cumulative emissions) is live and worsening — the carbon budget is depleting. However, the constraint's *form* (NDC pledge-and-review, net-zero targets) shows mandatrophy signals: the enforcement mechanism (pledge-and-review) has not tightened commensurately with the budget, creating a widening gap between the obligation's stringency and its implementation. Theater ratio rising from 0.10 to 0.28 tracks this. The constraint is not a piton — extraction is increasing, not atrophying — but the coordination machinery is lagging the extraction demand. This is a tangled rope under strain, not a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the mitigation_priority reading a distinct constraint from the adaptation_priority and degrowth_reading readings, or are they measurement perspectives on one constraint?',
    'Apply the ε-invariance test: if evaluating the constraint via mitigation metrics (emissions trajectory, carbon budget compliance) yields a different ε than evaluating via adaptation metrics (resilience investment, damage avoided) or degrowth metrics (material throughput, energy descent), they are distinct constraints. The kernel_id climate_response_obligation should decompose into three constraint stories with independent ε.',
    'If they are one constraint, the ε would be observer-relative and the classification unstable. Decomposition into three stories with linked network.affects_constraints preserves ε-invariance and allows each reading''s distinct beneficiary/victim structure to be measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints (per ε-invariance) or perspectives on one constraint').

omega_variable(
    future_generations_voice_proxy,
    'Can the mitigation obligation''s enforcement be legitimate without a procedural mechanism for future generations'' interests?',
    'Analyze whether existing proxies (youth litigation, future generations commissioners, constitutional climate clauses, intergenerational equity jurisprudence) functionally substitute for direct voice, or whether the absence of a future-generations seat in the enforcement architecture constitutes a structural legitimacy deficit that changes the constraint''s type.',
    'If no functional proxy exists, the constraint''s coordination claim is undermined — it extracts from the present for an absent party that cannot verify compliance. This would increase effective suppression and shift classification toward snare from the future-generations seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_voice_proxy, conceptual, 'Whether procedural representation of future generations is structurally necessary for the constraint''s coordination legitimacy').

omega_variable(
    burden_sharing_enforceability,
    'Is the Global North''s disproportionate mitigation burden (CBDR-RC) enforceable, or does its voluntary character make the extraction asymmetric in practice?',
    'Track climate finance flows (pledged vs. delivered), technology transfer terms, and NDC ambition gaps by income group. If Global North burden-sharing commitments are systematically unmet while Global South mitigation proceeds anyway, the constraint operates as extraction from the Global South without the promised counter-transfer — a snare dynamic masked by the coordination frame.',
    'If CBDR-RC is unenforced, the constraint''s extraction falls on Global South development space while Global North avoids its pledged share. This would reclassify the Global South seat from net-beneficiary to net-payer, shifting the constraint''s overall structure toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_sharing_enforceability, empirical, 'Whether the differential burden-sharing that legitimizes the constraint''s asymmetry is actually enforced').

omega_variable(
    stranded_asset_compensation_capture,
    'Does fossil capital''s political influence convert stranded asset risk into public bailout, reversing the victim→beneficiary directionality?',
    'Track fossil fuel subsidy reform, just-transition funding allocation, and compensation mechanisms for stranded assets. If public funds absorb fossil capital losses while mitigation costs remain on taxpayers/workers, fossil capital''s net directionality shifts from payer (d≈0.85) toward beneficiary (d<0.5).',
    'If fossil capital captures the transition''s fiscal upside while socializing losses, the constraint''s victim set shrinks and its extraction becomes regressive. The tangled_rope classification would hold but the beneficiary/victim structure would invert for the fossil capital seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_compensation_capture, empirical, 'Whether fossil capital''s structural position as victim is offset by political capture of transition finance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1992, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t1992, climate_response_obligation__mitigation_priority, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t1997, climate_response_obligation__mitigation_priority, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t2005, climate_response_obligation__mitigation_priority, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t2010, climate_response_obligation__mitigation_priority, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t2015, climate_response_obligation__mitigation_priority, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t2020, climate_response_obligation__mitigation_priority, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t2025, climate_response_obligation__mitigation_priority, theater_ratio, 2025, 0.27).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_tr_t2030, climate_response_obligation__mitigation_priority, theater_ratio, 2030, 0.28).

% Extraction over time
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t1992, climate_response_obligation__mitigation_priority, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t1997, climate_response_obligation__mitigation_priority, base_extractiveness, 1997, 0.22).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t2005, climate_response_obligation__mitigation_priority, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t2010, climate_response_obligation__mitigation_priority, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t2015, climate_response_obligation__mitigation_priority, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t2020, climate_response_obligation__mitigation_priority, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t2025, climate_response_obligation__mitigation_priority, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_be_t2030, climate_response_obligation__mitigation_priority, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t1992, climate_response_obligation__mitigation_priority, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t1997, climate_response_obligation__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t2005, climate_response_obligation__mitigation_priority, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t2010, climate_response_obligation__mitigation_priority, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t2015, climate_response_obligation__mitigation_priority, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t2020, climate_response_obligation__mitigation_priority, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t2025, climate_response_obligation__mitigation_priority, suppression_requirement, 2025, 0.54).
narrative_ontology:measurement(climate_response_obligation__mitigation_priority_su_t2030, climate_response_obligation__mitigation_priority, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is the mitigation_priority reading of the climate_response_obligation kernel. It decomposes the kernel's single label into a structurally distinct constraint with its own ε (0.68), beneficiary/victim structure (future generations benefit, current generation and fossil capital pay), and coordination function (carbon budget allocation). The adaptation_priority reading (inevitable warming, resilience investment) and degrowth_reading (throughput reduction) are sibling constraints with different ε, different coordination functions, and different burden distributions. All three link via affects_constraints to form the kernel's constraint family. The upstream constraint (mitigation_priority) influences downstream readings by setting the carbon budget trajectory that adaptation must respond to and degrowth must operate within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
