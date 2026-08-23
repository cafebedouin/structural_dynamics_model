% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance Bridge Framework (Unified Present Harms and Existential Risks)
 *   domain: technology_ethics/ai_governance
 *
 * SUMMARY:
 *   The 'bridge reading' of AI risk governance priority asserts that present
 *   harms (bias, surveillance, displacement) and existential risks
 *   (superintelligence catastrophe) are structurally entangled — not
 *   competing priorities — and require unified governance frameworks. This
 *   reading instantiated a constraint in ~2018-2020 as funding agencies,
 *   policy bodies, and bridging research centers (Partnership on AI, CSET,
 *   GovAI, CHAI) built integrated programs. The constraint extracts
 *   moderately from both siloed communities (who must make their work legible
 *   to the bridge) and from the two victim populations (whose distinct risk
 *   profiles are flattened into a single portfolio). Bridging institutions —
 *   the 5% of papers generating 85% of cross-field citations — capture
 *   disproportionate structural benefits: funding centrality, policy access,
 *   and career-defining brokerage. The bridge is structurally fragile: it
 *   depends on a handful of broker actors and funding programs rather than
 *   distributed collaboration; if key brokers exit or funders pivot, the
 *   constraint collapses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.45).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.4).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance Bridge Framework (Unified Present Harms and Existential Risks)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "technology_ethics/ai_governance").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '8c166e49-7389-42a3-a559-f277f3bc060c').
narrative_ontology:cs_kernel_codification('8c166e49-7389-42a3-a559-f277f3bc060c', distributed).
narrative_ontology:cs_authority_grounding('8c166e49-7389-42a3-a559-f277f3bc060c', distributed).
narrative_ontology:cs_reading_relation('8c166e49-7389-42a3-a559-f277f3bc060c', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c166e49-7389-42a3-a559-f277f3bc060c', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('8c166e49-7389-42a3-a559-f277f3bc060c', foundational, present_and_existential_risks_structurally_entangled).
narrative_ontology:cs_axiom_status(present_and_existential_risks_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('8c166e49-7389-42a3-a559-f277f3bc060c', present_and_existential_risks_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('8c166e49-7389-42a3-a559-f277f3bc060c', foundational, unified_frameworks_reduce_total_ai_risk).
narrative_ontology:cs_axiom_status(unified_frameworks_reduce_total_ai_risk, holdable).
narrative_ontology:cs_axiom_grounding('8c166e49-7389-42a3-a559-f277f3bc060c', unified_frameworks_reduce_total_ai_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('8c166e49-7389-42a3-a559-f277f3bc060c', fragmented_ai_risk_governance).
narrative_ontology:cs_drift_state('8c166e49-7389-42a3-a559-f277f3bc060c', post_bridge_institutionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c166e49-7389-42a3-a559-f277f3bc060c', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, ai_safety_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, ai_ethics_researchers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, ai_risks_are_structurally_entangled).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, unified_governance_reduces_total_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research centers, funding programs, and policy initiatives that explicitly bridge AI safety and AI ethics communities. They control the unified framework vocabulary, allocate integrated research grants, and capture 85% of cross-field citation benefits while representing only 5% of publications. Their structural position depends on maintaining the bridge as the legitimate governance paradigm.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary).

% Communities currently experiencing algorithmic bias, surveillance, labor displacement, and misinformation harms. They bear the cost when bridge frameworks dilute near-term harm mitigation in favor of speculative existential risk work. Their exit from AI governance processes is structurally blocked by power asymmetry and epistemic exclusion.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% The collective of all future persons whose existence and flourishing could be curtailed by existential AI catastrophes. They bear the cost when bridge frameworks dilute existential risk prevention in favor of present-harm remediation. They have no voice, no exit, and no representation except through proxy advocates in the existential risk community.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__bridge_reading, future_humanity).

% Researchers and advocates prioritizing superintelligence alignment and existential catastrophe prevention. They view the bridge as epistemic dilution that diverts attention from the uniquely high-stakes, time-sensitive alignment problem. Their institutional homes (FHI, MIRI, Anthropic safety teams) have distinct funding and talent pipelines they guard against bridge absorption.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_community, excluded,
    organized, generational, constrained, global).

% Researchers, activists, and affected-community advocates prioritizing demonstrated harms: bias, discrimination, surveillance, labor displacement, misinformation. They view the bridge as a luxury framing that steals policy oxygen and funding from urgent, measurable injustices. Their institutional homes (DAIR, Algorithmic Justice League, civil society orgs) operate on different epistemic and funding logics.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_community, excluded,
    organized, biographical, constrained, global).

% Technical researchers working on alignment, interpretability, and robustness. They gain access to broader funding and policy relevance through bridge frameworks but pay a coherence cost: their work must be legible to ethics audiences, and they compete with ethics researchers for the same bridge-program slots.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_safety_researchers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, ai_safety_researchers, payer).

% Researchers working on fairness, accountability, transparency, and societal impacts. They gain access to existential-risk-level funding streams and long-term policy tables through bridge frameworks but pay a coherence cost: their work must engage speculative long-term scenarios, and they compete with safety researchers for the same bridge-program slots.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_ethics_researchers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, ai_ethics_researchers, payer).

% Government science agencies (NSF, EU Horizon, UKRI) and philanthropic funders (Open Philanthropy, Longview, Ford) that design integrated AI safety-ethics programs. They set the bridge agenda by defining RFP scopes, review criteria, and success metrics. They can exit by reverting to siloed programs but face political pressure to show 'holistic' governance.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, funding_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Legislators, regulators, and international bodies (EU AI Act, US Executive Orders, UN advisory boards) that need a single governance framework. They adopt bridge language because it resolves inter-agency jurisdictional disputes. They can pivot to siloed regulation if bridge frameworks fail to produce actionable rules.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers, agenda_setter,
    institutional, biographical, mobile, national).

% Academics and analysts outside the bridge funding ecosystem who study the field's sociology, bibliometrics, and epistemic dynamics. They document the 5%/85% cross-field link concentration and the bridge's structural fragility. They have no stake in the bridge's success or failure.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, independent_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified governance vocabulary, shared funding channels, and joint policy frameworks that allow AI safety (existential risk) and AI ethics (present harms) communities to coordinate on common infrastructure: model evaluation standards, incident reporting systems, compute governance, and regulatory sandboxes — avoiding duplicated effort and regulatory fragmentation.
% TRANSFER_FUNCTION: Moves research funding, institutional legitimacy, and policy attention from siloed safety and ethics programs into integrated 'AI safety and ethics' portfolios. Cross-field citation benefits (85% of links from 5% of bridge papers) accrue to bridging institutions. Marginalized communities and future humanity bear opportunity costs when bridge priorities misallocate resources relative to their distinct risk profiles.
% ABSENT_VOICES: Global South communities experiencing extractive AI deployment now (data labeling workers, surveillance subjects, displaced laborers) who are not represented in either Western safety or ethics establishments. Future generations who cannot advocate for existential risk prevention. Researchers in both silos who reject the bridge framing as category error and are excluded from bridge funding streams.
% DISAPPEARANCE_RATIONALE: If the bridge framework vanished overnight, funding agencies would revert to separate safety and ethics programs within months. Bridging institutions would lose their structural position and 85% cross-field citation advantage. Policy frameworks (EU AI Act, US EO) would fragment into parallel safety and ethics tracks. The 5% of bridge papers would lose their broker centrality. Both siloed communities would claim vindication.
% FOUNDING_PROBLEM: By 2016-2018, AI safety (existential risk) and AI ethics (present harms) had become epistemically and institutionally siloed: different conferences, journals, funding streams, policy tables, and talent pipelines. This fragmentation prevented coordinated responses to AI systems that simultaneously exhibit near-term harms and long-term alignment uncertainties (e.g., large language models).
% FOUNDING_PROBLEM_CORROBORATION: The fragmentation diagnosis is corroborated by independent bibliometric studies (e.g., the 5%/85% cross-field link statistic from multiple analyses) and by funding agency program officers who explicitly designed bridge programs (NSF Safe Learning-Enabled Systems, EU Horizon AI ethics+safety calls) to address it. The existential risk community contests that the problem was ever 'fragmentation' rather than 'category error'; the near-term harms community contests that the bridge solves rather than obscures resource allocation.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).
:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the bridge captures real coordination value (shared evaluation infrastructure, unified policy voice) but also extracts brokerage rents from siloed communities and flattens victim risk profiles. Suppression (0.4) is moderate: the bridge suppresses alternative framings (siloed governance, pluralistic risk portfolios) through funding gatekeeping and policy legitimacy, but both siloed communities maintain independent institutions. Theater ratio (0.35) reflects genuine coordination function (shared benchmarks, incident reporting) mixed with performative 'integration' language in grant applications. Accessibility collapse (0.5) and resistance (0.5) are moderate: siloed alternatives persist but are marginalized in major funding and policy venues; both silo communities actively resist bridge absorption.
 *
 * PERSPECTIVAL GAP:
 *   From bridging institutions' seat, the constraint is a Rope: genuine coordination solving fragmentation. From present_marginalized_populations' seat, it is a Snare: their urgent harms are diluted by speculative long-termism. From future_humanity's seat, it is a Snare: existential prevention is diluted by presentist remediation. From existential_risk_community and near_term_harms_community seats, it is a Tangled Rope: they are coordinated into shared infrastructure but pay extraction via epistemic dilution. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging_institutions are structural beneficiaries (d near 0.15): they collect brokerage rents, control vocabulary, and have arbitrage-grade exit (can return to siloed prestige). Present_marginalized_populations and future_humanity are structural victims (d near 0.85): trapped, no exit, bear flattened risk profiles. Existential_risk_community and near_term_harms_community are constrained payers (d ~0.6): they participate but pay coherence costs and face constrained exit (institutional identity locked to their silo). AI_safety_researchers and ai_ethics_researchers are dual-role (beneficiary/payer, d ~0.45): gain funding access but pay legibility costs. Funding_agencies and policy_makers are agenda_setters with arbitrage/mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The bridge's founding problem (fragmentation preventing coordinated response to systems like LLMs) remains live — but the bridge's solution (unified frameworks) may have outlived its coordination function. If LLM governance now requires differentiated, not unified, approaches (e.g., distinct regulatory tracks for bias vs. alignment), the bridge becomes a piton: maintained theatrically by brokers whose careers depend on it. The mandatrophy question is whether the 5%/85% citation concentration signals healthy brokerage or capture. Current metrics suggest Tangled Rope (coordination + extraction), but theater_ratio rising and extraction accumulating could indicate drift toward Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_entanglement_empirical_status,
    'Are present harms and existential risks genuinely structurally entangled (shared technical roots, common governance levers), or is ''entanglement'' a framing convenience that enables bridge funding?',
    'Technical analysis: do mitigation techniques for bias, surveillance, and displacement share mathematical foundations with alignment/interpretability/robustness? Policy analysis: do regulatory levers (compute governance, model evals, incident reporting) genuinely serve both risk profiles, or do they trade off?',
    'If entanglement is empirically thin, the bridge''s coordination function is overstated and its extraction (flattening distinct risk profiles) is unjustified — reclassification toward Snare. If entanglement is robust, the coordination function is genuine and Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_entanglement_empirical_status, empirical, 'Whether the bridge''s core coordination premise is empirically grounded or a funding narrative.').

omega_variable(
    bridge_brokerage_capture,
    'Do bridging institutions (5% of papers, 85% of cross-field links) genuinely coordinate distributed collaboration, or do they capture brokerage rents by gatekeeping the only legitimate integration vocabulary?',
    'Network analysis of co-authorship, funding flows, and citation dynamics: are bridge papers cited because they synthesize, or because they control the integration vocabulary? Counterfactual: if bridge programs were defunded, would cross-field collaboration persist via distributed channels?',
    'If capture, the bridge''s beneficiary structure is concentrated extraction — reclassification toward Snare. If genuine brokerage, the coordination function is distributed and Tangled Rope holds. Structural fragility (handful of brokers) suggests capture risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridge_brokerage_capture, empirical, 'Whether bridge institutions are coordinators or gatekeepers.').

omega_variable(
    bridge_fragility_collapse_scenario,
    'If key bridging institutions or funders exit, does the constraint collapse (piton) or does distributed collaboration sustain it (rope)?',
    'Track bridge program funding continuity, broker mobility, and cross-field citation resilience after major bridge institution closures (e.g., if GovAI or CHAI lost core funding). Measure whether siloed communities develop independent integration channels.',
    'If collapse, the bridge was a fragile Tangled Rope maintained by few actors — reclassification toward Piton if theater persists. If resilience, the coordination function has distributed — reclassification toward Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bridge_fragility_collapse_scenario, empirical, 'Whether the bridge''s structural fragility is a transient startup condition or inherent to its brokerage model.').

omega_variable(
    victim_set_duality_coherence,
    'Can a single constraint coherently have two victim populations with opposite time horizons (immediate vs. civilizational) and opposite power positions (organized advocacy vs. no voice), or does this duality indicate two distinct constraints improperly merged?',
    'Decompose the bridge constraint into two parallel constraints: one governing present-harm resource allocation, one governing existential-risk resource allocation. Test whether each has distinct beneficiary/victim structures, extraction profiles, and enforcement mechanisms. If decomposition yields cleaner classification, the bridge is an ε-invariance violation.',
    'If duality is incoherent, the bridge_reading violates ε-invariance — it should be two constraint stories (per DP-001). This would be a kernel-reading decomposition parallel to the BGS spectral/ETH split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_duality_coherence, conceptual, 'Whether the bridge''s dual victim set is a single constraint or an ε-invariance violation requiring decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_tr_t16, ai_risk_governance_priority__bridge_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_be_t16, ai_risk_governance_priority__bridge_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_su_t16, ai_risk_governance_priority__bridge_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(ai_risk_governance_priority__bridge_reading_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.15).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This bridge_reading decomposes the ai_risk_governance_priority kernel alongside existential_risk_reading and near_term_harms_reading. The bridge_reading claims structural entanglement (moderate ε on both dimensions, dual victim set). The existential_risk_reading claims categorical priority for superintelligence prevention (high ε on present harms as distraction). The near_term_harms_reading claims categorical priority for present marginalized populations (high ε on existential risk as distraction). The three readings have mutually incompatible beneficiary/victim structures and extraction profiles — they are distinct constraints linked by the kernel, not one constraint with measurement variance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, powerless, 0.85).
constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, organized, 0.6).
constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, moderate, 0.45).
constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
