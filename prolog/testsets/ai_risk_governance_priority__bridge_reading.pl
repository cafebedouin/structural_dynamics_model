% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI Risk Governance Framework (Present Harms + Existential Risks)
 *   domain: governance/technology/ethics
 *
 * SUMMARY:
 *   AI risk governance faces a structural choice about how to organize
 *   attention and resources between two legitimate concerns: present harms
 *   affecting marginalized populations (algorithmic bias, misinformation,
 *   labor displacement, surveillance) and existential risks from advanced AI
 *   systems. The bridge reading claims these are non-mutually-exclusive,
 *   structurally entangled concerns requiring unified frameworks. This
 *   reading positions itself as a via media between specialized silos, but
 *   the unification comes at a structural cost: resources concentrate in
 *   bridging institutions, present-harm urgency is diluted by long-term
 *   framing, and both victim sets (present and future) have their advocates
 *   filtered through frame-brokering gatekeepers. This is a tangled rope: it
 *   provides genuine coordination by forcing the two communities to engage,
 *   but it extracts resources and authority from specialized advocates and
 *   concentrates power in the thin layer of researchers producing 85% of
 *   bridge citations.
 *
 * KEY AGENTS:
 *   - Bridging research institutions: the 5% of published work that accounts for 85% of cross-field links; they set the integrated agenda and benefit from positioning as essential interpreters.
 *   - Existential-risk researchers: powerful institutional actors with arbitrage exit (specialized funding); benefit from co-equal status within unified frameworks.
 *   - Present-harms researchers: moderate-power institutional actors with constrained exit; pay the cost of integration without guaranteed voice.
 *   - Marginalized populations (present harms): powerless, trapped victims whose interests depend on urgent near-term focus.
 *   - Future humanity: invoked but unrepresented; interests presumed by existential-risk researchers but never directly advocated.
 *   - Funding agencies: agenda-setters managing dual-objective portfolios; benefit from political legitimacy of addressing both camps.
 *   - Siloed risk communities: excluded from unified-framework authority but retain arbitrage exit through specialized funding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.52).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.48).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance Framework (Present Harms + Existential Risks)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "governance/technology/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'a652bca2-5c74-4703-980e-49508302549c').
narrative_ontology:cs_kernel_codification('a652bca2-5c74-4703-980e-49508302549c', distributed).
narrative_ontology:cs_authority_grounding('a652bca2-5c74-4703-980e-49508302549c', distributed).
narrative_ontology:cs_reading_relation('a652bca2-5c74-4703-980e-49508302549c', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a652bca2-5c74-4703-980e-49508302549c', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('a652bca2-5c74-4703-980e-49508302549c', foundational, present_and_existential_entanglement).
narrative_ontology:cs_axiom_status(present_and_existential_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('a652bca2-5c74-4703-980e-49508302549c', present_and_existential_entanglement, instrumental).
narrative_ontology:cs_axiom('a652bca2-5c74-4703-980e-49508302549c', foundational, unified_governance_necessity).
narrative_ontology:cs_axiom_status(unified_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a652bca2-5c74-4703-980e-49508302549c', unified_governance_necessity, conventional).
narrative_ontology:cs_reference_frame('a652bca2-5c74-4703-980e-49508302549c', siloized_governance_inadequacy).
narrative_ontology:cs_drift_state('a652bca2-5c74-4703-980e-49508302549c', contemporary_institutional_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a652bca2-5c74-4703-980e-49508302549c', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations_present_harms).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_harms_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research organizations and funding bodies that broker between present-harms and existential-risk communities (approximately 5% of published work accounting for 85% of cross-field citations). They set the integrated framework agenda, determine which problems receive joint funding, and decide how research agendas across the two camps are coordinated. They benefit from positioning themselves as essential interpreters of the unified field.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_research_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Researchers focused on long-term AI safety, superintelligence scenarios, and tail-risk prevention. Within a unified framework, their concerns are elevated to co-equal status with present-harms work, securing resources and legitimacy that would otherwise concentrate on nearer-term issues. They can exit to specialized existential-risk funding if the unified framework collapses, reducing their dependence on governance authority.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_researchers, beneficiary,
    powerful, civilizational, arbitrage, global).

% Researchers and advocates focused on algorithmic bias, misinformation, labor displacement, and surveillance harms affecting marginalized populations today. Under a unified framework, their work is folded into a broader agenda where present harms must compete with existential-risk framing for resources and priority-setting authority. They bear the cost of integration without guaranteed proportional voice in framework governance.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_harms_researchers, payer,
    moderate, biographical, constrained, global).

% Communities suffering present harms: algorithmic discrimination in hiring, lending, and criminal justice; misinformation targeting diasporas and minorities; labor displacement without retraining; surveillance capitalism targeting the economically vulnerable. Their interests depend on present-focused governance producing immediate remedies. A unified framework that treats their harms as one priority among many can dilute urgency and resources.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations_present_harms, payer,
    powerless, biographical, trapped, global).

% Unrepresented parties whose existential stakes are invoked in the framework but who have no seat in governance. Their interests are presumed by existential-risk researchers but never directly advocated by themselves. A unified framework that moderates existential-risk prioritization in order to balance present harms could reduce resources dedicated to their protection.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Government, foundation, and corporate funding bodies that allocate research resources. Under a unified framework, they are pressured to fund integrated projects and to use bridging criteria to assess relevance and impact. They benefit from reduced political friction (appearing to address both camps) but bear the operational cost of managing dual-objective research portfolios.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, funding_agencies, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, funding_agencies, observer).

% The specialized existential-risk and present-harms communities that would prefer to maintain separate governance, funding, and research agendas. They are excluded from the unified framework's priority-setting authority by its structural commitment to integration; alternative specialized funding allows them partial exit but at the cost of reduced institutional legitimacy and cross-disciplinary influence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, siloed_risk_communities, excluded,
    powerful, civilizational, arbitrage, global).

% AI development companies operating under governance frameworks. They observe and respond to unified-risk governance but are not direct stakeholders in the framework-setting process. They benefit from unified frameworks that distribute accountability across multiple risk dimensions, making any single focus area less demanding.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, technology_companies, observer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies governance attention on two legitimate risk domains (present harms and existential risks) that historically competed for resources and authority, creating a shared epistemic framework and funding mechanisms that require both communities to engage with each other's concerns.
% TRANSFER_FUNCTION: Moves research resources, policy authority, and institutional legitimacy from specialized silos into integrated research and governance bodies controlled by bridging institutions. Researchers in both camps contribute to a unified agenda but transfer priority-setting authority to frame-brokering entities.
% ABSENT_VOICES: Marginalized populations bearing present harms have no direct voice in unified-framework governance and are represented only through present-harms researchers, whose influence is diluted within a dual-objective structure. Future humans are invoked but structurally cannot advocate. Specialized communities (existential-risk or present-harms purists) are excluded from the unified agenda and can only influence it through exit to alternative funding.
% DISAPPEARANCE_RATIONALE: If the unified framework requirement vanished, governance would immediately decompose into two specialized systems: dedicated existential-risk prevention mechanisms and dedicated present-harms mitigation mechanisms, each with distinct funding, authority structures, and accountability measures. Research portfolios would bifurcate; policy authority would split.
% FOUNDING_PROBLEM: AI governance in the 2020s treated present harms and existential risks as separate, competing concerns managed by disconnected communities with different time horizons, funding sources, and institutional homes. This siloization meant governance attention was fragmented, policy solutions addressed only one dimension, and researchers working on one risk domain had little incentive to understand the other.
% FOUNDING_PROBLEM_CORROBORATION: Bridging researchers and unified-framework advocates attest the founding problem is still live, citing continued silo dynamics in funding and policy. Specialized existential-risk researchers contest this, arguing their work has always engaged with present-harms considerations; present-harms researchers attest siloization persists and that 'unified' frameworks often subordinate immediate concerns to long-term speculation. Independent analyses of funding flows and publication networks (outside advocacy organizations) confirm continued specialization despite growing bridge literature.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the unified framework provides genuine coordination benefit (both communities engage) but redistributes resources toward bridging institutions at the expense of specialized communities and present-harm urgency. Suppression (0.48) is moderate because the framework does not rely on coercive suppression of alternatives (both communities retain funding options) but does suppress the authority of non-bridging voices within governance structures. Theater ratio (0.41) is moderate-high: the integration narrative is partially performative—it allows bridging institutions to claim balanced stewardship while concentrating decision-making authority in their hands. Accessibility collapse (0.58) is moderate because alternatives (specialized governance) remain available through arbitrage exit but are politically costly; resistance (0.62) is substantial because both specialized communities actively resist the unified framework's claim to represent their interests. The temporal series show extractiveness rising from 0.38 to 0.52 as the unified framework becomes institutionalized (more decisions flow through bridging bodies), theater ratio peaking at 0.42 (maximum performative labor to justify bridge authority), then suppression stabilizing (the framework's governance structure has settled). This is a constraint that grows more extractive as it becomes standard practice.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (bridging institutions, funding agencies), the unified framework is essential coordination that forces necessary engagement between compartmentalized fields. From the payer seats (present-harms researchers, marginalized populations), the same framework is a mechanism for diluting urgent near-term concerns and subordinating present suffering to speculative long-term scenarios. From existential-risk researcher seats, the framework provides legitimacy and co-equal resource status. The engine computes these divergences from the structural data: constrained-exit actors (present-harms researchers, populations) sit further toward the target end of directionality; arbitrage-exit actors (existential-risk researchers, specialized funding access) sit closer to beneficiary. Bridging institutions with mobile exit and agenda-setting authority sit in the beneficiary zone.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions derive low d (near 0.2) because they set the agenda, benefit from the framework, and have mobile exit. Existential-risk researchers derive moderate-low d (near 0.35) because they benefit from co-equal status and have arbitrage exit, but must engage with present-harms framing in a framework they do not fully control. Present-harms researchers derive moderate-high d (near 0.65) because they bear the cost of diluted urgency and constrained exit—they cannot exit the unified frame without ceding governance authority to bridge institutions. Marginalized populations derive very high d (near 0.85) because they are trapped and their interests are funneled through institutional filters; future humanity also derives very high d as the most structurally distant and unrepresented set. No directionality overrides are necessary; the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (siloized governance) is contested—specialized communities dispute whether unification is necessary or whether silos enable more focused work. The founding-problem status (contested) plus the disappearance verdict (world rearranges) signals potential mandatrophy: the constraint could persist as theater (unified frameworks adopted for legitimacy while specialized communities maintain functional silos). The theater ratio's rise to 0.42 and stabilization suggests increasing performance without corresponding functional integration. An omega documents whether the constraint's persistence reflects genuine coordination necessity or institutional inertia and bureaucratic theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridge_necessity_vs_theater,
    'Does the unified framework genuinely enable coordination between siloed communities that would not otherwise engage, or does it primarily function as theater that allows bridging institutions to concentrate authority while specialized communities maintain functional silos?',
    'Longitudinal analysis of research collaboration patterns, funding flows, and joint publication rates before and after unified-framework adoption. Measurement of whether researchers from existential-risk and present-harms communities actually co-author, co-fund, or integrate their findings, or whether collaboration remains surface-level.',
    'If genuine coordination, the constraint is tangled_rope (real coordination with extraction). If primarily theater, the constraint reclassifies toward snare (extraction disguised as coordination). The theater_ratio trajectory is already suggestive—if it continues rising while collaboration metrics remain flat, theater is the dominant function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridge_necessity_vs_theater, empirical, 'Whether unified frameworks enable authentic inter-community engagement or function as legitimacy theater.').

omega_variable(
    present_harm_marginaliz_mechanism,
    'Is present-harms advocacy subordinated within unified frameworks by structural design (long-term framing dominates resource allocation) or by coincidental power dynamics (existential-risk researchers happen to have stronger institutional positions)?',
    'Comparative analysis of unified vs. specialized governance structures: do unified frameworks systematically allocate smaller fractions of resources to present-harms research than specialized present-harms governance bodies do? Are decision-making timescales in unified bodies structurally skewed toward long-term horizons?',
    'If subordination is structural design, the constraint is more extractive than authored metrics suggest (suppression is higher). If coincidental, the constraint''s extractiveness reflects real power dynamics that could shift without framework change. This affects whether fixing requires framework decomposition or just resource reallocation within the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harm_marginaliz_mechanism, empirical, 'Whether present-harms marginalization is built into unified frameworks or is a side effect of power imbalance.').

omega_variable(
    bridge_institution_fragility,
    'Is the unified framework dependent on a small number of bridging researchers and institutions (the 5% producing 85% of cross-field links), and would the framework collapse if those bridging actors lost funding, attention, or institutional position?',
    'Network analysis of citation and collaboration patterns: measurement of the degree distribution of bridge nodes. If bridge institutions are a narrow bottleneck, the system is fragile; if bridge function is distributed across many institutions, the system is robust. Scenario modeling of what happens if a single major bridging institution (e.g., a leading safety-ethics lab) shifts focus or loses funding.',
    'High fragility would mean the unified framework is a brittle coordination mechanism vulnerable to institutional disruption. Low fragility would suggest the framework has achieved distributed legitimacy. Fragility also affects whether the constraint should be reclassified as having higher suppression (must actively maintain the bridge layer to prevent decomposition into silos).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bridge_institution_fragility, empirical, 'Whether unified-framework governance depends on a thin layer of bridging institutions.').

omega_variable(
    sibling_reading_contention,
    'Do the existential_risk_reading and near_term_harms_reading truly coexist as live positions held by different parties, or has one reading been substantially delegitimized within governance institutions?',
    'Institutional audit: which reading(s) receive proportional funding from major governance bodies? Which reading(s) are represented in policy-making positions? Discourse analysis of governance documents: are both readings explicitly named and treated as legitimate, or is one framed as the ''serious'' position and the other as a distraction?',
    'If both readings remain genuinely live, the bridge_reading coexists_with both siblings. If one has been effectively foreclosed, the structural relationship shifts from coexistence to hierarchy, and the bridge_reading''s claim to unify becomes more like an assertion of dominance over a defeated alternative. This affects whether the constraint should include an omega about reading foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contention, empirical, 'Whether sibling readings remain equally legitimate within governance or one has been delegitimized.').

omega_variable(
    future_representation_mechanism,
    'How are the interests of future humanity represented within unified-framework governance, and are those representations constrained by the interests of present-harms advocates or existential-risk researchers?',
    'Governance audit: which stakeholders explicitly claim to represent future humanity? Are their framings independent of present constituencies, or are they absorbed into one reading''s strategic narrative? Longitudinal analysis: do future-focused arguments shift when they are articulated by different constituencies?',
    'If future humanity is represented by independent advocates with structural authority, the framework may authentically coordinate across temporal scales. If future interests are colonized by one reading''s narrative, the constraint functions as one reading (existential-risk or bridging) using future stakes as a legitimacy tool. This affects suppression measurement: if future interests are structurally unavoidable to represent, suppression is lower; if they can be narrated away, suppression is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_representation_mechanism, conceptual, 'Whether future humanity has autonomous representation or is absorbed into present constituencies'' strategic narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__bridge_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(ai_r_tr_t5, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__bridge_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_r_tr_t15, observed).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ai_r_tr_t20, observed).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__bridge_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(ai_r_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__bridge_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(ai_r_be_t5, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__bridge_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement_basis(ai_r_be_t15, observed).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(ai_r_be_t20, observed).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__bridge_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(ai_r_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__bridge_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(ai_r_su_t5, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__bridge_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(ai_r_su_t15, observed).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(ai_r_su_t20, observed).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__bridge_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(ai_r_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel ai_risk_governance_priority. The kernel concerns what priority-setting mechanism should govern AI risk governance. The bridge_reading claims present harms and existential risks are structurally entangled and require unified frameworks (ε ≈ 0.52, moderate extraction concentrated in bridging institutions). The existential_risk_reading claims existential risks must be the principal governance concern (expected ε higher, asymmetric victim set favoring future humanity). The near_term_harms_reading claims present harms must be the principal concern (expected ε lower if no extraction, or concentrated differently if bridging is subverted). These are three distinct constraints with different beneficiary/victim structures, not one constraint measured from three angles. They affect each other: the bridge_reading's institutional success constrains the authority of the other two; the other readings' legitimacy and resource availability depend partly on whether unified frameworks become mandatory. All three members of the kernel family should link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
