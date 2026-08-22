% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance Bridge Framework
 *   domain: technology/ethics/governance
 *
 * SUMMARY:
 *   The bridge reading in AI risk governance asserts that present harms
 *   (bias, displacement, surveillance) and existential risks
 *   (superintelligence, loss of control) are structurally entangled — solving
 *   either requires the same governance infrastructure, talent pipeline, and
 *   institutional capacity. This reading claims the 5% of papers that cite
 *   across both fields account for 85% of cross-field links, and that these
 *   'bridging institutions' (a handful of research orgs, funders, and policy
 *   brokers) are the actual coordination mechanism. The constraint extracts
 *   moderately from both victim populations — present marginalized groups
 *   whose immediate harms are deprioritized as 'not existential,' and future
 *   humanity whose stakes are invoked to justify centralized control — while
 *   routing resources to the bridging actors. The bridge is fragile: it
 *   depends on <10 key brokers who control funding, hiring, and narrative
 *   framing. If they defect, the field reverts to the two sibling readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.42).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.38).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance Bridge Framework").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "technology/ethics/governance").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '33ea4b1b-55ad-498b-92d5-856b0bdbd2a1').
narrative_ontology:cs_kernel_codification('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', distributed).
narrative_ontology:cs_authority_grounding('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', practice).
narrative_ontology:cs_interpretation_layer_present('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1').
narrative_ontology:cs_reading_relation('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', foundational, ai_risks_structurally_entangled).
narrative_ontology:cs_axiom_status(ai_risks_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', ai_risks_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', foundational, unified_governance_infrastructure_necessary).
narrative_ontology:cs_axiom_status(unified_governance_infrastructure_necessary, holdable).
narrative_ontology:cs_axiom_grounding('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', unified_governance_infrastructure_necessary, instrumental).
narrative_ontology:cs_reference_frame('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', fragmented_ai_governance_2018).
narrative_ontology:cs_drift_state('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', post_gpt4_policy_window, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33ea4b1b-55ad-498b-92d5-856b0bdbd2a1', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, policy_broker_actors).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity_stakeholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_researchers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, structural_entanglement_of_ai_risks).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, unified_governance_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A handful of research organizations (e.g., AI safety labs with ethics teams, dedicated bridge institutes) and funders (Open Philanthropy, NSF AI institutes, EU Horizon programs) that control the unified research agenda. They define what counts as 'bridging work,' allocate the majority of integrated funding, and set hiring/career norms. They can exit to either sibling reading at any time — their institutional identity is not fused to the bridge — but they capture the coordination surplus while the bridge holds.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary).

% Researchers who publish in both fields, hold joint appointments, or lead integrated projects. They gain cross-field legitimacy, access to bridge funding, and career capital from being 'bilingual.' But they pay in methodological compromise (satisfying two review cultures), broker dependency (their funding depends on <5 program officers), and identity risk (criticizing the bridge threatens their positioning). Exit to a single field is possible but costly — they lose the bridge premium.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_researchers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_researchers, payer).

% Policy entrepreneurs, congressional staffers, and international governance bodies (OECD, GPAI, UN advisory boards) who use the bridge framing as a decision heuristic: 'fund both, they're connected.' They benefit from a simplified narrative that justifies comprehensive AI legislation without choosing sides. They are not technically constrained by the bridge — they can pivot to either sibling reading as political winds shift — making them mobile rather than identity_locked.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_broker_actors, beneficiary,
    institutional, biographical, mobile, national).

% Communities experiencing algorithmic bias, labor displacement, surveillance, and misinformation now. Their harms are reframed as 'components of existential risk' or 'stepping stones to alignment,' which delays targeted remediation and redirects resources toward speculative long-term scenarios. They have no voice in the bridge governance structures, no exit from the AI systems harming them, and no leverage to demand prioritization. Their victimhood is structural: the bridge reading's resource allocation logic systematically deprioritizes immediate, legible harms in favor of speculative, high-leverage ones.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% The abstract referent of existential risk claims — 'humanity's long-term potential.' This non-agent entity is invoked to justify centralized governance, compute governance, and talent concentration in bridge institutions. Its victimhood is structural: the bridge reading uses its stakes to extract coordination surplus for present actors, while the actual reduction of existential risk remains empirically unvalidated. Exit is identity_locked in the sense that the frame 'we must protect the future' constitutes the identity of the longtermist field — abandoning the frame dissolves the field's rationale.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity_stakeholders, payer,
    powerless, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__bridge_reading, future_humanity_stakeholders).

% Researchers and advocates (e.g., MIRI-aligned, FHI alumni, longtermist funders) who hold the existential_risk_reading. They are excluded from the bridge's governance in the sense that the bridge reading dilutes their priority claim — 'existential risk is the only thing that matters' becomes 'existential risk is one of two entangled priorities.' They cannot exit the field (their identity and funding are fused to existential risk) but they resist the bridge's resource diversion toward near-term work. Their exclusion is partial: they still control significant independent funding and talent pipelines.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_advocates, excluded,
    organized, generational, constrained, global).

% Civil society organizations, algorithmic justice researchers, labor advocates, and affected-community representatives who hold the near_term_harms_reading. They are excluded from the bridge's governance because the bridge reframes their urgent, legible harms as 'subproblems' of a longer agenda. They resist the bridge's narrative capture but depend on some of the same funders (creating constrained exit). Their exclusion is more acute than the existential-risk advocates': they have less independent funding and their institutional bases (universities, NGOs) are more integrated into the bridge's career structures.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates, excluded,
    organized, biographical, constrained, global).

% Meta-researchers, philosophers of science, and governance analysts who study the field's dynamics without holding a reading. They see the full structure: the bridge's broker capture, the siblings' mutual exclusion, the dual victim extraction. They bear no cost and collect no rent from the constraint — their exit is analytical (they can change their analysis at any time).
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified research infrastructure, shared vocabulary, and joint funding mechanisms that allow AI safety and AI ethics communities to interoperate — shared benchmarks, cross-trained talent, common policy interfaces — instead of maintaining two parallel, non-communicating governance stacks.
% TRANSFER_FUNCTION: Moves funding, talent, and policy attention from both sibling fields toward bridge institutions and integrated programs. Present-harms work gets reframed as 'alignment-adjacent'; existential-risk work gets reframed as 'grounded in present systems.' The bridge institutions capture the coordination surplus (overhead, agenda-setting, broker fees).
% ABSENT_VOICES: Affected communities experiencing present harms (not their advocates, but the communities themselves) are structurally absent from bridge governance. Future generations (the literal referent of existential risk) are definitionally absent. Both are represented by advocates who are themselves embedded in the constraint's power structure.
% DISAPPEARANCE_RATIONALE: If the bridge framework vanished, funding would split back into two silos: longtermist funders would concentrate on speculative alignment; near-term funders would concentrate on bias/auditing/regulation. The shared infrastructure (benchmarks, talent pipelines, policy interfaces) would decay. Bridge institutions would lose their coordination rationale and either dissolve or pick a side. The field would revert to the two sibling readings with a contested boundary.
% FOUNDING_PROBLEM: By 2018–2020, AI governance had fragmented into two non-communicating fields: 'AI safety' (existential risk, theoretical, Bay Area/EA-funded) and 'AI ethics' (present harms, empirical, academic/civil-society-funded). Each field treated the other as confused or dangerous. Policymakers faced two mutually unintelligible expert communities. The bridge reading was built to solve this fragmentation — to create a unified field that could speak to power with one voice.
% FOUNDING_PROBLEM_CORROBORATION: The fragmentation narrative is attested by all three readings but with different valences. Bridge advocates (e.g., Center for AI Safety, Partnership on AI) attest the problem is live and the bridge is the solution. Existential-risk advocates (e.g., MIRI, some FHI alumni) attest the problem was manufactured — the fields were never fragmented, the ethics field was simply wrong about priorities. Near-term advocates (e.g., Algorithmic Justice League, Data & Society, ACM FAccT community) attest the fragmentation was real but the bridge is capture — the solution reproduces the power asymmetry. No source outside the three readings' benefiting parties corroborates a neutral account.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.42) reflects moderate but real extraction: the bridge reading commands disproportionate funding and policy attention relative to its empirical validation, and both victim populations concede ground (present-harms advocates accept longtermist framing; existential-risk advocates accept near-term guardrails as 'on the path'). Suppression (0.38) operates through funding gates and career incentives — researchers who reject the entanglement thesis struggle to place work or secure grants in either field. Theater ratio (0.52) is high and rising: the bridging function is increasingly performed through joint statements, shared conferences, and cross-citations rather than integrated technical work. The coordination function is real (unified governance infrastructure has genuine economies of scope) but the extraction is asymmetric: bridging actors capture the coordination surplus.
 *
 * PERSPECTIVAL GAP:
 *   The bridging institutions experience this as genuine coordination (rope-like from their seat: they built the infrastructure that connects the fields). The victim populations experience it as extraction with a coordination cover story (snare-adjacent from their seats: their specific concerns are subsumed into a unified agenda they did not choose). The engine computes this divergence from the structural data — the bridge reading's claimed_type (tangled_rope) captures the hybrid nature, but per-seat classification will differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions (agenda_setter/beneficiary) sit at d ≈ 0.15: they set the unified framework's terms and collect the coordination rents (funding, policy access, career capital). Integrated researchers (beneficiary/payer) sit at d ≈ 0.35: they gain cross-field legitimacy but pay in methodological compromise and broker dependency. Present marginalized populations (payer) sit at d ≈ 0.75: their immediate harms are reframed as 'stepping stones' to existential safety, delaying targeted remediation. Future humanity stakeholders (payer, identity_locked) sit at d ≈ 0.65: their risk is invoked to justify centralized governance that may not actually reduce their risk. Policy brokers (excluded from technical work but agenda-setting in policy) sit at d ≈ 0.45: they benefit from the bridge as a decision heuristic but bear reputational risk if the bridge collapses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented AI governance leaving gaps between near-term and long-term safety) is contested: present-harms advocates say the problem was manufactured to legitimize longtermist priorities; longtermists say the problem was always there but near-term work ignored it. The bridge reading persists because neither sibling can fully displace the other — the mandate has not atrophied but has been captured by the broker layer. Mandatrophy is unresolved: the coordination function is real but the broker capture is the constraint's current steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the bridge reading a genuine structural category or a rhetorical compromise between two entrenched readings?',
    'Citation network analysis of the 5% bridging papers: if they form a sustained research program with independent funding and career paths, the reading is structural; if they are predominantly conference-brokered survey pieces, it is a rhetorical compromise.',
    'If rhetorical compromise, the constraint is a scaffold propping up both sibling readings rather than an independent tangled_rope; ε would reflect maintenance cost of the compromise rather than coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the bridge reading instantiates a real coordination function or merely performs reconciliation.').

omega_variable(
    resource_flow_fragility,
    'Does the resource flow to integrated research depend on a handful of broker actors who could defect, or has it achieved distributed institutionalization?',
    'Track grant renewal patterns and hiring in integrated safety-ethics programs over 3–5 years: if >50% of funding flows through <5 principal investigators or program officers, the flow is broker-dependent.',
    'Broker-dependent flow means the constraint''s coordination function is structurally fragile — loss of brokers collapses the bridge, reverting the field to the two sibling readings. This would reclassify toward scaffold with implicit sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_flow_fragility, empirical, 'Structural fragility of the resource allocation mechanism sustaining the bridge.').

omega_variable(
    dual_victim_extraction_balance,
    'Does the unified framework extract symmetrically from both victim populations, or does one population bear disproportionate cost while the other''s risk is used as cover?',
    'Compare resource allocation within integrated programs: fraction of funding addressing present harms vs. existential scenarios, weighted by affected population size and urgency. Track policy outputs for differential implementation speed.',
    'Asymmetric extraction would reclassify one victim seat toward snare dynamics while the other remains coordinated — the engine computes this per-seat. The single ε value obscures this; the omega documents the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_victim_extraction_balance, empirical, 'Whether the constraint''s extraction falls unevenly across its two declared victim populations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding gates, publication norms, hiring criteria) or internalized (researchers self-censor to maintain bridge credibility)?',
    'Post-exit suppression trajectory: if researchers who leave bridge positions report continued self-censorship in new roles, the suppression has internalized. Compare suppression metrics in bridge vs. single-reading institutions.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the constraint travels with agents after exit, making identity_locked exit_options more accurate for bridge-positioned researchers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in bridge-positioned research careers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_bridge_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_risk_bridge_tr_t5, ai_risk_governance_priority__bridge_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ai_risk_bridge_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(ai_risk_bridge_tr_t15, ai_risk_governance_priority__bridge_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(ai_risk_bridge_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(ai_risk_bridge_tr_t25, ai_risk_governance_priority__bridge_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_risk_bridge_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_risk_bridge_be_t5, ai_risk_governance_priority__bridge_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(ai_risk_bridge_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ai_risk_bridge_be_t15, ai_risk_governance_priority__bridge_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(ai_risk_bridge_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(ai_risk_bridge_be_t25, ai_risk_governance_priority__bridge_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_bridge_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ai_risk_bridge_su_t5, ai_risk_governance_priority__bridge_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ai_risk_bridge_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(ai_risk_bridge_su_t15, ai_risk_governance_priority__bridge_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement(ai_risk_bridge_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(ai_risk_bridge_su_t25, ai_risk_governance_priority__bridge_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.08).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_safety_funding_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_policy_narrative_framing).

% DUAL FORMULATION NOTE:
% This constraint family (kernel ai_risk_governance_priority) decomposes the single label 'AI risk governance priority' into three structurally distinct constraints with different ε, victim sets, and beneficiary structures. The bridge reading claims entanglement (ε=0.42, dual victims); existential reading claims priority (ε≈0.55, future humanity victim); near-term reading claims priority (ε≈0.48, present marginalized victim). The bridge reading's lower ε reflects its coordination function; the siblings' higher ε reflects their exclusionary framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
