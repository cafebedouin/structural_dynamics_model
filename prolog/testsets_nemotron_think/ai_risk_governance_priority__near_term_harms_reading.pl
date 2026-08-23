% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Priority — Near-Term Harms Reading
 *   domain: technology_ethics/governance
 *
 * SUMMARY:
 *   This constraint story represents the 'near-term harms' reading of the
 *   contested kernel 'AI risk governance priority.' The reading asserts that
 *   governance resources, regulatory attention, and research funding should
 *   prioritize demonstrated present harms — algorithmic bias against
 *   marginalized groups, misinformation amplification, labor displacement
 *   from automation, and surveillance overreach — which disproportionately
 *   affect Global South populations and historically marginalized
 *   communities. The structural claim is that the dominant 'existential risk'
 *   framing functions as a constraint that extracts from present victims by
 *   diverting finite governance capacity toward speculative superintelligence
 *   scenarios, while the coordination function (need for AI governance
 *   frameworks) is real but captured. The constraint operates through funding
 *   allocation, regulatory agenda-setting, and narrative framing in policy
 *   venues.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.72).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Priority — Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology_ethics/governance").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '6c017e33-4fc9-4577-a6c4-1a093e61b51d').
narrative_ontology:cs_kernel_codification('6c017e33-4fc9-4577-a6c4-1a093e61b51d', distributed).
narrative_ontology:cs_authority_grounding('6c017e33-4fc9-4577-a6c4-1a093e61b51d', distributed).
narrative_ontology:cs_reading_relation('6c017e33-4fc9-4577-a6c4-1a093e61b51d', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c017e33-4fc9-4577-a6c4-1a093e61b51d', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('6c017e33-4fc9-4577-a6c4-1a093e61b51d', foundational, present_harms_have_moral_priority).
narrative_ontology:cs_axiom_status(present_harms_have_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('6c017e33-4fc9-4577-a6c4-1a093e61b51d', present_harms_have_moral_priority, empirically_contingent).
narrative_ontology:cs_axiom('6c017e33-4fc9-4577-a6c4-1a093e61b51d', foundational, xrisk_framing_diverts_resources_from_marginalized).
narrative_ontology:cs_axiom_status(xrisk_framing_diverts_resources_from_marginalized, holdable).
narrative_ontology:cs_axiom_grounding('6c017e33-4fc9-4577-a6c4-1a093e61b51d', xrisk_framing_diverts_resources_from_marginalized, empirically_contingent).
narrative_ontology:cs_reference_frame('6c017e33-4fc9-4577-a6c4-1a093e61b51d', present_harm_mitigation_framework).
narrative_ontology:cs_drift_state('6c017e33-4fc9-4577-a6c4-1a093e61b51d', current_ai_governance_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c017e33-4fc9-4577-a6c4-1a093e61b51d', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_safety_funding_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, civil_society_advocates).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, present_harm_evidence_based_prioritization).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, marginalized_voices_in_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major AI developers and deployers (Big Tech, frontier model companies) benefit from governance frameworks that prioritize speculative future risks over regulation of current deployments. The x-risk framing enables voluntary commitments, self-governance, and narrative capture of 'responsible AI' while deferring binding regulation on bias, labor displacement, and surveillance. They can arbitrage across jurisdictions and shape standard-setting bodies.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Researchers and institutions focused on existential AI risk (alignment, interpretability, control) receive concentrated philanthropic and governmental funding. Their career incentives, epistemic communities, and status structures are built around the x-risk framing. They can move between institutions but the field's incentive structure rewards x-risk prioritization.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, xrisk_research_community, beneficiary,
    organized, biographical, mobile, global).

% Philanthropic foundations (Open Philanthropy, Longview Philanthropy, etc.) and government AI safety institutes direct the majority of dedicated AI safety funding to x-risk research. Their grantmaking priorities, evaluation metrics, and institutional mandates encode the x-risk framing. They control resource allocation gates and can shift priorities but have not done so at scale.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_safety_funding_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Populations in the Global South bear disproportionate harms from deployed AI systems: algorithmic credit scoring excluding informal economies, content moderation erasing marginalized languages, surveillance exported from Global North, labor exploitation in data annotation. They cannot exit their geographic, economic, or identity position. Governance venues (OECD, GPAI, standard-setting bodies) structurally exclude them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, generational, identity_locked, global).

% Racial, gender, disability, and other marginalized groups face documented algorithmic discrimination in hiring, lending, policing, healthcare, and housing. The identity-lock is structural: one cannot exit being targeted by systems that encode historical bias. Governance resources for fairness audits, bias mitigation, and redress mechanisms are minimal compared to x-risk funding.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups_algorithmic_discrimination, payer,
    powerless, biographical, identity_locked, national).

% Workers in sectors facing AI-driven displacement (translation, illustration, coding, customer service, logistics) bear immediate economic harms. Exit is constrained: retraining is costly, labor markets are tightening, and algorithmic management extends into remaining jobs. Some organized labor resistance exists but is fragmented across jurisdictions and sectors.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation, payer,
    moderate, biographical, constrained, global).

% Digital rights organizations, algorithmic justice groups, labor unions, Global South coalitions expend resources advocating for present-harm prioritization in governance venues. They face asymmetric resource competition: industry lobbying budgets dwarf civil society capacity. Their exclusion from key standard-setting bodies (ISO/IEC JTC 1/SC 42, etc.) is structural.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_advocates, payer,
    organized, biographical, constrained, global).

% Legislators, regulators, and standard-setting bodies administer the governance agenda. They face competing pressures: industry lobbying and x-risk expert testimony vs. civil society evidence of present harms. Their incentive structures (re-election, regulatory capture, epistemic deference to 'technical experts') currently favor the x-risk framing. Some jurisdictions (EU, Brazil) show shift toward present-harm regulation.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, policy_makers_standard_setters, agenda_setter,
    institutional, biographical, mobile, national).

% Researchers documenting present harms (bias, fairness, accountability, transparency) produce the evidence base for this reading. They are structurally marginalized in funding and venue access compared to x-risk researchers. Their work validates the victim claims but does not capture the governance agenda. Some bridge to policy but face the same structural exclusion.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, academic_ai_ethics_researchers, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: AI governance requires coordinating research funding, regulatory standards, liability frameworks, and international norms across a rapidly evolving technology landscape. A prioritization mechanism is needed to allocate finite governance capacity.
% TRANSFER_FUNCTION: Moves governance attention, research funding, regulatory capacity, and policy legitimacy from present-harm mitigation (fairness audits, bias redress, worker protection, surveillance regulation) toward speculative superintelligence scenarios (alignment research, interpretability, control theory, long-term forecasting). The transfer is effected through philanthropic grantmaking, government AI safety institute mandates, standard-setting body agendas, and narrative framing in elite policy venues.
% ABSENT_VOICES: Global South governments and communities, directly affected workers in algorithmic management systems, marginalized communities subject to automated decision-making in welfare/policing/credit, and Global Majority AI researchers are structurally excluded from the venues where governance priorities are set (frontier AI safety summits, major philanthropic boardrooms, ISO standard-setting committees). They would object to the deprioritization of their demonstrated harms.
% DISAPPEARANCE_RATIONALE: If the near-term harms prioritization constraint vanished overnight, governance resources would not automatically flow to present harms — the x-risk framing would likely capture the vacuum. However, the explicit framing that 'present harms are the priority' currently functions as a countervailing claim in policy debates (EU AI Act, US AI Bill of Rights, Global South declarations). Its disappearance would remove a structural anchor for present-harm advocacy, enabling further capture. The world rearranges because the constraint currently sustains a contested but live alternative governance imaginary.
% FOUNDING_PROBLEM: The field of AI governance emerged from the recognition that AI systems produce real harms now — discriminatory outcomes, labor displacement, concentration of power, surveillance expansion — and that without deliberate governance, these harms would scale unchecked. The founding problem was how to govern AI in the public interest given asymmetric power between developers and affected populations.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early AI ethics literature (Noble 2018, Buolamwini & Gebru 2018, Crawford 2021), civil society declarations (Toronto Declaration 2018, Global South AI Governance Forum 2023), and regulatory preambles (EU AI Act Recitals, US Executive Order 14110). The x-risk community disputes that present harms are the founding problem, asserting the field was always about existential risk (Bostrom 2014, Yudkowsky 2008). No single corroborating source outside the beneficiary set resolves this — the contest is structural.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the x-risk framing captures the majority of dedicated AI safety funding and elite policy attention while present harms remain under-resourced relative to their demonstrated scale. Suppression (0.68) reflects structural barriers: marginalized populations lack representation in governance venues, Global South voices are excluded from standard-setting bodies, and workers have limited exit from algorithmic management systems. Theater ratio (0.42) captures the gap between public commitments to 'AI ethics' and actual resource allocation — ethics boards, fairness toolkits, and voluntary commitments exist but do not shift the funding/attention equilibrium. Accessibility collapse (0.74) is high because once algorithmic systems are deployed in hiring, lending, policing, and social services, opt-out is effectively impossible for affected populations. Resistance (0.55) is moderate and growing through regulatory action (EU AI Act, US executive orders), civil society litigation, and worker organizing, but remains fragmented across jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the technology company / x-risk research seat, the constraint appears as a genuine coordination problem (how to govern transformative AI) with reasonable prioritization of worst-case outcomes. From the victim seats, the same structure operates as extraction: their demonstrated harms are deprioritized while speculative scenarios capture resources. The engine computes this divergence from the structural data — the declared beneficiaries, victims, exit options, and power levels. The claimed type (tangled_rope) reflects the authoring seat's assessment that a real coordination function exists but is asymmetrically captured.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies are structural beneficiaries (d ≈ 0.15): the x-risk framing deflects regulation from current business models, captures the 'responsible AI' narrative, and secures favorable regulatory treatment through voluntary commitments. X-risk research community and funding institutions are beneficiaries (d ≈ 0.20): they receive concentrated funding and status from the prioritization. Global South populations, marginalized groups facing algorithmic discrimination, and displaced workers are targets (d ≈ 0.85-0.90): they bear the harms of deployed systems while governance resources flow elsewhere. Their exit options are identity_locked (marginalized groups cannot exit their identity-based targeting) or trapped (workers cannot exit labor markets). Civil society organizations and affected-community advocates are payers (d ≈ 0.75): they expend resources fighting for recognition within a system that structurally excludes them. Policy makers and standard-setting bodies are agenda_setters (d ≈ 0.40): they administer the constraint but face competing pressures from industry lobbying and civil society.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating governance for transformative AI systems) remains live, but the specific prioritization mechanism has drifted. The original mandate — preventing catastrophic outcomes — has been interpreted to privilege a narrow class of speculative scenarios over demonstrated harms. This is not pure mandatrophy (the problem hasn't disappeared) but a capture of the governance agenda. The constraint persists because the beneficiaries (companies, x-risk institutions) have the power to maintain the framing, while the victims lack the structural power to force re-prioritization. The theater ratio rise over the interval tracks the growth of 'AI safety' as a field that increasingly performs governance without addressing present harms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ''near_term_harms_reading'' a distinct constraint with its own stable ε, or does its ε depend on the coexistence with sibling readings?',
    'Test ε-invariance: if the existential_risk_reading and bridge_reading were removed from the discourse, would this reading''s extractiveness, beneficiary/victim structure, and classification remain stable? If ε shifts, the readings are not ε-invariant and the kernel decomposition needs revision.',
    'If ε is not invariant, the three readings are not structurally distinct constraints but observer-dependent framings of a single constraint — violating DP-001. This would require re-authoring as a single constraint with observer-axis variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading satisfies ε-invariance as a standalone constraint').

omega_variable(
    xrisk_framing_as_extraction_mechanism,
    'Does the existential risk framing functionally operate as an extraction mechanism (diverting resources from present harms), or is it a genuine independent priority that merely competes for limited resources?',
    'Counterfactual resource tracing: if x-risk funding were eliminated, would present-harm funding increase proportionally, or would the resources exit AI governance entirely? Structural analysis of funding pipelines and policy attention budgets.',
    'If functional extraction, the constraint is tangled_rope (coordination + asymmetric extraction). If mere competition, it may be rope with contested prioritization. Classification hinges on whether the x-risk framing actively suppresses present-harm alternatives or merely outcompetes them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(xrisk_framing_as_extraction_mechanism, empirical, 'Whether x-risk prioritization actively extracts from present-harm mitigation or merely competes').

omega_variable(
    marginalized_exit_structure,
    'Are the exit options for global_south_populations and marginalized_groups_algorithmic_discrimination accurately characterized as identity_locked, or does structural economic dependency create a trapped condition that is not identity-mediated?',
    'Field research on algorithmic system opt-out feasibility in Global South contexts: can populations practically avoid algorithmic credit scoring, gig work platforms, government service delivery systems? Distinguish identity-fusion from material dependency.',
    'If trapped rather than identity_locked, directionality derivation shifts d toward 1.0 (full target), increasing effective extraction for these seats. If identity_locked, the suppression persistence after formal exit is the key dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_exit_structure, empirical, 'Exit option characterization for primary victim groups').

omega_variable(
    bridge_reading_structural_role,
    'Does the bridge_reading function as a genuine third position, or does it operationally reinforce the existential_risk_reading by legitimizing its frame while conceding minimal resources to present harms?',
    'Analyze bridge_reading institutional homes (funding sources, conference venues, policy outputs): do they allocate >50% of resources to present-harm mitigation, or does the bridge frame serve as a pressure valve that preserves x-risk dominance?',
    'If bridge_reading reinforces x-risk framing, the reading_relations should be ''influences'' (near-term reading creates pressure that bridge reading absorbs) rather than ''coexists_with''. This changes the network topology of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bridge_reading_structural_role, empirical, 'Whether bridge_reading is structurally independent or a reinforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.15).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of kernel 'ai_risk_governance_priority'. The existential_risk_reading prioritizes superintelligence scenarios with victim set = all humanity (future generations), beneficiary = x-risk research community, low ε on present harms. The bridge_reading claims non-mutually-exclusive entanglement. This reading's ε is high on present harms because the standing arrangement under contest (current governance allocation) extracts from marginalized populations by deprioritizing their harms. The three readings have distinct ε values, beneficiary/victim structures, and resource flow mappings — they are structurally distinct constraints linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
