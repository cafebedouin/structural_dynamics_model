% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Existential Risk Governance Priority
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the 'existential_risk_reading' of the
 *   ai_risk_governance_priority kernel. It claims that AI governance must
 *   prioritize preventing superintelligence scenarios that could annihilate
 *   or permanently curtail humanity's potential. The reading establishes a
 *   victim set of all future humanity and beneficiaries including x-risk
 *   research institutions and AI labs that claim safety leadership. The
 *   constraint operates by directing governance resources, research funding,
 *   and policy attention toward alignment-as-control, adversarial testing,
 *   and AGI governance frameworks — with correspondingly less resource flow
 *   to present algorithmic bias, misinformation, labor displacement, and
 *   surveillance harms affecting marginalized populations now. The
 *   claimed_type is tangled_rope because the reading posits a genuine
 *   coordination function (solving the alignment problem for superintelligent
 *   systems) while simultaneously extracting from near-term harm mitigation
 *   communities through resource displacement and narrative marginalization.
 *   Active enforcement is required because the prioritization must be
 *   maintained against competing governance frames.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.45).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Existential Risk Governance Priority").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'bc342807-78e1-471e-831c-fbac88afd1e9').
narrative_ontology:cs_kernel_codification('bc342807-78e1-471e-831c-fbac88afd1e9', distributed).
narrative_ontology:cs_authority_grounding('bc342807-78e1-471e-831c-fbac88afd1e9', expertise).
narrative_ontology:cs_interpretation_layer_present('bc342807-78e1-471e-831c-fbac88afd1e9').
narrative_ontology:cs_reading_relation('bc342807-78e1-471e-831c-fbac88afd1e9', ai_risk_governance_priority__near_term_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('bc342807-78e1-471e-831c-fbac88afd1e9', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('bc342807-78e1-471e-831c-fbac88afd1e9', foundational, existential_risk_lexical_priority).
narrative_ontology:cs_axiom_status(existential_risk_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('bc342807-78e1-471e-831c-fbac88afd1e9', existential_risk_lexical_priority, deontological).
narrative_ontology:cs_axiom('bc342807-78e1-471e-831c-fbac88afd1e9', foundational, alignment_problem_requires_dedicated_resources).
narrative_ontology:cs_axiom_status(alignment_problem_requires_dedicated_resources, holdable).
narrative_ontology:cs_axiom_grounding('bc342807-78e1-471e-831c-fbac88afd1e9', alignment_problem_requires_dedicated_resources, empirically_contingent).
narrative_ontology:cs_reference_frame('bc342807-78e1-471e-831c-fbac88afd1e9', pre_agi_alignment_unsolved).
narrative_ontology:cs_drift_state('bc342807-78e1-471e-831c-fbac88afd1e9', post_gpt4_capabilities_surge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc342807-78e1-471e-831c-fbac88afd1e9', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, alignment_research_community).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harm_mitigation_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, marginalized_populations_affected_by_ai).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_ai_ethics_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_generation_representatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive concentrated funding, talent, and policy access from the existential risk governance frame. They define the research agenda, set evaluation benchmarks, and staff governance bodies. Their exit options are strong — they can pivot to other research domains or take industry roles. They benefit from the constraint's prioritization of speculative capabilities research.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Set the practical agenda for 'AI safety' through voluntary commitments, safety frameworks, and adversarial testing programs. They gain regulatory goodwill, recruitment advantages, and narrative control that insulates capabilities acceleration from stricter oversight. The constraint's enforcement (governance frameworks) is calibrated through their participation to not impede core business. They capture both agenda-setting and benefit extraction.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary).

% A research field defined by the existential risk frame. They receive funding and status from the constraint's prioritization but face constrained exit — their expertise is specialized to the alignment-as-control paradigm. Career capital is tied to the reading's continued dominance. They genuinely believe in the coordination function but their institutional position depends on the constraint's persistence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, alignment_research_community, beneficiary,
    organized, biographical, constrained, global).

% Communities working on algorithmic bias, misinformation, labor displacement, and surveillance harms. They experience resource displacement as funding and policy attention flow to existential risk frameworks. Their exit is constrained — their work addresses harms happening now, and they cannot pivot to speculative research without abandoning affected populations. They resist the prioritization through coalition-building and epistemic challenges.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harm_mitigation_communities, payer,
    organized, biographical, constrained, national).

% Bear the brunt of present AI harms (discriminatory systems, surveillance, displacement) while governance resources are directed toward speculative future risks. They have no meaningful exit from the systems harming them and no voice in governance prioritization. Their victimization is structural: the constraint's resource allocation treats their harms as lower priority. They are the 'all future humanity' victim set's present-day proxy — but the reading's frame renders them invisible.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, marginalized_populations_affected_by_ai, payer,
    powerless, immediate, trapped, global).

% Researchers whose work addresses present harms. They face funding diversion, citation displacement, and marginalization in governance forums. Exit is constrained: pivoting to alignment research requires retraining and abandons their domain expertise. They contest the reading's epistemology and resource allocation but lack the institutional power of x-risk institutions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_ai_ethics_researchers, payer,
    moderate, biographical, constrained, global).

% Proxy stakeholder for the declared victim group 'all future humanity.' No present-day agents occupy this seat — it is a structural placeholder for the reading's intergenerational claim. If instantiated (e.g., through legal standing for future generations, longtermist institutions), they would have zero exit options and absolute dependence on the constraint's coordination function succeeding. Their inclusion tests whether intergenerational victims can register in the directionality machinery.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_generation_representatives, payer,
    powerless, civilizational, trapped, universal).

% Allocate governance resources, set regulatory agendas, and adjudicate between competing risk frames. They observe the constraint's operation from the analytical seat — their decisions determine whether the prioritization becomes binding policy or remains one contested frame among others. They have analytical exit (can change frameworks) but their choices have structural consequences for all other seats.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, competition_authorities_and_policymakers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the alignment problem for superintelligent AI systems: ensures that systems vastly more capable than humans pursue goals compatible with human survival and flourishing, preventing loss of control scenarios.
% TRANSFER_FUNCTION: Moves research funding, talent, policy attention, and regulatory capacity from present-harm mitigation (algorithmic bias, misinformation, labor displacement, surveillance) toward alignment research, adversarial testing, and AGI governance frameworks. The transfer is from near-term harm communities to x-risk institutions and safety-claiming labs.
% ABSENT_VOICES: Marginalized populations currently harmed by AI systems are structurally excluded from the governance prioritization process — their harms are acknowledged but deferred. Global South AI ethics voices are absent from the x-risk research community's epistemic center. Future generations have no representatives. The bridge_reading's integrative frame is excluded by this reading's foreclosure of equal prioritization.
% DISAPPEARANCE_RATIONALE: If the existential risk prioritization vanished overnight, governance resources would redistribute toward present-harm mitigation, AI labs would lose safety-leadership regulatory cover, x-risk institutions would lose their primary funding justification, and the alignment research field would fragment. The global AI governance architecture would reorganize around demonstrated harms.
% FOUNDING_PROBLEM: The alignment problem: as AI systems approach and exceed human capabilities across domains, we lack technical methods to ensure they pursue goals compatible with human values. Without a solution, superintelligent systems could cause human extinction or permanent curtailment of potential. This problem was identified in early AI safety literature (Yudkowsky, Bostrom) and gained institutional traction after 2015.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the x-risk research community (beneficiaries) AND by independent AI researchers outside the beneficiary set who acknowledge alignment as an open technical challenge (e.g., Russell, Bengio, Hinton). However, the STATUS of the problem as 'requiring prioritization over present harms' is contested: near-term harm communities and bridge_reading proponents attest the problem is live but dispute the prioritization claim. No neutral arbiter exists — the status is structurally contested.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the reading's structural displacement of present-harm mitigation: funding, talent, and policy bandwidth flow to speculative capabilities research while demonstrated harms to existing populations receive less. The ε referent is the standing arrangement of AI governance prioritization as this reading sees it — not the reading's endorsed alternative. Suppression (0.45) is moderate because the constraint operates through agenda-setting and resource allocation rather than direct coercion, but the marginalization of near-term harm voices in governance forums constitutes real structural suppression. Theater ratio (0.32) captures the gap between the reading's stated coordination goal (preventing existential catastrophe) and the growing performative infrastructure of 'AI safety' branding by labs that simultaneously accelerate capabilities. Accessibility collapse (0.42) and resistance (0.58) reflect that alternative governance frames (near-term harms, bridge) remain live and contested — the constraint has not naturalized.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk research seat, the constraint is genuine coordination against a civilization-scale threat — the alignment problem is the central technical challenge. From the present-harm mitigation seat, the same structure operates as extraction: their demonstrated harms are deferred, their communities under-resourced, their epistemic authority displaced by a speculative frame. The AI lab seat experiences both: it gains legitimacy and regulatory cover from safety leadership claims while the constraint's enforcement (adversarial testing, governance frameworks) is calibrated to not impede capabilities progress. The engine will compute per-seat types from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: x_risk_research_institutions (collect funding, talent, policy access), ai_labs_claiming_safety_leadership (gain regulatory goodwill, recruitment advantage, narrative control), alignment_research_community (field definition, resource concentration). Victims: present_harm_mitigation_communities (bear resource displacement, epistemic marginalization), marginalized_populations_affected_by_ai (harms deferred as 'less important' than speculative risks), near_term_ai_ethics_researchers (field displacement, funding diversion). Future humanity is declared as a victim group but has no present-day agents — this is an omega variable. The agenda_setter is the x-risk research community plus allied policy actors; they set the governance frame. Directionality flows from present-harm seats toward the x-risk frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (preventing AI catastrophe) is live and escalating — capabilities advance while alignment remains unsolved. But the constraint risks mandatrophy if the coordination function (solving alignment) atrophies into a permanent governance frame that extracts from present-harm mitigation without delivering existential risk reduction. The theater ratio trajectory suggests this drift is underway. The constraint is tangled_rope, not snare, because the coordination function is genuine and the reading's proponents believe in it — but the extraction from near-term victims is structural and growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ai_risk_governance_priority kernel, and how does it structurally differ from near_term_harms_reading and bridge_reading?',
    'Comparative constraint decomposition: each reading must instantiate its own ε, beneficiary/victim structure, and coordination/transfer functions. The kernel_id and reading_id are authoritative for this reading; sibling readings are separate constraint files.',
    'If the readings cannot be cleanly decomposed into structurally distinct constraints with stable ε values, the kernel itself is underspecified and the contest is semantic, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Validates the ε-invariance decomposition of the AI risk governance kernel into three readings').

omega_variable(
    speculative_vs_present_extraction_balance,
    'Does the high ε on speculative capabilities and low ε on present algorithmic bias reflect the constraint''s actual operation, or does it encode the reading''s prioritization as a structural fact?',
    'Track resource allocation in AI governance funding, policy attention, and research output over time. Compare the share flowing to existential risk frameworks vs. present harm mitigation.',
    'If present-harm mitigation is structurally suppressed (suppression rising) rather than merely under-resourced, the constraint operates as a snare for near-term victims. If both receive sustained investment, the bridge_reading''s claim of non-mutual-exclusion is structurally vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_vs_present_extraction_balance, empirical, 'Whether the extraction profile reflects genuine capability uncertainty or reading-constituted prioritization').

omega_variable(
    future_humanity_victim_operationalization,
    'Can ''all future humanity'' function as a victim group in the engine''s directionality derivation, or does the absence of present-day representatives make the victim declaration analytically inert?',
    'The engine derives directionality from beneficiary/victim declarations + power + exit. A victim group with no present-day agents, no exit options, and no resistance capacity may not register in the computation. Test by adding a proxy stakeholder (e.g., ''future_generation_representatives'') with appropriate exit/power.',
    'If the victim declaration does not drive χ amplification for any seat, the constraint''s extractive structure is invisible to the classifier — the reading''s core moral claim has no structural purchase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_victim_operationalization, conceptual, 'Whether intergenerational victim declarations can operate in the directionality machinery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_existential_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_risk_existential_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_risk_existential_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(ai_risk_existential_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(ai_risk_existential_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(ai_risk_existential_tr_t25, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 25, 0.32).

% Extraction over time
narrative_ontology:measurement(ai_risk_existential_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_risk_existential_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ai_risk_existential_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ai_risk_existential_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(ai_risk_existential_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(ai_risk_existential_be_t25, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_existential_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_risk_existential_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(ai_risk_existential_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(ai_risk_existential_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(ai_risk_existential_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(ai_risk_existential_su_t25, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 25, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_capabilities_acceleration_governance).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_alignment_research_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint family (three readings of the ai_risk_governance_priority kernel) demonstrates ε-invariance decomposition: each reading has a distinct ε, beneficiary/victim structure, and coordination/transfer function. The existential_risk_reading has high ε on speculative capabilities; the near_term_harms_reading has high ε on present algorithmic bias; the bridge_reading claims a unified framework with moderate ε on both. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, organized, 0.65).
constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
