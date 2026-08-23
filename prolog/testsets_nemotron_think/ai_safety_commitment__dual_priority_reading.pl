% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety Dual-Priority Commitment (Non-Competing Priorities Reading)
 *   domain: technology_governance/ai_safety/risk_assessment
 *
 * SUMMARY:
 *   The 'AI safety requires addressing both existential risk and near-term
 *   harms as non-competing priorities' constraint is one reading of the
 *   contested ai_safety_commitment kernel. It emerged ca. 2018-2020 as a
 *   strategic unification of a fragmented field. The reading claims
 *   complementarity: work on near-term harms builds capacity for x-risk work
 *   (and vice versa), so the priorities don't compete. But under fixed
 *   budgets and talent pools, the claim faces a coherence challenge —
 *   resources allocated to one domain are not available to the other. The
 *   constraint extracts from both advocate populations by demanding they
 *   accept a framing that may not secure adequate resources for their
 *   specific priority, while benefiting the institutional field and funders
 *   who gain a unified, legible portfolio. The engine will compute per-seat
 *   classifications from the structural data: the institutional field
 *   (agenda_setter, arbitrage exit) should compute toward rope/coordination;
 *   both advocate populations (payer, identity_locked exit) should compute
 *   toward snare/tangled_rope extraction; trapped populations (future
 *   generations, marginalized communities) should compute toward snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.42).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.35).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety Dual-Priority Commitment (Non-Competing Priorities Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technology_governance/ai_safety/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'ccadb102-f4e2-4130-9db5-15ac1a6b7880').
narrative_ontology:cs_kernel_codification('ccadb102-f4e2-4130-9db5-15ac1a6b7880', distributed).
narrative_ontology:cs_authority_grounding('ccadb102-f4e2-4130-9db5-15ac1a6b7880', distributed).
narrative_ontology:cs_reading_relation('ccadb102-f4e2-4130-9db5-15ac1a6b7880', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccadb102-f4e2-4130-9db5-15ac1a6b7880', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('ccadb102-f4e2-4130-9db5-15ac1a6b7880', foundational, existential_and_near_term_harms_are_complementary).
narrative_ontology:cs_axiom_status(existential_and_near_term_harms_are_complementary, holdable).
narrative_ontology:cs_axiom_grounding('ccadb102-f4e2-4130-9db5-15ac1a6b7880', existential_and_near_term_harms_are_complementary, instrumental).
narrative_ontology:cs_axiom('ccadb102-f4e2-4130-9db5-15ac1a6b7880', secondary, unified_ai_safety_field_produces_greater_total_safety).
narrative_ontology:cs_axiom_status(unified_ai_safety_field_produces_greater_total_safety, holdable).
narrative_ontology:cs_axiom_grounding('ccadb102-f4e2-4130-9db5-15ac1a6b7880', unified_ai_safety_field_produces_greater_total_safety, empirically_contingent).
narrative_ontology:cs_reference_frame('ccadb102-f4e2-4130-9db5-15ac1a6b7880', unified_ai_safety_field_2018).
narrative_ontology:cs_drift_state('ccadb102-f4e2-4130-9db5-15ac1a6b7880', post_chatgpt_scaling_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ccadb102-f4e2-4130-9db5-15ac1a6b7880', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_institutional_field).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, unified_funding_narrative_beneficiaries).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, cross_cutting_research_agendas).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, future_generations_stakeholders).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, currently_marginalized_communities).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, ai_safety_field_unity_proposition).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, complementarity_of_risk_interventions_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research institutes, labs, and professional bodies that define the AI safety field's boundaries and funding priorities. They benefit from a unified field narrative that attracts broader funding and policy attention. They set the agenda by controlling conference tracks, journal scopes, and hiring norms. Exit is easy for individual researchers but institutionally costly — leaving the field means losing the 'AI safety' brand and associated resources.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_institutional_field, agenda_setter,
    institutional, generational, arbitrage, global).

% Funders (philanthropies, government agencies, corporate labs) who prefer a single coherent 'AI safety' portfolio over fragmented x-risk and near-term harm programs. The dual-priority framing simplifies grant-making, congressional testimony, and public communication. They can exit by rebranding portfolios, but the unified narrative reduces their transaction costs.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, unified_funding_narrative_beneficiaries, beneficiary,
    organized, biographical, mobile, global).

% Research programs claiming to address both risk categories simultaneously (e.g., interpretability, robustness, governance frameworks). They receive funding and legitimacy from the unified framing. Their exit is constrained because their research identity is built on the bridge — abandoning the dual-priority claim undermines their distinct value proposition.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, cross_cutting_research_agendas, beneficiary,
    moderate, biographical, constrained, global).

% Researchers and advocates who prioritize extinction-level risk from misaligned superintelligence. They pay through resource dilution: funding, talent, and policy bandwidth diverted to near-term harms under the 'non-competing' banner. Their professional identity is fused with the x-risk mission — leaving means abandoning the frame that justifies their life's work. They cannot exit without existential identity loss.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_advocates, payer,
    organized, civilizational, identity_locked, global).

% Researchers, civil society groups, and affected community representatives focused on deployed-system harms (bias, discrimination, labor exploitation, misinformation). They pay through epistemic marginalization: their concrete harms are reframed as 'stepping stones' to x-risk work, and their policy demands get subordinated to long-termist frameworks. Their identity is constituted through justice-for-affected-communities — the dual-priority frame asks them to see their work as instrumental to a future they may not prioritize.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_advocates, payer,
    organized, biographical, identity_locked, global).

% The hypothetical beneficiaries of genuine x-risk reduction. They pay if the dual-priority framing diverts resources from the highest-leverage extinction prevention work. They have no voice, no exit, and no representation in the current negotiation — their interests are mediated entirely by x-risk advocates who may themselves be diluted by the constraint.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_generations_stakeholders, payer,
    powerless, civilizational, trapped, universal).

% Communities experiencing algorithmic harm today (discriminatory hiring, predatory lending, surveillance, content moderation bias). They pay when their urgent remediation needs are deferred as 'compatible with' long-term safety rather than prioritized on their own terms. They are trapped in deployed systems they cannot opt out of, and their advocates' absorption into the dual-priority frame weakens targeted accountability pressure.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, currently_marginalized_communities, payer,
    powerless, immediate, trapped, global).

% Government bodies drafting AI regulation (EU AI Act, US executive orders, UK safety institute). They observe the field's internal framing to decide whether to fund a unified 'AI safety' portfolio or mandate separate x-risk and near-term harm tracks. Their analytical seat lets them see the resource trade-offs the 'non-competing' claim papers over.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a fragmented research and advocacy landscape under a single 'AI safety' banner, enabling larger funding pools, shared infrastructure (benchmarks, compute, talent pipelines), and a coherent policy interface for governments. Solves the collective action problem of a field splitting into mutually unintelligible camps.
% TRANSFER_FUNCTION: Moves funding, talent, and policy attention from both specialized camps (x-risk focused and near-term harm focused) toward cross-cutting research agendas and unified field institutions. The transfer is framed as non-zero-sum but operates under fixed resource envelopes — gains for bridge work come at the margin from both specialized camps.
% ABSENT_VOICES: Directly affected communities experiencing algorithmic harm today (often Global South, low-income, minority groups) are structurally excluded from the 'AI safety' epistemic community — their harms are represented by advocates who are themselves pressured into the dual-priority frame. Future generations have no voice. Competing framings from Global South AI ethics traditions (e.g., data colonialism, algorithmic sovereignty) are excluded by the Western institutional framing of 'safety'.
% DISAPPEARANCE_RATIONALE: If the dual-priority constraint vanished overnight, the field would likely fracture into distinct x-risk and near-term harm communities with separate funding streams, conferences, and policy asks. Funders would face higher transaction costs. Cross-cutting research would lose its distinctive legitimacy. Policy makers would need to engage two separate constituencies. The world rearranges because the constraint actively structures the field's institutional topology.
% FOUNDING_PROBLEM: Early AI safety field (ca. 2014-2018) split into 'longtermist/x-risk' and 'fairness/accountability/transparency' camps that talked past each other, competed for the same talent pool, and presented fragmented policy asks to governments. Funders and policymakers complained about the field's incoherence. The dual-priority framing emerged as a coalition-building device to unify the field under one banner.
% FOUNDING_PROBLEM_CORROBORATION: Field founders (e.g., FLI, CHAI, early FHI researchers) attest the fragmentation was real and the unification was strategic. Critics from both camps (x-risk purists like Yudkowsky-adjacent networks; near-term harm advocates like Algorithmic Justice League, Data & Society) attest the unification papered over genuine priority conflicts that resurface under scarcity. Independent sociological studies of the AI safety field (e.g., Whittaker et al., various STS analyses) corroborate the fragmentation-to-unification trajectory and its strategic character.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).
:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real resource diversion: cross-cutting agendas and unified institutions capture funding that would otherwise flow to specialized work. The 'non-competing' claim becomes extractive when scarcity makes it false — the framing prevents honest trade-off negotiation. Suppression (0.35) is moderate: the constraint doesn't legally ban specialized work but creates professional and funding incentives that marginalize pure x-risk or pure near-term work. Theater ratio (0.48) is high and rising: the complementarity rhetoric increasingly exceeds the empirical evidence for it (e.g., interpretability helps both, but most near-term fairness work doesn't reduce x-risk, and most x-risk theory doesn't reduce current bias). Accessibility collapse (0.38) is moderate: alternative framings (pure x-risk, pure near-term, pluralist 'separate but coordinated') remain intellectually available but institutionally disadvantaged. Resistance (0.55) is significant: both camps periodically revolt against the unification (e.g., 2023 open letters, funding splits, separate conference tracks).
 *
 * PERSPECTIVAL GAP:
 *   From the institutional field's seat, the constraint is a rope: genuine coordination solving fragmentation. From the x-risk advocate's seat, it's a snare/tangled_rope: their overriding priority is subordinated to a false equivalence. From the near-term harm advocate's seat, it's a snare/tangled_rope: their urgent justice claims are instrumentalized for a long-termist agenda. From trapped populations' seats, it's a snare: their interests are mediated by advocates who are themselves constrained. The engine computes this divergence from power, exit_options, and beneficiary/victim declarations — the claimed_type 'tangled_rope' reflects the authoring seat's judgment that the constraint has BOTH a real coordination function AND asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional field and funders are structural beneficiaries (d near 0.0-0.2): they collect coordination rents (larger field, simpler policy interface, reduced transaction costs). Both advocate populations are structural targets (d near 0.7-0.9): they bear the cost of diluted priorities and identity pressure to conform. Identity-locked exit for both advocate groups amplifies their effective extraction — they cannot leave the frame without losing their professional identity. Trapped populations (future generations, marginalized communities) have d near 1.0 but no voice in the constraint's maintenance. Policy makers sit at analytical (d=0.5) — they see the full structure but are not themselves extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (field fragmentation) was real but is now contested: the field has grown enough that separate institutions could sustain themselves. The dual-priority framing persists because it serves the institutional field and funders — a classic mandatrophy pattern where the solution (unification) outlives the problem (fragmentation) because the solution's beneficiaries control the agenda. The constraint is not 'resolved mandatrophy' — the mandate has not been formally acknowledged as obsolete; it's actively defended as still necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_empirical_basis,
    'Is the claimed complementarity between x-risk and near-term harm interventions empirically substantiated, or is it a strategic framing without evidence?',
    'Systematic review of whether research funded as ''cross-cutting'' (interpretability, robustness, governance) demonstrably advances both extinction prevention and near-term harm reduction, versus advancing neither deeply. Track funding flows: do unified portfolios actually allocate proportionally to both, or does one dominate?',
    'If complementarity is empirically thin, the ''non-competing'' claim is a cover story for resource capture by cross-cutting agendas — the constraint reclassifies toward snare. If substantively real, the constraint''s coordination function is genuine and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_empirical_basis, empirical, 'Whether the dual-priority claim rests on evidence or strategic framing.').

omega_variable(
    resource_allocation_coherence_under_scarcity,
    'Can the ''non-competing priorities'' framing coherently guide resource allocation decisions when budgets are fixed and trade-offs are unavoidable?',
    'Analyze actual allocation decisions in unified AI safety portfolios (Open Philanthropy, government institutes, corporate labs): when a dollar must go to either mechanistic interpretability (x-risk relevant) or bias auditing (near-term relevant), what decision rule is used? Does the dual-priority framing provide a decision procedure, or does it dissolve into ad hoc politics?',
    'If the framing provides no decision procedure under scarcity, it functions as a snare — it prevents honest trade-off negotiation while extracting legitimacy from both camps. If a coherent procedure exists (e.g., portfolio theory with explicit weights), the coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_coherence_under_scarcity, conceptual, 'Whether the constraint solves or papers over the scarcity problem.').

omega_variable(
    kernel_reading_structural_delta,
    'Does the dual_priority_reading''s victim set (union of both populations) and resource distribution attempt structurally differ from the sibling readings in a way that makes it a distinct constraint, not merely a rhetorical compromise?',
    'Compare the three readings'' stakeholder structures: existential_risk_reading has victims = future_generations (only), beneficiaries = x-risk_field; near_term_harms_reading has victims = currently_marginalized_communities (only), beneficiaries = FAT/ML_field; dual_priority_reading has victims = union, beneficiaries = unified_field. The structural delta is the dual_priority_reading''s attempt to serve two victim populations through one mechanism — this creates the coherence challenge.',
    'Confirms the ε-invariance principle: each reading is a distinct constraint with its own ε, stakeholder structure, and type. The dual_priority_reading''s ε (0.42) reflects the extraction from BOTH populations simultaneously — a structural property neither sibling reading possesses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural distinctness of this reading from its siblings per ε-invariance.').

omega_variable(
    identity_lock_mechanism_for_advocates,
    'What specific identity-fusion mechanism binds x-risk and near-term harm advocates to the dual-priority frame such that exit_options = identity_locked?',
    'Interview advocates from both camps: is their attachment to the ''AI safety'' label professional (career capital), ideological (worldview constitutive), relational (community membership), or institutional (their org''s brand)? Track what happens when individuals DO exit (e.g., researchers leaving ''AI safety'' for ''AI ethics'' or ''AI alignment'' as separate labels).',
    'If identity_locked is professional/ideological, the constraint''s extraction is amplified by career/worldview costs of exit. If it''s primarily institutional (org branding), the lock may weaken as new institutional homes emerge. This affects the engine''s directionality computation for both advocate seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_advocates, empirical, 'Mechanism of identity lock for the two advocate populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2018, ai_safety_commitment__dual_priority_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2020, ai_safety_commitment__dual_priority_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2022, ai_safety_commitment__dual_priority_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2024, ai_safety_commitment__dual_priority_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2026, ai_safety_commitment__dual_priority_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_safety_dual_priority_be_t2018, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2018, 0.18).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2020, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2022, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2022, 0.33).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2024, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2026, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_safety_dual_priority_su_t2018, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2018, 0.15).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2020, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2022, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2022, 0.28).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2024, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2024, 0.32).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2026, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.08).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint (dual_priority_reading) and its two siblings (existential_risk_reading, near_term_harms_reading) form the ai_safety_commitment constraint family. Each reading instantiates a different constraint from the same kernel with distinct ε, stakeholder structures, and claimed types. The dual_priority_reading claims complementarity (ε=0.42, tangled_rope); the existential_risk_reading claims lexical priority of x-risk (likely lower ε for x-risk advocates, higher for near-term advocates); the near_term_harms_reading claims priority of present harms (inverse pattern). All three coexist as live positions. The dual_priority_reading influences both siblings by capturing the unified 'AI safety' funding/policy interface, creating downstream resource pressure on specialized camps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, organized, 0.75).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
