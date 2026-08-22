% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated Alignment Commitment: Control and Justice as Non-Exclusive
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   The AI alignment field has crystallized around two dominant but partial
 *   readings: safety_control (preventing catastrophic loss of control over
 *   advanced AI) and ethics_justice (preventing reproduction of social bias
 *   and present-day harm). The integrated reading asserts these are
 *   non-exclusive — that alignment requires simultaneous attention to both
 *   control problems and justice problems, because the mechanisms that
 *   produce catastrophic misalignment and those that produce present-day
 *   injustice share structural roots (concentration of power, absence of
 *   accountable governance, optimization without legitimate constraint). This
 *   reading names a constraint: the field's institutional arrangement that
 *   fragments effort, funding, and legitimacy into silos, extracting from
 *   researchers and communities who would work across the boundary. The
 *   extractiveness (ε=0.42) reflects the cost of maintaining this
 *   fragmentation — duplicated infrastructure, missed interventions, and the
 *   active marginalization of integrated approaches. The constraint persists
 *   because each silo has institutional beneficiaries (dedicated funding
 *   streams, distinct publication venues, separate career ladders) and
 *   because the integrated approach lacks a natural institutional home.
 *
 * KEY AGENTS:
 *   - present_marginalized_populations: Primary beneficiary (powerless/identity_locked) — bears present-day harm from AI bias, exclusion, and labor displacement; would benefit from integrated governance that centers justice
 *   - future_humanity: Primary beneficiary (powerless/identity_locked) — bears existential risk from loss of control; would benefit from integrated governance that treats control as a justice problem
 *   - siloed_safety_researchers: Victim (organized/constrained) — researchers focused on control problems who lose legitimacy, funding, and intellectual scope when integration is marginalized
 *   - siloed_justice_researchers: Victim (organized/constrained) — researchers focused on justice problems who face identical marginalization from the control silo
 *   - fragmented_policy_communities: Victim (institutional/constrained) — governance bodies forced to choose between safety frameworks and justice frameworks, unable to enact integrated regulation
 *   - integrated_alignment_researchers: Payer/beneficiary (moderate/constrained) — researchers working across the boundary who bear career costs but capture the intellectual and practical gains of integration
 *   - major_ai_labs: Agenda_setter (institutional/arbitrage) — set research priorities, control compute access, benefit from fragmentation that lets them claim alignment via either silo without integrated accountability
 *   - funding_agencies: Agenda_setter (institutional/arbitrage) — allocate resources through siloed programs, reinforcing the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.42).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.35).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated Alignment Commitment: Control and Justice as Non-Exclusive").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '3d8ad9d0-3eb5-462f-8d01-a6bc4409088a').
narrative_ontology:cs_kernel_codification('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', distributed).
narrative_ontology:cs_authority_grounding('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', distributed).
narrative_ontology:cs_reading_relation('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', foundational, control_and_justice_co_constitutive).
narrative_ontology:cs_axiom_status(control_and_justice_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', control_and_justice_co_constitutive, deontological).
narrative_ontology:cs_axiom('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', foundational, siloed_arrangement_extracts_from_integration).
narrative_ontology:cs_axiom_status(siloed_arrangement_extracts_from_integration, holdable).
narrative_ontology:cs_axiom_grounding('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', siloed_arrangement_extracts_from_integration, empirically_contingent).
narrative_ontology:cs_reference_frame('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', ai_alignment_as_siloed_governance_target).
narrative_ontology:cs_drift_state('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', post_chatgpt_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d8ad9d0-3eb5-462f-8d01-a6bc4409088a', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_justice_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, fragmented_policy_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically excluded from AI development and disproportionately harmed by deployed systems (bias in hiring, lending, policing, healthcare). They bear present-day extraction from AI systems but have no structural power over alignment governance. Their 'exit' from AI harm is identity-locked — they cannot opt out of systems that govern their life chances. The integrated reading's operation would benefit them by making justice a co-constitutive alignment target rather than an afterthought.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, beneficiary,
    powerless, biographical, identity_locked, global).

% The collective subject of existential risk from loss of control over advanced AI. No individual can exit this risk; it is identity-locked at the species level. The integrated reading's operation would benefit them by treating control problems as inseparable from justice problems — preventing the concentration of power that enables both catastrophic misalignment and present-day injustice.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, identity_locked, universal).

% Researchers and institutions dedicated to control/alignment technical work (interpretability, scalable oversight, reward modeling). They lose when the integrated approach is marginalized: their work is framed as 'only technical,' missing justice dimensions that affect robustness; they compete for siloed funding; they face intellectual narrowing. Exit is constrained — moving toward integration risks career capital in the safety silo, but staying siloed limits impact.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_safety_researchers, payer,
    organized, biographical, constrained, global).

% Researchers and institutions dedicated to AI ethics, fairness, accountability, and present-day harm reduction. They face symmetric marginalization from the control silo: their work is framed as 'only near-term,' missing control dimensions that affect long-term justice; they compete for siloed funding; they face intellectual narrowing. Exit is constrained for identical structural reasons.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_justice_researchers, payer,
    organized, biographical, constrained, global).

% Governance bodies (legislatures, regulatory agencies, international forums) forced to choose between 'AI safety' frameworks and 'AI ethics' frameworks. They bear the cost of fragmented expertise, contradictory testimony, and inability to write integrated regulation. Exit is constrained — institutional mandates and political coalitions are built around the silos.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, fragmented_policy_communities, payer,
    institutional, generational, constrained, global).

% Researchers and practitioners working across the control/justice boundary (e.g., on participatory alignment, democratic governance of AI, justice-aware interpretability). They pay career costs (fewer dedicated venues, ambiguous funding fit, identity friction from both silos) but capture the intellectual and practical gains of integration — more robust interventions, broader coalitions, truer problem representation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, beneficiary).

% Frontier AI developers (e.g., OpenAI, Anthropic, DeepMind, Meta AI) who set de facto research priorities, control compute access, and define what counts as 'alignment work.' They benefit from fragmentation: they can claim alignment via safety investments OR ethics investments without being held to an integrated standard. Their exit is arbitrage-grade — they could restructure their own alignment portfolios overnight and have the resources to weather any transition.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, major_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% Government agencies (NSF, DARPA, EU Horizon, UK ARIA) and philanthropies (Open Philanthropy, Longview, etc.) that allocate alignment funding through siloed programs (AI Safety, AI Ethics, Responsible AI). They reinforce the constraint by designing RFPs, review panels, and metrics around the dichotomy. Exit is arbitrage-grade — they control the purse strings and could create integrated programs by fiat.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, funding_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The siloed arrangement coordinates specialized research communities, dedicated funding streams, and distinct policy interfaces — it solves the problem of 'how to build a field' by partitioning the alignment target into tractable sub-problems with legible metrics and communities.
% TRANSFER_FUNCTION: Moves research talent, funding, and institutional legitimacy from integrated approaches to siloed ones. Resources flow to researchers who stay in lane; career capital accrues to silo-legible work; policy attention splits into parallel tracks. The transfer is not zero-sum — the field grows overall — but the integrated margin is systematically under-resourced relative to its leverage.
% ABSENT_VOICES: Communities most affected by both present-day AI harm and future AI risk (Global South populations, indigenous communities, precarious workers, future generations) are structurally excluded from the alignment governance conversation. They would object to the dichotomy that treats their present harm and their existential risk as separate problems. Their absence is not accidental — the siloed arrangement has no mechanism for their representation.
% DISAPPEARANCE_RATIONALE: If the siloed constraint vanished overnight, funding agencies would restructure programs toward integrated priorities within grant cycles; major labs would consolidate safety and ethics teams; researchers would reorganize around unified problem statements; policy bodies would draft integrated frameworks. The field's intellectual and institutional architecture would reorganize — not collapse, but rearrange around the integrated target.
% FOUNDING_PROBLEM: The alignment field's founding problem was preventing catastrophic outcomes from advanced AI systems — initially framed narrowly as 'loss of control' (the safety_control reading). As the field grew, the justice dimension (bias, fairness, accountability) emerged as a co-constitutive problem, but the institutional form had already crystallized around the control frame.
% FOUNDING_PROBLEM_CORROBORATION: Safety_control proponents (e.g., MIRI, early FHI) attest the founding problem is purely control and remains live. Ethics_justice proponents (e.g., DAIR, algorithmic justice organizations) attest the founding problem was always broader and the control framing was ahistorical exclusion. Integrated proponents (e.g., Center for AI Safety's later work, Partnership on AI, interdisciplinary institutes) attest the founding problem was underspecified and the dichotomy is the constraint. No single party's attestation is definitive — the contest is structural.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).
:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is substantial but not dominant: the constraint extracts by forcing researchers and communities to choose silos, duplicating effort, and blocking interventions that require integrated analysis. Suppression (0.35) is moderate: the constraint persists through institutional inertia, funding structures, and social identity more than active coercion — researchers who integrate face career friction, not prohibition. Theater (0.28) reflects performative gestures toward integration (joint workshops, bridging grants) that don't restructure the underlying incentive landscape. Accessibility collapse (0.38) is partial: alternative integrated frameworks exist and are advocated, but lack institutional footholds. Resistance (0.52) is significant: a growing coalition explicitly rejects the dichotomy and builds integrated approaches, creating measurable pushback against the siloed arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (major labs, funders), the siloed arrangement appears as rational specialization — distinct problems, distinct methods, distinct communities. From the victim seats (siloed researchers, policy communities), the same arrangement appears as enforced fragmentation that blocks necessary integration. From the beneficiary seats (present marginalized, future humanity), the constraint is a structural barrier to the governance they need. The engine computes these divergences from the declared power/exit/role structure; the authored claim (tangled_rope) asserts the constraint has both genuine coordination function (field-building, resource allocation) and asymmetric extraction (siloed communities pay, integrated communities are excluded).
 *
 * DIRECTIONALITY LOGIC:
 *   Present marginalized populations and future humanity are structural beneficiaries (d near 0.0): the integrated reading's operation would subsidize them by redirecting alignment effort toward their interests. Siloed researchers and fragmented policy communities are structural victims (d near 1.0): they bear the cost of fragmentation through duplicated work, missed leverage, and institutional exclusion. Integrated researchers sit near symmetric (d ~ 0.5): they pay career costs but capture intellectual gains. Major AI labs and funding agencies are agenda_setters with arbitrage-grade exit (d ~ 0.15): they benefit from the fragmentation they administer and can exit the constraint by restructuring their own institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The alignment commitment's founding problem (preventing AI catastrophe) remains live, but the siloed institutional form has outlived its function — the field's growth and the convergence of control/justice mechanisms mean the arrangement now extracts more than it coordinates. The mandate has not been formally resolved; it has drifted into a state where the institutional form actively impedes the mandate. This is mandatrophy: the constraint persists because no single actor bears enough cost to fix it and no single actor benefits enough from the status quo to defend it explicitly — it is maintained by the aggregate inertia of siloed institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the integrated reading''s core premise (simultaneous attention to control and justice as non-exclusive) logically foreclose the safety_control_reading''s premise (alignment = preventing catastrophic loss of control) within a single governance framework?',
    'Analyze whether any single institutional framework can coherently maintain both premises without internal contradiction — specifically whether a framework that treats control and justice as co-constitutive can also treat control as the sole or primary alignment target.',
    'If forecloses, the safety_control_reading cannot be held within the same commitment framework as the integrated reading; if coexists_with or influences, both can persist as live positions in the field with structural pressure between them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between integrated and safety_control readings of the alignment kernel').

omega_variable(
    kernel_reading_boundary_justice,
    'Does the integrated reading''s core premise logically foreclose the ethics_justice_reading''s premise (alignment = preventing reproduction of social bias and present-day harm) within a single governance framework?',
    'Analyze whether any single institutional framework can coherently maintain both premises — specifically whether a framework that treats control and justice as co-constitutive can also treat present-day justice as the sole or primary alignment target.',
    'If forecloses, the ethics_justice_reading cannot be held within the same commitment framework as the integrated reading; if coexists_with or influences, both can persist as live positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_justice, conceptual, 'Structural relationship between integrated and ethics_justice readings of the alignment kernel').

omega_variable(
    extraction_from_siloed_fragmentation,
    'How much of the measured extractiveness (ε=0.42) derives from active exclusion of the integrated approach by siloed communities, versus inherent cost of the integration itself?',
    'Natural experiment: track resource allocation, publication venues, and hiring patterns as integrated safety-justice programs emerge; measure whether siloed communities'' resistance decreases when integration is institutionally rewarded rather than marginal.',
    'If extraction is primarily from active exclusion, the constraint is more snare-like (suppression of alternative); if primarily from integration cost, it is more rope-like (coordination overhead).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_from_siloed_fragmentation, empirical, 'Whether extractiveness comes from suppression of integrated approach or inherent coordination cost').

omega_variable(
    victim_set_coherence,
    'Is the combined victim set (present marginalized populations + future humanity) structurally coherent — do the same mechanisms extract from both — or does the integrated reading conflate two distinct extraction mechanisms?',
    'Map the causal pathways: for present marginalized populations, extraction operates through bias replication, exclusion from design, and labor displacement; for future humanity, extraction operates through control loss, misalignment, and existential risk pathways. Test whether interventions addressing one pathway reliably affect the other.',
    'If mechanisms are distinct, the integrated reading may be two constraints linked by network.affects_constraints rather than one constraint with a unified victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_coherence, conceptual, 'Coherence of the combined victim set across temporal scales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_commitment__integrated_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_commitment__integrated_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_commitment__integrated_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_commitment__integrated_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement(ai_a_tr_t2026, ai_alignment_commitment__integrated_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_commitment__integrated_reading, base_extractiveness, 2018, 0.25).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_commitment__integrated_reading, base_extractiveness, 2020, 0.32).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_commitment__integrated_reading, base_extractiveness, 2022, 0.38).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_commitment__integrated_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement(ai_a_be_t2026, ai_alignment_commitment__integrated_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_commitment__integrated_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_commitment__integrated_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_commitment__integrated_reading, suppression_requirement, 2022, 0.33).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_commitment__integrated_reading, suppression_requirement, 2024, 0.35).
narrative_ontology:measurement(ai_a_su_t2026, ai_alignment_commitment__integrated_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the ai_alignment_commitment kernel family. The safety_control_reading (ε ≈ 0.15, mountain-claimed) treats control as a technical coordination problem with negligible extraction. The ethics_justice_reading (ε ≈ 0.35, tangled_rope-claimed) treats justice as a sociotechnical coordination problem with moderate extraction from bias replication. This integrated_reading (ε = 0.42, tangled_rope) treats the siloed institutional arrangement itself as the constraint — its extraction derives from actively maintaining the boundary between control and justice communities. The ε values differ because the referents differ: safety_control reads the kernel as a technical fact; ethics_justice reads it as a sociotechnical practice; integrated reads it as an institutional arrangement that fragments effort.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, organized, 0.75).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, powerless, 0.05).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
