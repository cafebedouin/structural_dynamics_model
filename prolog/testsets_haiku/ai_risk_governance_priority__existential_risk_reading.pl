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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential Risk Prioritization in AI Governance
 *   domain: technological/governance/risk_assessment
 *
 * SUMMARY:
 *   The existential-risk reading of AI governance prioritization claims that
 *   preventing superintelligence scenarios capable of annihilating or
 *   permanently curtailing humanity's potential must be the governing concern
 *   structuring AI policy, resource allocation, and technical safety work.
 *   This reading emerges from the technical AI safety community and has
 *   become institutionalized in x-risk research centers and certain AI
 *   development companies. The claim is that the magnitude of potential harm
 *   justifies radical resource reallocation. The reading benefits x-risk
 *   institutions and AI labs claiming safety leadership, while deprioritizing
 *   near-term harm mitigation and transparency-based accountability. This
 *   story instantiates ONLY the existential-risk reading as a constraint; the
 *   near-term harms reading and bridge reading are separate constraints in
 *   the same kernel family, linked via network.affects_constraints. The
 *   claim/metric gap is deliberate and structural: the existential-risk
 *   reading claims to solve a genuine coordination problem (AI safety
 *   alignment), and the metrics show substantial extraction because the
 *   constraint's actual operation benefits institutional actors while
 *   deprioritizing documented present harms. The engine computes whether the
 *   coordination function justifies the extraction.
 *
 * KEY AGENTS:
 *   - xrisk_research_institutions: Primary beneficiary and agenda-setter. Define superintelligence risk, prioritize existential scenarios, capture governance resources.
 *   - ai_labs_claiming_safety_leadership: Primary beneficiary. Redirect regulatory attention from transparency/accountability to technical safety measures they control.
 *   - future_humanity: Abstract victim bearing speculative cost (non-agent entry; the abstract beneficiary of the constraint).
 *   - present_marginalized_populations: Concrete victims experiencing opportunity cost of deprioritized near-term harm work.
 *   - near_term_harm_mitigation_advocates: Payer. Work within institutional hierarchy that ranks their concerns below existential risk.
 *   - superintelligence_skeptics: Excluded. Their empirical objections are heard but carry little governance weight.
 *   - analytical_observer: Measures whether the constraint is what it claims to be.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.71).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential Risk Prioritization in AI Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technological/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '144442d3-3774-4a61-841b-65bd01d95565').
narrative_ontology:cs_kernel_codification('144442d3-3774-4a61-841b-65bd01d95565', distributed).
narrative_ontology:cs_authority_grounding('144442d3-3774-4a61-841b-65bd01d95565', extraction).
narrative_ontology:cs_reading_relation('144442d3-3774-4a61-841b-65bd01d95565', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('144442d3-3774-4a61-841b-65bd01d95565', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('144442d3-3774-4a61-841b-65bd01d95565', foundational, superintelligence_existential_priority).
narrative_ontology:cs_axiom_status(superintelligence_existential_priority, holdable).
narrative_ontology:cs_axiom_grounding('144442d3-3774-4a61-841b-65bd01d95565', superintelligence_existential_priority, empirically_contingent).
narrative_ontology:cs_axiom('144442d3-3774-4a61-841b-65bd01d95565', secondary, technical_alignment_tractability).
narrative_ontology:cs_axiom_status(technical_alignment_tractability, holdable).
narrative_ontology:cs_axiom_grounding('144442d3-3774-4a61-841b-65bd01d95565', technical_alignment_tractability, empirically_contingent).
narrative_ontology:cs_reference_frame('144442d3-3774-4a61-841b-65bd01d95565', existential_risk_governance_primacy).
narrative_ontology:cs_drift_state('144442d3-3774-4a61-841b-65bd01d95565', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('144442d3-3774-4a61-841b-65bd01d95565', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, technical_safety_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harm_mitigation_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).

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
 *   Extractiveness is 0.68 at interval end because the constraint concentrates institutional legitimacy and resources on existential-risk research while deprioritizing near-term harms, despite the empirical base for superintelligence scenarios remaining contested and heavily modeled on assumptions that lack corroboration outside the benefiting institutions. The constraint's persistence depends on maintaining this priority gap, which requires active suppression of competing governance frameworks (near-term harms, transparency-based oversight). Suppression is 0.71 because skeptics and near-term advocates are excluded from governance priority-setting despite the founding problem's status being contested. Theater is 0.52 and rising because the constraint increasingly operates through rhetorical framing and institutional gatekeeping rather than empirical demonstration: grant reviews rank existential-risk work higher; policy advisory positions are filled by x-risk researchers; conference prominence flows to superintelligence scenarios. The measurement series captures the extraction-accumulation pattern: as the constraint matured from 2015–2025, extractiveness rose (resources consolidated in x-risk institutions), theater ratio increased (more academic infrastructure and policy theater defending the priority), and suppression stabilized (the hierarchy of concerns became institutionalized and resistant to challenge). The shared time grid ensures every metric is authored at every examined point—no off-endpoint time points, no misaligned grids.
 *
 * PERSPECTIVAL GAP:
 *   From the xrisk_research_institutions and ai_labs_claiming_safety_leadership seats, the constraint appears as genuine coordination: they are addressing the highest-stakes problem and aligning technical work and governance around that problem. From the present_marginalized_populations and near_term_harm_mitigation_advocates seats, the same structure operates as enforced deprioritization of documented harms in favor of speculative catastrophes. The superintelligence_skeptics seat sees the constraint as justified extraction of resources based on empirical claims that have not been corroborated outside the benefiting institutions. The engine computes per-seat classification: the beneficiary seats will see coordination; the payer seats will see extraction. That divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   xrisk_research_institutions and ai_labs_claiming_safety_leadership are structural beneficiaries (d near 0.0): they set the agenda, collect institutional resources and prestige, and face no exit pressure from the constraint. present_marginalized_populations are structural targets (d near 1.0): trapped identity-locked into algorithmic systems, bearing the cost of deprioritized harm mitigation, with no voice in governance hierarchy. near_term_harm_mitigation_advocates have constrained exit (d near 0.7): they can leave AI governance, but staying requires accepting a governance hierarchy that ranks their work below existential-risk research. superintelligence_skeptics are excluded rather than participants (d undefined in the directionality framework because they are not integrated into the beneficiary-or-payer structure; the constraint persists by keeping them outside governance visibility). future_humanity is an abstract non-agent (assigned false under agent field) bearing speculative cost. The analytical_observer seat computes no d: it measures structure without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—ensuring advanced AI systems remain controllable and aligned with human values before superintelligence—is contested in status (live, dead, or substantially reframed). The disappearance verdict is world_rearranges: AI governance would reorganize if the existential-risk prioritization vanished. The mismatch (contested status + rearranges verdict) is the signal of potential mandatrophy. The founding problem may have been live in 2010–2015 (genuine uncertainty about AI alignment and superintelligence timelines). The problem's status is now contested because: (1) superintelligence scenarios remain unresolved empirically—no external corroboration has settled whether takeoff speed is fast or slow; (2) the constraint has metastasized from alignment-problem research into resource concentration in institutions benefiting from existential-risk framing; (3) the founding problem's reframing (from 'ensure control' to 'prioritize governance frameworks for AGI-readiness scenarios') serves beneficiary interests. If the constraint persists primarily to defend the institutional hierarchy rather than to address a live, corroborated founding problem, mandatrophy may be resolving—the constraint may be maintaining a dead or substantially shifted founding problem through institutional inertia and suppression of alternatives. This does not prove mandatrophy; it flags it as an unresolved question the empirical record will settle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_probability_unresolved,
    'What is the true probability of superintelligence scenarios occurring within the next 50–100 years, and with what confidence level?',
    'Cumulative empirical advances in AI capability measurement, takeoff-speed modeling, and alignment progress would gradually resolve this. External expert consensus across AI developers, ML researchers outside x-risk circles, and evidence from actual AI systems advancing toward AGI capabilities would narrow uncertainty bounds.',
    'If superintelligence probability is substantially lower than x-risk models assume (< 10%), the existential-risk prioritization is unjustified resource concentration and the constraint''s extraction becomes indefensible. If probability is as high as x-risk models claim (> 30%), the prioritization is justified and the constraint approaches genuine coordination. The measurement determines whether the constraint is mandatrophy or coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_probability_unresolved, empirical, 'Core empirical uncertainty: is superintelligence probable enough to justify governance prioritization?').

omega_variable(
    alignment_problem_tractability_contested,
    'Is the alignment problem (ensuring superintelligent AI systems remain controllable and value-aligned) tractable through technical safety research at the resource levels currently allocated, or is the allocation itself a form of optimistic fiction?',
    'Progress in mechanistic interpretability, adversarial robustness, and formal verification of AI behavior would demonstrate tractability. Alternatively, sustained stalling despite high research investment would suggest the problem is harder than modeled or misframed.',
    'If tractable, technical safety research deserves the resource concentration. If intractable, the constraint is funding research that cannot deliver on its promises, which is pure extraction disguised as safety work. If the problem is misframed (e.g., the real issue is governance and incentives, not technical alignment), the resource allocation is fundamentally misdirected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_problem_tractability_contested, empirical, 'Is the existential-risk reading''s core technical claim (alignment through technical safety work) empirically sound?').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the deprioritization of near-term harm work structural (policy hierarchy, funding mechanisms, governance rules that actively redirect resources) or internalized (near-term advocates accept the existential-risk framing and voluntarily lower their own priorities)?',
    'If external governance pressure (policy changes mandating near-term accountability, funding that flows to bias research, regulatory oversight of harm mitigation) is removed, do near-term advocates immediately increase their visibility and resources, or do they remain suppressed? Post-deprioritization behavior tests internalization.',
    'If structural, suppression is a raw enforcement property of the constraint. If internalized, the constraint''s effective suppression is higher than the structural measure suggests—victims carry the suppression into alternative governance models. This informs whether fixes require dismantling institutional hierarchy (structural) or recovering epistemic confidence in present-harm research (cultural/internalized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Mechanism of suppression: structural enforcement vs. internalized hierarchy acceptance.').

omega_variable(
    contested_kernel_empirical_base,
    'Which empirical facts would resolve the contest between the three sibling readings—existential-risk, near-term harms, and bridge readings—to determine which governance prioritization is actually justified?',
    'Measurement of (1) AI capability advancement rates and superintelligence timeline estimates updated quarterly; (2) documented harm rates from present algorithmic systems; (3) resource allocation elasticity: what additional resources directed to near-term harms work would reduce documented bias/displacement/surveillance, versus what resources directed to existential-risk research would reduce superintelligence probability. This would establish the marginal value per dollar for each governance path.',
    'If present harms are vastly more reducible than superintelligence probability, the bridge reading and near-term reading are justified and existential-risk prioritization is unjustified extraction. If superintelligence timeline is much faster than believed and tractable only through the safety research currently prioritized, existential-risk reading is justified. If both present harms and existential risks have comparable marginal returns to resources, the bridge reading (integrated governance) is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_empirical_base, empirical, 'Core empirical differences between sibling kernel readings: what data would resolve the governance contest?').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the existential-risk reading of the ai_risk_governance_priority kernel a genuine analytical position that can coexist with near-term and bridge readings in a single coherent governance framework, or does it represent a different commitment system (technical-safety-led governance vs. accountability-led governance) that forecloses the alternatives?',
    'Historical observation of whether institutions holding the existential-risk reading show structural capacity to integrate near-term harm mitigation without deprioritizing existential-risk work. If they consistently subordinate near-term concerns, the readings occupy different commitment systems (foreclosure). If they can genuinely integrate, the readings coexist in a single system (the bridge reading would be true).',
    'If the readings occupy different commitment systems, the kernel contest is not empirical—it is a dispute over which commitment system should govern AI policy (technical safety vs. accountability). If they coexist, the contest is empirical (which governance approach reduces more risk), and the bridge reading framework is vindicated. This determines whether the constraint is extractive or coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Do the kernel readings coexist in one framework or foreclose in different commitment systems?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.18).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (existential_risk_reading) of the contested kernel ai_risk_governance_priority. The kernel has three structurally distinct readings: existential_risk_reading (this file), near_term_harms_reading, and bridge_reading. Each reading instantiates a different constraint with different victim sets, beneficiary structures, and ε values. The readings coexist as live governance positions held by different institutional actors. The existential-risk reading influences the other two by setting resource constraints and governance hierarchy that deprioritizes near-term harm work. The bridge reading attempts to integrate both concerns but faces institutional pressure from the existential-risk prioritization. Each sibling reading should be authored as a separate constraint story with its own ε-invariance, its own base_properties, and its own stakeholder situation—they are not alternative perspectives on the same constraint, they are different constraints grounded in the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
