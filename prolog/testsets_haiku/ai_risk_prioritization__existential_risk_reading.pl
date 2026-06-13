% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential AI Risk Prioritization Frame
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel: the meaning of 'AI
 *   risk' and which harms should be prioritized. This reading instantiates
 *   the existential-risk frame: AI risk is primarily the extinction-level
 *   threat of misaligned superintelligent systems. The sibling reading
 *   (near_term_harms_reading) instantiates a different frame: AI risk is
 *   primarily the demonstrable discrimination, displacement, and surveillance
 *   harms from deployed systems today. These are not two perspectives on a
 *   single constraint—they are structurally distinct constraints with
 *   different victim sets, timescales, resource allocations, and beneficiary
 *   structures. This story authors the existential-risk reading as a clean,
 *   ε-invariant constraint. The sibling reading is a separate story.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Agenda-setter; frames the risk hierarchy; collects funding and influence.
 *   - longtermist_funders: Beneficiary; capital flows toward existential-risk research justified by this frame.
 *   - capability_control_advocates: Beneficiary; their proposals for AGI deployment pauses derive legitimacy from existential-risk framing.
 *   - near_term_affected_communities: Payer/victim; suffer reframing of their demonstrable harms as secondary or distraction.
 *   - algorithmic_justice_researchers: Payer/excluded; funding and salience drain from their research areas.
 *   - affected_future_persons: Invoked as moral patients; remain nonexistent and voiceless.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.72).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential AI Risk Prioritization Frame").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6').
narrative_ontology:cs_kernel_codification('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', distributed).
narrative_ontology:cs_authority_grounding('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', distributed).
narrative_ontology:cs_reading_relation('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', foundational, misaligned_superintelligence_poses_extinction_level_threat).
narrative_ontology:cs_axiom_status(misaligned_superintelligence_poses_extinction_level_threat, holdable).
narrative_ontology:cs_axiom_grounding('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', misaligned_superintelligence_poses_extinction_level_threat, empirically_contingent).
narrative_ontology:cs_axiom('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', foundational, extinction_risk_outweighs_non_extinction_harms_in_priority).
narrative_ontology:cs_axiom_status(extinction_risk_outweighs_non_extinction_harms_in_priority, holdable).
narrative_ontology:cs_axiom_grounding('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', extinction_risk_outweighs_non_extinction_harms_in_priority, deontological).
narrative_ontology:cs_axiom('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', secondary, alignment_research_is_necessary_and_sufficient_for_extinction_prevention).
narrative_ontology:cs_axiom_status(alignment_research_is_necessary_and_sufficient_for_extinction_prevention, holdable).
narrative_ontology:cs_axiom_grounding('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', alignment_research_is_necessary_and_sufficient_for_extinction_prevention, empirically_contingent).
narrative_ontology:cs_reference_frame('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', misalignment_extinction_primacy).
narrative_ontology:cs_drift_state('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a55c0e8-fa8d-4d3b-b9e7-de848c64aff6', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_affected_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, capability_control_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, longtermist_funders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major research organizations (Future of Humanity Institute, Center for Effective Altruism, Alignment Research Center, and related institutions) frame the primary AI risk as future misalignment of AGI systems. They set research agendas, allocate grant funding toward alignment work, publish frameworks treating extinction as the paramount concern, and exercise considerable influence over AI governance debates by defining risk hierarchy. Collects prestige, funding flows, and policy influence directly.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Large philanthropic funders committed to reducing existential risks (OpenPhilanthropy, Survival and Flourishing Fund, others) direct substantial capital toward AGI alignment research and longtermist AI safety work. This reading legitimizes their funding allocation; they benefit from the framing that future persons are the primary moral claimants. They also bear indirect costs where alignment-centric spending crowds out near-term harms mitigation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, longtermist_funders, payer).

% Researchers and policy advocates proposing AGI capability restrictions, compute governance, or deployment pauses justify their work as essential to manage extinction risk. The existential-risk frame is their legitimation; without it, their proposals to constrain AI development face efficiency and innovation objections. They depend on this frame holding to maintain salience and policy consideration.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, capability_control_advocates, beneficiary,
    moderate, generational, constrained, global).

% Populations suffering measurable discrimination, economic displacement, and surveillance harms from deployed AI systems now—facial recognition targeting marginalized groups, algorithmic hiring bias, algorithmic loan denial, automated welfare suspension. Their immediate harms are reframed as secondary or distraction from existential priority. They bear the suppression of near-term justice research funding and policy attention.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_affected_communities, payer,
    powerless, biographical, constrained, global).

% Academic researchers, civil rights advocates, and technologists working on measurable harms from deployed systems (bias, discrimination, surveillance, labor displacement). Their work is systematically de-prioritized when existential risk arguments capture funding and governance authority. Funding for near-term harm mitigation shrinks as capital flows toward longtermist research; policy discussions frame their work as less urgent or even as distraction from 'real' AI safety.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers, excluded).

% Nonexistent or not-yet-born persons who would bear the existential consequences of misaligned AGI. They are invoked as moral patients and primary beneficiaries of the existential-risk frame, yet have no seat in current governance or research prioritization. Their interests are represented through proxy institutions (x-risk organizations), not directly articulated.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, affected_future_persons, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, affected_future_persons).

% Large AI development firms (Anthropic, OpenAI, DeepMind, Meta, others) are excluded from setting the existential-risk research agenda, though they are targets of capability-control proposals justified by existential risk. They would contest the assessment that misalignment extinction is more probable than near-term harms or that capability restriction is warranted. Their exclusion from the frame-setting is enforced by institutional gatekeeping and epistemic authority claims.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_capability_companies, excluded,
    institutional, biographical, constrained, global).

% Legislators, regulators, and government advisors navigate between existential-risk framing (alignment, capability controls) and near-term harms (discrimination, displacement, surveillance). They receive testimony and analysis from both readings; the existential frame's institutional strength gives it disproportionate weight in policy formation, marginalizing near-term harm frameworks in priority-setting.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_makers, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes research effort and resource allocation around a coherent model of AI risk—preventing misalignment of AGI systems and ensuring human control over superintelligent systems. Solves the collective-action problem of fragmented risk assessment by creating a shared frame that unites x-risk researchers, funders, and capability-control advocates around a common priority hierarchy.
% TRANSFER_FUNCTION: Moves research funding, policy attention, and institutional prestige toward existential-risk research institutions and away from near-term algorithmic harms work. Redirects governance focus from immediate justice interventions toward long-term alignment and capability-control research. Near-term affected communities bear opportunity costs (funding not spent on their demonstrable harms); algorithmic justice researchers bear status and resource reduction within funding-allocation hierarchies.
% ABSENT_VOICES: Populations suffering present-day AI harms (algorithmic discrimination targets, job displacement communities, surveillance subjects, automated welfare denial victims) are not in the frame-setting conversation. They are named as victims but have no seat in governance. AI capability companies are excluded from defining risk probability and remedy. Affected future persons are represented by advocacy institutions, not directly. Their structural absence from governance is foundational to the constraint.
% DISAPPEARANCE_RATIONALE: If the existential-risk prioritization frame disappeared, funding flows would reallocate toward near-term harms and algorithmic justice; policy focus would shift from capability controls toward transparency, accountability, and harm-mitigation regulation; x-risk research institutions would face reduced resource availability and institutional influence. The AI governance landscape would reorganize around competing risk frameworks, with near-term harms reclaiming policy priority.
% FOUNDING_PROBLEM: As AI systems advance toward greater generality and scale, the risk of eventual misalignment of superintelligent systems poses potential extinction-level consequences requiring proactive research and development of alignment mechanisms before AGI systems are deployed.
% FOUNDING_PROBLEM_CORROBORATION: X-risk researchers and longtermist philosophers attest the founding problem is live and paramount. Near-term harms researchers, affected communities, and policy analysts focused on deployed-system governance attest the problem is speculative (conditioned on speculative AGI timelines with wide uncertainty ranges) and argue that demonstrable harms from current systems are the actual urgent problem. Economic analysis and equity audits from outside the x-risk funding ecosystem document funding redirection away from near-term harms over the past 10 years.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end). The constraint redirects substantial research funding and policy attention away from near-term work; this is extraction from powerless near-term communities. However, it is not pure extraction—the existential-risk frame does solve a real coordination problem (uniting distributed x-risk efforts into a shared research agenda), which is why Tangled Rope fits better than Snare. Suppression is high (0.72) because the constraint's persistence depends on actively suppressing the salience and legitimacy of near-term harm frameworks. When near-term researchers publish on algorithmic discrimination, the existential-risk frame recontextualizes their work as 'narrow' or 'distraction'; this is structural suppression. Theater is moderate (0.44) and rising: increasingly, existential-risk institutions maintain the frame through high-profile rhetoric about AGI timelines rather than through actual research breakthroughs, and capability-control advocacy oscillates between technical rigor and apocalyptic framing. Accessibility collapse is high (0.71): once the existential-risk frame is adopted (by funders, policy makers, research institutions), the alternative framing (near-term harms) becomes cognitively inaccessible—adopters of the existential frame interpret all contrary evidence (deployed harms, discrimination surveys, job displacement data) as evidence that 'we must solve existential risk first' rather than reconsidering the frame itself. Resistance is moderate (0.58) and comes from near-term affected communities and justice researchers, but it is structurally powerless—they have no seat in the funding allocation or governance structures that the frame controls.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk institutional seat, the arrangement is genuine coordination: uniting disparate researchers around a coherent model of AI risk and enabling focused capability-control research. From the near-term affected communities' seat, the same arrangement is enforced extraction: their immediate needs are reframed as less important and their researchers defunded. From the policy-maker seat, the constraint operates as a Rope (the frame solves the problem of comparing incommensurable AI risks) and as a Snare (the frame channels policy action toward less powerful constituency interests while suppressing attention to documented harms). The engine computes these divergences from the structural data: agenda-setter power + beneficiary + victim declarations produce per-seat classifications that diverge from the claimed unified rope.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutions and longtermist funders are near the beneficiary end (d ~0.2): they set the agenda, collect funding, gain institutional prestige. Near-term affected communities are near the target end (d ~0.85): they are powerless, structurally excluded, bear opportunity costs in funding redirection, and their harms are suppressed by reframing. Algorithmic justice researchers sit higher on the target scale (d ~0.75) than affected communities because they retain some institutional power (academic positions, publication platforms) but are suppressed within research governance structures. Capability-control advocates are asymmetric beneficiaries (d ~0.35): they benefit from the frame (legitimacy for proposals), but they also bear some cost (their work is contingent on this frame remaining dominant; if it falters, their research agenda loses justification). The affected_future_persons stake is modeled as beneficiary in role (they are invoked as the primary moral patients the frame protects) but structurally cannot collect or vote on resource allocations—the agent=false flag marks them as non-agent entities that the frame invokes but does not seat in governance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint appears as Tangled Rope: it has a genuine coordination function (uniting x-risk researchers toward a coherent research agenda) AND it has asymmetric extraction (benefits accrue to research institutions, costs fall on near-term affected communities). The extraction is only sustainable because (1) near-term communities are powerless and excluded from governance, and (2) the existential-risk frame is actively enforced via institutional gatekeeping (funding priorities, peer-review preferences, policy influence). Without active enforcement of the frame's supremacy, near-term harms researchers could reclaim equal or greater legitimacy—so it is Tangled Rope, not Rope. The theater-ratio rise (0.22 to 0.44 over 25 years) suggests that the functional coordination work (research breakthroughs, alignment techniques) has leveled off while the theatrical maintenance (urgent messaging about AGI timelines, policy advocacy) has intensified. This is not yet Piton-level performance (Piton would show theater > 0.5 and extraction plateauing while enforcement burden remains high), but the trajectory suggests institutional drift from functional to performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agi_timescale_epistemic_closure,
    'How is AGI timescale uncertainty resolved in decision-making? If AGI is 50+ years away, does existential risk prioritize correctly relative to near-term harms? If it is 10 years away, do near-term harm researchers underweight extinction risk?',
    'Post-hoc analysis: track AGI-timeline estimates in the literature; measure how timeline uncertainty actually drives resource allocation decisions; audit funding flows against stated timeline distributions; compare funding proportions to probabilistic risk weight (if P(AGI in 20 years) = 0.3, is x-risk funding 30% of total AI governance spending?). Resolve the closure via time: examine whether AGI actually arrives and whether alignment progress correlates with existential outcomes.',
    'If existential-risk prioritization is robust across timeline distributions, the reading''s ranking holds. If it is brittle—dependent on narrow timescale assumptions—the frame''s legitimacy weakens and resources should reallocate toward near-term harms. The constraint''s claimed type (Tangled Rope) would shift toward Snare if timescale estimates prove systematically inflated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agi_timescale_epistemic_closure, empirical, 'AGI timeline uncertainty and its role in existential-risk resource allocation.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of near-term harms research structural (funding institutions prefer to allocate capital toward existential-risk research, creating scarcity) or internalized (near-term researchers have psychologically accepted that their work is less important, reducing their advocacy)?',
    'Post-constraint-shift analysis: if the existential-risk frame weakened and near-term funding became available, would near-term researchers rapidly mobilize and absorb resources, or would suppression persist due to internalized priorities? Examine whether near-term researchers actively contest the frame or passively accept it.',
    'If suppression is primarily structural, removing the constraint (reallocating funding) remedies it; if primarily internalized, the constraint persists even after institutional gatekeeping loosens. The effective extraction (χ) is higher if internalized suppression is strong, as targets carry the suppression with them beyond the constraint''s original scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression in existential-risk vs. near-term-harms research divide.').

omega_variable(
    moral_patient_representation_ambiguity,
    'Can future nonexistent persons be represented in governance through proxy institutions (x-risk research organizations), or does their absence from direct voice fundamentally change the legitimacy of decisions made in their name?',
    'Normative argument from political philosophy and participatory governance theory. Empirical check: do actual research priorities set by institutions claiming to represent future persons match what such persons would prefer if they could express preferences? (Proxy measurement: compare longtermist research priorities to near-term communities'' expressed needs.)',
    'If future persons cannot be legitimately represented through institutional proxy, the existential-risk frame loses moral grounding independent of extinction probability. The constraint''s classification would shift from Tangled Rope (genuine coordination + extraction) toward Snare (pure extraction disguised as coordination on behalf of voiceless parties).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_patient_representation_ambiguity, conceptual, 'Legitimacy of proxy representation of nonexistent future persons in governance decisions.').

omega_variable(
    kernel_reading_foreclosure_check,
    'Does the core premise of the existential-risk reading logically foreclose the near-term-harms reading, or do both remain coherent positions within a single framework?',
    'Logical analysis: the existential-risk reading claims that misaligned AGI poses extinction-level risk (empirical claim) and that this risk should be prioritized (normative claim). The near-term-harms reading claims that deployed AI systems cause measurable discrimination today (empirical claim) and that these harms should be prioritized (normative claim). Both claims can be true; the dispute is prioritization, not truth. No logical foreclosure.',
    'Confirms the reading_relations classification as coexists_with (not forecloses): the readings are genuinely contestable alternatives held by different institutional coalitions, not derivatively incompatible. The constraint persists through institutional power, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_check, conceptual, 'Whether existential-risk reading logically forecloses near-term-harms reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_prioritization__existential_risk_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(ai_r_tr_t5, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_prioritization__existential_risk_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_r_tr_t15, observed).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ai_r_tr_t20, observed).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_prioritization__existential_risk_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(ai_r_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_r_be_t5, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ai_r_be_t15, observed).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_r_be_t20, observed).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_r_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(ai_r_su_t5, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_r_su_t15, observed).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_r_su_t20, observed).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ai_r_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The ai_risk_prioritization kernel decomposes into two structurally distinct constraints: existential_risk_reading (this story) and near_term_harms_reading (sibling story). The two readings coexist in contemporary AI governance discourse but compete for institutional authority, funding, and policy priority. Each reading instantiates different victim sets (future humanity vs. present-day affected communities), different timescales (10-100 years vs. immediate), and different beneficiaries (x-risk institutions vs. justice-focused organizations). Each story has its own ε (extraction as measured by resource reallocation and suppression); the two ε values differ substantially because the readings have incompatible victim definitions and prioritization hierarchies. This is kernel decomposition per the ε-invariance principle: when a natural-language concept (AI risk) admits multiple structurally distinct instantiations with different victim sets and extraction profiles, author separate constraint stories for each reading and link them via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
