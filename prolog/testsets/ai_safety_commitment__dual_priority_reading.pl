% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety Dual-Priority Commitment (Both Existential and Near-Term)
 *   domain: technological/governance/risk-assessment
 *
 * SUMMARY:
 *   The AI safety field presents a commitment to addressing both existential
 *   risk (alignment of superintelligent systems) and near-term harms (bias,
 *   labor displacement, misinformation from deployed systems) as
 *   non-competing, equally-prioritized safety concerns. This dual-priority
 *   reading frames safety as a unified commitment spanning timescales from
 *   present-day deployment to potential superintelligence scenarios. However,
 *   the structural reality diverges sharply: institutional resources,
 *   research prestige, and policy leverage flow predominantly toward
 *   existential risk, while near-term harm mitigation remains subordinate.
 *   The constraint achieves this resource asymmetry through rhetorical
 *   unification—allowing near-term safety work to claim legitimacy as
 *   instrumental to existential goals, and allowing existential researchers
 *   to claim comprehensiveness by including near-term concerns in the unified
 *   frame. The victim set is the union of both populations (those suffering
 *   present AI harms and hypothetical future populations at existential
 *   risk), but the mechanisms protecting each are structurally distinct and
 *   compete under resource scarcity. This reading is ONE interpretation of
 *   the contested kernel ai_safety_commitment; the sibling readings
 *   (existential_risk_reading, near_term_harms_reading) instantiate
 *   alternative priority orderings from the same kernel.
 *
 * KEY AGENTS:
 *   - safety_research_institutions: Institutional agenda-setters that coordinate the dual-priority framing and control resource allocation across both research streams; benefit from the framing's appearance of comprehensiveness.
 *   - near_term_harm_affected_populations: Present-day victims of deployed AI systems (workers, marginalized communities, Global South populations); structurally excluded from safety governance and trapped by the constraint's subordination of their immediate concerns.
 *   - future_populations_existential_risk: Hypothetical future beings; their interests are represented by proxy through existential risk researchers but face a structural coherence problem: mechanisms protecting them differ fundamentally from near-term harm mitigation.
 *   - existential_risk_specialists: Beneficiaries whose research is legitimated and resourced through the dual-priority framing; capture disproportionate institutional prominence.
 *   - near_term_safety_researchers: Subordinate beneficiaries who operate under the constraint that their work must frame itself as instrumental to existential safety, not as independent priority.
 *   - governance_coordination_bodies: Institutional agents attempting to enforce the dual-priority framing across policy domains; face coherence pressures from conflicting stakeholder demands.
 *   - affected_workers_advocacy: Excluded advocates for populations experiencing documented harms; operate in separate policy channels with limited leverage over safety research priorities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.62).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety Dual-Priority Commitment (Both Existential and Near-Term)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technological/governance/risk-assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'dd8d4672-4ff7-4176-9caf-ece06dd4739b').
narrative_ontology:cs_kernel_codification('dd8d4672-4ff7-4176-9caf-ece06dd4739b', fixed_text).
narrative_ontology:cs_authority_grounding('dd8d4672-4ff7-4176-9caf-ece06dd4739b', extraction).
narrative_ontology:cs_interpretation_layer_present('dd8d4672-4ff7-4176-9caf-ece06dd4739b').
narrative_ontology:cs_reading_relation('dd8d4672-4ff7-4176-9caf-ece06dd4739b', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd8d4672-4ff7-4176-9caf-ece06dd4739b', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('dd8d4672-4ff7-4176-9caf-ece06dd4739b', foundational, dual_timescale_coherence).
narrative_ontology:cs_axiom_status(dual_timescale_coherence, holdable).
narrative_ontology:cs_axiom_grounding('dd8d4672-4ff7-4176-9caf-ece06dd4739b', dual_timescale_coherence, deontological).
narrative_ontology:cs_axiom('dd8d4672-4ff7-4176-9caf-ece06dd4739b', secondary, institutional_unified_governance).
narrative_ontology:cs_axiom_status(institutional_unified_governance, holdable).
narrative_ontology:cs_axiom_grounding('dd8d4672-4ff7-4176-9caf-ece06dd4739b', institutional_unified_governance, instrumental).
narrative_ontology:cs_reference_frame('dd8d4672-4ff7-4176-9caf-ece06dd4739b', comprehensive_ai_safety_commitment).
narrative_ontology:cs_drift_state('dd8d4672-4ff7-4176-9caf-ece06dd4739b', contemporary_resource_scarcity_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dd8d4672-4ff7-4176-9caf-ece06dd4739b', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, safety_research_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, governance_coordination_bodies).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harm_affected_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, future_populations_existential_risk).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.62) reflects the institutional resource transfer from near-term harm work toward existential risk research, mediated through the dual-priority rhetorical frame. This is not maximum extraction (0.85+) because genuine coordination value exists—unified safety discourse does reduce some fragmentation costs—and because near-term safety work does receive some institutional legitimacy. However, extractiveness is high enough to register asymmetric resource flows and institutional subordination. The suppression score (0.58) reflects active maintenance work: the constraint requires continuous rhetorical effort to claim both priorities are equal while allocating resources asymmetrically; absent this active enforcement (institutional narrative work, funding prioritization, publication practices), the two camps would fragment into visibly separate tracks. Theater ratio (0.41, moderate-high) captures the gap between the stated commitment to dual priority and the actual resource distribution: significant institutional activity is devoted to maintaining the appearance of equal treatment while funding patterns contradict the claim. Accessibility collapse (0.48, low-moderate) reflects that alternatives exist and are visible: the near-term harm framing is fully articulated and has organizational backing (labor, civil rights, labor-focused policy); the existential risk framing is similarly complete. The constraint persists not because alternatives are invisible but because institutional coordination and funding flows favor the dual-priority reading. Resistance (0.72, high) is high because multiple constituencies actively contest this reading: near-term harm advocates push for independent priority and resources; affected populations experience the subordination directly; Global South representations object to the Northern-institution-centric framing. The measurement series show steady extraction accumulation (0.48 → 0.62) over the interval, with theater ratio rising (0.25 → 0.41) and suppression requirement increasing (0.45 → 0.58), consistent with a constraint whose coordination function is atrophying and whose actual operation is increasingly theatrical maintenance of a rhetorical commitment.
 *
 * PERSPECTIVAL GAP:
 *   From the safety research institution seat, the dual-priority framing is a genuine unification of previously fragmented discourse, enabling coordinated research and resource pooling across scales. From the near-term harm-affected populations' seat (both powerless payers), the constraint is a mechanism for subordinating their immediate, documented concerns to speculative long-term scenarios. From existential risk specialists' seat, the framing legitimates their research while appearing to include near-term concerns. From near-term safety researchers' seat, the constraint is a requirement to instrumentalize their work as serving existential goals rather than as independent ethical imperatives. From governance coordination bodies' seat, the dual-priority framing is operationally incoherent because resource scarcity forces real trade-offs at every decision point, yet the constraint requires treating both as equally important. The engine should compute these divergences from the structural data: institutional/beneficiary seats should compute toward rope or light tangled-rope; powerless/victim seats should compute toward snare or heavy tangled-rope depending on exit options and coupling. The perspectival gap is precisely where the tangled-rope classification sits—the constraint has real coordination function (unified safety discourse) but also asymmetric extraction (resource subordination) sustained by active institutional enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by power and exit. Safety research institutions (institutional power, constrained but mobile exit) sit near 0.2–0.3: they benefit from the framing, control its enforcement, and retain exit optionality (can shift focus independently). Existential risk specialists (institutional power, constrained exit) sit near 0.15–0.25: they are the primary beneficiaries, though they cannot fully exit the dual-priority constraint without losing their institutional legitimacy claim to comprehensiveness. Near-term safety researchers (moderate power, constrained exit) sit near 0.45–0.55: they nominally benefit (their work is legitimated within the frame) but are subordinated, and cannot exit without losing funding and publication venues. Affected populations (powerless, trapped exit) sit near 0.75–0.85: they bear documented costs, have no control over the constraint, and face identity-lock (identity_locked for future populations as abstract moral patients; trapped for present-day workers who depend on AI-using sectors for employment). Governance coordination bodies (institutional power, mobile exit) sit near 0.35–0.50: they must enforce a constraint they find incoherent, can partially exit through regulatory capture or jurisdictional withdrawal, but face reputational costs and political pressure if they do. Affected workers' advocates (moderate power, constrained exit) sit near 0.60–0.70: they pay through reduced policy leverage and attention scarcity, have moderate but not mobile exit (can shift to labor-only organizing but lose AI safety policy influence), and must operate in subordinate policy channels. The derivation chain here is: beneficiary/victim declarations + exit structure → directionality per power atom, then domain-specific overrides where necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint faces a live mandatrophy question: the founding problem (coordination failure between existential and near-term communities in the early 2020s) has not been solved by the dual-priority framing; it has been transformed into a structural subordination problem. The mandate was to unify safety discourse across timescales; the actual operation is institutional resource capture by existential risk specialists using the unified framing as cover. The key test: would removing the dual-priority constraint improve or worsen coordination? The tangled-rope classification asserts it would WORSEN coordination in the short term (the two camps would separate again, losing coordination value) but would ENABLE clearer institutional accountability, more honest resource allocation, and more direct service to affected populations. The constraint persists not because it solves the founding problem (it doesn't) but because it provides institutional legitimacy to existential risk research while appearing comprehensive. Mandatrophy is not yet realized (the constraint still provides coordination value, however asymmetric), but it is emergent: as near-term harms accumulate and Global South impacts become more visible, the gap between the dual-priority claim and subordinate near-term resource allocation will widen, creating pressure toward either genuine dual-track resourcing or explicit acknowledgment of a single-priority existential-focused regime. The measurement series show extraction stabilizing at t=20–25 (plateau at 0.62) while theater ratio stabilizes (0.41), suggesting the constraint has reached an equilibrium where the rhetoric is accepted and the subordination is no longer actively intensifying—a sign of normalized mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_scarcity_coherence,
    'Can a single governance institution genuinely pursue both existential risk mitigation and near-term harm prevention as equal priorities when resources are scarce and the interventions require different expertise, timescales, and institutional positions?',
    'Empirical test: examine actual resource allocation patterns and decision-making processes in institutions claiming dual priority. If trade-offs consistently subordinate near-term work, the claim is incoherent; if resources remain genuinely balanced across a full business cycle, coherence is sustained.',
    'If incoherent, the constraint should reclassify from tangled_rope (hybrid coordination/extraction) toward snare (pure extraction with coordination cover story). If coherent, the tangled_rope classification holds and extraction reflects legitimate institutional overhead rather than captured subordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_scarcity_coherence, empirical, 'Whether resource scarcity forces real trade-offs that contradict the dual-priority claim.').

omega_variable(
    moral_weight_of_future_populations,
    'What is the epistemically justified moral weight of hypothetical future populations (at existential risk) relative to present, documented victims of AI harms?',
    'Normative deliberation: moral philosophy, consequentialist frameworks, and stakeholder testimony from affected populations and ethicists. The resolution is conceptual rather than empirical—it depends on which moral framework grounds AI safety governance.',
    'If future populations carry equal or greater moral weight, the dual-priority framing is justified and extraction is legitimate institutional cost. If present populations carry greater weight (due to certainty of harm, present-time priority, or epistemic humility about future scenarios), near-term harm work should be prioritized, and the constraint should reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_weight_of_future_populations, preference, 'Whether the constraint''s victim weighting is morally defensible or smuggles in existential researcher preferences.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the subordination of near-term harm work primarily structural (funding flows, institutional position, publication venues controlled by existential researchers) or internalized (near-term researchers have internalized the existential priority and believe their work should serve it)?',
    'Post-constraint-removal trajectory: if near-term harm work immediately develops independent framing and funding channels when the dual-priority constraint is lifted, suppression was primarily structural. If near-term researchers continue framing their work as instrumental to existential concerns even after institutional pressure is removed, suppression is internalized.',
    'Structural suppression suggests the constraint is an enforcement apparatus maintaining extractive resource allocation; internalized suppression suggests identity-fusion between near-term researchers and existential safety mission, requiring deep institutional culture change to remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether near-term researchers'' subordination is externally enforced or self-perpetuated through mission internalization.').

omega_variable(
    alternative_kernel_readings_exhaustive,
    'Are the three declared sibling readings (dual_priority, existential_risk, near_term_harms) exhaustive of defensible readings of the ai_safety_commitment kernel, or do additional readings exist that are not yet institutionalized?',
    'Discourse analysis and stakeholder consultation: examine whether any normatively coherent reading of ''AI safety'' exists that is not captured by the three declared readings.',
    'If additional readings exist (e.g., a regulatory-compliance reading that treats safety as a business risk rather than a moral commitment, or a labor-integrated reading that centers worker agency in safety governance), the kernel is under-sampled and the constraint family is incomplete. If the three are exhaustive, the constraint family is complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_kernel_readings_exhaustive, conceptual, 'Whether the three declared readings capture all coherent positions or whether the kernel has additional readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__dual_priority_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_s_tr_t25, ai_safety_commitment__dual_priority_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__dual_priority_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ai_s_be_t25, ai_safety_commitment__dual_priority_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.49).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__dual_priority_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(ai_s_su_t25, ai_safety_commitment__dual_priority_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the contested kernel ai_safety_commitment. The dual_priority_reading (this story) claims both timescales are non-competing priorities under unified coordination. The existential_risk_reading isolates superintelligence alignment as the core safety imperative. The near_term_harms_reading isolates deployed-system regulation as the core imperative. All three readings address the same kernel—the institutional commitment to 'AI safety'—but instantiate different constraints by assigning different victim sets, coordination functions, and resource priorities. The network links establish that changes to one reading's institutional position influence the others: if existential risk research loses credibility, the dual-priority reading's coherence is challenged; if near-term harms become visible and costly, pressure mounts to separate the readings and allocate independent resources. Each sibling story should declare this family relationship in its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, powerless, 0.8).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
