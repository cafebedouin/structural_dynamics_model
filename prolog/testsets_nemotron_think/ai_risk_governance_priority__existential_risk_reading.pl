% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Existential Risk Priority Mandate in AI Governance
 *   domain: technological/governance/ethical
 *
 * SUMMARY:
 *   This constraint story represents the existential_risk_reading of the
 *   contested ai_risk_governance_priority kernel. The reading asserts that AI
 *   governance must prioritize preventing superintelligence scenarios above
 *   all other AI risks. Structurally, this operates as a tangled rope: it
 *   coordinates genuine extinction-prevention research (coordination
 *   function) while extracting resources from present harm mitigation and
 *   locking future humanity into alignment-as-control paradigms (asymmetric
 *   extraction). The mandate requires active enforcement through funding
 *   directives, regulatory frameworks, and institutional pressure.
 *   Beneficiaries are x-risk research institutions and AI labs leveraging
 *   safety leadership; victims include present-harm affected communities
 *   whose issues are deprioritized and future humanity who may inherit
 *   maladaptive governance lock-in.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.75).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.6).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential Risk Priority Mandate in AI Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technological/governance/ethical").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'f68ee931-0146-4b9f-b743-a50b7b5bc40d').
narrative_ontology:cs_kernel_codification('f68ee931-0146-4b9f-b743-a50b7b5bc40d', distributed).
narrative_ontology:cs_authority_grounding('f68ee931-0146-4b9f-b743-a50b7b5bc40d', expertise).
narrative_ontology:cs_interpretation_layer_present('f68ee931-0146-4b9f-b743-a50b7b5bc40d').
narrative_ontology:cs_reading_relation('f68ee931-0146-4b9f-b743-a50b7b5bc40d', ai_risk_governance_priority__near_term_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('f68ee931-0146-4b9f-b743-a50b7b5bc40d', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('f68ee931-0146-4b9f-b743-a50b7b5bc40d', foundational, existential_risk_lexical_priority).
narrative_ontology:cs_axiom_status(existential_risk_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('f68ee931-0146-4b9f-b743-a50b7b5bc40d', existential_risk_lexical_priority, instrumental).
narrative_ontology:cs_axiom('f68ee931-0146-4b9f-b743-a50b7b5bc40d', secondary, speculative_capability_tracking_justifies_resource_allocation).
narrative_ontology:cs_axiom_status(speculative_capability_tracking_justifies_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('f68ee931-0146-4b9f-b743-a50b7b5bc40d', speculative_capability_tracking_justifies_resource_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('f68ee931-0146-4b9f-b743-a50b7b5bc40d', extinction_prevention_primacy).
narrative_ontology:cs_drift_state('f68ee931-0146-4b9f-b743-a50b7b5bc40d', contemporary_ai_scaling_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f68ee931-0146-4b9f-b743-a50b7b5bc40d', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, existential_risk_lexical_priority).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, alignment_as_control_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the intellectual agenda for AI safety research, defining what counts as alignment progress and which capabilities warrant concern. Receive dedicated funding streams, talent pipelines, and institutional legitimacy from the priority mandate. Can redirect research focus but face pressure to produce legible progress on control methods.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary).

% Gain regulatory goodwill, talent attraction, and competitive moats by publicly committing to existential safety frameworks. Extract resource allocation toward alignment-as-control research that also advances capabilities. Can pivot between safety framing and capability acceleration; exit options include open-sourcing or jurisdictional arbitrage.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    institutional, biographical, arbitrage, global).

% Marginalized populations experiencing algorithmic bias, labor displacement, surveillance, and misinformation today. Resources that could mitigate these harms are diverted to speculative superintelligence prevention. No meaningful exit from the governance framework that deprioritizes their concerns; organizing capacity is limited by the very harms they face.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_communities, payer,
    powerless, immediate, trapped, global).

% The purported ultimate beneficiary of the priority mandate, but structurally excluded from consent or contestation. If the mandate locks in maladaptive governance paradigms or accelerates capabilities under safety cover, future generations inherit the consequences without voice. Their 'victim' status in the structural analysis reflects potential lock-in to alignment-as-control paradigms that may foreclose better futures.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% Researchers, civil society groups, and policymakers focused on demonstrated algorithmic harms. Their advocacy is structurally marginalized in priority-setting venues where existential risk framing dominates funding and attention. Exit requires either adopting x-risk vocabulary or accepting reduced influence; some defect to bridge frameworks.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harm_advocates, excluded,
    moderate, biographical, constrained, global).

% Translate competing risk framings into regulatory frameworks (EU AI Act, US Executive Orders, international standards). Face pressure from x-risk institutions to encode priority mandates and from near-term advocates to address present harms. Their choices determine which structural reading becomes embedded in law.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_governance_policymakers, agenda_setter,
    institutional, biographical, mobile, national).

% Evaluates the constraint's structural operation across all seats: whether the priority mandate functions as genuine coordination for extinction prevention, as extraction from present harms, or as a tangled rope with both properties. Sees the full resource flows, capability trajectories, and governance dynamics.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research effort toward AI alignment and control methods to prevent loss-of-control scenarios from superintelligent systems
% TRANSFER_FUNCTION: Moves funding, talent, and regulatory attention from present algorithmic harm mitigation toward speculative superintelligence risk prevention and alignment-as-control research
% ABSENT_VOICES: Present-harm affected communities (marginalized populations facing bias, displacement, surveillance) and near-term harm advocates are structurally excluded from priority-setting; future humanity cannot be present to consent or object to the governance paradigms chosen on their behalf
% DISAPPEARANCE_RATIONALE: Without the existential risk priority mandate, AI governance would reorient around demonstrated harms; x-risk institutions would lose dedicated funding streams; AI labs would lose safety-leadership regulatory moats; alignment-as-control research would compete on equal footing with other AI ethics priorities
% FOUNDING_PROBLEM: The perceived inevitability of superintelligent AI development and the catastrophic consequences of misalignment created urgency for dedicated governance priority overriding all other AI risk considerations
% FOUNDING_PROBLEM_CORROBORATION: X-risk researchers (Bostrom, Yudkowsky, Ord) attest the problem is live and urgent; AI capabilities researchers and near-term harm advocates attest the problem is speculative and diverts resources from present suffering; no consensus exists outside the x-risk community; the bridge reading explicitly contests the mutual exclusivity premise
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.75) is high because the priority mandate diverts substantial resources from demonstrated harms to speculative risks — the opportunity cost is real and measurable in funding allocations. Suppression (0.60) reflects active marginalization of near-term harm advocacy in governance venues, not merely passive neglect. Theater ratio (0.30) captures performative alignment work that advances capabilities under safety cover. Accessibility collapse (0.50) is moderate: near-term harm frameworks persist but are structurally disadvantaged. Resistance (0.65) is significant from near-term advocates and affected communities. The claimed type (tangled_rope) reflects genuine coordination + asymmetric extraction; the engine will compute per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk institution seat, the constraint is a rope coordinating extinction prevention. From the present-harm community seat, it is a snare extracting from the vulnerable. From the AI lab seat, it is a scaffold providing regulatory cover for capability advancement. The engine computes these divergences from the declared power/exit/role structure; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutions and AI labs are structural beneficiaries (d near 0.0-0.2): they collect funding, legitimacy, and regulatory moats. Present-harm communities are full targets (d near 1.0): they bear the opportunity cost with trapped exit. Future humanity is identity-locked (d ~0.7): purported beneficiary but structurally constrained by paradigms chosen without consent. Near-term advocates are constrained (d ~0.6): they can defect to bridge frameworks but lose priority-standing. Policymakers are symmetric (d ~0.5): they mediate but capture agenda-setting rents. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inevitable superintelligence + catastrophic misalignment) remains contested. If superintelligence timelines extend or prove infeasible, the mandate's coordination function atrophies while its extraction from present harms persists — a classic mandatrophy trajectory toward piton. The bridge reading's contention that both risk categories are entangled challenges the mutual exclusivity premise. The constraint's persistence depends on maintaining the urgency framing; evidence of capability plateaus would degrade its coordination legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure,
    'This constraint is one reading of the ai_risk_governance_priority kernel; how does the kernel''s contested nature affect classification stability across readings?',
    'Cross-reading structural comparison: each reading authors its own ε, beneficiary/victim sets, and type. The engine computes per-reading classifications; divergence across readings measures the kernel''s structural ambiguity.',
    'If sibling readings compute to different types (e.g., near_term_harms_reading = rope, bridge_reading = scaffold), the kernel itself is a site of structural contestation, not a stable constraint. This reading''s classification is reading-indexed, not kernel-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure, conceptual, 'Kernel-reading committer structure: this reading''s ε, beneficiaries, victims, and type are instantiated from one framing of a contested kernel.').

omega_variable(
    victim_ambiguity_future_humanity,
    'Is future humanity a victim of this constraint (locked into alignment-as-control paradigms that may foreclose better futures) or the intended beneficiary (protected from extinction)?',
    'Counterfactual governance trajectory analysis: if the priority mandate had not existed, would future humanity face better or worse outcome distributions? Requires modeling capability trajectories under alternative governance regimes.',
    'If future humanity is net victim, the constraint''s extraction is amplified (victim set includes the purported beneficiary). If net beneficiary, the extraction falls primarily on present-harm communities. Changes the moral geometry of the tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_ambiguity_future_humanity, conceptual, 'Whether the constraint''s claimed beneficiary (future humanity) is structurally a victim of the governance lock-in it creates.').

omega_variable(
    capability_trajectory_uncertainty,
    'Do speculative capability projections (scaling laws, takeover scenarios) justify current resource allocation toward alignment-as-control, or is the extraction premature?',
    'Empirical tracking of capability milestones vs. projected timelines; retrospective assessment of whether early alignment investment accelerated or retarded safe capability development.',
    'If projections are systematically overconfident, the constraint''s extractiveness is higher than warranted (resources diverted on false premises). If projections track, the coordination function is vindicated and extraction is the price of insurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_trajectory_uncertainty, empirical, 'Whether the speculative capabilities driving high ε are empirically grounded or narrative-driven.').

omega_variable(
    suppression_mechanism_near_term_marginalization,
    'Is the marginalization of near-term harm advocacy structural (funding mandates, venue exclusion) or internalized (community self-censorship to access x-risk funding)?',
    'Post-exit suppression trajectory: track near-term advocates who pivot to x-risk framing — does their advocacy for present harms recover if they leave x-risk venues?',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint reshapes advocate identity. If purely structural, coalition-building across risk categories remains possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_near_term_marginalization, empirical, 'Structural vs. internalized suppression of near-term harm advocacy under existential risk priority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(ai_r_tr_t2, observed).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(ai_r_tr_t4, observed).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(ai_r_tr_t6, observed).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement_basis(ai_r_tr_t8, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(ai_r_tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement_basis(ai_r_be_t2, observed).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement_basis(ai_r_be_t4, observed).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 6, 0.67).
narrative_ontology:measurement_basis(ai_r_be_t6, observed).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement_basis(ai_r_be_t8, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(ai_r_be_t10, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement_basis(ai_r_su_t2, observed).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement_basis(ai_r_su_t4, observed).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(ai_r_su_t6, observed).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(ai_r_su_t8, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(ai_r_su_t10, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.15).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the ai_risk_governance_priority kernel into three readings with distinct ε values: existential_risk_reading (high ε on speculative capabilities), near_term_harms_reading (high ε on present algorithmic bias), bridge_reading (moderate ε on both). The existential reading's resource claims structurally pressure the bridge reading's unified-framework viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
