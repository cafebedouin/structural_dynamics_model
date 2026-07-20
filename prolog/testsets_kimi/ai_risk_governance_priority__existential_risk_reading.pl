% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: AI Risk Governance Priority: Existential Risk Reading
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This constraint instantiates the existential_risk_reading of the
 *   contested ai_risk_governance_priority kernel. It treats the prevention of
 *   superintelligence scenarios as the overriding priority of AI governance,
 *   coordinating global research and policy capacity around speculative
 *   catastrophic risk. While the underlying extinction risk may constitute a
 *   genuine collective-action problem, the constraint's operation
 *   asymmetrically extracts resources and legitimacy for x-risk institutions
 *   and frontier labs, while deprioritizing demonstrated present harms borne
 *   by marginalized communities. The commitment system is distributed across
 *   technical papers, conference agendas, and institutional strategies rather
 *   than a single codified text.
 *
 * KEY AGENTS:
 *   - ai_safety_leadership_labs: Primary agenda-setter (institutional/arbitrage) â frames risk and captures legitimacy.
 *   - x_risk_research_institutions: Primary beneficiary (organized/mobile) â receives resource flows.
 *   - communities_facing_present_harms: Primary payer (powerless/trapped) â bears opportunity cost of deprioritization.
 *   - future_humanity: Excluded non-agent victim (powerless/trapped) â invoked but voiceless.
 *   - ai_ethics_and_safety_skeptics: Excluded analytical voice (organized/constrained) â structurally marginalized.
 *   - national_governance_bodies: Secondary agenda-setter (institutional/constrained) â administers frameworks imported from labs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.72).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance Priority: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '421d912a-b9ea-44d9-b9bd-9631eee7634e').
narrative_ontology:cs_kernel_codification('421d912a-b9ea-44d9-b9bd-9631eee7634e', distributed).
narrative_ontology:cs_authority_grounding('421d912a-b9ea-44d9-b9bd-9631eee7634e', extraction).
narrative_ontology:cs_interpretation_layer_present('421d912a-b9ea-44d9-b9bd-9631eee7634e').
narrative_ontology:cs_reading_relation('421d912a-b9ea-44d9-b9bd-9631eee7634e', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('421d912a-b9ea-44d9-b9bd-9631eee7634e', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('421d912a-b9ea-44d9-b9bd-9631eee7634e', foundational, superintelligence_priority_over_present_harms).
narrative_ontology:cs_axiom_status(superintelligence_priority_over_present_harms, holdable).
narrative_ontology:cs_axiom_grounding('421d912a-b9ea-44d9-b9bd-9631eee7634e', superintelligence_priority_over_present_harms, empirically_contingent).
narrative_ontology:cs_axiom('421d912a-b9ea-44d9-b9bd-9631eee7634e', foundational, alignment_as_primary_governance_mechanism).
narrative_ontology:cs_axiom_status(alignment_as_primary_governance_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('421d912a-b9ea-44d9-b9bd-9631eee7634e', alignment_as_primary_governance_mechanism, instrumental).
narrative_ontology:cs_reference_frame('421d912a-b9ea-44d9-b9bd-9631eee7634e', existential_risk_priority_framework).
narrative_ontology:cs_drift_state('421d912a-b9ea-44d9-b9bd-9631eee7634e', post_capabilities_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('421d912a-b9ea-44d9-b9bd-9631eee7634e', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_safety_leadership_labs).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, communities_facing_present_harms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the technical safety agenda for frontier AI development, frames existential risk as the overriding governance priority, and captures public funding, talent, and regulatory legitimacy through safety leadership claims. Can pivot to commercial or research agendas if the x-risk narrative loses salience.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_safety_leadership_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% Receives substantial philanthropic and governmental funding for alignment research, adversarial testing, and superintelligence scenario modeling. Institutional growth and researcher livelihoods depend on the existential risk frame remaining central to AI governance.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary,
    organized, generational, mobile, global).

% Bear algorithmic bias, automated surveillance, labor displacement, and environmental extraction from current AI systems. Their harms are classified as non-existential and deprioritized in governance agendas, leaving them without recourse or alternative policy channels.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, communities_facing_present_harms, payer,
    powerless, immediate, trapped, global).

% Their potential is invoked as the ultimate stake justifying governance prioritization, yet they have no present voice, no exit from the policy choices made in their name, and no recourse if those choices misallocate civilizational resources.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% Researchers and advocates who argue for prioritizing near-term harms or question the empirical basis of imminent existential risk timelines. Their frameworks are structurally marginalized in high-level governance forums and funding pools captured by the existential risk narrative.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_ethics_and_safety_skeptics, excluded,
    organized, biographical, constrained, national).

% Policymakers who adopt and enforce governance frameworks oriented toward speculative AGI scenarios, often importing technical framings directly from safety leadership labs. They administer the prioritization but do not independently capture the resource flows.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, national_governance_bodies, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents uncontrolled superintelligence development by coordinating global attention, research capacity, and governance frameworks around catastrophic risk scenarios before they materialize.
% TRANSFER_FUNCTION: Moves financial, human, and political capital from near-term harm mitigation and other governance domains into alignment research, adversarial testing, and speculative AGI governance frameworks, from public and philanthropic sources to x-risk institutions and frontier labs.
% ABSENT_VOICES: Communities experiencing present AI harms, future generations who cannot speak, and researchers skeptical of imminent existential risk timelines are structurally absent from priority-setting forums dominated by technical safety elites.
% DISAPPEARANCE_RATIONALE: If the prioritization vanished, capital and talent would redistribute toward present harms, deployment oversight, and near-term accountability mechanisms; the current institutional architecture of AI safety would contract and reorganize around a broader risk portfolio.
% FOUNDING_PROBLEM: The potential for transformative artificial intelligence to exceed human control and cause extinction or permanent civilizational curtailment.
% FOUNDING_PROBLEM_CORROBORATION: X-risk researchers and some physicists attest to the problem's severity. AI ethics scholars, social scientists, and marginalized community advocates contest both the probability and the framing; corroboration from outside the benefiting institutions is mixed and politically charged.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint diverts massive financial, human, and political capital toward speculative alignment research and AGI governance at the expense of near-term accountability. Suppression (0.65) reflects active marginalization of present-harm frameworks through funding capture, forum control, and epistemic dismissal. Theater ratio (0.55) captures the growing performative dimension of safety work â safety-washing, vague alignment commitments, and scenario planning that outruns empirical validation. Accessibility collapse (0.45) is moderate because alternative governance frameworks exist but are starved of resources rather than legally barred. Resistance (0.58) is substantial and rising, driven by AI ethics communities and affected populations contesting the frame. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of safety leadership labs, the constraint is rope â genuine coordination against an extinction threat that no single actor can unilaterally prevent. From the seat of communities facing present harms, the same structure is snare â their documented injuries are dismissed as non-priority while institutions capture resources in the name of hypothetical futures. The engine computes this divergence from the structural asymmetry in power, exit, and beneficiary declarations; the authored claim does not adjudicate between seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (labs and x-risk institutions) receive low directionality because they control the agenda and can exit or arbitrage. Victims (present-harm communities and future humanity) receive high directionality because they are trapped, voiceless, and bear the costs of the prioritization. Policymakers sit at moderate directionality: they administer enforcement but do not personally capture the extracted gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false binary of pure coordination versus pure extraction. Existential risk prevention is a structurally genuine coordination problem; however, the actual constraint bundles that coordination with asymmetric resource capture and epistemic suppression of alternatives. A pure rope reading would ignore the capture; a pure snare reading would deny the coordination. The tangled_rope classification captures the hybrid: the coordination function is real, but its persistence requires active enforcement that simultaneously extracts from those excluded by the frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_empirical_basis,
    'Is the empirical basis for imminent superintelligence existential risk sufficient to justify the current governance prioritization, or does it rest on unverifiable speculative projections?',
    'Historical tracking of AI capability forecasts versus outcomes; structured expert elicitation with falsifiable probability thresholds; analysis of whether safety investments track demonstrated risk curves.',
    'If the empirical basis is weak, the constraint''s extraction is disproportionate to its coordination function and the resource flow constitutes rent-seeking on speculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_risk_empirical_basis, empirical, 'Whether the existential risk justification is empirically grounded or speculative.').

omega_variable(
    future_humanity_agency,
    'Can future humanity be structurally victimized by a constraint that claims to act on their behalf but excludes them from deliberation?',
    'Philosophical analysis of intergenerational justice and representative legitimacy; no straightforward empirical resolution.',
    'If yes, the constraint''s victim structure is more complex than simple resource diversion, and the high directionality assigned to future humanity reflects a hostage relationship rather than direct extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_agency, conceptual, 'Whether future humanity''s exclusion creates a distinct victim structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of near-term harm frameworks structural (funding allocation, forum exclusion) or internalized (researchers adopting the x-risk frame as the only legitimate concern)?',
    'Funding-flow analysis and citation-network mapping to determine whether near-term research is structurally excluded or self-marginalized; interview data on researcher belief formation.',
    'Structural suppression implies active enforcement by agenda-setters; internalized suppression implies deeper narrative capture that would persist even if formal barriers were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative governance frames.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is the existential_risk_reading of the ai_risk_governance_priority kernel. Would classification change if the bridge_reading or near_term_harms_reading were adopted as the governing frame?',
    'Comparative analysis of resource allocation and stakeholder directionality under each reading; modeling how victim and beneficiary sets shift across the kernel''s readings.',
    'The existential risk reading produces higher extractiveness and a distinct victim set oriented toward future and present communities; alternative readings would redistribute directionality toward populations facing immediate algorithmic harms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'How sibling readings restructure the constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_xrisk_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_xrisk_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ai_xrisk_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ai_xrisk_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(ai_xrisk_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_xrisk_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_xrisk_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_xrisk_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(ai_xrisk_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(ai_xrisk_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_xrisk_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_xrisk_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(ai_xrisk_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(ai_xrisk_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(ai_xrisk_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, bridge_reading).

% DUAL FORMULATION NOTE:
% The ai_risk_governance_priority kernel decomposes into three structurally distinct constraints. The existential_risk_reading treats superintelligence prevention as the overriding priority with high extractiveness on speculative capabilities. The near_term_harms_reading would redistribute directionality toward present marginalized populations. The bridge_reading attempts unified coordination but is structurally pressured by the resource capture of the existential risk reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
