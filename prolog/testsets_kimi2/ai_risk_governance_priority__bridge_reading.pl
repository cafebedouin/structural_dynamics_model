% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance Bridge Reading (Unified Frameworks)
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the bridge reading of the contested
 *   ai_risk_governance_priority kernel. The bridge reading asserts that
 *   present harms and existential risks are structurally entangled and must
 *   be addressed through unified governance frameworks. Structurally, this
 *   functions as a tangled rope: it coordinates genuine cross-field
 *   collaboration (solving siloing) while extracting from both source
 *   communities through broker concentration. Victims include present
 *   marginalized populations (whose immediate demands are deferred) and
 *   future humanity (whose catastrophic-risk urgency is diluted).
 *   Beneficiaries are bridging institutions that capture cross-field citation
 *   authority and funding. The metrics and claim are independently authored:
 *   the claim is tangled_rope because the coordination is real but
 *   asymmetrically extractive; the metrics describe moderate extraction
 *   rising as broker institutions consolidate. Temporal measurements trace
 *   the framework's institutionalization from an initial coordination phase
 *   toward higher theatricality and extraction.
 *
 * KEY AGENTS:
 *   - bridging_institutions: Primary agenda_setter and beneficiary (institutional/arbitrage/global) â brokers cross-field collaboration and captures resource flows.
 *   - x_risk_researchers: Primary payer (organized/constrained/global) â bears agenda dilution and framing costs.
 *   - near_term_harms_advocates: Primary payer (moderate/constrained/national) â bears political abstraction and deferral costs.
 *   - marginalized_populations: Target payer (powerless/trapped/local) â experiences present harms that are tokenized and deferred.
 *   - future_humanity: Non-agent payer (powerless/trapped/universal) â interests invoked but diluted by broker moderation.
 *   - x_risk_purist_advocates and near_term_purist_advocates: Excluded voices â would reject the bridge but are kept out of dominant venues.
 *   - ai_governance_funders: Analytical observer (institutional/analytical/global) â supplies capital without direct experiential stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.55).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance Bridge Reading (Unified Frameworks)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '534f0497-fa33-4f9c-bd49-5a8f24eab627').
narrative_ontology:cs_kernel_codification('534f0497-fa33-4f9c-bd49-5a8f24eab627', distributed).
narrative_ontology:cs_authority_grounding('534f0497-fa33-4f9c-bd49-5a8f24eab627', distributed).
narrative_ontology:cs_reading_relation('534f0497-fa33-4f9c-bd49-5a8f24eab627', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('534f0497-fa33-4f9c-bd49-5a8f24eab627', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('534f0497-fa33-4f9c-bd49-5a8f24eab627', foundational, present_existential_risk_entanglement).
narrative_ontology:cs_axiom_status(present_existential_risk_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('534f0497-fa33-4f9c-bd49-5a8f24eab627', present_existential_risk_entanglement, empirically_contingent).
narrative_ontology:cs_axiom('534f0497-fa33-4f9c-bd49-5a8f24eab627', foundational, unified_governance_mandate).
narrative_ontology:cs_axiom_status(unified_governance_mandate, holdable).
narrative_ontology:cs_axiom_grounding('534f0497-fa33-4f9c-bd49-5a8f24eab627', unified_governance_mandate, instrumental).
narrative_ontology:cs_reference_frame('534f0497-fa33-4f9c-bd49-5a8f24eab627', distributed_collaborative_field).
narrative_ontology:cs_drift_state('534f0497-fa33-4f9c-bd49-5a8f24eab627', broker_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('534f0497-fa33-4f9c-bd49-5a8f24eab627', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, x_risk_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Broker cross-field relationships between AI safety and AI ethics, control high-visibility synthesis publication venues and integrated funding programs, and capture the majority of cross-disciplinary citation authority. Their legitimacy depends on continuously reproducing the entanglement narrative.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary).

% Must present catastrophic-risk arguments through present-harms linkage to access bridge-dominated funding and top-tier publication venues. Direct superintelligence-focused framing is treated as lacking ethics integration and is systematically deprioritized.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, x_risk_researchers, payer,
    organized, civilizational, constrained, global).

% Must translate concrete demands for immediate accountability into abstract long-term governance frameworks acceptable to safety-oriented funders. Political energy for present regulation is redirected into interdisciplinary white papers.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates, payer,
    moderate, biographical, constrained, national).

% Bear the lived costs of deployed AI harms. Their experiences are tokenized as present harms in bridge discourse, but demands for immediate remediation are deferred into future-oriented governance processes they did not design and cannot influence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% Cannot self-advocate. Its interests are invoked by x-risk researchers but are diluted by the bridge framework's demand for inclusive, incremental governance that slows catastrophic-risk mitigation in favor of broker-mediated consensus.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__bridge_reading, future_humanity).

% Would argue that existential risk warrants undiluted urgency and that present-harms integration is an existential distraction. They are structurally excluded from bridge-dominated funding panels and flagship conferences.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, x_risk_purist_advocates, excluded,
    organized, civilizational, constrained, global).

% Would argue that present harms to marginalized populations demand immediate non-abstracted political action. They are excluded from bridge venues that require safety-legitimacy credentials and long-term framing.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_purist_advocates, excluded,
    moderate, biographical, constrained, national).

% Provide capital that bridge institutions compete for. They evaluate success through cross-field collaboration metrics but do not directly experience the constraint's extractive or coordinating dynamics.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_governance_funders, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates two otherwise siloed research communitiesâAI safety and AI ethicsâinto a shared discourse, funding pool, and governance vocabulary, preventing mutual dismissal and enabling joint policy participation.
% TRANSFER_FUNCTION: Moves intellectual authority, citation credibility, and funding from isolated safety and ethics silos into brokered integrated research programs; moves political energy from present marginalized populations and future humanity into abstract long-term governance frameworks administered by bridging institutions.
% ABSENT_VOICES: Existential-risk purists who view present-harms integration as diluting extinction urgency, and near-term-harms purists who view x-risk framing as co-optation of concrete injustice. Both are structurally underrepresented in bridge-controlled venues.
% DISAPPEARANCE_RATIONALE: If the bridge framework vanished, cross-field citation networks and integrated funding streams would collapse. The fields might re-balkanize, or they might reorganize around more direct, less brokered advocacy. Bridging institutions would lose their structural role, while purists would regain venue access.
% FOUNDING_PROBLEM: Balkanization of AI governance into disconnected existential-safety and AI-ethics communities that mutually dismissed each other's concerns, producing policy incoherence and duplicated oversight gaps.
% FOUNDING_PROBLEM_CORROBORATION: Early field historians and policy white papers from Science and Technology Studies scholars and early NSF program officers attest the siloing. However, beneficiaries (bridging institutions) now dominate corroboration narratives, and outside critics argue the bridge has created a new extraction problem.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate because the coordination function is genuine: cross-field collaboration does reduce siloing and policy incoherence. However, the same structure concentrates authority in bridging institutions that do not bear the costs of the present harms or existential risks they moderate. Suppression (0.65) reflects active enforcement through funding gatekeeping, citation network control, and venue exclusion of purist framings. Theater_ratio (0.50) captures the rising share of performative integration activity (synthesis papers, bridge conferences) relative to substantive governance outcomes. Accessibility_collapse (0.55) indicates that pure near-term or pure x-risk advocacy is still possible but increasingly marginal in top-tier venues. Resistance (0.50) reflects active pushback from both purist communities. The measurement series use a single shared grid (0â10) to avoid misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The bridging institutions experience the constraint as genuine coordination they built and maintain; their seat computes toward rope. The payer seatsâx-risk researchers, near-term advocates, marginalized populations, and future humanityâexperience extraction through agenda dilution, political deferral, and tokenization; their seats compute toward snare. The engine derives this divergence from the structural asymmetry in exit options (arbitrage for brokers vs trapped/constrained for payers) and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions are declared beneficiaries and agenda_setters with arbitrage-grade exit, placing their directionality near the full-beneficiary end (low d). Marginalized populations and future humanity are declared victims with trapped exit, placing them near the full-target end (high d). X-risk and near-term researchers are intermediate: they retain some mobility but are constrained by funding and venue dependence, yielding elevated but not maximal d. The effective extraction chi is therefore amplified for the most powerless and damped for the institutional brokers.
 *
 * MANDATROPHY ANALYSIS:
 *   The bridge framework was built to solve genuine balkanization. The mandatrophy question is whether that problem is now dead and the framework persists as extraction. The founding_problem_status is contested: early siloing is documented, but current broker concentration suggests the solution has ossified into a new problem. If the problem were definitively dead and the framework purely inertial, it would reclassify as piton; if the extraction were pure cover with no coordination, as snare. The tangled_rope claim is upheld by the continuing existence of real coordination (shared vocabularies, joint policy participation) alongside asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    broker_concentration_naturalness,
    'Is the concentration of bridge authority in a small fraction of papers and broker actors a natural outcome of interdisciplinary synthesis, or a constructed bottleneck that extracts from both source fields?',
    'Citation-network analysis and funding-trail mapping to measure whether cross-field integration decentralizes over time or consolidates around the same broker nodes.',
    'If natural, the extraction is coordination cost; if constructed, the framework functions as a snare disguised as coordination and warrants institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_concentration_naturalness, empirical, 'Whether broker concentration is organic or extractive bottleneck').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of purist x-risk and near-term voices structural (enforced by funding and venue gatekeeping) or internalized (adopted as professional identity by researchers seeking legitimacy)?',
    'Track researcher framing shifts post-tenure or post-funding-change; if purist framing re-emerges when structural incentives relax, suppression is primarily structural.',
    'If internalized, effective suppression exceeds the structural measure and the constraint is more deeply embedded than visible enforcement suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of dissenting framings').

omega_variable(
    bridge_reading_victim_priority,
    'Does the bridge reading extract more from present marginalized populations (by deferring immediate regulatory intervention) or from future humanity (by diluting existential-risk urgency)?',
    'Comparative policy-impact analysis measuring whether bridge-framework outputs produce more near-term protective regulation or more long-term safety investment relative to counterfactual pure-advocacy scenarios.',
    'Resolution would shift the directionality profile: a near-term extraction skew would reclassify the bridge as nearer to a snare on marginalized populations; a future extraction skew would align it with dilution of x-risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bridge_reading_victim_priority, conceptual, 'Which victim seat bears the greater extraction').

omega_variable(
    kernel_sibling_relation_nature,
    'Does the bridge reading create genuine integration between existential-risk and near-term-harms readings, or does it structurally subsume both into a broker-managed middle ground?',
    'Measure resource and citation flows: if bridge institutions serve as obligatory passage points without which the sibling readings cannot reach funders or policymakers, the relation is subsumption; if they serve as translators that leave both readings intact and resourced, it is integration.',
    'If subsumption, the bridge reading is extractive toward both siblings and the kernel is a commitment system with extraction-based authority; if integration, the reading is a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_relation_nature, conceptual, 'Whether bridge reading integrates or subsumes sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_bridge_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_risk_bridge_tr_t2, ai_risk_governance_priority__bridge_reading, theater_ratio, 2, 0.34).
narrative_ontology:measurement(ai_risk_bridge_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(ai_risk_bridge_tr_t6, ai_risk_governance_priority__bridge_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(ai_risk_bridge_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(ai_risk_bridge_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(ai_risk_bridge_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_risk_bridge_be_t2, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2, 0.43).
narrative_ontology:measurement(ai_risk_bridge_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(ai_risk_bridge_be_t6, ai_risk_governance_priority__bridge_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(ai_risk_bridge_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(ai_risk_bridge_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_bridge_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_risk_bridge_su_t2, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2, 0.49).
narrative_ontology:measurement(ai_risk_bridge_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement(ai_risk_bridge_su_t6, ai_risk_governance_priority__bridge_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(ai_risk_bridge_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(ai_risk_bridge_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the ai_risk_governance_priority kernel family. The bridge reading decomposes from the same natural-language governance mandate as the existential-risk and near-term-harms readings, but with a distinct epsilon, beneficiary/victim structure, and stakeholder surface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
