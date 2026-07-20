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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Bridge Reading: Unified AI Risk Governance Framework
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint instantiates the bridge reading of the contested kernel
 *   'ai_risk_governance_priority': the claim that AI governance must treat
 *   present harms and existential risks as structurally entangled concerns
 *   requiring unified frameworks. The reading is defended by a small set of
 *   cross-field broker institutions that account for a disproportionate share
 *   of linking activity between AI safety and AI ethics. The constraint
 *   coordinates genuine interconnection but concentrates resources and
 *   prestige in the bridging layer, while subordinating both pure near-term
 *   harm advocates and pure existential-risk researchers. Future humanity and
 *   present marginalized populations bear diffuse costs of either dilution or
 *   delay. The kernel's sibling readingsâexistential_risk_reading and
 *   near_term_harms_readingâare held by different communities and coexist
 *   in public dispute; this reading does not logically foreclose either but
 *   influences their legitimacy conditions by occupying the center of funding
 *   and policy discourse.
 *
 * KEY AGENTS:
 *   - cross_field_bridge_institutions: Agenda-setters who control integrative venues and cross-citation networks
 *   - ai_governance_funders: Beneficiaries who gain administrative simplicity from a unified portfolio
 *   - present_marginalized_populations: Payers whose immediate material claims are subsumed into abstract frameworks
 *   - future_humanity: Payers whose existential interests are mediated through fragile present-day brokers
 *   - x_risk_research_purists: Payers whose focused agenda is diluted by integration demands
 *   - near_term_harms_advocates: Payers forced to align with long-term narratives to access shared venues
 *   - critical_ai_studies_scholars: Observers analyzing broker concentration and performative integration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.48).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.55).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Bridge Reading: Unified AI Risk Governance Framework").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '8725c378-0441-4bf2-b688-049a9353c73f').
narrative_ontology:cs_kernel_codification('8725c378-0441-4bf2-b688-049a9353c73f', distributed).
narrative_ontology:cs_authority_grounding('8725c378-0441-4bf2-b688-049a9353c73f', practice).
narrative_ontology:cs_interpretation_layer_present('8725c378-0441-4bf2-b688-049a9353c73f').
narrative_ontology:cs_reading_relation('8725c378-0441-4bf2-b688-049a9353c73f', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('8725c378-0441-4bf2-b688-049a9353c73f', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('8725c378-0441-4bf2-b688-049a9353c73f', foundational, entanglement_is_structural).
narrative_ontology:cs_axiom_status(entanglement_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('8725c378-0441-4bf2-b688-049a9353c73f', entanglement_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('8725c378-0441-4bf2-b688-049a9353c73f', foundational, unified_frameworks_are_necessary).
narrative_ontology:cs_axiom_status(unified_frameworks_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('8725c378-0441-4bf2-b688-049a9353c73f', unified_frameworks_are_necessary, instrumental).
narrative_ontology:cs_reference_frame('8725c378-0441-4bf2-b688-049a9353c73f', unified_safety_ethics_field).
narrative_ontology:cs_drift_state('8725c378-0441-4bf2-b688-049a9353c73f', post_integrative_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8725c378-0441-4bf2-b688-049a9353c73f', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, cross_field_bridge_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ai_governance_funders).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, x_risk_research_purists).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce integrative conferences, journals, and policy frameworks that explicitly link AI safety and AI ethics. They set the terms of unification, control cross-citation networks, and receive concentrated funding and prestige from maintaining the bridge. Their survival depends on the continued perception that the two fields are inseparable.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, cross_field_bridge_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Philanthropic and state funders who prefer a single 'AI governance' portfolio. They benefit from reduced administrative complexity and a unified narrative that justifies large, coherent grant-making strategies rather than fragmented safety or fairness portfolios.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_governance_funders, beneficiary,
    powerful, generational, mobile, global).

% Experience algorithmic bias, surveillance, and labor displacement. Their specific demands for immediate regulatory and distributive remedies are subsumed into abstract 'responsible AI' or long-term governance frameworks, diluting political urgency and redirecting resources toward research that does not address their material conditions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Bears the risk of catastrophic or existential outcomes from advanced AI. Their interests are mediated entirely through present-day bridging institutions; they have no direct voice and depend on the unified framework actually producing mitigation, rather than dissipating x-risk focus into diffuse governance talk.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Believe that superintelligence risk requires dedicated, focused technical and institutional effort. They experience the unified framework as forcing terminological and political compromises that weaken the specificity of x-risk arguments and divert talent to broad governance questions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, x_risk_research_purists, payer,
    moderate, civilizational, identity_locked, global).

% Advocate for immediate regulation of discriminatory or exploitative AI systems. They are pressured by the unified framework to align their campaigns with long-term speculative risk narratives in order to access shared funding and policy venues, fragmenting their grassroots coalitions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates, payer,
    moderate, biographical, constrained, national).

% Analyze the structural incentives of the bridge, documenting the concentration of broker institutions, the asymmetry of who pays and who benefits, and the performative nature of much 'integration' that does not redistribute power or resources to the marginalized or to safety-critical research.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, critical_ai_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents destructive siloing between AI safety and AI ethics communities, creating shared vocabularies and funding streams that reduce duplicated effort and political vulnerability to critics who played the communities against each other.
% TRANSFER_FUNCTION: Moves attention, funding, and epistemic prestige from specialized near-term harm and existential risk research communities to cross-cutting integrative institutions and broker researchers who manage the unified framework.
% ABSENT_VOICES: Grassroots organizers facing immediate algorithmic harm, technical safety researchers committed to pure superintelligence preparedness, and future generations are partially or fully excluded from the broker-dominated integrative venues.
% DISAPPEARANCE_RATIONALE: If the unified framework imperative vanished, funding streams would bifurcate back into safety and ethics silos, broker institutions would lose their coordinating role, and the current integrative journals and conferences would reorganize around narrower mandates or dissolve.
% FOUNDING_PROBLEM: AI ethics and AI safety operated in disconnected communities with overlapping technologies but non-overlapping conferences, vocabularies, and funding sources, leading to duplicated effort, missed opportunities for shared tooling, and political vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Early bridge-building researchers and philanthropic program officers attest to the silo problem. Critical STS scholars and field sociologists outside both beneficiary camps attest that the silo problem was real but has been superseded by a broker-concentration problem; no independent corroboration exists that the current broker-heavy structure is the only or best solution.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.48) because the unified framework does produce genuine coordination benefitsâreduced duplication and shared toolingâbut it also systematically transfers attention and resources to broker institutions that do not bear the costs of dilution. Suppression (0.55) is moderate: pure near-term and pure x-risk framings are not violently excluded, but they are marginalized in high-status funding and publication venues that require 'bridge-friendly' framing. Theater ratio (0.42) reflects the growing share of integrative activity that is performativeâconferences and white papers that re-state entanglement without altering power distributions or producing actionable standards. Accessibility collapse (0.60) captures the difficulty of proposing genuinely separated governance streams once the unified frame has achieved institutional dominance. Resistance (0.50) reflects active pushback from both x-risk purists and near-term advocates who experience the bridge as co-optation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination preventing destructive fragmentation; the payer seats experience it as a leaky umbrella that channels rain onto them while the broker holds the handle. The engine should compute divergent per-seat types: the bridge institutions may see a rope or tangled rope, while the marginalized and purists see extraction. The authored claim of tangled_rope captures the hybrid structure without adjudicating the perspectival dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridge institutions and funders sit near the beneficiary end: they collect prestige, citations, and administrative efficiency from the unified frame. Present marginalized populations and future humanity sit near the target endâthey bear the costs of diluted urgency and mediated, fragile representation. X-risk purists and near-term advocates are also targets (high d) because their specialized agendas are structurally subordinated, though they retain more voice than the fully excluded. The observer seat sees the asymmetry clearly but does not participate in the resource flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents pure mandatrophy by preserving a live coordination function: the silo problem was real, and some integration genuinely reduces waste. However, the founding problem's status is contested because the current broker-heavy architecture may have outlived the original silo problem. The bridge risks becoming a piton if the coordination function atrophies entirely into performative interdisciplinarity while the institutional shell persists. The theater_ratio and temporal measurements are authored to catch this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    broker_concentration_fragility,
    'Is the unified framework structurally resilient, or does its collapse risk follow from the extreme concentration of broker actors controlling cross-field links?',
    'Network analysis of co-authorship and citation graphs measuring Gini coefficient of bridge-institution centrality; longitudinal tracking of whether policy outcomes persist when key broker researchers exit.',
    'If fragility is high, the constraint is a scaffold or piton rather than a stable rope; if low, the bridge may be a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_concentration_fragility, empirical, 'Whether bridge governance depends on a fragile handful of brokers.').

omega_variable(
    future_humanity_victim_status,
    'Does the bridge reading serve future humanity''s interests better than a pure existential-risk reading by building broader coalitions, or does it dilute x-risk mitigation to the point of increasing their vulnerability?',
    'Counterfactual policy analysis comparing x-risk-relevant regulatory output under unified versus specialized governance frames, combined with expert elicitation from outside the bridge institutions.',
    'If future humanity are net beneficiaries, the victim declaration and directionality derivation invert; if net victims, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_victim_status, conceptual, 'Whether future humanity are beneficiaries or victims of the bridge.').

omega_variable(
    integration_performativity,
    'Does the unified framework produce actionable governance outcomes that neither specialized near-term nor specialized x-risk work could achieve alone, or does it primarily generate performative ''interdisciplinary'' outputs that leave power and resource distributions unchanged?',
    'Outcome auditing of integrated framework deliverables for concrete policy mechanisms, resource redistribution, or technical standards that would not have emerged from siloed efforts.',
    'If performative, theater_ratio is higher than authored and the constraint slides toward piton; if productive, extractiveness may be lower and the coordination function stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_performativity, empirical, 'Whether integration is functional or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__bridge_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__bridge_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__bridge_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the bridge reading of the ai_risk_governance_priority kernel, decomposed from the colloquial label into structurally distinct claims. The existential_risk_reading and near_term_harms_reading are sibling constraints with different epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
