% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaties as Nation-to-Nation Agreements (Indigenous Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'nation-to-nation' reading of
 *   historical treaties, which posits them as international agreements
 *   between sovereign equals, requiring ongoing consent and subject to modern
 *   treaty law principles. From this perspective, the current operational
 *   reality, where settler states often act unilaterally, is highly
 *   extractive and suppressive. The constraint (the ideal of nation-to-nation
 *   treaties) is a Tangled Rope because it has a genuine coordination
 *   function but is enforced asymmetrically, leading to ongoing extraction
 *   from Indigenous nations. This reading contrasts with the
 *   'extinguishment_reading' (treaties as property transactions) and the
 *   'stewardship_reading' (treaties as relational pacts for shared
 *   stewardship).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.75).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.78).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaties as Nation-to-Nation Agreements (Indigenous Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '44051a7d-e0ec-4d62-a6fe-e98ae25675b3').
narrative_ontology:cs_kernel_codification('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', fixed_text).
narrative_ontology:cs_authority_grounding('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', lineage).
narrative_ontology:cs_interpretation_layer_present('44051a7d-e0ec-4d62-a6fe-e98ae25675b3').
narrative_ontology:cs_reading_relation('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', foundational, indigenous_nations_retain_inherent_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_retain_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', indigenous_nations_retain_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', foundational, treaties_are_living_agreements).
narrative_ontology:cs_axiom_status(treaties_are_living_agreements, holdable).
narrative_ontology:cs_axiom_grounding('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', treaties_are_living_agreements, conventional).
narrative_ontology:cs_reference_frame('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', original_nation_to_nation_intent).
narrative_ontology:cs_drift_state('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', contemporary_legal_discourse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('44051a7d-e0ec-4d62-a6fe-e98ae25675b3', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_companies).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_citizens).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of unilateral resource extraction and land dispossession when treaties are not honored as nation-to-nation agreements. They are beneficiaries when their inherent sovereignty and consent rights are respected, leading to equitable co-governance and resource sharing. Their identity is deeply tied to their ancestral lands and treaty relationships.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary).

% Historically administer and interpret treaties, often unilaterally, benefiting from access to land and resources. They are constrained by international law and domestic legal challenges to uphold treaty obligations, but often resist ceding control. They benefit from the current extractive interpretation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, beneficiary).

% Directly benefit from settler state interpretations that allow unilateral access to lands and resources without requiring free, prior, and informed consent from Indigenous nations. They face increased costs and delays if nation-to-nation principles are fully implemented.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_companies, beneficiary,
    powerful, immediate, mobile, regional).

% Monitor and advocate for the application of international human rights and Indigenous rights standards (e.g., UNDRIP) to treaty interpretation, providing a normative framework that supports the nation-to-nation reading. They do not directly enforce but exert moral and legal pressure.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Indirectly benefit from the resources and economic activity generated by settler state control over treaty lands. They may bear indirect costs through legal challenges and social unrest arising from treaty disputes, or through taxes if reparations are made.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_citizens, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_citizens, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for peaceful coexistence, shared land use, and mutual respect between sovereign Indigenous nations and settler states, preventing conflict and ensuring equitable resource sharing based on ongoing consent.
% TRANSFER_FUNCTION: When violated, it facilitates the unilateral transfer of land, resources, and governance authority from Indigenous nations to settler states. When upheld, it mandates shared governance and equitable distribution of benefits.
% ABSENT_VOICES: Future generations of Indigenous peoples, whose inherent rights and relationship to land are directly impacted by current treaty interpretations; non-human entities (land, water, animals) whose well-being is tied to treaty observance and whose voices are mediated through Indigenous legal traditions.
% DISAPPEARANCE_RATIONALE: If the concept of treaties as nation-to-nation agreements vanished, the legal and moral basis for Indigenous rights and claims would be severely undermined, leading to intensified conflicts over land and resources, and a complete breakdown of any pretense of shared governance or reconciliation efforts. The entire legal and political landscape would be destabilized.
% FOUNDING_PROBLEM: To formalize relationships, establish boundaries, and enable peaceful settlement and resource access in newly encountered territories, often following periods of conflict or in anticipation of it, while recognizing the pre-existing sovereignty of Indigenous nations.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous oral histories, legal scholars specializing in Indigenous law, international legal experts, and UN declarations (e.g., UNDRIP) corroborate the ongoing nature and nation-to-nation intent of treaties. Settler state legal interpretations often contradict this, claiming the problem is resolved or that Indigenous sovereignty was ceded.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) and suppression (0.78) reflect the current state of affairs as seen by the nation-to-nation reading: settler states continue to benefit from unilateral control over treaty lands and resources, actively suppressing Indigenous self-determination and consent rights. Resistance is high (0.70) due to ongoing Indigenous activism and legal challenges. Theater ratio (0.35) indicates that while there are performative gestures of reconciliation and recognition, substantive changes to power dynamics and resource control are often lacking. The claimed type is Tangled Rope because the ideal of nation-to-nation relations is a genuine coordination function, but its implementation is characterized by asymmetric enforcement and extraction, requiring active struggle to uphold.
 *
 * PERSPECTIVAL GAP:
 *   The settler state's perspective often frames treaties as historical land cessions, minimizing ongoing obligations and Indigenous sovereignty. The nation-to-nation reading fundamentally challenges this, asserting a continuous, evolving relationship between co-equal sovereigns. This gap leads to divergent classifications of the same historical documents: for settler states, it might appear as a Rope (settlement) or Piton (historical artifact); for Indigenous nations, it is a Tangled Rope or Snare due to ongoing violations and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primarily targets (payers) of the current extractive interpretation, bearing the costs of land and resource loss, and the suppression of their sovereignty. Settler state governments and resource extraction companies are beneficiaries, profiting from the unilateral control and access. International legal bodies and settler citizens act as observers or indirect beneficiaries/payers, with varying degrees of influence and awareness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_interpretation_ambiguity,
    'Is the current interpretation of historical treaties by settler states a genuine reflection of original intent, or a post-hoc justification for resource extraction?',
    'Forensic historical and legal analysis, including Indigenous oral histories and international legal precedents, to reconstruct original intent and compare with current practice.',
    'If a post-hoc justification, the constraint''s extractiveness is confirmed as structural and intentional, strengthening the Snare classification; if genuine, it supports a more complex Tangled Rope where misinterpretation is the primary driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_interpretation_ambiguity, empirical, 'Ambiguity in treaty interpretation and its impact on extraction.').

omega_variable(
    sovereignty_recognition_gap,
    'To what extent do settler states genuinely recognize Indigenous nations as co-equal sovereigns, rather than as subordinate entities with limited rights?',
    'Analysis of legislative actions, court rulings, and policy implementation regarding Indigenous self-determination, land rights, and consent requirements for resource development.',
    'If recognition is minimal or performative, the constraint operates as a Snare, with the coordination narrative serving as cover; if substantive, it supports a Tangled Rope where genuine coordination is attempted but extraction persists due to power imbalances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_recognition_gap, empirical, 'Gap between claimed and actual recognition of Indigenous sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1970, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(hist_tr_t1980, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(hist_tr_t1990, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(hist_tr_t2010, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(hist_tr_t2020, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2020, 0.35).

% Extraction over time
narrative_ontology:measurement(hist_be_t1970, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(hist_be_t1980, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(hist_be_t1990, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1990, 0.73).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(hist_be_t2010, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(hist_be_t2020, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1970, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(hist_su_t1980, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1980, 0.73).
narrative_ontology:measurement(hist_su_t1990, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(hist_su_t2010, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(hist_su_t2020, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
