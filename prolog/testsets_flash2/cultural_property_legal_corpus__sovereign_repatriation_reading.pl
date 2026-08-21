% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Claim for Cultural Property
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereign repatriation' reading of the
 *   cultural property legal corpus, asserting that cultural artifacts are the
 *   sovereign property of successor states and that colonial acquisition was
 *   illegitimate extraction. Legitimate authority for these artifacts rests
 *   with states claiming historical continuity with expropriated peoples.
 *   This reading frames holding institutions and former colonial powers as
 *   extractors of identity capital, while successor states are beneficiaries.
 *   The constraint is classified as a Tangled Rope due to its genuine
 *   coordination function (providing a framework for repatriation) coupled
 *   with asymmetric extraction (from holding institutions to successor
 *   states) and the need for active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.65).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.7).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Claim for Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '39f6df94-e96c-46b0-ba6d-8c82ff1d3f18').
narrative_ontology:cs_kernel_codification('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', formalized).
narrative_ontology:cs_authority_grounding('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', lineage).
narrative_ontology:cs_interpretation_layer_present('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18').
narrative_ontology:cs_reading_relation('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', foundational, cultural_property_is_state_sovereignty).
narrative_ontology:cs_axiom_status(cultural_property_is_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', cultural_property_is_state_sovereignty, deontological).
narrative_ontology:cs_axiom('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', foundational, colonial_acquisition_is_illegitimate_extraction).
narrative_ontology:cs_axiom_status(colonial_acquisition_is_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', colonial_acquisition_is_illegitimate_extraction, deontological).
narrative_ontology:cs_reference_frame('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', post_colonial_international_law_framework).
narrative_ontology:cs_drift_state('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('39f6df94-e96c-46b0-ba6d-8c82ff1d3f18', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_governments).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim cultural artifacts as sovereign property, seeking their return to restore national identity and historical justice. They benefit from the symbolic and cultural capital repatriated artifacts represent, but face diplomatic and legal hurdles.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary,
    institutional, generational, constrained, national).

% Actively pursue repatriation as a matter of national sovereignty and historical redress. They gain political legitimacy and cultural enrichment from successful claims, but are constrained by international legal frameworks and the power of holding institutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_governments, beneficiary,
    institutional, generational, constrained, national).

% Primarily Western museums and cultural bodies that currently possess the artifacts. They bear the costs of repatriation (loss of collection, financial expense, reputational damage) and resist claims, citing preservation, universal access, and legal ownership arguments.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions, payer,
    institutional, generational, constrained, global).

% The states whose historical actions led to the acquisition of the artifacts. They face diplomatic pressure, legal challenges, and reputational costs associated with the legacy of colonial extraction. Their exit options are constrained by international norms and the political will of successor states.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_powers, payer,
    institutional, generational, constrained, global).

% Develop and interpret international conventions and norms regarding cultural property. They mediate disputes and provide frameworks for negotiation, but their authority is often advisory and dependent on state ratification and compliance.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Often the original creators and stewards of the artifacts, but their claims are frequently subsumed or overlooked by the sovereign repatriation framework, which prioritizes state-to-state claims. They are excluded from direct negotiation in this reading.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    powerless, civilizational, identity_locked, local).

% Argue for artifacts as shared human heritage, prioritizing preservation and access over national ownership. They are excluded from the core sovereign repatriation framework, which focuses on national claims.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_advocates, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for resolving disputes over cultural property by assigning legitimate ownership to successor states, thereby coordinating international legal and diplomatic efforts towards repatriation.
% TRANSFER_FUNCTION: Transfers symbolic capital, cultural identity, and sometimes physical artifacts from former colonial powers and holding institutions to successor states, rectifying historical injustices.
% ABSENT_VOICES: Indigenous communities, whose direct ancestral claims are often distinct from state sovereignty, are largely absent from the state-centric sovereign repatriation discourse. Universal heritage advocates are also sidelined, as their focus on global access conflicts with national ownership claims.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the international legal landscape for cultural property would become highly fragmented. Repatriation efforts would lose their primary legal and moral grounding, leading to increased disputes, stalled negotiations, and a perpetuation of colonial-era holdings. The symbolic and political capital of successor states would be diminished.
% FOUNDING_PROBLEM: The historical injustice of colonial-era expropriation of cultural artifacts, leading to their dispersal in former colonial powers and a loss of cultural heritage for newly independent nations.
% FOUNDING_PROBLEM_CORROBORATION: Historians, post-colonial scholars, and international legal experts outside of the directly benefiting states corroborate the ongoing nature of this historical injustice and the need for redress. UN resolutions and UNESCO conventions also attest to the problem's live status.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the current holding of artifacts by former colonial powers is seen as an ongoing extraction of cultural and symbolic capital from successor states. Suppression is high (0.7) due to the legal and diplomatic barriers that prevent easy repatriation, requiring sustained political and legal pressure from successor states. Theater ratio is low (0.2) as the debate is largely substantive, though some holding institutions engage in performative gestures of 'cultural exchange' rather than full repatriation. Accessibility collapse is moderate (0.4) as alternatives (e.g., direct negotiation, legal challenges) exist but are difficult, and resistance is high (0.75) from both sides of the debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of successor states, this is a just and necessary framework for historical redress. From the perspective of holding institutions, it is an extractive demand that threatens their collections and universal access mission. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states and post-colonial governments are beneficiaries (d near 0.0) as they gain cultural and symbolic capital through repatriation. Holding institutions and former colonial powers are payers (d near 1.0) as they bear the costs of repatriation and the loss of collection. International legal bodies act as agenda-setters, shaping the discourse and legal frameworks. Indigenous communities and universal heritage advocates are excluded, as their claims are not central to this state-centric reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (redressing colonial injustice) is still live, preventing it from being mislabeled as a Piton. Its extractive nature for holding institutions, coupled with its coordination function for successor states, prevents it from being a pure Snare. The active enforcement and clear beneficiaries/victims confirm its Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_indigenous_rights,
    'Does the sovereign repatriation framework adequately address the rights and claims of indigenous communities, or does it merely transfer ownership from one state to another, potentially re-enacting colonial power dynamics?',
    'Analysis of repatriation cases where indigenous communities'' claims diverge from or are subsumed by state claims, and the outcomes for those communities.',
    'If indigenous rights are consistently sidelined, the framework''s legitimacy is weakened, and its ''beneficiary'' status for successor states becomes more extractive for indigenous peoples, potentially shifting the classification towards a Snare for indigenous communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_indigenous_rights, conceptual, 'Ambiguity regarding the ultimate beneficiaries and potential re-enactment of colonial power dynamics under the guise of state sovereignty.').

omega_variable(
    cost_of_repatriation_vs_symbolic_value,
    'Is the financial and logistical cost of repatriation for holding institutions proportional to the symbolic and cultural value gained by successor states, or is it an economically inefficient transfer?',
    'Economic analysis comparing the direct costs of repatriation (transport, conservation, display infrastructure) with quantitative and qualitative assessments of cultural and national identity benefits.',
    'If costs are disproportionately high relative to benefits, it could indicate a less efficient coordination mechanism, potentially lowering the ''rope'' aspect of the Tangled Rope classification. If the symbolic value is deemed incalculable, the economic cost becomes secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_repatriation_vs_symbolic_value, empirical, 'Whether the economic costs of repatriation are justified by the intangible benefits.').

omega_variable(
    universal_access_tradeoff,
    'Does repatriation to successor states genuinely enhance global access to cultural heritage, or does it restrict access by concentrating artifacts in fewer, potentially less accessible, locations?',
    'Empirical study of post-repatriation access patterns (e.g., digital access, physical exhibition, research opportunities) compared to pre-repatriation access.',
    'If access is significantly restricted, it strengthens the ''universal_heritage_reading'' and highlights a potential negative externality of the sovereign repatriation framework, challenging its broader coordination claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_access_tradeoff, empirical, 'Trade-off between national ownership and universal access to cultural heritage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
