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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Principle for Cultural Property
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereign repatriation' reading of the
 *   cultural property legal corpus, asserting that cultural artifacts are
 *   sovereign property of successor states and that colonial acquisition was
 *   illegitimate extraction. Legitimate authority for these artifacts rests
 *   with states claiming historical continuity with expropriated peoples.
 *   This reading emphasizes national ownership and historical justice, often
 *   in tension with 'universal heritage' or 'indigenous stewardship'
 *   perspectives. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates international legal efforts while simultaneously
 *   extracting from holding institutions and suppressing alternative claims.
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
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Principle for Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '986d5e25-6b61-40d0-b8d0-21b53abd69fe').
narrative_ontology:cs_kernel_codification('986d5e25-6b61-40d0-b8d0-21b53abd69fe', formalized).
narrative_ontology:cs_authority_grounding('986d5e25-6b61-40d0-b8d0-21b53abd69fe', lineage).
narrative_ontology:cs_interpretation_layer_present('986d5e25-6b61-40d0-b8d0-21b53abd69fe').
narrative_ontology:cs_reading_relation('986d5e25-6b61-40d0-b8d0-21b53abd69fe', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('986d5e25-6b61-40d0-b8d0-21b53abd69fe', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('986d5e25-6b61-40d0-b8d0-21b53abd69fe', foundational, colonial_acquisition_illegitimate).
narrative_ontology:cs_axiom_status(colonial_acquisition_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('986d5e25-6b61-40d0-b8d0-21b53abd69fe', colonial_acquisition_illegitimate, deontological).
narrative_ontology:cs_axiom('986d5e25-6b61-40d0-b8d0-21b53abd69fe', foundational, successor_state_sovereignty).
narrative_ontology:cs_axiom_status(successor_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('986d5e25-6b61-40d0-b8d0-21b53abd69fe', successor_state_sovereignty, conventional).
narrative_ontology:cs_reference_frame('986d5e25-6b61-40d0-b8d0-21b53abd69fe', post_colonial_justice_framework).
narrative_ontology:cs_drift_state('986d5e25-6b61-40d0-b8d0-21b53abd69fe', contemporary_indigenous_rights_movement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('986d5e25-6b61-40d0-b8d0-21b53abd69fe', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim cultural artifacts as sovereign property, seeking their return to restore national identity and historical justice. They benefit from the symbolic capital and cultural enrichment of repatriation, but face diplomatic and legal hurdles.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary,
    institutional, generational, constrained, global).

% Possess large collections of artifacts acquired during colonial periods. They bear the costs of repatriation (loss of collection, financial expense, reputational damage) and resist claims by asserting universal heritage or superior preservation capabilities. Their exit is constrained by legal and ethical pressure.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_museums, payer,
    organized, generational, constrained, global).

% Hold artifacts acquired through various means, often with less public scrutiny than institutions. They face increasing legal and reputational risks from repatriation claims, bearing the cost of potential loss of property and market value. Their exit is constrained by legal frameworks and public opinion.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors, payer,
    moderate, biographical, constrained, global).

% Develop and interpret international conventions and norms regarding cultural property. They mediate disputes, issue recommendations, and provide legal frameworks that influence repatriation outcomes, effectively setting the agenda for claims and enforcement.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Often have direct cultural and spiritual ties to artifacts, but their claims are frequently subsumed under the sovereign rights of successor states in this reading. They are excluded from direct legal standing in many international frameworks, despite being the original expropriated peoples.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    powerless, civilizational, identity_locked, local).

% Argue for artifacts as shared human heritage, prioritizing preservation and access regardless of origin. Their perspective is often sidelined by the sovereign repatriation framework, which prioritizes national ownership and historical justice over universal access.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for resolving disputes over cultural property by assigning clear ownership based on historical sovereignty and rectifying past colonial injustices, thereby coordinating international legal and diplomatic efforts.
% TRANSFER_FUNCTION: Transfers ownership and physical custody of cultural artifacts from holding institutions and private collectors (primarily in former colonial powers) to successor states (primarily in formerly colonized regions), along with associated symbolic and cultural capital.
% ABSENT_VOICES: Indigenous communities, whose direct cultural ties are often overlooked in favor of state sovereignty, and universal heritage advocates, who prioritize global access and preservation, are largely excluded from the primary legal and diplomatic channels that this reading emphasizes.
% DISAPPEARANCE_RATIONALE: If this principle vanished, the international legal landscape for cultural property would revert to a more fragmented state, with fewer clear grounds for repatriation claims. Holding institutions would face less pressure, and successor states would lose a key tool for historical redress, leading to a significant rearrangement of power dynamics in cultural diplomacy.
% FOUNDING_PROBLEM: The historical injustice of colonial expropriation of cultural artifacts, leading to their dispersal and retention in former colonial powers, and the subsequent lack of clear legal mechanisms for their return.
% FOUNDING_PROBLEM_CORROBORATION: Successor states and international legal bodies attest that the problem of colonial injustice and the need for repatriation remains live, citing ongoing diplomatic efforts and unresolved claims. While holding institutions may contest the extent of 'injustice' or the 'live' status of the problem, the international consensus, supported by UN resolutions and UNESCO conventions, corroborates its continued relevance.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).

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
 *   The extractiveness (0.65) reflects the significant costs borne by holding institutions and collectors through repatriation, as well as the diplomatic and legal friction generated. Suppression (0.70) is high because this reading actively suppresses alternative claims (e.g., universal heritage, direct indigenous ownership) by prioritizing state-to-state claims and requiring active enforcement of international conventions. The theater ratio (0.20) is relatively low, indicating that while there is performative diplomacy, the core function of rectifying historical injustice and transferring property is real. The increasing extractiveness and suppression over time reflect the growing international pressure for repatriation and the hardening of legal positions.
 *
 * PERSPECTIVAL GAP:
 *   Successor states experience this as a legitimate and necessary mechanism for historical justice and cultural restoration. Holding institutions, however, experience it as an extractive demand that challenges their collections and institutional mission, often viewing it as a political rather than purely legal or ethical imperative. The international legal bodies navigate these competing perspectives, attempting to coordinate a framework that is seen as legitimate by a broad range of actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states are the primary beneficiaries (d=0.0-0.2) as they gain cultural and symbolic capital. Holding museums and private collectors are the primary payers (d=0.8-1.0) as they lose property and bear associated costs. International legal bodies act as agenda-setters (d=0.4-0.6), mediating and enforcing the framework. Indigenous communities and universal heritage advocates are largely excluded (d=0.9-1.0 for indigenous communities due to identity-lock, d=0.7-0.8 for universal heritage advocates due to their claims being sidelined), as their claims are either subsumed or actively suppressed by the state-centric focus of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling genuine efforts to rectify historical injustice as pure extraction, while also acknowledging the extractive impact on holding institutions. The 'live' status of the founding problem (colonial injustice) suggests it is not a piton. However, the contestation around the founding problem's status and the exclusion of certain voices (e.g., indigenous communities) indicate that the coordination function may be serving an extractive agenda for some beneficiaries, pushing it towards a Tangled Rope classification rather than a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_indigenous_rights,
    'Does the emphasis on state sovereignty in repatriation claims adequately address the rights and cultural continuity of indigenous communities, or does it merely transfer ownership from one state to another, bypassing original custodians?',
    'Analysis of repatriation cases where indigenous communities'' claims diverge from or are subsumed by successor state claims; legal frameworks that grant direct standing to indigenous groups.',
    'If indigenous rights are consistently bypassed, the ''beneficiary'' status of successor states becomes more extractive from the perspective of indigenous communities, potentially reclassifying the constraint as a Snare for those communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_indigenous_rights, conceptual, 'Ambiguity in whether state sovereignty truly represents the interests of original expropriated peoples.').

omega_variable(
    legitimacy_of_colonial_acquisition,
    'To what extent can any colonial-era acquisition of cultural property be considered legitimate under contemporary international law and ethics, even if legally permissible at the time of acquisition?',
    'Evolving international legal norms and ethical guidelines, as reflected in new conventions or court rulings that retroactively assess the legitimacy of historical acquisitions.',
    'A stronger consensus on the categorical illegitimacy of colonial acquisition would increase the ''extractiveness'' and ''suppression'' metrics for holding institutions, solidifying the Snare-like aspects of their retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_colonial_acquisition, preference, 'Contestation over the historical legitimacy of colonial-era cultural property transfers.').

omega_variable(
    reading_divergence_impact,
    'How would the classification of this constraint change if the ''universal_heritage_reading'' or ''indigenous_stewardship_reading'' were adopted as the dominant framework?',
    'Comparative analysis of hypothetical legal and diplomatic outcomes under alternative dominant readings.',
    'If ''universal_heritage'' dominated, ''holding_museums'' might become beneficiaries (as custodians), and ''successor_states'' might become payers (losing exclusive claims). If ''indigenous_stewardship'' dominated, ''indigenous_communities'' would become primary beneficiaries, and ''successor_states'' might become payers if their claims conflict with direct communal ownership.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_impact, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'cultural_property_legal_corpus' kernel, alongside 'universal_heritage_reading' and 'indigenous_stewardship_reading'. Each reading represents a distinct structural claim about ownership and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
