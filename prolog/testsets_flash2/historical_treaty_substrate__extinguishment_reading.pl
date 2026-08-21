% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate (Extinguishment Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'extinguishment reading' of historical
 *   treaties, where Indigenous parties are understood to have ceded
 *   territorial sovereignty in exchange for defined reserves and payments.
 *   This reading frames treaties as completed property transactions, enabling
 *   settler expansion and resource extraction. It is a specific
 *   interpretation of a contested kernel, distinct from 'nation-to-nation' or
 *   'stewardship' readings. The high extractiveness and suppression reflect
 *   the ongoing costs borne by Indigenous nations under this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.92).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.88).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '62e2014b-c7f6-4543-80e4-dece01fabd31').
narrative_ontology:cs_kernel_codification('62e2014b-c7f6-4543-80e4-dece01fabd31', fixed_text).
narrative_ontology:cs_authority_grounding('62e2014b-c7f6-4543-80e4-dece01fabd31', lineage).
narrative_ontology:cs_interpretation_layer_present('62e2014b-c7f6-4543-80e4-dece01fabd31').
narrative_ontology:cs_reading_relation('62e2014b-c7f6-4543-80e4-dece01fabd31', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('62e2014b-c7f6-4543-80e4-dece01fabd31', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('62e2014b-c7f6-4543-80e4-dece01fabd31', foundational, territorial_sovereignty_is_divisible_and_transferable).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_divisible_and_transferable, holdable).
narrative_ontology:cs_axiom_grounding('62e2014b-c7f6-4543-80e4-dece01fabd31', territorial_sovereignty_is_divisible_and_transferable, conventional).
narrative_ontology:cs_axiom('62e2014b-c7f6-4543-80e4-dece01fabd31', foundational, treaties_are_property_transactions).
narrative_ontology:cs_axiom_status(treaties_are_property_transactions, holdable).
narrative_ontology:cs_axiom_grounding('62e2014b-c7f6-4543-80e4-dece01fabd31', treaties_are_property_transactions, conventional).
narrative_ontology:cs_reference_frame('62e2014b-c7f6-4543-80e4-dece01fabd31', terra_nullius_and_crown_paramountcy).
narrative_ontology:cs_drift_state('62e2014b-c7f6-4543-80e4-dece01fabd31', contemporary_indigenous_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('62e2014b-c7f6-4543-80e4-dece01fabd31', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_landowners_and_resource_industries).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations_and_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets historical treaties as instruments of land cession and extinguishment of Indigenous sovereignty, enabling unfettered jurisdiction and resource development. Benefits from clear title and administrative simplicity. Actively defends this interpretation in courts and policy.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate on lands understood to be 'ceded' by treaties, relying on the extinguishment reading for security of tenure and access to resources. Their economic activity is directly enabled by this interpretation, and they lobby for its maintenance.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_landowners_and_resource_industries, beneficiary,
    organized, biographical, mobile, regional).

% Are treated as having ceded inherent sovereignty over vast territories, retaining only limited, enumerated rights to reserves and annuities. They bear the costs of lost land, resources, and self-determination, and are forced to litigate to assert any remaining rights. Their identity is deeply tied to their ancestral lands, making 'exit' from the relationship impossible.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations_and_communities, payer,
    powerless, generational, identity_locked, local).

% Adjudicates treaty disputes, often relying on historical legal precedents that favor the extinguishment reading, even while acknowledging evolving legal principles. Their decisions reinforce or challenge the constraint's operation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous framework for land ownership and governance, allowing for the expansion of settler society and resource development by defining Indigenous rights as limited and territorial claims as extinguished.
% TRANSFER_FUNCTION: Transfers vast territorial sovereignty and resource control from Indigenous nations to the settler state, in exchange for defined reserves, one-time payments, and limited, enumerated rights.
% ABSENT_VOICES: The original Indigenous signatories, whose understanding of the treaties as ongoing, relational pacts for shared stewardship or nation-to-nation agreements is systematically excluded from the dominant legal and historical narratives that uphold the extinguishment reading.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading of treaties vanished overnight, the legal basis for much of the settler state's territorial jurisdiction and resource ownership would collapse. Land titles would become contested, resource projects would halt, and the entire constitutional framework would require renegotiation with Indigenous nations, leading to a profound societal reorganization.
% FOUNDING_PROBLEM: The settler state faced the problem of legitimizing its expansion into Indigenous territories and securing access to resources, while managing Indigenous populations and avoiding open conflict.
% FOUNDING_PROBLEM_CORROBORATION: Settler state governments and resource industries attest the problem of clear title and resource access is still live, requiring the stability provided by the extinguishment reading. Indigenous legal scholars and historians, from outside the benefiting parties, corroborate that the problem of settler expansion and control was indeed the founding impetus, but dispute the legitimacy and ongoing status of the 'solution'.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.92) is high because this reading fundamentally redefines Indigenous land and resource relationships from inherent sovereignty to limited, enumerated rights, enabling massive wealth transfer to the settler state and industries. Suppression (0.88) is also high, as this reading is actively enforced through legal systems, policy, and historical narratives that deny Indigenous claims to ongoing sovereignty. The theater ratio (0.45) reflects the performative aspects of 'consultation' or 'reconciliation' that often occur within the framework of assumed extinguishment, masking the underlying extractive structure. Resistance (0.70) is significant, driven by ongoing Indigenous legal challenges and activism.
 *
 * PERSPECTIVAL GAP:
 *   The settler state and industries perceive this as a legitimate, settled legal framework for land management, while Indigenous nations experience it as an ongoing act of dispossession and suppression of their inherent rights. The engine's classification will highlight this divergence, showing a 'snare' from the Indigenous perspective versus a 'rope' or 'scaffold' from the settler perspective, despite the 'claimed_type' being 'snare' from the author's analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler state governments and resource industries are clear beneficiaries (low directionality), as this reading legitimizes their control over vast territories. Indigenous nations and communities are the primary targets (high directionality), bearing the costs of lost sovereignty and resources, with their identity deeply tied to the land, leading to an 'identity_locked' exit option. The judiciary, while an 'observer', often reinforces the constraint through precedent, making it an indirect enforcer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_vs_stewardship_reading,
    'Is the historical treaty substrate fundamentally about land cession and extinguishment of sovereignty, or about relational pacts for shared stewardship?',
    'Re-examination of historical Indigenous oral traditions and legal systems, alongside settler archival records, to reconstruct the original intent and understanding of all parties at the time of treaty signing.',
    'If the stewardship reading is validated, the current constraint (extinguishment reading) would be reclassified from a Snare to a Tangled Rope or even a Rope, as its foundational premise of unilateral cession would be undermined, requiring renegotiation of land and resource governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_vs_stewardship_reading, conceptual, 'Ambiguity in the core interpretation of historical treaties.').

omega_variable(
    extinguishment_vs_nation_to_nation_reading,
    'Are historical treaties domestic property transactions, or international agreements between sovereign nations?',
    'International legal review and comparative analysis of treaty law principles applied to Indigenous treaties, particularly in light of modern international human rights and Indigenous rights declarations.',
    'If the nation-to-nation reading is validated, the current constraint would be reclassified, as it would imply ongoing Indigenous sovereignty and the need for modern consent, fundamentally altering the power dynamics and reducing the settler state''s unilateral authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinguishment_vs_nation_to_nation_reading, conceptual, 'Ambiguity in the legal status and framework of historical treaties.').

omega_variable(
    internalized_suppression_indigenous_communities,
    'To what extent has the suppression inherent in the extinguishment reading been internalized by Indigenous communities, affecting their capacity for resistance and self-determination?',
    'Longitudinal studies of Indigenous community resilience, cultural revitalization efforts, and the impact of legal victories on self-governance and well-being, assessing whether suppression persists after external legal barriers are challenged.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than structural measures suggest, requiring additional focus on decolonization and healing processes beyond legal reforms. If not, external structural changes will have a more immediate and direct impact on reducing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_indigenous_communities, empirical, 'Structural vs. internalized suppression mechanism within Indigenous communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__extinguishment_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__extinguishment_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(hist_tr_t90, historical_treaty_substrate__extinguishment_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement(hist_tr_t120, historical_treaty_substrate__extinguishment_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.45).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 60, 0.9).
narrative_ontology:measurement(hist_be_t90, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 90, 0.93).
narrative_ontology:measurement(hist_be_t120, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 120, 0.92).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(hist_su_t90, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 90, 0.88).
narrative_ontology:measurement(hist_su_t120, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 120, 0.88).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
