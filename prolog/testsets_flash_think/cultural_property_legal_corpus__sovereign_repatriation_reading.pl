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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Principle for Cultural Property
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereign repatriation' reading
 *   of the cultural property legal corpus. It posits that cultural artifacts
 *   are the sovereign property of successor states, that colonial acquisition
 *   was illegitimate extraction, and that legitimate authority for these
 *   artifacts rests with states claiming historical continuity with
 *   expropriated peoples. This reading frames repatriation as a matter of
 *   state sovereignty and historical justice, often contrasting with
 *   'universal heritage' or 'indigenous stewardship' perspectives. The
 *   claimed type is 'tangled_rope' because it coordinates international legal
 *   norms while simultaneously extracting from holding institutions and
 *   former colonial powers to benefit successor states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.75).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Principle for Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'c9e91428-c0bc-4f0f-9319-91a1e8cba58b').
narrative_ontology:cs_kernel_codification('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', formalized).
narrative_ontology:cs_authority_grounding('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', lineage).
narrative_ontology:cs_interpretation_layer_present('c9e91428-c0bc-4f0f-9319-91a1e8cba58b').
narrative_ontology:cs_reading_relation('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', foundational, cultural_property_is_state_sovereignty).
narrative_ontology:cs_axiom_status(cultural_property_is_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', cultural_property_is_state_sovereignty, deontological).
narrative_ontology:cs_axiom('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', foundational, colonial_acquisition_is_illegitimate).
narrative_ontology:cs_axiom_status(colonial_acquisition_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', colonial_acquisition_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', post_colonial_sovereignty_framework).
narrative_ontology:cs_drift_state('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', contemporary_indigenous_rights_advocacy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c9e91428-c0bc-4f0f-9319-91a1e8cba58b', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, expropriated_peoples).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert sovereignty over cultural artifacts originating from their territory, seeking their return from former colonial powers and holding institutions. They drive diplomatic and legal efforts for repatriation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, agenda_setter,
    institutional, generational, constrained, global).

% Are the direct cultural inheritors of the artifacts, whose identity and historical continuity are restored through repatriation. Their claims are primarily mediated through successor states in this legal framework.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, expropriated_peoples, beneficiary,
    powerless, generational, identity_locked, local).

% Currently possess the cultural artifacts. They face legal and reputational pressure for repatriation, incurring costs through diplomatic friction, legal challenges, and potential loss of collections. They often argue for universal heritage or preservation roles.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions, payer,
    institutional, generational, constrained, global).

% Are historically responsible for the acquisition of cultural property during colonial periods. They bear diplomatic and moral costs associated with past actions and often support holding institutions in resisting repatriation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_powers, payer,
    institutional, generational, constrained, global).

% Develop, interpret, and mediate international conventions and norms related to cultural property. They provide a framework for claims but do not directly benefit or pay in terms of property transfer.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Argue that cultural artifacts belong to all humanity and should be preserved and made accessible by institutions best equipped to do so, often opposing state-centric repatriation claims.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_advocates, observer,
    organized, generational, mobile, global).

% Hold direct, often sacred, cultural ties to artifacts but are frequently marginalized in state-centric legal frameworks, with their claims subsumed by or secondary to those of successor states.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    powerless, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an international legal and normative framework for resolving disputes over cultural property, aiming to bring order to claims of ownership and restitution in a post-colonial world.
% TRANSFER_FUNCTION: Transfers legal and symbolic ownership (and sometimes physical custody) of cultural artifacts from holding institutions and former colonial powers to successor states, as a form of historical redress.
% ABSENT_VOICES: Indigenous communities, whose direct cultural stewardship claims are often subsumed or ignored by the state-centric framework of this reading, would argue for direct restitution to their communities.
% DISAPPEARANCE_RATIONALE: If this principle vanished, the legal and moral basis for repatriation claims would collapse, leading to continued disputes, potentially increased illicit trafficking, and a lack of clear international norms for cultural heritage restitution, fundamentally reorganizing international cultural diplomacy.
% FOUNDING_PROBLEM: The historical injustice of colonial expropriation of cultural artifacts and the need for a legal and ethical framework to address restitution in the post-colonial era, recognizing the sovereign rights of newly independent states.
% FOUNDING_PROBLEM_CORROBORATION: International conventions (e.g., UNESCO 1970, UNIDROIT 1995), resolutions from the UN General Assembly, and extensive scholarly work in international law and post-colonial studies corroborate the problem and its ongoing relevance. Successor states and many NGOs also attest to its live status.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.55) as repatriation involves significant diplomatic and legal friction, but the core transfer of symbolic capital is substantial. Suppression is high (0.75) because holding institutions and former colonial powers actively resist repatriation, requiring sustained international pressure and legal enforcement. Theater ratio is moderate (0.45) as many institutions engage in performative dialogues about restitution without always committing to full repatriation. The metrics show a gradual increase over time, reflecting the growing international pressure for repatriation and the corresponding resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of successor states, this principle is a matter of justice and restoration of sovereignty. From the perspective of holding institutions, it represents a threat to their collections and a challenge to their perceived role as custodians of universal heritage. The engine's computation of per-seat classifications will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states and the expropriated peoples they represent are the primary beneficiaries, gaining cultural and symbolic capital. Holding institutions and former colonial powers are the payers, bearing the costs of potential loss of collections, reputational damage, and legal expenses. International legal bodies act as observers and agenda-setters, shaping the framework but not directly benefiting from the property transfer. Indigenous communities are structurally excluded from direct agency in this state-centric reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_vs_universal_heritage_legitimacy,
    'Is the sovereign claim of successor states over cultural property fundamentally more legitimate than the ''universal heritage'' claim of holding institutions?',
    'International consensus shifts, or a landmark ruling by an international court that definitively prioritizes one claim over the other in a broad range of cases.',
    'If universal heritage is prioritized, the extractiveness from holding institutions would decrease, and the constraint might reclassify towards a ''rope'' for global coordination of preservation. If state sovereignty is further entrenched, extractiveness would remain high, reinforcing the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_universal_heritage_legitimacy, conceptual, 'Ambiguity regarding the ultimate legitimate custodian of cultural property.').

omega_variable(
    state_vs_indigenous_representation,
    'Does the successor state truly represent the interests and cultural continuity of the expropriated indigenous peoples, or do indigenous communities hold a distinct, more direct claim?',
    'Direct legal recognition of indigenous communities as primary claimants in international law, or a shift in national laws to empower indigenous groups to pursue claims independently of the state.',
    'If indigenous claims are recognized as distinct and primary, the ''successor_states'' might shift from ''agenda_setter'' to a more ''intermediary'' role, and the ''expropriated_peoples'' would gain direct agency, potentially altering the constraint''s beneficiary structure and reducing the ''excluded'' status of indigenous communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_indigenous_representation, empirical, 'Whether state sovereignty adequately represents indigenous cultural rights.').

omega_variable(
    enforcement_effectiveness_vs_diplomatic_friction,
    'To what extent is the observed ''suppression'' a result of genuine legal enforcement, versus diplomatic friction and reputational pressure that can be resisted indefinitely?',
    'Analysis of the success rate of repatriation claims in binding international arbitration or national courts, versus cases resolved through non-binding diplomatic negotiations.',
    'If enforcement is weak, the constraint''s effective suppression is lower than measured, indicating a more ''piton''-like persistence through inertia and theatrical engagement. If legal enforcement proves consistently effective, the ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_effectiveness_vs_diplomatic_friction, empirical, 'Distinguishing between effective legal enforcement and mere diplomatic pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(cult_tr_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(cult_be_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(cult_su_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, illicit_antiquities_trade_regulation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, museum_acquisition_ethics).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'cultural_property_legal_corpus' kernel, focusing on state sovereignty and post-colonial restitution. It is linked to sibling readings that emphasize universal heritage or indigenous stewardship, as these framings compete within the same domain of international cultural property law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
