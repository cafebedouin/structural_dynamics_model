% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Indigenous Stewardship of Cultural Property (Indigenous Reading)
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint represents the 'indigenous stewardship' reading of the
 *   cultural property legal corpus, asserting that cultural artifacts are
 *   sacred or communal property of indigenous communities, and legitimate
 *   authority rests with those communities maintaining cultural continuity.
 *   It views the holding of such artifacts by colonial successor states or
 *   Western museums as an ongoing act of extraction. The high extractiveness
 *   and suppression reflect the historical and structural barriers indigenous
 *   communities face in reclaiming their heritage. The claimed type is
 *   'snare' because the coordination story (preservation, universal access)
 *   is seen as cover for ongoing extraction, maintained by legal and
 *   institutional coercion.
 *
 * KEY AGENTS:
 *   - indigenous_communities: Primary beneficiary (organized/identity_locked) — seeks repatriation and control
 *   - colonial_successor_states: Primary payer (institutional/constrained) — illegitimate holders, face demands for repatriation
 *   - western_museums: Primary payer (institutional/constrained) — extractors holding stolen property
 *   - private_collectors: Payer (powerful/constrained) — illegitimate owners, face potential loss of assets
 *   - international_legal_bodies: Observer (institutional/analytical) — influence discourse and policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.92).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Indigenous Stewardship of Cultural Property (Indigenous Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '6dc4d5ca-0447-4499-a124-36b8eb74edbe').
narrative_ontology:cs_kernel_codification('6dc4d5ca-0447-4499-a124-36b8eb74edbe', distributed).
narrative_ontology:cs_authority_grounding('6dc4d5ca-0447-4499-a124-36b8eb74edbe', practice).
narrative_ontology:cs_reading_relation('6dc4d5ca-0447-4499-a124-36b8eb74edbe', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('6dc4d5ca-0447-4499-a124-36b8eb74edbe', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('6dc4d5ca-0447-4499-a124-36b8eb74edbe', foundational, cultural_property_is_inalienable_indigenous_heritage).
narrative_ontology:cs_axiom_status(cultural_property_is_inalienable_indigenous_heritage, holdable).
narrative_ontology:cs_axiom_grounding('6dc4d5ca-0447-4499-a124-36b8eb74edbe', cultural_property_is_inalienable_indigenous_heritage, deontological).
narrative_ontology:cs_axiom('6dc4d5ca-0447-4499-a124-36b8eb74edbe', foundational, cultural_continuity_confers_legitimate_authority).
narrative_ontology:cs_axiom_status(cultural_continuity_confers_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('6dc4d5ca-0447-4499-a124-36b8eb74edbe', cultural_continuity_confers_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('6dc4d5ca-0447-4499-a124-36b8eb74edbe', pre_colonial_indigenous_stewardship).
narrative_ontology:cs_drift_state('6dc4d5ca-0447-4499-a124-36b8eb74edbe', contemporary_international_law, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6dc4d5ca-0447-4499-a124-36b8eb74edbe', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, western_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, private_collectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities are the rightful stewards and owners of their cultural artifacts, maintaining a continuous spiritual and cultural connection. They seek repatriation and control over their heritage, viewing its current holding by external institutions as ongoing harm and extraction. Their identity is deeply intertwined with these objects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, beneficiary,
    organized, generational, identity_locked, local).

% These states inherited or perpetuated colonial legal frameworks that legitimized the acquisition of indigenous cultural property. Under this reading, they are illegitimate holders and face demands for repatriation, incurring costs in legal challenges, diplomatic pressure, and potential loss of cultural assets. Their claim to sovereignty over these objects is challenged.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, payer,
    institutional, generational, constrained, national).

% Major repositories of indigenous cultural artifacts, often acquired during colonial periods. They claim a role in preservation and universal access, but under this reading, they are extractors holding stolen property. They face reputational damage, legal challenges, and the cost of repatriation, which includes deaccessioning and logistical expenses.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, western_museums, payer,
    institutional, biographical, constrained, global).

% Individuals who own indigenous cultural artifacts, often acquired through markets that originated from colonial expropriation. Under this reading, their ownership is illegitimate. They face legal challenges, public pressure, and the potential loss of valuable assets without compensation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, private_collectors, payer,
    powerful, biographical, constrained, global).

% These bodies (e.g., UNESCO, UN Human Rights Council) develop conventions and recommendations regarding cultural property. They observe the contest between indigenous claims, state sovereignty, and universal heritage, and their pronouncements can influence the legitimacy and enforcement of repatriation efforts.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the recognition of indigenous communities as the primary authorities for the stewardship and interpretation of their cultural heritage, ensuring cultural continuity and spiritual well-being.
% TRANSFER_FUNCTION: It seeks to transfer control, ownership, and physical custody of cultural artifacts from colonial successor states, Western museums, and private collectors to indigenous communities, along with the associated rights to interpretation and use.
% ABSENT_VOICES: The voices of past generations of indigenous peoples, whose cultural property was forcibly taken or illicitly acquired, are central to this reading but are only represented through their descendants. Their direct testimony of expropriation and cultural harm is absent but foundational.
% DISAPPEARANCE_RATIONALE: If this constraint (the indigenous stewardship reading) disappeared, the global discourse on cultural property would revert to state-centric or universalist framings, significantly diminishing indigenous claims. Repatriation efforts would stall, and the moral imperative for restitution would weaken, fundamentally altering international law and cultural policy.
% FOUNDING_PROBLEM: The problem this reading addresses is the historical and ongoing dispossession of indigenous cultural heritage due to colonialism, leading to cultural erosion, spiritual harm, and the denial of self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous communities globally, supported by human rights organizations, post-colonial scholars, and some international legal experts, attest that the problem of cultural dispossession and its harms remains profoundly live. This corroboration comes from outside the direct beneficiaries of the current holding institutions.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because, from this reading's perspective, the entire system of holding indigenous cultural property outside its communities of origin is fundamentally extractive. Suppression (0.92) is also very high, reflecting the immense legal, financial, and institutional power wielded by states and museums, which actively suppress indigenous claims through legal frameworks, resource disparities, and diplomatic inertia. Theater ratio (0.45) is rising as institutions increasingly engage in performative gestures of dialogue and 'shared heritage' while resisting substantive repatriation. Accessibility collapse (0.70) is high because alternatives to the current system (e.g., direct negotiation, international arbitration) are severely constrained by existing legal precedents and power imbalances. Resistance (0.80) is also high, reflecting the sustained and growing global movement for indigenous rights and repatriation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of indigenous communities (beneficiaries), the constraint is a snare, as it perpetuates the extraction of their heritage. From the perspective of colonial successor states and Western museums (payers), the constraint (the demand for repatriation) is an external pressure that threatens their collections and legal frameworks. The engine's classification will highlight this divergence, showing the 'snare' nature from the indigenous seat and a 'tangled rope' or 'snare' from the holding institutions' seats, depending on their perceived coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are the full beneficiaries (d=0.0) of this reading, as it legitimizes their claims and seeks to restore their heritage. Colonial successor states, Western museums, and private collectors are the targets (d=1.0), as they are the entities from whom the artifacts would be repatriated, bearing the costs of restitution and loss of collections. International legal bodies are observers (d=0.5), analyzing the situation without direct benefit or cost from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the current holding of indigenous cultural property as a 'rope' or 'scaffold' for preservation. By emphasizing the ongoing harm and illegitimate authority, it highlights the extractive nature (snare) rather than a benign coordination or temporary support function. The persistence of the constraint (the current holding system) is due to the active suppression of indigenous claims, not a genuine coordination problem it solves for all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_acquisition,
    'Is the historical acquisition of indigenous cultural property by colonial powers and subsequent holding by Western institutions legitimate under contemporary international law and ethics?',
    'International court rulings, binding UN resolutions, or widespread adoption of the UN Declaration on the Rights of Indigenous Peoples (UNDRIP) as customary international law.',
    'If deemed illegitimate, it strengthens the case for mandatory repatriation and reclassifies current holdings as pure extraction. If deemed legitimate (under older legal frameworks), it weakens indigenous claims and supports the ''universal heritage'' or ''sovereign repatriation'' readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_acquisition, conceptual, 'The fundamental question of the moral and legal legitimacy of historical cultural property transfers.').

omega_variable(
    cultural_continuity_definition,
    'How is ''cultural continuity'' defined and measured for the purpose of establishing legitimate stewardship, especially for communities impacted by severe historical disruption?',
    'Development of internationally recognized, culturally sensitive criteria for assessing community connection and continuity, involving indigenous scholars and elders.',
    'A clear, inclusive definition strengthens indigenous claims by providing a robust basis for identifying rightful stewards. An overly narrow or externally imposed definition could exclude some communities, weakening the reading''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_continuity_definition, empirical, 'Ambiguity in defining the core criterion for legitimate indigenous authority over cultural property.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, resource disparities) or internalized (historical trauma, erosion of traditional knowledge systems)?',
    'Post-repatriation community empowerment trajectories: if communities continue to face internal barriers to stewardship after external artifacts are returned, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — communities carry the suppression with them even after external barriers are removed, requiring additional support for full cultural restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of cultural dispossession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(cult_tr_t2020, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1970, 0.95).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1985, 0.92).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2010, 0.89).
narrative_ontology:measurement(cult_be_t2020, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2020, 0.88).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1970, 0.98).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1985, 0.95).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2000, 0.93).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(cult_su_t2020, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2020, 0.92).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, international_human_rights_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'cultural_property_legal_corpus' kernel. This 'indigenous stewardship' reading emphasizes the rights of indigenous communities, contrasting with the 'universal heritage' (global access) and 'sovereign repatriation' (state ownership) readings. Each reading instantiates a distinct constraint with different beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
