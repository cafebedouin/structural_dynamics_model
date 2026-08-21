% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the Religious Zionist reading of Jewish
 *   sovereignty over Eretz Yisrael, where the divine promise to the Jewish
 *   people grounds an inalienable territorial claim. Statehood is seen as a
 *   theological fulfillment, making territorial maximalism and the rejection
 *   of partition legitimate. This reading inherently subordinates or excludes
 *   Palestinian claims to the land. The high extractiveness and suppression
 *   reflect the ongoing conflict and displacement inherent in this
 *   interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.95).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '1e015166-ca35-4599-9432-2a4e0b4e9ede').
narrative_ontology:cs_kernel_codification('1e015166-ca35-4599-9432-2a4e0b4e9ede', fixed_text).
narrative_ontology:cs_authority_grounding('1e015166-ca35-4599-9432-2a4e0b4e9ede', lineage).
narrative_ontology:cs_interpretation_layer_present('1e015166-ca35-4599-9432-2a4e0b4e9ede').
narrative_ontology:cs_reading_relation('1e015166-ca35-4599-9432-2a4e0b4e9ede', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('1e015166-ca35-4599-9432-2a4e0b4e9ede', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e015166-ca35-4599-9432-2a4e0b4e9ede', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('1e015166-ca35-4599-9432-2a4e0b4e9ede', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('1e015166-ca35-4599-9432-2a4e0b4e9ede', foundational, divine_title_to_land_of_israel).
narrative_ontology:cs_axiom_status(divine_title_to_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('1e015166-ca35-4599-9432-2a4e0b4e9ede', divine_title_to_land_of_israel, theological).
narrative_ontology:cs_axiom('1e015166-ca35-4599-9432-2a4e0b4e9ede', foundational, jewish_sovereignty_as_theological_fulfillment).
narrative_ontology:cs_axiom_status(jewish_sovereignty_as_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('1e015166-ca35-4599-9432-2a4e0b4e9ede', jewish_sovereignty_as_theological_fulfillment, theological).
narrative_ontology:cs_reference_frame('1e015166-ca35-4599-9432-2a4e0b4e9ede', biblical_covenant_and_divine_mandate).
narrative_ontology:cs_drift_state('1e015166-ca35-4599-9432-2a4e0b4e9ede', contemporary_international_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1e015166-ca35-4599-9432-2a4e0b4e9ede', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, international_law_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the fulfillment of a divine promise, grounding an inalienable territorial claim to Eretz Yisrael. This claim is central to their collective identity and theological purpose, making any territorial compromise an existential and spiritual threat.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, beneficiary,
    institutional, civilizational, identity_locked, universal).

% Acts as the political embodiment and enforcer of the divine promise, asserting sovereignty over the entire land. Its policies, including settlement expansion and control over disputed territories, are justified as steps towards theological fulfillment. Exit from this framework would mean abandoning its foundational religious-national identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of this claim, including displacement, loss of land, and denial of self-determination. Their historical presence and claims to the land are subordinated or rendered illegitimate by the divine mandate. Exit options are severely limited by military and political control.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Provides a framework for territorial claims based on self-determination, U.N. resolutions, and post-colonial principles. This framework is often rejected or deemed irrelevant by the religious Zionist reading, which prioritizes divine title over human-made law. Its voice is present in global discourse but excluded from the internal logic of the constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_law_framework, excluded,
    institutional, civilizational, analytical, global).

% Benefit from the security and national identity provided by the state, which is strengthened by the religious-national narrative. However, they may also bear the costs of international isolation and ongoing conflict resulting from policies driven by this reading, without necessarily subscribing to the theological justification.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_citizens, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action and identity of the Jewish people around a shared theological and territorial vision, providing a coherent framework for national purpose and land claims.
% TRANSFER_FUNCTION: Transfers sovereignty and control over the land of Eretz Yisrael from its indigenous inhabitants (Palestinians) to the Jewish people, based on a divine mandate.
% ABSENT_VOICES: The Palestinian people's historical narrative, claims to self-determination, and rights under international law are systematically excluded from the foundational logic of this constraint. International legal bodies and human rights organizations also represent voices that are dismissed or subordinated.
% DISAPPEARANCE_RATIONALE: If the divine promise as an inalienable territorial claim vanished, the entire ideological and legal basis for the State of Israel's maximalist territorial policies would collapse. This would necessitate a fundamental re-evaluation of borders, sovereignty, and the rights of all inhabitants, leading to a complete rearrangement of the political landscape.
% FOUNDING_PROBLEM: The historical dispersion and persecution of the Jewish people, culminating in the Holocaust, necessitated a secure homeland where Jewish sovereignty could be exercised.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and many Israeli citizens attest that the problem of Jewish insecurity and the need for a sovereign homeland remains live, citing ongoing antisemitism and regional threats. This is corroborated by historical evidence of Jewish persecution and the continued need for self-determination, though the specific territorial maximalism is contested by many outside this reading.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) due to the absolute nature of the territorial claim, which demands full control over the land, leading to significant displacement and dispossession of the Palestinian people. Suppression is also very high (0.9) as this claim is actively enforced through military occupation, settlement expansion, and legal frameworks that deny Palestinian rights, with strong resistance from the Palestinian people (0.85). Theater ratio is low (0.1) because the theological justification is genuinely held and directly drives policy, with little performative cover. Accessibility collapse is high (0.9) because the divine mandate leaves almost no room for alternative territorial arrangements or shared sovereignty from this perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people as a covenant community, this is a just and necessary fulfillment of a divine promise. From the perspective of the Palestinian people, it is a structure of pure extraction and dispossession. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people as a covenant community and the State of Israel are the primary beneficiaries, experiencing the constraint as a fulfillment of destiny and a source of legitimacy. The Palestinian people are the primary victims, bearing the full cost of displacement and denial of self-determination. International law frameworks are excluded, as their principles are often deemed secondary to the divine mandate. Secular Israeli citizens are beneficiaries of national security and identity but may also bear costs from the conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine promise an empirically verifiable or universally accepted basis for territorial claims in contemporary international relations?',
    'Analysis of international legal precedents and diplomatic recognition of religiously-based territorial claims; assessment of the epistemic status of theological claims in secular governance.',
    'If not universally accepted, the constraint''s legitimacy in the international arena collapses, increasing external pressure and potentially reclassifying it as a snare from an international law perspective. If accepted, its legitimacy is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic status of divine mandate as a basis for sovereignty.').

omega_variable(
    palestinian_agency_and_resistance,
    'To what extent does Palestinian resistance and ongoing claims to self-determination challenge the ''accessibility collapse'' and ''suppression'' metrics, indicating a more dynamic and contested reality than the current values suggest?',
    'Longitudinal study of Palestinian political organization, demographic trends, and international advocacy efforts; analysis of the effectiveness of resistance in altering the constraint''s enforcement or perceived legitimacy.',
    'If Palestinian agency is found to significantly erode the constraint''s effective suppression or create viable alternative futures, the ''accessibility_collapse'' and ''suppression'' metrics might be lower, indicating a more ''tangled_rope'' dynamic where extraction is actively contested, rather than a fully ''snare'' where alternatives are completely foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_agency_and_resistance, empirical, 'Impact of Palestinian resistance on the constraint''s stability and perceived inevitability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2005, 0.9).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2014, 0.92).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2014, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_sovereignty_palestine' kernel. Its theological maximalism influences and is influenced by other readings of Jewish sovereignty and Palestinian rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
