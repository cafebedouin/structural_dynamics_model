% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Covenant & Continuity Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint describes the 'covenant and continuity' reading of
 *   territorial sovereignty legitimacy for the State of Israel. It asserts
 *   legitimacy from ancient divine promise, continuous Jewish presence in the
 *   land, and modern international recognition (Balfour, UN Partition, 1948
 *   establishment). This reading frames settlements as 'return' and views the
 *   Partition Plan as a compromise of pre-existing rights. The constraint is
 *   classified as a Snare due to its high extractiveness and suppression of
 *   alternative claims, particularly from the Palestinian population. The
 *   metrics reflect the ongoing enforcement required to maintain this claim
 *   against significant resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.92).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy: Covenant & Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'b619a517-2f5f-44f5-8c34-6a5891da5647').
narrative_ontology:cs_kernel_codification('b619a517-2f5f-44f5-8c34-6a5891da5647', formalized).
narrative_ontology:cs_authority_grounding('b619a517-2f5f-44f5-8c34-6a5891da5647', lineage).
narrative_ontology:cs_interpretation_layer_present('b619a517-2f5f-44f5-8c34-6a5891da5647').
narrative_ontology:cs_reading_relation('b619a517-2f5f-44f5-8c34-6a5891da5647', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_reading_relation('b619a517-2f5f-44f5-8c34-6a5891da5647', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('b619a517-2f5f-44f5-8c34-6a5891da5647', foundational, divine_covenant_as_land_grant).
narrative_ontology:cs_axiom_status(divine_covenant_as_land_grant, holdable).
narrative_ontology:cs_axiom_grounding('b619a517-2f5f-44f5-8c34-6a5891da5647', divine_covenant_as_land_grant, theological).
narrative_ontology:cs_axiom('b619a517-2f5f-44f5-8c34-6a5891da5647', foundational, continuous_jewish_presence_as_unbroken_claim).
narrative_ontology:cs_axiom_status(continuous_jewish_presence_as_unbroken_claim, holdable).
narrative_ontology:cs_axiom_grounding('b619a517-2f5f-44f5-8c34-6a5891da5647', continuous_jewish_presence_as_unbroken_claim, conventional).
narrative_ontology:cs_reference_frame('b619a517-2f5f-44f5-8c34-6a5891da5647', biblical_mandate_and_historical_continuity).
narrative_ontology:cs_drift_state('b619a517-2f5f-44f5-8c34-6a5891da5647', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b619a517-2f5f-44f5-8c34-6a5891da5647', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises sovereignty over the territory, grounding its legitimacy in a combination of ancient religious covenant, continuous historical presence, and modern international recognition. Benefits from the constraint by maintaining control and justifying its actions.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Bears the costs of this legitimacy claim, experiencing displacement, loss of land, and denial of self-determination. Their historical narrative and claims to the land are systematically suppressed by this reading.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Benefits from the existence and security of the State of Israel as a fulfillment of historical and religious aspirations, and as a refuge. Provides political and financial support, reinforcing the legitimacy claims.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora, beneficiary,
    organized, generational, mobile, global).

% Bear the geopolitical costs of the ongoing conflict and the displacement of the Palestinian population. Their own claims to regional stability and influence are challenged by the persistence of this legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states, payer,
    institutional, generational, constrained, regional).

% Acts as an international arbiter, having played a role in the 1947 Partition Plan and subsequent resolutions. Its legitimacy is invoked by both sides, but its capacity to enforce resolutions is limited by member state interests.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, united_nations, observer,
    institutional, generational, analytical, global).

% Analyze the legal arguments for and against the various claims to sovereignty, often highlighting the tension between historical rights, self-determination, and effective control. Their analysis can influence international opinion but has no direct enforcement power.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the historical, religious, and political narratives that underpin the claim to Jewish sovereignty over the land, providing a coherent basis for state action and international support.
% TRANSFER_FUNCTION: Transfers territorial control, resources, and political legitimacy to the State of Israel, and away from the Palestinian population and their claims to self-determination.
% ABSENT_VOICES: The voices of indigenous Palestinian communities dispossessed before 1948, whose existence and claims are largely erased by a narrative focused on ancient covenant and continuous Jewish presence, are absent from the core framing of this legitimacy claim.
% DISAPPEARANCE_RATIONALE: If this specific legitimacy claim vanished overnight, the foundational narrative for the State of Israel's territorial control would collapse, leading to a profound re-evaluation of borders, rights, and international obligations, fundamentally altering the geopolitical landscape of the Middle East.
% FOUNDING_PROBLEM: The historical problem of Jewish statelessness, persecution, and the aspiration for a national homeland, combined with the need to establish a secure and internationally recognized state.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and the Jewish diaspora attest the problem is live, citing ongoing antisemitism and security threats. Palestinian and Arab states, along with many international observers, attest that while the founding problem was real, the current arrangement has created new problems of dispossession and conflict, making its status contested.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading directly justifies the dispossession and control of land and resources from the Palestinian population. Suppression is very high (0.92) as it requires active military, legal, and political enforcement to counter alternative claims and maintain control. The 'continuous presence' aspect, while historically complex, is used to suppress the significance of modern demographic realities. Theater ratio is moderate (0.45): while there are genuine security concerns, a significant portion of the narrative and enforcement serves to maintain the legitimacy claim itself, rather than purely functional state operations. The temporal measurements show a general increase in extractiveness and suppression, reflecting the hardening of positions and expansion of control over time, with some fluctuations around peace processes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and its supporters, this constraint is a foundational 'Rope' or even 'Mountain' of historical and divine right, enabling coordination for national survival. From the Palestinian perspective, it is a 'Snare' that actively dispossesses and suppresses their existence and rights. The engine's classification as Snare reflects the structural asymmetry and high extraction inherent in this specific reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and the Jewish diaspora are primary beneficiaries, gaining territorial control, security, and the fulfillment of a national-religious narrative. The Palestinian population and Arab states are victims, bearing the costs of displacement, conflict, and denial of self-determination. International bodies and scholars act as observers, analyzing the claims and consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (establishing a secure Jewish homeland) is still 'live' for its beneficiaries. However, the means of achieving this mandate, as interpreted by this reading, have become highly extractive and suppressive for others. The classification as Snare prevents mislabeling this as a benign coordination mechanism by highlighting the active enforcement and identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_presence_vs_demographic_majority,
    'To what extent does ''continuous Jewish presence'' (historical claim) legitimately override or coexist with ''demographic majority and continuous residence'' (modern self-determination claim) in determining sovereignty?',
    'International legal consensus on the hierarchy of historical vs. modern self-determination principles, or a negotiated political settlement that reconciles both claims.',
    'If modern demographic majority is prioritized, the extractiveness of this reading would be significantly higher, as it actively suppresses that claim. If historical presence is given equal or greater weight, the extractiveness might be perceived as lower by its proponents, but the suppression of the counter-claim remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_presence_vs_demographic_majority, conceptual, 'Ambiguity in weighting historical vs. modern claims to land.').

omega_variable(
    divine_promise_as_legal_basis,
    'Is ''divine promise'' a legitimate basis for territorial sovereignty in a secular international legal framework, or is it a theological claim that cannot be adjudicated by international law?',
    'A shift in international legal norms to either incorporate or explicitly exclude theological claims as a basis for state sovereignty, or a re-framing of the claim by its proponents into purely historical/political terms.',
    'If divine promise is deemed illegitimate as a legal basis, the ''naturalness'' of this constraint would collapse, increasing its perceived extractiveness and reliance on pure force. If it were somehow integrated, the constraint''s legitimacy would be bolstered, but its universal applicability would be challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_as_legal_basis, conceptual, 'Theological vs. secular grounding of sovereignty claims.').

omega_variable(
    partition_as_creation_vs_compromise,
    'Was the 1947 UN Partition Plan an act of creating new rights to sovereignty, or a compromise of pre-existing, divinely-ordained rights?',
    'A re-interpretation of the historical and legal intent of the UN resolution by international bodies, or a consensus among historical scholars on its foundational nature.',
    'If the Partition Plan created new rights, then subsequent actions beyond its scope (e.g., settlements) are more clearly extractive. If it was a compromise of pre-existing rights, then the current state might be seen as a partial fulfillment of those rights, reducing perceived extractiveness by proponents but not by victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_as_creation_vs_compromise, conceptual, 'Interpretation of the 1947 UN Partition Plan''s legal effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(terr_tr_t2014, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2014, 0.5).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.75).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(terr_be_t2014, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2014, 0.88).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(terr_su_t2014, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2014, 0.95).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, settlement_expansion_legitimacy).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, right_of_return_denial).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_sovereignty_legitimacy' kernel. Its claims directly influence and are influenced by other readings of sovereignty and related policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
