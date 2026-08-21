% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Israeli Territorial Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Zionist Refuge' reading of
 *   Israeli territorial legitimacy. It frames Israel's existence and
 *   territorial control as justified by historical persecution, divine
 *   promise, and international recognition (UN Partition Plan), with 1948
 *   borders being uncontested and post-1967 boundaries negotiable based on
 *   security. Palestinian displacement is viewed as a consequence of Arab
 *   rejection of partition. This reading prioritizes the security and
 *   self-determination of the Jewish people. The metrics reflect the ongoing,
 *   actively enforced nature of this claim, which entails significant
 *   extraction from and suppression of the Palestinian population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.65).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.78).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Israeli Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'be94e0ec-9a5a-4569-9397-a3ff9aaf9d69').
narrative_ontology:cs_kernel_codification('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', formalized).
narrative_ontology:cs_authority_grounding('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', lineage).
narrative_ontology:cs_interpretation_layer_present('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69').
narrative_ontology:cs_reading_relation('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', foundational, jewish_self_determination_in_ancestral_homeland).
narrative_ontology:cs_axiom_status(jewish_self_determination_in_ancestral_homeland, holdable).
narrative_ontology:cs_axiom_grounding('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', jewish_self_determination_in_ancestral_homeland, deontological).
narrative_ontology:cs_axiom('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', foundational, un_partition_resolution_as_foundational_legitimacy).
narrative_ontology:cs_axiom_status(un_partition_resolution_as_foundational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', un_partition_resolution_as_foundational_legitimacy, conventional).
narrative_ontology:cs_reference_frame('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', post_1948_sovereign_state).
narrative_ontology:cs_drift_state('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', contemporary_international_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be94e0ec-9a5a-4569-9397-a3ff9aaf9d69', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts its right to exist within secure, defensible borders, drawing on historical, religious, and international legal claims. Administers and enforces territorial control, including in disputed areas, prioritizing security. Benefits from the current territorial arrangement and the international recognition it has secured.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the security and stability provided by the state's territorial control. Their identity and sense of belonging are deeply tied to the land. They bear the costs of ongoing conflict but perceive the current arrangement as necessary for survival and self-determination.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens, beneficiary,
    organized, biographical, constrained, national).

% Displaced from their homes in 1948 and subsequent conflicts, they bear the cost of dispossession and the denial of their right of return. Their situation is a direct consequence of the territorial claims and enforcement actions of the State of Israel, as interpreted by this reading.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under Israeli military occupation or administrative control, experiencing restrictions on movement, land use, and self-governance. Their daily lives are directly impacted by the territorial claims and security measures, with limited avenues for redress or exit.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_occupied_territories, payer,
    powerless, biographical, identity_locked, local).

% Provided the original partition plan and continues to monitor the conflict, passing resolutions that are often contested by the parties. Its role is to uphold international law and facilitate a peaceful resolution, but its enforcement power is limited.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, united_nations, observer,
    institutional, civilizational, analytical, global).

% Historically rejected the UN partition plan and engaged in conflicts with Israel. While some have normalized relations, others continue to challenge Israel's territorial claims, particularly regarding the occupied territories and Palestinian rights. Their historical actions are framed by this reading as contributing to Palestinian displacement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_states, excluded,
    institutional, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a secure homeland for the Jewish people, coordinating their historical, religious, and national aspirations with a sovereign territorial state, and offering refuge from persecution.
% TRANSFER_FUNCTION: Transfers territorial control and sovereignty from a contested historical status to the State of Israel, enabling the establishment of a Jewish majority state and the administration of land and resources. This implicitly transfers land and rights from displaced Palestinians.
% ABSENT_VOICES: Palestinian voices asserting continuous habitation and the right of return are largely absent from the foundational narratives of this reading, which prioritizes Jewish self-determination and security. Their displacement is framed as a consequence of regional conflict rather than a direct outcome of this legitimacy claim.
% DISAPPEARANCE_RATIONALE: If this reading of legitimacy vanished, the State of Israel's foundational claims to its current territory would be fundamentally undermined. This would necessitate a radical re-evaluation of borders, rights of return, and the very nature of the state, leading to a profound rearrangement of the geopolitical landscape and the lives of millions.
% FOUNDING_PROBLEM: The historical persecution of the Jewish people, culminating in the Holocaust, and the absence of a secure, sovereign homeland where Jewish self-determination could be exercised.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its supporters attest that the founding problem of Jewish insecurity and the need for a refuge remains live, citing ongoing antisemitism and regional threats. While some international bodies acknowledge the historical persecution, the 'live' status of the problem as a justification for current territorial arrangements is contested by Palestinian advocates and some international observers, who point to the displacement of another population as a consequence.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the ongoing cost borne by Palestinians in terms of land, resources, and self-determination, which is a direct outcome of this reading's territorial claims. Suppression (0.78) is high due to the active military and administrative enforcement required to maintain control over disputed territories and manage Palestinian resistance. The theater ratio (0.4) indicates that while security concerns are genuine, a significant portion of the justification and enforcement activity serves to maintain the territorial status quo and deflect challenges to its legitimacy, rather than solely addressing immediate threats. The claimed type is 'tangled_rope' because it genuinely coordinates the security and national aspirations of Israeli citizens while simultaneously extracting from and suppressing Palestinians through the same structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Israeli citizens, this constraint is a necessary 'rope' for survival and self-determination, ensuring a refuge from historical persecution. From the perspective of Palestinians, it operates as a 'snare' or 'tangled_rope,' enforcing their displacement and subjugation. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as coordination and victims as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and its citizens are the primary beneficiaries, gaining security, sovereignty, and a national home (low directionality). Palestinian refugees and residents of occupied territories are the primary targets, bearing the costs of displacement, occupation, and denial of rights (high directionality). The UN acts as an observer, while Arab states are largely excluded from this reading's internal logic, often framed as historical antagonists whose actions contributed to the current situation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_promise_empirical_status,
    'Is the ''divine promise'' a valid basis for territorial legitimacy in contemporary international law and secular political theory, or is it a theological claim that cannot be empirically or legally adjudicated?',
    'Conceptual analysis within international legal frameworks and political philosophy to determine the admissibility of theological claims in state legitimacy, or a global consensus shift on the role of religious texts in sovereignty.',
    'If deemed inadmissible, a foundational pillar of this reading''s legitimacy would be weakened, potentially shifting its classification towards a more purely constructed (and thus more extractive) constraint. If accepted, it reinforces the ''mountain-like'' aspect of the claim for its adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_empirical_status, conceptual, 'The role of divine promise in territorial legitimacy.').

omega_variable(
    security_justification_proportionality,
    'Are the security measures and territorial controls enacted under this reading proportional to the actual threats faced, or do they exceed what is necessary, serving instead to expand control and suppress dissent?',
    'Independent, verifiable assessment by international security experts and human rights organizations, comparing security outcomes with the impact on Palestinian populations and alternative, less restrictive measures.',
    'If disproportionate, the ''suppression'' and ''extractiveness'' metrics would be re-evaluated as higher, and the ''theater_ratio'' would increase, indicating that security is partly a cover for other objectives. This would push the classification further towards ''snare'' for the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_justification_proportionality, empirical, 'Proportionality of security measures to actual threats.').

omega_variable(
    palestinian_displacement_causality,
    'Is Palestinian displacement primarily a consequence of Arab rejection of the UN partition plan and subsequent wars, or a direct result of the Zionist project''s territorial establishment and expansion?',
    'Historical research and archival analysis, including declassified documents and oral histories from all parties, to reconstruct the causal chain of events and intentions leading to displacement.',
    'If displacement is found to be a direct result of the Zionist project, this reading''s ''extractiveness'' and ''suppression'' would be seen as more inherent and less a byproduct of external conflict, strengthening the ''snare'' aspect. If primarily due to Arab rejection, it reinforces the ''tangled_rope'' aspect where external factors contribute to the constraint''s extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_displacement_causality, empirical, 'Causality of Palestinian displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel. It focuses on Israel's legitimacy from a Zionist refuge perspective, distinct from Palestinian autochthony or two-state coexistence readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
