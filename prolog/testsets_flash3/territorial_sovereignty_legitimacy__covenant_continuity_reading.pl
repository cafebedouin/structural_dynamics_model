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
 *   human_readable: Territorial Sovereignty Legitimacy (Covenant-Continuity Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'covenant_continuity_reading' of
 *   territorial sovereignty legitimacy in the Israeli-Palestinian conflict.
 *   It posits that the legitimacy of the State of Israel's sovereignty
 *   derives from a combination of ancient divine promise (covenant),
 *   continuous Jewish presence in the land, and modern international
 *   recognition (Balfour Declaration, UN Partition Plan, 1948 establishment).
 *   This reading frames the State of Israel's existence and territorial
 *   claims as a fulfillment of a pre-existing right, rather than solely a
 *   creation of modern political acts. Settlements are viewed as a return to
 *   ancestral lands, and the temporal scope of legitimacy extends to biblical
 *   periods, surviving periods of demographic absence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.65).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.7).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy (Covenant-Continuity Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '39800e41-cc71-4e15-8cf4-07f9ac1fbdd7').
narrative_ontology:cs_kernel_codification('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', fixed_text).
narrative_ontology:cs_authority_grounding('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', lineage).
narrative_ontology:cs_interpretation_layer_present('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7').
narrative_ontology:cs_reading_relation('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', foundational, divine_covenant_as_foundational_right).
narrative_ontology:cs_axiom_status(divine_covenant_as_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', divine_covenant_as_foundational_right, theological).
narrative_ontology:cs_axiom('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', foundational, unbroken_jewish_presence_as_legitimacy).
narrative_ontology:cs_axiom_status(unbroken_jewish_presence_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', unbroken_jewish_presence_as_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', biblical_covenant_and_historical_presence).
narrative_ontology:cs_drift_state('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('39800e41-cc71-4e15-8cf4-07f9ac1fbdd7', '').
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

% Asserts sovereignty based on historical, religious, and international legal claims. Actively enforces its territorial control and legal framework, viewing settlements as a return to ancestral lands. Benefits from the continuity narrative by legitimizing its presence and actions.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Experiences loss of land, displacement, and restricted movement under the asserted sovereignty. Their claims to self-determination and continuous residence are suppressed by the dominant narrative and its enforcement. Bears the direct costs of territorial disputes and occupation.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Benefits from the existence of a Jewish state, often viewing it as a fulfillment of historical and religious aspirations. Provides political and financial support, reinforcing the covenant-continuity narrative. Does not directly administer the constraint but gains identity and security from it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora, beneficiary,
    organized, generational, mobile, global).

% Bear political and economic costs from the ongoing conflict, including refugee burdens and regional instability. Their support for Palestinian self-determination is often in tension with their own national interests and international relations. Their options are limited by geopolitical realities.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states, payer,
    institutional, generational, constrained, regional).

% Attempts to mediate the conflict and uphold international law, including resolutions related to the partition plan and the status of occupied territories. Its legitimacy is invoked by all parties, but its enforcement power is limited by member state interests.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, united_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent historical and legal framework for the establishment and continued existence of the State of Israel, coordinating national identity, historical memory, and international diplomatic efforts around a foundational narrative.
% TRANSFER_FUNCTION: Transfers legitimacy and territorial control to the State of Israel, drawing on ancient religious texts, historical presence, and modern international agreements. This transfer is at the expense of alternative claims to sovereignty and self-determination in the same territory.
% ABSENT_VOICES: The Palestinian population's historical narrative, emphasizing continuous residence and self-determination, is largely excluded from the dominant discourse that frames legitimacy through covenant and continuity. Indigenous rights advocates and critical post-colonial scholars also challenge this framing.
% DISAPPEARANCE_RATIONALE: If this specific reading of sovereignty legitimacy vanished, the foundational justification for the State of Israel's territorial claims would be severely undermined. This would necessitate a radical re-evaluation of borders, rights, and historical narratives, leading to a profound rearrangement of political and social structures in the region and globally.
% FOUNDING_PROBLEM: The historical problem of Jewish statelessness and persecution, coupled with the desire for national self-determination and a return to an ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Jewish national security and self-determination is attested by historical events (Holocaust, antisemitism) and ongoing regional threats, corroborated by international bodies (e.g., UN resolutions on Israel's right to exist) and a broad consensus within the Jewish community globally. However, the specific territorial claims derived from this problem are contested by the Palestinian population and many international observers.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (providing a framework for national identity and statehood) but also involves significant asymmetric extraction from the Palestinian population, whose counter-claims are suppressed. Extractiveness is high (0.65) due to the displacement and dispossession experienced by Palestinians. Suppression is also high (0.7) as the narrative actively delegitimizes alternative claims and requires continuous enforcement to maintain territorial control. The theater ratio is moderate (0.2), reflecting that while there are genuine historical and religious elements, some aspects of the narrative are performatively maintained to justify ongoing actions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and the Jewish diaspora, this reading provides a coherent and deeply meaningful basis for sovereignty, offering security and identity. From the perspective of the Palestinian population, it is a narrative of dispossession and ongoing injustice, actively suppressing their claims to self-determination and historical presence. The engine's classification will reflect this divergence, showing a beneficiary outcome for the former and a victim outcome for the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and the Jewish diaspora are beneficiaries, as the constraint directly legitimizes their claims and provides a framework for national identity and security. The Palestinian population and Arab states are victims, as their counter-claims are suppressed, and they bear the costs of territorial disputes and displacement. The UN acts as an observer, attempting to mediate without direct enforcement power over the core legitimacy claims.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification helps prevent mislabeling by highlighting the dual nature of the constraint. While it coordinates a powerful national identity and historical narrative (Rope-like function), it simultaneously extracts heavily from a disempowered population through active enforcement (Snare-like function). The 'contested' status of the founding problem further underscores the ongoing tension between the original mandate and its current extractive operation, preventing it from being seen as a pure coordination mechanism or a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_presence_threshold,
    'What constitutes ''continuous presence'' sufficient to ground a sovereignty claim, and how does it weigh against other forms of historical or demographic presence?',
    'Comparative legal analysis of indigenous land claims and international law on historical rights, combined with demographic and archaeological evidence over long time scales.',
    'A higher threshold for ''continuous presence'' or a stronger weighting for recent demographic majorities would weaken this reading''s claim, potentially shifting its classification towards a more extractive type. A lower threshold or stronger weighting would reinforce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_presence_threshold, conceptual, 'Ambiguity in defining and weighing ''continuous presence'' in sovereignty claims.').

omega_variable(
    divine_covenant_status,
    'Is a divine covenant a valid basis for modern territorial sovereignty in international law, or is it a theological claim distinct from secular legal frameworks?',
    'Analysis of international legal precedents and the evolving role of religious claims in state formation and recognition. Examination of how other states with religious founding narratives navigate secular international law.',
    'If divine covenant is deemed an invalid or secondary basis for secular sovereignty, the ''covenant_continuity_reading'' loses a significant pillar of its legitimacy, increasing its reliance on active enforcement and potentially shifting its classification towards a Snare. If it is recognized as a legitimate historical grounding, the reading''s coordination function is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_covenant_status, conceptual, 'The legal and political status of divine covenant as a basis for sovereignty.').

omega_variable(
    partition_plan_interpretation,
    'Is the UN Partition Plan (1947) interpreted as creating new rights to sovereignty for both Jewish and Arab populations, or as a compromise on pre-existing rights?',
    'Detailed historical and legal analysis of the intent and reception of the Partition Plan by all parties and international bodies at the time, and its subsequent interpretation in international jurisprudence.',
    'If the Partition Plan is seen as creating new rights, it challenges the ''pre-existing right'' aspect of the covenant-continuity reading, potentially increasing its perceived extractiveness. If it''s seen as a compromise on pre-existing rights, it reinforces the reading''s historical claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_plan_interpretation, empirical, 'Interpretation of the UN Partition Plan''s role in establishing sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.4).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_sovereignty_legitimacy' kernel. It is linked to 'self_determination_reading' and 'existential_matrix_reading' as sibling interpretations of the same core issue.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
