% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious Zionist Legitimacy Basis (Post-1967 Restoration Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models the religious Zionist interpretation of Zionism,
 *   particularly as it evolved after 1967, framing the establishment and
 *   expansion of the State of Israel as the fulfillment of divine promise and
 *   a step in the messianic process. This reading prioritizes religious
 *   obligation over secular political considerations and mandates territorial
 *   maximalism. It is one reading of the 'zionist_legitimacy_basis' kernel,
 *   alongside 'national_liberation_reading' and 'settler_colonial_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.92).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.95).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Zionist Legitimacy Basis (Post-1967 Restoration Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '1f51171a-fe66-4c85-8eaf-87dd650af9ce').
narrative_ontology:cs_kernel_codification('1f51171a-fe66-4c85-8eaf-87dd650af9ce', formalized).
narrative_ontology:cs_authority_grounding('1f51171a-fe66-4c85-8eaf-87dd650af9ce', lineage).
narrative_ontology:cs_interpretation_layer_present('1f51171a-fe66-4c85-8eaf-87dd650af9ce').
narrative_ontology:cs_reading_relation('1f51171a-fe66-4c85-8eaf-87dd650af9ce', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f51171a-fe66-4c85-8eaf-87dd650af9ce', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('1f51171a-fe66-4c85-8eaf-87dd650af9ce', foundational, divine_covenant_land_ownership).
narrative_ontology:cs_axiom_status(divine_covenant_land_ownership, holdable).
narrative_ontology:cs_axiom_grounding('1f51171a-fe66-4c85-8eaf-87dd650af9ce', divine_covenant_land_ownership, theological).
narrative_ontology:cs_axiom('1f51171a-fe66-4c85-8eaf-87dd650af9ce', foundational, messianic_redemption_through_territorial_control).
narrative_ontology:cs_axiom_status(messianic_redemption_through_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('1f51171a-fe66-4c85-8eaf-87dd650af9ce', messianic_redemption_through_territorial_control, theological).
narrative_ontology:cs_reference_frame('1f51171a-fe66-4c85-8eaf-87dd650af9ce', biblical_covenant_and_messianic_prophecy).
narrative_ontology:cs_drift_state('1f51171a-fe66-4c85-8eaf-87dd650af9ce', contemporary_political_reality, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1f51171a-fe66-4c85-8eaf-87dd650af9ce', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_political_parties).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_left).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_covenant_fulfillment).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_redemption_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively participate in settlement expansion in the West Bank, viewing it as a divinely mandated act of redemption. Their identity is deeply intertwined with the territorial claims and the religious interpretation of the state's purpose. They benefit from state support for settlements and the ideological justification for their actions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, beneficiary,
    organized, generational, identity_locked, regional).

% Form coalitions and influence government policy to promote settlement expansion, annexation, and the application of religious law, framing these actions as essential steps in the messianic process. They administer state resources to support their ideological goals and benefit from the political power derived from this mandate.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_political_parties, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of territorial expansion, displacement, and military occupation. Their land is confiscated, movement restricted, and political aspirations suppressed under a framework that denies their indigenous claims in favor of a divine mandate. They have no viable exit from the occupied territories.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population, payer,
    powerless, generational, trapped, regional).

% Opposes the religious-nationalist agenda, viewing it as undermining democratic values, perpetuating conflict, and leading to international isolation. They bear the costs of increased militarization, diplomatic pressure, and internal social division, but are constrained by the political dominance of the religious right.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_left, payer,
    moderate, biographical, constrained, national).

% Monitor and condemn settlement activity as violations of international law. Their observations and resolutions provide an alternative normative framework but have limited direct enforcement power against the religious Zionist interpretation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared religious-nationalist vision for the state of Israel, providing a coherent ideological framework for territorial expansion and the application of religious law, uniting diverse religious communities under a common messianic purpose.
% TRANSFER_FUNCTION: Transfers land, resources, and political power from the Palestinian population and secular Israeli institutions to religious Zionist settlers and political parties, justified by a divine mandate for territorial restoration.
% ABSENT_VOICES: Palestinian voices are systematically excluded from the discourse that defines the legitimacy of the state's actions, as their claims are rendered illegitimate by the religious framework. International legal frameworks are acknowledged but often dismissed as irrelevant to divine decree.
% DISAPPEARANCE_RATIONALE: If the religious restoration reading of Zionism vanished, the ideological justification for settlement expansion would collapse, leading to a profound crisis of legitimacy for the current government's policies. The political landscape would shift dramatically, potentially opening pathways for alternative political solutions and a re-evaluation of territorial claims.
% FOUNDING_PROBLEM: The perceived historical injustice of Jewish exile and the theological imperative to restore the biblical Land of Israel as a prelude to messianic redemption.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and their followers attest that the problem is profoundly live, citing ongoing threats to Jewish existence and the unfulfilled aspects of messianic prophecy. Critics, including secular Israelis and international observers, argue that while historical exile is real, the post-1967 interpretation of 'restoration' has become a cover for political expansionism, with corroboration from historical analyses of the shift in religious Zionist ideology post-1967.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.92) is high due to the systematic dispossession and subjugation of the Palestinian population, justified by a theological claim that overrides their rights. Suppression (0.95) is extremely high, as the constraint's persistence relies on active military and legal enforcement to maintain control over occupied territories and suppress Palestinian resistance. Theater ratio (0.15) is low because the religious mandate is genuinely held and actively pursued, with little performative pretense; the actions directly align with the stated theological goals. Accessibility collapse is high (0.88) because the religious framework fundamentally delegitimizes alternative claims to the land, making exit or alternative solutions nearly unthinkable for those operating within this frame. Resistance is high (0.90) due to ongoing Palestinian struggle against occupation and displacement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious Zionist settlers and political parties, this is a divinely mandated process of restoration, justifying all actions. From the perspective of the Palestinian population, it is a snare of pure extraction and violent displacement. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist settlers and political parties are clear beneficiaries and agenda-setters, deriving land, power, and ideological justification from this reading. The Palestinian population is the primary victim, bearing the full cost of displacement and subjugation. Secular Israeli leftists are also payers, bearing the costs of conflict and international isolation, though their exit options are less constrained than Palestinians'. International law bodies act as observers, providing an alternative analytical frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine restoration) is considered 'live' by its beneficiaries, preventing a mandatrophy resolution from within this frame. However, the high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that from an external perspective, the constraint functions as a snare, leveraging a religious narrative for political and territorial gain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine mandate for territorial maximalism an empirically verifiable claim, or a theological/deontological one?',
    'Conceptual analysis of theological texts and historical interpretations; no empirical resolution is possible for a purely theological claim.',
    'If purely theological, its force is internal to the belief system and cannot be refuted by external facts, making the constraint highly resistant to empirical challenge. If it were somehow empirically contingent, its legitimacy could be challenged by historical or archaeological evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic grounding of the divine mandate claim.').

omega_variable(
    messianic_process_political_instrumentalization,
    'To what extent is the ''messianic process'' narrative genuinely held as a theological belief, versus instrumentalized to justify political and territorial expansion?',
    'Sociological studies of religious Zionist communities, analysis of political rhetoric versus private theological discourse, and examination of policy outcomes independent of stated religious intent.',
    'If primarily instrumentalized, the constraint''s ''theater_ratio'' would be higher, and its ''extractiveness'' would be more clearly seen as political rather than divinely ordained, potentially weakening its legitimacy for some internal actors. If genuinely held, the constraint is more robust to secular critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_process_political_instrumentalization, empirical, 'The sincerity vs. instrumentalization of the messianic narrative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal discrimination) or internalized (psychological impact of religious-nationalist narratives on Palestinian identity)?',
    'Post-occupation suppression trajectory: if suppression persists after military and legal mechanisms are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after structural barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(zion_tr_t1980, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(zion_tr_t1995, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement(zion_tr_t2010, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(zion_be_t1980, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(zion_be_t1995, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(zion_be_t2010, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1980, 0.82).
narrative_ontology:measurement(zion_su_t1995, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1995, 0.88).
narrative_ontology:measurement(zion_su_t2010, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_expansion_policy).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_right_of_return_denial).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This 'religious_restoration_reading' emphasizes divine mandate and messianic process, distinct from the 'national_liberation_reading' (secular self-determination) and the 'settler_colonial_reading' (ethno-state through displacement). Each reading constitutes a structurally distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
