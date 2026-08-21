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
 *   human_readable: Zionist Legitimacy: Religious Restoration Reading (Post-1967)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models the religious Zionist interpretation of Zionism,
 *   particularly after the 1967 Six-Day War, which views the establishment
 *   and expansion of Israel as a fulfillment of divine promise and a step in
 *   the messianic process. This reading prioritizes religious obligation over
 *   secular political considerations and mandates territorial maximalism. It
 *   is one reading of the 'zionist_legitimacy_basis' kernel, distinct from
 *   national liberation or settler-colonial interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.88).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.92).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionist Legitimacy: Religious Restoration Reading (Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '07f0dec0-4884-4b15-bed7-825e82a0163c').
narrative_ontology:cs_kernel_codification('07f0dec0-4884-4b15-bed7-825e82a0163c', formalized).
narrative_ontology:cs_authority_grounding('07f0dec0-4884-4b15-bed7-825e82a0163c', lineage).
narrative_ontology:cs_interpretation_layer_present('07f0dec0-4884-4b15-bed7-825e82a0163c').
narrative_ontology:cs_reading_relation('07f0dec0-4884-4b15-bed7-825e82a0163c', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('07f0dec0-4884-4b15-bed7-825e82a0163c', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('07f0dec0-4884-4b15-bed7-825e82a0163c', foundational, divine_mandate_for_eretz_israel).
narrative_ontology:cs_axiom_status(divine_mandate_for_eretz_israel, holdable).
narrative_ontology:cs_axiom_grounding('07f0dec0-4884-4b15-bed7-825e82a0163c', divine_mandate_for_eretz_israel, theological).
narrative_ontology:cs_axiom('07f0dec0-4884-4b15-bed7-825e82a0163c', foundational, settlement_as_messianic_process).
narrative_ontology:cs_axiom_status(settlement_as_messianic_process, holdable).
narrative_ontology:cs_axiom_grounding('07f0dec0-4884-4b15-bed7-825e82a0163c', settlement_as_messianic_process, theological).
narrative_ontology:cs_reference_frame('07f0dec0-4884-4b15-bed7-825e82a0163c', post_1967_religious_awakening).
narrative_ontology:cs_drift_state('07f0dec0-4884-4b15-bed7-825e82a0163c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('07f0dec0-4884-4b15-bed7-825e82a0163c', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_dissenters).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_mandate_for_land).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_redemption_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively pursue and expand settlement in disputed territories, viewing it as a divine commandment and a step in the messianic process. Their identity is deeply fused with the land and the religious narrative, making any territorial compromise an existential threat to their worldview.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, agenda_setter,
    organized, generational, identity_locked, regional).

% Benefit from the ideological legitimacy and political support provided by the religious Zionist movement, which reinforces claims to the entire land. While not all institutions fully endorse the religious narrative, they often accommodate or enable its territorial expansion for political stability and coalition building.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions, beneficiary,
    institutional, generational, constrained, national).

% Bear the direct costs of territorial expansion, displacement, and loss of self-determination. Their presence is often viewed as an obstacle to the divine plan, leading to systematic suppression of their rights and claims. Exit options are severely limited by military occupation and legal restrictions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Pay the costs of international isolation, military burden, and internal social division resulting from policies driven by religious territorial maximalism. They are ideologically opposed to the religious justification for expansion but find their political influence constrained by the power of the religious Zionist bloc.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_dissenters, payer,
    moderate, biographical, constrained, national).

% Observes and often condemns the expansion of settlements and the associated human rights violations, but its ability to impose effective counter-constraints is limited by geopolitical considerations and the religious narrative's internal coherence for its adherents.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared religious-national identity and a collective sense of purpose among religious Zionists, providing a theological framework for territorial claims and political action, particularly post-1967.
% TRANSFER_FUNCTION: Transfers land, resources, and political power from the Palestinian population to religious Zionist settlers and the Israeli state, justified by a divine mandate.
% ABSENT_VOICES: Palestinian voices are systematically excluded from the discourse that legitimizes this constraint; their historical narrative and claims to the land are dismissed as irrelevant or antithetical to the divine plan. International legal frameworks are also often disregarded.
% DISAPPEARANCE_RATIONALE: If the religious justification for territorial maximalism vanished, the political landscape would fundamentally shift. The settlement enterprise would lose its primary ideological engine, leading to immense internal political upheaval, potential territorial concessions, and a re-evaluation of the Israeli state's relationship with the occupied territories.
% FOUNDING_PROBLEM: The perceived historical injustice of Jewish exile and the theological imperative to redeem the land of Israel as a step towards messianic redemption.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and their followers attest that the problem is profoundly live, citing ongoing spiritual and national imperatives. While secular historians might offer alternative interpretations, within the religious Zionist framework, the divine promise remains an active, unfulfilled mandate.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.88) because the religious mandate directly justifies the appropriation of land and resources from the Palestinian population. Suppression is also very high (0.92) as the theological imperative often overrides human rights concerns and international law, requiring active enforcement to maintain control over disputed territories and suppress Palestinian resistance. Theater ratio is low (0.15) because the religious justification is genuinely held and actively pursued, not merely performative; the actions directly align with the stated divine mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist perspective, this is a divinely mandated process, not an extractive constraint. From the Palestinian perspective, it is pure, religiously justified extraction and suppression. The engine's classification as 'snare' reflects the structural reality of extraction and suppression, regardless of the internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist settlers are primary beneficiaries and agenda-setters, as their identity and purpose are fulfilled by the constraint's operation (d near 0.0). Israeli state institutions benefit from the ideological support, though they also bear some costs of international condemnation (d closer to 0.2). The Palestinian population is the primary victim, bearing the full costs of displacement and loss of sovereignty (d near 1.0). Secular Israeli dissenters are payers, bearing the costs of internal division and international pressure (d closer to 0.7).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine promise, messianic process) is considered 'live' by its adherents, preventing mandatrophy. The classification as 'snare' highlights that even a deeply held, non-atrophied mandate can drive high extraction and suppression when it overrides the rights and claims of other populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine mandate for the land an empirically verifiable claim, or a theological/deontological one?',
    'This question is irresolvable by empirical means, as it pertains to theological belief. Resolution would require a shift in epistemic framework or a theological consensus.',
    'If treated as an empirical claim, its lack of empirical grounding would undermine the constraint''s legitimacy. As a theological claim, it remains internally coherent for adherents, making external challenge difficult.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic status of the divine mandate claim.').

omega_variable(
    territorial_maximalism_necessity,
    'Is territorial maximalism a necessary consequence of the religious restoration narrative, or an interpretation that could be decoupled?',
    'Analysis of diverse religious Zionist interpretations: do all interpretations mandate maximalist territorial control, or are there alternative readings that prioritize spiritual over physical redemption?',
    'If separable, the constraint''s extractiveness could be reduced without abandoning the core religious narrative. If inseparable, the high extractiveness is intrinsic to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_necessity, conceptual, 'Whether territorial maximalism is an intrinsic or contingent element of the religious restoration reading.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for religious Zionist settlers a genuine internal commitment versus a product of state-supported incentives and social pressure?',
    'Longitudinal studies of settler communities under varying levels of state support and social cohesion; analysis of individual exit narratives.',
    'If primarily state-supported, the ''identity_locked'' status is partly a function of external coercion, suggesting a higher degree of structural suppression. If primarily internal, it reinforces the deep ideological grounding of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'The balance between internal commitment and external support in settler identity-lock.').


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
narrative_ontology:measurement(zion_be_t1995, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1995, 0.82).
narrative_ontology:measurement(zion_be_t2010, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1980, 0.82).
narrative_ontology:measurement(zion_su_t1995, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1995, 0.87).
narrative_ontology:measurement(zion_su_t2010, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_expansion_policy).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_right_of_return).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This 'religious_restoration_reading' emphasizes divine promise and messianic process, influencing and being influenced by the 'national_liberation_reading' and 'settler_colonial_reading' through competing claims to legitimacy and territory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
