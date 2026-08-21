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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Zionism as Religious Restoration (Post-1967 Reading)
 *   domain: political_history/nationalism/religious_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'religious_restoration_reading' of
 *   the 'zionist_legitimacy_basis' kernel. It describes Zionism as the
 *   fulfillment of a divine promise and a messianic process, particularly as
 *   interpreted and acted upon by religious Zionist movements post-1967. This
 *   reading emphasizes religious obligation over secular political
 *   considerations and mandates territorial maximalism. The high
 *   extractiveness and suppression reflect the consequences for those not
 *   included in the divine mandate, while the low theater ratio indicates a
 *   deeply held and genuinely enacted belief system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.88).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.92).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionism as Religious Restoration (Post-1967 Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religious_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'aac11af0-c116-482d-86c3-dce740e6435c').
narrative_ontology:cs_kernel_codification('aac11af0-c116-482d-86c3-dce740e6435c', fixed_text).
narrative_ontology:cs_authority_grounding('aac11af0-c116-482d-86c3-dce740e6435c', lineage).
narrative_ontology:cs_interpretation_layer_present('aac11af0-c116-482d-86c3-dce740e6435c').
narrative_ontology:cs_reading_relation('aac11af0-c116-482d-86c3-dce740e6435c', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('aac11af0-c116-482d-86c3-dce740e6435c', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('aac11af0-c116-482d-86c3-dce740e6435c', foundational, divine_mandate_for_land_of_israel).
narrative_ontology:cs_axiom_status(divine_mandate_for_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('aac11af0-c116-482d-86c3-dce740e6435c', divine_mandate_for_land_of_israel, theological).
narrative_ontology:cs_axiom('aac11af0-c116-482d-86c3-dce740e6435c', foundational, messianic_redemption_through_statehood).
narrative_ontology:cs_axiom_status(messianic_redemption_through_statehood, holdable).
narrative_ontology:cs_axiom_grounding('aac11af0-c116-482d-86c3-dce740e6435c', messianic_redemption_through_statehood, theological).
narrative_ontology:cs_reference_frame('aac11af0-c116-482d-86c3-dce740e6435c', biblical_covenant_and_return).
narrative_ontology:cs_drift_state('aac11af0-c116-482d-86c3-dce740e6435c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aac11af0-c116-482d-86c3-dce740e6435c', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionists).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions_aligned_with_religious_zionism).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israelis_opposed_to_religious_maximalism).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, international_law_frameworks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents who believe the establishment and expansion of the State of Israel is a divinely mandated step in the messianic process. They actively promote policies of territorial maximalism and settlement, viewing the land as an inalienable religious inheritance. Their identity is deeply fused with this mission.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionists, agenda_setter,
    organized, generational, identity_locked, national).

% Government ministries, military units, and legal bodies whose policies and actions are shaped by or benefit from the religious Zionist narrative, particularly regarding settlement expansion and control over disputed territories. They provide the enforcement mechanism for the territorial claims.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions_aligned_with_religious_zionism, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_institutions_aligned_with_religious_zionism, agenda_setter).

% The indigenous population whose land, resources, and self-determination are directly impacted by the territorial claims and settlement activities justified by this religious reading. They bear the primary costs of displacement, loss of sovereignty, and violence.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinians, payer,
    powerless, generational, trapped, regional).

% Citizens who may support a Jewish state but reject the religious justification for territorial maximalism, often on pragmatic, democratic, or ethical grounds. They bear the social, political, and security costs of policies driven by this reading, and face internal political marginalization.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israelis_opposed_to_religious_maximalism, payer,
    moderate, biographical, constrained, national).

% International bodies, states, and NGOs that observe and often critique the actions justified by this reading, particularly concerning human rights and international law. Their influence is often limited by geopolitical considerations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_community, observer,
    institutional, civilizational, analytical, global).

% The body of treaties, customs, and principles (e.g., Geneva Conventions, right to self-determination) that are often invoked by critics but are systematically disregarded or reinterpreted by adherents of this reading when they conflict with its core tenets.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_law_frameworks, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zionist_legitimacy_basis__religious_restoration_reading, international_law_frameworks).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionists).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies religious Zionist communities and political factions around a shared, divinely ordained mission to settle and control the entire Land of Israel, providing a coherent ideological framework for political action and territorial expansion.
% TRANSFER_FUNCTION: Transfers land, water, and sovereignty from Palestinians to Israeli settlers and state control, justified by a theological claim to the land. It also transfers political and moral authority to religious interpretations of national destiny.
% ABSENT_VOICES: Palestinians are structurally excluded from the discourse that legitimizes their displacement. International legal frameworks are dismissed as irrelevant or hostile. Secular Israeli critics are often marginalized within the national narrative.
% DISAPPEARANCE_RATIONALE: If the belief in Zionism as a divinely mandated messianic process vanished overnight, the primary ideological justification for settlement expansion and territorial maximalism would collapse. This would fundamentally alter the political landscape, potentially leading to a re-evaluation of borders, a shift in national priorities, and a profound crisis of identity for many adherents.
% FOUNDING_PROBLEM: The historical exile and persecution of the Jewish people, culminating in the Holocaust, combined with a theological belief in the divine promise of the Land of Israel as an eternal inheritance, necessitating a return and restoration.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (exile, persecution, divine promise) is attested by religious texts (Torah, Talmud), rabbinic tradition, and historical narratives within the religious Zionist community. However, the 'divine promise' aspect is not corroborated by external, secular, or international legal bodies, which often view the conflict through lenses of national self-determination or settler-colonialism.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very high (0.88) because this reading justifies the appropriation of land and resources from Palestinians based on a theological claim, with little to no compensation or recognition of their rights. Suppression is also very high (0.92) as the state apparatus, influenced by this ideology, actively enforces territorial control and suppresses Palestinian resistance, often through military and legal means. Accessibility collapse is high (0.80) for those who do not share the religious premise, as their claims are rendered illegitimate. Resistance is high (0.75) from Palestinians and their allies. Theater ratio is low (0.10) because the belief in divine mandate is genuinely held and forms the core justification for actions, rather than being a mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious Zionists, this constraint is a righteous fulfillment of a divine covenant, a 'rope' coordinating a sacred mission. From the perspective of Palestinians, it is a 'snare' of dispossession and oppression. The engine's classification as 'tangled_rope' captures both the strong internal coordination function for adherents and the severe external extraction for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionists and aligned Israeli state institutions are the primary beneficiaries, gaining land, political power, and a sense of fulfilling a divine mission. Palestinians are the primary victims, experiencing displacement, loss of land, and denial of self-determination. Secular Israelis who oppose this maximalist interpretation also bear costs in terms of social division and international isolation. International law frameworks are excluded, as their principles are often overridden by the religious mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by highlighting the active, coercive enforcement required to maintain the constraint, despite its strong internal coordination narrative. The 'live' status of the founding problem (divine promise) from the adherents' perspective, combined with the 'world_rearranges' disappearance verdict, indicates a deeply embedded and actively maintained constraint, not a piton. The high extractiveness and suppression further distinguish it from a genuine rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine mandate for territorial maximalism an empirically verifiable claim, a theological truth, or a political construct?',
    'This question is fundamentally irresolvable by empirical means. Resolution depends on adopting a specific epistemological framework (e.g., theological, secular-historical, post-colonial studies).',
    'If treated as an empirical claim and found wanting, the legitimacy basis for territorial expansion would collapse. If accepted as a theological truth, it remains an unchallengeable foundation for adherents. If seen as a political construct, it becomes subject to political and ethical critique.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic status of the divine mandate claim.').

omega_variable(
    legitimacy_of_religious_claims_in_statecraft,
    'To what extent should religious claims serve as the primary basis for state policy and territorial sovereignty in a pluralistic international system?',
    'Resolution depends on a normative preference for secular governance, religious law, or a hybrid model. It is a question of political philosophy and international relations, not empirical fact.',
    'If religious claims are deemed an illegitimate basis for state policy, the constraint''s legitimacy would be severely undermined in the international arena. If accepted, it would strengthen the constraint''s internal coherence but likely exacerbate external conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_religious_claims_in_statecraft, preference, 'The normative acceptability of religious claims as a basis for state policy.').

omega_variable(
    coordination_vs_extraction_for_adherents,
    'Is the internal coordination function for religious Zionists genuinely beneficial, or is it a form of identity-locked extraction where adherents pay high costs (e.g., social isolation, conflict) for a perceived divine reward?',
    'Longitudinal studies of adherent well-being, exit narratives, and comparative analysis with non-identity-locked communities pursuing similar goals. If exit costs are primarily social/identity-based rather than material, it points to identity-locked extraction.',
    'If found to be identity-locked extraction, the ''rope'' aspect of the Tangled Rope classification would be re-evaluated as a more subtle form of snare, even for its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_for_adherents, empirical, 'Whether internal coordination for adherents is also a form of identity-locked extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(zion_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(zion_tr_t1997, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1997, 0.09).
narrative_ontology:measurement(zion_tr_t2007, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2007, 0.09).
narrative_ontology:measurement(zion_tr_t2017, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(zion_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.75).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement(zion_be_t1997, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1997, 0.83).
narrative_ontology:measurement(zion_be_t2007, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2007, 0.85).
narrative_ontology:measurement(zion_be_t2017, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2017, 0.87).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(zion_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.8).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(zion_su_t1997, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1997, 0.88).
narrative_ontology:measurement(zion_su_t2007, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2007, 0.9).
narrative_ontology:measurement(zion_su_t2017, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2017, 0.91).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_expansion).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_right_of_return_denial).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, jerusalem_sovereignty_claims).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This 'religious_restoration_reading' focuses on divine mandate and messianic process, distinct from the 'national_liberation_reading' (secular self-determination) and the 'settler_colonial_reading' (analytical framework of indigenous displacement). Each reading presents a structurally distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
