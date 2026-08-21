% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionism as Jewish National Liberation
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint story models Zionism as a national liberation movement
 *   for a persecuted indigenous people returning to their ancestral homeland.
 *   From this reading's perspective, the movement's primary function is to
 *   secure self-determination and refuge for the Jewish people. The
 *   displacement of Palestinian Arabs is acknowledged as a consequence, but
 *   justified by historical connection and the imperative for Jewish national
 *   rights, with Arab opposition delegitimized as a denial of these rights.
 *   The claimed type is 'rope' reflecting the internal coordination function,
 *   while the metrics reflect the structural realities of displacement and
 *   suppression inherent in the establishment of the state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.65).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.8).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as Jewish National Liberation").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '2a67adaa-8fb5-419e-b10f-663c183d6920').
narrative_ontology:cs_kernel_codification('2a67adaa-8fb5-419e-b10f-663c183d6920', formalized).
narrative_ontology:cs_authority_grounding('2a67adaa-8fb5-419e-b10f-663c183d6920', lineage).
narrative_ontology:cs_interpretation_layer_present('2a67adaa-8fb5-419e-b10f-663c183d6920').
narrative_ontology:cs_reading_relation('2a67adaa-8fb5-419e-b10f-663c183d6920', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('2a67adaa-8fb5-419e-b10f-663c183d6920', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('2a67adaa-8fb5-419e-b10f-663c183d6920', foundational, jewish_right_to_self_determination_in_ancestral_land).
narrative_ontology:cs_axiom_status(jewish_right_to_self_determination_in_ancestral_land, holdable).
narrative_ontology:cs_axiom_grounding('2a67adaa-8fb5-419e-b10f-663c183d6920', jewish_right_to_self_determination_in_ancestral_land, deontological).
narrative_ontology:cs_axiom('2a67adaa-8fb5-419e-b10f-663c183d6920', foundational, historical_persecution_necessitates_sovereign_refuge).
narrative_ontology:cs_axiom_status(historical_persecution_necessitates_sovereign_refuge, holdable).
narrative_ontology:cs_axiom_grounding('2a67adaa-8fb5-419e-b10f-663c183d6920', historical_persecution_necessitates_sovereign_refuge, empirically_contingent).
narrative_ontology:cs_reference_frame('2a67adaa-8fb5-419e-b10f-663c183d6920', basel_program_and_un_partition).
narrative_ontology:cs_drift_state('2a67adaa-8fb5-419e-b10f-663c183d6920', contemporary_post_1967_expansion, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2a67adaa-8fb5-419e-b10f-663c183d6920', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_people_national_group).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, state_of_israel).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, international_zionist_organizations).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_right_to_self_determination).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, historical_connection_to_land_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks and finds national self-determination and refuge from persecution in their ancestral homeland, coordinating collective action to establish and maintain the State of Israel. Their identity is deeply intertwined with the success of this project, which is framed as a return to indigenous roots.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_people_national_group, beneficiary,
    powerful, generational, identity_locked, global).

% The sovereign entity established by the Zionist movement, responsible for governance, security, and the implementation of Zionist principles. It enforces laws and policies that secure the Jewish majority and national character of the state, justifying displacement as a necessary outcome of national liberation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, state_of_israel, agenda_setter,
    institutional, generational, arbitrage, national).

% Indigenous inhabitants who experienced displacement, dispossession, and loss of self-determination as a result of the Zionist project. Their opposition is delegitimized as denial of Jewish rights, and their ability to resist is severely constrained by the state's enforcement mechanisms.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Neighboring states that initially opposed the establishment of Israel, leading to conflicts and ongoing geopolitical tensions. They bear the costs of regional instability and refugee crises, with limited ability to alter the fundamental facts on the ground established by the Zionist project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states, payer,
    institutional, biographical, constrained, regional).

% Organizations worldwide that provide political, financial, and ideological support to the State of Israel and the Zionist movement, advocating for its legitimacy as a national liberation project on the global stage.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_zionist_organizations, beneficiary,
    organized, generational, mobile, global).

% International bodies and states that have recognized Israel's right to exist but also express concerns about Palestinian rights, settlements, and the ongoing conflict. They observe, mediate, and sometimes impose diplomatic pressure, navigating competing narratives of legitimacy.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_community_un, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, state_of_israel).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a secure homeland and self-determination for the Jewish people, coordinating their return and settlement in their ancestral land, and establishing a sovereign state to protect them from persecution.
% TRANSFER_FUNCTION: Transfers sovereignty, land, and resources from existing inhabitants to the Jewish national movement; transfers security, self-determination, and national identity to Jewish people.
% ABSENT_VOICES: Palestinian Arabs, whose narrative of indigenous rights and self-determination is systematically delegitimized and suppressed as a denial of Jewish rights. Anti-Zionist Jewish voices, who offer alternative visions for Jewish safety and identity, are also marginalized within this framework.
% DISAPPEARANCE_RATIONALE: If the Zionist project and the State of Israel vanished overnight, it would lead to a massive geopolitical rearrangement in the Middle East, a profound crisis for Jewish identity and security globally, and a fundamental shift in international relations. The arrangements for millions of people depend on its existence.
% FOUNDING_PROBLEM: Centuries of antisemitic persecution, pogroms, and the Holocaust, culminating in the urgent need for a secure national refuge for the Jewish people, coupled with a deep historical and religious connection to the land of Israel.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of antisemitism and persecution are widely documented by historians and international bodies. The UN Partition Plan and subsequent recognition of Israel by many nations attest to the international community's acknowledgment of the need for a Jewish state. However, Palestinian and Arab narratives contest the legitimacy of the solution chosen, arguing the problem was solved at their expense.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is set at 0.65, reflecting the significant costs borne by Palestinian Arabs through displacement and dispossession, even as this reading justifies these outcomes as necessary for Jewish national liberation. Suppression is high (0.8) due to the active delegitimization of Palestinian narratives and resistance, and the enforcement required to maintain the state's Jewish character. Theater ratio is low (0.1) because the national liberation narrative is deeply held and genuinely believed by its proponents, not primarily performative. Accessibility collapse (0.75) is high as alternatives to a Jewish state (e.g., a binational state) were actively suppressed. Resistance (0.8) is high due to ongoing Palestinian opposition. The measurement series track these dynamics over the history of the State of Israel.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people and the State of Israel, this constraint is a legitimate and necessary act of national liberation and self-determination. From the perspective of Palestinian Arabs, it is a process of dispossession and ongoing oppression. The engine will compute these divergent classifications based on the structural roles and metrics, highlighting the deep perspectival gap inherent in the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people (as a national group) and the State of Israel are the primary beneficiaries (low d), gaining self-determination, security, and sovereignty. Palestinian Arabs and Arab states are the primary targets/payers (high d), bearing the costs of displacement, dispossession, and regional instability. International Zionist organizations are beneficiaries, supporting the movement. The international community acts as an observer, mediating between competing claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contested_indigeneity_claim,
    'Is the claim of indigenous return universally accepted, or is it contested by other historical and anthropological narratives?',
    'Comparative historical and anthropological studies of indigenous movements and settler-colonialism, assessing the fit of the ''indigenous return'' framework against alternative interpretations.',
    'If the indigenous claim is widely contested or found to be a partial framing, it would weaken the moral and legal justification for displacement within this reading, potentially increasing its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_indigeneity_claim, conceptual, 'Ambiguity regarding the indigenous status of the Jewish people in the context of modern nationalism.').

omega_variable(
    displacement_justification_validity,
    'Is the displacement of Palestinian Arabs a necessary and justified consequence of Jewish national liberation, or an act of dispossession that undermines the liberation claim?',
    'Analysis of alternative historical pathways and proposed solutions (e.g., binationalism, federalism) that might have achieved Jewish self-determination without extensive displacement, or international legal precedents on indigenous rights versus national self-determination.',
    'If the displacement is found to be unjustifiable or avoidable, it would significantly increase the perceived extractiveness and suppression of this constraint, potentially reclassifying it towards a Snare or Tangled Rope even from a sympathetic perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_justification_validity, conceptual, 'The moral and political validity of justifying displacement for national liberation.').

omega_variable(
    historical_connection_vs_modern_nationalism,
    'To what extent does modern political Zionism genuinely represent an unbroken indigenous return, versus a modern nationalist project with colonial characteristics?',
    'Historical and sociological analysis comparing the development of Zionism to other 19th/20th-century nationalist movements and settler-colonial projects, focusing on land acquisition, population transfer, and state-building practices.',
    'If the ''modern nationalist with colonial characteristics'' framing gains stronger empirical support, it would fundamentally challenge the ''national liberation'' narrative, leading to a re-evaluation of the constraint''s legitimacy and its classification towards a more extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_connection_vs_modern_nationalism, empirical, 'The nature of Zionism as either indigenous return or a modern nationalist/colonial project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1987, 0.09).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(zion_tr_t2014, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2014, 0.11).
narrative_ontology:measurement(zion_tr_t2023, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1987, 0.65).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(zion_be_t2014, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2014, 0.66).
narrative_ontology:measurement(zion_be_t2023, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1987, 0.78).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement(zion_su_t2014, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2014, 0.81).
narrative_ontology:measurement(zion_su_t2023, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_right_of_return_denial).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israeli_settlement_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'zionist_legitimacy_basis' kernel. It focuses on Zionism as a national liberation movement for the Jewish people, emphasizing historical connection and the imperative for a secure homeland. Other readings, such as the 'settler_colonial_reading' and 'religious_restoration_reading', offer alternative framings of the same underlying historical phenomenon, with different structural implications and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
