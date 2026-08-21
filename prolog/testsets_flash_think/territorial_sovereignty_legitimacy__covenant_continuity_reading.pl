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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   This constraint story instantiates the 'covenant_continuity_reading' of
 *   territorial sovereignty legitimacy in the Israeli-Palestinian context.
 *   This reading asserts that the legitimacy of Jewish sovereignty over the
 *   land derives from a combination of ancient divine promise (covenant),
 *   continuous Jewish presence in the land throughout history, and modern
 *   international recognition (Balfour Declaration, UN Partition Plan, 1948
 *   establishment). It frames the establishment of the Israeli state and
 *   subsequent actions (e.g., settlements) as a return to a pre-existing
 *   right, rather than the creation of a new right or an act of colonization.
 *   The constraint is claimed as a Mountain by its proponents, reflecting its
 *   perceived unchangeable, divinely-ordained nature, even as its operation
 *   is highly extractive and suppressive from other perspectives.
 *
 * KEY AGENTS:
 *   - israeli_state: Primary agenda_setter (institutional/constrained)
 *   - jewish_people: Primary beneficiary (organized/identity_locked)
 *   - palestinian_people: Primary payer (powerless/trapped)
 *   - arab_states: Secondary payer (institutional/constrained)
 *   - international_community: Observer (institutional/analytical)
 *   - religious_zionist_movements: Agenda setter (organized/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.9).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, mountain).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy: Covenant & Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).
domain_priors:emerges_naturally(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '9f466892-b6f2-4567-a60d-1e7d759f178d').
narrative_ontology:cs_kernel_codification('9f466892-b6f2-4567-a60d-1e7d759f178d', fixed_text).
narrative_ontology:cs_authority_grounding('9f466892-b6f2-4567-a60d-1e7d759f178d', lineage).
narrative_ontology:cs_interpretation_layer_present('9f466892-b6f2-4567-a60d-1e7d759f178d').
narrative_ontology:cs_reading_relation('9f466892-b6f2-4567-a60d-1e7d759f178d', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_reading_relation('9f466892-b6f2-4567-a60d-1e7d759f178d', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('9f466892-b6f2-4567-a60d-1e7d759f178d', foundational, divine_covenant_grants_land).
narrative_ontology:cs_axiom_status(divine_covenant_grants_land, holdable).
narrative_ontology:cs_axiom_grounding('9f466892-b6f2-4567-a60d-1e7d759f178d', divine_covenant_grants_land, theological).
narrative_ontology:cs_axiom('9f466892-b6f2-4567-a60d-1e7d759f178d', foundational, continuous_jewish_presence_maintains_claim).
narrative_ontology:cs_axiom_status(continuous_jewish_presence_maintains_claim, holdable).
narrative_ontology:cs_axiom_grounding('9f466892-b6f2-4567-a60d-1e7d759f178d', continuous_jewish_presence_maintains_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('9f466892-b6f2-4567-a60d-1e7d759f178d', biblical_mandate_and_historical_right).
narrative_ontology:cs_drift_state('9f466892-b6f2-4567-a60d-1e7d759f178d', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9f466892-b6f2-4567-a60d-1e7d759f178d', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_people).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary political entity asserting and enforcing this claim, deriving its foundational legitimacy and territorial rights from the ancient covenant, continuous presence, and international recognition. It actively defends and expands its territorial control based on this narrative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% As a collective, they are the beneficiaries of the divine promise and the continuous historical connection to the land, which this reading frames as their ancestral homeland. Their identity is deeply intertwined with this claim, making exit from its framework unthinkable for many.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_people, beneficiary,
    organized, generational, identity_locked, global).

% Bear the primary costs of this claim, experiencing displacement, loss of land, and denial of self-determination rights. Their historical narrative of continuous residence is subordinated or dismissed by this reading, and their options for political agency are severely constrained.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Experience political and diplomatic costs due to the ongoing conflict stemming from this claim. While some have normalized relations, others continue to oppose the claim's implications for Palestinian rights and regional stability. Their ability to influence the situation is limited by geopolitical realities.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states, payer,
    institutional, generational, constrained, regional).

% Comprises various states and international organizations that acknowledge aspects of the claim (e.g., UN Partition Plan, 1948 establishment) but often contest its broader territorial implications or its impact on Palestinian rights. They attempt to mediate and apply international law, but their influence is often limited.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_community, observer,
    institutional, generational, analytical, global).

% Actively promote and implement the covenant aspect of this claim, particularly regarding settlement expansion. Their actions are driven by a deep ideological commitment to the divine promise and continuous presence, viewing them as non-negotiable rights.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_movements, agenda_setter,
    organized, generational, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational narrative that coordinates national identity, purpose, and territorial claims for the Israeli state and Jewish people, linking historical, religious, and modern political justifications.
% TRANSFER_FUNCTION: Transfers ultimate legitimacy and territorial rights to the Israeli state and Jewish people, based on a combination of ancient covenant, continuous presence, and modern international recognition, at the cost of denying or diminishing the competing claims of the Palestinian people.
% ABSENT_VOICES: The voices of the indigenous Palestinian population, particularly those dispossessed or displaced since 1948, whose historical narrative of continuous residence and self-determination is largely excluded or subordinated within this framework.
% DISAPPEARANCE_RATIONALE: If this foundational narrative of legitimacy (covenant, continuity, and specific interpretations of international recognition) were to vanish, the entire basis for the Israeli state's territorial claims and national identity would be profoundly undermined, leading to a fundamental reordering of political and territorial arrangements in the region and a crisis of national purpose.
% FOUNDING_PROBLEM: To establish a legitimate and secure homeland for the Jewish people in their ancestral land, addressing centuries of diaspora, persecution, and the existential threat revealed by the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and many Jewish people attest that the founding problem of Jewish security and self-determination remains live. While international bodies corroborated the need for a Jewish homeland post-WWII, the specific territorial claims and the means of their implementation are contested by the Palestinian people and many international observers, who argue the problem has shifted or been resolved in ways that create new injustices.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_sovereignty_legitimacy__covenant_continuity_reading),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is very high (0.85) because this reading asserts a foundational right that inherently denies or subordinates the competing claims of another population, leading to significant material and political costs for them. `suppression` is also very high (0.90) as the claim requires active enforcement and the suppression of alternative narratives and political expressions to maintain its dominance. `theater_ratio` is low (0.10) because the core tenets of this reading are deeply held beliefs and historical interpretations, not mere performance. `accessibility_collapse` is high (0.80) for those who do not share this narrative, as it fundamentally limits their ability to assert alternative political futures. `resistance` is extremely high (0.95) due to the direct and profound conflict with the self-determination claims of the Palestinian people.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a just and natural order, a fulfillment of historical and divine promises, leading to a 'Mountain' classification from their seat. Opponents, however, experience it as a highly extractive and suppressive 'Snare' or 'Tangled Rope' that denies their fundamental rights. The engine's classification will highlight this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The `israeli_state` and `jewish_people` are clear beneficiaries, as the constraint directly legitimizes their territorial claims and national identity, leading to low directionality. The `palestinian_people` and `arab_states` are victims/payers, bearing the costs of displacement, conflict, and denied self-determination, resulting in high directionality. The `international_community` acts as an observer, attempting to mediate, with an analytical directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is far from mandatrophy. Its mandate (establishing and securing a Jewish homeland based on these principles) is actively pursued and fiercely defended. The high extractiveness and suppression are direct consequences of its live and contested function, not signs of atrophy. The 'founding_problem_status' being 'live' further confirms this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the ''ancient covenant (divine promise)'' an empirically verifiable claim, a theological truth, or a foundational narrative?',
    'This question is largely irresolvable by empirical means, as it pertains to theological or narrative frameworks. Resolution would depend on a shift in epistemic grounding or a re-evaluation of the role of religious texts in political legitimacy.',
    'If treated purely as a theological claim, its force in secular international law is diminished, potentially weakening the constraint''s legitimacy for non-adherents. If framed as a foundational narrative, its persuasive power depends on shared cultural acceptance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'Ambiguity regarding the epistemic status of the divine covenant claim.').

omega_variable(
    continuity_of_presence_threshold,
    'What constitutes ''continuous Jewish presence'' sufficient to ground a claim of territorial sovereignty, given periods of demographic shifts and varying degrees of political control?',
    'Historical and demographic analysis, potentially involving archaeological and genetic studies, combined with a conceptual agreement on the threshold for ''continuity'' in legal and political claims. This is highly contested.',
    'A stricter definition of ''continuous presence'' could weaken the historical component of this reading''s legitimacy, while a looser definition would reinforce it, potentially at the expense of other historical claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_of_presence_threshold, empirical, 'Ambiguity in the definition and historical evidence for ''continuous presence''.').

omega_variable(
    international_law_primacy,
    'To what extent do ancient covenants and historical claims supersede or interact with modern international law principles, such as self-determination and the prohibition on acquiring territory by force?',
    'International legal adjudication, diplomatic negotiations, and shifts in global norms regarding the hierarchy of legal and historical claims. This is a matter of ongoing legal and political debate.',
    'If modern international law is deemed primary, aspects of this reading (e.g., settlements) could be reclassified as illegal or illegitimate. If historical/covenantal claims are given primacy, it could undermine the international legal order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_primacy, preference, 'The unresolved tension between historical/covenantal claims and modern international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(terr_tr_t1938, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1938, 0.1).
narrative_ontology:measurement(terr_tr_t1959, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1959, 0.1).
narrative_ontology:measurement(terr_tr_t1980, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(terr_tr_t2001, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.4).
narrative_ontology:measurement(terr_be_t1938, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1938, 0.55).
narrative_ontology:measurement(terr_be_t1959, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1959, 0.7).
narrative_ontology:measurement(terr_be_t1980, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(terr_be_t2001, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2001, 0.82).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.5).
narrative_ontology:measurement(terr_su_t1938, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1938, 0.65).
narrative_ontology:measurement(terr_su_t1959, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1959, 0.75).
narrative_ontology:measurement(terr_su_t1980, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1980, 0.82).
narrative_ontology:measurement(terr_su_t2001, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2001, 0.87).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'territorial_sovereignty_legitimacy' kernel. Each reading presents a different structural basis for legitimacy, leading to different classifications and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
