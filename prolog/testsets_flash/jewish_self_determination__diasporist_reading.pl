% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Vision of Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the diasporist reading of Jewish
 *   self-determination, which posits that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and minority
 *   rights, rather than territorial sovereignty. It views Zionism as a
 *   dangerous deviation that ties Jewish fate to a militarized state,
 *   endangering diaspora Jews and suppressing alternative Jewish political
 *   expressions. The constraint is classified as a Piton because the
 *   diasporist alternative, while historically significant, has seen its
 *   institutional support and public visibility atrophy under the hegemony of
 *   Zionist narratives, requiring performative maintenance to remain visible.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary beneficiary (organized/mobile) — seeks pluralistic survival.
 *   - jews_coerced_into_zionism: Primary payer (powerless/identity_locked) — sacrifices pluralistic identity.
 *   - jews_endangered_by_israel_actions: Secondary payer (powerless/trapped) — faces increased risk due to conflation.
 *   - zionist_institutions: Agenda setter (institutional/constrained) — actively suppresses diasporist alternatives.
 *   - host_nations: Observer (institutional/analytical) — impacts diasporist viability through policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.65).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Vision of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__diasporist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56').
narrative_ontology:cs_kernel_codification('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', distributed).
narrative_ontology:cs_authority_grounding('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', distributed).
narrative_ontology:cs_reading_relation('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', foundational, diaspora_pluralism_is_optimal_security).
narrative_ontology:cs_axiom_status(diaspora_pluralism_is_optimal_security, holdable).
narrative_ontology:cs_axiom_grounding('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', diaspora_pluralism_is_optimal_security, instrumental).
narrative_ontology:cs_axiom('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', foundational, territorial_sovereignty_endangers_jews).
narrative_ontology:cs_axiom_status(territorial_sovereignty_endangers_jews, holdable).
narrative_ontology:cs_axiom_grounding('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', territorial_sovereignty_endangers_jews, empirically_contingent).
narrative_ontology:cs_reference_frame('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', post_emancipation_diaspora_flourishing).
narrative_ontology:cs_drift_state('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', contemporary_zionist_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bbf8a7d5-a8ba-4afc-a431-a1fa05a35c56', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israel_actions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities seek to maintain distinct Jewish identities and cultures within diverse host nations, advocating for minority rights and pluralism as the most secure path for Jewish continuity. They benefit from a framework that de-links Jewish fate from a single state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Jews who feel pressured or ideologically compelled to align with Zionist narratives, even if it contradicts their own political or ethical views. They pay by sacrificing their pluralistic identity and critical thought, often facing ostracization if they dissent.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    powerless, biographical, identity_locked, global).

% Jews in diaspora who face increased antisemitism or security risks due to actions of the Israeli state, with which they are often conflated by non-Jewish populations. They pay with their safety and sense of belonging in their home countries.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israel_actions, payer,
    powerless, immediate, trapped, global).

% Organizations and political bodies that actively promote Zionism as the sole legitimate expression of Jewish self-determination. They work to suppress alternative Jewish political visions and frame dissent as disloyalty, maintaining a monopoly on representing 'Jewish interests'.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Governments and societies where diaspora Jewish communities reside. Their policies on minority rights and religious freedom directly impact the viability of diasporist flourishing, but they are often influenced by the dominant Zionist narrative regarding Jewish identity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_nations, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish communities globally around a shared vision of pluralistic survival and advocacy for minority rights, fostering solidarity across diverse national contexts.
% TRANSFER_FUNCTION: Transfers political and cultural capital from a singular, militarized state project (Zionism) back to diverse, locally rooted diaspora communities, emphasizing cultural and religious continuity over territorial nationalism.
% ABSENT_VOICES: Many secular and religious anti-Zionist Jewish voices, particularly those from Mizrahi and Sephardic traditions, have been historically marginalized or silenced within mainstream Jewish institutions dominated by Ashkenazi Zionist perspectives. They would advocate for a radical re-evaluation of Jewish identity and political strategy.
% DISAPPEARANCE_RATIONALE: If the diasporist constraint vanished, the default would likely revert to a Zionist-hegemonic understanding of Jewish identity, further marginalizing pluralistic alternatives and potentially increasing the vulnerability of diaspora Jews by tying their fate more tightly to the Israeli state.
% FOUNDING_PROBLEM: The historical problem of Jewish vulnerability to antisemitism and persecution, and the question of how to secure Jewish continuity and flourishing in a post-emancipation world.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish political thought and scholars of diaspora studies, independent of Zionist institutions, corroborate that the question of Jewish security and identity remains a live and contested problem, with diasporism offering a distinct, historically grounded answer.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the cost to diaspora Jews of having their identity and political interests monopolized by Zionism, leading to a loss of pluralism and increased vulnerability. Suppression (0.65) is high due to the active efforts of Zionist institutions to marginalize and delegitimize diasporist perspectives, limiting the space for alternative Jewish political thought. The high theater ratio (0.7) indicates that much of the 'maintenance' of the diasporist vision is performative, struggling to gain institutional traction against the dominant Zionist narrative, rather than being a robust, self-sustaining movement. The values reflect the period from the founding of Israel (1948) to the present, during which Zionist hegemony has steadily increased.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, the diasporist reading is a dangerous, atavistic, or even antisemitic rejection of Jewish self-determination, and thus would compute as a Snare or Tangled Rope that undermines Jewish security. From the diasporist perspective, Zionism itself is the extractive force, and the diasporist vision is a Rope or Scaffold for genuine Jewish flourishing. The engine's classification of this reading as a Piton reflects the atrophied state of the diasporist alternative within the broader contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities are beneficiaries as they align with the constraint's core premise of pluralistic survival. Jews coerced into Zionism and those endangered by Israeli actions are payers, bearing the costs of suppressed alternatives and conflated identity. Zionist institutions act as agenda-setters, actively enforcing the suppression of diasporist alternatives. Host nations are observers, as their policies can either enable or constrain diasporist flourishing.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist reading argues that the mandate for Jewish self-determination has been misdirected by Zionism. The original problem of Jewish security remains live, but the diasporist solution (pluralism, minority rights) has atrophied as a dominant political force, while the Zionist solution (territorial sovereignty) has become hegemonic. The Piton classification reflects this mandatrophy: the diasporist alternative persists, but its primary function as a widely adopted, institutionally supported path to Jewish flourishing has largely atrophied, maintained more by intellectual and activist performance than by widespread institutional adoption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diasporist_hegemony_contest,
    'Is the diasporist vision genuinely atrophied (a Piton), or is it a suppressed but actively resisted alternative (a Snare from the perspective of Zionist institutions)?',
    'Analysis of funding flows, institutional support, and media representation for diasporist vs. Zionist organizations. If diasporist organizations receive significant, independent funding and media presence, it suggests active suppression rather than mere atrophy.',
    'If actively suppressed, the constraint''s effective suppression is higher, and its classification might shift towards a Snare from the perspective of those who benefit from its suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diasporist_hegemony_contest, empirical, 'Ambiguity between atrophy and active suppression of the diasporist alternative.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a distinct reading of Jewish self-determination, or merely a critique of Zionism that does not offer a coherent alternative framework?',
    'Conceptual analysis of diasporist texts and movements to identify a positive, actionable program for Jewish flourishing distinct from anti-Zionism. If no such program exists, it is not a distinct reading but a negative critique.',
    'If not a distinct reading, the kernel ''Jewish self-determination'' might collapse to fewer, more dominant readings, and this constraint would be reclassified as a ''critique_of_zionism'' rather than a ''diasporist_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether diasporism constitutes a coherent alternative framework for Jewish self-determination.').

omega_variable(
    identity_lock_mechanism,
    'For ''Jews coerced into Zionism'', is the identity_locked exit option primarily due to professional identity (career path dependence), relational identity (self-concept constituted through the relationship), ideological identity (worldview that makes exit unthinkable), or institutional identity (the organization has ''become'' its function)?',
    'Qualitative sociological studies and interviews with individuals who have attempted to ''exit'' or dissent from Zionist frameworks, analyzing the specific mechanisms of social and professional pressure they encountered.',
    'Understanding the dominant identity-fusion mechanism would inform targeted interventions to create more genuine exit options, potentially lowering the effective suppression of alternative Jewish political expressions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Specific mechanism binding ''Jews coerced into Zionism'' to the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.5).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.55).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__diasporist_reading, theater_ratio, 1987, 0.6).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.65).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__diasporist_reading, theater_ratio, 2014, 0.68).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__diasporist_reading, base_extractiveness, 1987, 0.5).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__diasporist_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__diasporist_reading, suppression_requirement, 1987, 0.6).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__diasporist_reading, suppression_requirement, 2014, 0.64).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the 'Jewish self-determination' kernel. Each reading offers a distinct structural claim about how Jewish collective survival and flourishing are best secured, with different beneficiaries, victims, and classifications. This diasporist reading emphasizes pluralism and minority rights over territorial sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
