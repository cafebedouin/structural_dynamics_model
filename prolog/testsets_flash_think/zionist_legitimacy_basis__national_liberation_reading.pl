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
 *   This constraint models Zionism as a national liberation movement for a
 *   persecuted indigenous people returning to their ancestral homeland. From
 *   this perspective, the movement's core function is to secure Jewish
 *   self-determination and provide a safe haven. The metrics reflect the
 *   structural outcomes of this process, including significant extraction
 *   from and suppression of Arab Palestinians, which this reading justifies
 *   as necessary for achieving Jewish rights and security. The claimed type
 *   'tangled_rope' acknowledges the dual function of coordination for Jewish
 *   liberation and asymmetric extraction from other populations, requiring
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.65).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.85).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as Jewish National Liberation").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'cb0d1d26-b585-44a5-bbad-955d6197f522').
narrative_ontology:cs_kernel_codification('cb0d1d26-b585-44a5-bbad-955d6197f522', formalized).
narrative_ontology:cs_authority_grounding('cb0d1d26-b585-44a5-bbad-955d6197f522', lineage).
narrative_ontology:cs_interpretation_layer_present('cb0d1d26-b585-44a5-bbad-955d6197f522').
narrative_ontology:cs_reading_relation('cb0d1d26-b585-44a5-bbad-955d6197f522', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('cb0d1d26-b585-44a5-bbad-955d6197f522', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('cb0d1d26-b585-44a5-bbad-955d6197f522', foundational, jewish_people_are_a_nation).
narrative_ontology:cs_axiom_status(jewish_people_are_a_nation, holdable).
narrative_ontology:cs_axiom_grounding('cb0d1d26-b585-44a5-bbad-955d6197f522', jewish_people_are_a_nation, deontological).
narrative_ontology:cs_axiom('cb0d1d26-b585-44a5-bbad-955d6197f522', foundational, historical_connection_to_land_of_israel).
narrative_ontology:cs_axiom_status(historical_connection_to_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('cb0d1d26-b585-44a5-bbad-955d6197f522', historical_connection_to_land_of_israel, conventional).
narrative_ontology:cs_reference_frame('cb0d1d26-b585-44a5-bbad-955d6197f522', jewish_self_determination_in_homeland).
narrative_ontology:cs_drift_state('cb0d1d26-b585-44a5-bbad-955d6197f522', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb0d1d26-b585-44a5-bbad-955d6197f522', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically subjected to persecution and statelessness, this group views Zionism as the fulfillment of their right to self-determination and a secure homeland. They benefit from the existence of Israel as a refuge and a center of Jewish life, even if they do not reside there.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora, beneficiary,
    organized, generational, constrained, global).

% As citizens of the State of Israel, they are the direct beneficiaries of the national liberation project, enjoying sovereignty, security, and cultural self-expression. Their identity is deeply intertwined with the state's existence, making exit unthinkable for many.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_citizens, beneficiary,
    institutional, biographical, identity_locked, national).

% From this reading's perspective, their opposition to Zionism is delegitimized as a denial of Jewish rights. However, they structurally bear the costs of displacement, loss of land, and statelessness resulting from the establishment and expansion of Israel. Their options are limited by military and political realities.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_palestinians, payer,
    powerless, generational, trapped, local).

% The political and organizational leadership that articulated the vision of Zionism, mobilized support, and directed the efforts to establish and maintain the State of Israel. They set the policies and narratives that define the movement's goals and justifications.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_movement_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Comprising various states, international organizations, and non-governmental bodies, this group observes, debates, and sometimes intervenes in the Israeli-Palestinian conflict. Their positions range from supporting Israel's right to exist to condemning its actions, but they do not directly control the constraint's operation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_community, observer,
    institutional, generational, analytical, global).

% Neighboring states that have historically opposed Zionism and borne significant costs through wars, refugee crises, and regional instability. Their ability to influence the constraint is constrained by geopolitical realities and their own internal dynamics.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states, payer,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, israeli_citizens).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the return, settlement, and self-determination of the Jewish people in their ancestral homeland, providing a secure refuge from historical persecution and a center for national and cultural revival.
% TRANSFER_FUNCTION: Transfers sovereignty, land, and political control from existing (Ottoman/British Mandate/Palestinian Arab) authorities to the Jewish people, establishing the State of Israel.
% ABSENT_VOICES: Palestinian Arabs (historically excluded from decisions regarding their land and future), and anti-Zionist Jewish groups (who reject the ethno-nationalist framing of Jewish identity and statehood).
% DISAPPEARANCE_RATIONALE: If Zionism as a national liberation movement and its resulting state vanished overnight, the State of Israel would cease to exist. This would lead to a complete reorganization of political, social, and demographic structures in the region, and a new, profound crisis for the Jewish people globally, fundamentally altering the geopolitical landscape.
% FOUNDING_PROBLEM: The historical persecution, antisemitism, and statelessness of the Jewish people, culminating in the Holocaust, and the desire for self-determination and security in their ancestral land.
% FOUNDING_PROBLEM_CORROBORATION: Jewish historical narratives, international resolutions (e.g., UN Partition Plan), testimonies of Holocaust survivors, and ongoing antisemitism globally corroborate the founding problem. From this reading's perspective, the need for Jewish self-determination and security remains live, justifying the constraint's continued existence.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) due to the displacement and dispossession of Arab Palestinians, even if this reading frames it as a justified consequence of national liberation. Suppression is very high (0.85) as the establishment and maintenance of the state required active suppression of Arab opposition and alternative claims to the land. Theater ratio is low (0.15) because the core claims of national liberation and historical connection are genuinely held by proponents. Accessibility collapse is high (0.8) for both Jewish statelessness (historically) and Palestinian self-determination (post-1948). Resistance is high (0.8) reflecting ongoing conflict. The temporal measurements show a rise in extractiveness and suppression as the movement gained power and established the state, reflecting the increasing costs borne by those resisting the project.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish beneficiaries, the constraint is a vital coordination mechanism for survival and self-determination. From the perspective of Arab Palestinians, it is a mechanism of dispossession and control. The engine will compute this divergence from the structural data, showing different classifications for different seats, despite the 'national liberation' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora and Israeli citizens are structural beneficiaries (d near 0.0) as they gain a homeland and self-determination. Arab Palestinians and Arab states are targets/payers (d near 1.0) as they bear the costs of displacement, loss of land, and conflict. Zionist movement leadership is the agenda-setter, actively shaping and enforcing the constraint. The international community acts as an observer, with varying degrees of influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_liberation_vs_settler_colonialism,
    'Is Zionism fundamentally a national liberation movement for an indigenous people, or is it a settler-colonial project?',
    'Comparative historical analysis of other national liberation and settler-colonial movements, focusing on patterns of indigenous displacement, land acquisition, and the relationship between the arriving population and existing inhabitants.',
    'If reclassified as settler-colonial, the constraint''s extractiveness and suppression would be seen as inherent to its nature, not a justified consequence, leading to a higher effective extraction for victims and a reclassification towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_liberation_vs_settler_colonialism, conceptual, 'Ambiguity regarding the fundamental nature of Zionism.').

omega_variable(
    justification_of_displacement,
    'To what extent does historical persecution and ancestral connection justify the displacement of existing populations from their land?',
    'Ethical and legal analysis of competing claims to self-determination and land rights, potentially involving international legal precedents on indigenous rights and post-colonial justice.',
    'If the justification for displacement is found insufficient, the extractiveness from Arab Palestinians would be re-evaluated as unjust and unmitigated, increasing their effective extraction and potentially shifting the constraint towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(justification_of_displacement, preference, 'Ethical justification for the costs imposed on Arab Palestinians.').

omega_variable(
    arab_opposition_legitimacy,
    'Is Arab opposition to Zionism primarily a denial of Jewish rights, or a legitimate expression of Palestinian self-determination and resistance to displacement?',
    'Analysis of historical documents, political statements, and social movements from both sides, seeking to understand the motivations and goals of Arab opposition independently of Zionist narratives.',
    'If Arab opposition is recognized as legitimate self-determination, the suppression metric would be seen as directly targeting a valid claim, increasing its perceived severity and the effective extraction for Arab Palestinians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_opposition_legitimacy, conceptual, 'Legitimacy of Arab opposition to the Zionist project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1900, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1900, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.14).
narrative_ontology:measurement(zion_tr_t2023, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(zion_be_t1900, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.3).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(zion_be_t2023, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1900, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.45).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.83).
narrative_ontology:measurement(zion_su_t2023, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_right_of_return_denial).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israeli_settlement_expansion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
