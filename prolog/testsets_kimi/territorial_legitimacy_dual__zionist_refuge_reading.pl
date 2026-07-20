% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Legitimacy Framework
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story models the Zionist refuge reading of the
 *   territorial legitimacy kernel: the claim that Israel's sovereignty is
 *   legitimated by three pillarsâhistorical Jewish persecution culminating
 *   in the Holocaust, divine biblical promise of Eretz Yisrael, and
 *   acceptance of the UN Partition Plan (Resolution 181). From this reading,
 *   the 1948 founding is uncontested, post-1967 territorial control is
 *   negotiable but security-driven, Palestinian displacement is framed as a
 *   consequence of Arab rejection of partition, and ongoing territorial
 *   control is justified by existential security needs. The constraint
 *   functions as a political-ideological framework that simultaneously
 *   coordinates Jewish collective survival and extracts territorial and
 *   demographic costs from Palestinian populations. It is authored as one
 *   reading of a contested kernel; sibling readings (Palestinian autochthony,
 *   two-state coexistence) are structurally related but not described herein
 *   per Îµ-invariance discipline.
 *
 * KEY AGENTS:
 *   - israeli_state: Agenda-setter (institutional/generational/constrained) â administers the tripartite legitimacy framework and territorial control apparatus
 *   - jewish_israeli_citizens: Beneficiary (organized/biographical/constrained) â receive self-determination, security, and settlement benefits
 *   - palestinian_refugees: Primary target (powerless/generational/trapped) â bear extraction through exclusion from return and property restitution
 *   - palestinian_communities_in_occupied_territories: Secondary target (powerless/biographical/trapped) â bear ongoing territorial extraction and military administration
 *   - un_member_states: Observer (institutional/generational/analytical) â recognize, withhold, or conditionally recognize the legitimacy framework
 *   - international_human_rights_organizations: Observer (organized/generational/analytical) â contest security justifications and document extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.72).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.78).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Legitimacy Framework").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '48fd3f0e-8e8a-4538-b9cf-be67e4b665da').
narrative_ontology:cs_kernel_codification('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', fixed_text).
narrative_ontology:cs_authority_grounding('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', lineage).
narrative_ontology:cs_interpretation_layer_present('48fd3f0e-8e8a-4538-b9cf-be67e4b665da').
narrative_ontology:cs_reading_relation('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', foundational, theological_covenantal_land_claim).
narrative_ontology:cs_axiom_status(theological_covenantal_land_claim, holdable).
narrative_ontology:cs_axiom_grounding('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', theological_covenantal_land_claim, theological).
narrative_ontology:cs_axiom('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', foundational, persecution_mandates_sovereign_refuge).
narrative_ontology:cs_axiom_status(persecution_mandates_sovereign_refuge, holdable).
narrative_ontology:cs_axiom_grounding('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', persecution_mandates_sovereign_refuge, deontological).
narrative_ontology:cs_reference_frame('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', partition_mandate_refuge_state).
narrative_ontology:cs_drift_state('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', post_1967_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48fd3f0e-8e8a-4538-b9cf-be67e4b665da', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_communities_in_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers military control over pre-1967 territory and occupied territories, operates settlement authorities, and conducts diplomacy asserting legitimacy based on persecution refuge, divine promise, and UN partition acceptance. Military service is mandatory for most citizens. The state cannot abandon the territorial legitimacy framework without dissolving its constitutional self-understanding.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Receive national identity, security protection, and access to state resources including settlement subsidies. Subject to military conscription or reserve duty. Emigration is legally possible but socially and familially constrained; national identity is fused with the territorial state project.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_israeli_citizens, beneficiary,
    organized, biographical, constrained, national).

% Hold property claims and right-of-return demands unrecognized by the Israeli legal system. Dependent on UNRWA and host country toleration. Stateless or hold weak citizenship. Cannot return to pre-1948 localities because the legitimacy framework defines such return as an existential threat to the Jewish state.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under Israeli military administration in the West Bank or under blockade in Gaza, with separate legal systems from settlers. Subject to land confiscation, movement permits, and settlement expansion. The Palestinian Authority exercises limited self-governance under Israeli security control. Exit is blocked by Israeli control of borders and the absence of a sovereign Palestinian state.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_communities_in_occupied_territories, payer,
    powerless, biographical, trapped, local).

% Extend diplomatic recognition to Israel, vote on resolutions regarding occupation and refugee status, and occasionally apply sanctions or condition aid. Their engagement is analytical and diplomatic; they do not bear direct costs or extract direct benefits from the territorial arrangement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, un_member_states, observer,
    institutional, generational, analytical, global).

% Rejected the original partition plan and fought wars against Israel; now largely excluded from direct negotiation frameworks that determine Palestinian fate. Some states have normalized relations with Israel while others maintain hostility, but none set the agenda for the legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_league_states, excluded,
    organized, generational, constrained, regional).

% Document and publish reports on violations of international humanitarian law in occupied territories. Contest the proportionality of security justifications. Operate analytically without direct stake in territorial outcomes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes sovereign statehood and collective self-determination for a historically persecuted population lacking effective international protection, providing military defense, institutional governance, and demographic refuge.
% TRANSFER_FUNCTION: Moves territorial control, settlement rights, state resources, and demographic predominance from Palestinian refugees and occupied communities to the Israeli state and Jewish Israeli citizenry, justified by the tripartite legitimacy claim.
% ABSENT_VOICES: Palestinian refugees and their descendants who reject the partition framework's legitimacy are structurally absent from Israeli constitutional discourse; anti-Zionist and non-Zionist Jewish voices are marginalized within Israeli political institutions; Palestinian citizens of Israel with competing national narratives are politically under-represented.
% DISAPPEARANCE_RATIONALE: If this legitimacy framework disappeared, the territorial and demographic arrangements it justifiesâsettlement enterprise, military occupation, refugee exclusionâwould lose their primary ideological foundation. Israeli constitutional identity would face existential crisis, and Palestinian territorial and return claims would resurface with fundamentally altered legitimacy conditions.
% FOUNDING_PROBLEM: The Jewish people faced statelessness, systemic persecution culminating in genocide, and exclusion or conditional tolerance in existing nations; the problem was the absence of a sovereign territorial refuge capable of guaranteeing collective physical survival and political self-determination.
% FOUNDING_PROBLEM_CORROBORATION: UN General Assembly Resolution 181 (1947) recognized the problem but is the enacting text, not an external corroborator. Palestinian historians and anti-colonial scholars contest the framing from outside the beneficiary set. No uncontested external corroboration exists; the founding narrative is primarily self-asserted by the benefiting parties.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the legitimacy framework underwrites territorial expansion, refugee exclusion, and differential citizenship that structurally benefit one population at the expense of another. Suppression (0.78) is high because the framework's persistence depends on active military and legal suppression of Palestinian return claims and territorial sovereignty. Theater_ratio (0.45) reflects significant performative maintenance: security justifications are partly genuine but increasingly serve settlement expansion and demographic control. Accessibility_collapse (0.68) captures how, once the tripartite framework is accepted, Palestinian return or single-state equality becomes nearly unthinkable within the framework. Resistance (0.58) reflects persistent Palestinian opposition and growing international institutional dissent. The temporal series shows extraction and suppression rising sharply after 1967, with theater accumulating as security arguments became routinized beyond original partition scope.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and Jewish Israeli citizen seat experience this constraint as protective coordinationâa necessary refuge from persecution and existential threatâcomputing toward the coordination side of the spectrum. The Palestinian refugee and occupied territory seats experience the same structure as territorial extraction and demographic engineering, computing toward high effective extraction. The UN member state seat sits analytically, with lower directionality, often oscillating between recognition and condemnation. The engine computes this divergence from structural data rather than reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (israeli_state, jewish_israeli_citizens) derive low directionality: the constraint subsidizes their collective self-determination and security. Victims (palestinian_refugees, palestinian_communities_in_occupied_territories) derive high directionality: the constraint extracts from them through exclusion, dispossession, and military administration. The Israeli state's exit is constrained not by external barriers alone but by constitutional and identity fusion with the territorial claim; Palestinian exits are trapped by statelessness and occupation. UN observers are analytical with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the Tangled Rope classification, this constraint could be misread as pure Rope (legitimate coordination for refugees) or pure Snare (colonial extraction). The Tangled Rope classification captures that the coordination functionâorganizing statehood for a persecuted peopleâis genuine and historically real, while the asymmetric extractionâongoing occupation, settlement, and refugee exclusionâis equally real and co-enforced through the same legitimacy framework. The claim/metric independence is maintained: the constraint is claimed as tangled_rope while metrics honestly describe high extraction and suppression, allowing the engine to measure divergence if any seat computes differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the zionist_refuge_reading one coexisting frame among others, or does its core premise foreclose Palestinian autochthony claims within any single legitimacy framework?',
    'Comparative structural analysis of whether Jewish sovereignty claims based on divine promise and persecution logically preclude indigenous Palestinian territorial rights, or whether both can be held simultaneously (e.g., binational frameworks).',
    'If foreclosing, the constraint computes as more extractive for Palestinian seats and the sibling relation upgrades; if coexisting, the political space for two-state or confederal solutions remains structurally open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading forecloses or coexists with sibling readings').

omega_variable(
    security_narrative_veracity,
    'Do post-1967 security concerns justify the full scope of territorial control and settlement, or do they function as instrumental cover for demographic and territorial expansion?',
    'Empirical assessment of settlement patterns relative to security threat maps; correlation between territorial withdrawal opportunities and security justification intensity.',
    'If security claims are proportional to threat, the coordination component is larger and extraction lower; if decoupled, theater_ratio rises and the constraint approaches snare classification for occupied territory seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_narrative_veracity, empirical, 'Whether security justifications are genuine or performative cover').

omega_variable(
    partition_boundary_bindingness,
    'Does UN partition acceptance bind this reading to 1948 boundaries, or does it legitimize any territory subsequently controlled?',
    'Legal-historical analysis of Zionist discourse and state practice: whether Resolution 181 is cited as a territorial ceiling or as a floor/starting point.',
    'If partition is a ceiling, post-1967 control is pure extraction without coordination cover; if a floor, the reading can absorb territorial expansion into its legitimacy structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_boundary_bindingness, conceptual, 'Whether UN partition acceptance functions as boundary limit or launch point').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(terr_tr_t19, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 19, 0.3).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(terr_tr_t76, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 76, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(terr_be_t19, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 19, 0.58).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 45, 0.63).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(terr_be_t76, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 76, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(terr_su_t19, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 19, 0.6).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(terr_su_t76, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 76, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_legitimacy_dual kernel. Its sibling readings instantiate structurally distinct constraints with different beneficiary/victim structures and epsilon values. Decomposition follows the epsilon-invariance principle: the natural-language label 'Israeli-Palestinian legitimacy' conflates multiple structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
