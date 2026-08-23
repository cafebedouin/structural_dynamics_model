% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Zionist Refuge Legitimacy (Territorial Sovereignty)
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the zionist_refuge_reading of the
 *   territorial_legitimacy_dual kernel. The reading grounds Israeli
 *   territorial sovereignty in three legitimizing pillars: the historical
 *   persecution of Jews culminating in the Holocaust, the Biblical divine
 *   promise of the Land of Israel, and the acceptance of the 1947 UN
 *   Partition Plan. From this reading, 1948 independence is structurally
 *   uncontested, 1967 boundaries are negotiable, Palestinian displacement is
 *   framed as the consequence of Arab rejection rather than Zionist
 *   expulsion, and ongoing security concerns justify territorial control
 *   beyond the Green Line. Sibling readings (palestinian_autochthony_reading,
 *   two_state_coexistence_reading) are treated as separate constraints linked
 *   through the kernel family network.
 *
 * KEY AGENTS:
 *   - israeli_government: Primary agenda-setter (institutional/arbitrage) â administers sovereignty and security apparatus, derives legitimacy from the tripartite founding narrative
 *   - jewish_communities: Primary beneficiary (organized/mobile) â receives refuge, self-determination, and Law of Return rights subsidized by the territorial arrangement
 *   - palestinian_refugees: Primary target (powerless/trapped) â bears displacement, exile, and denial-of-return costs
 *   - palestinian_citizens_of_israel: Secondary target (moderate/constrained) â bears asymmetric framework costs within citizenship, including land and institutional constraints
 *   - regional_arab_states: Excluded disputant (powerful/constrained) â framed as causally responsible for displacement, their legitimacy narratives are structurally excluded
 *   - international_community: Analytical observer (institutional/analytical) â adjudicates recognition and international law, rarely enforces alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.82).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Legitimacy (Territorial Sovereignty)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '480c4ca3-437d-491e-aef0-dfefad911172').
narrative_ontology:cs_kernel_codification('480c4ca3-437d-491e-aef0-dfefad911172', fixed_text).
narrative_ontology:cs_authority_grounding('480c4ca3-437d-491e-aef0-dfefad911172', lineage).
narrative_ontology:cs_interpretation_layer_present('480c4ca3-437d-491e-aef0-dfefad911172').
narrative_ontology:cs_reading_relation('480c4ca3-437d-491e-aef0-dfefad911172', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('480c4ca3-437d-491e-aef0-dfefad911172', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('480c4ca3-437d-491e-aef0-dfefad911172', foundational, divine_promise_land_of_israel).
narrative_ontology:cs_axiom_status(divine_promise_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('480c4ca3-437d-491e-aef0-dfefad911172', divine_promise_land_of_israel, theological).
narrative_ontology:cs_axiom('480c4ca3-437d-491e-aef0-dfefad911172', foundational, persecution_mandates_sovereign_refuge).
narrative_ontology:cs_axiom_status(persecution_mandates_sovereign_refuge, holdable).
narrative_ontology:cs_axiom_grounding('480c4ca3-437d-491e-aef0-dfefad911172', persecution_mandates_sovereign_refuge, instrumental).
narrative_ontology:cs_reference_frame('480c4ca3-437d-491e-aef0-dfefad911172', zionist_sovereignty_framework).
narrative_ontology:cs_drift_state('480c4ca3-437d-491e-aef0-dfefad911172', post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('480c4ca3-437d-491e-aef0-dfefad911172', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_government).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_communities).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers territorial sovereignty and security apparatus, deriving domestic and international legitimacy from the founding narrative of historical persecution, divine promise, and UN partition acceptance. Sets policies on borders, settlements, refugee return, and the constitutional definition of the state as Jewish and democratic.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from a sovereign state offering refuge, cultural self-determination, and political expression. Includes Israeli citizens and diaspora communities who hold rights of entry and settlement under the Law of Return, subsidized by the territorial arrangement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_communities, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of displacement and the denial of return, residing in refugee camps or external exile. Their ancestral land claims and collective rights are structurally overridden by the legitimacy framework that prioritizes Jewish refuge and sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Hold formal citizenship but bear asymmetric costs of the Jewish-state framework, including land expropriation, institutional discrimination, and qualified equality. Their presence is tolerated within the state while its self-definition extracts full civic parity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Rejected the 1947 partition plan and are framed within this reading as responsible for Palestinian displacement. Their alternative historical narratives and legitimacy claims are structurally excluded from the framework's legitimizing discourse.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, regional_arab_states, excluded,
    powerful, generational, constrained, regional).

% Recognizes Israel under the UN partition framework while periodically contesting occupation and settlement expansion. Provides the diplomatic and legal arena where legitimacy claims are adjudicated, but rarely enforces alternatives.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international recognition and state institutions to provide sovereign territorial expression and collective security for a historically stateless and persecuted population.
% TRANSFER_FUNCTION: Transfers territorial control and political sovereignty from the indigenous Palestinian population to the Jewish refugee and settler population, justified by UN partition authorization and historical necessity.
% ABSENT_VOICES: Palestinian refugees asserting right of return are excluded from the legitimacy narrative; regional Arab states are present in discourse only as rejectionists whose opposition justifies displacement rather than as legitimate disputants.
% DISAPPEARANCE_RATIONALE: If this legitimacy framework vanished, Israeli territorial sovereignty would lose its primary historical and legal justification, forcing renegotiation of borders, refugee status, and the constitutional character of the state. The regional order would fundamentally rearrange.
% FOUNDING_PROBLEM: Jewish statelessness and persecution in Europe and the Middle East culminating in the Holocaust, creating an urgent need for internationally recognized sovereign refuge.
% FOUNDING_PROBLEM_CORROBORATION: Jewish diaspora organizations and Israeli state institutions attest to the historical persecution and the need for refuge. Palestinian historians, critical international scholars, and Arab state representatives attest that the partition itself was an imposed colonial arrangement and that the founding problem was weaponized to justify dispossession. Corroboration is contested and split across beneficiary and payer seats, with no outside-neutral consensus.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.85) is high because the territorial arrangement systematically transfers land, political autonomy, and return rights from the Palestinian population to the Jewish population. Suppression (0.82) is high because the arrangement's persistence depends on actively suppressing Palestinian return claims, alternative sovereignty frameworks, and internal dissent through military, legal, and diplomatic means. Theater ratio (0.55) is elevated because over the 75-year interval, the original refuge-and-security justification has accrued performative layersârhetorical invocation of existential threat and settlement expansion framed as securityâthat exceed functional security requirements. Accessibility collapse (0.80) is high because once the legitimacy framework is accepted, alternatives (right of return, single democratic state, pre-1948 status quo) become practically unthinkable within the international diplomatic consensus. Resistance (0.85) is high because the constraint meets sustained violent and nonviolent resistance from Palestinian movements and regional actors. The temporal series show extraction and suppression accumulating as the occupation deepens and the initial partition boundaries are exceeded.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as legitimate self-determination and necessary security, computing toward coordination. The payer seats experience it as dispossession and exclusion, computing toward extraction. The engine resolves this divergence through directionality: Israeli government and Jewish communities have low directionality (subsidized by the constraint), while Palestinian refugees and citizens have high directionality (targets of extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli government sits near the beneficiary end because it collects sovereignty and security capacity from the arrangement. Jewish communities sit near the beneficiary end because the constraint subsidizes their collective self-determination and refuge rights. Palestinian refugees sit at the full-target end because the constraint extracts their territorial presence and return rights. Palestinian citizens of Israel sit at mid-high target because the constraint extracts full equality while offering nominal citizenship. The high spatial scope (national/regional) amplifies effective extraction for the powerless refugee population, whose exit is trapped and whose scope is regional.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy prevents mislabeling this as pure extraction (snare) by requiring acknowledgment of the founding problem: Jewish statelessness and persecution was a real coordination failure that the arrangement genuinely addressed. However, the founding problem status is contested, and the temporal measurements show extraction accumulating beyond the original partition boundaries, indicating that the coordination function has been progressively overshadowed by territorial expansion. The mandatrophy is unresolvedâthe founding problem has morphed into an ongoing occupation and displacement problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_promise_secular_status,
    'Is the divine promise component a theological claim with independent normative force, or a cultural-historical narrative deployed for secular political legitimation?',
    'Discourse analysis of policy justifications and judicial reasoning to measure theological versus security-civic invocation frequencies.',
    'If primarily theological, the constraint''s grounding is non-falsifiable and deontological; if primarily instrumental, the legitimacy becomes contingent on functional outcomes and vulnerable to empirical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_promise_secular_status, conceptual, 'Theological versus instrumental status of divine promise in legitimacy claims').

omega_variable(
    partition_authority_legitimacy,
    'Did the UN partition plan represent a legitimate exercise of international authority under the colonial mandate system, or was it an imposed arrangement lacking indigenous consent?',
    'Historical analysis of the UN Special Committee on Palestine deliberations and the extent of Palestinian representation in 1947.',
    'If the partition lacked legitimate authority, the foundational legal premise of this reading collapses, shifting classification toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_authority_legitimacy, empirical, 'UN partition plan authority and consent legitimacy').

omega_variable(
    displacement_causation,
    'Was Palestinian displacement in 1948 primarily caused by Arab state rejection of partition, or by pre-planned Zionist military expulsion?',
    'Archival and oral history comparison of Arab rejection effects versus Plan Dalet and Haganah operational records.',
    'If rejection was not the primary cause, the reading''s moral distribution of responsibility collapses, invalidating its key legitimizing narrative and likely reclassifying the constraint as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causation, empirical, 'Causal mechanism of 1948 Palestinian displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_refuge_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zionist_refuge_tr_t15, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(zionist_refuge_tr_t30, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(zionist_refuge_tr_t45, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(zionist_refuge_tr_t60, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(zionist_refuge_tr_t75, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(zionist_refuge_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(zionist_refuge_be_t15, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(zionist_refuge_be_t30, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(zionist_refuge_be_t45, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 45, 0.78).
narrative_ontology:measurement(zionist_refuge_be_t60, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 60, 0.82).
narrative_ontology:measurement(zionist_refuge_be_t75, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 75, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(zionist_refuge_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(zionist_refuge_su_t15, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(zionist_refuge_su_t30, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(zionist_refuge_su_t45, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 45, 0.76).
narrative_ontology:measurement(zionist_refuge_su_t60, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(zionist_refuge_su_t75, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 75, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the zionist_refuge_reading of the territorial_legitimacy_dual kernel, which decomposes the colloquial label of Israeli-Palestinian territorial legitimacy into three structurally distinct readings with different epsilon values, beneficiary structures, and empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
