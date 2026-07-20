% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Jewish Ethnic-National State Framework (Post-Zionist Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint story models the post-Zionist reading of Israeli
 *   statehood: the Zionist project succeeded in establishing sovereign
 *   statehood, but the ongoing ethnic-national frameworkâencompassing the
 *   Law of Return, the Jewish National Fund land regime, military
 *   administration of occupied territories, and demographic engineeringânow
 *   operates as an extractive structure obstructing civic equality for
 *   Palestinian citizens and regional integration. The constraint is claimed
 *   as tangled_rope because the state apparatus continues to provide genuine
 *   coordination (security, governance, immigration absorption for refugees)
 *   while the ethnic privilege layer enforces asymmetric extraction. Jewish
 *   citizens are the beneficiaries; Israeli Palestinian citizens and occupied
 *   Palestinians are the victims. The reading treats the founding emergency
 *   as substantially resolved by the achievement of statehood and military
 *   dominance, rendering continued ethnic dominance a form of institutional
 *   inertia or extraction rather than existential necessity.
 *
 * KEY AGENTS:
 *   - israeli_state: Agenda-setter (institutional/constrained) â administers the ethnic-national legal framework through laws, land agencies, and military occupation
 *   - jewish_citizens: Primary beneficiary (organized/mobile) â receives preferential land access and immigration rights under the Law of Return
 *   - israeli_palestinian_citizens: Primary target (moderate/constrained) â second-class citizens inside the self-defined Jewish state
 *   - occupied_palestinians: Primary target (powerless/trapped) â under military occupation and separate legal regime without citizenship rights
 *   - palestinian_refugees: Excluded voice (powerless/trapped) â displaced and barred from return, absent from Israeli political discourse
 *   - international_human_rights_observers: Analytical observer (analytical/analytical) â documents asymmetry from outside and corroborates extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.72).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.7).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Ethnic-National State Framework (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '0e1bb28a-a83d-4838-9dbe-5f75e563c16c').
narrative_ontology:cs_kernel_codification('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', fixed_text).
narrative_ontology:cs_authority_grounding('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', lineage).
narrative_ontology:cs_interpretation_layer_present('0e1bb28a-a83d-4838-9dbe-5f75e563c16c').
narrative_ontology:cs_reading_relation('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', foundational, ethnic_statehood_obstructs_civic_equality).
narrative_ontology:cs_axiom_status(ethnic_statehood_obstructs_civic_equality, holdable).
narrative_ontology:cs_axiom_grounding('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', ethnic_statehood_obstructs_civic_equality, empirically_contingent).
narrative_ontology:cs_axiom('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', foundational, founding_mandate_obsolete).
narrative_ontology:cs_axiom_status(founding_mandate_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', founding_mandate_obsolete, empirically_contingent).
narrative_ontology:cs_reference_frame('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', zionist_foundational_emergency).
narrative_ontology:cs_drift_state('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', contemporary_post_hegemony, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e1bb28a-a83d-4838-9dbe-5f75e563c16c', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ethnic-national legal framework through the Law of Return, land agencies such as the Jewish National Fund and Israel Land Authority, and military government in occupied territories. Defines state land access, immigration rights, and residency through Jewish-national criteria, and maintains demographic majority objectives through planning, zoning, and separate legal regimes.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Receive preferential access to state land, immigration rights under the Law of Return, and dominant institutional representation. Their national self-determination is realized through state institutions that structurally prioritize Jewish collective interests over strict civic equality.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens, beneficiary,
    organized, biographical, mobile, national).

% Citizens of Israel who are excluded from the Law of Return, face restricted land purchase on most state land, receive unequal municipal budgeting, and live under institutional definitions that treat their presence as a demographic threat. Civic equality is blocked by the state's self-definition as Jewish.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens, payer,
    moderate, generational, constrained, national).

% Palestinians in the West Bank, Gaza, and East Jerusalem living under Israeli military control without citizenship rights in the sovereign state. Subject to separate military legal codes, movement restrictions, and territorial fragmentation that facilitates Jewish settlement expansion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinians, payer,
    powerless, immediate, trapped, regional).

% Descendants of Palestinians displaced in 1948 and 1967, barred from returning to their homes by the citizenship and entry framework. Structurally excluded from Israeli political discourse and from most bilateral negotiation frameworks.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees, excluded,
    powerless, generational, trapped, regional).

% United Nations bodies, non-governmental organizations, and academic researchers documenting asymmetric land access, occupation practices, and discrimination. Their findings are systematically sidelined in Israeli policymaking but corroborate the extractive structure for external audiences.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national self-determination, diaspora immigration absorption, and territorial control by structuring state membership, land tenure, and legal rights around ethnic-national criteria.
% TRANSFER_FUNCTION: Moves land access, immigration rights, residency status, and institutional power from Palestinian populationsâcitizens, occupied, and refugeesâto Jewish citizens and immigrants, underwritten by state violence and legal discrimination.
% ABSENT_VOICES: Palestinian refugees are excluded from Israeli civic discourse and from most negotiation frameworks; occupied Palestinians have no vote in the sovereign state that rules them; Israeli Palestinian political parties are periodically marginalized or excluded from governing coalitions.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished, the Law of Return would close, land access would equalize, military occupation would lose its demographic rationale, and regional integration would become structurally possibleâIsraeli and Palestinian political arrangements would reorganize around civic rather than ethnic boundaries.
% FOUNDING_PROBLEM: Jewish statelessness in Europe and the Middle East; persecution and lack of safe refuge; the need for collective self-determination and immigration sanctuary.
% FOUNDING_PROBLEM_CORROBORATION: Post-Zionist historians and sociologists attest that the existential threat to Jewish survival justifying the framework has been transformed by the achievement of statehood and military hegemony; Israeli Palestinian citizens and international historians corroborate that the founding emergency no longer describes the operative condition.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is moderate-high because the Law of Return, land access asymmetries, and military occupation transfer substantial resources and rights from Palestinians to Jewish beneficiaries on an ongoing basis. Suppression (0.70) is high because the framework requires active legal, military, and bureaucratic enforcementâincluding separate legal systems, movement restrictions, and demolition of unauthorized Palestinian construction. Theater ratio (0.40) reflects the growing gap between Israel's self-presentation as a liberal democracy and the ethnocratic reality of its institutions. Accessibility collapse (0.65) is substantial because alternatives such as full civic equality, one-state binationalism, or refugee return are treated as existential threats to the state's self-definition. Resistance (0.75) is high due to Palestinian civil society organizing, periodic uprisings, and sustained international human rights pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state institutions) experiences the constraint as necessary coordination for Jewish national survival and diaspora integration. The beneficiary seat (Jewish citizens) largely experiences it as benign background conditions of citizenship. The payer seats (Palestinian citizens and occupied populations) experience it as active extraction enforced by state violence and legal exclusion. The engine computes this divergence from the structural data; the post-Zionist reading does not treat these perspectives as equally valid but as asymmetrical positions within an extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens derive low directionality (beneficiary side) from the Law of Return and preferential land access. Israeli Palestinian citizens derive moderate-high directionality because they are structurally excluded from these benefits while subjected to the same state coercive apparatus. Occupied Palestinians derive the highest directionalityâthey bear the costs of territorial control and settlement expansion without citizenship rights or effective exit. State institutions sit at low-to-moderate directionality: they administer extraction but are identity-locked to the Zionist framework, constraining their own capacity for reform.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-Zionist reading resolves mandatrophy by declaring the founding problem of Jewish statelessness deadâstatehood has been achieved and the state is militarily dominant. The ethnic-national framework persists beyond its founding function, now obstructing rather than enabling survival. This produces the mismatch signature: founding_problem_status is dead while disappearance_verdict is world_rearranges. If the framework vanished, the world would rearrange toward civic equality and regional integration, confirming the mandate has outlived its problem and persists by inertia and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethnic_dominance_statehood_separability,
    'Is Jewish collective self-determination structurally inseparable from ethnically discriminatory state institutions (Law of Return, land regime, separate legal systems), or can the latter be dissolved while the former persists?',
    'Comparative analysis of consociational or civic-national models where diaspora return privileges have been phased out without state collapse; constitutional reform scenarios and natural experiments in equalizing land access.',
    'If inseparable, the ethnic framework is inherent to the state''s existence and reads closer to a Mountain for Jewish self-determination. If separable, the ethnic framework is a removable extractive layer and post-Zionist de-ethnicization is structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethnic_dominance_statehood_separability, conceptual, 'Whether Jewish self-determination requires ethnic state dominance or can persist under civic equality.').

omega_variable(
    founding_problem_current_status,
    'Has the founding problem of Jewish statelessness and persecution been resolved by the achievement of sovereign statehood and military capacity, or does ongoing regional hostility perpetuate an emergency condition justifying the framework?',
    'Historical analysis of threat levels relative to 1948; assessment of whether the state''s military hegemony and diplomatic integration render the emergency framing empirically obsolete.',
    'If the founding problem is dead, the framework''s persistence is mandatrophyâa dead mandate generating extraction. If live, part of the measured extraction is arguably defensible as ongoing self-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_current_status, empirical, 'Whether the founding emergency persists or is obsolete.').

omega_variable(
    kernel_reading_boundary,
    'This constraint is one reading of the contested kernel jewish_sovereignty_palestine. How do sibling readings reframe the beneficiary-victim structure and epsilon values?',
    'Cross-reading comparison: liberal_nationalist reading frames Jewish citizens as rightful beneficiaries with no victims; settler_colonial reading frames all Jewish Israelis as beneficiaries of colonial dispossession with higher epsilon; religious_zionist reading frames the territory as theological entitlement with no extractive dimension.',
    'Each reading produces a different constraint with different epsilon and directionality. This reading''s moderate-high extractiveness and declared victimhood of Palestinians is not shared by all sibling readings, and the kernel must be decomposed into separate epsilon-invariant stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Committer boundary marking this as one reading of a contested kernel with structurally distinct siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t0, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t12, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t25, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t38, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 38, 0.32).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t50, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t63, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 63, 0.39).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_tr_t76, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 76, 0.4).

% Extraction over time
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t0, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t12, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t25, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t38, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 38, 0.58).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t50, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t63, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 63, 0.68).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_be_t76, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 76, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t0, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t12, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t25, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t38, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 38, 0.65).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t50, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t63, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 63, 0.7).
narrative_ontology:measurement(jewish_sovereignty_palestine_postz_su_t76, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 76, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jewish_sovereignty_palestine. The kernel decomposes into five structurally distinct constraints because the epsilon values, beneficiary-victim structures, and classification types differ across readings. This reading (post_zionist) frames the constraint as a tangled rope with moderate-high extractiveness; sibling readings range from legitimacy-without-extraction (liberal nationalist) to pure extraction (settler colonial). Each reading is a separate constraint story linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
