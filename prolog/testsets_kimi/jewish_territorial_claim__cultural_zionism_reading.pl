% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Territorial Claim (Palestine)
 *   domain: political_history/settler_colonialism
 *
 * SUMMARY:
 *   This constraint instantiates the cultural_zionism_reading of the
 *   jewish_territorial_claim kernel, associated with Ahad Ha'am and the
 *   movement for a Jewish spiritual and cultural center in Palestine. The
 *   reading explicitly disavows the necessity of political sovereignty or a
 *   Jewish demographic majority, emphasizing instead quality of settlement,
 *   Hebrew cultural revival, and the potential for binational coexistence.
 *   Within the broader Zionist movement, this reading functioned as a
 *   coordination mechanism for Jewish cultural renewal while simultaneously
 *   operating as a vehicle for territorial acquisition and demographic
 *   transformation under colonial conditions.
 *
 * KEY AGENTS:
 *   - Cultural Zionist settlers (beneficiary/identity-locked): Jewish immigrants who benefit from the cultural-center framing and the land and institutions it secures, fused to the Zionist project.
 *   - Zionist cultural institutions (agenda_setter/constrained): Administrative bodies that set settlement priorities and enforce the quality-over-quantity principle.
 *   - Palestinian Arab communities (payer/trapped): Indigenous population bearing the costs of land transfer and demographic displacement.
 *   - British Mandatory Authority (observer/analytical): Colonial power whose legal framework enables and observes the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.55).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.45).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Territorial Claim (Palestine)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '71015cc8-eb2d-4b61-a30a-79ba32bd3e7d').
narrative_ontology:cs_kernel_codification('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', fixed_text).
narrative_ontology:cs_authority_grounding('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', lineage).
narrative_ontology:cs_interpretation_layer_present('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d').
narrative_ontology:cs_reading_relation('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', foundational, cultural_center_sufficiency).
narrative_ontology:cs_axiom_status(cultural_center_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', cultural_center_sufficiency, deontological).
narrative_ontology:cs_axiom('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', foundational, binational_accommodation_viability).
narrative_ontology:cs_axiom_status(binational_accommodation_viability, holdable).
narrative_ontology:cs_axiom_grounding('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', binational_accommodation_viability, empirically_contingent).
narrative_ontology:cs_reference_frame('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', cultural_renaissance_territorial_center).
narrative_ontology:cs_drift_state('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', statehood_consolidation_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71015cc8-eb2d-4b61-a30a-79ba32bd3e7d', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, cultural_zionist_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, zionist_cultural_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish immigrants and settlers drawn to Palestine by the vision of a Hebrew cultural and spiritual renaissance. They establish agricultural colonies, schools, and cultural institutions oriented toward national revival rather than statehood. Their identity is fused with the Zionist project; leaving Palestine means abandoning the core self-concept of national rebirth. They benefit from land acquisition and institutional support while bearing the labor of settlement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, cultural_zionist_settlers, beneficiary,
    moderate, biographical, identity_locked, national).

% Cultural departments of the World Zionist Organization, Hebrew language committees, and educational trusts that administer settlement priorities, fund cultural projects, and set the ideological tone of the Yishuv. They enforce the quality-over-quantity principle in immigration and land purchase. Their continuation depends on Diaspora Jewish philanthropy and congress mandates; they cannot easily exit the Zionist institutional framework.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, zionist_cultural_institutions, agenda_setter,
    organized, generational, constrained, national).

% Indigenous Palestinian Arab peasants, landowners, and urban communities who bear the costs of Jewish land purchases, immigration-driven demographic change, and the gradual transfer of territorial and economic resources to Zionist institutions. Under British Mandatory rule, their political claims are suppressed by colonial legal frameworks that facilitate Zionist settlement. Exit means displacement or emigration from their homeland.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_communities, payer,
    powerless, generational, trapped, national).

% The colonial administration exercising the League of Nations Mandate for Palestine. It observes and occasionally regulates the balance between Zionist immigration and Arab unrest, while its legal and military infrastructure indirectly enforces the territorial claims of the Jewish national home. It could alter the constraint by restricting land transfers or immigration, but generally maintains the framework established by the Balfour Declaration.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, british_mandatory_authority, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national cultural revival by establishing a territorial center in Palestine where Hebrew language, literature, and spiritual life can be renewed without requiring the immediate apparatus of sovereign statehood or a Jewish demographic majority.
% TRANSFER_FUNCTION: Moves land, institutional resources, and demographic presence from Palestinian Arab communities and Ottoman or British territorial administration to Jewish cultural settlers and Zionist cultural institutions.
% ABSENT_VOICES: Palestinian Arab peasants displaced by land purchases, and Jewish binationalist intellectuals arguing for equal political partnership, were structurally marginalized in Zionist congress deliberations that privileged the cultural-center framing over Arab political claims.
% DISAPPEARANCE_RATIONALE: Without the cultural-center framing and the institutional network it sustained, the Yishuv would have lacked the legitimizing narrative of spiritual return and the Hebrew cultural infrastructure that differentiated Zionist settlement from pure colonialism. The Zionist project would likely have shifted earlier toward either explicit political statism (abandoning cultural gradualism) or dissolution.
% FOUNDING_PROBLEM: The perceived crisis of Jewish cultural and spiritual dissolution in Diaspora Europe, and the failure of assimilation, generating a need for renewed Hebrew cultural life in a territorial center.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography from within the movement attests the cultural crisis. Critical historians outside the Zionist beneficiary set, including post-Zionist and Palestinian scholars, contest whether Palestine was the necessary solution or whether the problem was primarily antisemitism requiring political rather than cultural remedies.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the cultural framing deliberately slowed the pace of territorial acquisition and avoided explicit state coercion, yet the structural dynamic remained settler-colonial: land purchases, institutional segregation, and demographic engineering displaced indigenous presence. Suppression (0.45) reflects the colonial legal order and land regimes that privileged Jewish settlement without the direct military coercion of maximalist Zionism. Theater ratio is low (0.25) because the cultural motivation was largely sincere, though it provided legitimizing cover for territorial expansion. Resistance (0.55) captures Palestinian Arab opposition and the eventual marginalization of the cultural reading by political Zionism. Metrics are authored independently of the claimed type; the engine will compute per-seat divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the settler beneficiary seat, the constraint is experienced as cultural salvation and national coordination. From the indigenous payer seat, the same structure is experienced as gradual territorial erosion and structural displacement under colonial protection. The British observer seat sees a manageable imperial arrangement. These divergences are structurally derived from the beneficiary-victim asymmetry and the differential exit options (identity-locked vs. trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Cultural Zionist settlers are structural beneficiaries (low d): the constraint subsidizes their cultural project and land access. Zionist cultural institutions are near-beneficiary but agenda-setting (low-moderate d). Palestinian Arab communities are full targets (high d): the constraint extracts land and political space from them. The British authority sits near symmetric (moderate d): it neither primarily gains nor loses from the cultural framing itself, though it administratively sustains it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve Jewish cultural dissolution in Europe. By the mid-twentieth century, the movement had shifted decisively toward sovereign statehood, suggesting the founding problem was either solved by other means (the Holocaust and Israeli statehood transformed the Diaspora condition) or the mandate atrophied as political Zionism superseded the cultural center model. The persistence of the claim in later discourse is partly theatrical, but the authored metrics treat the historical interval honestly rather than projecting terminal statehood back onto the cultural period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_political_extraction,
    'Does the cultural-center framing structurally reduce extraction compared to political-sovereignty framings, or does it merely delay and diffuse the same extractive outcome?',
    'Comparative historical analysis of land transfer rates, institutional segregation, and demographic displacement under cultural Zionist settlement (pre-1948) versus political Zionist statehood (post-1948).',
    'If extraction rates are comparable, the cultural reading is a tangled rope whose coordination story masks equivalent extraction; if significantly lower, the reading may compute closer to rope for beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_political_extraction, empirical, 'Whether cultural framing alters the extractive structural outcome.').

omega_variable(
    binational_potential,
    'Was the binational framework a genuine structural possibility of this reading, or was it always subordinate to the ultimate telos of Jewish demographic and institutional dominance?',
    'Archival analysis of cultural Zionist congress deliberations and land-purchase patterns to determine whether binationalism was a committed constraint or a provisional tactical framing.',
    'If binationalism was a committed axiom, the constraint''s suppression metric may be overstated; if tactical, the reading converges toward its more extractive siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_potential, conceptual, 'Whether binationalism was foundational or tactical within this reading.').

omega_variable(
    kernel_reading_boundary,
    'How does the epsilon value of this reading relate to its sibling readings within the same kernel?',
    'Corpus comparison across the constraint family (jewish_territorial_claim) to measure structural divergence in beneficiary-victim asymmetry and enforcement requirements.',
    'Validates the epsilon-invariance decomposition: if sibling readings show similar epsilon, the kernel should not have been split; if divergent, the decomposition is structurally sound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between this kernel reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_cz_tr_t0, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jtc_cz_tr_t10, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(jtc_cz_tr_t20, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jtc_cz_tr_t30, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(jtc_cz_tr_t40, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(jtc_cz_tr_t50, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(jtc_cz_be_t0, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jtc_cz_be_t10, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(jtc_cz_be_t20, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(jtc_cz_be_t30, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(jtc_cz_be_t40, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(jtc_cz_be_t50, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jtc_cz_su_t0, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jtc_cz_su_t10, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(jtc_cz_su_t20, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(jtc_cz_su_t30, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(jtc_cz_su_t40, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(jtc_cz_su_t50, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the cultural_zionism_reading of the jewish_territorial_claim kernel. It decomposes from the broader Zionism natural-language label alongside political, labor, and revisionist readings. Each reading carries distinct epsilon values, beneficiary structures, and coordination functions. Cultural Zionism is typically less extractive and less suppressive than revisionist Zionism, but more diffuse in its territorial claims than political Zionism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
