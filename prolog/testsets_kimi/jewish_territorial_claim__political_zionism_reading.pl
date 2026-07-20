% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionist Territorial Sovereignty Claim
 *   domain: political history / settler colonialism / nationalism studies
 *
 * SUMMARY:
 *   The political Zionist reading of the Jewish territorial claim treats
 *   sovereign statehood with a Jewish demographic majority as the necessary
 *   and sufficient solution to antisemitism and the Jewish Question. Emerging
 *   from Theodor Herzl's programmatic writings and the Basel Program (1897),
 *   this reading prioritizes state-building over cultural or socialist
 *   content, frames the Arab Palestinian majority as a demographic obstacle
 *   to sovereignty, and treats population transfer as a necessary mechanism.
 *   It functions as a coordination mechanism for Jewish national action
 *   across diaspora communities while asymmetrically extracting land,
 *   political authority, and demographic space from Arab Palestinian
 *   inhabitants. This constraint is one reading of the contested kernel
 *   jewish_territorial_claim; sibling readings include cultural Zionism
 *   (spiritual center without sovereignty), labor Zionism (socialist
 *   settlement), and revisionist Zionism (maximalist territorial claims).
 *
 * KEY AGENTS:
 *   - zionist_political_leadership: agenda_setter (institutional/global) â articulates the sovereignty claim and mobilizes international resources
 *   - jewish_diaspora_communities: beneficiary (organized/global) â provides funds and immigrants, receives promised statehood solution
 *   - jewish_yishuv_settlers: beneficiary (moderate/regional) â builds demographic and military facts on the ground
 *   - arab_palestinian_inhabitants: payer (moderate/regional) â faces displacement and political subordination as demographic obstacle
 *   - british_mandatory_authority: observer (institutional/global) â enables immigration and land transfer under mandate
 *   - binationalist_jewish_opposition: excluded (moderate/regional) â advocates alternatives marginalized within Zionist institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.78).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.85).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Territorial Sovereignty Claim").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political history / settler colonialism / nationalism studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, 'afcf2c13-4a41-4ac4-972a-e063563e8278').
narrative_ontology:cs_kernel_codification('afcf2c13-4a41-4ac4-972a-e063563e8278', distributed).
narrative_ontology:cs_authority_grounding('afcf2c13-4a41-4ac4-972a-e063563e8278', lineage).
narrative_ontology:cs_interpretation_layer_present('afcf2c13-4a41-4ac4-972a-e063563e8278').
narrative_ontology:cs_reading_relation('afcf2c13-4a41-4ac4-972a-e063563e8278', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('afcf2c13-4a41-4ac4-972a-e063563e8278', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('afcf2c13-4a41-4ac4-972a-e063563e8278', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('afcf2c13-4a41-4ac4-972a-e063563e8278', foundational, statehood_required_for_survival).
narrative_ontology:cs_axiom_status(statehood_required_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('afcf2c13-4a41-4ac4-972a-e063563e8278', statehood_required_for_survival, instrumental).
narrative_ontology:cs_axiom('afcf2c13-4a41-4ac4-972a-e063563e8278', foundational, jewish_majority_as_democratic_prerequisite).
narrative_ontology:cs_axiom_status(jewish_majority_as_democratic_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('afcf2c13-4a41-4ac4-972a-e063563e8278', jewish_majority_as_democratic_prerequisite, conventional).
narrative_ontology:cs_reference_frame('afcf2c13-4a41-4ac4-972a-e063563e8278', political_sovereignty_majority_framework).
narrative_ontology:cs_drift_state('afcf2c13-4a41-4ac4-972a-e063563e8278', late_mandate_partition_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afcf2c13-4a41-4ac4-972a-e063563e8278', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_yishuv_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, arab_palestinian_inhabitants).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_self_determination_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, majoritarian_statehood_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and enforces the claim that Jewish survival requires territorial sovereignty with a Jewish majority in Palestine. Mobilizes international diplomatic support, immigration flows, and settlement capital. Their authority is constituted by fidelity to this specific reading; abandoning the majority-demographic premise would dissolve their legitimacy and the movement's core platform.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive the promised solution to persecution through statehood. Contribute funds, political support, and immigrants to the territorial project. Alternatives such as assimilation, diaspora nationalism, or binationalism are suppressed within communal discourse as inadequate or existentially dangerous.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_communities, beneficiary,
    organized, biographical, constrained, global).

% Settle the land, build economic and military facts on the ground, and directly benefit from the emerging state's institutions. Their presence is the demographic material of the majority claim; they cannot easily relocate without abandoning invested capital, collective labor, and community security structures.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_yishuv_settlers, beneficiary,
    moderate, biographical, constrained, regional).

% Live on the land targeted for Jewish majority sovereignty. Face displacement, land transfer, and political subordination as the obstacle to the demographic prerequisite. Their national claims are treated as illegitimate or secondary within the Zionist institutional framework. Exit means expulsion or accepting minority status in an ethnic state.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, arab_palestinian_inhabitants, payer,
    moderate, biographical, trapped, regional).

% Administers Palestine under League of Nations mandate, balancing Zionist immigration against Arab majority interests and imperial strategy. They enable the demographic transformation through land and immigration policy but do not themselves collect the sovereign benefit.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandatory_authority, observer,
    institutional, generational, constrained, global).

% Advocate for Jewish-Arab binational state or non-territorial cultural autonomy. Their alternatives are marginalized within Zionist institutional politics as utopian or traitorous. They are structurally excluded from the decision-making apparatus that allocates land, immigration certificates, and military resources.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, binationalist_jewish_opposition, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_communities).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national action across diaspora factions toward a single territorial solution to statelessness and antisemitism, replacing fragmented philanthropic or local defense strategies with a unified sovereignty project.
% TRANSFER_FUNCTION: Moves land, political authority, and demographic majority from Arab Palestinian inhabitants to the Jewish national collective, via settlement, immigration, and ultimately transfer or displacement.
% ABSENT_VOICES: Arab Palestinian national representatives and binationalist Jewish intellectuals are present in the territory but excluded from the Zionist institutional framework that determines immigration quotas, land purchases, and political goals. Their counter-claims are heard by the British mandatory administration but not by the Zionist executive.
% DISAPPEARANCE_RATIONALE: If the territorial sovereignty claim with majority requirement vanished overnight, the Zionist project would lose its organizing principle; Jewish immigration would lack a coherent destination, Arab Palestinians would retain majority political authority, and the British mandate would face a radically different set of political demands.
% FOUNDING_PROBLEM: Persistent antisemitic persecution and statelessness of Jews in Europe; the failure of emancipation and assimilation to secure Jewish collective safety.
% FOUNDING_PROBLEM_CORROBORATION: Jewish diaspora communities and international observers attested to pogroms and legal discrimination. However, Palestinian Arab leaders and binationalist Jewish groups contested that territorial sovereignty with a Jewish majority was the necessary or just remedy, arguing instead for civil rights in existing states or binational arrangements. Corroboration of the problem is broad; corroboration of this specific solution from outside the benefiting parties is sparse.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the constraint extracts land, political agency, and demographic presence from Arab Palestinians to maintain a Jewish majority. Suppression is higher still (0.85) because the claim's persistence requires actively suppressing Palestinian national claims, binational alternatives, and Arab political equality. Theater is moderate (0.38): the coordination function for Jewish diaspora safety is genuine, but an increasing share of activity performs demographic engineering dressed as security necessity. Accessibility collapse is high (0.82) because once the political Zionist framework dominates Zionist institutions, alternatives such as binationalism or cultural autonomy collapse in legitimacy. Resistance is substantial (0.76) from Arab Palestinian revolts and from marginalized Jewish opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the Zionist executive seat, the constraint is urgent coordination for collective survival against genocidal threat; from the Arab Palestinian seat, it is an actively enforced extraction of homeland and political agency. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   zionist_political_leadership and jewish_diaspora_communities sit near the beneficiary end (low d): the constraint is built to deliver statehood and collective safety to the Jewish national collective. jewish_yishuv_settlers also benefit but bear frontier costs. arab_palestinian_inhabitants sit near the full-target end (high d): the constraint structurally defines them as an obstacle to be removed or reduced. british_mandatory_authority sits near symmetric: they enable the constraint but do not ultimately collect the sovereign benefit. binationalist_jewish_opposition is excluded, with no directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure rope (which would ignore the Arab Palestinian victimization and transfer mechanism) or pure snare (which would deny the genuine coordination function for a persecuted diaspora). The R5 genealogy shows the founding problemâantisemitic persecutionâwas real and corroborated, but the arrangement's specific territorial-majority solution and its methods generated a distinct extraction layer. This captures the dual nature: coordination for one collective through extraction from another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the political Zionist reading the only coherent interpretation of the Jewish territorial claim, or do the sibling readings represent structurally valid alternatives?',
    'Comparative historical analysis of the four readings'' institutional outcomes and their relationship to the kernel text and practice.',
    'If sibling readings are structurally valid, the political Zionist reading''s exclusivity claim is a constructed constraint rather than a necessary derivation; this would shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether political Zionism dominates by logical necessity or historical contingency.').

omega_variable(
    transfer_mechanism_necessity,
    'Was population transfer a structurally necessary mechanism for achieving Jewish majority sovereignty, or a contingent policy choice that could have been avoided?',
    'Counterfactual historical analysis and examination of alternative demographic and political pathways such as federation, slower immigration, or binationalism.',
    'If necessary, the extractiveness is inherent to the constraint''s logic; if contingent, the extraction is a separable policy layer that could be stripped, potentially reclassifying the core claim as less extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transfer_mechanism_necessity, empirical, 'Whether transfer was structurally necessary or contingent.').

omega_variable(
    antisemitism_as_empirical_driver,
    'Does the persistence of antisemitism empirically validate the political Zionist premise that statelessness is the root cause, or does the reading construct a teleological narrative that selects evidence?',
    'Historical-sociological analysis of antisemitic violence rates pre- and post-statehood, and comparison with other stateless or diaspora groups.',
    'If the empirical basis is weak, the coordination function is built on a contested empirical premise, raising the theater_ratio and challenging the rope component of the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antisemitism_as_empirical_driver, empirical, 'Empirical grounding of the founding problem in the Jewish experience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_pol_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jtc_pol_tr_t10, jewish_territorial_claim__political_zionism_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(jtc_pol_tr_t20, jewish_territorial_claim__political_zionism_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(jtc_pol_tr_t30, jewish_territorial_claim__political_zionism_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(jtc_pol_tr_t40, jewish_territorial_claim__political_zionism_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(jtc_pol_tr_t50, jewish_territorial_claim__political_zionism_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(jtc_pol_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jtc_pol_be_t10, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(jtc_pol_be_t20, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(jtc_pol_be_t30, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(jtc_pol_be_t40, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(jtc_pol_be_t50, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jtc_pol_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jtc_pol_su_t10, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(jtc_pol_su_t20, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(jtc_pol_su_t30, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(jtc_pol_su_t40, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(jtc_pol_su_t50, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
