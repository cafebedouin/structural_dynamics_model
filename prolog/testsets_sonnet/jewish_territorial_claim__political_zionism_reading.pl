% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism: Diplomatic-State Solution to the Jewish Question Requiring Sovereign Jewish Majority
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This story isolates the political Zionist reading of the Jewish
 *   territorial claim: the Herzlian-Weizmannite diplomatic-statist strand
 *   that treats antisemitism as an ineliminable feature of diaspora
 *   existence, and treats an internationally chartered sovereign state with a
 *   Jewish demographic majority as the necessary and sufficient remedy. This
 *   reading is distinguished from labor Zionism (which prioritizes socialist
 *   settlement and 'facts on the ground' over diplomacy), cultural Zionism
 *   (which explicitly rejects the sovereignty/majority requirement in favor
 *   of a spiritual center), and revisionist Zionism (which maximizes
 *   territorial scope to both banks of the Jordan and endorses immediate
 *   coercive force). The political Zionist reading is structurally centered
 *   on great-power diplomacy (Ottoman concessions, the Balfour Declaration,
 *   the Mandate) as the mechanism, and treats the existing Arab population as
 *   a demographic and diplomatic obstacle to be managed — through land
 *   purchase, immigration timing, and eventually population transfer
 *   proposals — rather than as a partner or as an incidental feature of the
 *   settlement project (contrast labor Zionism's builder ethos) or an
 *   irrelevant question (contrast cultural Zionism's non-sovereigntist
 *   framing).
 *
 * KEY AGENTS:
 *   - zionist_organization_diplomatic_leadership: sets the diplomatic agenda, organized/mobile — pursues sovereignty via great-power patronage
 *   - european_jewish_communities_seeking_refuge: primary intended beneficiary, moderate/constrained — the persecuted population the program claims to solve for
 *   - palestinian_arab_peasantry: primary payer, powerless/trapped — land tenure and residence treated as the demographic obstacle
 *   - palestinian_arab_urban_elites: payer and excluded, moderate/constrained — political voice foreclosed by the bilateral diplomatic channel
 *   - british_mandatory_authority: institutional patron and enforcer, institutional/arbitrage — administers the machinery that operationalizes the claim
 *   - non_zionist_diaspora_jews: excluded, moderate/mobile — reject the statehood-as-only-remedy premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.62).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.58).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Diplomatic-State Solution to the Jewish Question Requiring Sovereign Jewish Majority").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '1f0c1dfe-b4e8-408c-99fb-9d8285ef87db').
narrative_ontology:cs_kernel_codification('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', distributed).
narrative_ontology:cs_authority_grounding('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', distributed).
narrative_ontology:cs_reading_relation('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', foundational, sovereignty_with_demographic_majority_is_necessary_remedy).
narrative_ontology:cs_axiom_status(sovereignty_with_demographic_majority_is_necessary_remedy, holdable).
narrative_ontology:cs_axiom_grounding('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', sovereignty_with_demographic_majority_is_necessary_remedy, instrumental).
narrative_ontology:cs_axiom('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', foundational, diplomatic_great_power_patronage_is_legitimate_mechanism).
narrative_ontology:cs_axiom_status(diplomatic_great_power_patronage_is_legitimate_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', diplomatic_great_power_patronage_is_legitimate_mechanism, conventional).
narrative_ontology:cs_axiom('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', secondary, arab_population_presence_is_obstacle_to_be_diplomatically_managed).
narrative_ontology:cs_axiom_status(arab_population_presence_is_obstacle_to_be_diplomatically_managed, holdable).
narrative_ontology:cs_axiom_grounding('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', arab_population_presence_is_obstacle_to_be_diplomatically_managed, instrumental).
narrative_ontology:cs_reference_frame('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', herzlian_diplomatic_statist_program).
narrative_ontology:cs_drift_state('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', post_1948_state_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f0c1dfe-b4e8-408c-99fb-9d8285ef87db', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, european_jewish_communities_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_organization_diplomatic_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, future_jewish_state_citizenry).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_peasantry).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_urban_elites).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, non_zionist_diaspora_jews_targeted_by_transfer_logic_debates).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_question_requires_territorial_solution).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, diplomatic_legal_sovereignty_achievable_through_great_power_patronage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Herzl and successors pursue a chartered, internationally recognized Jewish state through negotiation with imperial powers (Ottoman Porte, later Britain), treating diplomacy and demographic majority-building as the primary instruments. They set the movement's institutional agenda through congresses, the Jewish Agency, and fundraising apparatus, and can pivot territory or patron without abandoning the core demand: sovereignty over a defined territory with a Jewish demographic majority.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_organization_diplomatic_leadership, agenda_setter,
    organized, generational, mobile, global).

% Facing pogroms, legal discrimination, and rising nationalist antisemitism, these communities receive the promise that sovereign statehood — not emancipation within existing states — resolves their exposure. Emigration to Palestine offers an exit from persecution but is itself constrained by British quotas, cost, and the political timetable of the movement's diplomacy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, european_jewish_communities_seeking_refuge, beneficiary,
    moderate, biographical, constrained, continental).

% The prospective demographic majority whose eventual sovereignty and security the entire diplomatic-political program is oriented toward securing; they inherit the state's legitimacy claims but also its founding demographic arithmetic and the unresolved status of the population displaced or subordinated to produce that arithmetic.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, future_jewish_state_citizenry, beneficiary,
    moderate, civilizational, constrained, national).

% Cultivators and tenant farmers on land increasingly purchased by Zionist institutions from absentee landlords; the political program's demographic-majority requirement treats their continued presence as the central obstacle to be managed, displaced, or diplomatically negotiated around, with land sales and eviction as its practical instrument regardless of their consent or awareness.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_peasantry, payer,
    powerless, generational, trapped, local).

% Notable families and nascent nationalist leadership who see political rights, land tenure, and eventual self-determination foreclosed by a diplomatic program negotiated over their heads with imperial patrons; their objections are registered in petitions, riots, and delegations to London but are structurally outside the bilateral Zionist-imperial negotiating table the political program relies on.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_urban_elites, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, palestinian_arab_urban_elites, excluded).

% Bundist, assimilationist, and territorially-agnostic Jewish communities who reject the premise that emancipation-in-place has failed and statehood is the only remedy; the political Zionist program treats their continued diaspora existence as the very Jewish Question requiring solution, effectively arguing their own preferred remedies (autonomism, socialist internationalism, assimilation) into irrelevance without their assent.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, non_zionist_diaspora_jews_targeted_by_transfer_logic_debates, excluded,
    moderate, biographical, mobile, global).

% Issues and administers the Balfour Declaration and Mandate, adjudicating immigration quotas and land transfer regulations between Zionist diplomatic pressure and Arab resistance; holds the enforcement machinery (military, police, legal courts) that makes the political program's territorial claim operative on the ground, and can withdraw or reverse that machinery as imperial interest shifts.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandatory_authority, observer,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandatory_authority, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a scattered, legally vulnerable diaspora population around a single diplomatic-political demand — internationally chartered sovereignty with a Jewish demographic majority — replacing fragmented emancipation strategies with one negotiable, state-seeking program that great powers could recognize and patronize.
% TRANSFER_FUNCTION: Moves land, political voice, and demographic weight from the resident Palestinian Arab population to the incoming Jewish settler population and its emerging state institutions, financed by diaspora philanthropic capital and legitimated through imperial patronage (Ottoman concession-seeking, then the Balfour Declaration and British Mandate).
% ABSENT_VOICES: Palestinian Arab urban and peasant populations are negotiated around rather than with — the political program's central diplomatic channel runs between the Zionist Organization and imperial patrons, structurally excluding Arab political representation from the table where the territory's disposition is decided. Non-Zionist and anti-Zionist diaspora Jews who reject the statehood premise are also absent from a movement that speaks for 'the Jewish people' as a whole.
% DISAPPEARANCE_RATIONALE: Had the political-diplomatic program not secured great-power patronage and territorial concentration, the Jewish Question in Europe would have been addressed (or left unaddressed) through emancipation, autonomism, emigration to multiple destinations, or continued vulnerability without a territorial-sovereignty remedy; Palestine's demographic and political trajectory, land tenure patterns, and the entire subsequent conflict architecture would differ substantially.
% FOUNDING_PROBLEM: Legal emancipation in Europe had not ended antisemitism; pogroms, the Dreyfus Affair, and rising racial nationalism convinced Herzl and allies that Jews would remain structurally vulnerable in any diaspora status, however legally equal on paper, and that only territorial sovereignty with control over one's own state apparatus (borders, army, immigration) could remove that vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Holocaust and comparative genocide studies (outside the Zionist movement itself) corroborate that stateless and minority status left European Jews without effective protection against mass violence, supporting the founding diagnosis. However, Bundist, assimilationist, and post-colonial historians dispute that territorial sovereignty over an already-inhabited land was the only or best remedy, and Palestinian historians and demographers attest that the 'solution' transferred the vulnerability onto a different population rather than resolving it structurally.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily across the interval (0.28 to 0.62) tracking land purchase concentration, immigration growth, and the shift from a minority to majority-seeking demographic program after the Balfour Declaration (1917) and through the interwar Mandate period — the transfer of land and political standing from the Arab population intensifies as the demographic-majority mechanism matures. Suppression rises correspondingly (0.2 to 0.58) as British Mandatory enforcement (immigration control, land transfer ordinances, police power) becomes the active machinery sustaining the claim against Arab resistance (the 1929 riots, the 1936-39 revolt). Theater ratio is comparatively low and slightly declining (0.3 to 0.2) because the coordination function here — organizing a persecuted diaspora around an actionable political program — was substantively real and institutionally executed (Jewish Agency, World Zionist Organization, fundraising and immigration infrastructure), not primarily performative; it ticks up slightly by 1948 as diplomatic rhetoric outpaces what negotiation alone could deliver against armed conflict. Resistance is high throughout (0.72) reflecting sustained Arab political and armed opposition. Accessibility collapse is moderate (0.5): alternative remedies for the Jewish Question (assimilation, autonomism, other territorial options considered and rejected such as Uganda) remained live debates within the movement itself for years, so alternatives had not fully collapsed even as the Palestine-majority program became dominant.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist diplomatic leadership and future citizenry seats compute the arrangement as coordination — solving a genuine and previously unaddressed vulnerability through the only mechanism (sovereignty) they judge adequate to that vulnerability's scale. The Palestinian Arab payer seats compute the same diplomatic and administrative apparatus as an enforced transfer of land and political standing, executed through instruments (land purchase regulation, immigration control, eventual transfer proposals) that never required their consent. The engine's per-seat computation should show this divergence directly from the structural power/exit data rather than from any claimed label.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish communities and the future state citizenry sit toward the beneficiary end: the program's diplomatic apparatus is built and justified in their name, though their own exit options (emigration quotas, cost, persecution timing) were themselves constrained — this is a beneficiary population that did not fully control the mechanism built to serve it. The Zionist diplomatic leadership sits closest to the agenda-setting/beneficiary pole with real mobility (access to multiple capitals, patrons, and fallback territorial options historically). Palestinian Arab peasantry sit at the extraction pole: trapped, powerless, and structurally treated as the variable to be solved for in the demographic-majority equation — land purchases and eventual transfer logic operate directly on their tenure and residence. Palestinian Arab urban elites bear costs but retain some capacity to organize resistance and petition, hence moderate power despite exclusion from the primary negotiating channel. The British Mandatory Authority is a distinct institutional actor whose interests (imperial strategy, later withdrawal) diverge from both Zionist and Arab interests — it enforces the machinery but is not itself a beneficiary of the demographic outcome, which is why it is coded observer/agenda_setter dual rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — diaspora Jewish vulnerability to majoritarian violence absent legal protection — was real and is independently corroborated by genocide scholarship outside the movement. But the political Zionist reading's specific remedy (sovereignty over an already-inhabited territory, requiring demographic majority) does not become obsolete merely because the founding diagnosis was correct; the mismatch here is not dead-problem-live-arrangement but rather contested-mechanism-for-live-problem. The classification as tangled_rope rather than pure snare rests on this: there is a genuine coordination function (organizing a stateless, persecuted population around an actionable collective remedy) operating through the same structure that also produces asymmetric extraction from a population that never opted into that remedy. Collapsing this into pure snare would erase the coordination problem the founding diagnosis genuinely names; collapsing it into rope or mountain would erase the documented displacement and enforcement machinery. Seat divergence is the point: from the beneficiary seat this looks like overdue collective self-defense; from the payer seat it looks like enforced demographic engineering executed through diplomatic and administrative means rather than raw force alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnosis_versus_remedy_separability,
    'Is the founding diagnosis (diaspora statelessness leaves Jews structurally vulnerable to majoritarian violence) separable from the specific remedy political Zionism proposed (sovereignty requiring demographic majority in an already-inhabited territory), such that the diagnosis could be corroborated as correct while the remedy is judged to have imposed unjustified costs on a third population?',
    'Comparative analysis of alternative proposed remedies considered by the same movement and era (territorialism, Uganda Plan, autonomism, emigration diversification) and their counterfactual cost profiles relative to the Palestine-majority program actually pursued.',
    'If separable, the constraint''s coordination function (corroborated) and its extractive mechanism (contested) can be evaluated independently, supporting a tangled_rope rather than snare or mountain reading. If inseparable — if only territorial sovereignty in a specific location could have solved the diagnosed problem at the scale required — the coordination and extraction functions collapse into one, strengthening either a mountain-of-necessity or snare-of-inevitable-cost reading depending on further contested premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diagnosis_versus_remedy_separability, conceptual, 'Whether the corroborated founding diagnosis licenses this specific territorial-sovereignty remedy over its historical alternatives.').

omega_variable(
    transfer_intent_versus_emergent_displacement,
    'To what extent did political Zionist leadership treat Arab population transfer as a considered, necessary policy instrument (per the story''s structural delta) versus an emergent consequence of land purchase and immigration dynamics that leadership rationalized after the fact?',
    'Archival analysis of Zionist Organization and Jewish Agency internal deliberations, correspondence, and planning documents (e.g., the Peel Commission transfer discussions, Ben-Gurion''s diaries) coded for premeditation versus post-hoc rationalization.',
    'If transfer was substantially premeditated policy, extractiveness and suppression scores understate the constraint''s severity and a reclassification toward snare becomes more defensible. If substantially emergent, the tangled_rope classification with genuine (if asymmetric) coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_intent_versus_emergent_displacement, empirical, 'Whether transfer functioned as premeditated mechanism or emergent rationalized consequence within this reading.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where precisely does political Zionism''s diplomatic-statist emphasis end and labor Zionism''s settlement-building emphasis begin, given that the same institutional leadership (e.g., Weizmann, Ben-Gurion across different periods) moved between diplomatic and settlement-building registers?',
    'Periodization analysis distinguishing phases where diplomatic negotiation with imperial patrons was the primary lever (pre-1917, Mandate-charter negotiations) from phases where facts-on-the-ground settlement building dominated strategy (1920s-30s Yishuv consolidation), cross-referenced against which reading''s structural delta best fits each phase.',
    'A cleaner periodization would support treating this story''s 1897-1948 interval as itself spanning a transition between readings, potentially requiring the interval to be split or the story''s endpoint metrics reweighted toward the diplomatic (pre-1917) phase where this reading is most structurally distinct from labor Zionism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Structural boundary ambiguity between the political and labor Zionist readings across the same historical actors and institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.3).
narrative_ontology:measurement(jewi_tr_t1905, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1905, 0.28).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.24).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1929, 0.22).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.28).
narrative_ontology:measurement(jewi_be_t1905, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1905, 0.34).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.48).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1929, 0.55).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1939, 0.6).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.2).
narrative_ontology:measurement(jewi_su_t1905, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1905, 0.28).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1929, 0.5).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1939, 0.55).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle (a single natural-language label — 'Zionism' or 'the Jewish territorial claim' — conflates structurally distinct claims about mechanism, scope, and treatment of the resident Arab population). political_zionism_reading is the diplomatic-statist strand (this file); labor_zionism_reading substitutes settlement-and-labor mechanism for diplomacy; cultural_zionism_reading rejects the sovereignty/majority requirement altogether (structurally the lowest-extraction sibling); revisionist_zionism_reading maximizes territorial scope and endorses immediate coercive force (structurally the highest-extraction sibling). Each carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged because their extraction and suppression profiles differ by wide margins that would violate ε-invariance if forced into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
