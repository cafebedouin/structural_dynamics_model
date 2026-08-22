% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Political Zionist Reading: Statehood as Solution to the Jewish Question
 *   domain: political/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This story instantiates the political Zionist reading of the contested
 *   jewish_territorial_claim kernel: the diagnosis that antisemitism is
 *   fundamentally a problem of Jewish statelessness, solvable only through
 *   territorial sovereignty secured by a Jewish demographic majority. Unlike
 *   the cultural Zionist reading (spiritual center, no sovereignty
 *   requirement) or the labor Zionist reading (socialist settlement-building
 *   as the primary mechanism), the political reading centers diplomacy and
 *   statecraft — securing great-power patronage (Ottoman concessions, then
 *   the Balfour Declaration and the Mandate) — as the decisive lever, and
 *   treats the existing Arab population's numerical predominance as the chief
 *   obstacle to be managed rather than a co-equal claim to be negotiated as
 *   between nations. This is a distinct constraint from its siblings, not a
 *   different observation angle on one: the extraction structure, victim set,
 *   and enforcement mechanism differ by reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.62).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.58).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Reading: Statehood as Solution to the Jewish Question").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '158dfcd1-b769-4f72-ab52-daf2a6412c12').
narrative_ontology:cs_kernel_codification('158dfcd1-b769-4f72-ab52-daf2a6412c12', distributed).
narrative_ontology:cs_authority_grounding('158dfcd1-b769-4f72-ab52-daf2a6412c12', distributed).
narrative_ontology:cs_reading_relation('158dfcd1-b769-4f72-ab52-daf2a6412c12', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('158dfcd1-b769-4f72-ab52-daf2a6412c12', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('158dfcd1-b769-4f72-ab52-daf2a6412c12', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('158dfcd1-b769-4f72-ab52-daf2a6412c12', foundational, sovereignty_with_majority_is_necessary_and_sufficient_remedy).
narrative_ontology:cs_axiom_status(sovereignty_with_majority_is_necessary_and_sufficient_remedy, holdable).
narrative_ontology:cs_axiom_grounding('158dfcd1-b769-4f72-ab52-daf2a6412c12', sovereignty_with_majority_is_necessary_and_sufficient_remedy, empirically_contingent).
narrative_ontology:cs_axiom('158dfcd1-b769-4f72-ab52-daf2a6412c12', secondary, diplomatic_legitimation_precedes_and_licenses_demographic_transformation).
narrative_ontology:cs_axiom_status(diplomatic_legitimation_precedes_and_licenses_demographic_transformation, holdable).
narrative_ontology:cs_axiom_grounding('158dfcd1-b769-4f72-ab52-daf2a6412c12', diplomatic_legitimation_precedes_and_licenses_demographic_transformation, instrumental).
narrative_ontology:cs_reference_frame('158dfcd1-b769-4f72-ab52-daf2a6412c12', first_zionist_congress_basel_program).
narrative_ontology:cs_drift_state('158dfcd1-b769-4f72-ab52-daf2a6412c12', post_1948_partition_and_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('158dfcd1-b769-4f72-ab52-daf2a6412c12', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, diaspora_jewish_communities_facing_persecution).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, non_zionist_diaspora_jews_advocating_alternatives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_question_requires_territorial_solution).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, statehood_ends_diaspora_vulnerability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Herzlian-Weizmannite diplomatic leadership pursues great-power patronage (Ottoman, then British) to secure a charter for Jewish sovereignty. Frames the Jewish Question as fundamentally a problem of statelessness solvable only by a Jewish-majority polity with its own instruments of coercion. Negotiates borders, immigration quotas, and demographic targets as the primary lever of policy; treats the existing Arab population's demographic weight as the chief obstacle to the sovereignty project rather than as a co-equal national claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, agenda_setter,
    organized, generational, arbitrage, national).

% Jews under pogrom violence, legal disability, and rising racial antisemitism in Europe are offered statehood as the promised remedy: a sovereign refuge where minority status itself, and thus persecution, is structurally impossible. Many have no other viable exit and depend entirely on the movement's diplomatic success and on organized immigration infrastructure to escape worsening conditions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, diaspora_jewish_communities_facing_persecution, beneficiary,
    powerless, biographical, constrained, continental).

% Jewish settlers already resident or newly arriving build the demographic and institutional base (land purchase, agricultural colonies, proto-state bodies) the sovereignty claim depends on. They gain security, land, and eventual citizenship in the projected state, but bear the costs of communal defense, land disputes, and recurrent violence generated by the demographic contest itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population, payer).

% The existing majority population of Palestine finds its continued numerical predominance treated by the political program as the central problem to be solved rather than a political fact to be accommodated. Land transfers, immigration policy, and eventually population transfer proposals are evaluated by their effect on achieving Jewish demographic majority. Has no seat in the diplomatic negotiations (Balfour, Peel, mandate administration) that determine its future and no recognized channel to contest the sovereignty claim on equal terms.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Bundists, autonomists, and assimilationist Jews who argue the Jewish Question is better solved through minority rights, cultural autonomy, or socialist transformation within existing states find their alternative diagnosis increasingly marginalized as the political Zionist movement consolidates institutional funding, diplomatic recognition, and the narrative monopoly over what counts as a serious answer to antisemitism.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, non_zionist_diaspora_jews_advocating_alternatives, payer,
    moderate, biographical, constrained, continental).

% Holds the Mandate, issues the Balfour Declaration commitment, and administers immigration quotas and land regulation, arbitrating (unevenly) between Zionist demands for accelerated Jewish immigration and Arab demands for representative government reflecting existing demographic majority. Its enforcement choices (White Papers, immigration caps, policing) directly shape whether the political program's demographic project can proceed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, observer,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed, persecuted diaspora Jewish communities around a single diplomatic and institutional project — securing international recognition and territorial sovereignty — that no individual community or philanthropic effort could achieve alone, converting scattered vulnerability into organized political leverage.
% TRANSFER_FUNCTION: Moves diplomatic recognition, land, and eventual sovereign control from the existing Arab-majority political order of Palestine to an emergent Jewish political leadership and its settler base, financed by diaspora capital and legitimated by great-power patronage; the transfer is justified by reference to Jewish need but its object is Palestinian land and political standing.
% ABSENT_VOICES: Palestinian Arab political representatives are structurally absent from the founding diplomatic instruments (Balfour Declaration, early Zionist Congresses) that set the terms of the demographic project; non-Zionist Jewish alternatives (Bundist, autonomist) are present in Jewish communal debate but increasingly lose institutional standing and funding as political Zionism consolidates.
% DISAPPEARANCE_RATIONALE: If the political-Zionist program's specific commitment to territorial sovereignty with Jewish demographic majority disappeared, diaspora Jewish politics would likely re-orient toward the alternatives it displaced (minority-rights frameworks, cultural autonomism, or non-statist cultural Zionism), the demographic contest over Palestine's land and immigration policy would cease to be organized around majority-achievement as the central variable, and the institutional architecture built to pursue statehood (settlement agencies, national funds, diplomatic missions) would lose their organizing purpose.
% FOUNDING_PROBLEM: European Jewish communities faced escalating legal disability, pogrom violence, and an emergent racial antisemitism (crystallized for Herzl by the Dreyfus Affair) that emancipation and assimilation had failed to resolve; the founding claim was that only a state of their own, with a Jewish majority able to exercise sovereign self-defense, could permanently remove Jews from minority vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of antisemitism (including non-Zionist scholars) corroborate that pre-1948 persecution was real and that emancipation had structurally failed to secure Jewish safety in much of Europe — this part of the founding problem is attested well outside Zionist ranks. Whether the specific remedy of demographic-majority sovereignty in Palestine was the only or best solution, versus minority-rights or diasporic alternatives, remains disputed by Bundist historiography and by Palestinian and post-colonial scholarship, which locate the same founding narrative as retrospective justification for a settlement project whose demographic logic predates and outlives the crisis it claims to answer.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction rises across the interval (0.35 to 0.62) as the diplomatic project moves from aspirational congress resolutions (1897) toward concrete instruments transferring land, immigration control, and eventual sovereign authority (1917 Balfour, 1922 Mandate, 1937 Peel proposals, 1947-48 partition and statehood) — each step converts diplomatic recognition into material transfer at Palestinian Arab expense. Suppression rises in step (0.2 to 0.58) tracking the shift from voluntary land purchase and negotiation toward organized paramilitary and eventually state capacity (Haganah, and by 1948 active population displacement) required to secure demographic majority against a resident population that did not consent to minoritization. Theater ratio stays low and roughly flat (0.1 to 0.2): the coordination function (organizing dispersed, endangered diaspora communities around institutions and diplomacy) is largely real and substantive rather than performative, even as its downstream effects become more extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (Zionist political leadership) this reads as coordination: a stateless, endangered people organizing to secure the only remedy that has ever reliably ended minority vulnerability — sovereignty. From the payer seat (Palestinian Arab population) the identical structure reads as an externally sponsored demographic transformation project executed without their consent and increasingly by force. The engine computes both seat-classifications from the same structural data; the divergence between 'coordination for the persecuted' and 'extraction from the incumbent' is the substantive finding, not a contradiction to be resolved by picking a side.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities facing persecution and the Zionist leadership sit toward the beneficiary end: the arrangement is built to and does deliver refuge, institutional standing, and eventually sovereignty. The yishuv settler population is dual-positioned — beneficiary of the emerging state's protections, but payer of the recurrent violence the demographic contest itself generates. The Palestinian Arab population sits at the full-target end: trapped exit options, no seat in the founding diplomatic instruments, and its demographic weight treated instrumentally as the variable the whole program is organized to overcome. Non-Zionist diaspora Jews advocating minority-rights or autonomist alternatives are payers in a narrower sense: they bear the cost of institutional marginalization as political Zionism captures diaspora funding and moral authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the Jewish Question as physical and political vulnerability under conditions of statelessness — was genuinely live at founding and corroborated by scholarship well outside Zionist ranks; this blocks any easy 'purely coordination or purely extraction' verdict. But the SPECIFIC remedy chosen (majority sovereignty in a specific already-populated territory) carries a cost structure that the political reading's own internal logic treats as a secondary problem to be managed (transfer, demographic engineering) rather than as the core moral question. Classifying this as tangled_rope rather than snare or rope preserves both halves: real coordination function for a real founding problem, alongside asymmetric extraction from a population with no voice in the founding instruments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_necessity_vs_constructed_inevitability,
    'Was territorial sovereignty with Jewish demographic majority genuinely the only structural solution available to the pre-1948 Jewish Question, or was it one contested option among several (minority rights frameworks, cultural autonomism, binational statehood) that political Zionist diplomacy successfully naturalized as inevitable through institutional consolidation?',
    'Comparative assessment of interwar minority-rights regimes (League of Nations minority treaties) for their actual protective efficacy versus their political failure, combined with historiographical analysis of why Bundist and autonomist alternatives lost institutional and financial support within diaspora Jewish communities independent of their merits.',
    'If minority-rights alternatives were structurally viable and lost primarily due to political-Zionist institutional capture of diaspora resources rather than genuine inadequacy, the coordination-function claim weakens substantially and the classification shifts toward snare (extraction from both non-Zionist Jews and Palestinian Arabs, coordination function overstated). If minority-rights frameworks were genuinely inadequate given the trajectory of 1930s-40s European antisemitism, the coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_necessity_vs_constructed_inevitability, conceptual, 'Whether statehood-with-majority was a necessary response or a constructed-as-inevitable political choice among live alternatives.').

omega_variable(
    transfer_as_designed_mechanism_vs_emergent_outcome,
    'Within the political Zionist reading specifically, was Arab population displacement treated from early on (Herzl''s diaries, later Peel Commission-era transfer discussions) as a necessary designed mechanism for achieving majority, or did it emerge contingently from the demographic contest without being part of the reading''s original theory of the case?',
    'Close reading of primary Zionist Congress proceedings, Herzl''s private diaries, and Jewish Agency internal deliberations across the interval, distinguishing early-period rhetoric from post-1937 (Peel Commission) explicit transfer planning.',
    'If transfer was present as a designed mechanism from the founding period, the extraction is intrinsic to the reading from t0 and the rising extractiveness trajectory understates early-period culpability. If transfer emerged as an adaptation to failed negotiation later in the interval, the temporal measurement series (rising extraction) more accurately captures a genuine drift rather than a concealed founding intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_as_designed_mechanism_vs_emergent_outcome, empirical, 'Whether population transfer was foundational design or emergent adaptation within the political Zionist program specifically.').

omega_variable(
    sibling_reading_boundary_ambiguity,
    'Where exactly does the political Zionist reading''s emphasis on diplomatic statecraft over settlement-building draw its boundary against the labor Zionist reading, given that in practice the same historical actors (e.g. the Jewish Agency) pursued both diplomacy and settlement simultaneously and the two readings'' institutional infrastructure substantially overlapped?',
    'This is a committer-structure question, not a resolvable empirical one: it depends on which causal lever (diplomatic recognition vs. facts-on-the-ground settlement) a given historical actor or historian treats as doing the primary explanatory work for how sovereignty was actually achieved.',
    'Under a diplomacy-primary framing (this story), the extraction is mediated through international legitimation instruments (Balfour, Mandate, UN Partition) and the victim set centers on those excluded from those instruments. Under a settlement-primary framing (labor_zionism_reading), extraction is mediated through land acquisition and demographic colonization on the ground, with a distinguishable if overlapping victim set (displaced tenant farmers vs. politically excluded elites). The two framings are not fully separable in the historical record, which is why they are authored as separate constraints rather than as one story with a measurement parameter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_ambiguity, conceptual, 'Structural ambiguity in where the political and labor Zionist readings'' causal claims diverge given overlapping institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t1897, observed).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.14).
narrative_ontology:measurement_basis(jewi_tr_t1917, observed).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1929, 0.17).
narrative_ontology:measurement_basis(jewi_tr_t1929, observed).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.19).
narrative_ontology:measurement_basis(jewi_tr_t1936, observed).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1939, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.35).
narrative_ontology:measurement_basis(jewi_be_t1897, observed).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement_basis(jewi_be_t1917, observed).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1929, 0.52).
narrative_ontology:measurement_basis(jewi_be_t1929, observed).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.57).
narrative_ontology:measurement_basis(jewi_be_t1936, observed).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1939, 0.6).
narrative_ontology:measurement_basis(jewi_be_t1939, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.2).
narrative_ontology:measurement_basis(jewi_su_t1897, observed).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.32).
narrative_ontology:measurement_basis(jewi_su_t1917, observed).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1929, 0.45).
narrative_ontology:measurement_basis(jewi_su_t1929, observed).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1936, 0.55).
narrative_ontology:measurement_basis(jewi_su_t1936, observed).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1939, 0.58).
narrative_ontology:measurement_basis(jewi_su_t1939, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the jewish_territorial_claim kernel, each authored as a separate constraint per the epsilon-invariance principle: cultural_zionism_reading (lowest extraction — drops the majority-sovereignty requirement), labor_zionism_reading (settlement-primary mechanism, overlapping institutional actors with this reading), political_zionism_reading (this story — diplomacy-primary, statehood-with-majority as the diagnosed solution), and revisionist_zionism_reading (highest extraction — maximalist territory, explicit force-compelled acceptance doctrine). Each carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged because measuring 'the Zionist territorial claim' by different observables (spiritual-center content vs. statecraft vs. settlement vs. maximalist-force doctrine) yields genuinely different extraction profiles, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
