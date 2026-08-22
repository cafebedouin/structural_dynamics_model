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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Political Zionism: Statehood as Solution to the Jewish Question
 *   domain: political/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This story instantiates the political Zionism reading of the contested
 *   jewish_territorial_claim kernel: the strand descending from Herzl through
 *   the Basel Program and the Jewish Agency's diplomatic strategy, which
 *   holds that antisemitism is structurally unsolvable within diaspora
 *   civic-emancipation frameworks and that only sovereign territorial
 *   statehood with a secured Jewish demographic majority resolves the Jewish
 *   Question. This reading is state-building-first: cultural content (the
 *   Hebrew revival, spiritual center) is instrumental to the sovereignty
 *   project rather than an end in itself (contrast the
 *   cultural_zionism_reading), and it treats the existing Arab majority
 *   population as the central strategic obstacle to be managed toward Jewish
 *   demographic majority — a premise that generates the transfer question
 *   (population transfer considered as a legitimate policy mechanism, most
 *   explicitly in later Jewish Agency-era planning documents and the 1937
 *   Peel Commission's transfer recommendation) without embracing the
 *   revisionist reading's maximalist both-banks claim or Iron Wall doctrine
 *   of compelled acceptance.
 *
 * KEY AGENTS:
 *   - zionist_political_leadership: primary agenda-setter, diplomatic strategy
 *   - diaspora_jews_seeking_refuge: intended primary beneficiary, no strategic voice
 *   - yishuv_settler_population: on-ground beneficiary and co-administrator
 *   - palestinian_arab_population: primary target of the demographic-majority strategy
 *   - palestinian_peasant_tenants: directly displaced by land-purchase mechanism
 *   - british_mandatory_authority: adjudicating institutional power, mobile exit
 *   - ottoman_and_successor_diplomatic_powers: excluded-Arab-voice diplomatic architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.68).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.6).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Statehood as Solution to the Jewish Question").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '4b4b31eb-e01f-4687-8f10-0a32561cec1a').
narrative_ontology:cs_kernel_codification('4b4b31eb-e01f-4687-8f10-0a32561cec1a', distributed).
narrative_ontology:cs_authority_grounding('4b4b31eb-e01f-4687-8f10-0a32561cec1a', distributed).
narrative_ontology:cs_reading_relation('4b4b31eb-e01f-4687-8f10-0a32561cec1a', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b4b31eb-e01f-4687-8f10-0a32561cec1a', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('4b4b31eb-e01f-4687-8f10-0a32561cec1a', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('4b4b31eb-e01f-4687-8f10-0a32561cec1a', foundational, sovereignty_is_necessary_condition_for_jewish_safety).
narrative_ontology:cs_axiom_status(sovereignty_is_necessary_condition_for_jewish_safety, holdable).
narrative_ontology:cs_axiom_grounding('4b4b31eb-e01f-4687-8f10-0a32561cec1a', sovereignty_is_necessary_condition_for_jewish_safety, empirically_contingent).
narrative_ontology:cs_axiom('4b4b31eb-e01f-4687-8f10-0a32561cec1a', foundational, demographic_majority_is_prerequisite_of_viable_statehood).
narrative_ontology:cs_axiom_status(demographic_majority_is_prerequisite_of_viable_statehood, holdable).
narrative_ontology:cs_axiom_grounding('4b4b31eb-e01f-4687-8f10-0a32561cec1a', demographic_majority_is_prerequisite_of_viable_statehood, instrumental).
narrative_ontology:cs_reference_frame('4b4b31eb-e01f-4687-8f10-0a32561cec1a', herzlian_statist_diplomacy).
narrative_ontology:cs_drift_state('4b4b31eb-e01f-4687-8f10-0a32561cec1a', post_1948_state_founding, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b4b31eb-e01f-4687-8f10-0a32561cec1a', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, diaspora_jews_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_peasant_tenants).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_question_requires_territorial_solution).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, diplomatic_sovereignty_as_normalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diplomats and organizers (Herzl's successors through the Jewish Agency) who pursue great-power charters, negotiate with Ottoman and later British authorities, and treat a Jewish-majority state as the necessary and sufficient solution to antisemitism. They set the movement's institutional agenda, control immigration and land-purchase funds, and can relocate their diplomatic effort across European capitals as circumstances shift.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, agenda_setter,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, beneficiary).

% Populations facing pogroms, legal exclusion, and later existential persecution in Europe, for whom the promise of sovereign statehood offers the only proposed escape from statelessness. They have little say over the movement's strategic choices but are the intended beneficiaries the state-building program is justified by.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, diaspora_jews_seeking_refuge, beneficiary,
    powerless, biographical, trapped, continental).

% Jewish settlers already resident in Palestine who purchase land, build agricultural and urban settlements, and organize toward a demographic majority. They benefit from land transfers and institutional development funded by diaspora capital, and they administer the emerging quasi-state institutions (Jewish Agency, Histadrut, self-defense militias) that the political program depends on.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, yishuv_settler_population, agenda_setter).

% The existing majority population of Palestine, whose continued numerical and political predominance is treated by the political program as the central obstacle to achieving Jewish sovereignty. Land purchases displace tenant cultivators; political organizing for a Jewish-majority state necessarily contemplates population transfer or subordination as a mechanism, since a demographic majority cannot otherwise be secured. They have no seat in the diplomatic negotiations that determine their political future.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Fellahin cultivating land later purchased by Jewish National Fund agencies from absentee landlords; they are frequently evicted once land changes hands, since the buyers' purpose is Jewish settlement rather than continuity of existing tenancy. They have no legal standing in the transactions that dispossess them and no practical means of relocation given the regional land market's constriction.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_peasant_tenants, payer,
    powerless, biographical, trapped, local).

% Holds the Mandate and issues the Balfour Declaration's implementing framework, adjudicating (and periodically restricting) Jewish immigration and land transfer through White Papers. It can and does change policy in response to Arab and Jewish pressure and its own imperial strategic calculus, giving it far greater exit and leverage than either population it governs.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandatory_authority, observer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandatory_authority, agenda_setter).

% Great powers (Ottoman Empire, then Britain and France) whose charters and declarations the political Zionist program depends upon, but who negotiate primarily among themselves and with Zionist leadership rather than with the Arab population whose territory is being allocated. Palestinian Arab political representatives are structurally excluded from the diplomatic instruments (Balfour Declaration, Sykes-Picot, Mandate terms) that determine the territory's disposition.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, ottoman_and_successor_diplomatic_powers, excluded,
    institutional, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coordinated diplomatic and organizational vehicle for persecuted diaspora Jewish populations to pursue sovereign statehood as a structural remedy to statelessness — pooling political lobbying, immigration funding, and land purchase into a single movement rather than leaving refuge-seeking fragmented and undirected.
% TRANSFER_FUNCTION: Moves land, political sovereignty, and demographic control from the existing Palestinian Arab population to an incoming Jewish settler population and its diplomatic leadership, financed by diaspora capital and legitimated by great-power charters negotiated without Arab participation.
% ABSENT_VOICES: Palestinian Arab landowners, tenant cultivators, and political representatives are not party to the Balfour Declaration, the Mandate terms, or the Jewish Agency's internal strategic deliberations, despite being the population whose numerical majority the program explicitly identifies as the central obstacle to be overcome.
% DISAPPEARANCE_RATIONALE: Absent the political program's specific commitment to territorial sovereignty with a Jewish demographic majority, diaspora Jewish organizing could have pursued (and in sibling readings did pursue) cultural autonomy, diaspora nationalism, or socialist settlement without statehood as the organizing telos — the land-purchase and immigration-quota apparatus, and the transfer question it generates, would not take the specific shape they took.
% FOUNDING_PROBLEM: Legal emancipation in Europe had not ended antisemitic violence or exclusion (pogroms, the Dreyfus affair, exclusion from professions and citizenship in practice); Herzl and successors concluded that only sovereign territorial statehood, not civic equality within existing states, could resolve Jewish vulnerability to persecution.
% FOUNDING_PROBLEM_CORROBORATION: Zionist leadership and diaspora refugee testimony (particularly post-1933) attest the founding problem remained acutely live. Cultural Zionists (Ahad Ha'am and successors) and Jewish anti-Zionist or non-Zionist voices of the same era attest the problem was real but contested whether territorial sovereignty with demographic majority was the necessary solution rather than one solution among several; Palestinian Arab political leadership of the period, external to the movement, contested the framing that their majority status constituted 'the problem' the program needed to solve.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored as substantial (0.68 by 1948) and rising across the interval because the demographic-majority requirement is not a byproduct of settlement but the organizing objective — land purchase, immigration policy, and eventually transfer planning are all instrumentalized toward it, and the cost lands concretely on Palestinian tenant cultivators and, at the political level, on the Arab population's capacity to remain the demographic majority in its own territory. Suppression rises sharply after the Balfour Declaration (1917) and through the Mandate period as the diplomatic and immigration apparatus hardens into active enforcement (land transfer restrictions on both sides, para-military organization, British mandate policing) — this is a genuine enforcement ratchet, not merely rising extraction, so suppression_requirement is tracked as its own series. Theater ratio stays low throughout: the coordination function (organizing refuge for a genuinely persecuted population) is real and substantially non-performative, which is precisely why this reading computes as tangled_rope rather than snare — a coordination function coexists with the asymmetric extraction rather than serving as its cover story.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jews facing persecution are declared beneficiaries with essentially no exit (trapped, powerless) — the movement's legitimacy rests on their genuine need, and directionality for this group sits toward the subsidized end structurally even though they hold no strategic power. Zionist political leadership and the yishuv settler population are beneficiaries with real organizational power and mobile-to-arbitrage exit (leadership can shift diplomatic venues; settlers administer land and institutions on the ground). Palestinian Arabs — both the general population and specifically the peasant tenants displaced by land transactions — are the structural targets: the program's demographic-majority objective is defined in terms of their political and numerical subordination, and their exit options are trapped (no comparable diplomatic or economic leverage, no alternative territory, no seat in the negotiating instruments). The British Mandatory Authority sits outside the beneficiary/victim axis proper but functions as an inconsistent gatekeeper whose policy oscillations (Balfour, White Papers) shape how much extraction the constraint can enact at any given time.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — European antisemitism as an unsolvable civic-emancipation failure — was real and remained live through 1948 (worsening catastrophically after 1933), so this is not a case of an obsolete mandate persisting past its function; founding_problem_status is authored 'contested' only because the SOLUTION's specific form (territorial sovereignty requiring demographic majority, as opposed to cultural autonomy or diaspora nationalism) is what is disputed, not the underlying problem. This prevents the classification from either (a) treating the whole program as pure extraction because it produced Arab displacement, which would erase the genuine and urgent coordination function it served for a persecuted population, or (b) treating it as pure coordination because the underlying need was genuine, which would erase the asymmetric cost imposed on a population that never consented to being treated as an obstacle. Tangled Rope holds both facts as required by its gate: real coordination (refuge for a stateless, persecuted population) AND asymmetric extraction (systematic displacement and political subordination of the existing population), sustained by active enforcement (land-transfer law, immigration quotas, militia organization) rather than by voluntary agreement from all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_as_necessary_versus_contingent_mechanism,
    'Was population transfer (of the existing Arab population) a necessary logical entailment of the political Zionism program''s demographic-majority requirement, or a contingent policy option some leaders endorsed and others rejected within the same reading?',
    'Close reading of internal Jewish Agency planning documents, Peel Commission testimony, and correspondence across the 1920s-1940s to establish whether transfer was treated as structurally required once demographic majority was fixed as the goal, or as one option debated against alternatives (binational state, cantonization, continued minority status with political guarantees).',
    'If transfer was structurally necessary given the majority requirement, the extraction and victim declarations in this story understate the severity of what the program''s own logic entailed. If transfer was genuinely contingent and rejected by significant factions within this same reading, the story''s extractiveness score may overstate the program''s uniformity and should be split further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_as_necessary_versus_contingent_mechanism, conceptual, 'Whether transfer was entailed by or merely compatible with the demographic-majority objective.').

omega_variable(
    sibling_reading_boundary_political_vs_labor,
    'Where exactly does the political_zionism_reading''s diplomatic-statist strategy end and the labor_zionism_reading''s settlement-first strategy begin, given that in practice the same institutions (Jewish Agency, Jewish National Fund) pursued both simultaneously through most of the Mandate period?',
    'Track institutional funding allocation and strategic priority-setting within the Jewish Agency across the 1920s-1940s to determine whether diplomatic lobbying or ground settlement was treated as primary at different junctures, and whether the two readings'' agents substantially overlap or diverge.',
    'If the two readings'' agent populations and mechanisms overlap heavily in practice, some of this story''s extraction attribution may need to be shared with or redirected toward the labor_zionism_reading sibling constraint, changing the relative ε each carries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_political_vs_labor, conceptual, 'Institutional overlap between the political and labor Zionism readings complicates clean attribution of extraction.').

omega_variable(
    great_power_charter_legitimacy,
    'Does the Balfour Declaration and subsequent Mandate framework constitute a legitimate international legal instrument establishing Jewish claims, or an act of imperial disposition of territory whose Arab population was never consulted and therefore an illegitimate foundation regardless of the underlying humanitarian need it responded to?',
    'This is not empirically resolvable; it depends on which theory of territorial and political legitimacy (self-determination as of the negotiation date vs. self-determination as retroactively defined vs. great-power prerogative) one adopts.',
    'Under a self-determination-of-existing-population framework, the suppression and accessibility_collapse scores would likely be authored higher, since the diplomatic instrument itself is illegitimate ab initio. Under a humanitarian-necessity or realist-diplomacy framework, the same instrument is read as a defensible, if imperfect, response to an urgent crisis, supporting the current moderate accessibility_collapse score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_charter_legitimacy, preference, 'Legitimacy of the Balfour/Mandate diplomatic instrument depends on contested theories of self-determination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.12).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1929, 0.14).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.16).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1939, 0.17).
narrative_ontology:measurement(jewi_tr_t1945, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1945, 0.17).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.18).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.35).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.5).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1929, 0.58).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.64).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1939, 0.66).
narrative_ontology:measurement(jewi_be_t1945, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1945, 0.66).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.2).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.35).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1929, 0.48).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1936, 0.58).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1939, 0.62).
narrative_ontology:measurement(jewi_su_t1945, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the jewish_territorial_claim kernel. political_zionism_reading occupies the diplomatic-statist center of the kernel space: it shares the labor_zionism_reading's commitment to eventual sovereignty (unlike cultural_zionism_reading) but pursues it primarily through great-power diplomacy rather than settlement-first fact-building, and it shares no premise with revisionist_zionism_reading's maximalist both-banks claim or compelled-acceptance doctrine. Each sibling carries its own ε, beneficiary/victim structure, and classification; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
