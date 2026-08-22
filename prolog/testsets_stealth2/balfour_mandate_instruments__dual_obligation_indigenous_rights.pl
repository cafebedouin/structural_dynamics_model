% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Palestine Mandate Dual Obligation — Indigenous Rights Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   Under the dual-obligation reading, the Mandate instruments for Palestine
 *   bind the administering power to protect the existing Arab population's
 *   civil and political rights and land tenure on equal or superior footing
 *   with the commitment to develop a Jewish national home, and they
 *   subordinate the national-home program to self-determination and
 *   minority-protection norms. The operative structure this reading
 *   instantiates is the protective-restrictive apparatus: land-transfer
 *   ordinances shielding Arab occupancy, immigration ceilings capping
 *   demographic change, and the — never delivered — obligation to move the
 *   territory toward representative government for its majority. The
 *   apparatus performed a real coordination function (holding an agrarian
 *   society intact under a colonization program its majority rejected) while
 *   extracting asymmetrically: Zionist institutions were blocked from the
 *   land and immigration their project required, and the mandatory power was
 *   pinned between irreconcilable commitments it could satisfy only by
 *   violating one party or the other. This file is one member of a
 *   three-reading family over the same kernel text; the sibling files and the
 *   epsilon differences between them are recorded in kernel_context and the
 *   network note. Metrics are authored descriptively of the arrangement's
 *   actual operation; the claimed type is stated from this reading's seat,
 *   independently of the metrics. KEY AGENTS (by structural relationship): -
 *   palestinian_arab_communities: Primary beneficiary (organized/trapped) —
 *   tenure and demographic position protected; sovereignty deferred; carried
 *   the revolt-suppression costs - palestinian_arab_landowning_elites:
 *   Concentrated beneficiary (organized/constrained) — scarcity rents and
 *   protected holdings; straddled the protection via intermediary sales -
 *   zionist_institutions: Primary target (powerful/arbitrage) — land
 *   acquisition and immigration capped; routed around via intermediaries,
 *   shipborne entry, and great-power diplomacy -
 *   british_mandatory_administration: Agenda-setter and bearer
 *   (institutional/mobile) — wrote and enforced the ordinances; paid in
 *   casualties, expenditure, and standing; exited by terminating the Mandate
 *   - exiled_arab_political_leadership: Excluded seat (moderate/trapped) —
 *   deported during the revolt; absent from the terminal negotiations -
 *   league_permanent_mandates_commission: Analytical observer
 *   (institutional/analytical) — heard petitions, issued observations, held
 *   no enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.74).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.74).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Palestine Mandate Dual Obligation — Indigenous Rights Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, '9a8a424e-8320-4ac6-8d40-35efe913b793').
narrative_ontology:cs_kernel_codification('9a8a424e-8320-4ac6-8d40-35efe913b793', fixed_text).
narrative_ontology:cs_authority_grounding('9a8a424e-8320-4ac6-8d40-35efe913b793', lineage).
narrative_ontology:cs_interpretation_layer_present('9a8a424e-8320-4ac6-8d40-35efe913b793').
narrative_ontology:cs_reading_relation('9a8a424e-8320-4ac6-8d40-35efe913b793', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('9a8a424e-8320-4ac6-8d40-35efe913b793', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('9a8a424e-8320-4ac6-8d40-35efe913b793', foundational, existing_population_protection_superordinate).
narrative_ontology:cs_axiom_status(existing_population_protection_superordinate, holdable).
narrative_ontology:cs_axiom_grounding('9a8a424e-8320-4ac6-8d40-35efe913b793', existing_population_protection_superordinate, deontological).
narrative_ontology:cs_axiom('9a8a424e-8320-4ac6-8d40-35efe913b793', foundational, majority_self_determination_trajectory).
narrative_ontology:cs_axiom_status(majority_self_determination_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('9a8a424e-8320-4ac6-8d40-35efe913b793', majority_self_determination_trajectory, deontological).
narrative_ontology:cs_reference_frame('9a8a424e-8320-4ac6-8d40-35efe913b793', article22_dual_trust_tutelage).
narrative_ontology:cs_drift_state('9a8a424e-8320-4ac6-8d40-35efe913b793', mandate_terminal_phase_1939_1948, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a8a424e-8320-4ac6-8d40-35efe913b793', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_elites).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_institutions).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_covenant_article22_sacred_trust).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, mcmahon_hussein_independence_promise).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_treaty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tenant farmers, townspeople, and villagers whose occupancy and cultivation the land-transfer ordinances shield from purchase-based displacement, and whose demographic majority the immigration ceilings preserve. The same population carried the heaviest costs of the period: the 1936-39 revolt brought military repression, collective fines, house demolitions, and the destruction of much of its armed and political leadership, while the representative institutions invoked on its behalf were never seated. Leaving the country was not a realistic option; the protection and the costs arrived in the same villages.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, payer).

% Urban notable families and rural landlords whose holdings and communal standing the transfer restrictions preserved, and whose political brokerage the protection framework kept indispensable. Restriction raised the scarcity price of Arab-held land, enriching holders inside the closed zones; some families had already liquidated holdings to Zionist buyers before the bans and others found ways to sell through intermediaries afterward, so the elite straddled the protection it publicly demanded.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_elites, beneficiary,
    organized, generational, constrained, regional).

% The Jewish Agency, Zionist Executive, and settlement bodies, for whom the ceilings and zone bans capped the two inputs their project ran on: immigrant arrivals and land title. They answered with routing — purchases through third-party intermediaries where direct transfer was barred, shipborne immigration outside the quotas, and a standing diplomatic campaign in London and later Washington to overturn the restrictions. Their way out of the rules was to go around them rather than leave the territory.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_institutions, payer,
    powerful, generational, arbitrage, global).

% The Colonial Office and the Jerusalem administration, which wrote the land-transfer ordinances, scheduled the immigration quotas, and supplied the police and troops that held the arrangement in place. Every enforcement choice satisfied one party's demand by violating the other's, drawing casualties, expenditure, parliamentary attack, and damage to Britain's standing across the Arab and Muslim world. The administration's exit was sovereign and final: terminate the Mandate and withdraw, which it announced in 1947 and completed in 1948.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, payer).

% National commanders and committee heads deported or driven abroad during and after the revolt — the mufti's circle foremost — barred from returning to Palestine and therefore absent from the 1939 London Conference's decisive rounds and every negotiation that followed. The people with the strongest claim to speak for the protected population were physically removed from the room where the arrangement's final form was decided.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, exiled_arab_political_leadership, excluded,
    moderate, generational, trapped, regional).

% The Geneva body that reviewed the mandatory's annual reports and heard Arab petitions arguing that the protection clauses outranked the national-home clause. It recorded objections, questioned the accredited representatives, and issued observations, but commanded no enforcement power; its archive is the fullest outside record of the protection case being pressed and deferred.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_elites).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds an agrarian society and its tenure system intact while a colonization program its majority rejected proceeds under ceiling and zone rules; prevents open land-race collapse, keeps public order between two national movements, and gives the mandatory a single administrable rule-set for land registration and immigration scheduling.
% TRANSFER_FUNCTION: Moves land-title and entry rights away from Zionist acquisition channels toward preservation of existing Arab holdings and the majority's demographic position; moves security and administrative costs onto the mandatory power; and moves political concession-space toward Arab representation claims — while moving nothing at all down the one channel the protection clauses promised, representative institutions.
% ABSENT_VOICES: The Arab majority's representative voice was never seated: three legislative-council proposals (1922, 1930, 1935) died without an election being held, and after 1937 the national leadership sat in exile or detention. Displaced tenant families from the pre-restriction land sales had no organizational voice at all. The seats that shaped the arrangement's operation were the mandatory, the Zionist institutions, and the Arab elites who remained; the population in whose name the protection clauses stood was structurally absent from every body that administered them.
% DISAPPEARANCE_RATIONALE: Land transfers would have resumed on the open market, shipping and entry would have uncapped, the agrarian protection structure would have dissolved within a season, and both national movements' trajectories — and the territory's eventual borders — rearrange around unconstrained competition. Every named seat's position depends on the arrangement existing.
% FOUNDING_PROBLEM: Between 1915 and 1917 Britain made commitments in opposite directions: encouragement of Arab independence in the Hussein-McMahon correspondence and, in 1917, support for a Jewish national home in Palestine, while the League Covenant assigned Class A mandates a tutelary road to independence. The arrangement was built to hold these incompatible promises together on one territory — administering a national-home program without dispossessing or disenfranchising the existing majority.
% FOUNDING_PROBLEM_CORROBORATION: The King-Crane Commission findings (1919) — an inquiry outside both the British and Zionist parties — recorded the majority's opposition and recommended sharply limiting the national-home program, corroborating that the reconciliation problem was real and unsolved from the outset. The Permanent Mandates Commission petition record shows the protection case pressed for two decades without delivery. Zionist parties denied the equal-obligation premise itself, and British internal memoranda acknowledged the incompatibility while denying it publicly; no party outside the Arab beneficiary set affirms that the problem was ever solved.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the apparatus blocked the national-home project's two core inputs — land title and immigrant arrivals — while delivering only part of its own protective bargain: tenant-security ordinances and the post-1939 transfer zones were real, but the representative-government half was never seated at all, so the extraction landed on Zionists and the mandatory without a fully delivered offsetting dividend. Suppression (0.68) reflects sustained coercive administration rather than total coercion: land registries, immigration schedules, emergency regulations, deportations, and garrison deployments were the apparatus's load-bearing machinery. Theater (0.45) is substantial because a growing share of the protection activity was declaratory — constitutional proposals repeatedly shelved, late-period enforcement increasingly hollow — while the land and immigration controls retained genuine function throughout. Accessibility collapse is moderate (0.45): the option space never closed, since Zionist routing, British withdrawal, and successive partition proposals stayed live. Resistance is high (0.72): the Arab Revolt of 1936-39, the Zionist insurgency and mass unauthorized immigration of 1944-47, and continuous diplomatic warfare met the apparatus from both flanks. The temporal series run on one shared nine-point grid (every tracked metric authored at every examined year). The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: it spikes with the revolt suppression of 1936-39, relaxes under wartime manpower drain in 1942, ratchets again with the post-war immigration crackdowns, and remains high at termination — the apparatus died not because force relaxed but because the political will and legitimacy to apply it collapsed. The oscillation visible in the series (crisis, restriction, relaxation, accumulation) is itself part of the mechanism: each relaxation signaled to both movements that the rules were negotiable, raising the stakes of the next round. The base_properties scalars are the interval-end snapshot and match the 1948 series values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently, and the divergence is the point. From the Zionist seat the apparatus operates as near-pure obstruction of a promised refuge — its coordination story (order, protection) reads as cover for blocking the national home — placing that seat near the extraction pole. From the Arab communal seat the same structure is protection only partially delivered: real tenure shields, no representative institutions, revolt-level costs — closer to an incomplete coordination bargain. From the mandatory's seat it is a double commitment that could be honored only by betraying one party, experienced as a trap it eventually exited by leaving. Coalition dynamics cut across the target set: the two declared targets aligned in practice whenever loosening served them both, and the 1939 London round briefly isolated the Zionist seat behind a London-Arab détente — coalition potential among same-side payers was real and episodically exercised. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. The two Arab seats sit at the beneficiary end: the communities (trapped exit) receive the protection the apparatus exists to deliver, so their derived d is low and effective extraction inverts toward subsidy; the landowning elites (constrained exit) collect the concentrated share — scarcity rents inside the closed zones — placing them nearest the full-beneficiary pole. Zionist institutions are declared victims and derive high d, but their arbitrage-grade exit dampens effective extraction: intermediary purchases, unauthorized shipborne immigration, and patronage-shifting from London to Washington meant the binding never fully bit. The mandatory administration is dual-positioned — agenda_setter running the apparatus, listed among victims because the arrangement consumed its casualties, budget, and international standing — so its derived d sits mid-range rather than at either pole; the secondary_role declaration carries this to the engine. No directionality_overrides are used: the role-plus-exit data differentiates every seat, and the override mechanism keys on power atoms, which would misfire here by dragging the institutional observer (the Mandates Commission) along with the institutional administrator.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are prevented. Reading the apparatus as pure coordination erases the asymmetric extraction: the same ordinances that shielded Arab occupancy priced Zionist institutions out of the land market and capped the immigration their project ran on, and the political half of the protection bargain was never delivered at all. Reading it as pure extraction erases the real protective function: tenant-security ordinances and the post-1939 transfer zones measurably slowed dispossession, and the immigration ceiling held the demographic door partially closed for a decade. The tangled_rope claim keeps both faces on the table. On mandatrophy proper: the founding reconciliation problem was never resolved, but the arrangement did not decay into theatrical maintenance — it was terminated by withdrawal in 1948 while its functions were still partly live, which is death by collapse rather than by outliving its function. The R5 interview returns founding_problem_status=contested against disappearance_verdict=world_rearranges, so no zombie flag fires; the genealogy record shows a problem disputed at its root (one sibling reading denies the equal obligation exists) rather than a mandate silently outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading do the Mandate instruments themselves compel — equal-or-superior protection obligations, national-home primacy, or interpreter discretion?',
    'Drafting-history analysis (Curzon''s 1919 objections, the 1922 Churchill White Paper clarification, the omission of political-rights language from the final text) combined with doctrinal analysis of Covenant Article 22 priority against Mandate Articles 2 and 6, and the Permanent Mandates Commission''s own jurisprudence on the petitions.',
    'Determines which family member''s beneficiary/victim structure is textually grounded; resolving toward primacy inverts this file''s epsilon structure entirely, and resolving toward discretion relocates the constraint out of the texts altogether.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'The kernel contest itself: allocation of the instruments'' meaning among the three readings.').

omega_variable(
    transitional_status_ambiguity,
    'Was the Mandate structurally transitional — Covenant tutelage implying an independence terminus, as realized in Iraq (1932) and Transjordan (1946) — or open-ended?',
    'Comparative Class-A mandate trajectories holding the national-home variable aside, plus Colonial Office papers on the intended terminus and why none was ever scheduled for Palestine.',
    'If transitional, the apparatus is scaffold-shaped and its twenty-six-year persistence without the promised terminus is arrested transition, shifting lifecycle analysis toward a degraded-transitional reading and making the missing sunset clause itself the diagnostic fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_status_ambiguity, empirical, 'Whether the arrangement carried an implicit sunset that was never operationalized.').

omega_variable(
    protection_delivery_asymmetry,
    'Did the apparatus enforce its restrictive half (against Zionist acquisition and entry) more effectively than its protective half (representative government, tenant security)?',
    'Compare land-registry and ordinance enforcement records against the fate of successive constitutional proposals (1922, 1930, 1935 legislative councils, none ever elected), and against eviction and tenancy-complaint volumes.',
    'If the protective half was largely undelivered, the declared beneficiaries collected less than the structure implies, effective directionality for the Arab seats shifts upward, and the coordination-function gate evidence weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protection_delivery_asymmetry, empirical, 'Asymmetry between enforcement of restrictions and delivery of protections.').

omega_variable(
    elite_vs_cultivator_capture,
    'Did tenure protection accrue mainly to cultivator communities or to the landowning elite via scarcity rents and pre-ban liquidations?',
    'Ownership-concentration and land-price series 1929-1947, village-level transfer records, and the documented pre-restriction sales by large owners to Zionist buyers.',
    'Relocates gain_flow between the two Arab seats and modulates how much of the extraction''s receipt concentrates at the elite seat versus diffusing across the communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_vs_cultivator_capture, empirical, 'Which Arab seat actually captured the protection dividend.').

omega_variable(
    epsilon_baseline_sensitivity,
    'Against which counterfactual is epsilon computed — the Ottoman status quo, an unrestricted land and immigration market, or the primacy reading''s promised development trajectory?',
    'Fix the referent to the standing Mandate settlement per the reading-indexed epsilon rule, then document residual sensitivity by recomputing against each candidate baseline.',
    'Epsilon moves on the order of plus or minus 0.1 across baselines without changing the tangled structure; adopting the primacy baseline would belong to the sibling file, not this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_baseline_sensitivity, conceptual, 'Baseline-dependence of the authored epsilon over a fixed referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.24).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1925, 0.26).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.29).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1933, 0.31).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.36).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.28).
narrative_ontology:measurement(balf_tr_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1942, 0.34).
narrative_ontology:measurement(balf_tr_t1945, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1945, 0.4).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.45).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1925, 0.57).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.6).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1933, 0.63).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.67).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.76).
narrative_ontology:measurement(balf_be_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1942, 0.69).
narrative_ontology:measurement(balf_be_t1945, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1945, 0.71).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1925, 0.44).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.5).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1933, 0.54).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.74).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.77).
narrative_ontology:measurement(balf_su_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1942, 0.62).
narrative_ontology:measurement(balf_su_t1945, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% 'The Palestine Mandate' is one colloquial label over three structurally distinct constraints sharing a fixed kernel text. This file (dual_obligation_indigenous_rights) authors epsilon for the standing Mandate settlement as the protection-superordinate reading assesses it: beneficiaries are the Arab population seats, targets are the Zionist institutions and the mandatory. The sibling file jewish_national_home_primacy inverts the beneficiary/victim sets and authors its own epsilon over the same territory and period; mandatory_interpretive_discretion relocates the operative structure from the texts to the interpreter's unreviewable adjudication. The readings differ in epsilon, victim sets, and classification; they are linked as one family through affects_constraints, and the upstream textual claim (what the instruments say) is cited as evidence within each downstream reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
