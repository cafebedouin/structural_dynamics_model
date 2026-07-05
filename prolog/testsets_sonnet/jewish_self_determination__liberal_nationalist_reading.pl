% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination as Liberal-Nationalist Coordination Claim
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the liberal-nationalist reading of the contested
 *   Jewish self-determination kernel: the claim that Jews constitute a nation
 *   entitled, on the same normative grounds as other peoples, to
 *   self-determination — grounded in secular nationalist theory (shared
 *   history, language, persecution, and collective identity) rather than
 *   divine covenant or indigeneity claims. On this reading the constraint
 *   functions as a coordination rope: it seeks to resolve a genuine
 *   allocation problem between two national movements with claims to
 *   overlapping territory, via partition or mutual recognition, and asserts
 *   in principle no victim, since the framework's own premise is that a
 *   parallel, co-equal Palestinian national claim is coordinated rather than
 *   displaced. This is a single reading among five siblings in the kernel
 *   (diasporist_reading, indigenous_return_reading,
 *   religious_covenant_reading, settler_colonial_reading) — each of those is
 *   a structurally distinct constraint with its own ε, beneficiaries, and
 *   victims, not a different observable of this one. Whether the coordination
 *   promise of THIS reading was honored in practice, or the reading serves as
 *   a legitimating gloss over an outcome closer to the
 *   settler_colonial_reading's structure, is the central empirical omega
 *   below, not something resolved within this story.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_seeking_refuge: primary beneficiary (moderate/constrained) — historically persecuted, dispersed population for whom sovereignty functions as refuge guarantee
 *   - israeli_jewish_citizens: beneficiary and agenda-setter (institutional/mobile) — administer the sovereign expression of the claim
 *   - palestinian_national_movement: excluded seat whose parallel claim tests whether the coordination promise holds (organized/trapped)
 *   - international_state_system: analytical observer administering competing recognition claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.32).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.28).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination as Liberal-Nationalist Coordination Claim").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political philosophy / nationalism studies / postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, '57923e11-83d2-4dec-aae8-7f59ee1497ed').
narrative_ontology:cs_kernel_codification('57923e11-83d2-4dec-aae8-7f59ee1497ed', distributed).
narrative_ontology:cs_authority_grounding('57923e11-83d2-4dec-aae8-7f59ee1497ed', distributed).
narrative_ontology:cs_reading_relation('57923e11-83d2-4dec-aae8-7f59ee1497ed', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('57923e11-83d2-4dec-aae8-7f59ee1497ed', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('57923e11-83d2-4dec-aae8-7f59ee1497ed', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('57923e11-83d2-4dec-aae8-7f59ee1497ed', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('57923e11-83d2-4dec-aae8-7f59ee1497ed', foundational, secular_peoplehood_grounds_equal_national_claim).
narrative_ontology:cs_axiom_status(secular_peoplehood_grounds_equal_national_claim, holdable).
narrative_ontology:cs_axiom_grounding('57923e11-83d2-4dec-aae8-7f59ee1497ed', secular_peoplehood_grounds_equal_national_claim, conventional).
narrative_ontology:cs_axiom('57923e11-83d2-4dec-aae8-7f59ee1497ed', foundational, territorial_sovereignty_is_necessary_for_jewish_collective_security).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_necessary_for_jewish_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('57923e11-83d2-4dec-aae8-7f59ee1497ed', territorial_sovereignty_is_necessary_for_jewish_collective_security, instrumental).
narrative_ontology:cs_reference_frame('57923e11-83d2-4dec-aae8-7f59ee1497ed', post_wwi_national_self_determination_principle).
narrative_ontology:cs_drift_state('57923e11-83d2-4dec-aae8-7f59ee1497ed', post_1993_oslo_and_contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('57923e11-83d2-4dec-aae8-7f59ee1497ed', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, national_self_determination_principle).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, peoplehood_criterion_of_shared_history_language_and_persecution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities dispersed across many states, historically subject to expulsion, pogroms, and genocide, for whom a recognized national home functions as a guarantee of last-resort refuge and a locus of collective cultural and political agency. Their claim rests on treating Jews as a people with shared history, language revival, and persecution experience analogous to other national groups recognized under the self-determination principle.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge, beneficiary,
    moderate, generational, constrained, global).

% Exercise sovereignty through a functioning state, administer immigration (Law of Return) and territorial governance, and treat the state as the institutional expression of the national claim. Their exit options are comparatively strong (many hold or can obtain other citizenships), which differentiates their structural position from diaspora communities who lack a sovereign fallback.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens, agenda_setter).

% Advances a competing and, on this reading, structurally parallel national claim to overlapping territory. This reading treats their claim as coordinate rather than foreclosed — the rope function is precisely the coordination problem of reconciling two co-equal national claims through partition or power-sharing. Whether that coordination has actually occurred, and at what cost to this group, is the central omega of this story; the reading's own premise is that no victim is created in principle, which this stakeholder's situation is positioned to test.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, excluded,
    organized, generational, trapped, regional).

% The UN framework and constituent states that recognize (unevenly and contestedly) national self-determination claims generally, and have extended partial, disputed recognition to both Israeli and Palestinian national claims. Adjudicates legitimacy through diplomatic recognition, treaty-making, and international law without direct territorial stake.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_state_system, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled basis — parallel to the self-determination claims of other stateless or historically persecuted peoples — for treating Jewish national aspiration as legitimate and coordinating it with competing claims to the same territory via partition, mutual recognition, or power-sharing arrangements.
% TRANSFER_FUNCTION: In principle, this reading does not describe a resource transfer from a victim group to a beneficiary group — it describes an allocation problem between two co-equal national claims. Whatever de facto transfers of land, sovereignty, or security have occurred are, on this reading's own terms, a separate empirical question about implementation, not a structural feature of the claim itself.
% ABSENT_VOICES: The Palestinian national movement is structurally the loudest absent voice in the pure form of this reading — the reading's coordination story only remains a rope if partition or equivalent power-sharing actually resolves rather than merely narrates their claim. Religious-covenant and indigenous-return proponents within Jewish thought are also sidelined here, since this reading deliberately grounds the claim in secular liberal-nationalist theory rather than divine mandate or indigeneity.
% DISAPPEARANCE_RATIONALE: If the liberal-nationalist claim were simply withdrawn, proponents argue the practical result would be catastrophic exposure of a stateless persecuted people (world_rearranges from their seat); critics of this specific framing argue that other readings (covenant, indigeneity, or diasporism) would simply substitute as the operative justification, leaving underlying facts on the ground largely unchanged (world_unchanged from their seat) — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The founding problem this reading names is the absence of Jewish national sovereignty amid recurring statelessness, expulsion, and, culminating in the Holocaust, genocide — a problem the liberal international order's own self-determination principle was invoked to solve by extending the same normative category used for other national movements.
% FOUNDING_PROBLEM_CORROBORATION: Historians of nationalism and international-law scholars outside the Zionist movement itself (e.g., scholars tracing the League of Nations mandate system and post-WWII refugee and statelessness law) corroborate that statelessness and persecution were live, severe problems this claim responded to. Independent human-rights and international-law bodies, however — genuinely outside the beneficiary group — dispute whether the problem remains 'live' in its founding form or has been transformed into a governance and occupation problem that the original coordination framing does not adequately address; this is contested rather than settled from outside the benefiting parties.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.32 at interval end) because the reading's own structure posits no victim — it is a claim about parallel entitlement, not a claim about resource transfer from an identified victim group. Suppression is authored moderate-low (0.28): the claim itself does not require active coercive suppression of alternatives to be advanced as a normative position, though its application in territorial practice generates contested suppression dynamics that belong more properly to implementation-level constraints, not to this abstract normative reading. Resistance is authored moderate (0.55) because the claim as stated is genuinely and persistently contested by multiple other traditions (religious, anti-Zionist diasporist, and Palestinian national) even though the claim itself, as an abstract equal-entitlement proposition, is not obviously coercive.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities and Israeli citizens sit toward the beneficiary end: the claim, if honored by the international order, subsidizes their collective security and political agency. The Palestinian national movement is deliberately NOT declared a victim on this reading's own terms — the reading's structural premise is that partition/coordination resolves the claim without creating a victim. This is exactly the premise the omega below interrogates: whether that coordination premise holds empirically, or whether the theoretical rope-status masks accumulated tangled-rope or snare dynamics documented instead in the settler_colonial_reading sibling constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (statelessness and persecution culminating in genocide) is treated as historically live at founding and contested as to present status — this prevents the story from either dismissing the claim as obsolete or treating it as permanently and uncritically vindicated. The founding_problem_status is authored 'contested' rather than 'live' or 'dead' because corroboration diverges sharply between historians of the founding period (who affirm the problem was real and severe) and contemporary human-rights observers (who argue the operative problem today is a governance/occupation dynamic not addressed by the original coordination framing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_promise_fulfillment,
    'Has the liberal-nationalist reading''s coordination premise (two co-equal national claims resolved via partition or power-sharing) actually been realized, or does the historical record show one claim consistently subordinating the other through territorial and legal mechanisms — in which case the operative constraint is structurally closer to the settler_colonial_reading sibling?',
    'Comparative analysis of partition plan implementation history, territorial control trajectories, and the legal status of Palestinian residents under different jurisdictions, cross-referenced against the settler_colonial_reading and indigenous_return_reading sibling stories'' own metrics.',
    'If the coordination premise has not been realized, this reading''s claimed rope classification would be exposed as a normatively attractive but empirically unfulfilled framing, and the network edge to the settler_colonial_reading sibling becomes the operative account of actual outcomes; if realized, the rope classification stands as descriptively accurate rather than merely aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_promise_fulfillment, empirical, 'Whether the partition/coordination premise central to this reading has been empirically honored.').

omega_variable(
    peoplehood_criterion_contestation,
    'Is ''shared history, language revival, and persecution experience'' a sufficient and non-arbitrary criterion for national peoplehood entitled to self-determination, or is the criterion itself contestable in ways that make the equal-claim premise question-begging?',
    'Comparative political theory analysis of how the self-determination principle has been applied (or denied) to other diasporic, religious, or historically dispersed groups making analogous claims, to test whether the criterion is applied consistently.',
    'If the criterion is applied inconsistently across cases, the ''equal claim'' framing central to this reading''s title is weakened as a matter of principle, independent of any empirical partition question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peoplehood_criterion_contestation, conceptual, 'Whether the peoplehood criterion grounding equal self-determination claims is applied consistently or ad hoc.').

omega_variable(
    kernel_framing_selection,
    'Why treat the liberal-nationalist framing (secular peoplehood + parallel claims) as the primary reading rather than the indigenous_return_reading (indigeneity/decolonization) or the religious_covenant_reading (divine mandate), given that all three are simultaneously invoked by different constituencies within the same political movement?',
    'Textual and historical analysis of Zionist movement discourse across its factions (political Zionism vs. religious Zionism vs. cultural Zionism) to determine which framing predominates in which institutional contexts (diplomacy vs. religious law vs. settlement policy).',
    'Different framings ground different classifications: the liberal-nationalist reading computes as a low-ε rope; the indigenous_return_reading and religious_covenant_reading would carry different beneficiary/victim structures and likely different ε values, since they invoke different legitimating logics with different exposure to counter-claims. This is exactly the situation the kernel/reading architecture is built to handle — each framing is authored as its own sibling constraint rather than blended into one story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Under-determination among plausible framings of the same underlying political claim, each producing a different sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t13, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 13, 0.12).
narrative_ontology:measurement(jewi_tr_t27, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 27, 0.15).
narrative_ontology:measurement(jewi_tr_t47, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 47, 0.18).
narrative_ontology:measurement(jewi_tr_t63, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 63, 0.19).
narrative_ontology:measurement(jewi_tr_t76, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 76, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jewi_be_t13, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 13, 0.26).
narrative_ontology:measurement(jewi_be_t27, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 27, 0.3).
narrative_ontology:measurement(jewi_be_t47, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 47, 0.31).
narrative_ontology:measurement(jewi_be_t63, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 63, 0.32).
narrative_ontology:measurement(jewi_be_t76, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 76, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__liberal_nationalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the colloquial label 'the Jewish claim to self-determination / Zionism' under the ε-invariance principle. Each sibling instantiates a structurally distinct claim with its own beneficiary/victim structure and ε: liberal_nationalist_reading (this story, rope, low-moderate ε, no victim in principle), indigenous_return_reading (decolonization framing), settler_colonial_reading (colonization framing, victims named), religious_covenant_reading (divine-mandate framing, independent of secular political legitimacy), and diasporist_reading (rejects territorial sovereignty as the solution, treating this reading's core premise as foreclosed rather than merely competing). The diasporist_reading is marked 'forecloses' from this reading's side because the liberal-nationalist claim's foundational axiom — that territorial sovereignty is necessary for Jewish collective security — is logically incompatible with the diasporist claim that diaspora pluralism, not sovereignty, is the correct and safer vehicle for collective flourishing; a single coherent framework cannot hold both as its organizing premise, though different real-world parties do hold each. The other three readings coexist with or are distinguishable framings that this reading does not logically rule out, even though they compete with it for descriptive accuracy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
