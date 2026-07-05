% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Post-Zionist Reading: Ethnic-National Framework as Obstacle to Civic Equality
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the post-Zionist reading of the
 *   jewish_sovereignty_palestine kernel: the Zionist project's founding
 *   coordination function (refuge from statelessness and genocide) was
 *   genuinely achieved through statehood, but the ethnic-national
 *   institutional architecture built to achieve it — the Law of Return's
 *   asymmetric citizenship pathway, differential land administration through
 *   quasi-state Jewish national bodies, and the 2018 Basic Law defining the
 *   state constitutionally as the nation-state of the Jewish people
 *   specifically — has outlived the acute survival emergency that justified
 *   it and now functions as an ongoing structure of ethnic privilege that
 *   obstructs civic equality for Palestinian citizens and blocks regional
 *   integration. This is a distinct constraint from the
 *   liberal_nationalist_reading (which holds the same framework as an ongoing
 *   legitimate exercise of self-determination, not an obstacle), the
 *   settler_colonial_reading (which denies any legitimate coordination
 *   function ever existed and reads the entire project as a displacement
 *   regime from inception), the religious_zionist_reading (theological
 *   grounding, no post-facto obsolescence claim), and the
 *   cultural_zionist_reading (which never required statehood or demographic
 *   dominance at all, so has no 'framework outlived its function' structure).
 *   The ε here (0.62) reflects a claim of moderate-to-high but not maximal
 *   extraction: coordination genuinely occurred and was not merely cover, but
 *   its continuation now imposes real, identifiable, asymmetric costs on
 *   Palestinian citizens and occupied populations.
 *
 * KEY AGENTS:
 *   - jewish_israeli_citizens: primary beneficiary (organized/mobile) — full civic-political access and land privilege
 *   - jewish_diaspora_via_law_of_return: beneficiary (moderate/arbitrage) — automatic citizenship pathway unavailable to Palestinians
 *   - palestinian_citizens_of_israel: primary payer (moderate/trapped) — formal citizenship, structurally subordinated national status
 *   - occupied_west_bank_palestinians: primary payer (powerless/trapped) — no citizenship, no vote, controlled by the same state
 *   - gaza_residents: payer (powerless/trapped) — blockade and campaigns downstream of the founding security doctrine
 *   - israeli_state_institutions: agenda_setter (institutional/arbitrage) — administers and could revise the framework
 *   - post_zionist_scholars_and_activists: observer/excluded — analytical seat marginalized in mainstream discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.62).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.58).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Post-Zionist Reading: Ethnic-National Framework as Obstacle to Civic Equality").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'c31eb3a1-2413-4a0a-b17a-e976500bd723').
narrative_ontology:cs_kernel_codification('c31eb3a1-2413-4a0a-b17a-e976500bd723', distributed).
narrative_ontology:cs_authority_grounding('c31eb3a1-2413-4a0a-b17a-e976500bd723', distributed).
narrative_ontology:cs_reading_relation('c31eb3a1-2413-4a0a-b17a-e976500bd723', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('c31eb3a1-2413-4a0a-b17a-e976500bd723', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('c31eb3a1-2413-4a0a-b17a-e976500bd723', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c31eb3a1-2413-4a0a-b17a-e976500bd723', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('c31eb3a1-2413-4a0a-b17a-e976500bd723', foundational, founding_coordination_was_real_but_has_expired).
narrative_ontology:cs_axiom_status(founding_coordination_was_real_but_has_expired, holdable).
narrative_ontology:cs_axiom_grounding('c31eb3a1-2413-4a0a-b17a-e976500bd723', founding_coordination_was_real_but_has_expired, empirically_contingent).
narrative_ontology:cs_axiom('c31eb3a1-2413-4a0a-b17a-e976500bd723', secondary, ethnic_national_framework_severable_from_jewish_safety).
narrative_ontology:cs_axiom_status(ethnic_national_framework_severable_from_jewish_safety, holdable).
narrative_ontology:cs_axiom_grounding('c31eb3a1-2413-4a0a-b17a-e976500bd723', ethnic_national_framework_severable_from_jewish_safety, instrumental).
narrative_ontology:cs_reference_frame('c31eb3a1-2413-4a0a-b17a-e976500bd723', post_1948_refuge_state_framework).
narrative_ontology:cs_drift_state('c31eb3a1-2413-4a0a-b17a-e976500bd723', post_2018_basic_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c31eb3a1-2413-4a0a-b17a-e976500bd723', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_diaspora_via_law_of_return).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_residents).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, state_founding_narratives_can_outlive_their_founding_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold automatic access to citizenship, land allocation through quasi-state bodies historically tied to Jewish national institutions, and full civic-political participation. Most do not experience the ethnic-national framework as constraining; many regard it as the settled basis of a state they consider legitimate and necessary for collective security after historical persecution.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens, beneficiary,
    organized, generational, mobile, national).

% Any Jewish person worldwide can claim automatic citizenship and immediate land/housing access rights under the Law of Return, a pathway categorically unavailable to Palestinians with generational ties to the same territory, including refugees with documented pre-1948 residence.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_diaspora_via_law_of_return, beneficiary,
    moderate, biographical, arbitrage, global).

% Hold formal citizenship but experience systematic asymmetries in land allocation (much state land is administered through Jewish National Fund arrangements from which they are effectively excluded), municipal budgeting, and symbolic exclusion from a state that defines itself in nation-state law as the nation-state of the Jewish people specifically. Voting and legal recourse exist but the constitutional-symbolic order structurally subordinates their national identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, trapped, national).

% Live under military administration without citizenship or vote in the state that controls movement, land use, and resource access in the territory they inhabit, while adjacent settlements populated under the same Law of Return framework enjoy full civil law and voting rights. Exit is not available; residency itself is precarious.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_west_bank_palestinians, payer,
    powerless, generational, trapped, regional).

% Live under blockade and periodic military campaigns justified in part by security doctrines tied to the state's founding demographic and territorial project. Movement in and out is controlled externally; no reciprocal claim to sovereignty or resource access exists.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_residents, payer,
    powerless, immediate, trapped, regional).

% Regional normalization and integration processes are repeatedly destabilized by the unresolved Palestinian question, which is itself downstream of the ethnic-national framework. These states have interests in stable regional integration but are not parties to Israel's internal constitutional structure and can only apply external pressure or incentive.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_arab_states, excluded,
    institutional, generational, constrained, regional).

% Administers land law, citizenship law, and the 2018 Basic Law defining the state's character as the nation-state of the Jewish people. Sets and enforces the boundary between who receives the beneficiary track and who receives the constrained track; could revise the framework toward a civic-national or binational model but bears no proportionate cost from the status quo.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Israeli and diaspora intellectuals, historians, and civil society groups who argue the founding ethnic-national framework has outlived any defensive necessity and now actively obstructs civic equality and regional peace. They publish, litigate, and organize but are marginalized in mainstream political discourse and sometimes face professional or social sanction for the position.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, post_zionist_scholars_and_activists, observer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, post_zionist_scholars_and_activists, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original Zionist project coordinated Jewish collective self-defense and refuge after centuries of persecution culminating in genocide, solving a genuine and historically urgent problem of statelessness and vulnerability to violence.
% TRANSFER_FUNCTION: The ethnic-national legal and institutional framework moves land access, citizenship privilege, and symbolic national recognition toward Jewish citizens and eligible diaspora, and moves land, movement rights, and equal civic-national status away from Palestinian citizens and occupied populations.
% ABSENT_VOICES: Palestinian refugees outside the territory (1948 and 1967 displaced populations) have no seat in Israeli domestic deliberation at all; Palestinian citizens of Israel have formal voice but structurally subordinated influence over the constitutional-symbolic order itself.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework (Law of Return asymmetry, nation-state Basic Law, differential land administration) were removed overnight and replaced with a civic-national or binational framework, land allocation, citizenship eligibility, and constitutional self-definition would all have to be renegotiated; regional normalization dynamics tied to the Palestinian question would shift substantially; a large population currently excluded from full civic equality would gain standing.
% FOUNDING_PROBLEM: Jewish statelessness and vulnerability to genocidal violence in a world where no state offered reliable refuge or protection, resolved through establishment of a state defined explicitly as a Jewish national home with guaranteed refuge via the Law of Return.
% FOUNDING_PROBLEM_CORROBORATION: Liberal-nationalist and religious-Zionist voices attest the founding problem remains live given continued antisemitism and regional hostility. Post-Zionist scholars, a substantial minority of Israeli historians (the 'New Historians' tradition), international human rights bodies, and Palestinian civil society attest from outside the beneficiary group that statehood has been durably achieved and the ethnic-privilege architecture now functions primarily to maintain demographic and territorial advantage rather than to solve an active survival problem.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) is authored moderate-to-high, not maximal, because the constraint retains a genuine historical coordination residue (statehood as refuge was a real solved problem) alongside a substantial and growing extractive component (asymmetric land and citizenship architecture). Suppression (0.58) reflects legal/institutional enforcement of the citizenship and land asymmetries plus military administration of occupied populations, though it is not total — Palestinian citizens retain courts, votes, and some political organization. Theater ratio rises over the interval (0.15 to 0.42) reflecting the post-Zionist claim that increasing performative invocation of 'security necessity' and 'demographic threat' rhetoric has displaced the original acute survival justification as the state matured and consolidated. Accessibility collapse (0.5) is moderate: alternative civic-national or binational frameworks are conceivable and actively argued by named seats in this story, so alternatives have not collapsed as completely as in a mountain-type constraint. Resistance (0.72) is high, reflecting substantial organized pushback from Palestinian citizens, international human rights bodies, and post-Zionist scholars against the framework's continuation.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli state institution seat, the framework is coordination successfully defending a hard-won and still-necessary collective good. From the occupied Palestinian seat, the same legal architecture is experienced as extraction with no coordination benefit reaching them at all. The post-Zionist reading's distinguishing claim is that this gap is no longer justified by the original emergency — the engine's per-seat computation should show this divergence rather than resolve it, since resolving it is exactly the contested political question the kernel context exists to preserve.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Israeli citizens and diaspora beneficiaries sit near the full-beneficiary end: automatic legal advantages (Law of Return, land access) with high mobility and low structural cost. Palestinian citizens of Israel sit toward the target end despite holding formal citizenship, because their national status is constitutionally subordinated and their exit options are effectively trapped (leaving means abandoning ancestral land claims with no reciprocal state elsewhere). Occupied West Bank and Gaza populations sit at the extreme target end: no citizenship, no vote, no exit, direct administration by the state whose founding framework this story evaluates. Israeli state institutions occupy the agenda_setter position with the widest degrees of freedom to revise the framework yet bear none of its costs — this asymmetry between administrative power and cost-bearing is the structural core of the tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) preserves the historically real coordination function — Jewish statehood did solve an acute, well-documented survival problem — while still naming the asymmetric extraction that persists after that problem's acute phase passed. Classifying this as a pure snare would erase the genuine founding coordination and the ongoing security concerns some stakeholders still hold as live; classifying it as a rope or mountain would erase the documented, ongoing, unequal costs borne by Palestinian citizens and occupied populations. The founding_problem_status is authored as 'contested' rather than 'dead' precisely to avoid mandatrophy in the other direction — declaring the founding problem categorically resolved would overclaim on behalf of the post-Zionist reading what remains a genuinely disputed empirical and political question among the story's own stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_genuinely_resolved,
    'Has the acute survival emergency that justified the original ethnic-national framework been durably resolved, or does continued regional hostility and antisemitism mean the founding problem remains live in a form that still justifies the framework''s continuation?',
    'Long-run assessment of regional security dynamics, normalization treaty durability, and independent measurement of antisemitic violence trends against the specific claim that statehood-level protection remains necessary versus a civic-national alternative providing equivalent protection.',
    'If the founding problem is genuinely dead, the post-zionist reading''s tangled_rope classification strengthens toward snare (extraction with no live coordination residue). If genuinely still live, the classification should retain more rope character than tangled_rope credits it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genuinely_resolved, conceptual, 'Whether the original survival emergency justifying the ethnic-national framework has actually ended or merely changed form.').

omega_variable(
    kernel_reading_selection_basis,
    'Why was the post_zionist framing selected over the liberal_nationalist, settler_colonial, religious_zionist, or cultural_zionist readings as the reading instantiated in this story, and what would change under each sibling?',
    'This story was generated as an assigned reading within a declared kernel contest (kernel_id: jewish_sovereignty_palestine); the assignment was structural (manifest-driven), not adjudicative — no claim is made that post_zionist is the ''correct'' reading among the five. Under liberal_nationalist_reading, beneficiaries/victims collapse toward a single legitimate self-determination story with no obstruction claim. Under settler_colonial_reading, the coordination_function answer would be denied entirely (no genuine coordination ever occurred) and extractiveness would be authored higher. Under religious_zionist_reading, the entire secular-obsolescence structure this story relies on would not apply. Under cultural_zionist_reading, there would be no ethnic-national statehood apparatus to critique at all.',
    'Selecting a different reading produces a structurally different constraint (different ε, different beneficiary/victim sets, different claimed_type) rather than a different measurement of the same constraint — this is the ε-invariance principle applied at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Documents that this story is one of five declared sibling readings of a single contested kernel, generated per manifest assignment.').

omega_variable(
    de_zionization_feasibility,
    'Is de-Zionization of state institutions (removing the ethnic-national legal architecture while preserving Jewish safety and collective identity) a coherent, achievable policy path, or does it require dismantling protections that remain genuinely necessary?',
    'Comparative institutional analysis of civic-national states with strong minority protections for historically persecuted groups (e.g., post-apartheid constitutional design, other multi-national federations) applied to the specific security and demographic conditions of Israel/Palestine.',
    'If feasible without loss of genuine protection, the extractive component of the current framework is more clearly severable and the tangled_rope''s victim-cost claim strengthens. If infeasible, part of what this story counts as extraction may be irreducible coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(de_zionization_feasibility, preference, 'Whether the post-Zionist reform program is a live institutional possibility or a value-laden aspiration without a feasible mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jewi_tr_t1980, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(jewi_tr_t2010, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.48).
narrative_ontology:measurement(jewi_be_t1980, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(jewi_be_t2010, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewi_su_t1980, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.48).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(jewi_su_t2010, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__post_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings decomposed from the natural-language label 'the Zionist project' / 'Jewish sovereignty in Palestine,' per the ε-invariance principle: the label conflates structurally distinct claims (theological entitlement, secular self-determination right, settler-colonial displacement, obsolete-but-real coordination, and cultural-renaissance-without-statehood) that carry different ε values, different beneficiary/victim sets, and different classifications. Each sibling is authored as its own constraint file and linked here via affects_constraints. The post_zionist_reading's distinguishing structural claim is temporal-obsolescence: it grants the liberal_nationalist_reading's coordination premise as historically real, then argues the institutional means have outlived their justification — a move unavailable to settler_colonial_reading (denies the premise ever held) or religious_zionist_reading (grounds the claim outside secular obsolescence entirely).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
