% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine-Covenant Title Claim Binding Territorial Sovereignty (Religious-Covenant Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story authors ONE reading of the jewish_self_determination kernel:
 *   the claim that Jewish title to the land derives from divine covenant,
 *   making territorial sovereignty a religious obligation independent of
 *   secular political frameworks. The epsilon referent is the standing
 *   arrangement under contest - the operative regime in which covenant
 *   interpretation disciplines territorial policy through state power
 *   (settlement administration, coalition politics, delegitimization of
 *   compromise) - assessed from the analytical seat, never the arrangement
 *   this reading would endorse. Within the believing framework the covenant
 *   presents as immutable divine command, a mountain phenomenology of zero
 *   freedom; operationally the arrangement is maintained by identifiable
 *   institutions, collects identifiable gains, and imposes identifiable costs
 *   on non-assenting parties, which is the structure authored here. The
 *   sibling readings (liberal_nationalist_reading, indigenous_return_reading,
 *   settler_colonial_reading, diasporist_reading) are separate constraints
 *   with their own epsilon values and beneficiary/victim structures, linked
 *   via network.affects_constraints. Interval mapping: T0 = 1967
 *   (post-Six-Day-War ignition of covenant politics), T57 = 2024.
 *
 * KEY AGENTS:
 *   - religious_zionist_rabbinic_leadership: Agenda setter (institutional/identity_locked) - interprets the covenant, defines territorial obligation, commands the movement
 *   - settlement_enterprise_residents: Primary beneficiary (organized/constrained) - receives land, subsidies, protection, theological legitimation
 *   - palestinian_residents_of_occupied_territories: Primary target (powerless/trapped) - bears displacement, expropriation, and total foreclosure of standing
 *   - israeli_secular_compromise_advocates: Secondary target (moderate/constrained) - negotiating position delegitimized as betrayal
 *   - israeli_state_apparatus: Enforcement arm and dual bearer (institutional/constrained) - administers the claim while carrying its diplomatic and security costs
 *   - diaspora_jewish_institutions: Secondary beneficiary (powerful/mobile) - funds and legitimizes, absorbs reputational costs
 *   - international_legal_institutions: Excluded adjudicator (institutional/constrained) - foreclosed by the claim's independence-from-secular-frameworks clause
 *   - political_theory_analysts: Analytical observer - sees the full structure, bears none of it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.73).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.8).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.73).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine-Covenant Title Claim Binding Territorial Sovereignty (Religious-Covenant Reading)").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, 'dd2c7748-be3b-4800-8741-02985c2b5c4f').
narrative_ontology:cs_kernel_codification('dd2c7748-be3b-4800-8741-02985c2b5c4f', fixed_text).
narrative_ontology:cs_authority_grounding('dd2c7748-be3b-4800-8741-02985c2b5c4f', lineage).
narrative_ontology:cs_interpretation_layer_present('dd2c7748-be3b-4800-8741-02985c2b5c4f').
narrative_ontology:cs_reading_relation('dd2c7748-be3b-4800-8741-02985c2b5c4f', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd2c7748-be3b-4800-8741-02985c2b5c4f', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd2c7748-be3b-4800-8741-02985c2b5c4f', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('dd2c7748-be3b-4800-8741-02985c2b5c4f', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('dd2c7748-be3b-4800-8741-02985c2b5c4f', foundational, land_title_derives_from_divine_covenant).
narrative_ontology:cs_axiom_status(land_title_derives_from_divine_covenant, holdable).
narrative_ontology:cs_axiom_grounding('dd2c7748-be3b-4800-8741-02985c2b5c4f', land_title_derives_from_divine_covenant, theological).
narrative_ontology:cs_axiom('dd2c7748-be3b-4800-8741-02985c2b5c4f', foundational, territorial_sovereignty_is_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('dd2c7748-be3b-4800-8741-02985c2b5c4f', territorial_sovereignty_is_religious_obligation, theological).
narrative_ontology:cs_axiom('dd2c7748-be3b-4800-8741-02985c2b5c4f', secondary, secular_frameworks_cannot_adjudicate_covenant_title).
narrative_ontology:cs_axiom_status(secular_frameworks_cannot_adjudicate_covenant_title, holdable).
narrative_ontology:cs_axiom_grounding('dd2c7748-be3b-4800-8741-02985c2b5c4f', secular_frameworks_cannot_adjudicate_covenant_title, theological).
narrative_ontology:cs_reference_frame('dd2c7748-be3b-4800-8741-02985c2b5c4f', eternal_covenantal_land_grant).
narrative_ontology:cs_drift_state('dd2c7748-be3b-4800-8741-02985c2b5c4f', contemporary_post_1967_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dd2c7748-be3b-4800-8741-02985c2b5c4f', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_rabbinic_leadership).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise_residents).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, diaspora_jewish_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_occupied_territories).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, israeli_secular_compromise_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, diaspora_jewish_institutions).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, biblical_land_promise_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, halakhic_settlement_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshiva heads, the Chief Rabbinate, and movement rabbis who interpret the biblical land promise and rule on whether territory may be ceded. They train the settler leadership cadre, certify the theological legitimacy of state actions, and mobilize voters and youth movements. Their authority exists only inside the covenant framework; stepping outside it would dissolve the basis of their office.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Several hundred thousand Israelis living in settlements across the West Bank. They receive land allocation, housing subsidies, tax benefits, army protection, and road infrastructure justified theologically as fulfilling the promise. Leaving would mean abandoning homes, communities, and a life project; the 2005 Gaza withdrawal showed exit is possible but at severe personal and communal cost.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise_residents, beneficiary,
    organized, biographical, constrained, regional).

% Palestinians in the West Bank and East Jerusalem whose towns and farmland lie inside the area the covenant claim covers. They live under military administration, face settlement expansion onto their land, movement restrictions, and home demolitions. They have no vote in the state administering the claim and no standing inside the theological framework that justifies it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_occupied_territories, payer,
    powerless, generational, trapped, regional).

% Israeli politicians, jurists, and activists who favor territorial compromise or a two-state outcome. Each election cycle they compete against a bloc that frames ceding land as violating a divine promise, which narrows which platforms are electable and marks compromise as betrayal. Their options are shifting coalitions, retiring from politics, or emigrating.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, israeli_secular_compromise_advocates, payer,
    moderate, biographical, constrained, national).

% Government ministries, the IDF Civil Administration, and land authorities that administer the occupied territories and enforce settlement law. Formally secular, they increasingly implement policies whose operative justification is theological. They also carry the diplomatic isolation, legal exposure, and security burden the settlement project generates.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, israeli_state_apparatus, payer).

% Major North American and European Jewish organizations that fund settlement-adjacent projects and defend the claim internationally. The covenant framing reinforces communal identity and fundraising, but it also divides their memberships and exposes them to reputational cost abroad; they can and do distance themselves from particular policies.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diaspora_jewish_institutions, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, diaspora_jewish_institutions, payer).

% UN bodies, the International Court of Justice, and treaty frameworks that adjudicate occupation law and annexation. The covenant claim declares itself independent of exactly these secular frameworks, so their rulings are met with theological counterclaims they have no purchase on; they continue issuing opinions with limited enforcement leverage.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_legal_institutions, excluded,
    institutional, generational, constrained, global).

% Scholars of nationalism, religion and politics, and postcolonial theory who study how sacred-text claims interact with modern sovereignty. They take no side in the dispute and bear none of its costs; their seat is observational.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, political_theory_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, settlement_enterprise_residents).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates costly, multi-generational commitment by the national-religious community to settlement and land-retention: it answers why individuals should sacrifice comfort, safety, and international standing for territory, sustaining mobilization that purely secular appeals did not achieve in this constituency.
% TRANSFER_FUNCTION: Moves land, housing, budget, and military protection toward the settlement enterprise; moves interpretive authority over territory from courts and parliaments to rabbinic ruling; moves compromise options off the negotiating table, transferring bargaining outcomes toward the maximalist position.
% ABSENT_VOICES: Palestinian residents of the territories are the clearest absent voice: the covenant's beneficiary structure is defined by membership in the covenant, which excludes them by construction, and they hold no seat in the coalition that applies the claim. Anti-Zionist haredi authorities speak in Jewish discourse but stand outside the governing coalition. International legal institutions speak but are answered only from within the framework's own terms.
% DISAPPEARANCE_RATIONALE: Without the covenant claim, settlement loses its theological engine: ceding territory becomes an ordinary policy trade-off, the religious parties' territorial platform loses its binding force, coalition mathematics rearranges, and the conflict's religious dimension collapses into a negotiable political dispute. Identities, budgets, and legal structures built around the claim would all have to reorganize.
% FOUNDING_PROBLEM: Two older problems: preserving communal identity and attachment to the land across two millennia of exile, and - in its modern political form after 1967 - reconciling a Torah-anchored community's allegiance with a secular state's sovereignty over the covenant land.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the benefiting parties is ample: academic historians and scholars of religious Zionism attest the doctrine's founding role and continuing operation; Israeli constitutional lawyers attest its collision with secular legal frameworks; anti-Zionist haredi authorities corroborate the covenant's centrality while disputing the sovereignty conclusion; Palestinian and international legal analyses corroborate its operative role in settlement policy.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.73, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.73 at interval end) because the covenant claim converts a contested theological premise into binding territorial policy: land, budget, and legal protection flow to the settlement enterprise while compromise options are removed from the table for everyone, including parties who reject the premise. Suppression is higher still (0.80) because the arrangement persists by actively closing alternatives - heresy framing of compromise, state enforcement of settlement administration, and the explicit subordination of secular and international frameworks - not by participant preference. Theater is moderate-low (0.32): ceremonial and symbolic invocation is real but the operative function (mobilization, legitimation, administration) is substantive. Accessibility_collapse is 0.60: alternatives collapse nearly completely inside the believing community but persist in the broader polity. Resistance is 0.66: intifadas, international legal pressure, the Israeli peace camp, and internal religious dissent all contest the arrangement. The claim/metric gap is deliberate and structural: the reading PRESENTS itself as mountain (divine command admits no degrees of freedom), while the authored claim is tangled_rope because the arrangement's persistence requires active enforcement and distributes gains and losses asymmetrically - that divergence is data, not error. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point. The suppression_requirement trajectory is an enforcement ratchet (settlement administration buildout, delegitimization machinery, judicial entrenchment), not noise; the shallow dips at T40 reflect the 2005 disengagement shock, after which the arrangement rebounded harder.
 *
 * PERSPECTIVAL GAP:
 *   Within the believing framework the arrangement is experienced as obligation rather than imposition: the rabbinic and settler seats should compute heavily damped burdens, a mountain-like phenomenology in which no alternative is even thinkable. The same structure from the Palestinian resident's seat computes as near-total foreclosure with no exit and no standing, and from the secular compromiser's seat as a steadily shrinking space of electable positions. The state seat splits: it wields the arrangement and pays for it. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options; the interior/exterior divergence is the story's central measurement, and no authored scalar reconciles it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic leadership and settlement residents sit near the beneficiary end - the arrangement subsidizes them with land, authority, and legitimation, and their identity lock deepens the subsidy. Palestinian residents sit nearest the full-target end, amplified by trapped exit, powerless standing, and the regional scope over which the claim operates. Secular compromise advocates sit high-target with constrained exit: the foreclosure operates on their bargaining position rather than their persons. The state apparatus sits mid-range - it administers and enforces the claim yet bears its diplomatic isolation and security costs, which the secondary payer role records. Diaspora institutions sit low-to-mid; their mobility damps the derived directionality despite real reputational costs. International legal institutions are not coordinated by the arrangement but foreclosed by it - the independence-from-secular-frameworks clause is aimed squarely at their adjudication, placing them high-target with no enforcement purchase inside the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandates divide cleanly. The exile-era mandate - preserving identity and attachment across dispersion - remains live wherever the community faces assimilation pressure. The post-1967 mandate - binding a Torah community's allegiance to a secular state's sovereignty - is substantially accomplished. The current expansionary operation exceeds both: it no longer solves the problem it was built for so much as generate returns from its own perpetuation. Calling the arrangement pure coordination erases the foreclosed seats; calling it pure extraction erases the genuine multi-generational commitment it coordinates. The tangled_rope structure keeps both visible, and because the founding problem is still live for part of its constituency, no mandatrophy resolution is declared - the mismatch consumer should watch the expansion function outgrowing the founding one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_constructed_doctrine,
    'Is the covenant claim a genuinely binding divine command - mountain-like within its own metaphysics, with zero degrees of freedom - or a constructed political doctrine maintained by identifiable institutions that collect from it?',
    'Not resolvable by data: it turns on prior metaphysical commitments. Structurally observable proxies include whether the claim''s application tracks rabbinic institutional interest across cases, and whether internal reinterpretation occurs when doctrine and interest diverge.',
    'If the mountain phenomenology is taken at face value, the arrangement reads as obligation with negligible extraction for assenting seats; if constructed, the full tangled_rope or snare profile applies to every seat the claim binds without their assent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_command_vs_constructed_doctrine, conceptual, 'The reading''s mountain self-presentation versus its operationally maintained, beneficiary-bearing structure.').

omega_variable(
    seat_weighting_of_extractiveness,
    'Which seats dominate the constraint''s effective extractiveness - the assenting seats inside the framework (who report obligation, not burden) or the non-assenting seats it binds (Palestinian residents, secular compromisers, international adjudicators)?',
    'Per-seat classification from the declared structural data: compare computed effective extraction for identity-locked assenting seats against trapped and constrained target seats, and examine which seat set determines the aggregate verdict.',
    'Weighting assenting seats drives the aggregate toward rope-like coordination; weighting non-assenting seats drives it toward high-extraction tangled_rope or snare. The scalar epsilon (0.73) already prices the contested-space view; the omega records that the intra-framework view would price it near zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seat_weighting_of_extractiveness, conceptual, 'Whether the framework''s interior experience or its exterior incidence governs classification.').

omega_variable(
    doctrinal_reversibility_of_foreclosure,
    'Is the foreclosure of territorial compromise reversible through doctrinal reinterpretation - land-for-peace rulings exist in the halakhic tradition - or structurally locked by the settlement enterprise''s entrenchment?',
    'Track halakhic rulings and movement responses across the withdrawal events on record (Sinai 1982, Gaza 2005) and any future negotiated withdrawal: if rulings permitting cession gain traction, the foreclosure is soft; if each withdrawal hardens the prohibition, it is locked.',
    'Reversible foreclosure keeps the arrangement a tangled_rope with a live coordination core; locked foreclosure pushes the compromise-victim and Palestinian seats toward snare-grade incidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_reversibility_of_foreclosure, empirical, 'Whether the covenant''s territorial prohibition is doctrinally elastic or entrenched.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the jewish_self_determination kernel; how would the classification change under each sibling reading''s instantiation?',
    'Author the sibling stories and compare: liberal_nationalist_reading yields a bargained claim with lower epsilon for compromise seats; indigenous_return_reading shifts the beneficiary/victim framing toward decolonization; settler_colonial_reading expands the victim set and raises epsilon; diasporist_reading dissolves the sovereignty obligation entirely.',
    'The disagreement is located in the source of title; whichever source prevails rewrites the beneficiary/victim structure and hence every seat''s directionality. Cross-reading comparison is only valid across separately authored stories, never by averaging inside this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: sibling readings as alternative constraints over the same kernel.').

omega_variable(
    conviction_vs_entrenchment,
    'Is the settlement community''s persistence driven by theological conviction (ideological identity fusion) or by sunk-cost material entrenchment (homes, subsidies, security dependence)?',
    'Natural experiment across subsidy and security policy changes: if removal of material support collapses retention while doctrine is unchanged, entrenchment dominates; if retention persists at material loss, conviction dominates.',
    'Conviction-dominated persistence means the arrangement survives enforcement decay (identity_locked dynamics deepen); entrenchment-dominated persistence means it is enforcement-dependent and would soften if the state withdrew support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_entrenchment, empirical, 'Ideological versus structural sources of beneficiary-seat persistence.').

omega_variable(
    delegitimization_internalization,
    'Is the declining electoral viability of secular compromise positions structural (coalition rules, settlement-anchored bloc voting) or internalized (compromise advocates pre-conceding the theological frame to remain electable)?',
    'Post-shift trajectory: if compromise platforms recover when coalition arithmetic changes, the suppression was structural; if advocates continue self-limiting after the arithmetic opens, the frame has been internalized.',
    'Internalized suppression means the target seat carries the foreclosure with it after formal barriers lift, raising effective suppression above the structural measure; purely structural suppression would release quickly under changed rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegitimization_internalization, empirical, 'Structural versus internalized suppression on the secular-compromiser seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__religious_covenant_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(jewi_tr_t10, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__religious_covenant_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(jewi_tr_t30, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__religious_covenant_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__religious_covenant_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t50, observed).
narrative_ontology:measurement(jewi_tr_t57, jewish_self_determination__religious_covenant_reading, theater_ratio, 57, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__religious_covenant_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(jewi_be_t10, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__religious_covenant_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(jewi_be_t30, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__religious_covenant_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(jewi_be_t40, observed).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__religious_covenant_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement_basis(jewi_be_t50, observed).
narrative_ontology:measurement(jewi_be_t57, jewish_self_determination__religious_covenant_reading, base_extractiveness, 57, 0.73).
narrative_ontology:measurement_basis(jewi_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__religious_covenant_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(jewi_su_t10, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__religious_covenant_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(jewi_su_t30, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__religious_covenant_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(jewi_su_t40, observed).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__religious_covenant_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement_basis(jewi_su_t50, observed).
narrative_ontology:measurement(jewi_su_t57, jewish_self_determination__religious_covenant_reading, suppression_requirement, 57, 0.8).
narrative_ontology:measurement_basis(jewi_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Jewish claim to the land' decomposes into five structurally distinct readings of one kernel, each with its own epsilon, beneficiary/victim structure, and classification. This member instantiates the religious_covenant_reading. The liberal_nationalist and indigenous_return members are upstream legitimacy bases this reading frequently cites or absorbs; the settler_colonial member is the downstream indictment whose premise this reading's foundational axiom contradicts; the diasporist member rejects the sovereignty conclusion outright. Family members must be compared as separate stories, never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
