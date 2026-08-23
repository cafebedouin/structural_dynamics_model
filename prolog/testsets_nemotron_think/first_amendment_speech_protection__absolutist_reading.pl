% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Speech Protection
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment — 'no law means no law' —
 *   presents speech protection as a categorical mountain: a natural-law-like
 *   constraint that admits only narrow, historically fixed exceptions
 *   (incitement, obscenity, defamation, true threats). This reading claims
 *   the constraint emerges from the constitutional text itself, requiring no
 *   active enforcement beyond judicial application of the fixed exceptions.
 *   However, the structural data reveals identifiable beneficiaries
 *   (speakers, majority groups, media institutions) who capture a liberty
 *   surplus, and identifiable victims (targeted minorities, marginalized
 *   communities) who bear the externalized costs of hate speech, harassment,
 *   and incitement shielded by the categorical rule. The mountain claim is
 *   maintained by an interpretive layer (courts administering exceptions)
 *   that absorbs drift without surfacing revision. This is a false summit
 *   mountain candidate: the constraint presents as natural law but
 *   distributes benefits and harms along identifiable structural lines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.22).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.28).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, mountain).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd').
narrative_ontology:cs_kernel_codification('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', fixed_text).
narrative_ontology:cs_authority_grounding('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', lineage).
narrative_ontology:cs_interpretation_layer_present('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd').
narrative_ontology:cs_reading_relation('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', foundational, first_amendment_text_is_categorical).
narrative_ontology:cs_axiom_status(first_amendment_text_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', first_amendment_text_is_categorical, deontological).
narrative_ontology:cs_axiom('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', foundational, harm_to_listeners_is_not_a_ground_for_regulation).
narrative_ontology:cs_axiom_status(harm_to_listeners_is_not_a_ground_for_regulation, holdable).
narrative_ontology:cs_axiom_grounding('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', harm_to_listeners_is_not_a_ground_for_regulation, deontological).
narrative_ontology:cs_axiom('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', secondary, historical_exceptions_are_exhaustive_and_fixed).
narrative_ontology:cs_axiom_status(historical_exceptions_are_exhaustive_and_fixed, holdable).
narrative_ontology:cs_axiom_grounding('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', historical_exceptions_are_exhaustive_and_fixed, conventional).
narrative_ontology:cs_reference_frame('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', founding_text_absolutism).
narrative_ontology:cs_drift_state('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', contemporary_hate_speech_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5c7c3290-3f1c-4c26-bd0c-9504d4fe5fdd', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_groups).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, media_institutions).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, marginalized_communities).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, first_amendment_absolutism).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, categorical_speech_protection).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, textual_literalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups who speak, publish, or express ideas. The absolutist rule shields them from content-based regulation, granting near-total protection for their speech acts. Exit from the constraint's protection is not needed — they are its primary beneficiaries. Their situation improves the more absolute the rule.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers, beneficiary,
    organized, biographical, mobile, national).

% Demographic and cultural majorities whose speech norms dominate public discourse. The categorical rule protects majority speech from minority-driven regulatory demands (e.g., hate speech laws). They can shift platforms, jurisdictions, or framing to avoid any residual regulation — exit is near-arbitrage. They capture the liberty surplus while externalizing harm.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_groups, beneficiary,
    powerful, generational, arbitrage, national).

% Corporate and institutional press entities that set the terms of public discourse. They invoke absolutist doctrine to resist regulation of content moderation, liability, and ownership concentration. They benefit from the rule's barrier to government action while exercising private gatekeeping power. Exit options include jurisdictional arbitrage and platform migration.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, media_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, media_institutions, beneficiary).

% Racial, religious, gender, and sexual minorities disproportionately targeted by hate speech, harassment, and incitement shielded by absolutist doctrine. They bear the systemic costs — psychological harm, dignitary injury, chilled participation, violence — without legal recourse under the categorical rule. Exit from the harm is structurally blocked: they cannot leave their identity, and the speech environment permeates civic life.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, biographical, trapped, national).

% Communities defined by immutable or socially enforced identity markers (race, caste, religion, disability) that absorb the cumulative oppression externalized by absolutist protection. The constraint fuses their identity with vulnerability: the more absolute the speech rule, the more their identity becomes a target. Exit is identity-locked — leaving the community is neither possible nor a remedy; the harm tracks the identity.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, national).

% Judicial bodies that interpret and apply the First Amendment. Under the absolutist reading, courts are constrained to enforce the categorical rule, but in practice they administer the 'narrow historical exclusions' (incitement, true threats, obscenity, defamation) that do the real work of boundary-drawing. Their institutional legitimacy depends on maintaining the absolutist frame while managing its exceptions.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Academic observers who analyze the constraint from outside the partisan contest. They document the gap between absolutist text and doctrinal practice, the racialized distribution of harm, and the institutional incentives that sustain the mountain claim. Their seat is analytical — they neither collect nor pay, but their work shapes the legitimacy conditions for all other seats.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, categorical rule that eliminates governmental discretion to suppress disfavored speech, solving the coordination problem of predictable speech boundaries for speakers and institutions.
% TRANSFER_FUNCTION: Externalizes the costs of harmful speech (harassment, hate speech, incitement, dignitary harm) from speakers and majority groups onto targeted minorities and marginalized communities who bear the systemic oppression costs without legal recourse.
% ABSENT_VOICES: Targeted minorities and marginalized communities who bear the externalized harm but are structurally excluded from the absolutist framing's beneficiary calculus; international human rights bodies and comparative constitutional courts that recognize dignity-based speech limits; future generations who inherit a speech ecology shaped by unchecked majoritarian discourse.
% DISAPPEARANCE_RATIONALE: If categorical protection vanished overnight, governments would regulate hate speech, targeted harassment, and incitement more aggressively; targeted minorities would gain legal recourse and dignity protections; the speech ecosystem would reorganize around harm-based standards with proportional balancing; media institutions would lose their absolute shield against content regulation; the majoritarian liberty surplus would compress.
% FOUNDING_PROBLEM: Preventing government censorship of political dissent and minority viewpoints by establishing an absolute textual barrier to content-based speech regulation, rooted in the founding generation's experience with sedition laws and press suppression.
% FOUNDING_PROBLEM_CORROBORATION: The founding generation's own Sedition Act of 1798 and the absolutist reading's reliance on 'narrow historical exclusions' (incitement, obscenity, defamation, true threats) corroborate that the categorical claim has always had contested, judicially administered boundaries. Scholars outside the absolutist tradition — critical race theorists (Matsuda, Delgado, Crenshaw), feminist legal scholars (MacKinnon), and comparative constitutionalists — attest the founding problem was never purely about categorical text but about the distribution of power between speakers and the vulnerable; no corroborating source outside the absolutist beneficiary set treats the categorical claim as settled.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22) is low but non-zero: the absolutist rule extracts systemic harm-bearing from protected speakers and transfers it to vulnerable groups. The reading claims ε≈0; the structural reality shows measurable externalization. Suppression (0.28) is low from the state-action perspective (the constraint limits government) but structurally suppresses minority recourse — the constraint suppresses legal remedies for harm. Theater ratio (0.42) is substantial: the categorical frame performs natural-law immunity while the exception machinery does the real boundary work. Accessibility collapse (0.88) is high — alternatives (balancing, harm-based standards) are treated as conceptually incoherent within the absolutist frame. Resistance (0.12) is low — the constraint meets little active resistance because its beneficiaries are powerful and its victims are trapped.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the speaker/majority/media seats (beneficiaries with mobile/arbitrage exit), the constraint computes as mountain or rope — genuine coordination with minimal extraction. From the targeted minority/marginalized community seats (payers with trapped/identity_locked exit), the same constraint computes as snare or tangled_rope — active suppression of remedy, asymmetric harm externalization. The courts seat (agenda_setter, institutional, analytical exit) sits in tension: it administers the mountain claim while managing the exception machinery that prevents the mountain from collapsing. The legal scholar seat (observer) sees the full structure — the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: speakers (organized, mobile exit → d near 0.0), majority_groups (powerful, arbitrage exit → d near 0.0), media_institutions (institutional, arbitrage exit, dual agenda_setter/beneficiary → d near 0.0). Victims declared: targeted_minorities (powerless, trapped exit → d near 1.0), marginalized_communities (powerless, identity_locked exit → d near 1.0). Courts are agenda_setters with analytical exit — they administer but do not structurally benefit from the extraction; their d derives from institutional role, not beneficiary status. The directionality derivation amplifies extraction for the payer seats (trapped, identity_locked) and dampens it for beneficiary seats (mobile, arbitrage), producing the seat divergence the engine measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading prevents mislabeling in two directions: (1) It blocks the 'pure coordination' error by exposing the victim seats — the constraint is not a rope because identifiable parties pay systemic costs without consent or recourse. (2) It blocks the 'pure extraction' error by acknowledging the genuine coordination function — the categorical rule does solve a real collective-action problem (government censorship) for its beneficiaries. The classification tension (mountain claim vs. snare computation for payer seats) is the mandatrophy signal: the founding problem (preventing government censorship) is live but contested; the arrangement has outgrown its founding function and now externalizes harm on a structural scale. The false summit mountain signature captures this: the constraint claims natural-law immunity while operating as a constructed distribution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the absolutist speech constraint a genuine natural-law mountain (textual literalism as irreducible limit) or a constructed constraint that benefits identifiable agents (speakers, majorities, media) by externalizing harm to trapped minorities?',
    'Historical-institutional analysis: if the categorical rule''s boundaries (the ''narrow historical exclusions'') have shifted to track majority interests rather than textual semantics, and if the beneficiary/victim distribution correlates with power rather than text, the mountain claim is constructed. Comparative constitutional evidence: jurisdictions with harm-based speech regimes (Canada, Germany, EU) achieve comparable political liberty with less minority harm — if liberty survives without categorical absolutism, the mountain is not a necessary condition.',
    'If constructed, the false_summit_mountain signature fires and the engine reclassifies to tangled_rope (coordination + asymmetric extraction). The mountain claim is then a cover story for a distribution mechanism. If natural law, the victim seats are collateral to an irreducible limit — the harm is not extraction but tragedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the mountain claim is a genuine natural law or a constructed false summit benefiting identifiable agents.').

omega_variable(
    harm_externalization_mechanism,
    'How does the categorical rule structurally externalize harm to minorities — is the externalization a necessary byproduct of categorical protection, or an engineered feature of the exception machinery?',
    'Doctrinal genealogy of the ''narrow historical exclusions'': trace whether incitement, true threats, and fighting words doctrines were narrowed precisely when they began to protect minority targets (e.g., Brandenburg v. Ohio narrowing incitement during civil rights era; R.A.V. v. St. Paul striking hate speech ordinance). If exception boundaries contract to shield majority speech while expanding to regulate minority speech, the externalization is engineered.',
    'If engineered, the constraint is a snare for payer seats — the coordination story is cover for asymmetric extraction. If necessary byproduct, the constraint is a tragic mountain — the harm is the price of the coordination function, not its purpose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_externalization_mechanism, empirical, 'Whether harm externalization is a necessary cost of categorical protection or an engineered feature of the exception architecture.').

omega_variable(
    historical_exclusions_scope,
    'Are the ''narrow historical exclusions'' truly narrow and fixed by history, or do they function as a living exception architecture that adapts to protect the mountain claim''s beneficiaries?',
    'Corpus analysis of First Amendment exception jurisprudence 1791-present: measure the semantic drift of ''incitement,'' ''obscenity,'' ''defamation,'' ''true threats'' against the absolutist text. If the exceptions absorb all regulatory pressure while the categorical rule remains formally intact, the exclusions are the real constraint and the mountain is theater.',
    'If the exclusions are the living constraint, the theater_ratio is understated — the mountain is almost entirely performative. The constraint family would re-center on the exception architecture as the primary extraction mechanism, with the absolutist text as its legitimation layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_exclusions_scope, empirical, 'Whether the historical exclusions are fixed boundaries or a living exception architecture that does the real regulatory work.').

omega_variable(
    kernel_reading_fork_structure,
    'Does the absolutist reading logically foreclose the harm_limited_reading and categorical_balancing_reading within a single commitment framework, or do the readings coexist as live positions held by different institutional coalitions?',
    'Institutional mapping: identify which courts, scholars, and advocacy organizations hold each reading as operative. If no single institution or coalition holds more than one reading simultaneously (e.g., the ACLU does not simultaneously litigate absolutist and harm-limited positions in the same case), the readings foreclose each other within frameworks. If different factions of the same movement hold different readings situationally, they coexist.',
    'If forecloses: the kernel has genuine structural fractures — adopting one reading commits a party to rejecting the others. If coexists_with: the kernel is a site of ongoing contestation without logical resolution; the constraint family models a persistent pluralism. The reading_relations in cs_structure encode this author''s judgment; the engine tests it against axiom contradictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_fork_structure, conceptual, 'Structural relationship between the absolutist reading and its sibling readings — foreclosure vs. coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 233).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_absolutist_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fa_absolutist_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(fa_absolutist_tr_t100, first_amendment_speech_protection__absolutist_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(fa_absolutist_tr_t150, first_amendment_speech_protection__absolutist_reading, theater_ratio, 150, 0.36).
narrative_ontology:measurement(fa_absolutist_tr_t200, first_amendment_speech_protection__absolutist_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement(fa_absolutist_tr_t233, first_amendment_speech_protection__absolutist_reading, theater_ratio, 233, 0.42).

% Extraction over time
narrative_ontology:measurement(fa_absolutist_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fa_absolutist_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(fa_absolutist_be_t100, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(fa_absolutist_be_t150, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 150, 0.18).
narrative_ontology:measurement(fa_absolutist_be_t200, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement(fa_absolutist_be_t233, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 233, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fa_absolutist_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(fa_absolutist_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(fa_absolutist_su_t100, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(fa_absolutist_su_t150, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 150, 0.24).
narrative_ontology:measurement(fa_absolutist_su_t200, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 200, 0.26).
narrative_ontology:measurement(fa_absolutist_su_t233, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 233, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.08).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the first_amendment_speech_protection constraint family. The absolutist reading claims categorical protection (mountain) with ε≈0.22; the harm_limited_reading centers demonstrable harm as the limiting principle (tangled_rope, higher ε); the categorical_balancing_reading treats protection as category-based balancing (rope/scaffold hybrid). The three readings share the First Amendment text as kernel but instantiate different constraints with different beneficiary/victim structures and ε values. The absolutist reading's mountain claim functions as a legitimation layer for the exception architecture that does the real boundary work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, institutional, 0.15).
constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, powerful, 0.05).
constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
