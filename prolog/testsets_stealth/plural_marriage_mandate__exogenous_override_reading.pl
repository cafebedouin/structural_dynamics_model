% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Enforced Abandonment of Plural Marriage (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   Between 1882 and 1890 the United States escalated from statute to
 *   confiscation to dismantle plural marriage among the Latter-day Saints:
 *   test-oath disfranchisement, mass cohabitation prosecutions, corporate
 *   dissolution, and the threatened escheatment of temples culminated in the
 *   October 1890 announcement ending the practice. This story instantiates
 *   the exogenous_override_reading of the plural_marriage_mandate kernel: the
 *   abandonment was produced by federal coercion, not prophetic revelation,
 *   and the requirement's divine status was never withdrawn by its giver. The
 *   epsilon referent is fixed per the kernel-reading rule: the standing
 *   arrangement under contest is the coercion-backed abandonment regime
 *   itself — the machinery and the surrender it purchased — assessed by this
 *   reading's own lights, in which the practice was a held-divine requirement
 *   and its termination stripped practitioners of a covenant under threat.
 *   Values are reading-indexed over that fixed referent; the sibling readings
 *   author their own stories over the same referent with their own epsilon.
 *   Claim and metrics are authored independently: the claimed type is what
 *   this reading holds structurally true, and the metrics describe the
 *   arrangement's documented operation — where the engine's computed types
 *   diverge from the claim, that divergence is data, not error.
 *
 * KEY AGENTS:
 *   - federal_government: agenda setter (institutional/arbitrage) — enacts and funds the enforcement machinery, collects territorial conformity and the religious-exercise precedent
 *   - federal_judiciary: enforcement administrator (institutional/arbitrage) — prosecutes, sentences, sustains escheatment, paces the enforcement waves
 *   - anti_polygamy_reform_coalition: primary beneficiary (organized/mobile) — forty-year campaign delivered in full while bearing none of the machinery's costs
 *   - territorial_gentile_politicians: secondary beneficiary (moderate/mobile) — gains office and franchise space as practitioners are disfranchised
 *   - practicing_polygamists: primary target (organized/constrained) — imprisonment, fines, disfranchisement, family dissolution
 *   - plural_wives_and_children: primary target (powerless/trapped) — lose providers, legal recognition, and any voice in the decision
 *   - lds_first_presidency: coerced administrator (institutional/constrained) — bears indictment and escheatment, then administers the abandonment internally
 *   - rank_and_file_latter_day_saints: burdened constituency (organized/identity_locked) — surrenders a held-eternal principle for communal survival and eventual statehood
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.78).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.61).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Enforced Abandonment of Plural Marriage (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, 'f43c0f88-2220-43b5-b92c-a60cd08ee405').
narrative_ontology:cs_kernel_codification('f43c0f88-2220-43b5-b92c-a60cd08ee405', fixed_text).
narrative_ontology:cs_authority_grounding('f43c0f88-2220-43b5-b92c-a60cd08ee405', lineage).
narrative_ontology:cs_interpretation_layer_present('f43c0f88-2220-43b5-b92c-a60cd08ee405').
narrative_ontology:cs_reading_relation('f43c0f88-2220-43b5-b92c-a60cd08ee405', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('f43c0f88-2220-43b5-b92c-a60cd08ee405', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('f43c0f88-2220-43b5-b92c-a60cd08ee405', foundational, manifesto_was_coercive_capitulation_not_revelation).
narrative_ontology:cs_axiom_status(manifesto_was_coercive_capitulation_not_revelation, holdable).
narrative_ontology:cs_axiom_grounding('f43c0f88-2220-43b5-b92c-a60cd08ee405', manifesto_was_coercive_capitulation_not_revelation, empirically_contingent).
narrative_ontology:cs_axiom('f43c0f88-2220-43b5-b92c-a60cd08ee405', foundational, plural_marriage_remains_binding_divine_requirement).
narrative_ontology:cs_axiom_status(plural_marriage_remains_binding_divine_requirement, holdable).
narrative_ontology:cs_axiom_grounding('f43c0f88-2220-43b5-b92c-a60cd08ee405', plural_marriage_remains_binding_divine_requirement, theological).
narrative_ontology:cs_reference_frame('f43c0f88-2220-43b5-b92c-a60cd08ee405', canonized_divine_mandate).
narrative_ontology:cs_drift_state('f43c0f88-2220-43b5-b92c-a60cd08ee405', smoot_hearing_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f43c0f88-2220-43b5-b92c-a60cd08ee405', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalition).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, territorial_gentile_politicians).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_first_presidency).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_legal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, reynolds_belief_action_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the executive branch enacted the Morrill, Edmunds, and Edmunds-Tucker statutes, funded marshals and prosecutors, dissolved the church's corporate charter, and moved to escheat its property. It sets the terms under which the practice may continue or cease, and it collects territorial political conformity, a uniform national marriage rule, and the precedent that federal law binds conscientious communal practice. No countervailing power inside the territory can alter the arrangement against its will.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Federal courts in Utah prosecute hundreds of unlawful-cohabitation and bigamy cases, uphold the distinction between protected belief and punishable action in Reynolds v. United States, sustain the corporate dissolution and property escheatment in Late Corporation v. United States, and administer test-oath disfranchisement. Judges and commissioners control sentencing severity, amnesty petitions, and the pace of enforcement waves.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% National reform associations, Protestant denominations, and women's organizations campaigned for four decades for federal suppression of plural marriage. The enforcement statutes and the 1890 abandonment deliver their objective in full. None of the machinery's costs touch them — no member faces prosecution, fine, or disfranchisement — and they disband into victory with their platform absorbed into the major parties.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalition, beneficiary,
    organized, generational, mobile, national).

% Non-Mormon officeholders, federal appointees, and settlers in Utah gain offices, contracts, jury seats, and electoral space as test oaths and disfranchisement remove polygamists from the franchise and from officeholding. Their position improves mechanically with each prosecution wave, and they can leave the territory at any time without penalty.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, territorial_gentile_politicians, beneficiary,
    moderate, biographical, mobile, regional).

% Men with plural households face arrest for unlawful cohabitation, months of prison labor, fines, and lifelong disfranchisement; hundreds serve sentences between 1884 and 1890 while marshals raid settlements. Continuing means prosecution; fleeing to the Mexico or Canada colonies means abandoning wives, children, and livelihood; going underground means a fugitive's life. Formal renunciation carries a spiritual cost they hold to be eternal, yet the legal alternative is the loss of liberty and property.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    organized, biographical, constrained, continental).

% Plural wives lose husbands to prison or flight, and with them household income, social standing, and any legal recognition of their marriages; children inherit a family form their society criminalizes. They hold no vote in the councils deciding the practice's fate, most hold no independent property, and no exit exists for them that does not mean dissolving the family itself.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children, payer,
    powerless, generational, trapped, continental).

% The church presidency faces outstanding indictments, the escheatment of temples and meetinghouses, and dissolution of the corporate church; its president is in hiding when the announcement issues. It signs the abandonment, then administers it internally — instructing officers, closing new plural marriages, and issuing a second declaration in 1904 to close the gap between public compliance and private continuance. Its live options are capitulation, resistance to institutional destruction, or scattering the community into exile.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_first_presidency, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_first_presidency, agenda_setter).

% Ordinary members sustain the abandonment in conference because their membership, temple access, and entire community are bound to the institution. They surrender a principle many held to be eternally required, and in exchange the community regains legal peace, amnesties, and eventually statehood. Leaving the church to keep the practice means losing family, community, and salvific assurance simultaneously, so exit exists mostly as schism at the margins.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, rank_and_file_latter_day_saints, payer,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, rank_and_file_latter_day_saints, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns marriage practice in a consolidated religious territory with the national legal standard, resolves the jurisdictional conflict between territorial self-government under church direction and federal sovereignty, and standardizes a single marriage rule across states and territories.
% TRANSFER_FUNCTION: Moves compliance — cessation of plural marriage, submission to federal court jurisdiction, dissolution of theocratic territorial governance — from the Latter-day Saint community to the United States government; moves liberty, property, and franchise from practicing polygamists to the state; and moves political control of Utah from church leadership to federally appointed officials and non-Mormon settlers.
% ABSENT_VOICES: The people whose families the decision dissolves — men in hiding awaiting arrest and plural wives with no independent legal standing — had no seat in the councils that produced the announcement; the surrender was negotiated by a presidency dealing with federal power over the heads of the covenant-holders. Continuationist dissenters afterward objected that no one bearing the requirement had consented to its abandonment.
% DISAPPEARANCE_RATIONALE: Remove the enforcement machinery overnight in 1890 and the practice resumes openly within the year: adherents held it eternally required and had sustained it through twenty-eight years of escalating statutes. Utah's statehood path, the disposition of escheated property, the fate of the imprisoned, and the national precedent on religious exercise all reorganize around the resumed practice.
% FOUNDING_PROBLEM: A territorially concentrated religious polity practiced a marriage form contrary to federal statute, voted as a bloc, and governed its territory through church institutions; Congress built the enforcement machinery to assert federal legal supremacy, break the bloc's political power, and terminate the practice.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous congressional debate, the Supreme Court's opinions in Reynolds v. United States (1879) and Late Corporation of the Church v. United States (1890), and the 1894 amnesty proclamation attest the founding problem as stated. Academic historians of religion and law writing outside any party to the dispute, and the continuationist dissenting tradition itself, attest its resolution — the practice ended, Utah entered the Union in 1896, and the enforcement machinery was dismantled; the dissent literature further attests that the resolution was submission rather than solution. No part of this attestation rests on the benefiting parties alone.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time grid mapping: T0=1882 (Edmunds Act, enforcement becomes real), T5=1887 (Edmunds-Tucker, corporate dissolution and escheatment), T8=1890 (the announcement, issued with the church president in hiding and indictments pending), T13=1895 (amnesty and transition), T17=1899 (post-statehood calm), T22=1904 (Second Manifesto and the Smoot hearings), T25=1907 (Smoot seated, renewed internal discipline). All three tracked series run on this one shared grid. Extractiveness peaks at T8 — the moment of surrender extracts maximally, since the covenant is repudiated under maximum duress — then settles near 0.78 as the forfeiture banks: dissolved families, abandoned ordinance, and the precedent remain extracted even after active pressure recedes. Suppression follows a two-wave enforcement arc rather than a monotonic ratchet: 0.58 at the Edmunds baseline, peaking 0.91 in 1890, relaxing to 0.55 after amnesty and statehood as compliance internalizes, then reactivating to 0.64 during the Smoot era when post-announcement continuances surfaced. This is enforcement-wave dynamics driven by external political attention, not intermittent reinforcement as an extraction technique. Theater rises steadily from 0.10 to ~0.51: before 1890 the machinery was overtly coercive with little declaratory dressing; after the announcement the 'voluntary revelation' frame carries the compliance load while private continuance persisted, widening the performance-to-function gap until the 1904 discipline narrowed it again. Accessibility collapse is 0.68 — flight to the colonies and underground continuance remained real alternatives, but at ruinous cost, so alternatives collapsed only partly. Resistance is 0.58: hundreds prosecuted, continuationist dissent, litigation, and flight, ultimately overwhelmed by 1907. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal seats the arrangement reads as lawful enforcement of a democratically enacted standard — coordination-forward, low personal cost, arbitrage-grade position. From the practitioner seats the identical statutes read as persecution: prison labor for cohabitation, families dissolved by design, franchise stripped. The presidency seat experiences a forced choice between institutional destruction and covenant repudiation, then inherits the administrator's burden of enforcing the surrender on its own members. Same instruments, opposite moral worlds; the engine derives this divergence from the declared roles, power atoms, and exit options rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the federal government collects conformity, political control, and precedent at negligible cost to itself, and the reform coalition collects its forty-year objective while bearing nothing. Victims derive high directionality amplified by exit structure — practicing polygamists are constrained (flight and underground exist but at ruinous cost), and plural wives and children are fully trapped, sitting nearest the full-target end. The presidency is genuinely dual-positioned: it pays heavily (indictment, escheatment, forced repudiation) yet administers the resulting order, netting a moderately high directionality rather than the low value an administrator seat alone would suggest. Rank-and-file members sit mixed: they pay a held-eternal principle but receive normalization, amnesty, and statehood, pulling them toward symmetric. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct per-agent relationships, and the coarse power-atom keying of overrides would misfire across this story's heterogeneous institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a consolidated religious polity defying federal marriage law and controlling a territory — died with the surrender and statehood, yet the arrangement persists as settled law with enforcement dormant. That dead-problem-plus-world-rearranging-dependence mismatch is exactly the configuration the genealogy interview flags, and it guards against two symmetrical errors. First, it blocks the retrospective laundering by which the post-1890 calm gets read as evidence the abandonment was consensual: the calm was purchased by a decade of imprisonment and confiscation, and the suppression series documents the purchase price. Second, it blocks the reverse error of denying the real coordination function — uniform marriage law and the resolution of the sovereignty conflict were genuine goods the arrangement delivered. The mandate's original function is resolved; what persists is the banked settlement, and the classification keeps the coercive origin visible rather than letting the settled outcome retroactively re-describe the surrender as voluntary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_seat_epsilon_indexicality,
    'This story authors epsilon from the exogenous_override seat of the plural_marriage_mandate kernel; what epsilon and victim structure would the sibling readings author over the same referent, and how would classification move?',
    'Generate the sibling stories (endogenous_reinterpretation_reading, institutional_pragmatism_reading) and compare computed classifications across the constraint family.',
    'Under the endogenous reading the abandonment is covenantal obedience, the victim set thins toward voluntary sacrifice, and the type trends toward rope or tangled_rope; under the pragmatist reading the extraction is acknowledged but attributed to institutional strategy, trending tangled_rope. Cross-family comparison isolates how much of this story''s severity is reading-indexed rather than referent-fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_seat_epsilon_indexicality, conceptual, 'Epsilon is a property of the reading; sibling readings over the same referent will classify differently.').

omega_variable(
    woodruff_revelation_versus_duress,
    'What share of the 1890 decision''s causation was coercive pressure versus independently motivated revelatory experience as the church president reported it?',
    'Historiographic weighing of the president''s diaries, council minutes, contemporaneous correspondence, and the timing of the announcement against the pending escheatment and indictment schedule.',
    'If a substantial independent revelatory motive is established, part of the measured extraction reattributes from external imposition to internal choice and the snare reading weakens toward tangled_rope; if the record shows duress dominating, the exogenous attribution firms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(woodruff_revelation_versus_duress, empirical, 'Causal weight of coercion versus claimed revelation in the abandonment decision.').

omega_variable(
    victim_consent_or_submission,
    'Did the practicing polygamists and their families consent to the abandonment, as the sustained conference votes suggest, or did they submit under a suppression campaign that had already broken organized resistance?',
    'Post-announcement continuance rates, disciplinary cases, continuationist dissent literature, and the eventual fundamentalist schisms as revealed-preference evidence of withheld consent.',
    'Genuine consent would lower effective suppression on the victim seats and soften the extraction asymmetry; documented submission under prior coercion keeps suppression high and supports the snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_consent_or_submission, empirical, 'Whether victim-seat compliance reflects assent or coerced conformity.').

omega_variable(
    post_enforcement_type_drift,
    'Does the post-1896 decay of active enforcement mark a completed extraction whose proceeds are banked, or a transition toward inertial persistence in which the prohibition survives mainly as habit and memory?',
    'Track the prohibition''s maintenance costs and violation rates after enforcement winds down: if the norm holds with near-zero enforcement indefinitely, the arrangement is drifting toward inertia; if violations recur whenever attention lapses, active enforcement remains constitutive.',
    'Long-run drift toward inertia would reclassify the surviving prohibition away from pure extraction toward a degraded-inertial form; stable enforced extraction keeps the snare classification live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_enforcement_type_drift, empirical, 'Whether falling suppression signals banked compliance or incipient inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmm_exogenous_tr_t0, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t0, observed).
narrative_ontology:measurement(pmm_exogenous_tr_t5, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t5, observed).
narrative_ontology:measurement(pmm_exogenous_tr_t8, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t8, observed).
narrative_ontology:measurement(pmm_exogenous_tr_t13, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 13, 0.43).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t13, observed).
narrative_ontology:measurement(pmm_exogenous_tr_t17, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 17, 0.47).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t17, observed).
narrative_ontology:measurement(pmm_exogenous_tr_t22, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 22, 0.53).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t22, observed).
narrative_ontology:measurement(pmm_exogenous_tr_t25, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(pmm_exogenous_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(pmm_exogenous_be_t0, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(pmm_exogenous_be_t0, observed).
narrative_ontology:measurement(pmm_exogenous_be_t5, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement_basis(pmm_exogenous_be_t5, observed).
narrative_ontology:measurement(pmm_exogenous_be_t8, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 8, 0.82).
narrative_ontology:measurement_basis(pmm_exogenous_be_t8, observed).
narrative_ontology:measurement(pmm_exogenous_be_t13, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 13, 0.77).
narrative_ontology:measurement_basis(pmm_exogenous_be_t13, observed).
narrative_ontology:measurement(pmm_exogenous_be_t17, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 17, 0.75).
narrative_ontology:measurement_basis(pmm_exogenous_be_t17, observed).
narrative_ontology:measurement(pmm_exogenous_be_t22, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 22, 0.79).
narrative_ontology:measurement_basis(pmm_exogenous_be_t22, observed).
narrative_ontology:measurement(pmm_exogenous_be_t25, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement_basis(pmm_exogenous_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(pmm_exogenous_su_t0, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(pmm_exogenous_su_t0, observed).
narrative_ontology:measurement(pmm_exogenous_su_t5, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement_basis(pmm_exogenous_su_t5, observed).
narrative_ontology:measurement(pmm_exogenous_su_t8, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 8, 0.91).
narrative_ontology:measurement_basis(pmm_exogenous_su_t8, observed).
narrative_ontology:measurement(pmm_exogenous_su_t13, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 13, 0.66).
narrative_ontology:measurement_basis(pmm_exogenous_su_t13, observed).
narrative_ontology:measurement(pmm_exogenous_su_t17, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 17, 0.55).
narrative_ontology:measurement_basis(pmm_exogenous_su_t17, observed).
narrative_ontology:measurement(pmm_exogenous_su_t22, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 22, 0.64).
narrative_ontology:measurement_basis(pmm_exogenous_su_t22, observed).
narrative_ontology:measurement(pmm_exogenous_su_t25, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(pmm_exogenous_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the plural_marriage_mandate kernel per the epsilon-invariance principle: the colloquial label 'the 1890 Manifesto' conflates three structurally distinct claims about the same event, each with its own epsilon, victim set, and classification. This story (exogenous_override_reading) authors the coercion-causation claim with high epsilon and a practitioner victim set. The endogenous_reinterpretation_reading authors the genuine-revelation claim over the same referent with a thin victim set and low epsilon. The institutional_pragmatism_reading authors the strategic-adaptation claim with acknowledged extraction attributed to institutional strategy. Each member links the others via network edges; cross-family comparison separates reading-indexed variance from referent-fixed structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
