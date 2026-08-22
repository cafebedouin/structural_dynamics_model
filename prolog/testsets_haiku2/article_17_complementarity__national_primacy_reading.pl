% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity: National Primacy Reading
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes complementarity as the
 *   principle governing the International Criminal Court's jurisdiction. The
 *   national primacy reading interprets this principle as a
 *   sovereignty-protection mechanism: national courts are presumptively
 *   adequate unless proven sham; the burden is on the ICC to demonstrate that
 *   a state is unwilling or unable to prosecute. This reading prioritizes
 *   state autonomy, respects local institutional legitimacy, and treats the
 *   ICC as a backstop. It benefits national judiciaries (who retain
 *   presumptive jurisdiction) and sovereignty-maximizing states (who resist
 *   external accountability oversight). It constrains victims in
 *   weak-but-genuine proceedings (whose cases fall outside ICC reach if their
 *   domestic courts meet the low threshold of 'genuine') and the ICC itself
 *   (which must invest substantial resources to prove inadmissibility rather
 *   than presuming the question open). The measurement series tracks the
 *   extraction and theater-ratio rise as the reading has been applied to
 *   block ICC cases from increasingly-scrutinized domestic proceedings, while
 *   suppression-requirement has stabilized as the institutional machinery of
 *   admissibility determinations has matured.
 *
 * KEY AGENTS:
 *   - National judiciaries: hold presumptive jurisdiction and set the adequacy standard against which ICC admissibility is measured
 *   - Sovereignty-maximizing states: benefit from a high bar for ICC intervention and frame ICC actions as neo-colonial overreach
 *   - Victims in weak-but-genuine proceedings: foreclosed from ICC because domestic courts meet the formal threshold despite substantive inadequacy
 *   - ICC Office of the Prosecutor: bears the evidentiary burden to prove unwillingness or inability
 *   - International oversight advocates: excluded from the beneficiary set under this reading; argue for broader accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.68).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.72).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity: National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '076a39d9-a960-48ee-8efe-ff1a908fd06a').
narrative_ontology:cs_kernel_codification('076a39d9-a960-48ee-8efe-ff1a908fd06a', fixed_text).
narrative_ontology:cs_authority_grounding('076a39d9-a960-48ee-8efe-ff1a908fd06a', lineage).
narrative_ontology:cs_interpretation_layer_present('076a39d9-a960-48ee-8efe-ff1a908fd06a').
narrative_ontology:cs_reading_relation('076a39d9-a960-48ee-8efe-ff1a908fd06a', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('076a39d9-a960-48ee-8efe-ff1a908fd06a', foundational, national_courts_presumptively_adequate).
narrative_ontology:cs_axiom_status(national_courts_presumptively_adequate, holdable).
narrative_ontology:cs_axiom_grounding('076a39d9-a960-48ee-8efe-ff1a908fd06a', national_courts_presumptively_adequate, deontological).
narrative_ontology:cs_axiom('076a39d9-a960-48ee-8efe-ff1a908fd06a', foundational, state_sovereignty_immune_from_external_override).
narrative_ontology:cs_axiom_status(state_sovereignty_immune_from_external_override, holdable).
narrative_ontology:cs_axiom_grounding('076a39d9-a960-48ee-8efe-ff1a908fd06a', state_sovereignty_immune_from_external_override, deontological).
narrative_ontology:cs_reference_frame('076a39d9-a960-48ee-8efe-ff1a908fd06a', state_sovereignty_primacy_framework).
narrative_ontology:cs_drift_state('076a39d9-a960-48ee-8efe-ff1a908fd06a', contemporary_accountability_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('076a39d9-a960-48ee-8efe-ff1a908fd06a', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_enforcement_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, alleged_perpetrators_from_weak_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_prosecution_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain presumptive primacy over prosecutions within their territory. The complementarity reading protects their jurisdiction from ICC override absent a demonstrable sham proceeding. They defend this as respecting state sovereignty and the legitimacy grounded in local democratic institutions. In practice, they benefit from a high bar for ICC intervention — weak prosecutions that fall short of 'unwilling or unable' persist unchallenged. Their exit option is strong: they set the standard against which ICC admissibility is measured.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter).

% Use the national primacy reading to resist ICC involvement in their internal affairs, even when national prosecutions are inadequate by many substantive measures. The reading allows them to frame ICC intervention as neo-colonial overreach and to control the narrative of their own accountability. They benefit from a doctrine that treats their courts as adequate unless proven sham — a status that comes slowly and requires ICC to marshal evidence of bad faith, not merely ineptitude or resource constraint.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, arbitrage, national).

% Have initiated prosecutions in their own courts, meeting the formal threshold for genuine proceedings, but those courts lack capacity, independence, or institutional robustness to deliver accountability. Under the national primacy reading, they are foreclosed from ICC recourse — the reading's high inadmissibility threshold means their cases are treated as adequately handled domestically, despite the practical inadequacy. They cannot exit the national system and are denied international remedy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_proceedings, payer,
    powerless, biographical, trapped, national).

% Bears the cost of defending its jurisdiction against every admissibility challenge. The national primacy reading shifts the burden onto the ICC to prove unwillingness or inability rather than presuming the question open. The Office of the Prosecutor must invest substantial investigative resources to demonstrate that a proceeding is a sham before the Court can assert complementary jurisdiction — a structural tax on ICC capacity that can be leveraged to reduce the number of cases it can pursue.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecution_capacity, payer,
    institutional, generational, constrained, global).

% Benefit from the high threshold for proving judicial inadequacy. If their state's courts are conducting some form of proceeding against them, even if compromised or underfunded, the national primacy reading keeps ICC jurisdiction at bay. They can contest admissibility by pointing to the existence of national proceedings and forcing the ICC to prove those proceedings are designed to shield them — a demanding evidentiary bar.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, alleged_perpetrators_from_weak_states, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for interpreting complementarity as an accountability trigger mechanism that activates when states fail, not a sovereignty shield that requires proof of sham. They are excluded from the beneficiary set under the national primacy reading because that reading treats their advocacy as threatening state sovereignty. They would argue the reading enables impunity and privatizes justice to state capacity; they remain outside the institutional decision-making that produces the reading's application.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_oversight_advocates, excluded,
    organized, generational, identity_locked, global).

% Monitor the reading's application to assess risk to their own citizens and sovereignty. They tend to support the national primacy reading insofar as it protects them from ICC jurisdiction, but may shift stance when crimes in other regions (especially where they have geopolitical interest) demand ICC intervention. Their observation position gives them leverage over how states interpret the reading in practice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, powerful_states_with_geopolitical_interest, observer,
    powerful, generational, arbitrage, global).

% The legal kernel that embeds complementarity as a principle but leaves 'unwilling or unable' undefined. Both readings (national primacy and international oversight) claim fidelity to the statute; the reading contest is about what the statute's text legitimately instantiates, not about what the statute says.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, rome_statute_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_17_complementarity__national_primacy_reading, rome_statute_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a two-tier criminal accountability system: national jurisdictions handle prosecution by default; ICC intervenes only when national courts are provably inadequate. This avoids duplicative prosecution, respects state legitimacy, and preserves ICC as a backstop rather than a primary forum. The coordination problem it solves: how to pursue accountability for grave crimes without empowering a supranational tribunal to override state jurisdictions and without allowing capable states to escape responsibility.
% TRANSFER_FUNCTION: Transfers primary jurisdiction and the authority to define adequacy from international to national institutions. Victims, perpetrators, and evidence remain under national court control unless the ICC successfully challenges that arrangement. The reading also transfers the burden of proof: the ICC must prove inadequacy rather than national courts proving adequacy.
% ABSENT_VOICES: Victims in non-cooperating or minimally-cooperating states who cannot access ICC because their national courts are demonstrably inadequate but fall short of the 'sham' threshold; human rights organizations that favor international accountability over deference to sovereignty; populations in weak-state jurisdictions who distrust local courts but are foreclosed from international remedy.
% DISAPPEARANCE_RATIONALE: If this reading of complementarity disappeared and the international_oversight_reading took its place, the ICC's admissibility threshold would lower, many more cases from weak-but-genuine domestic proceedings would reach the ICC, state cooperation incentives would shift, and the operational jurisdiction of national courts would shrink. The reading is the structural mechanism that protects state primacy; its absence would reorganize the international accountability system toward greater ICC reach.
% FOUNDING_PROBLEM: Post-Cold War nation-building and the creation of the ICC created a risk of neo-colonial intervention by powerful states using international courts to override weaker states' sovereignty and undermine their institutions. Complementarity was devised to respect state capacity and legitimacy — to assume that national courts are adequate unless proven catastrophically incompetent, thereby protecting smaller states from arbitrary ICC override while still creating a safety valve for cases of genuine judicial collapse.
% FOUNDING_PROBLEM_CORROBORATION: Supported by state delegations to the Rome Statute negotiations (documented in conference records emphasizing sovereignty protection); endorsed by many African states and developing-world coalitions suspicious of international court overreach; contested by human rights organizations, victims' advocates, and international justice scholars who argue the founding problem understates the prevalence of judicial inadequacy (not just collapse) and that state capacity was never the main barrier to accountability.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score rises from 0.45 (at the statute's founding, when the reading was a theoretical possibility) to 0.68 (present day, as it has been operationalized to block many cases). This reflects the reading's increasing use as a mechanism to shield weak-but-genuine domestic proceedings from ICC scrutiny. The theater ratio climbs from 0.22 to 0.48, indicating that a growing share of the reading's enforcing activity is theatrical — admissibility briefings and determinations that defend the reading's legitimacy (sovereignty, state capacity, local democratic legitimacy) rather than substantively assessing whether the domestic proceedings actually deliver accountability. Suppression is high and stable (0.72): the reading is maintained by the institutional suppression of alternatives — alternative interpretations of complementarity are marginalized in ICT jurisprudence and state practice; victims' arguments for broader ICC reach are absorbed into the adequacy-testing framework and typically rejected; human rights advocacy is channeled into attempting to prove unwillingness rather than challenging the reading itself. The shared time grid aligns all three metrics: at each time point, extractiveness, theater_ratio, and suppression_requirement are all authored on the same calendar. Extraction rises as the reading is applied and defended; theater rises as enforcement activity increasingly focuses on procedural legitimacy rather than outcome assessment; suppression stays stable because the institutional machinery for the reading is well-established and does not require escalating coercion — the reading has become the default expectation.
 *
 * PERSPECTIVAL GAP:
 *   From the national judiciary and sovereignty-maximizing state seats, the reading is protecting legitimate institutions and preventing neo-colonial override. From the victim seat, especially victims in weak-but-genuine proceedings, the reading is a mechanism of exclusion and abandonment: it keeps international remedy out of reach precisely when domestic remedy is inadequate. The ICC seat sees the reading as imposing an insurmountable evidentiary burden that makes the Court's complementary jurisdiction illusory. These perspectives should compute to different types at each seat: national judiciaries and states compute as beneficiaries under a coordination frame (the reading is a cooperative arrangement they benefit from); victims compute as targets under an extraction frame (the reading extracts their access to international remedy); the ICC computes as partially constrained (the reading binds its discretion). The engine derives these from power + exit + beneficiary/victim declarations; the authored claim (tangled_rope) sits between the cooperative frame and the extraction frame, reflecting the reading's hybrid character — it coordinates state primacy while extracting from victims in weak-state contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are structural beneficiaries: they hold primary jurisdiction, set the adequacy standard, and face a burden-shifting requirement that favors them. Their directionality (d) sits near 0.0 (full beneficiary): they extract benefit without paying cost. Victims in weak-but-genuine proceedings are structural targets: they are foreclosed from ICC remedy, trapped in domestic systems that fall short of adequate, and their interests are subordinated to sovereignty. Their directionality sits near 1.0 (full target). The ICC sits near 0.5 to 0.6 (constrained target): it has some agenda-setting capacity within the complementarity framework but is burdened with proof obligations and cannot assert jurisdiction without meeting a high bar set by the reading. No directionality override is needed; the beneficiary/victim + power + exit derivation chain produces the right d for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Cold War risk of neo-colonial ICC override) was live at the statute's inception. By 2026, the founding problem's status is contested: sovereignty advocates argue it remains live and ever-present; accountability advocates argue it has largely resolved (the ICC has not overridden states arbitrarily; the real problem is now state non-cooperation and judicial inadequacy). The reading persists in defending against a threat many analysts believe is no longer the primary barrier to accountability. This is a mandatrophy candidate: the arrangement is being maintained primarily to defend against a founding problem that is no longer the active concern, while it creates new problems (victims foreclosed from remedy). The theater ratio (0.48) supports this: enforcement activity is now largely focused on procedural legitimacy (admissibility briefings, state cooperation performances) rather than on assessing whether accountability is actually happening. The reading has not been formally superseded (no amendment to Article 17), but its justification has become increasingly detached from the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_threshold_definition,
    'What evidence standard demonstrates that a domestic proceeding is a ''sham'' rather than merely weak or underfunded? How much evidence of bad-faith design (versus institutional incapacity) is required to trigger ICC jurisdiction under this reading?',
    'Examination of ICC admissibility jurisprudence and state practice: where have courts actually found unwillingness? What evidence triggered jurisdiction assertions? Comparative analysis of cases rejected as ''genuine'' despite substantive inadequacy.',
    'If ''sham'' is defined narrowly (explicit bad faith, institutional collusion), the national primacy reading forecloses ICC access to nearly all cases from weak-but-genuine proceedings. If ''sham'' is defined more broadly (indifference, structural inability to investigate high-level perpetrators), the reading''s protective effect weakens substantially, and the international_oversight_reading gains practical force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sham_threshold_definition, empirical, 'The evidentiary bar for proving unwillingness or inability.').

omega_variable(
    state_cooperation_leverage,
    'Is the national primacy reading''s protection of state sovereignty a genuine coordination benefit (states genuinely commit to accountability if given jurisdiction), or does it operate primarily as a mechanism to extract immunity from states that would otherwise face ICC pressure?',
    'Controlled comparison: in cases where states retained primary jurisdiction, did they actually prosecute more thoroughly or faster than ICC cases progressed? Did state-retained cases lead to comparable accountability outcomes, or did they systematically stall?',
    'If states genuinely use retained jurisdiction for accountability, the reading coordinates legitimate state action and is a true rope. If states use it primarily for delay or selective prosecution, the reading is a snare masked as coordination — pure extraction of immunity disguised as sovereignty respect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_leverage, empirical, 'Whether state-retained jurisdiction delivers accountability or shields perpetrators.').

omega_variable(
    reading_contention_kernel_underdetermination,
    'Does the Rome Statute text itself privilege the national primacy reading, or does the text''s silence on ''unwilling or unable'' mean both readings are structurally plausible interpretations of the same statute?',
    'Textual analysis of the statute''s negotiating history and structure; examination of whether the statute''s language logically entails either reading or leaves both open; assessment of whether subsequent amendments or authoritative interpretation have settled the question.',
    'If the statute text underdetermines the reading, the national primacy reading is one contingent institutional choice, not the statute''s true meaning — this reduces its legitimacy claim and opens space for the international_oversight_reading to assert equal textual warrant. If the text privileges national primacy, the international_oversight_reading is a forced (or activist) reinterpretation, and this reading''s legitimacy strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_kernel_underdetermination, conceptual, 'Whether the kernel''s statutory text entails or merely permits the national primacy reading.').

omega_variable(
    neo_colonial_threat_persistence,
    'Has the post-Cold War risk of neo-colonial ICC override (the founding problem) persisted as a live threat, or has the ICC''s actual restraint and the growth of African and developing-world judiciaries made this threat largely theoretical?',
    'Historical survey of ICC cases and admissibility decisions: has the ICC overridden states arbitrarily, or has it deferred extensively? Examination of state-ICC cooperation patterns and whether developing-world resistance reflects actual neo-colonial threat or principled sovereignty preference.',
    'If the neo-colonial threat is live, the national primacy reading remains justified as a protective mechanism. If the threat has largely resolved, the reading has become a mandatrophic zombie: maintained to defend against a problem that is no longer primary, while creating new problems (victim foreclosure, impunity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neo_colonial_threat_persistence, empirical, 'Whether the founding problem persists as a live institutional threat.').

omega_variable(
    reading_family_contention,
    'To what extent is the contention between the national_primacy_reading and the international_oversight_reading a principled disagreement about state sovereignty versus international accountability, versus a conflict over which institutions (national courts or the ICC) capture power and resources?',
    'Analysis of state positions: do states that advocate the national primacy reading actually deliver strong domestic accountability, or do they use the reading selectively (accepting it for others, resisting it for themselves)? Do they support the reading because they trust their institutions or because they want to evade external scrutiny?',
    'If the contention is principled, both readings remain legitimate options and the kernel genuinely admits both. If the contention is largely about institutional capture, the national primacy reading is an extractive mask for sovereignty theft, and the international_oversight_reading better serves accountability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_family_contention, preference, 'The underlying motivation for state-level support of the national primacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1998, article_17_complementarity__national_primacy_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(arti_tr_t2005, article_17_complementarity__national_primacy_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(arti_tr_t2012, article_17_complementarity__national_primacy_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(arti_tr_t2018, article_17_complementarity__national_primacy_reading, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(arti_tr_t2023, article_17_complementarity__national_primacy_reading, theater_ratio, 2023, 0.46).
narrative_ontology:measurement(arti_tr_t2026, article_17_complementarity__national_primacy_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t1998, article_17_complementarity__national_primacy_reading, base_extractiveness, 1998, 0.45).
narrative_ontology:measurement(arti_be_t2005, article_17_complementarity__national_primacy_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(arti_be_t2012, article_17_complementarity__national_primacy_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(arti_be_t2018, article_17_complementarity__national_primacy_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(arti_be_t2023, article_17_complementarity__national_primacy_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement(arti_be_t2026, article_17_complementarity__national_primacy_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1998, article_17_complementarity__national_primacy_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(arti_su_t2005, article_17_complementarity__national_primacy_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(arti_su_t2012, article_17_complementarity__national_primacy_reading, suppression_requirement, 2012, 0.67).
narrative_ontology:measurement(arti_su_t2018, article_17_complementarity__national_primacy_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(arti_su_t2023, article_17_complementarity__national_primacy_reading, suppression_requirement, 2023, 0.71).
narrative_ontology:measurement(arti_su_t2026, article_17_complementarity__national_primacy_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel article_17_complementarity. The sibling reading (international_oversight_reading) interprets complementarity as an accountability-trigger mechanism with a lower admissibility threshold and broader ICC reach. The two readings are authored as separate constraints with independent ε values (0.68 for national_primacy_reading, expected ~0.38-0.45 for international_oversight_reading) because they instantiate structurally distinct claims about what the same Rome Statute article does: the national primacy reading extracts from victims in weak-state jurisdictions and protects national courts; the international_oversight_reading would extract from state sovereignty and expand ICC reach. These are not the same constraint viewed from different angles — they are different constraints instantiated by different readings of the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
