% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Equality as Universal Principle Requiring Iterative Expansion
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the UNIVERSALIST READING of the
 *   contested kernel 'all_men_created_equal.' The universalist reading holds
 *   that equality is a universal principle whose scope expands iteratively as
 *   historical consciousness broadens and excluded groups assert their
 *   rightful inclusion, regardless of founder intent or the restricted
 *   application the founding generation practiced. Under this reading, the
 *   constraint's extractiveness is moderate because scope expansion imposes
 *   coordination costs on all affected institutions — courts must develop new
 *   doctrine, legislatures must pass enabling statutes, executives must
 *   implement novel protections, and incumbent power holders must yield
 *   privileged positions. The reading generates a genuine tangled_rope
 *   structure: marginalized groups coordinate on a shared claim (inclusion
 *   under universal equality), while established power holders pay the cost
 *   of accommodation. This story does NOT adjudicate between the
 *   universalist, originalist, and textualist-paradox readings; it
 *   instantiates one of them with its own ε, beneficiary/victim structure,
 *   and directionality profile. The other readings are separate constraint
 *   stories (not authored here) linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - marginalized_groups_claiming_inclusion: Primary beneficiaries (organized, generational horizon, constrained exit) — invoke the universalist reading to justify inclusion claims
 *   - constitutional_progressives: Secondary beneficiaries (organized, generational, mobile exit) — advance the reading as interpretive authority and benefit from its institutional dominance
 *   - established_power_holders: Primary payers (institutional, generational, constrained exit) — lose exclusive privilege and resource control as scope expands
 *   - those_resisting_scope_expansion: Secondary payers (powerful, biographical, constrained exit) — defend narrower readings and bear the costs of political and legal contestation
 *   - courts_and_enforcement_apparatus: Agenda-setter (institutional, generational) — determines what counts as a valid equality claim and at what pace expansion proceeds
 *   - originalist_interpreters: Excluded (institutional, generational) — their alternative reading is foreclosed by the universalist frame's axioms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.45).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Equality as Universal Principle Requiring Iterative Expansion").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'f731d008-cfc7-423f-a1a8-b9d1171b2936').
narrative_ontology:cs_kernel_codification('f731d008-cfc7-423f-a1a8-b9d1171b2936', fixed_text).
narrative_ontology:cs_authority_grounding('f731d008-cfc7-423f-a1a8-b9d1171b2936', lineage).
narrative_ontology:cs_interpretation_layer_present('f731d008-cfc7-423f-a1a8-b9d1171b2936').
narrative_ontology:cs_reading_relation('f731d008-cfc7-423f-a1a8-b9d1171b2936', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f731d008-cfc7-423f-a1a8-b9d1171b2936', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('f731d008-cfc7-423f-a1a8-b9d1171b2936', foundational, universal_text_autonomous_from_founder_intent).
narrative_ontology:cs_axiom_status(universal_text_autonomous_from_founder_intent, holdable).
narrative_ontology:cs_axiom_grounding('f731d008-cfc7-423f-a1a8-b9d1171b2936', universal_text_autonomous_from_founder_intent, deontological).
narrative_ontology:cs_axiom('f731d008-cfc7-423f-a1a8-b9d1171b2936', foundational, equality_scope_expands_with_moral_consciousness).
narrative_ontology:cs_axiom_status(equality_scope_expands_with_moral_consciousness, holdable).
narrative_ontology:cs_axiom_grounding('f731d008-cfc7-423f-a1a8-b9d1171b2936', equality_scope_expands_with_moral_consciousness, instrumental).
narrative_ontology:cs_reference_frame('f731d008-cfc7-423f-a1a8-b9d1171b2936', universal_equality_mandate).
narrative_ontology:cs_drift_state('f731d008-cfc7-423f-a1a8-b9d1171b2936', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f731d008-cfc7-423f-a1a8-b9d1171b2936', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, constitutional_progressives).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, established_power_holders).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, those_resisting_scope_expansion).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, human_dignity_as_universal_category).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, constitutional_text_transcends_founder_intent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically excluded from the promise of equality — women, enslaved and formerly enslaved people, indigenous peoples, religious minorities, LGBTQ individuals — who invoke the universalist reading to claim standing as bearers of equal rights. They advance legal and political movements arguing that the principle's universal language mandates their inclusion regardless of founder silence or intent to the contrary. Their exit from the political system is constrained; their power derives from coalition-building and constitutional interpretation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    organized, generational, constrained, national).

% Jurists, scholars, and political actors who advance the universalist reading as both interpretive method and normative commitment. They benefit from the reading's dominance in contemporary constitutional culture; their professional and political standing is enhanced by its adoption. They possess exit options: theoretically they could abandon the reading, but doing so would fracture their institutional alliances and damage their legitimacy within progressive constitutional networks.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_progressives, beneficiary,
    organized, generational, mobile, national).

% Incumbent institutional actors whose authority and resource distribution depended on the narrower reading of equality — property owners, political majorities, state and federal governments organized around exclusionary norms. They bear the costs of scope expansion: loss of exclusive privilege, reallocation of resources, institutional restructuring, and diminished interpretive authority over constitutional meaning.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, established_power_holders, payer,
    institutional, generational, constrained, national).

% Political and legal actors — judges, legislators, organized social movements — who resist the universalist reading by advancing competing interpretations (originalist or textualist paradox framings). They resist because expansion threatens their understanding of constitutional legitimacy, their political coalitions, or their institutional interests. Their suppression mechanisms include counter-jurisprudence, legislative action, and constitutional amendment campaigns.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, those_resisting_scope_expansion, payer,
    powerful, biographical, constrained, national).

% Legal scholars and judges advancing the originalist reading as authoritative. They would argue that equality's meaning is bounded by 18th-century understandings and that expansion beyond that scope violates the separation of powers and judicial legitimacy. They are structurally excluded from the universalist reading's framing — their alternative reading is precisely what the universalist reading forecloses.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_interpreters, excluded,
    institutional, generational, constrained, national).

% Historians and constitutional scholars who document what the founders intended or understood equality to mean. They occupy an analytical seat: their work is consumed by all readings but serves no single reading's institutional interests. They measure the distance between what was intended and what the universalist reading claims the text requires.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, founding_generation_interpreters, observer,
    analytical, civilizational, analytical, national).

% The judicial and administrative machinery that interprets and enforces the universalist reading — courts that recognize new equality claims, agencies that implement expansion, enforcement officers who operationalize broadened protections. They set the pace and scope of expansion; their interpretive decisions determine what counts as a valid equality claim at any given moment.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, courts_and_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, binding principle — equality — that coordinates moral status assignment and legal protection across indefinitely expanding constituencies. The principle solves the problem of legitimating inclusion claims: absent a universal standard, each new claimant group must negotiate separately; with the principle, claims are adjudicated against a single criterion. It coordinates the answer to 'on what grounds does anyone deserve equal treatment?' with 'everyone, universally, regardless of founder intent or status at the time of founding.'
% TRANSFER_FUNCTION: Moves interpretive authority from founders and their explicit enumeration toward the text itself and its universal language. Moves material resources and rights from those who benefited from exclusion toward newly included groups. Moves political power from incumbent institutions toward courts and expansionist coalitions as arbiters of equality's scope.
% ABSENT_VOICES: Originalist interpreters are structurally excluded: their reading treats the universalist reading as a misreading, and the universalist frame does not seat them as co-interpretive authority. Populations excluded from the founding (enslaved people, women, indigenous peoples, non-property-holders) have retrospectively acquired voice through the universalist reading, but in the moment of founding their voices were absent and remain non-recoverable. The textualist paradox reading — which points out the contradiction between universal language and restricted application — is acknowledged but marginalized in the universalist frame (treated as diagnostic of the problem the reading solves, not as a competing authority).
% DISAPPEARANCE_RATIONALE: If the universalist reading disappeared and were replaced by strict originalism, constitutional legitimacy would revert to founder intent; equality claims from groups not explicitly named or intended would lose their constitutional grounding; institutional power would shift back toward legislative majorities and away from courts; expansionist coalitions would lose their primary legal mechanism for advancement. Resource distributions, civil rights protections, and political coalitions organized around the universalist reading would all reorganize around the narrower originating scope.
% FOUNDING_PROBLEM: The Declaration and Constitution use universal language ('all men are created equal,' 'equal protection') while explicitly or implicitly excluding vast populations (enslaved people, women, indigenous peoples, non-property holders). The tension between universal text and restricted application generates two problems: first, the performative contradiction — how can the text be universal if its application is bounded? Second, the legitimacy crisis — on what grounds can exclusion persist when the founding document promises inclusion to 'all'?
% FOUNDING_PROBLEM_CORROBORATION: Originalist interpreters (Scalia, Originalism; Randy Barnett, Restoring the Lost Constitution) attest the founding problem is not a problem: the text's meaning was bounded by founder understanding, and expansion beyond that is judicial usurpation. Universalist interpreters (Fiss, A Way Out of the Woods; Balkin, Living Originalism adapted; constitutional expansion jurisprudence) attest the problem is live and ongoing: the tension between text and application persists and demands resolution through interpretive expansion. Historians (David Waldstreicher, Runaway America; Pauline Maier, American Scripture) document the founding generation's ambivalence and internal disagreement about slavery and inclusion, corroborating that the problem was unresolved from the beginning. Social movements and amicus curiae briefs from excluded groups consistently invoke the problem to justify their claims.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The universalist reading sits at moderate extractiveness (0.45) because scope expansion imposes real coordination costs: legal doctrine must be developed, institutions must implement new protections, incumbent arrangements must be restructured. Suppression is moderate (0.38) because the reading faces sustained political and legal resistance; originalists and strict constructionists actively resist its further expansion, but they cannot fully suppress it because it has achieved institutional legitimacy in contemporary jurisprudence. Theater is low (0.22): the reading's coordination function (establishing a universal principle to ground inclusion claims) is genuine; the 'security review' analogy from the platform example does not apply here — the reading is what it claims to be. The measurement series trace 250 years: extractiveness rises sharply from 1776 to 1964 as the universalist reading gains institutional force and scope expands (civil rights, women's suffrage, religious freedom expansion, LGBTQ rights); it plateaus after 2000 as resistance hardens and expansion costs become visible. Suppression_requirement peaks in 1868 (post-Civil War reconstruction, maximum contestation) and declines as the reading becomes institutionally embedded. Theater_ratio rises through the 20th century as performative aspects (celebrating inclusion, ritualizing equal protection doctrine) accumulate, then stabilizes once the reading's cultural acceptance solidifies.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (marginalized groups, constitutional progressives), the universalist reading is genuine coordination: it establishes a single binding principle that their inclusion claims can invoke, replacing ad-hoc negotiation with principled adjudication. From the payer seat (established power holders, those resisting expansion), the same structure appears as coercive reinterpretation: the reading rewrites the constitution's meaning beyond the founders' intent, extracting privilege and authority they believed were secured. The engine computes this divergence from the stakeholder power levels, exit options, and beneficiary/victim declarations — the reading does not resolve it, the structure produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups have organized power and constrained exit (they cannot leave the national political system to escape the constraint; inclusion is what they are fighting for). They are structural beneficiaries (d near 0.0): the universalist reading directly benefits them. Constitutional progressives are organized and have mobile exit (theoretically they could abandon progressivism) but their professional standing and institutional alliances are invested in the reading; they are beneficiaries (d moderately low, ~0.2). Established power holders are institutional and face constrained exit (they cannot simply abandon the constitutional system); they are targets (d near 1.0). Those resisting scope expansion are powerful but not institutional in the same sense; they face moderate exit options (political defection, building opposing coalitions); they are partially targeted (d moderately high, ~0.7). Courts sit as agenda-setter with analytical exit, computing d from the structural data rather than experiencing the constraint themselves. The directionality profile explains why courts face pressure from marginalized groups claiming the universalist reading (low-d beneficiaries mobilizing for inclusion) and resistance from originalists (high-d payers defending narrower scope).
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading avoids mandatrophy miscoding by anchoring its mandate to an identifiable present-day problem: the unresolved tension between universal text and restricted application. This tension remains live because: (1) new populations continue to claim inclusion (LGBTQ individuals, undocumented immigrants, disabled people), (2) originalists and textualists continue to resist the reading's authority, and (3) the coordination function (establishing universal equality as the grounding for inclusion claims) remains functional — it solves a real problem for claimant groups. If the universalist reading were treated as a Piton, the theater_ratio would be much higher (~0.7+) because the reading would be maintained primarily for performative reasons; instead it sits at 0.22 because the coordination function is genuine. The measurement plateau after 2000 reflects not mandate atrophy but rather institutionalization: the reading is no longer actively expanding (resistance has hardened), so the rate of change flattens, but the reading itself remains functional as a legal and political framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_intent_vs_text_autonomy,
    'Does the Constitution''s text possess an autonomous meaning independent of founder intent, or is the text always and only the founder''s intended meaning?',
    'This is a conceptual question about the nature of textual interpretation and constitutional authority. Different jurisprudential schools (living constitutionalism, originalism, textual realism) give different answers. No empirical data can resolve it; it is decided by which interpretive paradigm the relevant institutional actors adopt.',
    'If text is autonomous from intent, the universalist reading''s claim to expansion authority is strengthened — the text''s universal language mandates expansion regardless of what founders intended. If text is bound to intent, the universalist reading loses its primary normative anchor and becomes a disguised policy preference rather than faithful constitutional interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_intent_vs_text_autonomy, conceptual, 'The irreducible interpretive question about whether constitutional meaning is textually autonomous or intent-bound.').

omega_variable(
    expansion_velocity_and_institutional_capacity,
    'How fast can equality''s scope expand without exhausting institutional capacity to implement protections and without triggering backlash that destabilizes the constitutional order?',
    'Empirical observation: track the rate of successful expansion claims relative to institutional reaction and countermobilization. Acceleration that outpaces implementation capacity or triggers constitutional amendment campaigns suggests a threshold.',
    'If expansion velocity exceeds institutional capacity, the universalist reading itself becomes unsustainable — courts and legislatures simply cannot keep pace, and the reading loses efficacy. Severe backlash could trigger constitutional amendment to re-narrow equality''s scope, turning the expansion into a temporary cycle rather than irreversible progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_velocity_and_institutional_capacity, empirical, 'Whether the pace of scope expansion can be sustained indefinitely or faces endogenous institutional limits.').

omega_variable(
    benefit_distribution_across_claimant_groups,
    'Do all marginalized groups claiming inclusion benefit equally from the universalist reading, or does the reading''s framework advantage some groups over others?',
    'Historical and comparative analysis: track success rates of inclusion claims across groups (women''s suffrage, civil rights, LGBTQ rights, disability access, immigrant rights). Measure whether all groups achieve inclusion at similar speed and completeness.',
    'If benefits are unevenly distributed, the universalist reading may not constitute genuine coordination across marginalized groups — it may coordinate some groups'' inclusion while leaving others behind, generating internal contestation and revealing the reading itself as extractive toward less-advantaged claimant groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_distribution_across_claimant_groups, empirical, 'Whether the universalist reading distributes its coordination benefits uniformly across all claimant groups or systematically advantages some.').

omega_variable(
    universalist_vs_originalist_foreclosure,
    'Do the universalist and originalist readings logically foreclose each other, or can both be held within a single interpretive framework?',
    'Examine whether a jurist or theorist could simultaneously hold: (1) the text''s universal language mandates expansion-ready interpretation (universalist) AND (2) the text''s meaning is fixed by founder intent (originalist). If this is logically impossible, they foreclose each other; if jurists actually hold both in some form (e.g., ''originalism for rights, living constitutionalism for structural provisions''), they coexist.',
    'If they foreclose each other, the constraint story instantiates a kernel reading whose fundamental premises directly contradict its siblings — no single constitutional framework could hold all three readings. If they coexist, the readings differ in axioms and application but share enough conceptual ground to compete within the same jurisprudential discourse without logical explosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalist_vs_originalist_foreclosure, conceptual, 'Whether the universalist and originalist readings are logically incompatible (foreclosed) or merely competing within a shared framework (coexisting).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement_basis(all__tr_t1776, projected).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__universalist_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement_basis(all__tr_t1868, observed).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.16).
narrative_ontology:measurement_basis(all__tr_t1920, observed).
narrative_ontology:measurement(all__tr_t1964, all_men_created_equal__universalist_reading, theater_ratio, 1964, 0.24).
narrative_ontology:measurement_basis(all__tr_t1964, observed).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(all__tr_t2000, observed).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__universalist_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(all__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement_basis(all__be_t1776, projected).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__universalist_reading, base_extractiveness, 1868, 0.35).
narrative_ontology:measurement_basis(all__be_t1868, observed).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement_basis(all__be_t1920, observed).
narrative_ontology:measurement(all__be_t1964, all_men_created_equal__universalist_reading, base_extractiveness, 1964, 0.48).
narrative_ontology:measurement_basis(all__be_t1964, observed).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(all__be_t2000, observed).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__universalist_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(all__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.25).
narrative_ontology:measurement_basis(all__su_t1776, projected).
narrative_ontology:measurement(all__su_t1868, all_men_created_equal__universalist_reading, suppression_requirement, 1868, 0.52).
narrative_ontology:measurement_basis(all__su_t1868, observed).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement_basis(all__su_t1920, observed).
narrative_ontology:measurement(all__su_t1964, all_men_created_equal__universalist_reading, suppression_requirement, 1964, 0.42).
narrative_ontology:measurement_basis(all__su_t1964, observed).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement_basis(all__su_t2000, observed).
narrative_ontology:measurement(all__su_t2026, all_men_created_equal__universalist_reading, suppression_requirement, 2026, 0.38).
narrative_ontology:measurement_basis(all__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'all_men_created_equal.' The kernel itself cannot be a single constraint because its different readings instantiate different ε values, beneficiary/victim structures, and directionality profiles. The universalist_reading instantiates moderate extractiveness (0.45) with marginalized groups as beneficiaries and power holders as payers. The originalist_reading (separate story) instantiates much lower extractiveness by treating founder intent as binding. The textualist_paradox_reading (separate story) instantiates higher extractiveness by treating the tension between text and application as unresolvable. All three stories share the same kernel (the text 'all men are created equal'); they differ in how they read the text's scope and authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
