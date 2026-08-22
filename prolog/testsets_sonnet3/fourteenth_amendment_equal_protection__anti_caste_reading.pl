% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection as Anti-Caste Mandate (Anti-Subordination Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the anti-caste (anti-subordination) reading of
 *   the Fourteenth Amendment's Equal Protection Clause: the Clause is read to
 *   require the state to actively dismantle racial, gender, and status
 *   hierarchy, not merely to refrain from explicit classification. Under this
 *   reading, group status may permissibly enter state decision-making as a
 *   remedial tool, structural inequality itself becomes a constitutional
 *   wrong the state must address, and disparate impact — not only disparate
 *   treatment — triggers scrutiny. This is a distinct constraint from the
 *   sibling formal-equality reading (constraint_id: formal_equality_reading,
 *   not authored here), which holds that the Clause forbids state
 *   racial/status classification absent compelling justification regardless
 *   of remedial intent. The two readings share the constitutional text and
 *   history but diverge sharply on whether group-conscious state action is
 *   the Clause's mandate or its violation — they are linked via
 *   network.affects_constraints and each carries its own ε, since asking 'how
 *   extractive is Equal Protection' produces different answers depending on
 *   which reading answers it (ε-invariance requires two files, not one
 *   averaged constraint).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection as Anti-Caste Mandate (Anti-Subordination Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'f67457ca-899d-492b-a0a1-760e9f49341d').
narrative_ontology:cs_kernel_codification('f67457ca-899d-492b-a0a1-760e9f49341d', fixed_text).
narrative_ontology:cs_authority_grounding('f67457ca-899d-492b-a0a1-760e9f49341d', lineage).
narrative_ontology:cs_interpretation_layer_present('f67457ca-899d-492b-a0a1-760e9f49341d').
narrative_ontology:cs_reading_relation('f67457ca-899d-492b-a0a1-760e9f49341d', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('f67457ca-899d-492b-a0a1-760e9f49341d', foundational, equal_protection_targets_group_hierarchy_not_classification).
narrative_ontology:cs_axiom_status(equal_protection_targets_group_hierarchy_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('f67457ca-899d-492b-a0a1-760e9f49341d', equal_protection_targets_group_hierarchy_not_classification, deontological).
narrative_ontology:cs_axiom('f67457ca-899d-492b-a0a1-760e9f49341d', secondary, state_action_may_use_group_status_as_remedial_tool).
narrative_ontology:cs_axiom_status(state_action_may_use_group_status_as_remedial_tool, holdable).
narrative_ontology:cs_axiom_grounding('f67457ca-899d-492b-a0a1-760e9f49341d', state_action_may_use_group_status_as_remedial_tool, instrumental).
narrative_ontology:cs_reference_frame('f67457ca-899d-492b-a0a1-760e9f49341d', reconstruction_era_anti_subordination_purpose).
narrative_ontology:cs_drift_state('f67457ca-899d-492b-a0a1-760e9f49341d', contemporary_colorblind_jurisprudence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f67457ca-899d-492b-a0a1-760e9f49341d', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, racial_minority_communities).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, women_facing_structural_discrimination).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_status_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_agencies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, nonminority_applicants_in_remedial_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, municipalities_bearing_compliance_costs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, private_employers_under_disparate_impact_liability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have historically been subject to state-sanctioned and state-tolerated caste hierarchy (segregation, redlining, disenfranchisement). Under this reading, they are the intended beneficiaries of affirmative remedial programs — set-asides, disparate-impact liability, majority-minority districting, integration mandates. Their exit from the hierarchy depends on the state actively dismantling it rather than merely refraining from new discrimination.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, racial_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Benefit from heightened scrutiny of gender classifications read as anti-subordination doctrine (VMI, Frontiero-line reasoning) rather than purely formal symmetry. Structural inequality in wages, caregiving burden, and institutional access is treated as a constitutional concern the state must address, not merely a private matter it must avoid causing.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women_facing_structural_discrimination, beneficiary,
    organized, generational, constrained, national).

% Includes groups subordinated by status markers beyond race/sex where courts and legislatures extend the anti-caste logic (disability, in some readings sexual orientation). They gain standing to claim that facially neutral rules perpetuating group subordination are themselves constitutionally suspect.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_status_groups, beneficiary,
    moderate, generational, constrained, national).

% Federal and state civil rights offices, courts applying disparate-impact doctrine, and legislatures authorizing affirmative action administer and expand the anti-caste reading. They set compliance standards, bring enforcement actions, and define what counts as impermissible structural subordination. Their institutional mandate and budget depend on the reading remaining operative.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, civil_rights_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals denied admission, contracts, or employment positions because a remedial program under this reading weighted race, sex, or status as a corrective factor. They bear a concentrated, identifiable cost for a diffuse historical wrong they did not personally cause; their only recourse is litigation challenging the specific program's tailoring.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, nonminority_applicants_in_remedial_programs, payer,
    moderate, biographical, constrained, national).

% Local governments must fund desegregation orders, monitor housing and hiring patterns for disparate impact, and defend remedial policies in court. Compliance consumes budget and administrative capacity that would otherwise fund other services; noncompliance risks federal funding cutoffs or judicial oversight.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, municipalities_bearing_compliance_costs, payer,
    moderate, biographical, constrained, regional).

% Face liability not only for intentional discrimination but for facially neutral practices (testing regimes, credit checks, seniority systems) that perpetuate group disparities, unless justified by business necessity. They absorb litigation risk and compliance overhead as the price of operating under the anti-caste standard.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, private_employers_under_disparate_impact_liability, payer,
    powerful, biographical, constrained, national).

% Judges, scholars, and litigants committed to colorblind constitutionalism would object that this reading authorizes exactly the racial classification the Equal Protection Clause was meant to forbid. Within courts and jurisdictions where the anti-caste reading is dominant doctrine, their framework has no operative purchase — they must litigate the kernel itself to be heard, which is the subject of the sibling reading, not this constraint.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_adherents, excluded,
    organized, generational, trapped, national).

% Analyze doctrinal drift between the anti-subordination and anticlassification readings across case law eras (Warren/Burger Court expansion, Rehnquist/Roberts Court contraction), documenting which reading dominates in which period and why.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, diffuse).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action toward dismantling entrenched group hierarchies that private ordering and formal nondiscrimination rules alone have failed to unwind — school integration, employment access, political representation — by authorizing the state to take group status into account as a remedial tool.
% TRANSFER_FUNCTION: Moves opportunities, seats, contracts, and institutional positions from applicants who would prevail under a pure merit or seniority baseline to members of groups the state has determined bear the ongoing effects of historical subordination; also moves compliance costs from subordinated communities to institutions administering the contested resource.
% ABSENT_VOICES: Adherents of the formal-equality reading are structurally excluded from this reading's operative logic wherever it is doctrinally dominant — their objection that the remedy replicates the classification the Clause forbids cannot be raised as a defeater within this reading, only as a claim that a different reading should govern instead.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading disappeared overnight, affirmative action programs, disparate-impact liability, and structural remedial decrees (school integration orders, minority set-asides, gender-conscious remedies) would lose their constitutional predicate; enforcement agencies would lose their doctrinal basis for compelling group-conscious remedies, and beneficiary groups would need to rely on statutory or formal-equality claims instead, which have different (generally narrower) reach.
% FOUNDING_PROBLEM: Formal legal equality after Reconstruction and again after Brown proved insufficient to dismantle entrenched racial and status hierarchy: facially neutral rules (literacy tests, seniority systems, single-family zoning) reproduced subordination without any explicit classification, and hierarchy persisted despite the formal removal of legal barriers.
% FOUNDING_PROBLEM_CORROBORATION: Empirical sociologists and economists studying persistent racial wealth, wage, and residential segregation gaps attest the underlying structural subordination problem remains substantially live, independent of the reading's beneficiary groups. Formal-equality adherents and some minority scholars (e.g., certain colorblind-conservative Black legal scholars) dispute that state group-conscious remedy is the correct or still-necessary tool, arguing the problem has shifted in character since the doctrine's founding era.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the concentrated, identifiable costs this reading imposes on nonminority individuals denied specific opportunities under remedial programs, and on employers and municipalities bearing disparate-impact compliance costs — real transfers, not merely diffuse social cost. It sits below a pure snare reading because the coordination function (dismantling entrenched, empirically documented hierarchy) is genuine and the beneficiary class is broad and historically well-evidenced, not a narrow capture group. Suppression (0.42) is moderate: the reading does not forbid dissenting legal argument, but it does require compliance once a program or decree issues, backed by judicial and federal funding enforcement. Resistance is high (0.72) because this reading has been continuously and vigorously contested in courts, legislatures, and public discourse since its articulation — it has never achieved uncontested settlement the way a mountain claim would. Accessibility collapse is moderate-low (0.35): the formal-equality alternative remains a fully live, litigable framework, not foreclosed as a practical matter, which is itself why this constraint requires a sibling story rather than treatment as a settled kernel.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary-group seat, the reading looks like overdue correction of a documented, persistent wrong the state itself helped construct. From the nonminority-applicant-payer seat, the same doctrine looks like state-sanctioned classification imposing a concentrated, individually borne cost for a collective historical debt. The engine computes these as different seat-level types from the same structural data; neither seat's perception is treated as the story's verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated groups (racial minorities, women facing structural discrimination, other status groups) are coded as beneficiaries because this reading was constructed specifically to authorize state action on their behalf — their directionality sits near the beneficiary end even though many members do not personally receive a remedial benefit in any given case, because the constraint's design targets them as a class. Nonminority applicants denied a specific remedial-program seat, and institutions bearing compliance costs, are coded as payers/targets — the transfer is concrete and traceable to the doctrine's operation. Enforcement agencies are agenda-setters: they administer, expand, and depend institutionally on the reading's continued operation, which is a different relationship than either beneficiary or payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that formal legal equality proved insufficient against facially neutral hierarchy-reproducing rules — remains empirically corroborated by persistent structural gaps in wealth, housing, and political representation (attested by researchers outside the beneficiary set). This blocks a simple mandatrophy verdict (dead problem, live mandate) but does not resolve the doctrine into a mountain: the founding_problem_status is authored as 'contested' because a substantial body of opinion, including from within some subordinated communities, argues the character of the problem has shifted since 1954 in ways state group-conscious remedy no longer well addresses. The classification as tangled_rope rather than snare or rope reflects that: genuine coordination function (addressing documented structural hierarchy) coexists with genuine, non-trivial cost concentrated on identifiable non-beneficiary parties, and the arrangement persists only through active judicial and administrative enforcement — exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_caste_vs_formal_equality_locus,
    'Is the disagreement between the anti-caste and formal-equality readings located in the meaning of ''equal protection'' itself, or in a downstream empirical dispute about whether facially neutral rules currently perpetuate group subordination?',
    'Track whether formal-equality adherents and anti-caste adherents converge when presented with the same empirical evidence of a specific facially neutral rule''s disparate effects — if convergence occurs, the disagreement is substantially empirical; if it persists despite evidentiary agreement, the disagreement is in the interpretive premise itself.',
    'If the disagreement is substantially empirical, the two readings could in principle converge as evidence accumulates, meaning this is not a permanent kernel split but a temporarily bifurcated reading. If it is genuinely a premise-level split (outcome-focus vs. mechanism-focus), the two constraints remain permanently distinct regardless of evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_caste_vs_formal_equality_locus, conceptual, 'Whether the anti-caste/formal-equality split is empirical or interpretive at its root.').

omega_variable(
    remedial_program_sunset_question,
    'Does the anti-caste reading contemplate its own remedial programs as permanently justified by ongoing structural hierarchy, or as scaffolded correctives meant to sunset once measurable disparities close?',
    'Examine whether courts applying this reading have ever required or upheld sunset provisions in race-conscious remedial programs (e.g., narrow-tailoring review requiring periodic reassessment) versus treating the remedial authority as indefinite.',
    'If sunset is doctrinally required, this reading functions closer to a scaffold (transitional, justified by the transition) for specific programs even while the general interpretive reading persists as tangled_rope; if remedial authority is treated as indefinite so long as disparity persists, the tangled_rope classification is more stable across the doctrine''s lifespan.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_program_sunset_question, empirical, 'Whether remedial programs under this reading are structurally transitional or indefinite.').

omega_variable(
    beneficiary_class_heterogeneity,
    'Do all members of the declared beneficiary groups (racial minority communities, women, other status groups) actually benefit from group-conscious remedial programs, or do intra-group class and status differences mean the concrete beneficiaries are a narrower subset (e.g., already-advantaged members positioned to capture set-asides or preferences)?',
    'Empirical distributional analysis of who actually receives seats, contracts, or positions under specific remedial programs, disaggregated by intra-group class position.',
    'If benefits concentrate among already-advantaged subgroup members, the beneficiary declaration overstates diffuse group benefit and understates a narrower capture dynamic, which would push the classification toward a more concentrated extraction structure for any given program even while the doctrine-level reading remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_heterogeneity, empirical, 'Whether declared group beneficiaries are the actual concrete beneficiaries of specific programs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(four_tr_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(four_tr_t1980, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.35).
narrative_ontology:measurement(four_be_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1968, 0.48).
narrative_ontology:measurement(four_be_t1980, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.55).
narrative_ontology:measurement(four_su_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1968, 0.65).
narrative_ontology:measurement(four_su_t1980, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__anti_caste_reading, 0.12).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_reading).

% DUAL FORMULATION NOTE:
% This constraint and formal_equality_reading are the two competing readings of the fourteenth_amendment_equal_protection kernel. Each authors its own ε: this reading (anti-caste) authors ε=0.58 reflecting substantial, concentrated remedial-program costs against a genuine hierarchy-dismantling coordination function; the sibling reading authors its own independent ε reflecting the cost structure formal-equality adherents identify (classification harm, administrability costs) rather than an average or blend of the two. Neither file's ε is adjusted to reconcile with the other — per the ε-invariance principle, the natural-language label 'Equal Protection' names two structurally distinct constraints, decomposed into two stories linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
