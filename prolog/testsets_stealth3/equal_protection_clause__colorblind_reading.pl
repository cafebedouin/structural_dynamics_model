% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading — Anti-Classification Rule
 *   domain: constitutional law/political philosophy/education policy
 *
 * SUMMARY:
 *   This story instantiates the colorblind reading of the Fourteenth
 *   Amendment's Equal Protection Clause: government may never sort
 *   individuals by race, and the clause's rights-bearers are persons, not
 *   groups. The rule is administered by the federal judiciary through strict
 *   scrutiny, protects every individual within national jurisdiction against
 *   governmental racial classification (including hostile classifications
 *   directed at minorities), and in its contemporary operation forecloses the
 *   race-conscious instruments favored by public universities, state
 *   agencies, and civil-rights organizations. The claim/metric gap is
 *   deliberate and small here: the reading is CLAIMED as rope (a stable
 *   coordination rule with universal individual beneficiaries) and the
 *   authored metrics describe genuinely low-extraction, actively enforced,
 *   lightly theatrical operation — the engine computes per-seat
 *   classifications from the structural data and may diverge at the
 *   cost-bearing seat. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda setter (institutional/analytical) — composes
 *   and enforces the anti-classification doctrine -
 *   individual_rights_bearers_of_every_race: universal beneficiary
 *   (moderate/constrained) — holds the guarantee against any governmental
 *   sorting - college_applicants_suing_for_colorblind_admissions: concrete
 *   contemporary beneficiary (organized/constrained) — litigates against
 *   race-conscious admissions - race_conscious_policy_proponents: primary
 *   cost-bearing seat (organized/constrained) — universities, agencies, and
 *   advocacy organizations whose race-conscious instruments are invalidated -
 *   historically_subordinated_individuals: dual-positioned seat
 *   (moderate/trapped) — protected by the rule against hostile classification
 *   while barred from deploying it remedially
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional power, analytical exit, national scope) — interprets the equal protection guarantee, applies strict scrutiny to racial classifications, and composes the doctrine that defines what counts as a forbidden classification
 *   - individual_rights_bearers_of_every_race: beneficiary (moderate power, constrained exit, generational horizon, national scope) — every person within jurisdiction holds the guarantee that the state will not distribute burdens or benefits by ancestry; they cannot exit constitutional jurisdiction, but the rule travels protectively with them
 *   - college_applicants_suing_for_colorblind_admissions: beneficiary (organized power, constrained exit, biographical horizon, national scope) — applicants who litigate through membership organizations against race-conscious admissions; their remedy is doctrinal, not exit
 *   - race_conscious_policy_proponents: payer (organized power, constrained exit, generational horizon, national scope) — public university systems, state and local agencies, and civil-rights organizations whose preferred governance instruments are struck down; they bear invalidation, redesign costs, and recurring litigation losses, and cannot opt out of the constitutional constraint
 *   - historically_subordinated_individuals: beneficiary with secondary payer position (moderate power, trapped exit, generational horizon, national scope) — members of historically subordinated groups are shielded by the rule against hostile classification while simultaneously unable to invoke race-conscious remediation for disparities the rule declines to reach; they have no exit from jurisdiction or from the legacy the rule leaves unaddressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.3).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Colorblind Reading — Anti-Classification Rule").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional law/political philosophy/education policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'c3b9ce2d-480c-4f66-9477-c528e7e85e76').
narrative_ontology:cs_kernel_codification('c3b9ce2d-480c-4f66-9477-c528e7e85e76', fixed_text).
narrative_ontology:cs_authority_grounding('c3b9ce2d-480c-4f66-9477-c528e7e85e76', lineage).
narrative_ontology:cs_interpretation_layer_present('c3b9ce2d-480c-4f66-9477-c528e7e85e76').
narrative_ontology:cs_reading_relation('c3b9ce2d-480c-4f66-9477-c528e7e85e76', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('c3b9ce2d-480c-4f66-9477-c528e7e85e76', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('c3b9ce2d-480c-4f66-9477-c528e7e85e76', foundational, racial_classifications_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('c3b9ce2d-480c-4f66-9477-c528e7e85e76', racial_classifications_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('c3b9ce2d-480c-4f66-9477-c528e7e85e76', foundational, individual_not_group_is_rights_bearer).
narrative_ontology:cs_axiom_status(individual_not_group_is_rights_bearer, holdable).
narrative_ontology:cs_axiom_grounding('c3b9ce2d-480c-4f66-9477-c528e7e85e76', individual_not_group_is_rights_bearer, deontological).
narrative_ontology:cs_reference_frame('c3b9ce2d-480c-4f66-9477-c528e7e85e76', colorblind_civic_equality_baseline).
narrative_ontology:cs_drift_state('c3b9ce2d-480c-4f66-9477-c528e7e85e76', contemporary_post_sffa, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c3b9ce2d-480c-4f66-9477-c528e7e85e76', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_rights_bearers_of_every_race).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, college_applicants_suing_for_colorblind_admissions).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, race_conscious_policy_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, historically_subordinated_individuals).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, historically_subordinated_individuals).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, colorblind_constitution_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, strict_scrutiny_for_racial_classifications).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_equality_before_the_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the equal protection guarantee and composes the doctrine that defines what counts as a forbidden racial classification. Strikes down offending statutes and programs, awards injunctive relief, and refines the applicable tests term by term. Gains doctrinal authority from administering the rule; bears none of its compliance costs; answers to appointment pipelines rather than to any party the rule protects or burdens.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Every person within national jurisdiction holds the guarantee that government will not distribute burdens, benefits, penalties, or privileges by reference to ancestry or skin color. The guarantee follows them into admissions offices, jury rooms, contracting processes, and electoral maps. They cannot exit constitutional jurisdiction, but the rule constrains the state rather than them, and its protection does not depend on which group they belong to.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_rights_bearers_of_every_race, beneficiary,
    moderate, generational, constrained, national).

% Applicants to selective public and private universities who litigate, through membership organizations, against admissions processes that weigh race. They seek doctrinal invalidation rather than exit — applying elsewhere does not answer their objection — and their wins dismantle the challenged processes for all subsequent applicants.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, college_applicants_suing_for_colorblind_admissions, beneficiary,
    organized, biographical, constrained, national).

% Public university systems, state and local agencies, and civil-rights organizations that design and defend programs allocating opportunities by race — admissions preferences, set-asides, targeted outreach regimes. The rule invalidates their preferred instruments, forces repeated redesign around race-neutral proxies, and exposes them to recurring litigation they frequently lose. They operate entirely inside national constitutional jurisdiction and have no arbitrage path around the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, race_conscious_policy_proponents, payer,
    organized, generational, constrained, national).

% Members of historically subordinated racial groups hold the rule's protection against hostile classification — the same guarantee every individual holds — while simultaneously being unable to invoke race-conscious remediation for disparities that trace to past official subordination. They have no exit from national jurisdiction and none from the accumulated legacy the rule declines to reach; they experience the constraint as shield and bar at once.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_individuals, beneficiary,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, historically_subordinated_individuals, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single, administrable boundary on state action: government must allocate burdens and benefits without reference to citizens' race. This solves a recurring collective-action problem — preventing political majorities from capturing state machinery for racial faction — and gives every citizen the same guarantee, replacing case-by-case negotiation over whose classification counts as benign with one rule applied to all.
% TRANSFER_FUNCTION: Moves discretion over the use of racial categories away from political majorities and program administrators and toward individual rights-bearers enforceable in court; converts race-targeted policy capacity into race-neutral policy capacity. No money or goods move; what moves is decision rights and instrument availability.
% ABSENT_VOICES: Constituencies of the sibling readings object from outside the governing frame: remedial-reading advocates argue the rule freezes historical injustice in place by forbidding the tools that would address it, and diversity-reading educators argue it strips institutions of a pedagogical interest they hold compelling. They appear as litigants, dissenting justices, and academic critics rather than as governing voices. The Reconstruction-generation framers whose mixed record both sides claim are historically unavailable.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, governments could immediately resume sorting by race: admissions, public contracting, districting, policing, and benefits administration would reorganize around whichever coalitions held power, and the surrounding architecture of antidiscrimination law built on the anti-classification core would lose its foundation. Every seated party's position depends on the rule's existence — as protection, as foreclosure, or as jurisdiction.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to solve the problem of state racial caste: Southern states deploying law to subordinate the formerly enslaved through Black Codes, denied legal capacity, and racially allocated rights and punishments. The colorblind reading frames that founding problem as governmental sorting by race itself, and reads the amendment as a permanent ban on the practice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the Reconstruction congressional record and ratification-era sources attest the founding problem of state racial caste; a century of enforcement history (school segregation litigation, anti-miscegenation cases, jury-selection and redistricting cases) attests its recurrence whenever the rule's guard slackened; and the sibling readings' own adherents concede the founding problem was and is real while disputing the prescribed remedy. No party to the contemporary contest denies that state racial classification is a live danger; they dispute only its proper treatment.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12 at interval end) because the rule transfers no resource stream: its costs are foreclosed policy options and litigation losses, not captured goods, and no seat collects what it takes. Suppression (0.30) is real but narrow — judicial enforcement and litigation chill close one instrument class (race-conscious classification) while leaving the large space of race-neutral substitutes open and compliance cheap. Theater is low (0.15): the doctrine genuinely binds, and challenged programs actually change or die. Accessibility_collapse (0.55) reflects partial closure: once the rule is understood, race-conscious options are legally closed, but race-neutral proxies and the live sibling readings keep the option space from collapsing entirely. Resistance (0.60) is high because this is among the most continuously contested questions in the domain — sibling-reading constituencies fight it in every branch and every term. The temporal series run on one shared eight-point grid. The suppression_requirement series is authored deliberately to trace enforcement-capacity dynamics: enforcement demand climbed as race-conscious policymaking proliferated mid-interval, dipped slightly at the doctrinal accommodation around t=50, and resumed climbing as post-SFFA enforcement waves began. Extractiveness drifts gently upward across the interval, reflecting the shift in the rule's operative incidence from blocking subordinating classifications toward blocking remedial ones — a shift critics read as rising extraction and the reading itself reads as constant low-cost rule application.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the applicant-beneficiary seat the rule is pure protection: a guarantee that no committee will weigh ancestry against them. From the proponent seat the same rule operates as foreclosure — the invalidation of instruments they regard as necessary and legitimate, with recurring material losses. The historically-subordinated seat holds both experiences simultaneously: shield against hostile classification, bar against remedial deployment. The judiciary seat experiences administration and doctrinal authority accrual rather than either protection or burden. The engine derives these divergences from the declared positions; the authored rope claim records the author's structural judgment that coordination dominates, and does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the beneficiary end of d: the universal rights-bearer class and the litigating applicants receive the rule's protection without bearing its costs. The declared cost-bearing group (race_conscious_policy_proponents) sits near the target end: the rule's operative burden lands on them as invalidation and litigation exposure, and their exit is constrained — they operate inside national constitutional jurisdiction with no arbitrage. Historically_subordinated_individuals are dual-declared, pulling their derived d toward the middle. The judiciary carries a directionality override (institutional -> 0.30): the derivation chain has no beneficiary/victim data for the administrative seat, and the canonical fallback would misread administration as target position; the override records that the judiciary mildly gains authority from the constraint it administers while bearing none of its costs. National scope amplifies effective extraction modestly in the engine's arithmetic; suppression is authored as a raw structural property and is not scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — governmental deployment of racial caste — remains live, and the rule has not outlived its function; founding_problem_status is live and disappearance_verdict is world_rearranges, so no dead-mandate/zombie flag arises. The classification discipline matters here in both directions. Adherents market the rule as mountain-like — self-evident, eternal, beyond politics — and the authored data refuses that framing: the constraint is constructed, actively enforced, and heavily resisted, not a natural limit. Critics market it as snare-like — an extraction machine for dominant groups — and the authored data refuses that too: extraction is low, no seat collects what the rule takes (receipt is diffuse), and the rule's protection extends to the critics' own constituents. The rope claim keeps the genuine coordination function visible (a stable, administrable boundary on state racial sorting that all individuals hold) while the per-seat computation registers the proponent-seat cost asymmetry for comparison against the sibling readings, where that asymmetry is theorized as the rule's core defect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the colorblind reading of the equal_protection_clause kernel; what structural differences would the remedial and diversity sibling readings introduce if either became the governing reading?',
    'Doctrinal succession through Supreme Court majority realignment, or Article V amendment adopting a sibling reading; tracked via appointment pipelines, certiorari grants in race-classification cases, and state legislative responses.',
    'Under the remedial reading the beneficiary set becomes historically subordinated groups, race-conscious policy proponents become coordinated actors rather than cost-bearers, and epsilon rises substantially; under the diversity reading beneficiaries become student bodies as collectives and epsilon rises moderately. Cross-reading comparison of the three stories is the designed measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    permanence_vs_doctrinal_reversibility,
    'Is the colorblind rule a permanent structural feature of the constitutional order (race never relevant) or a doctrinal settlement reversible by future judicial majorities?',
    'Track judicial appointment pipelines, certiorari activity in racial-classification cases, and durability of post-SFFA precedents across successive terms.',
    'If reversible, the constraint''s persistence tracks judicial composition rather than settled structure, and its classification should weight mutability heavily; if durable across compositional change, the permanence claim strengthens and the constraint behaves as fixed infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_doctrinal_reversibility, empirical, 'Whether the reading''s permanence claim survives compositional turnover.').

omega_variable(
    formal_symmetry_operative_incidence,
    'Despite formal symmetry, does the rule''s operative burden in practice fall almost entirely on minority-preferential classifications, leaving inherited distributions untouched?',
    'Code all equal-protection racial-classification challenges across the interval by policy valence (burden-imposing versus benefit-extending) and outcome; compare challenge rates and success rates across valence classes.',
    'Strong asymmetry raises effective extraction at the seats of historically subordinated individuals and pushes per-seat computation toward hybrid coordination/extraction; demonstrated symmetry supports the pure-coordination reading and the authored rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_symmetry_operative_incidence, empirical, 'Whether formally symmetric operation is asymmetric in incidence.').

omega_variable(
    original_meaning_remediation_authorization,
    'Does the Fourteenth Amendment''s original public meaning mandate colorblindness, or does it authorize race-conscious remediation of the subordination it abolished?',
    'Historical-linguistic analysis of the 39th Congress debates, ratification-era state constitutions, and contemporaneous freedmen''s Bureau legislation and school provisions.',
    'If remediation was authorized at founding, the colorblind reading loses lineage priority to the remedial sibling and its authority grounding weakens; if the founding record is colorblind, this reading''s permanence and lineage claims strengthen considerably.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_meaning_remediation_authorization, empirical, 'Founding-era evidence contest between colorblind and remedial lineage claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__colorblind_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__colorblind_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_clause__colorblind_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__colorblind_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equal_protection_clause__colorblind_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement_basis(equa_tr_t50, observed).
narrative_ontology:measurement(equa_tr_t60, equal_protection_clause__colorblind_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement_basis(equa_tr_t60, observed).
narrative_ontology:measurement(equa_tr_t70, equal_protection_clause__colorblind_reading, theater_ratio, 70, 0.15).
narrative_ontology:measurement_basis(equa_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__colorblind_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__colorblind_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_clause__colorblind_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__colorblind_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equal_protection_clause__colorblind_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement_basis(equa_be_t50, observed).
narrative_ontology:measurement(equa_be_t60, equal_protection_clause__colorblind_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement_basis(equa_be_t60, observed).
narrative_ontology:measurement(equa_be_t70, equal_protection_clause__colorblind_reading, base_extractiveness, 70, 0.12).
narrative_ontology:measurement_basis(equa_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__colorblind_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__colorblind_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_clause__colorblind_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__colorblind_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equal_protection_clause__colorblind_reading, suppression_requirement, 50, 0.26).
narrative_ontology:measurement_basis(equa_su_t50, observed).
narrative_ontology:measurement(equa_su_t60, equal_protection_clause__colorblind_reading, suppression_requirement, 60, 0.27).
narrative_ontology:measurement_basis(equa_su_t60, observed).
narrative_ontology:measurement(equa_su_t70, equal_protection_clause__colorblind_reading, suppression_requirement, 70, 0.3).
narrative_ontology:measurement_basis(equa_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'equal protection' covers three structurally distinct claims that share one constitutional text. This story (colorblind_reading) decomposes the label per the epsilon-invariance principle: the colorblind rule has very low epsilon, universal individual beneficiaries, and an unconditional prohibition; the remedial sibling has high epsilon, group beneficiaries, and a remediation mandate; the diversity sibling sits between, with collective educational beneficiaries and a permissive compelling-interest test. The colorblind reading currently exerts upstream pressure on both siblings — its recent doctrinal victories have narrowed the diversity reading's operating space and foreclosed the remedial reading's preferred instruments — which is why the influence edges run from this story outward. Each member links to the others; no member averages over the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__colorblind_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
