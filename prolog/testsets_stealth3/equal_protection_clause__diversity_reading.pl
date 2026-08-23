% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_equal_protection_clause__diversity_reading, []).

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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Diversity Reading — Race-Conscious Admissions Authorized by Compelling Educational Interest
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   From Regents v. Bakke (1978) through Grutter v. Bollinger (2003) to SFFA
 *   v. Harvard (2023), the diversity reading of the Equal Protection Clause
 *   authorized selective universities to consider race in admissions,
 *   provided the consideration served the educational benefits of a varied
 *   student body and survived narrow tailoring. The reading's distinctive
 *   structure: ALL students — including white and Asian American students —
 *   are the declared beneficiaries of the diverse learning environment, while
 *   minority students are admitted as the instrumental means of producing
 *   that benefit. Payment falls on applicants displaced by the allocation
 *   (documented disproportionately as Asian American applicants in the SFFA
 *   record) and, less visibly, on the minority students whose group
 *   membership is conscripted as the justifying input. CONSTRAINT FAMILY NOTE
 *   (epsilon-invariance decomposition): the colloquial label 'affirmative
 *   action case law' conflates three structurally distinct constraints — the
 *   colorblind reading (all racial classifications forbidden), the remedial
 *   reading (race-conscious remediation of group subordination required), and
 *   this diversity reading (conditioned permission serving an all-students
 *   benefit). Each carries its own epsilon, beneficiary/victim structure, and
 *   classification; they are linked via network.affects_constraints, not
 *   merged into one observable-dependent story.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setting interpreter (institutional/constrained) — draws and polices the permissibility boundary; withdrew authorization in 2023
 *   - selective_universities: primary beneficiary-administrator (institutional/constrained) — operates the policy, collects compositional control and reputational positioning
 *   - minority_admitted_students: dual-positioned seat (moderate/constrained) — receives access while supplying the justifying benefit to classmates
 *   - asian_american_applicants: primary paying seat (organized/constrained) — bore displaced admission odds for four decades, prevailed in 2023
 *   - state_ban_electorates: excluded voice (organized/trapped) — banned the practice by ballot initiative, overridden by federal doctrine
 *   - constitutional_scholars: analytical observer (analytical/analytical) — supply the empirical briefs and competing doctrinal accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.62).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.66).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Diversity Reading — Race-Conscious Admissions Authorized by Compelling Educational Interest").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '7d37cf89-191e-4e84-afa3-11f90da83805').
narrative_ontology:cs_kernel_codification('7d37cf89-191e-4e84-afa3-11f90da83805', fixed_text).
narrative_ontology:cs_authority_grounding('7d37cf89-191e-4e84-afa3-11f90da83805', lineage).
narrative_ontology:cs_interpretation_layer_present('7d37cf89-191e-4e84-afa3-11f90da83805').
narrative_ontology:cs_reading_relation('7d37cf89-191e-4e84-afa3-11f90da83805', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('7d37cf89-191e-4e84-afa3-11f90da83805', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('7d37cf89-191e-4e84-afa3-11f90da83805', foundational, educational_diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_interest, overridden).
narrative_ontology:cs_axiom_grounding('7d37cf89-191e-4e84-afa3-11f90da83805', educational_diversity_is_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('7d37cf89-191e-4e84-afa3-11f90da83805', secondary, race_conscious_means_require_narrow_tailoring).
narrative_ontology:cs_axiom_status(race_conscious_means_require_narrow_tailoring, holdable).
narrative_ontology:cs_axiom_grounding('7d37cf89-191e-4e84-afa3-11f90da83805', race_conscious_means_require_narrow_tailoring, conventional).
narrative_ontology:cs_reference_frame('7d37cf89-191e-4e84-afa3-11f90da83805', bakke_grutter_diversity_framework).
narrative_ontology:cs_drift_state('7d37cf89-191e-4e84-afa3-11f90da83805', post_sffa_2023, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('7d37cf89-191e-4e84-afa3-11f90da83805', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, students_in_diverse_classrooms).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_admitted_students).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_admitted_students).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, educational_benefits_of_diversity_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, narrow_tailoring_strict_scrutiny).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, holistic_individualized_review).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and polices the boundary between permissible and impermissible consideration of race in university admissions. Reviews institutional programs against the compelling-interest and tailoring standards, strikes down programs that cross the line (quota systems in Bakke, mechanical point awards in Gratz), and in 2023 withdrew authorization for the practice altogether. It cannot step outside the doctrinal framework it inherits; change arrives only through its own case-by-case reinterpretation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Design and administer admissions at institutions that turn away most applicants. They gain control over student-body composition, the ability to present themselves as sites of cross-racial learning, and a defensible legal footing for weighing race. They defend the policy in litigation at great expense; after 2023 several continue pursuing similar composition through facially neutral proxies. Abandoning the practice entirely invites attack from one political flank; continuing it now invites legal attack from the other.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, selective_universities, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, selective_universities, agenda_setter).

% Gain access to selective institutions they might otherwise be turned away from, with the credentials and networks that follow. Their presence is justified to others as the ingredient that produces their classmates' educational experience; they carry the representational burden of standing for their group and the stigma risk the Court itself acknowledged. Leaving means forfeiting the access the policy secured.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_admitted_students, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_admitted_students, payer).

% Applicants with strong academic records who, per the litigation record, faced lower admission odds at some elite institutions than similarly situated peers of other races. Organized into a legal coalition that carried the challenge to the Supreme Court over nearly a decade. Individually they can apply elsewhere or reapply; collectively they absorbed the displaced odds for four decades before prevailing in 2023.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, asian_american_applicants, payer,
    organized, biographical, constrained, global).

% Voters in California, Washington, Michigan, and other states that barred public institutions from considering race by ballot initiative. Their expressed preference was overridden nationally by the doctrinal framework for decades; they had no way to opt out of federal constitutional doctrine short of amending the Constitution.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, state_ban_electorates, excluded,
    organized, generational, trapped, regional).

% Map the doctrinal lines, publish competing accounts of what equal protection requires, and produce the social-science statements on classroom diversity that courts cite as evidence. They neither collect nor pay under the arrangement; their assessments shape which reading captures the next generation of judges.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard for how selective institutions may allocate scarce seats when race is one consideration: individualized, holistic review aimed at the educational value of a varied student body, displacing both rigid quotas and total colorblindness. Universities, applicants, and courts coordinate on one test — compelling interest plus narrow tailoring — instead of fifty divergent state regimes.
% TRANSFER_FUNCTION: Moves admission probability at selective institutions from higher-indexed applicants (disproportionately Asian American in the documented period) toward underrepresented minority applicants; and moves justificatory labor onto minority students themselves, whose group membership is treated as the input generating classmates' educational benefit.
% ABSENT_VOICES: Colorblind-reading partisans sat outside the doctrinal majority for forty-five years — in dissenting opinions, in state ballot initiatives, and finally in the SFFA majority. Remedial-reading partisans were also absent: justice-centered advocates objected that celebrating classroom benefits for the already-advantaged was a thin substitute for repairing documented subordination. Both camps spoke from dissent columns and ballot boxes rather than from the operative standard.
% DISAPPEARANCE_RATIONALE: If the diversity authorization vanished overnight — as it effectively did in 2023 — admissions offices restructure immediately: essay prompts, recruitment pipelines, and the relative weights of legacy, athlete, and socioeconomic factors all recalibrate; applicant behavior shifts; litigation posture inverts; ban-state universities become the national template rather than the exception. The 2023 decision demonstrated exactly this rearrangement in real time.
% FOUNDING_PROBLEM: After Brown, institutions confronting segregation's legacy needed a lawful way to integrate elite education; by 1978 the question was whether equal protection — a guarantee originally secured to protect freed slaves — could tolerate racial classifications used to the advantage of minorities. Justice Powell's Bakke opinion constructed the diversity ground precisely because open-ended remediation looked legally unmanageable to him: an interest that could justify preferences indefinitely and without limiting principle.
% FOUNDING_PROBLEM_CORROBORATION: Universities and their associations attest the problem is live (persistent homogeneity harms learning; preparatory pipelines remain unequal). The SFFA majority attests it is dead as a legal matter (racial balancing is not a compelling interest; diversity cannot authorize what the Constitution forbids). Outside both benefiting parties, the historical record corroborates the original problem — de jure exclusion is documented fact — while whether it remains acute is disputed by economists and sociologists on both sides. No neutral arbiter attests a single status; the split itself is the signal.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: real access moved to minority admits, paid for by displaced applicants and by the instrumentalization of the admitted students themselves; narrow tailoring held it below pure-extraction levels for most of the interval, but the SFFA record showed the practiced version extracting more than the doctrine admitted. Suppression 0.66: the doctrine actively forbade rival forms (quotas, set-asides, mechanical point systems — Gratz), demanded escalating evidentiary compliance (Fisher I's strict-scrutiny-in-fact demand), and finally prohibited the practice outright; enforcement hardened monotonically as resistance rose. Theater 0.42: 'critical mass' functioned as a numerically managed target behind holistic language per the SFFA statistical findings, yet the educational-benefits research program was partly genuine — performative share grew as the gap between stated criteria and compositional practice widened. Accessibility_collapse 0.40: alternatives persisted and were demonstrably usable — Texas percent plans, class-based preferences, ban-state regimes — so understanding the constraint did not close the option space. Resistance 0.72: sustained and organized across four decades (Hopwood, Proposition 209, Proposal 2, Fisher twice, SFFA). Claim/metric independence maintained: tangled_rope is claimed from structure (genuine coordination function + asymmetric payment + active enforcement); the metrics are authored independently as descriptive history. All three temporal series share one seven-point grid (1978-2023) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Four seats should compute differently from identical doctrine. The university seat experiences a workable coordination standard it administers and defends — closer to pure coordination from inside the admissions office. The displaced-applicant seat experiences an enforced transfer of admission odds it never consented to — closer to pure extraction. The minority-student seat experiences both access and being-used, a genuinely hybrid position. The ban-state electorate seat experienced the entire edifice as illegitimate regardless of tailoring quality. Same text, same cases, four different computed types — the divergence is the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Selective universities derive low d from the beneficiary declaration (compositional control flows to them; d near the beneficiary end). Asian American applicants derive high d from the victim declaration plus constrained exit (elite slots are scarce; d near the target end). Minority admitted students are declared BOTH beneficiary and victim: deriving d from the beneficiary side alone would place them near 0.1, ignoring that the reading's own structure makes them the instrument of others' benefit — hence the explicit override to 0.35, reflecting real access gains against real instrumentalization and stigma costs (Grutter itself flagged the stigma risk). The federal judiciary sits outside the beneficiary/victim declarations; the canonical fallback governs. It collects institutional authority from administering the boundary, not material rents — no override authored because no single d captures a court's reflexive relationship to its own doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are blocked here. First, the diversity reading is often misread as transitional because of Grutter's 'expect that 25 years from now racial preferences will no longer be necessary' remark — but no codified sunset exists; the justification is steady-state educational value, not a bridge to a post-preference world, so has_sunset_clause is false and the scaffold gate does not fire. Second, mandatrophy analysis blocks calling the arrangement pure extraction: the coordination function (one lawful national standard replacing fifty divergent state regimes and unmanaged quota wars) was real and load-bearing. The mandate was resolved negatively in 2023 — authorization withdrawn while institutional practices persist in residual and proxy forms — which is why founding_problem_status is contested rather than dead: the parties dispute whether the underlying inequality the reading answered is live or solved, and the mismatch consumer reads that dispute rather than any flattering origin myth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the diversity_reading of the equal_protection_clause kernel; how would instantiating a sibling reading change the structural facts?',
    'Adopt colorblind_reading: the permitted practice disappears, the beneficiary/victim structure dissolves into a single class of individual rights-bearers, and epsilon collapses toward zero as a shield doctrine. Adopt remedial_reading: beneficiaries become historically subordinated groups, victims become institutions resisting remediation, and epsilon rises with the strength of the mandated remedies.',
    'Classification is reading-relative: the colorblind instantiation is a different constraint entirely, and the remedial instantiation is a higher-extraction arrangement with an inverted victim set. Cross-reading comparisons must compare stories, not metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame uncertainty: which reading of the equal protection kernel is instantiated.').

omega_variable(
    diversity_benefits_empirical_status,
    'Do the claimed educational benefits of classroom diversity survive independent empirical scrutiny?',
    'Pre-registered replication of the Michigan/Gurin-line studies and successor literature; natural experiments from ban-state universities operating without race-conscious admissions.',
    'If the benefits fail replication, the coordination function is cover and the arrangement drifts toward pure extraction; if they hold, the mixed coordination-plus-payment structure is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefits_empirical_status, empirical, 'Empirical foundation of the compelling-interest claim.').

omega_variable(
    critical_mass_quota_equivalence,
    'Was ''critical mass'' a genuinely qualitative concept or a numerically managed target administered through holistic-review language?',
    'Admissions-file discovery and statistical reconstruction of year-over-year compositional stability against stated criteria (largely completed in the SFFA v. Harvard record).',
    'Quota-equivalence drives theater_ratio sharply upward and supports reclassification toward pure extraction for the 2003-2023 segment; genuine qualitative review keeps performative maintenance low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_mass_quota_equivalence, empirical, 'Whether the tailoring language masked numerical management.').

omega_variable(
    minority_net_position_instrumentalization,
    'Net of access gains, do admitted minority students benefit or pay once instrumentalization, stigma, and representational burden are counted?',
    'Longitudinal cohort comparisons (ban-state versus race-conscious-era admissions): graduation, earnings, and self-reported belonging outcomes.',
    'If net-negative, this seat flips from beneficiary to payer and effective extraction rises; if net-positive, the dual-role authoring stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_net_position_instrumentalization, empirical, 'Net position of the instrumentalized beneficiary seat.').

omega_variable(
    post_sffa_residual_domain_persistence,
    'Does the diversity reading remain operative in any domain after 2023 (military-academy carve-out, private institutions, faculty hiring), or is it terminally superseded?',
    'Track post-2023 lower-court applications and subsequent Supreme Court treatment of the carve-outs.',
    'Persistence extends the constraint''s life in diminished form; terminal supersession dates the reading''s death at 2023 and closes the lifecycle within this interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_sffa_residual_domain_persistence, empirical, 'Whether the 2023 endpoint is death or dormancy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_diversity_reading_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t1978, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t1986, equal_protection_clause__diversity_reading, theater_ratio, 1986, 0.22).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t1986, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t1996, equal_protection_clause__diversity_reading, theater_ratio, 1996, 0.27).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t1996, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.32).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2003, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.36).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2013, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2016, equal_protection_clause__diversity_reading, theater_ratio, 2016, 0.39).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2016, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(ep_diversity_reading_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t1978, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t1986, equal_protection_clause__diversity_reading, base_extractiveness, 1986, 0.47).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t1986, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t1996, equal_protection_clause__diversity_reading, base_extractiveness, 1996, 0.51).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t1996, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.57).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2003, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.59).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2013, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2016, equal_protection_clause__diversity_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2016, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(ep_diversity_reading_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.34).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t1978, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t1986, equal_protection_clause__diversity_reading, suppression_requirement, 1986, 0.41).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t1986, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t1996, equal_protection_clause__diversity_reading, suppression_requirement, 1996, 0.49).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t1996, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2003, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.59).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2013, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2016, equal_protection_clause__diversity_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2016, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.66).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'equal protection treatment of affirmative action' decomposes into three constraint stories per the epsilon-invariance principle: colorblind_reading (prohibition — no beneficiaries of permitted practice exist), remedial_reading (mandate — historically subordinated groups as beneficiaries, resisting institutions as payers), and this diversity_reading (conditioned permission — all-students beneficiary structure with minority students as instrumental means, moderate epsilon bounded by narrow tailoring). Upstream/downstream: the colorblind and remedial readings are cited as evidence within diversity-reading debates (Grutter weighed remediation arguments before choosing diversity), and the 2023 colorblind victory now constrains this reading's residual domains. Family members link mutually via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__diversity_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
