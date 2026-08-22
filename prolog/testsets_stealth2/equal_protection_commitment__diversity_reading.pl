% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [OVERRULED_SFFA_2023]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection — Diversity Reading: Race as Plus Factor for Educational Diversity
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   Between Bakke (1978) and SFFA v. Harvard (2023), the diversity reading of
 *   the Equal Protection Clause governed a standing arrangement: selective
 *   universities could consider race as one flexible factor within holistic,
 *   individualized review, justified by educational diversity as a compelling
 *   state interest and policed by strict scrutiny. This story authors THAT
 *   reading only, as a clean epsilon-invariant constraint over the
 *   arrangement it licensed. The claim/metric gap is deliberate: the reading
 *   presents the arrangement as bounded coordination serving a compelling
 *   interest, while the authored metrics describe a hybrid — a real
 *   coordination function, real diffuse extraction from applicants, actively
 *   enforced throughout, with a compliance-performance layer that grew every
 *   time the Court demanded better-documented narrow tailoring. The engine
 *   measures the divergence; the claim is not reconciled to the metrics. Per
 *   the epsilon-invariance principle, the sibling readings (remedial,
 *   colorblind) are separate stories linked through the network; this file's
 *   epsilon (0.28) is indexed to this reading's own assessment of the
 *   standing arrangement — the reading sees bounded, procedurally mediated
 *   costs, not the categorical violation its colorblind sibling would see in
 *   the identical conduct. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda-setting arbiter (institutional/constrained) —
 *   defines and enforces the doctrinal boundary; collects no rents -
 *   selective_universities: administering beneficiary
 *   (institutional/constrained) — operates holistic review, collects
 *   discretion and defensibility - underrepresented_minority_applicants:
 *   margin beneficiary (powerless/constrained) — elevated admission odds,
 *   unverifiable individual treatment -
 *   diversity_pipeline_dependent_institutions: downstream beneficiary
 *   (powerful/mobile) — collects workforce diversity without operating
 *   admissions - civil_rights_legal_organizations: defending beneficiary
 *   (organized/mobile) — collects mission relevance from the arrangement's
 *   periodic defense - all_selective_admissions_applicants: diffuse payer
 *   (powerless/constrained) — bears the procedural opacity of holistic review
 *   - disfavored_applicant_groups: concentrated payer (organized/constrained)
 *   — bears displacement; organized into litigation vehicles -
 *   ban_state_public_universities: coerced former participant
 *   (institutional/trapped) — forced out by electorate, bears
 *   proxy-transition costs - constitutional_law_scholars: analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setting arbiter (institutional/constrained) — writes and enforces the doctrinal boundary; collects no rents
 *   - selective_universities: administering beneficiary (institutional/constrained) — designs and operates holistic review; collects discretion, reputation, and litigation-defensible class composition
 *   - underrepresented_minority_applicants: margin beneficiary (powerless/constrained) — receives elevated admission probability; cannot verify individual treatment; carries possible unaccounted dignity costs
 *   - diversity_pipeline_dependent_institutions: downstream beneficiary (powerful/mobile) — military academies and large employers collecting workforce-diversity benefits without operating admissions
 *   - civil_rights_legal_organizations: defending beneficiary (organized/mobile) — collects funding cycles, caseloads, and public standing from the arrangement's continued existence and periodic defense
 *   - all_selective_admissions_applicants: diffuse payer (powerless/constrained) — bears procedural opacity and the holistic-packaging arms race; exit means forgoing selective higher education
 *   - disfavored_applicant_groups: concentrated payer (organized/constrained) — bears substantive displacement; individually lacking standing, collectively financing a two-decade litigation campaign
 *   - ban_state_public_universities: coerced former participant (institutional/trapped) — removed from the arrangement by voter initiative; bears measured diversity losses and proxy-rebuilding costs
 *   - constitutional_law_scholars: analytical observer (analytical/analytical) — interprets the doctrine, staffs amicus coalitions on every side; collects and bears nothing material
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.55).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection — Diversity Reading: Race as Plus Factor for Educational Diversity").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).
narrative_ontology:has_sunset_clause(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'd88557ca-3325-4d63-8275-b092a20130ea').
narrative_ontology:cs_kernel_codification('d88557ca-3325-4d63-8275-b092a20130ea', fixed_text).
narrative_ontology:cs_authority_grounding('d88557ca-3325-4d63-8275-b092a20130ea', lineage).
narrative_ontology:cs_interpretation_layer_present('d88557ca-3325-4d63-8275-b092a20130ea').
narrative_ontology:cs_reading_relation('d88557ca-3325-4d63-8275-b092a20130ea', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('d88557ca-3325-4d63-8275-b092a20130ea', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('d88557ca-3325-4d63-8275-b092a20130ea', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, overridden).
narrative_ontology:cs_axiom_grounding('d88557ca-3325-4d63-8275-b092a20130ea', educational_diversity_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('d88557ca-3325-4d63-8275-b092a20130ea', secondary, race_as_flexible_plus_factor_under_individualized_review).
narrative_ontology:cs_axiom_status(race_as_flexible_plus_factor_under_individualized_review, overridden).
narrative_ontology:cs_axiom_grounding('d88557ca-3325-4d63-8275-b092a20130ea', race_as_flexible_plus_factor_under_individualized_review, instrumental).
narrative_ontology:cs_reference_frame('d88557ca-3325-4d63-8275-b092a20130ea', grutter_compelling_interest_framework).
narrative_ontology:cs_drift_state('d88557ca-3325-4d63-8275-b092a20130ea', post_sffa_overruling, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('d88557ca-3325-4d63-8275-b092a20130ea', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_selective_admissions_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, disfavored_applicant_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, diversity_pipeline_dependent_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, civil_rights_legal_organizations).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, ban_state_public_universities).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, strict_scrutiny_framework).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, holistic_individualized_assessment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates the boundary the doctrine marks: reviews university admissions programs under strict scrutiny, decides which uses of race survive, and writes the opinions (Bakke, Grutter, Fisher, SFFA) that define and redefine the arrangement. Collects no tuition advantages and no admission slots; its stake is doctrinal coherence and institutional legitimacy. Its room to maneuver is bounded by precedent, its own composition, and the case-or-controversy requirement.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Designs and operates holistic admissions under the doctrine's protection: composes classes to institutional mission, documents individualized review to survive litigation, and collects the discretion, reputational standing, and diverse classrooms the arrangement makes defensible. Exit would mean abandoning either selective admissions or mission-driven class composition; both are core to institutional identity.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, selective_universities, beneficiary).

% Receive elevated admission probability at the margin where the racial factor applies, along with access to selective institutions' credentials and networks. They bear whatever stigma attaches to preferential admission and cannot individually verify how the factor weighed in their own file. Alternatives outside selective admissions exist but carry real costs in mobility and signaling.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants, beneficiary,
    powerless, biographical, constrained, national).

% Military service academies, large employers, and professional schools that supported the arrangement because their leadership pipelines depend on diverse graduate pools. They collect workforce-composition benefits without operating admissions; their alternative is building their own pipeline development at higher cost.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, diversity_pipeline_dependent_institutions, beneficiary,
    powerful, generational, mobile, national).

% Litigate to defend the arrangement, supply the amicus record, and organize the defense coalitions each challenge requires. Their funding cycles, caseloads, and public standing are bound to the arrangement's continued existence and periodic defense. Deprioritizing the issue is organizationally feasible but strategically costly.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, civil_rights_legal_organizations, beneficiary,
    organized, generational, mobile, national).

% Submit to holistic review whose operation they cannot observe: the arrangement obscures why any individual file succeeds or fails, so no applicant can verify what role race played in their own decision. They bear the procedural opacity and the arms-race cost of holistic packaging — essays, narratives, consulting. Exit means forgoing selective higher education.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_selective_admissions_applicants, payer,
    powerless, biographical, constrained, national).

% Applicants whose admission odds fall where the racial factor weighs against their demographic profile — the population later organized into litigation vehicles such as Students for Fair Admissions. They bear the substantive displacement cost. Individually they lack standing and leverage; collectively they financed a two-decade litigation campaign that ultimately reached the Court.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, disfavored_applicant_groups, payer,
    organized, biographical, constrained, national).

% Public university systems in states where voters outlawed race-conscious admissions (California after Proposition 209, Michigan after Proposal 2). They were forced out of the arrangement by initiative, then bore the cost of rebuilding class diversity through proxies — percent plans, socioeconomic weighting — with measured declines in underrepresented enrollment. Their exit was not chosen; it was imposed by their own electorates.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, ban_state_public_universities, payer,
    institutional, generational, trapped, regional).

% Analyze the doctrine's coherence, publish the competing readings, and staff the amicus briefs on every side. They collect nothing material from the arrangement's operation and bear none of its costs; their stake is interpretive.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a working settlement between university mission autonomy and the Fourteenth Amendment's equality guarantee: institutions get a judicially recognized framework for composing classes with race as one element of individualized review, and the polity gets a bounded, reviewable form of race-consciousness that stops short of quotas.
% TRANSFER_FUNCTION: Moves marginal admission probability toward underrepresented minority applicants; moves decision discretion and litigation-defensibility to universities; moves procedural legibility away from every applicant (holistic files obscure what role any factor played); moves compliance costs onto admissions bureaucracies.
% ABSENT_VOICES: Denied applicants in every admissions cycle — including those displaced by the racial factor — had no seat in the doctrinal conversation; their interests entered only decades later through organized litigation vehicles. Applicants to non-selective institutions, unaffected by the arrangement, were absent entirely. State voters in ban states spoke only through blunt initiatives, not through participation in the doctrine's design.
% DISAPPEARANCE_RATIONALE: Overnight disappearance (before 2023) would force every race-conscious admissions program to restructure immediately: universities would pivot to percent plans and socioeconomic proxies, pending litigation would collapse or reverse, pipeline institutions would lose expected workforce diversity, and the doctrinal settlement the arrangement embodied would reopen — approximately what the 2023 overruling actually produced.
% FOUNDING_PROBLEM: After formal segregation ended, selective institutions using race-conscious admissions (UC Davis's medical-school set-aside was the triggering case) had no secure constitutional footing: explicit quotas were politically and legally toxic, but outright colorblindness threatened to re-inscribe de facto exclusion. The arrangement was built to solve the problem of giving race-conscious admissions a legitimate, bounded, judicially defensible form short of quotas.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary attests alone: the overruling majority in SFFA attests the founding problem was misconceived (the interest was not measurable, the means not narrowly tailored, no endpoint existed); the four-justice dissent attests the problem remains live and the interest compelling; ban-state electorates attested by initiative that they rejected the arrangement's solution; the social-science record on educational benefits is disputed by expert witnesses on both sides of the SFFA trial. Corroboration exists abundantly outside the beneficiary set — but it corroborates opposite answers.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.28 sits in the low-moderate band the reading's own lights predict: costs are real (displaced applicants, opaque files, compliance burdens) but procedural, bounded by strict scrutiny, and judged justified against the compelling interest. Suppression 0.55 is a raw structural property, unscaled by power or scope: the arrangement persisted through active judicial enforcement — five- and four-vote majorities, escalating narrow-tailoring demands, litigation exposure for every program — not through participant consensus; yet it coerced no individual directly and left legal alternatives open. Theater 0.40: each doctrinal tightening converted admissions practice into documented performance (critical-mass narratives, file-by-file review records, diversity justifications drafted for courts), a Goodhart drift visible in the rising theater series. Accessibility collapse 0.30: alternatives never collapsed — percent plans, class-based proxies, and ban-state regimes operated throughout, and the colorblind alternative ultimately won. Resistance 0.80: a quarter-century litigation arc (Hopwood, Gratz/Grutter, Fisher I-II, SFFA), state initiatives, and legislative bans. All three tracked series share one seven-point grid (1978-2023); suppression_requirement is tracked because the story's dynamic IS enforcement-capacity change — the ratchet tightened until the structure broke. There is no cyclical oscillation: the trajectory is a monotonic ratchet, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical doctrine. From the university seat the arrangement is autonomy-enabling coordination it built, documented, and defended — a workable settlement. From the disfavored-applicant seat the same structure is opaque displacement administered without individual accountability. From the judiciary's seat it is a managed compromise requiring escalating maintenance. Ban-state public universities experienced it as an externally imposed regime their own electorates then revoked. The engine derives these per-seat classifications from power, exit, and directional position; the divergence between the university seat and the applicant seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: universities collect discretion and defensibility, and they are the receipt surface — the arrangement's operative gains demonstrably accrue to the university seat, while minority applicants receive transfers as instruments of the diversity interest rather than as capturers of its surplus. Underrepresented minority applicants sit slightly above pure beneficiary: they receive marginal admission probability but cannot verify their own treatment and may carry unaccounted dignity costs (omega dignitary_harm_accounting). Pipeline institutions and civil-rights organizations collect derivative benefits. Payers sit high: all applicants bear procedural opacity diffusely; disfavored applicant groups bear concentrated displacement and organized to escape it; ban-state publics bore coerced transition costs. The judiciary sits near symmetric — it neither collects nor pays materially; its stake is doctrinal coherence. No directionality_overrides are authored: overrides key to power atoms, and every atom in this story spans opposed directionalities (the institutional atom alone contains a beneficiary university seat, a symmetric judicial arbiter, and a payer ban-state seat), so any atom-level override would misclassify someone; the structural derivation from roles and exit options is left to stand.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving race-conscious admissions a legitimate form short of quotas — is contested, not dead: the overruling majority declares it misconceived while the dissent and much of the academy maintain it. The arrangement therefore did not die of mandate expiry; it was killed by a rival reading of the same kernel, which is a different failure mode than mandatrophy. The declared 25-year sunset (Grutter, 2003) was never mechanized — no automatic expiration, no mandatory review trigger — so the transitional self-description functioned as legitimation rather than as a sunset device (omega grutter_sunset_functionality). The classification prevents two misreadings: calling the arrangement pure coordination ignores the diffuse applicant extraction and the compliance theater that grew with every doctrinal tightening; calling it pure extraction ignores the genuine coordination function, the absence of any capturer beyond the university seat's discretionary gain, and the real educational goods delivered while the arrangement stood. The rising theater series tracks partial mandatrophy in the Goodhart sense — documentation displacing educational function — while the arrangement lived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the diversity reading of the equal_protection_commitment kernel; would the remedial or colorblind reading of the same amendment text yield a different constraint with different beneficiaries, victims, and epsilon?',
    'Generate the sibling stories (remedial_reading, colorblind_reading) and compare computed classifications across the family; divergence in victim sets and epsilon locates the disagreement.',
    'Under the colorblind reading the same admissions arrangements are maximally extractive violations with no beneficiary seat; under the remedial reading the victims shift to subordinated castes and the standing arrangement''s epsilon rises as an insufficient remedy. Classification is reading-relative; cross-reading comparison is the measurement this family exists to take.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: sibling readings of one kernel instantiate structurally different constraints.').

omega_variable(
    grutter_sunset_functionality,
    'Does the Grutter Court''s declared 25-year expectation constitute a functional sunset clause making the arrangement transitional support, or was it rhetorical framing laid over an enforced steady-state equilibrium?',
    'Mechanism analysis of the doctrinal record: search for any operational sunset device (automatic expiration, mandatory review trigger, codified endpoint). None exists; the expectation was never justiciable and no institution was tasked with enforcing it.',
    'If functional, the arrangement leans scaffold and its 2023 termination reads as an early sunset; if rhetorical, the transitional language is legitimation cover on a hybrid that rivals, not clocks, ended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grutter_sunset_functionality, conceptual, 'Whether the declared sunset expectation was a mechanism or rhetoric.').

omega_variable(
    educational_benefits_evidence,
    'Do racially diverse classrooms actually produce the educational and leadership-pipeline benefits the compelling-interest claim rests on?',
    'Longitudinal cohort studies of post-admission outcomes; the SFFA trial record and dueling expert testimony; natural experiments from ban-state enrollment shifts.',
    'If benefits are substantiated, the coordination function is real and epsilon stays low-moderate; if not, the coordination story thins toward cover and the arrangement''s extraction share rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educational_benefits_evidence, empirical, 'Empirical foundation of the compelling-interest claim.').

omega_variable(
    applicant_cost_attribution,
    'Can the costs borne by individual applicants be attributed to the racial factor at all, given holistic review''s opacity?',
    'Ban-state natural experiments comparing enrollment composition before and after prohibition; file-level analyses where admissions records permit reconstruction.',
    'If attribution is impossible, the victim set stays diffuse and effective extraction remains procedural-low; if attributable, identifiable displaced applicants emerge and effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(applicant_cost_attribution, empirical, 'Attributability of applicant-level costs under holistic review.').

omega_variable(
    dignitary_harm_accounting,
    'Does the arrangement impose stigmatic or mismatch costs on its intended beneficiaries that the reading''s own benefit accounting excludes?',
    'Matched-cohort studies of minority-student outcomes under preferential versus race-neutral regimes; survey evidence on perceived stigma among admitted students.',
    'If real and material, underrepresented minority applicants'' directionality rises above pure beneficiary, compressing the asymmetry and distributing burden more evenly across seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignitary_harm_accounting, empirical, 'Unaccounted costs falling on the arrangement''s intended beneficiaries.').

omega_variable(
    kernel_unity_underdetermination,
    'Is the equal protection commitment a single kernel with three readings, or three distinct commitments that merely share Fourteenth-Amendment vocabulary?',
    'Test whether the readings contest a shared reference object (the same amendment text under interpretation) or merely share terminology; examine whether any reading''s adoption presupposes the others'' rejection.',
    'If three distinct commitments, the family network edges dissolve and each story stands alone; if one kernel, cross-reading contamination propagation through the network is meaningful.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_unity_underdetermination, conceptual, 'Framing under-determination: kernel unity versus terminological coincidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1987, equal_protection_commitment__diversity_reading, theater_ratio, 1987, 0.18).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_commitment__diversity_reading, theater_ratio, 1996, 0.25).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.32).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_commitment__diversity_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_commitment__diversity_reading, theater_ratio, 2016, 0.44).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.5).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1987, equal_protection_commitment__diversity_reading, base_extractiveness, 1987, 0.24).
narrative_ontology:measurement(equa_be_t1996, equal_protection_commitment__diversity_reading, base_extractiveness, 1996, 0.27).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.3).
narrative_ontology:measurement(equa_be_t2010, equal_protection_commitment__diversity_reading, base_extractiveness, 2010, 0.31).
narrative_ontology:measurement(equa_be_t2016, equal_protection_commitment__diversity_reading, base_extractiveness, 2016, 0.33).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(equa_su_t1987, equal_protection_commitment__diversity_reading, suppression_requirement, 1987, 0.38).
narrative_ontology:measurement(equa_su_t1996, equal_protection_commitment__diversity_reading, suppression_requirement, 1996, 0.45).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(equa_su_t2010, equal_protection_commitment__diversity_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(equa_su_t2016, equal_protection_commitment__diversity_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'equal protection' decomposes into three structurally distinct readings of one amendment text. This story authors the diversity reading only: epsilon 0.28 over the standing race-conscious-holistic-admissions arrangement as the reading's own lights assess it. The colorblind sibling rates any racial factor as a categorically impermissible classification (epsilon near-maximal, no beneficiary seat); the remedial sibling rates the same arrangement as an inadequate response to caste perpetuation (victims shift to subordinated castes). Each sibling is a separate file linked here; the family shares the kernel, not the epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
