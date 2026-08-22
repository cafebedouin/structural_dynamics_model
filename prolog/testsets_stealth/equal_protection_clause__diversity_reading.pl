% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [REPUDIATED_POST_SFFA_2023]
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
 *   human_readable: Equal Protection — Educational Diversity Reading (Permission for Race-Conscious Admissions Serving All-Student Benefit)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the equal protection clause
 *   kernel: the diversity reading, under which race-conscious admissions
 *   policies are constitutionally permissible when they serve compelling
 *   educational diversity interests benefiting all students. The standing
 *   arrangement under contest — the ε referent, assessed by this reading's
 *   own lights — is the regime of race-conscious, individually reviewed
 *   admissions governed by strict scrutiny that ran from the 1978 plurality
 *   opinion through the 2003 reaffirmance to its 2023 repudiation. The
 *   reading's distinctive structure: the declared primary beneficiary is the
 *   whole student body (including students whose race counts against them in
 *   admissions), minority admits are justified instrumentally as contributors
 *   to others' education, the constraint claims permanence (ongoing
 *   educational value, expressly not a remedial sunset), and narrow tailoring
 *   bounds the permitted means. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination function plus
 *   asymmetric costs, actively enforced) while the metrics independently
 *   describe moderately extractive, progressively harder-enforced operation —
 *   the engine measures the divergence; neither the claim nor the metrics
 *   were tuned to the other.
 *
 * KEY AGENTS:
 *   - selective_universities: agenda-setting beneficiary (institutional/constrained) — designs the programs, defends them in court, collects the composed cohort and reputational product
 *   - all_students: declared primary beneficiary (moderate/mobile) — receives the diffuse educational-environment benefit the rationale exists to produce
 *   - race_disadvantaged_applicants: primary target (powerless/immediate/constrained) — bears the denial-of-admission cost, concentrated and unreviewable at the individual level
 *   - instrumentalized_minority_admits: dual-positioned target-beneficiary (powerless/biographical/constrained) — receives admission while carrying the representational burden of being valued as a means
 *   - federal_judiciary: enforcement agenda-setter (institutional/generational/analytical) — administers the compelling-interest test and ultimately withdrew the rationale
 *   - race_neutral_alternative_proponents: excluded voice (moderate/generational/trapped) — held outside the operative framework until its collapse
 *   - admissions_officers: operational payer (moderate/biographical/mobile) — absorbs the documentation burden the enforcement standard generates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.45).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.5).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection — Educational Diversity Reading (Permission for Race-Conscious Admissions Serving All-Student Benefit)").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'e26787db-ca14-4ab2-87a8-60b643e132ad').
narrative_ontology:cs_kernel_codification('e26787db-ca14-4ab2-87a8-60b643e132ad', fixed_text).
narrative_ontology:cs_authority_grounding('e26787db-ca14-4ab2-87a8-60b643e132ad', lineage).
narrative_ontology:cs_interpretation_layer_present('e26787db-ca14-4ab2-87a8-60b643e132ad').
narrative_ontology:cs_reading_relation('e26787db-ca14-4ab2-87a8-60b643e132ad', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('e26787db-ca14-4ab2-87a8-60b643e132ad', equal_protection_clause__remedial_reading, influences).
narrative_ontology:cs_axiom('e26787db-ca14-4ab2-87a8-60b643e132ad', foundational, student_body_diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(student_body_diversity_is_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('e26787db-ca14-4ab2-87a8-60b643e132ad', student_body_diversity_is_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('e26787db-ca14-4ab2-87a8-60b643e132ad', foundational, racial_means_justified_by_universal_educational_benefit).
narrative_ontology:cs_axiom_status(racial_means_justified_by_universal_educational_benefit, holdable).
narrative_ontology:cs_axiom_grounding('e26787db-ca14-4ab2-87a8-60b643e132ad', racial_means_justified_by_universal_educational_benefit, instrumental).
narrative_ontology:cs_reference_frame('e26787db-ca14-4ab2-87a8-60b643e132ad', educational_diversity_compelling_interest).
narrative_ontology:cs_drift_state('e26787db-ca14-4ab2-87a8-60b643e132ad', post_sffa_2023, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e26787db-ca14-4ab2-87a8-60b643e132ad', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, race_disadvantaged_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, instrumentalized_minority_admits).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, instrumentalized_minority_admits).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, admissions_officers).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, strict_scrutiny_survival_standard).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, academic_deference_to_university_judgment).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, diversity_yields_educational_benefits_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer admissions processes that weigh race as one element among many in individualized review. They gather the educational and reputational product of diverse cohorts and the legal flexibility to pursue composition goals, while carrying the burden of documenting that their programs are carefully bounded — evidence assembled chiefly for court review. Abandoning race-conscious review is available to them at any time, though it carries mission and reputational costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, selective_universities, beneficiary).

% Enroll in classrooms whose composition the policy shapes. The declared benefit flows to every student regardless of race: exposure to peers with different backgrounds and viewpoints is said to improve learning and preparation for civic and professional life. Individual students experience this benefit diffusely and cannot direct it; they can take their enrollment elsewhere.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    moderate, biographical, mobile, national).

% Apply to selective institutions where their race weighs against them in comparative assessment. The loss lands immediately and individually — a denial in a single cycle — and there is no way to opt out of the process short of forgoing elite applications altogether. Most never learn how race figured in their particular file.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, race_disadvantaged_applicants, payer,
    powerless, immediate, constrained, national).

% Gain admission through processes that count their presence as valuable to other students' education. They receive a real opportunity while carrying representational expectations — speaking for their group in class, staffing diversity programming, absorbing scrutiny of whether they belong. Leaving the institution would forfeit the opportunity along with the burden.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, instrumentalized_minority_admits, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, instrumentalized_minority_admits, beneficiary).

% Reviews whether each program serves a compelling interest through carefully bounded means. It supplies the test every program must pass, tightens or relaxes the standard across decades of cases, and in 2023 withdrew approval from the rationale entirely. It bears none of the programs' costs and collects none of their products.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, federal_judiciary, observer).

% Implement file-by-file review under documentation standards built for litigation; professional craft increasingly consists of producing defensibility records alongside admissions decisions. They can change employers or institutions more easily than the applicants they evaluate.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, admissions_officers, payer,
    moderate, biographical, mobile, national).

% Argue from outside the operative conversation that lottery, percentage-plan, and socioeconomic mechanisms achieve comparable compositional goals without racial sorting. Courts treated their proposals as things programs must merely consider and reject with reasons, never as co-equal frameworks; their approach received a full hearing only after 2023.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, race_neutral_alternative_proponents, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared constitutional standard under which hundreds of selective institutions may assemble racially diverse student bodies through individualized review, and gives courts one uniform test for evaluating such programs — solving the coordination problem of how race may lawfully figure in admissions across thousands of otherwise-independent institutions.
% TRANSFER_FUNCTION: Moves admission opportunities at selective institutions from applicants whose race counts against them toward applicants whose race contributes to compositional goals; moves decision-making discretion toward university administrators; and moves litigation risk and documentation cost onto program defenders and their staff.
% ABSENT_VOICES: Race-neutral-alternative proponents would object that comparable diversity is achievable without racial sorting; colorblind theorists would object that any racial classification wrongs the individuals it touches; rejected applicants themselves almost never appear — the framework hears universities' educational judgments (deferred to in good faith) far more readily than applicants' individual claims.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, admissions at hundreds of selective institutions would reorganize: race-conscious review ends, universities scramble for race-neutral proxies, entering-class composition shifts, and litigation redirects toward state statutes and federal funding conditions. This is not hypothetical — the arrangement's actual termination in 2023 produced precisely this rearrangement within one admissions cycle.
% FOUNDING_PROBLEM: How can publicly accountable universities pursue the educational benefits of student-body diversity — and respond to the exclusionary legacies embedded in elite admissions pipelines — without violating the Fourteenth Amendment's guarantee of equal protection, once explicit racial quotas became unconstitutional?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties, with seat-split valence: historical scholarship documenting pre-civil-rights exclusionary admissions (religious and racial quotas at elite institutions) attests the founding problem was real; employer and service-academy testimony in the litigation record attests the workforce-pluralism half remained live; the 2023 Supreme Court majority and colorblind jurists attest the problem is either illegitimate or unsolvable by racial means, and that the proffered solution failed strict scrutiny. No seat disputes that the problem existed; every seat disputes whether it remains live and what may address it.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).
:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the arrangement pairs a real coordination product — a lawful, uniform pathway for assembling diverse cohorts — with real, concentrated costs: applicants denied on racial grounds they cannot inspect, minority admits valued partly as instruments, and a compliance apparatus whose output is litigation-defensibility rather than pedagogy. Suppression (0.50) is authored as a raw structural property, unscaled — the engine scales only extractiveness by directionality and scope. Its trajectory rises across the interval because the enforcement standard hardened: early review deferred substantially to institutional judgment; late review demanded near-fatal proof of tailoring and prohibited quota-form solutions outright. Theater (0.33) reflects the accumulating share of activity that performs precision the process lacks — 'critical mass' language that avoids numerical targets while pursuing them, amicus boilerplate, tailoring documents written for judges rather than educators. Accessibility collapse is low-moderate (0.35): race-neutral proxies, percentage plans, and colorblind operation remained lawful and visible alternatives throughout, which is why resistance stayed high (0.70) — fifty years of continuous litigation, statewide bans, and political contestation, ending in outright repudiation. The measurement series run on one shared time grid (six points, all three metrics authored at each) so no metric's end-state is silently substituted into earlier rows. The dynamic is monotonic hardening, not cyclical: no intermittent-reinforcement mechanism drives it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the university seat the arrangement is an enabling permission it built and defended — a coordination instrument it would reconstruct if struck down. From the rejected-applicant seat the same structure operates as an unreviewable individual loss imposed by actors who never see his file. From the minority-admit seat it is simultaneously an opened door and a representational tax. From the judiciary seat it is an administrable test, cost-free to administer. Coalition potential matters at the powerless end: the applicant class eventually coordinated (the 2023 challenge was effectively class litigation by organized plaintiffs), which is how a powerless seat moved a doctrine its individual members never could. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. all_students sits near the beneficiary end: diffuse declared benefit, mobile exit. selective_universities sits low-to-moderate: net collector of the arrangement's product (cohort, reputation, discretion) despite compliance costs, with exit available but mission-costly. race_disadvantaged_applicants sits near the full-target end: concentrated cost, immediate horizon, constrained exit (every peer institution runs a similar process). instrumentalized_minority_admits lands mid-range: the admission benefit pulls toward the beneficiary end while the instrumental burden and locked-in participation pull toward the target end. federal_judiciary is near-symmetric — an administrator that neither pays nor collects materially. No directionality_overrides are authored, deliberately: the two powerless seats sit at opposite ends of the d axis, and an override keyed to the powerless power atom would misapply to whichever seat it did not describe. The role-plus-exit derivation differentiates them correctly; a power-atom-keyed override cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading was structured against mandatrophy from the start: it claimed permanence (ongoing educational value, expressly distinguished from the remedial reading's corrective-sunset logic), so it never carried a mandate that could quietly outlive its function. Nor did it atrophy into performance before dying — it was repudiated outright while still operationally live, which is repudiation, not mandatrophy. The mismatch consumer finds no zombie signal: founding_problem_status is contested (not dead) and disappearance_verdict is world_rearranges, and the world demonstrably did rearrange on termination. The residual diversity bureaucracy that persists at some institutions after 2023 is a candidate piton — but it is a DIFFERENT constraint (administrative apparatus maintaining itself after its justifying rationale was withdrawn) and belongs in its own story under the decomposition discipline, not folded into this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the equal protection clause governs racial classifications — this reading''s conditional permission for universal-benefit diversity ends, the colorblind prohibition, or the remedial mandate — and is this story''s beneficiary/victim structure the right one for the clause at all?',
    'Doctrinal resolution (the 2023 repudiation settled the federal question for the current Court), constitutional amendment, sustained scholarly and political realignment, or state-level divergence that reopens the contest.',
    'Adopting the colorblind sibling eliminates this constraint''s beneficiary structure entirely (a prohibition has no students who benefit from its operation) and changes the ε referent to the suppression of race-conscious means; adopting the remedial sibling replaces the all-students beneficiary with historically subordinated groups and imports sunset logic this reading expressly disclaims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the equal_protection_clause kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    universal_benefit_distribution_question,
    'Is the declared all-students benefit descriptively real, or does the educational benefit concentrate on some students while the costs concentrate on others — making the primary beneficiary declaration partly fictitious?',
    'Longitudinal outcome studies of diverse cohorts disaggregated by race, admit-type, and institutional tier, with credible identification of compositional effects on individual learning and civic outcomes.',
    'If benefits are concentrated while costs are concentrated on different seats, effective extraction rises on the paying seats and the arrangement computes closer to pure extraction riding a thin coordination story; if the universal benefit is robust, the coordination side strengthens and the moderate reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_benefit_distribution_question, empirical, 'Whether the diversity benefit is genuinely universal or unevenly distributed across the student body.').

omega_variable(
    instrumentalization_net_cost,
    'How large is the instrumental burden on minority admits — representational labor, group-speaking expectations, belonging scrutiny — relative to the admission opportunity they receive?',
    'Within-institution studies of minority-student workload, campus-climate surveys, and attrition patterns comparing admits under race-conscious and race-neutral regimes.',
    'A heavy net instrumental cost raises effective extraction on that seat and could flip its computed position from net beneficiary to net target, tightening the arrangement''s overall extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_net_cost, empirical, 'Net cost of being valued as a means to others'' education versus the value of the admission itself.').

omega_variable(
    narrow_tailoring_function_vs_ritual,
    'Does narrow tailoring actually bound the arrangement''s costs, or did it decay into litigation ritual — documentation produced for courts that changed little about how admissions decisions were made?',
    'Compare program behavior before and after the documentation demands intensified mid-interval: did tailoring requirements alter admit outcomes and process design, or mainly generate defensibility records?',
    'If tailoring is largely ritual, the theater share is understated and the arrangement''s real bounding mechanism was weaker than authored — pushing the computed type toward extraction-heavy territory; if tailoring genuinely constrained programs, the moderate profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_function_vs_ritual, empirical, 'Whether the tailoring requirement performed real limiting work or ceremonial work.').

omega_variable(
    post_repudiation_revival_pressure,
    'Will the diversity reading revive — through personnel change on the bench, state-level adoption, or migration of the rationale to private institutions and other jurisdictions — or does the 2023 repudiation hold permanently?',
    'Track subsequent litigation strategies, state constitutional developments, private-university practice, and comparative-law adoptions of the diversity rationale over the coming decade.',
    'Revival restores the arrangement''s operation and reactivates this story''s beneficiary/victim structure; permanent repudiation confirms the terminal state and shifts analytic attention to the residual administrative apparatus as a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_repudiation_revival_pressure, empirical, 'Persistence question: whether the repudiated reading exerts revival pressure or stays overridden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__diversity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t9, equal_protection_clause__diversity_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement_basis(equa_tr_t9, observed).
narrative_ontology:measurement(equa_tr_t18, equal_protection_clause__diversity_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(equa_tr_t18, observed).
narrative_ontology:measurement(equa_tr_t27, equal_protection_clause__diversity_reading, theater_ratio, 27, 0.27).
narrative_ontology:measurement_basis(equa_tr_t27, observed).
narrative_ontology:measurement(equa_tr_t36, equal_protection_clause__diversity_reading, theater_ratio, 36, 0.31).
narrative_ontology:measurement_basis(equa_tr_t36, observed).
narrative_ontology:measurement(equa_tr_t45, equal_protection_clause__diversity_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement_basis(equa_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__diversity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t9, equal_protection_clause__diversity_reading, base_extractiveness, 9, 0.36).
narrative_ontology:measurement_basis(equa_be_t9, observed).
narrative_ontology:measurement(equa_be_t18, equal_protection_clause__diversity_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement_basis(equa_be_t18, observed).
narrative_ontology:measurement(equa_be_t27, equal_protection_clause__diversity_reading, base_extractiveness, 27, 0.44).
narrative_ontology:measurement_basis(equa_be_t27, observed).
narrative_ontology:measurement(equa_be_t36, equal_protection_clause__diversity_reading, base_extractiveness, 36, 0.45).
narrative_ontology:measurement_basis(equa_be_t36, observed).
narrative_ontology:measurement(equa_be_t45, equal_protection_clause__diversity_reading, base_extractiveness, 45, 0.45).
narrative_ontology:measurement_basis(equa_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__diversity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t9, equal_protection_clause__diversity_reading, suppression_requirement, 9, 0.33).
narrative_ontology:measurement_basis(equa_su_t9, observed).
narrative_ontology:measurement(equa_su_t18, equal_protection_clause__diversity_reading, suppression_requirement, 18, 0.38).
narrative_ontology:measurement_basis(equa_su_t18, observed).
narrative_ontology:measurement(equa_su_t27, equal_protection_clause__diversity_reading, suppression_requirement, 27, 0.43).
narrative_ontology:measurement_basis(equa_su_t27, observed).
narrative_ontology:measurement(equa_su_t36, equal_protection_clause__diversity_reading, suppression_requirement, 36, 0.47).
narrative_ontology:measurement_basis(equa_su_t36, observed).
narrative_ontology:measurement(equa_su_t45, equal_protection_clause__diversity_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement_basis(equa_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, title_vi_recipient_nondiscrimination).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'equal protection and affirmative action' decomposes into three structurally distinct constraints sharing one fixed-text kernel. The colorblind reading is prohibition-shaped (no beneficiaries; its ε referent is suppressed race-conscious means). The remedial reading mandates group-specific correction (historically subordinated groups as beneficiaries; sunset-prone transitional logic). This diversity reading permits universal-benefit classification (all students as declared beneficiaries; permanent by design; moderate ε). Each member gets its own ε, victims, and temporal structure because each reading's standing arrangement under contest differs — not because one constraint is measured differently. Upstream/downstream structure: this reading's doctrinal success degraded the remedial reading's legitimacy (broad remedial justifications were narrowed out of viability before this reading's reaffirmance), while the colorblind reading persisted as the dissenting frame until it was adopted by the majority in 2023. The statutory twin (Title VI recipient nondiscrimination) inherits whichever reading governs the constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
