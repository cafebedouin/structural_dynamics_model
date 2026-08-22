% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial/Diversity Reading (Conditional Permission for Race-Conscious State Action)
 *   domain: constitutional law/education policy/civil rights
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the remedial_reading — of the
 *   contested equal_protection_kernel: the proposition that the Fourteenth
 *   Amendment's Equal Protection Clause permits race-conscious state action
 *   when narrowly tailored to remedy documented historical exclusion or to
 *   serve a compelling diversity interest, as operationalized in selective
 *   public-university admissions. The standing arrangement under contest
 *   (epsilon's referent) is race-conscious admissions as practiced under this
 *   reading's permission, assessed by this reading's own lights — which treat
 *   such action as presumptively legitimate but strictly bounded; the
 *   reading's endorsed alternative is not the referent. The kernel contest is
 *   deliberately NOT described inside the constraint body: the colorblind and
 *   antisubordination readings are separate constraint files, linked through
 *   network.affects_constraints and documented in the omega variables.
 *   Claim/metric independence is preserved: the constraint is CLAIMED as
 *   tangled_rope (a genuine coordination channel carrying asymmetric
 *   extraction under active judicial enforcement) while the authored metrics
 *   describe the arrangement's actual operation across the Bakke-to-SFFA
 *   interval, including its post-2023 contraction.
 *
 * KEY AGENTS:
 *   - supreme_court: Agenda setter (institutional/constrained) — defines and polices the permission's boundary; its jurisdiction grows with each ruling
 *   - public_universities: Dual-positioned operator (institutional/constrained) — administers programs, collects enrollment flexibility, bears documentation and dissolution risk
 *   - historically_excluded_group_applicants: Primary beneficiary (organized/constrained) — receive the admission-probability transfer
 *   - race_blind_displaced_applicants: Primary target (moderate/constrained) — bear the seat loss individually and per cycle
 *   - civil_rights_advocacy_organizations: Secondary beneficiary (organized/mobile) — collect vindication, membership, and precedential wins
 *   - class_disadvantaged_nonpreferred_applicants: Excluded voice (powerless/trapped) — comparable disadvantage with no seat in the narrow-tailoring conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.34).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.72).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial/Diversity Reading (Conditional Permission for Race-Conscious State Action)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional law/education policy/civil rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '2b5cb265-8414-4acc-b8eb-cfee3535827c').
narrative_ontology:cs_kernel_codification('2b5cb265-8414-4acc-b8eb-cfee3535827c', fixed_text).
narrative_ontology:cs_authority_grounding('2b5cb265-8414-4acc-b8eb-cfee3535827c', lineage).
narrative_ontology:cs_interpretation_layer_present('2b5cb265-8414-4acc-b8eb-cfee3535827c').
narrative_ontology:cs_reading_relation('2b5cb265-8414-4acc-b8eb-cfee3535827c', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('2b5cb265-8414-4acc-b8eb-cfee3535827c', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('2b5cb265-8414-4acc-b8eb-cfee3535827c', foundational, racial_means_subject_to_proportional_review_not_categorical_bar).
narrative_ontology:cs_axiom_status(racial_means_subject_to_proportional_review_not_categorical_bar, holdable).
narrative_ontology:cs_axiom_grounding('2b5cb265-8414-4acc-b8eb-cfee3535827c', racial_means_subject_to_proportional_review_not_categorical_bar, conventional).
narrative_ontology:cs_axiom('2b5cb265-8414-4acc-b8eb-cfee3535827c', secondary, documented_exclusion_qualifies_as_compelling_interest).
narrative_ontology:cs_axiom_status(documented_exclusion_qualifies_as_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('2b5cb265-8414-4acc-b8eb-cfee3535827c', documented_exclusion_qualifies_as_compelling_interest, empirically_contingent).
narrative_ontology:cs_reference_frame('2b5cb265-8414-4acc-b8eb-cfee3535827c', proportionality_narrow_tailoring_frame).
narrative_ontology:cs_drift_state('2b5cb265-8414-4acc-b8eb-cfee3535827c', contemporary_post_sffa, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2b5cb265-8414-4acc-b8eb-cfee3535827c', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_group_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, public_universities).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, race_blind_displaced_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, public_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, strict_scrutiny_framework).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_interest_test).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, narrow_tailoring_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and polices the boundary the clause draws around race-conscious admissions. Decides which programs survive review, has narrowed the permission repeatedly (striking quota systems, mechanical point awards, and finally the diversity rationale), and explicitly left a narrow door open for programs addressing documented historical exclusion. Its authority over university admissions expands with each major ruling; it is bound by its own precedents and by the appointment politics that shape its composition.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Design and operate admissions at selective public campuses. Gain the ability to shape entering classes around diversity and remedial goals when their programs survive review; bear the documentation burden of proving any race-conscious step is justified and necessary, the litigation exposure attached to each program, and the risk of having programs dissolved outright. Cannot walk away from the doctrine while operating under state charters and federal funding; their practical room to maneuver has shrunk with each adverse ruling.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, public_universities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, public_universities, payer).

% Applicants from groups with documented histories of exclusion from selective higher education. Race-conscious consideration raises their admission probabilities at the most selective institutions; their access to those seats depends on programs surviving review. Falling back on less selective institutions or other states is possible but carries real costs in resources, networks, and outcomes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_group_applicants, beneficiary,
    organized, generational, constrained, national).

% Applicants who would have been admitted under a race-blind comparison but lose the seat because consideration flowed to others. The loss lands individually, per cycle, and is discovered only in the decision letter. Recourse is limited: reapply, attend a less preferred institution, or join litigation — an option that became realistic only when an organized plaintiff coalition formed in the 2010s.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, race_blind_displaced_applicants, payer,
    moderate, biographical, constrained, national).

% Litigate to defend and extend race-conscious admissions and related remedies. Collect organizational vindication, member engagement, and precedential wins when programs survive; their briefs and framing shaped both the remedial argument and the broader equality arguments. They can and do shift attention between jurisdictions and issue areas when one front closes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Economically disadvantaged applicants who fall outside the categories that carry remedial or diversity weight. Their disadvantage is comparable in kind, and often in degree, to that of preferred-group applicants, but the doctrine as operated gives their circumstances no explicit consideration. No organizational vehicle represents them inside the narrow-tailoring conversation; their objection surfaces mainly through amicus filings and political debate rather than through any seat in the process.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, class_disadvantaged_nonpreferred_applicants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, historically_excluded_group_applicants).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an open-ended political conflict over racial preference in admissions into a judicially administered two-part test: institutions pursuing remediation of documented exclusion or diversity gains a defined lawful channel, and the means available to them are capped and reviewable, giving applicants, institutions, and lower courts a shared standard for what state use of race is allowed.
% TRANSFER_FUNCTION: Moves admission probability — and the lifetime returns attached to selective-education access — from applicants who would prevail under race-blind comparison toward applicants from groups carrying remedial or diversity weight; secondarily moves compliance costs (documentation, litigation risk, program-design overhead) onto the institutions running the programs.
% ABSENT_VOICES: Class-disadvantaged applicants outside the preferred categories would object that their exclusion is comparable and unremediated; they have no litigating organization of their own and enter the record only through amici. Applicants in states that abolished the permission by referendum had no seat in the doctrinal conversation that produced it. Early generations of displaced applicants lacked any organized voice at all — the payer seat stayed silent for roughly three decades before coalition litigation gave it one.
% DISAPPEARANCE_RATIONALE: Selective admissions reorganize immediately — this is not hypothetical: when the diversity rationale fell in 2023, campuses rewrote essay prompts, rebuilt recruitment pipelines, shifted toward socioeconomic and first-generation signals, and litigation strategies flipped sides within a single admissions cycle. Thousands of admission outcomes per year change hands, and the advocacy apparatus on both sides redirects to new fronts. Whatever one thinks of the arrangement, the world is visibly organized around it.
% FOUNDING_PROBLEM: State-enforced segregation and documented exclusion had shut Black Americans and other historically excluded groups out of selective higher education; after Brown, institutions and courts needed lawful instruments to dismantle that exclusion, and the operative question became whether the Fourteenth Amendment permits race-conscious instruments at all, and on what terms.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is split and comes from outside the benefiting parties on both sides: federal desegregation litigation (the Fordice line on Mississippi's still-dual system into the 1990s) documents live remnants of the founding exclusion well after Brown; historians corroborate both the original exclusions and their recession out of living memory; the SFFA majority found the asserted ongoing interests unmeasurable and time-limited, while dissenting justices and social-science amici attested continuing effects. No single external source settles the status — which is itself the finding recorded here.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).
:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.34 at interval end) is moderate and falling: the displaced-admit transfer peaked mid-interval as programs drifted from documented remediation toward diversity rationales, then contracted sharply when SFFA collapsed the diversity prong. Suppression (0.72) is high and rose monotonically across the whole interval: the arrangement's persistence has always depended on active judicial enforcement — strict-scrutiny review dissolved programs (Hopwood, Gratz) and finally the plus-factor itself (SFFA) — so the enforcement trajectory is an intensifying ratchet, not decay; this is why suppression_requirement is tracked while the underlying structural picture stays coherent. Theater_ratio (0.62) is high at interval end: after SFFA, residual race-conscious practice migrates into essay prompts and personal-narrative channels that diverge from the declared doctrinal standard, and the 'critical mass' vocabulary of the Grutter era was already unmeasurable by design. Accessibility_collapse (0.30) is low: the doctrine affirmatively requires consideration of race-neutral alternatives (percent plans, socioeconomic weighting), so alternatives stay open and are even mandated. Resistance (0.68) is high: five decades of litigation, state referenda (Prop 209, Proposal 2), and recurring legislative conflict. All three tracked series share one six-point grid (1978-2023 mapped to 0-45) so no metric row is backfilled or misaligned.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (the Court), the arrangement is a doctrine it administers — each major ruling extends its authority over admissions. From the beneficiary seat (historically excluded group applicants), the same structure is a remedy: admission probability it would not otherwise hold. From the payer seat (race-blind displaced applicants), it is a taking — a seat lost on a criterion the applicant cannot control, discovered only in the decision letter. Universities straddle: they collect enrollment-composition flexibility while bearing documentation burdens and dissolution risk. The displaced-applicant seat was organizationally silent for most of the interval — individual plaintiffs bearing a diffuse burden — until late-forming coalition litigation (the Students for Fair Admissions line) converted it into an organized force; coalition formation, not any change in the transfer itself, is what moved the arrangement's fate.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded group applicants sit nearest the beneficiary pole (low d): the arrangement subsidizes their admission probabilities, and their exit — attending less selective institutions — is real but costly, keeping them short of arbitrage. Public universities derive net benefit (mission flexibility, enrollment control) but carry compliance drag, placing them moderately above the beneficiary pole. Civil rights advocacy organizations collect vindication, membership, and precedential wins — low d, mobile exit. Race-blind displaced applicants sit near the full-target pole (high d): they bear the entire seat-transfer, their exit within a cycle is nil, and the merit-assessment dimension of the loss amplifies effective extraction beyond the raw transfer. Class-disadvantaged non-preferred applicants bear a real cost the doctrine's own accounting never registers — their exclusion is by omission, not classification — which is why they appear as an excluded voice rather than a declared victim: the operative mechanism takes nothing from them directly; it simply never counts their injury.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dismantling documented, state-enforced exclusion from higher education — is half-alive: the de jure regimes that generated the original remedial claims have receded out of living memory, and post-Croson/Hopwood evidentiary risk drove universities off the remedial prong almost entirely, while the diversity prong carried the arrangement until SFFA extinguished it. The R5 mismatch check reads status=contested against verdict=world_rearranges: neither zombie-flag nor clean resolution — the mandate is partially obsolete and the arrangement demonstrably organizes the world. The constraint carries a latent transitional logic (Grutter's twenty-five-year expectation functioned as a soft sunset), but the transition never completed and the sunset was repudiated by overruling rather than reached — evidence that the coordination function had become steady-state rather than transitional, which is why tangled_rope, not scaffold, is the honest claim. Trajectories to watch: if the remedial prong's documentation basis fully lapses (see omega remedial_documentation_lapse), the residual permission drifts toward inertial maintenance — a permission kept alive theatrically that few institutions can safely exercise — or toward pure extraction if seat-allocation continues without coordination cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_delta,
    'This constraint is the remedial_reading instantiation of the equal_protection_kernel; what structural elements would the sibling readings (colorblind_reading, antisubordination_reading) change?',
    'Compare against the sibling story files: colorblind_reading empties the beneficiary set and converts every race-classified applicant into a target of the prohibition''s enforcement; antisubordination_reading replaces the displaced-admit victim set with groups whose treatment entrenches hierarchy and extends the beneficiary set to any group dismantling caste.',
    'Under colorblind_reading the permission disappears entirely and the constraint recomputes as a categorical bar with maximal suppression of race-conscious practice; under antisubordination_reading the victim set relocates and the documentation obligation becomes a hierarchy audit rather than narrow-tailoring proof — different epsilon, different victims, different type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    remedial_documentation_lapse,
    'Does the remedial prong still rest on currently documented historical exclusion, or has the evidentiary basis lapsed while the permission persists through the diversity prong and institutional inertia?',
    'Audit admissions-program justifications and state desegregation findings (e.g., the Fordice-line decrees) for active documentation of exclusion; measure how often programs cite documented exclusion versus diversity across the interval.',
    'If documentation has lapsed, the remedial branch functions as cover and the arrangement''s coordination function thins toward pure seat allocation, pushing classification away from tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_documentation_lapse, empirical, 'Whether the remedial justification retains a live evidentiary basis.').

omega_variable(
    narrow_tailoring_indeterminacy,
    'Is ''narrowly tailored'' determinate enough that outcomes track program design rather than judicial composition?',
    'Code outcomes of strict-scrutiny challenges against program-design features versus panel composition across the Bakke-to-SFFA record.',
    'If outcomes track bench composition, enforcement is agenda-setter-relative and the constraint''s stability is a property of appointment politics rather than of the doctrine''s own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_indeterminacy, empirical, 'Determinacy of the narrow-tailoring test.').

omega_variable(
    displacement_scale_uncertainty,
    'How many applicants per cycle are displaced from admission by race-conscious consideration, and how concentrated is the burden across groups and institutions?',
    'Admissions microdata audits of the kind compelled in SFFA discovery; counterfactual race-blind reruns of admission cycles.',
    'Sets the magnitude of the victim-side extraction term: small, dispersed displacement supports a coordination-heavy reading; large, concentrated displacement pushes toward extraction-dominated classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_scale_uncertainty, empirical, 'Scale and concentration of the displaced-admit population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epk_remedial_tr_t0, equal_protection_kernel__remedial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(epk_remedial_tr_t9, equal_protection_kernel__remedial_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(epk_remedial_tr_t18, equal_protection_kernel__remedial_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(epk_remedial_tr_t27, equal_protection_kernel__remedial_reading, theater_ratio, 27, 0.44).
narrative_ontology:measurement(epk_remedial_tr_t36, equal_protection_kernel__remedial_reading, theater_ratio, 36, 0.5).
narrative_ontology:measurement(epk_remedial_tr_t45, equal_protection_kernel__remedial_reading, theater_ratio, 45, 0.62).

% Extraction over time
narrative_ontology:measurement(epk_remedial_be_t0, equal_protection_kernel__remedial_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(epk_remedial_be_t9, equal_protection_kernel__remedial_reading, base_extractiveness, 9, 0.44).
narrative_ontology:measurement(epk_remedial_be_t18, equal_protection_kernel__remedial_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(epk_remedial_be_t27, equal_protection_kernel__remedial_reading, base_extractiveness, 27, 0.56).
narrative_ontology:measurement(epk_remedial_be_t36, equal_protection_kernel__remedial_reading, base_extractiveness, 36, 0.6).
narrative_ontology:measurement(epk_remedial_be_t45, equal_protection_kernel__remedial_reading, base_extractiveness, 45, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(epk_remedial_su_t0, equal_protection_kernel__remedial_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(epk_remedial_su_t9, equal_protection_kernel__remedial_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(epk_remedial_su_t18, equal_protection_kernel__remedial_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(epk_remedial_su_t27, equal_protection_kernel__remedial_reading, suppression_requirement, 27, 0.62).
narrative_ontology:measurement(epk_remedial_su_t36, equal_protection_kernel__remedial_reading, suppression_requirement, 36, 0.66).
narrative_ontology:measurement(epk_remedial_su_t45, equal_protection_kernel__remedial_reading, suppression_requirement, 45, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Equal Protection / affirmative action doctrine' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: the colorblind reading (categorical prohibition; negligible extraction for the prohibition itself, maximal for any classified practice it strikes), the remedial reading (this file; conditional permission with bounded, shifting extraction), and the antisubordination reading (hierarchy-direction test; an entirely different victim set). Each carries its own epsilon, beneficiaries, and victims; the files cross-link through affects_constraints. Upstream/downstream structure: the remedial reading's documented-exclusion requirement is cited as factual predicate by the antisubordination frame, while the colorblind reading's categorical premise is what the SFFA coalition mobilized to collapse the remedial permission.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
