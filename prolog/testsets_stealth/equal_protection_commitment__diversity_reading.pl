% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [SUPERSEDED (SFFA v. Harvard 2023); live as kernel reading]
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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection — Diversity Reading (Race as One Factor Among Many for Educational Diversity)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   From Regents of the University of California v. Bakke (1978) through
 *   Grutter v. Bollinger (2003) to its termination in Students for Fair
 *   Admissions v. Harvard (2023), this arrangement operated as a standing
 *   constitutional permission: universities could consider race as one factor
 *   among many in composing each entering class, justified by the educational
 *   benefits of a diverse student body rather than by remediation of past
 *   wrongs. The permission carried an enforcement perimeter — strict
 *   scrutiny, individualized holistic review, no quotas, no mechanical point
 *   awards — policed by the federal courts with steadily increasing
 *   intensity. Universities gained enrollment discretion under the license;
 *   applicants passed through an evaluation in which the racial component was
 *   undisclosed, leaving rejected applicants unable to individuate any claim.
 *   The arrangement ended not by attrition but by formal repudiation, yet the
 *   underlying problem it addressed — pursuing integrated student bodies
 *   under an anti-classification text — visibly persists in post-2023
 *   institutional behavior. KEY AGENTS (by structural relationship): -
 *   supreme_court: agenda-setting enforcer ([institutional]/[analytical]) —
 *   writes and revises the tailoring standards; the only seat able to
 *   redefine or withdraw the permission - selective_universities: primary
 *   beneficiary and operational administrator ([institutional]/[constrained])
 *   — compose classes under the license, bear compliance and litigation
 *   burdens - college_applicants: primary payer ([powerless]/[constrained]) —
 *   subject to holistic review whose racial component is undisclosed;
 *   rejected applicants cannot individuate their claims -
 *   underrepresented_minority_applicants: secondary beneficiary
 *   ([moderate]/[constrained]) — admission probabilities raised by the
 *   race-conscious component; pass through the same opaque evaluation -
 *   admissions_officers: dual-positioned operator ([moderate]/[mobile]) — run
 *   the review machinery, gain professional discretion, bear deposition and
 *   documentation burdens - colorblind_constitutional_litigants: excluded
 *   challenger ([organized]/[mobile]) — four decades of test-case litigation
 *   mounted from outside the framework's premises
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.34).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.58).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection — Diversity Reading (Race as One Factor Among Many for Educational Diversity)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'd5410d69-9ee2-49f7-afe8-3beae77589f7').
narrative_ontology:cs_kernel_codification('d5410d69-9ee2-49f7-afe8-3beae77589f7', fixed_text).
narrative_ontology:cs_authority_grounding('d5410d69-9ee2-49f7-afe8-3beae77589f7', lineage).
narrative_ontology:cs_interpretation_layer_present('d5410d69-9ee2-49f7-afe8-3beae77589f7').
narrative_ontology:cs_reading_relation('d5410d69-9ee2-49f7-afe8-3beae77589f7', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('d5410d69-9ee2-49f7-afe8-3beae77589f7', equal_protection_commitment__remedial_reading, influences).
narrative_ontology:cs_axiom('d5410d69-9ee2-49f7-afe8-3beae77589f7', foundational, educational_diversity_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('d5410d69-9ee2-49f7-afe8-3beae77589f7', educational_diversity_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('d5410d69-9ee2-49f7-afe8-3beae77589f7', secondary, narrow_tailoring_individualized_holistic_review).
narrative_ontology:cs_axiom_status(narrow_tailoring_individualized_holistic_review, holdable).
narrative_ontology:cs_axiom_grounding('d5410d69-9ee2-49f7-afe8-3beae77589f7', narrow_tailoring_individualized_holistic_review, conventional).
narrative_ontology:cs_reference_frame('d5410d69-9ee2-49f7-afe8-3beae77589f7', diversity_as_compelling_interest_framework).
narrative_ontology:cs_drift_state('d5410d69-9ee2-49f7-afe8-3beae77589f7', post_sffa_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d5410d69-9ee2-49f7-afe8-3beae77589f7', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, admissions_officers).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, college_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, admissions_officers).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, strict_scrutiny_narrow_tailoring).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, academic_freedom_deference_in_admissions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which uses of race in admissions survive constitutional challenge and writes the standards admissions offices must design around. Reviews programs under strict scrutiny, articulates the tailoring requirements, and periodically revisits the settlement its precedents struck. Holds the only seat that can redefine or withdraw the permission; collects no revenue from its operation and cannot exit the seat.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Administer holistic admissions under the doctrinal license: weigh race alongside grades, tests, essays, and background in composing each entering class. Gain enrollment discretion and the compositional control the license protects; bear the compliance burden of documenting tailoring and defending programs in litigation. Cannot exit constitutional review; their alternative is designing race-neutral processes, which several state systems already operate under referendum mandates.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, selective_universities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, selective_universities, agenda_setter).

% Submit files to holistic review in which race may enter the evaluation without disclosure. Bear the cost of unverifiable evaluation: a rejected applicant cannot learn how race figured in the decision, and discovery into committee deliberations is shielded. Have no voice in the design of the process they submit to. Exit means applying to less selective institutions or abroad; within selective admissions there is no outside option.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, college_applicants, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, college_applicants, excluded).

% Compete for seats under a system whose race-conscious component raises their admission probabilities at selective institutions. Benefit from the compositional goal their presence serves; also pass through the same opaque evaluation as every other applicant, with the same inability to inspect how their own file was scored. Exit mirrors that of all applicants: take the offer extended or go elsewhere.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Run the review machinery: read files, calibrate class composition, and document the process for auditors and courts. Gain professional discretion and mission alignment from the individualized-review craft; bear deposition exposure, training burdens, and the work of maintaining records shaped to survive judicial scrutiny. Can leave the profession, and the craft and its commitments travel with them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, admissions_officers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, admissions_officers, payer).

% Organize challenges to any racial classification in admissions and supply the plaintiffs and funding behind four decades of test cases. Stand outside the framework's premises: their core claim — that no racial classification is ever permissible — holds no constructive seat inside the diversity framework and reaches it only as a challenge to be adjudicated and rejected. Their mobility lies in redirection: state referenda, legislation, and successive plaintiff profiles.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_constitutional_litigants, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared constitutional standard under which thousands of admissions offices can operate race-conscious review without per-decision constitutional crisis: strict scrutiny with articulated tailoring requirements converts an open-ended conflict over every admissions cycle into a predictable review framework institutions can plan around.
% TRANSFER_FUNCTION: Moves evaluative discretion toward universities (license to weigh race within judicially defined bounds); moves opacity costs onto applicants (evaluation whose racial component is undisclosed and uninspectable); moves litigation and compliance costs onto universities and challengers alike; moves admission probability toward underrepresented minority applicants at the margin.
% ABSENT_VOICES: Rejected applicants who never learn how race factored into their specific file are structurally absent: confidentiality of deliberations denies them the standing observation from which objection is built — they cannot testify to a harm they cannot see. The colorblind coalition, though loudly present in courtrooms, was excluded from the framework's constructive premises: heard only as a challenge to be rejected, never as a participant in defining the interest.
% DISAPPEARANCE_RATIONALE: The 2023 termination supplied the natural experiment: within months, hundreds of institutions rewrote admissions processes, essay weight rose, legacy preferences drew new scrutiny, and underrepresented-minority enrollment at the most selective schools declined measurably. The arrangement demonstrably organized a large institutional world; its removal rearranged that world immediately and visibly.
% FOUNDING_PROBLEM: How can publicly accountable universities pursue integrated student bodies and the educational benefits of diversity under a Fourteenth Amendment whose core command forbids racial classification — after Brown foreclosed segregation and with no consensus on colorblindness. Bakke's specific version: UC Davis's quota was indefensible, but was any race-consciousness defensible?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the SFFA dissenting opinions attest the problem is live and unresolved; amicus filings by major employers and former military leadership attested a continuing institutional need for diverse pipelines; civil-rights organizations document post-2023 enrollment declines demonstrating the problem's persistence. The SFFA majority attests a different status — that the interest as framed is not legitimately achievable by these means — so corroboration of liveness is broad even where the solution's legitimacy is disputed. No party inside the university beneficiary set is relied upon.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.34 at interval end, inside the 0.20-0.35 expected band) because the arrangement is procedural rather than substantive: it transfers no money and mandates no outcome; what it moves is evaluative discretion (to universities) and opacity costs (onto applicants), plus compliance and litigation burdens distributed across all seats. Suppression is authored 0.58 as a structural average across seats: applicants have no exit from selective-admissions opacity, but universities retain a real race-neutral alternative (nine state systems operated under bans), which caps structural coercion below the enforcement intensity the courts applied. Theater rises to 0.50 because a large share of late-period activity was litigation-proofing — critical-mass rationales drafted for the record, race-neutral-alternative studies performed for the file, documentation shaped by anticipated discovery — a performative share the SFFA trial record exposed directly. Accessibility_collapse is 0.40: alternatives (percent plans, socioeconomic proxies, ban-state regimes) persisted and operated alongside the arrangement. Resistance is 0.78: Hopwood, statewide referenda in nine states, Fisher I and II, and the SFFA campaign constitute sustained organized opposition across four decades. The three measurement series run on one shared time grid (1978, 1985, 1995, 2003, 2016, 2023) with every metric authored at every point. Trajectories are monotonic — this was a litigation ratchet, not an oscillating cycle; no intermittent-reinforcement mechanism is present. The suppression_requirement series is authored because enforcement-capacity change is a central dynamic of this story: judicial policing hardened from Powell's deferential single-opinion framework (0.25) through Grutter's bright lines (0.48) and Fisher II's no-deference tailoring review (0.65) to SFFA's effectively categorical bar (0.85) immediately before termination. The base_properties.suppression scalar (0.58) deliberately sits below the series endpoint: the scalar describes structural coercion across seats at maturity, while the series traces enforcement intensity specifically — the two constructs diverge because the arrangement's final years combined moderate structural coercion with maximal judicial policing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the university seat the arrangement is mission-essential flexibility under irksome but manageable review — a framework it helped design and defends as academic freedom. From the applicant seat the same structure is unverifiable evaluation: a process whose decisive inputs are concealed, with no discovery route and no appeal on the concealed dimension. From the court seat it is an administrable doctrine with workable bright lines. Among same-nominal-level actors, applicants diverge not by power but by benefit flow: underrepresented minority applicants gain admission probability from the very component whose concealment burdens every applicant equally. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Selective_universities derive low directionality (declared beneficiary, institutional power) but their constrained exit — accreditation, endowments, public missions tie them to the jurisdiction — keeps them nearer the target side than an arbitrage-grade beneficiary would sit. Underrepresented_minority_applicants derive low d with moderate power. Admissions_officers sit mid-low: dual-positioned, gaining discretion while bearing compliance burdens, with mobile exit. College_applicants derive high d: declared victim, individually powerless, constrained exit — no applicant can opt out of opaque evaluation while seeking selective admission. The supreme_court sits near symmetric as administrator: it enforces the perimeter and collects no rents from its operation. No directionality_overrides are authored: the derivation chain produces the right relationships from the beneficiary/victim declarations plus exit options, because the two institutional seats differ in role (not power), and role enters the derivation through the declarations themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. A pure-coordination mislabel would erase the asymmetric burden applicants bear — obscured individual claims maintained by confidentiality of deliberations and enforced by judicial machinery. A pure-extraction mislabel would erase the genuine coordination function: a single stable national standard under which thousands of admissions offices could plan, replacing per-decision constitutional crisis with predictable review, and shielding mission-driven composition from ad hoc political attack. On the genealogy interview, the founding problem remains live — post-repeal institutional scrambling (essay-weight recalibration, recruitment retargeting, pipeline redesign) demonstrates the underlying problem unsolved — so no zombie flag fires; the mismatch consumer reads live-status x world_rearranges and finds coherence. The theater trajectory to 0.50 marks Goodhart drift toward litigation-proofing, a symptom of aging enforcement rather than mandate death. The O'Connor 25-year expectation deserves separate treatment: had it been an operative sunset, the arrangement would have been transitional support approaching declared expiry; its dictum status keeps it a durable permission with an aspiration attached — the distinction the grutter_sunset_expectation_bindingness omega carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_position,
    'This constraint is one reading of the equal_protection_commitment kernel (reading: diversity_reading). What structural differences would the sibling readings — colorblind_reading and remedial_reading — introduce if instantiated instead?',
    'Comparative classification across the three sibling story files: identical referent (race-consciousness in admissions under the Fourteenth Amendment), divergent beneficiary/victim declarations and epsilon.',
    'colorblind_reading would declare no beneficiaries (no permitted use survives) and treat every racially classified applicant as bearing the full burden; remedial_reading would relocate beneficiaries to historically subordinated groups and anchor the victim set in caste-perpetuating selection structures. Epsilon and type would diverge sharply across readings over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_position, conceptual, 'Committer structure: this story instantiates the diversity reading of the equal-protection kernel; sibling readings are separate constraints linked through the network layer.').

omega_variable(
    compelling_interest_disagreement_locus,
    'Where exactly is the inter-reading disagreement located — which structural element of the kernel do the readings divide on?',
    'Doctrinal analysis isolating the contested element: which state interests are compelling enough to justify racial classification — remediation of identified discrimination (remedial reading), viewpoint-diversity educational benefits (this reading), or none whatsoever (colorblind reading).',
    'This reading''s classification turns on the diversity interest surviving strict scrutiny; if the interest locus shifted to remediation, the entire beneficiary/victim structure of this story would change wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compelling_interest_disagreement_locus, conceptual, 'The disagreement between kernel readings is located in the compelling-interest element, not in the tailoring element all readings accept.').

omega_variable(
    grutter_sunset_expectation_bindingness,
    'Was Grutter''s 25-year expectation (''we expect that 25 years from now, the use of racial preferences will no longer be necessary'') a binding sunset commitment or non-operative dictum?',
    'Doctrinal analysis of the passage''s operative status and subsequent courts'' treatment of it; comparison with arrangements carrying self-executing expiries.',
    'If binding, the arrangement was transitional support approaching a declared expiry (scaffold-like); as dictum, it is a durable permission with an aspiration attached — supporting the tangled_rope reading authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grutter_sunset_expectation_bindingness, conceptual, 'Whether the arrangement carried an operative sunset clause or merely rhetorical temporariness.').

omega_variable(
    individualized_harm_individuability,
    'Can the harm to any rejected applicant be individuated — how many rejected applicants would have been admitted but for the racial factor, and is that counterfactual recoverable at all?',
    'Counterfactual admissions modeling of the kind deployed in the SFFA litigation (competing expert simulations of admit/reject decisions under race-neutral versus race-conscious processes).',
    'Determines whether the victim set bears material allocative harm or primarily procedural harm from opacity; materially individuable harm would push epsilon above the authored band, purely obscured harm holds it low-moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualized_harm_individuability, empirical, 'Whether the obscured individual claims are recoverable harms or structurally unknowable ones.').

omega_variable(
    holistic_practice_post_repeal_persistence,
    'Did the review practice this reading licensed persist after its doctrinal termination — did race-consciousness migrate into essay evaluation and recruitment targeting, or did practice converge toward genuine race-neutrality?',
    'Post-2023 admissions outcome data, admissions-office practice audits, and enrollment-composition series at previously race-conscious institutions.',
    'Persistent practice under a revoked license indicates the arrangement''s function outlived its authorization (mandate-function decoupling); convergence indicates the doctrine, not the practice, was the load-bearing element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_practice_post_repeal_persistence, empirical, 'Whether the licensed practice outlived the license.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_diversity_reading_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t1978, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t1985, equal_protection_commitment__diversity_reading, theater_ratio, 1985, 0.11).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t1985, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t1995, equal_protection_commitment__diversity_reading, theater_ratio, 1995, 0.17).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t1995, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2003, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2016, equal_protection_commitment__diversity_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2016, observed).
narrative_ontology:measurement(ep_diversity_reading_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.5).
narrative_ontology:measurement_basis(ep_diversity_reading_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(ep_diversity_reading_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.16).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t1978, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t1985, equal_protection_commitment__diversity_reading, base_extractiveness, 1985, 0.19).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t1985, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t1995, equal_protection_commitment__diversity_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t1995, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.27).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2003, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2016, equal_protection_commitment__diversity_reading, base_extractiveness, 2016, 0.31).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2016, observed).
narrative_ontology:measurement(ep_diversity_reading_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.34).
narrative_ontology:measurement_basis(ep_diversity_reading_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(ep_diversity_reading_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t1978, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t1985, equal_protection_commitment__diversity_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t1985, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t1995, equal_protection_commitment__diversity_reading, suppression_requirement, 1995, 0.36).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t1995, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.48).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2003, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2016, equal_protection_commitment__diversity_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2016, observed).
narrative_ontology:measurement(ep_diversity_reading_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.85).
narrative_ontology:measurement_basis(ep_diversity_reading_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the constitutionality of affirmative action' decomposes into three structurally distinct readings of the equal_protection_commitment kernel, per the epsilon-invariance principle. This file instantiates the diversity reading: universities hold the beneficiary positions, all applicants bear obscured individual claims, and epsilon is low-moderate (0.34) over a procedural permission. The colorblind reading (separate file) declares no beneficiaries and treats every racially classified applicant as bearing the full burden; the remedial reading (separate file) relocates beneficiaries to historically subordinated groups and anchors victims in caste-perpetuating selection structures. The upstream/downstream structure runs through doctrinal citation: Bakke's diversity rationale was cited as the controlling settlement that narrowed remedial justifications to institution-specific findings, and colorblind challenges were adjudicated against it — hence the influences edge to remedial_reading and the foreclosure edge to colorblind_reading declared in cs_structure. Each sibling links back through its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
