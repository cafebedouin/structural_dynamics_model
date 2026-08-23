% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [SUPERSEDED]
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
 *   human_readable: Equal Protection Diversity Rationale for Race-Conscious Admissions (Diversity Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   A selective-public-university admissions regime in which race may be
 *   weighed as one factor among many in individualized file review,
 *   authorized as serving the compelling interest of educational diversity.
 *   The arrangement ran from the 1978 splintered decision that first
 *   articulated the rationale to the 2023 ruling that withdrew it.
 *   Universities administered holistic review and collected compositional
 *   discretion; applicants competed under evaluation they could not inspect,
 *   with race capable of weighing for or against any file and no
 *   individualized accounting afterward. This story instantiates ONE reading
 *   of the equal protection kernel (see kernel_context); per the family
 *   decomposition, the same standing arrangement assessed under the
 *   colorblind reading authors near-maximal epsilon (every racial
 *   classification a wrong, all classified parties victims), and under the
 *   remedial reading authors a different profile again (under-inclusive
 *   relative to caste dismantling). This file authors epsilon from the
 *   diversity reading's own lights: procedural, bounded, real but limited
 *   extraction.
 *
 * KEY AGENTS:
 *   - public_research_universities: agenda-setting beneficiary (institutional/constrained) - administers the arrangement and collects compositional discretion
 *   - admissions_applicant_pool: primary target (powerless/constrained) - bears diffuse, opaque competitive costs
 *   - plus_factor_recipients: secondary beneficiary (powerless/mobile) - receives weighted consideration
 *   - historically_excluded_communities: dual-positioned (organized/identity-locked) - intended beneficiaries who also supply the classification the arrangement runs on
 *   - federal_courts: enforcement agenda-setter (institutional/analytical) - polices the boundaries; withdrew the permission in 2023
 *   - diversity_administrators: professional beneficiary (moderate/identity-locked)
 *   - race_neutral_criteria_advocates: excluded opposition (organized/trapped)
 *   - constitutional_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.45).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Diversity Rationale for Race-Conscious Admissions (Diversity Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'd6b311e0-71a0-4079-9e9f-fc25d7e9a23e').
narrative_ontology:cs_kernel_codification('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', fixed_text).
narrative_ontology:cs_authority_grounding('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', lineage).
narrative_ontology:cs_interpretation_layer_present('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e').
narrative_ontology:cs_reading_relation('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', educational_diversity_compelling_interest, instrumental).
narrative_ontology:cs_axiom('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', secondary, individualized_holistic_review_requirement).
narrative_ontology:cs_axiom_status(individualized_holistic_review_requirement, holdable).
narrative_ontology:cs_axiom_grounding('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', individualized_holistic_review_requirement, conventional).
narrative_ontology:cs_reference_frame('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', inclusive_equality_citizenship_frame).
narrative_ontology:cs_drift_state('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', post_sffa_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d6b311e0-71a0-4079-9e9f-fc25d7e9a23e', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, public_research_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, plus_factor_recipients).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, admissions_applicant_pool).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, historically_excluded_communities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, diversity_administrators).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, historically_excluded_communities).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, strict_scrutiny_individualized_review_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer admissions at selective public institutions. Under the arrangement they compose entering classes through holistic file review in which an applicant's race may be weighed alongside grades, test scores, background, and essays. They gain discretionary control over class composition and a settled legal footing for pursuing institutional missions; they absorb compliance costs, litigation exposure, and periodic political backlash. Leaving the arrangement means adopting race-neutral selection methods, which several state systems did under voter mandates, while peer institutions and accrediting bodies continue to press diversity goals.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, public_research_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, public_research_universities, beneficiary).

% Everyone competing for seats at selective institutions under the arrangement. Because review is holistic and deliberation confidential, no applicant can see how race figured in their own decision; unsuccessful applicants receive no individualized accounting at all. Competition concentrates on the most selective campuses, so weight given to some files displaces others without any participant observing the trade. Applying elsewhere is possible but does not exit the evaluation method at any school that uses it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, admissions_applicant_pool, payer,
    powerless, biographical, constrained, national).

% Applicants whose files were strengthened by favorable weight given to their race or ethnicity in holistic review. Most learn only the admit or deny decision, never how the factor entered their evaluation. Some later report stigma or confidence concerns attributed to preferential consideration; others gained access to institutions otherwise out of reach. Exit consists of applying to other institutions, including state systems that do not use race.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, plus_factor_recipients, beneficiary,
    powerless, biographical, mobile, national).

% Black, Latino, Native, and other communities whose historical exclusion the arrangement addresses. Members gained admission pathways at selective institutions, and community organizations defended the arrangement in court and at the ballot. The same communities supply the classification the arrangement runs on: eligibility turns on their identity, institutions cite them as justification, and members carry the representational burden of being the stated reason for other people's admissions outcomes. Stepping out of that position is not available without dissolving the very category the arrangement assigns them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, historically_excluded_communities, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, historically_excluded_communities, payer).

% Adjudicate whether specific admissions programs survive strict scrutiny: whether the diversity interest is compelling, whether review is truly individualized, whether race-neutral alternatives could achieve similar classes. Successive major opinions defined, consolidated, and finally withdrew the permission. Their docket sets the arrangement's boundaries; they collect nothing from its operation and bear none of its admissions consequences.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Admissions deans, enrollment managers, and diversity officers whose professional roles center on composing diverse classes and defending the practice publicly. Careers, offices, and program budgets were built around the arrangement's continuation. Moving to race-neutral methods means rebuilding recruitment pipelines and redefining professional purpose; several institutions kept their mission statements intact after the practice ended.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, diversity_administrators, beneficiary,
    moderate, biographical, identity_locked, national).

% Voters, legislators, and litigants who pressed for admissions decided without racial weighing. Outside the doctrinal settlement for decades: courts declined their reading, so their avenues were ballot initiatives in individual states and repeated lawsuits, each failing until the final challenge succeeded. Within the arrangement's lifetime they could not opt out of its coverage - public universities in non-ban states applied it regardless of their preferences.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, race_neutral_criteria_advocates, excluded,
    organized, generational, trapped, national).

% Legal academics and historians who map the doctrine's lineage from the Reconstruction amendments through the Plessy dissent, the civil-rights rulings, the 1978 articulation, the 2003 consolidation, and the 2023 reversal. They publish critiques and defenses, trace the doctrinal migration from remedial to diversity justifications, and bear no admissions stakes.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, public_research_universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a stable constitutional framework under which selective public universities may weigh race as one element of individualized file review: it resolves the collision between institutional missions and equal-protection guarantees, channels race-consciousness away from quotas toward holistic assessment, and lowers litigation uncertainty for institutions and applicants alike.
% TRANSFER_FUNCTION: Moves allocative discretion over scarce selective-admission seats from applicants - who surrender transparent, uniformly applied criteria and any individualized account of how race entered their own decision - to universities, which gain control of class composition; incidental access gains flow to some applicants, diffuse competitive costs to others.
% ABSENT_VOICES: Applicants themselves never sat in the conversation: the settlement was negotiated among universities, federal courts, and civil-rights organizations, while the people whose files were weighed had no seat and received no individualized accounting afterward. Advocates of strictly race-neutral selection stood outside the doctrinal consensus for decades, heard only through ballot initiatives and repeated litigation until the final challenge succeeded.
% DISAPPEARANCE_RATIONALE: Selective admissions at hundreds of institutions reorganize immediately: class compositions shift, recruitment pipelines rebuild around race-neutral criteria, diversity offices restructure, and pending litigation collapses. The arrangement structured who attended the most selective institutions for forty-five years; its removal visibly rearranged enrollments in ban states and nationally after 2023.
% FOUNDING_PROBLEM: After courts rejected remedial justifications for race-conscious state action, selective universities needed a constitutionally durable basis for continuing to weigh race in admissions; the diversity rationale supplied that authorization where remediation could no longer survive strict scrutiny.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court's own opinions corroborate the genealogy from outside the university beneficiary set: the late-1980s state-action cases closed the remedial route, the 2003 decision rested authorization solely on the diversity interest while attaching a 25-year expectation, and the 2023 reversal found the interest not compelling at all - three successive authoritative attestations that the founding authorization lapsed. State legislatures and ballot measures reaching the opposite policy conclusion nonetheless agree the original authorization is gone.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.28: real but bounded - the procedural frame (individualized review, no quotas) capped the most extractive forms, while holistic confidentiality left individual claims uninspectable and diffuse competitive costs fell across the pool. Suppression 0.45 is a raw structural property, unscaled by power or scope: the doctrine constrained universities to individualized methods and left applicants no recourse to inspect their own evaluation, yet race-neutral alternatives stayed open and were adopted wherever voters mandated them. Only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater 0.38: the review function was real, but 'critical mass' was never defined and compliance documentation grew into a performance layer. Accessibility collapse 0.35: alternatives demonstrably persisted (percentage plans, socioeconomic proxies, ban-state systems). Resistance 0.72: forty-five years of nearly continuous litigation, ballot measures in multiple states, and legislative action, ending in successful repeal. The three measurement series share one grid ({0,9,18,27,36,45}; t=0 is the 1978 articulation, t=45 the 2023 repeal year). The terminal points record the repeal-year collapse - withdrawal of the permission and abandonment of enforcement - which is termination, not reform, and should not be read as the arrangement becoming benign.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the university seat the arrangement is mission infrastructure it built, defends, and staffs - coordination it experiences as its own. From the applicant-pool seat the same structure is opaque evaluation: costs are real, diffuse, and unverifiable, and zero-sum competition for elite seats impedes the coalition power that usually disciplines extraction from powerless classes. From the court seat it is a scrutiny problem with no stake in outcomes. From the administrator seat it is professional identity. The engine computes per-seat types from these structural positions; the divergence between the university seat's coordination experience and the applicant seat's extraction experience is the perspectival datum.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities sit nearest the beneficiary end: they collect the discretion the arrangement transfers, and their constrained exit (accreditation, peer pressure, mission commitments) deepens their stake. The applicant pool sits nearest the target end: it pays in displaced probability and forfeited transparency, with no inspection rights. Plus-factor recipients hold low directionality but weaker than universities - individual, mobile, often unaware of the factor's operation. Historically excluded communities occupy a genuinely dual position: access gains pull them toward the beneficiary end; supplying the classification the arrangement runs on, and carrying the representational burden, push them back toward the middle. Courts sit near symmetric - they enforce without collecting. Administrators hold low directionality amplified by identity lock: careers fused to the arrangement's continuation. No directionality overrides were needed; the beneficiary/victim declarations plus exit atoms derive these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding authorization was remedial in origin and died doctrinally in the late 1980s and 1990s, when courts held remediation could not sustain race-conscious state action at this scale; the arrangement survived by migrating to the diversity rationale, which was itself given a 25-year expectation that no institution operationalized. The mandate thus outlived its function twice over. Resolving mandatrophy here prevents two mislabels: reading the arrangement as pure extraction would erase the genuine coordination function (a stable framework that channeled race-consciousness away from quotas and lowered litigation uncertainty for forty-five years); reading it as pure coordination would erase the applicant-side costs and the opacity that made them invisible. The dead founding problem combined with a world_rearranges disappearance verdict flags the zombie pattern for cross-check against the theater path - consistent with the theater ratio's climb from 0.18 to 0.55 across the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (diversity_reading) of the equal_protection_commitment kernel; what structural deltas would the sibling readings (colorblind_reading, remedial_reading) produce if instantiated instead?',
    'Compare against the sibling constraint stories in the same family; each sibling authors its own epsilon, beneficiary/victim sets, and type from its own lights over the same standing arrangement.',
    'Under colorblind_reading, universities leave the beneficiary set, every racially classified applicant enters the victim set, and epsilon approaches the maximum; under remedial_reading, the victim set re-specifies to caste-perpetuating institutional structures and the arrangement reads as under-inclusive rather than extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel reading this story instantiates and what siblings would change.').

omega_variable(
    compelling_interest_empirical_basis,
    'Does the educational-diversity interest rest on measurable educational benefits, or on institutional assertion immune to evidence?',
    'Longitudinal studies of learning outcomes, viewpoint exchange, and post-graduation results across demographically varied versus homogeneous cohorts.',
    'If benefits fail replication, the reading''s foundational axiom loses its instrumental warrant and foreclosure pressure toward the colorblind reading intensifies; robust findings stabilize the hybrid coordination-extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_empirical_basis, empirical, 'Empirical status of the diversity rationale''s core premise.').

omega_variable(
    critical_mass_operationalization,
    'Was the operative target of ''critical mass'' genuine viewpoint diversity, or de facto demographic representation hidden behind deliberately undefined terminology?',
    'Compare achieved class compositions against demographic baselines across institutions and years; mechanical tracking of population shares indicates representation targets rather than diversity of viewpoints.',
    'A representation-target finding raises effective extraction well above the authored 0.28 and pushes the arrangement toward the snare end; a composition-insensitive finding supports the procedural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_operationalization, empirical, 'Whether the arrangement''s real target was diversity or proportional representation.').

omega_variable(
    applicant_penalty_magnitude,
    'How large were the individual admission penalties borne by disfavored applicants, given that holistic review concealed them?',
    'Natural experiments from state bans and the 2023 federal repeal: pre/post admission outcomes by applicant subgroup.',
    'Large measured penalties raise epsilon above the authored value and strengthen the extraction half of the hybrid reading; negligible penalties support reclassification toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(applicant_penalty_magnitude, empirical, 'Magnitude of concealed individual costs across the applicant pool.').

omega_variable(
    grutter_sunset_sincerity,
    'Was the announced 25-year sunset expectation a sincere transition commitment or performative deference with no operational content?',
    'Search the pre-2023 institutional record for funded transition plans, published milestones, or board-level wind-down decisions.',
    'Absence of any transition machinery confirms the sunset as theater and supports inertial-drift diagnosis had the arrangement survived; presence would partially rehabilitate a transitional-support reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grutter_sunset_sincerity, empirical, 'Sincerity of the doctrine''s embedded sunset expectation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_diversity_reading_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(epc_diversity_reading_tr_t0, observed).
narrative_ontology:measurement(epc_diversity_reading_tr_t9, equal_protection_commitment__diversity_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement_basis(epc_diversity_reading_tr_t9, observed).
narrative_ontology:measurement(epc_diversity_reading_tr_t18, equal_protection_commitment__diversity_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(epc_diversity_reading_tr_t18, observed).
narrative_ontology:measurement(epc_diversity_reading_tr_t27, equal_protection_commitment__diversity_reading, theater_ratio, 27, 0.34).
narrative_ontology:measurement_basis(epc_diversity_reading_tr_t27, observed).
narrative_ontology:measurement(epc_diversity_reading_tr_t36, equal_protection_commitment__diversity_reading, theater_ratio, 36, 0.41).
narrative_ontology:measurement_basis(epc_diversity_reading_tr_t36, observed).
narrative_ontology:measurement(epc_diversity_reading_tr_t45, equal_protection_commitment__diversity_reading, theater_ratio, 45, 0.55).
narrative_ontology:measurement_basis(epc_diversity_reading_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(epc_diversity_reading_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(epc_diversity_reading_be_t0, observed).
narrative_ontology:measurement(epc_diversity_reading_be_t9, equal_protection_commitment__diversity_reading, base_extractiveness, 9, 0.26).
narrative_ontology:measurement_basis(epc_diversity_reading_be_t9, observed).
narrative_ontology:measurement(epc_diversity_reading_be_t18, equal_protection_commitment__diversity_reading, base_extractiveness, 18, 0.31).
narrative_ontology:measurement_basis(epc_diversity_reading_be_t18, observed).
narrative_ontology:measurement(epc_diversity_reading_be_t27, equal_protection_commitment__diversity_reading, base_extractiveness, 27, 0.34).
narrative_ontology:measurement_basis(epc_diversity_reading_be_t27, observed).
narrative_ontology:measurement(epc_diversity_reading_be_t36, equal_protection_commitment__diversity_reading, base_extractiveness, 36, 0.33).
narrative_ontology:measurement_basis(epc_diversity_reading_be_t36, observed).
narrative_ontology:measurement(epc_diversity_reading_be_t45, equal_protection_commitment__diversity_reading, base_extractiveness, 45, 0.12).
narrative_ontology:measurement_basis(epc_diversity_reading_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(epc_diversity_reading_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(epc_diversity_reading_su_t0, observed).
narrative_ontology:measurement(epc_diversity_reading_su_t9, equal_protection_commitment__diversity_reading, suppression_requirement, 9, 0.34).
narrative_ontology:measurement_basis(epc_diversity_reading_su_t9, observed).
narrative_ontology:measurement(epc_diversity_reading_su_t18, equal_protection_commitment__diversity_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement_basis(epc_diversity_reading_su_t18, observed).
narrative_ontology:measurement(epc_diversity_reading_su_t27, equal_protection_commitment__diversity_reading, suppression_requirement, 27, 0.48).
narrative_ontology:measurement_basis(epc_diversity_reading_su_t27, observed).
narrative_ontology:measurement(epc_diversity_reading_su_t36, equal_protection_commitment__diversity_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement_basis(epc_diversity_reading_su_t36, observed).
narrative_ontology:measurement(epc_diversity_reading_su_t45, equal_protection_commitment__diversity_reading, suppression_requirement, 45, 0.08).
narrative_ontology:measurement_basis(epc_diversity_reading_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional status of race-conscious admissions' decomposes into three structurally distinct constraints sharing one kernel text (the Equal Protection Clause): this diversity reading (epsilon 0.28, universities benefit, applicants bear diffuse costs, procedural frame), the colorblind reading (the same arrangement assessed as categorically impermissible - near-maximal epsilon, all classified parties victims), and the remedial reading (assessed as under-inclusive relative to caste dismantling - different victim specification entirely). Each story carries its own stable epsilon; this file links both siblings via affects_constraints. Direction of influence: the diversity reading's doctrinal success crowded out the remedial reading's legal force, and the colorblind reading's final victory reversed the diversity reading outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
