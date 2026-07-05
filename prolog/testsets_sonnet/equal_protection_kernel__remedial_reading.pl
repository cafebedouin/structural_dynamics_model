% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Equal Protection Clause — Remedial/Diversity-Interest Reading (Race-Conscious Admissions)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the remedial/diversity-interest reading of the
 *   Equal Protection Clause's application to race-conscious university
 *   admissions: the view, dominant in doctrine from Bakke (1978) through
 *   Grutter (2003) and eroding through Fisher and Students for Fair
 *   Admissions (2023), that narrowly tailored consideration of race is
 *   constitutionally permissible when justified by a compelling interest in
 *   remedying documented historical exclusion or achieving educational
 *   diversity. This is a distinct constraint from the colorblind reading
 *   (which forecloses any racial classification regardless of purpose) and
 *   the antisubordination reading (which asks not whether race was used but
 *   whether the action entrenches or dismantles caste hierarchy). The three
 *   readings produce different beneficiary/victim sets and different
 *   classifications from the same clause text; this story addresses only the
 *   remedial reading, per the ε-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - historically_excluded_applicant_groups: primary beneficiary (moderate/constrained) — gains plus-factor consideration
 *   - universities_pursuing_diversity_mandates: agenda_setter/beneficiary (institutional/constrained) — designs and defends the admissions scheme
 *   - marginally_displaced_applicants: primary payer (moderate/constrained) — bears the marginal admissions cost without individualized recourse
 *   - federal_judiciary: observer/agenda_setter (institutional/analytical) — sets and eventually narrowed the doctrine's boundaries
 *   - state_legislatures_and_ballot_initiatives: excluded (organized/mobile) — barred the practice locally, outside the federal constitutional conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.42).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.31).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial/Diversity-Interest Reading (Race-Conscious Admissions)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '045ad9c5-782b-47e4-a97d-edac440ae5da').
narrative_ontology:cs_kernel_codification('045ad9c5-782b-47e4-a97d-edac440ae5da', fixed_text).
narrative_ontology:cs_authority_grounding('045ad9c5-782b-47e4-a97d-edac440ae5da', lineage).
narrative_ontology:cs_interpretation_layer_present('045ad9c5-782b-47e4-a97d-edac440ae5da').
narrative_ontology:cs_reading_relation('045ad9c5-782b-47e4-a97d-edac440ae5da', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('045ad9c5-782b-47e4-a97d-edac440ae5da', equal_protection_kernel__antisubordination_reading, influences).
narrative_ontology:cs_axiom('045ad9c5-782b-47e4-a97d-edac440ae5da', foundational, documented_historical_exclusion_justifies_remedial_classification).
narrative_ontology:cs_axiom_status(documented_historical_exclusion_justifies_remedial_classification, overridden).
narrative_ontology:cs_axiom_grounding('045ad9c5-782b-47e4-a97d-edac440ae5da', documented_historical_exclusion_justifies_remedial_classification, empirically_contingent).
narrative_ontology:cs_axiom('045ad9c5-782b-47e4-a97d-edac440ae5da', foundational, compositional_diversity_is_compelling_educational_interest).
narrative_ontology:cs_axiom_status(compositional_diversity_is_compelling_educational_interest, holdable).
narrative_ontology:cs_axiom_grounding('045ad9c5-782b-47e4-a97d-edac440ae5da', compositional_diversity_is_compelling_educational_interest, instrumental).
narrative_ontology:cs_axiom('045ad9c5-782b-47e4-a97d-edac440ae5da', secondary, narrow_tailoring_bounds_permissible_race_conscious_action).
narrative_ontology:cs_axiom_status(narrow_tailoring_bounds_permissible_race_conscious_action, holdable).
narrative_ontology:cs_axiom_grounding('045ad9c5-782b-47e4-a97d-edac440ae5da', narrow_tailoring_bounds_permissible_race_conscious_action, conventional).
narrative_ontology:cs_reference_frame('045ad9c5-782b-47e4-a97d-edac440ae5da', post_brown_transitional_remedy_framework).
narrative_ontology:cs_drift_state('045ad9c5-782b-47e4-a97d-edac440ae5da', students_for_fair_admissions_2023, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('045ad9c5-782b-47e4-a97d-edac440ae5da', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_applicant_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_pursuing_diversity_mandates).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, marginally_displaced_applicants).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_diversity_interest_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, narrow_tailoring_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups subject to documented historical exclusion from higher education gain a plus-factor consideration in admissions. They cannot individually verify whether any specific admission was attributable to the policy versus their own qualifications, and the benefit is contingent on courts continuing to recognize the diversity or remedial rationale as compelling.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_applicant_groups, beneficiary,
    moderate, generational, constrained, national).

% Design and administer admissions processes that weigh race as one factor among many, and must document narrow tailoring to survive strict scrutiny. They gain reputational and mission-legitimacy benefits from demonstrable diversity, but bear compliance costs and litigation exposure, and must periodically re-justify the practice as courts narrow the doctrine's boundaries.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_pursuing_diversity_mandates, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, universities_pursuing_diversity_mandates, beneficiary).

% Applicants who would have been admitted under a strictly race-blind process but are not admitted because a race-conscious factor shifted the marginal decision. They cannot identify with certainty that they were displaced by the policy specifically, which forecloses individualized legal remedy even where the systemic effect is real; their only recourse is facial or as-applied constitutional challenge to the admissions scheme itself.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, marginally_displaced_applicants, payer,
    moderate, biographical, constrained, national).

% Adjudicates whether specific admissions programs satisfy strict scrutiny's narrow-tailoring requirement, sets the durability window for the reading (historically signaled as time-limited, e.g. the Grutter 25-year expectation), and can foreclose the reading entirely through subsequent rulings, as it substantially did in 2023.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, federal_judiciary, agenda_setter).

% In several states, voters or legislatures banned race-conscious admissions entirely via ballot initiative or statute, operating independently of the federal constitutional floor. Their preference for a colorblind approach was not treated as authoritative on the constitutional question itself while the remedial reading held sway, only as a permissible state-level policy choice layered on top of it.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_legislatures_and_ballot_initiatives, excluded,
    organized, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a limited, time-bounded institutional response to documented historical exclusion from higher education access, allowing universities to pursue compositional diversity as an educational good without each admissions decision individually re-litigating the constitutional question.
% TRANSFER_FUNCTION: Moves marginal admissions slots at selective institutions from applicants who would prevail under strict race-blind ranking to applicants from groups the institution has determined warrant a plus factor, and moves reputational/mission-legitimacy value to the administering university.
% ABSENT_VOICES: Applicants displaced at the margin are structurally unable to identify themselves as displaced (no counterfactual admission letter exists to prove it), so they cannot organize as a class the way an identifiable victim group could; state legislatures that banned the practice locally are excluded from shaping the federal constitutional baseline itself.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, admissions offices would revert to facially race-blind criteria (as many now must post-2023), compositional diversity at selective institutions would shift measurably within one to two admissions cycles, and the litigation and compliance apparatus built around narrow-tailoring documentation would become moot — a substantial institutional and legal infrastructure currently depends on this reading's continued availability.
% FOUNDING_PROBLEM: Selective universities and professional schools remained overwhelmingly non-diverse for decades after formal desegregation because facially neutral criteria reproduced historical exclusion; the reading was built to permit institutions to remedy that reproduction directly rather than waiting for facially neutral policy to self-correct.
% FOUNDING_PROBLEM_CORROBORATION: Universities and civil rights organizations attest the founding problem persists, citing continued underrepresentation relative to population share. Independent empirical work (e.g. studies of post-ban admissions in California and Michigan) is more mixed, and the Supreme Court's 2023 majority — a body outside the beneficiary set — concluded that the remedial rationale as historically administered lacked the durable, measurable endpoint the doctrine originally promised, effectively finding the founding problem inadequately bounded rather than resolved or ongoing.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) reflects a genuine but bounded transfer: marginal admissions slots move from one applicant pool to another, but the transfer is capped by narrow-tailoring requirements (holistic review, no quotas, individualized consideration) that constrain how much redistribution the doctrine permits. Suppression (0.31) is moderate — the doctrine does not compel any university to adopt race-conscious admissions, but institutions that do so must actively defend the practice against ongoing litigation, which functions as continuous enforcement pressure. Theater ratio (0.28) captures that some institutional diversity statements substitute performative commitment for the narrow, court-defensible documentation the doctrine actually requires — a documented pattern in post-Grutter compliance audits. Accessibility collapse (0.35) is moderate-low: race-blind admissions always remained a live legal alternative for institutions that chose it, and post-2023 became the mandatory alternative, so this reading never fully foreclosed the colorblind approach at the institutional level even while it was doctrinally dominant. Resistance (0.68) is high and rising over the interval — the doctrine faced sustained, well-resourced legal challenge culminating in its substantial narrowing in 2023, which is itself strong evidence this was never an uncontested coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the university/beneficiary-group seat, the arrangement reads as remedial coordination solving a real historical distortion in access. From the marginally displaced applicant's seat, the same structure operates as an enforced transfer they cannot even individually identify, let alone contest — this asymmetry of legibility (beneficiaries know they benefit; payers often cannot prove they paid) is itself a structural feature the engine should register as suppression-adjacent even though it is not coercive in the classical sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded applicant groups and administering universities are coded toward the beneficiary end: the former receive a plus factor, the latter receive institutional legitimacy and pursue the policy voluntarily where legally permitted. Marginally displaced applicants are coded toward the target end: they bear a real but diffuse and individually unprovable cost, which if anything makes their effective burden harder to remedy than a clearly identified injury would be — this is reflected in `constrained` rather than `trapped` exit, since race-blind institutions and alternative pathways exist, but not without real cost of relocation or reapplication.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented historical exclusion from selective higher education) does not cleanly resolve into either 'fully live' or 'fully dead' — hence founding_problem_status is authored as contested rather than either extreme. Treating this as settled coordination (ignoring the payer seat) would mislabel a genuine transfer as costless; treating it as pure extraction (ignoring the beneficiary seat and the coordination function of correcting documented historical distortion) would mislabel a bounded remedial mechanism as naked rent-seeking. The tangled_rope classification is intended to hold both: real coordination function (correcting documented exclusion, which the compelling-interest doctrine requires be shown, not merely asserted) AND asymmetric extraction (a real, if diffuse, cost falling on a specific payer class) sustained by active enforcement (ongoing litigation defense).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_versus_colorblind_kernel_dominance,
    'Is the equal_protection_kernel''s authoritative reading determined by which doctrinal era''s Supreme Court composition is asked, or is there a principled way to identify which reading the clause text itself compels?',
    'Track whether subsequent courts (post-2023) treat Students for Fair Admissions as a final resolution of the kernel or as one more oscillation in a longer historical cycle between remedial and colorblind readings (Plessy-era, Brown-era, Bakke-era, current era).',
    'If the kernel''s authoritative reading is genuinely cyclical rather than settled, this story''s classification as an active, currently-operative tangled_rope should be understood as historically bounded (1978-2023) rather than a permanent structural fact — which is why the interval is authored as closed rather than open-ended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_versus_colorblind_kernel_dominance, conceptual, 'Whether the kernel has one correct reading or cycles between readings by judicial composition.').

omega_variable(
    displaced_applicant_identifiability,
    'Can the class of marginally displaced applicants ever be identified with enough specificity to constitute a legally cognizable, individually remediable harm, or is the harm structurally diffuse by the nature of holistic admissions review?',
    'Empirical admissions-data reconstruction (as attempted in the Harvard/SFFA litigation record) comparing race-conscious and simulated race-blind admit lists to estimate displacement rates and identifiability.',
    'If displacement is genuinely unidentifiable at the individual level, the victim group is real in aggregate but permanently under-vindicated by individual-rights litigation frameworks — which affects how much weight the payer seat''s `constrained` exit rating should carry versus a rating closer to `trapped`.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_applicant_identifiability, empirical, 'Whether the payer class can ever obtain individualized legal recognition of its harm.').

omega_variable(
    remedial_purpose_documentation_sincerity,
    'When universities document a remedial or diversity rationale to satisfy strict scrutiny, is the documentation a sincere account of institutional reasoning or a post-hoc legal justification constructed to survive litigation?',
    'Compare internal institutional deliberation records (where available through discovery, as in the Harvard litigation) against the public-facing compelling-interest justifications submitted to courts.',
    'If documentation is substantially post-hoc, the theater_ratio for this constraint should be revised upward, and the coordination_function claim weakens relative to a pure extraction reading; sincere documentation supports the tangled_rope characterization as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_purpose_documentation_sincerity, empirical, 'Whether narrow-tailoring documentation reflects genuine institutional reasoning or litigation strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__remedial_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__remedial_reading, theater_ratio, 2013, 0.28).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_kernel__remedial_reading, theater_ratio, 2018, 0.32).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__remedial_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(equa_be_t2018, equal_protection_kernel__remedial_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.25).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__remedial_reading, suppression_requirement, 2013, 0.29).
narrative_ontology:measurement(equa_su_t2018, equal_protection_kernel__remedial_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the equal_protection_kernel (remedial, colorblind, antisubordination), each authored as a separate ε-invariant constraint per the decomposition rule. The remedial reading's ε (0.42, tangled_rope) reflects a bounded, court-supervised transfer mechanism; the colorblind reading (a categorical prohibition) and antisubordination reading (a hierarchy-focused test) would carry different beneficiary/victim structures and different ε values from the same clause text. The remedial reading structurally forecloses the colorblind reading during periods of judicial dominance and is itself substantially foreclosed by the colorblind reading's 2023 ascendance — the two readings cannot both govern live doctrine simultaneously, which is the rare `forecloses` relation rather than the more common `coexists_with`.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
