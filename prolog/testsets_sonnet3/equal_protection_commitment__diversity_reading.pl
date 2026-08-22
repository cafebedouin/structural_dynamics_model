% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Equal Protection — Diversity Rationale for Race-Conscious Admissions
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This story instantiates the DIVERSITY READING of the equal protection
 *   kernel: the doctrinal line running from Bakke (1978) through Grutter
 *   (2003) to SFFA v. Harvard/UNC (2023), holding that achieving educational
 *   diversity is a compelling state interest that permits universities to
 *   consider race as one factor among many in individualized, holistic
 *   review, subject to strict scrutiny and a bar on quotas. This is a
 *   distinct constraint from the colorblind reading (which forecloses any
 *   racial classification) and the remedial reading (which grounds
 *   race-consciousness in dismantling caste subordination rather than
 *   pedagogical diversity benefit). The three readings share a kernel — the
 *   Fourteenth Amendment's equal protection clause — but diverge on what the
 *   clause commits the state to, producing different beneficiary/victim
 *   structures and different ε. This story's ε is authored low-moderate
 *   (0.28) because the constraint is procedural (a standard of review
 *   conditioning WHEN race may be considered) rather than substantive (a rule
 *   about WHO gets what), and because narrow tailoring and individualized
 *   review formally cabin the extraction — though the SFFA majority's own
 *   finding that the rationale lacked a logical endpoint and was not
 *   meaningfully measurable is itself evidence pointing toward higher
 *   effective extraction than the doctrine's formal limits suggest.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.32).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection — Diversity Rationale for Race-Conscious Admissions").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'c90487f4-6e98-4c8c-9a90-94a46b4d77e0').
narrative_ontology:cs_kernel_codification('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', fixed_text).
narrative_ontology:cs_authority_grounding('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', lineage).
narrative_ontology:cs_interpretation_layer_present('c90487f4-6e98-4c8c-9a90-94a46b4d77e0').
narrative_ontology:cs_reading_relation('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', foundational, pedagogical_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(pedagogical_diversity_is_compelling_state_interest, overridden).
narrative_ontology:cs_axiom_grounding('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', pedagogical_diversity_is_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', secondary, race_may_be_one_factor_among_many_in_individualized_review).
narrative_ontology:cs_axiom_status(race_may_be_one_factor_among_many_in_individualized_review, overridden).
narrative_ontology:cs_axiom_grounding('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', race_may_be_one_factor_among_many_in_individualized_review, conventional).
narrative_ontology:cs_reference_frame('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', bakke_diversity_compelling_interest).
narrative_ontology:cs_drift_state('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', post_sffa_2023, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('c90487f4-6e98-4c8c-9a90-94a46b4d77e0', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants_admitted).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, university_administrators).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, rejected_applicants_all_backgrounds).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, asian_american_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers holistic admissions processes, invoking educational diversity as a compelling interest to justify considering race as one factor among many. Retains broad discretion over how 'holistic' review is weighted and documented, and controls the opacity of the process — courts grant deference to universities' own judgment about what diversity requires and how to achieve it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain admission at rates that reflect the diversity rationale's operation; benefit from the university's asserted interest in a diverse student body. Carries a secondary cost: the diversity rationale's individualized-consideration framing invites scrutiny of whether their admission reflects merit or racial preference, a stigma cost the remedial reading's justice framing would not attach in the same way.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_minority_applicants_admitted, beneficiary,
    moderate, biographical, constrained, national).

% Denied admission to a specific institution in a zero-sum seat allocation. Cannot know precisely how race functioned as 'one factor among many' because holistic review is opaque by design; cannot mount an individualized equal-protection claim against a process constructed specifically to avoid producing an individually-traceable racial harm.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, rejected_applicants_all_backgrounds, payer,
    powerless, immediate, trapped, national).

% Statistically bear a disproportionate share of the diversity rationale's rejection cost relative to academic credentials, per litigation record (Students for Fair Admissions). Have mobilized as an organized litigant class but remain structurally unable to force disclosure of exactly how race was weighted in any individual denial, because the diversity framework's discretion is precisely what shields the weighting from individualized review.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Professional careers and institutional legitimacy are built around administering the holistic, mission-driven admissions apparatus the diversity rationale authorizes. Benefit from the discretion the rationale confers and from insulation against strict-scrutiny challenge so long as the process is framed as pursuing educational diversity rather than racial balancing.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, university_administrators, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, university_administrators, agenda_setter).

% Adjudicates strict-scrutiny challenges to race-conscious admissions, historically granting universities substantial deference (Grutter) before substantially narrowing that deference (SFFA v. Harvard/UNC, 2023). Its own doctrine has shifted the ground under this reading during the interval measured.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows universities to pursue a pedagogically-grounded goal — a student body diverse enough to produce cross-racial learning, reduce stereotyping, and prepare students for a heterogeneous society and workforce — without being categorically barred from considering race at all, while cabining that consideration with strict scrutiny (narrow tailoring, individualized review, no quotas).
% TRANSFER_FUNCTION: Moves admission seats at selective institutions among applicant pools; shifts institutional legal and reputational risk from a categorical race-blindness rule to a supervised, fact-intensive discretion regime that universities must defend case by case.
% ABSENT_VOICES: Individual rejected applicants of any background are structurally absent from the decision that determined their outcome — holistic review is designed not to generate an individually-legible racial reason a specific applicant could contest. Asian American applicants as a class have organized litigation to be heard but the doctrine's discretion made their statistical showing insufficient for two decades before SFFA.
% DISAPPEARANCE_RATIONALE: If the diversity rationale were withdrawn (as it substantially was in SFFA v. Harvard, 2023), universities lose the specific compelling-interest justification for considering race directly; institutions must find race-neutral proxies (geography, income, first-generation status) or accept demographic shifts in enrollment. The post-SFFA period already shows universities rearranging admissions criteria and disclosure practices in direct response.
% FOUNDING_PROBLEM: Selective universities in the late 20th century argued that after formal desegregation, racially homogeneous student bodies persisted due to structural disparities in K-12 preparation, wealth, and legacy networks, and that colorblind facially-neutral admissions criteria alone would reproduce segregation-era enrollment patterns at elite institutions.
% FOUNDING_PROBLEM_CORROBORATION: Universities and affirmative-action advocacy organizations attest the underlying disparities (K-12 funding gaps, wealth gaps, legacy admissions preferences) remain live. Independent of the beneficiary institutions, the Supreme Court majority in SFFA v. Harvard/UNC (2023) found the diversity rationale had become unmoored from measurable, judicially-administrable ends and that universities could not demonstrate the racial classification was narrowly tailored to a compelling interest that had a logical endpoint — an attestation from the adjudicating body, not the universities themselves, that the founding problem as originally framed no longer supports the remedy as administered.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.28: moderate because a real, cognizable coordination interest (pedagogical diversity benefits) exists and constrains the practice via strict scrutiny, but non-trivial because holistic review's opacity means the actual weight given to race in any individual decision is unverifiable, and the rationale's 'no logical endpoint' problem (per SFFA) indicates the constraint drifted toward broader use over time — hence extractiveness rises from 0.18 (1978, immediately post-Bakke, narrowly cabined) to a peak near 0.30 (2018, pre-SFFA, expansive interpretation under Grutter/Fisher) before receding slightly to 0.28 as SFFA (2023) narrowed the doctrine within the interval's endpoint. Suppression rises steadily (0.20 to 0.32) reflecting the increasing procedural and evidentiary machinery (diversity studies, holistic-review documentation, amicus infrastructure) universities built to defend the practice against escalating strict-scrutiny challenge. Theater ratio is modest but rising (0.12 to 0.24 then settling at 0.22), reflecting the genuine but partially performative nature of 'individualized consideration' processes that must be documented in a form defensible in litigation regardless of their actual causal role in any decision.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are the clear structural beneficiary: the diversity rationale is precisely the doctrinal instrument that preserves their admissions discretion against a categorical colorblindness rule, and they administer the process (agenda_setter). Underrepresented minority applicants admitted under this framework are beneficiaries in outcome but carry a secondary directional cost — the diversity framing (unlike the remedial framing) invites public and legal scrutiny of whether their admission reflects merit, a stigma the remedial reading's justice-based framing does not equally impose. Rejected applicants of all backgrounds are diffuse payers with no individualized cause of action, and Asian American applicants are a payer group with organized standing but a structurally difficult evidentiary burden — precisely because the discretion the diversity rationale grants is what makes individual racial causation nearly impossible to prove, which is why their eventual win in SFFA required a statistical, class-wide showing rather than an individual one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (racially homogeneous elite enrollment persisting after formal desegregation despite facially neutral criteria) is contested as to whether it remains live in its original form. The SFFA majority's finding — that the diversity rationale had no logical stopping point and could not be tied to measurable ends — is precisely a mandatrophy signal: an arrangement whose original justification (a temporary corrective measure, addressed under the remedial reading) had been re-grounded in an open-ended pedagogical rationale (this reading) that never generated its own sunset condition. This story does not resolve whether the diversity rationale should sunset; it documents that the founding-problem status is genuinely contested and that the reading's absence of a declared endpoint is itself a structural feature the Court identified as a defect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_reading_logical_endpoint,
    'Does the diversity rationale, as a doctrinal construct, admit any principled stopping point (a measurable ''sufficient diversity'' condition), or is its indefiniteness structural to the reading itself?',
    'Track whether any post-SFFA institution successfully defends a race-conscious admissions program under a narrower diversity theory with a stated, judicially administrable endpoint. Absence of any such successful defense over a decade would corroborate the SFFA majority''s structural indefiniteness finding.',
    'If genuinely endpoint-less, this reading is structurally scaffold-like in aspiration (transitional coordination) but never carried an authored sunset clause — which is exactly the gap SFFA identified and used to substantially foreclose it in the university context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_reading_logical_endpoint, conceptual, 'Whether the diversity rationale has (or could have) a principled endpoint distinct from open-ended discretion.').

omega_variable(
    diversity_vs_remedial_beneficiary_overlap,
    'To what extent do the diversity reading and the remedial reading produce overlapping real-world beneficiary sets (the same admitted students) despite resting on distinct normative premises (pedagogical benefit vs. anti-subordination)?',
    'Compare admissions outcome data under diversity-rationale-justified programs against outcomes that would obtain under an explicitly remedial (anti-subordination) framework targeting the same disparities; assess whether the population of beneficiaries differs meaningfully.',
    'High overlap would suggest the readings are doctrinally distinct but practically fungible, meaning courts'' choice between them functions more as a legitimacy/framing device than as a substantive constraint on outcomes — relevant to whether SFFA''s rejection of the diversity rationale specifically (while leaving remedial-type arguments less directly addressed) has the practical bite the Court intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_vs_remedial_beneficiary_overlap, empirical, 'Whether diversity and remedial readings produce the same beneficiaries in practice despite different premises.').

omega_variable(
    holistic_review_opacity_as_extraction_shield,
    'Is the opacity of holistic review (which prevents individual applicants from establishing individualized racial injury) a necessary feature of any race-conscious admissions process under strict scrutiny, or is it a design choice that specifically shields the diversity rationale from more searching individual-level review?',
    'Compare admissions systems that disclose individual weighting rationale (if any exist post-SFFA transparency requirements) against fully opaque holistic systems, on measures of both litigation exposure and racial-composition outcomes.',
    'If opacity is a design choice rather than a functional necessity, the suppression metric authored here (0.32) understates the degree to which the constraint''s persistence depends on preventing individually-legible claims, which would push the constraint''s classification toward snare for the payer seat specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(holistic_review_opacity_as_extraction_shield, conceptual, 'Whether holistic-review opacity is functionally necessary or a shield against individual claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(equa_tr_t1988, equal_protection_commitment__diversity_reading, theater_ratio, 1988, 0.14).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__diversity_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_commitment__diversity_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.18).
narrative_ontology:measurement(equa_be_t1988, equal_protection_commitment__diversity_reading, base_extractiveness, 1988, 0.2).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.24).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__diversity_reading, base_extractiveness, 2013, 0.27).
narrative_ontology:measurement(equa_be_t2018, equal_protection_commitment__diversity_reading, base_extractiveness, 2018, 0.3).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1988, equal_protection_commitment__diversity_reading, suppression_requirement, 1988, 0.22).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.26).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__diversity_reading, suppression_requirement, 2013, 0.29).
narrative_ontology:measurement(equa_su_t2018, equal_protection_commitment__diversity_reading, suppression_requirement, 2018, 0.31).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).

% DUAL FORMULATION NOTE:
% This constraint, equal_protection_commitment__remedial_reading, and equal_protection_commitment__colorblind_reading form a three-member constraint family reading the same Fourteenth Amendment equal-protection kernel. Each authors its own ε: the colorblind reading (near-mountain — forbids classification categorically, ε near-zero for the classification itself but potentially high suppression cost to those the classification would have helped), the remedial reading (moderate-high ε, explicitly anti-subordination premised, narrower beneficiary class tied to specific historical subordination), and this diversity reading (low-moderate ε, procedural rather than substantive, universities as primary beneficiary). The colorblind reading substantially forecloses this diversity reading in the university-admissions context as of SFFA (2023); the remedial reading and this reading coexist as live alternative justifications courts and litigants continue to press in different contexts (e.g., military academy admissions, which SFFA left open).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
