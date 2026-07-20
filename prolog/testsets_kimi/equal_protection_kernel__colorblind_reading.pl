% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Colorblind Reading of the Equal Protection Clause
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the colorblind_reading of the
 *   equal_protection_kernel. It holds that the Equal Protection Clause
 *   categorically forbids any state use of racial classification regardless
 *   of purpose, producing a formally equality framework that blocks
 *   race-conscious remediation in state university admissions and other
 *   domains. The reading forecloses both the remedial_reading (permitting
 *   race-conscious remediation) and the antisubordination_reading (targeting
 *   subordination rather than classification) within a single interpretive
 *   framework.
 *
 * KEY AGENTS:
 *   - federal_judiciary (agenda_setter / institutional / analytical) â administers and enforces the colorblind interpretation through judicial review
 *   - state_universities (payer / institutional / constrained) â lose policy flexibility and must eliminate race-conscious admissions
 *   - underrepresented_applicants (payer / powerless / trapped) â lose remedial pathways; cannot exit the constraint's distributive effect
 *   - overrepresented_applicants (beneficiary / moderate / mobile) â gain relative competitive position under formally neutral rules
 *   - civil_rights_advocates (excluded / organized / constrained) â structurally marginalized from the interpretive framework
 *   - constitutional_originalists (beneficiary / organized / constrained) â gain ideological vindication and doctrinal influence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.72).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.82).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Colorblind Reading of the Equal Protection Clause").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '2f5d8697-6a7b-4714-bde4-18823a75fdda').
narrative_ontology:cs_kernel_codification('2f5d8697-6a7b-4714-bde4-18823a75fdda', fixed_text).
narrative_ontology:cs_authority_grounding('2f5d8697-6a7b-4714-bde4-18823a75fdda', lineage).
narrative_ontology:cs_interpretation_layer_present('2f5d8697-6a7b-4714-bde4-18823a75fdda').
narrative_ontology:cs_reading_relation('2f5d8697-6a7b-4714-bde4-18823a75fdda', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('2f5d8697-6a7b-4714-bde4-18823a75fdda', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('2f5d8697-6a7b-4714-bde4-18823a75fdda', foundational, racial_classification_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classification_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('2f5d8697-6a7b-4714-bde4-18823a75fdda', racial_classification_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('2f5d8697-6a7b-4714-bde4-18823a75fdda', foundational, state_neutrality_no_remedial_duty).
narrative_ontology:cs_axiom_status(state_neutrality_no_remedial_duty, holdable).
narrative_ontology:cs_axiom_grounding('2f5d8697-6a7b-4714-bde4-18823a75fdda', state_neutrality_no_remedial_duty, deontological).
narrative_ontology:cs_reference_frame('2f5d8697-6a7b-4714-bde4-18823a75fdda', anti_classification_baseline).
narrative_ontology:cs_drift_state('2f5d8697-6a7b-4714-bde4-18823a75fdda', post_sffa_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f5d8697-6a7b-4714-bde4-18823a75fdda', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, overrepresented_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, constitutional_originalists).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, underrepresented_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, state_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, anti_classification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Equal Protection Clause through judicial review, striking down race-conscious state policies and policing university admissions. Derives institutional authority from the constitutional text and precedent. Exit is analytical â the judiciary can revise interpretation but only through the slow mechanism of doctrinal evolution or new appointments.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Operate admissions programs under constitutional constraint. Lose the policy tool of race-conscious holistic review and must redesign admissions around race-neutral proxies. Exit is constrained because noncompliance means litigation loss, funding risk, and reputational sanction; they cannot simply disregard federal constitutional interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, state_universities, payer,
    institutional, biographical, constrained, national).

% Apply to selective state universities under a regime that forbids explicit consideration of their underrepresented status. Lose a remedial pathway designed to offset inherited structural disadvantage. Exit is trapped because racial identity is immutable for admissions purposes; they cannot opt out of the constraint's distributive effect.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, underrepresented_applicants, payer,
    powerless, biographical, trapped, national).

% Compete for selective admissions slots under formally neutral criteria. Benefit from the removal of race-conscious holistic factors that would otherwise advantage underrepresented competitors. Exit is mobile because they can apply widely and the constraint subsidizes their relative competitive position across the national market.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, overrepresented_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Advocate for antisubordination and remedial readings of the Clause. Are structurally excluded from the colorblind interpretive framework â their arguments that race-conscious remediation is constitutionally permitted are ruled out a priori. Exit is constrained because they must operate within a legal discourse that forecloses their core claims.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocates, excluded,
    organized, generational, constrained, national).

% Promote an interpretive methodology that treats the colorblind reading as the original meaning of the Fourteenth Amendment. Gain ideological vindication and institutional influence as the reading becomes doctrine. Exit is constrained because their influence depends on judicial appointment politics and doctrinal adherence.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_originalists, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, overrepresented_applicants).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line judicial rule against state use of racial classifications, providing formal equality, predictability, and a check on state racial caste systems in constitutional review.
% TRANSFER_FUNCTION: Moves admissions opportunity away from underrepresented minority applicants and race-conscious remediation authority away from state universities; moves relative competitive advantage toward overrepresented applicants and interpretive authority toward the federal judiciary and originalist frameworks.
% ABSENT_VOICES: Civil rights advocates and antisubordination jurists who argue the Clause permits or requires race-conscious remediation to dismantle inherited hierarchy; they are structurally excluded because the categorical anti-classification framework rules their reading out a priori.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished overnight, state universities would immediately resume race-conscious admissions programs; underrepresented applicant access pipelines would shift within a single cycle; the federal judiciary would lose a flagship instrument of constitutional control over state education policy; and the originalist legal movement would lose its current doctrinal anchor.
% FOUNDING_PROBLEM: State-mandated racial segregation and explicit racial caste systems under Jim Crow; the need for a judicially enforceable limit on state racial classification.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era historians attest the original target was de jure segregation (now dead). Critical race theorists and civil rights practitioners attest the Clause was understood to permit remediation. Originalist jurists â beneficiaries of this reading â assert the problem is ongoing racial classification. No uncontested outside corroboration exists; the beneficiary parties frame the problem as live while excluded parties dispute the framing entirely.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint removes a remedial tool from historically excluded groups under conditions of inherited structural inequality, transferring competitive opportunity to overrepresented groups. Suppression is higher still (0.82) because the constraint persists only through active judicial enforcement striking down race-conscious policies and excluding remedial alternatives. Theater is moderate (0.35): the judicial reasoning is substantive but increasingly formalistic, with growing performance of neutrality as the gap between formal equality and substantive outcomes widens. Resistance (0.70) reflects persistent legal and political opposition from civil rights advocates and universities.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and constitutional originalists experience this constraint as genuine coordination (a clear rule preventing state racial discrimination), while underrepresented applicants and state universities experience it as extraction (loss of remedial tools and institutional autonomy). The engine computes this divergence from structural data: beneficiaries have mobile or constrained exit and low directionality; victims have trapped or constrained exit and high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Overrepresented applicants and constitutional originalists are structural beneficiaries (low d): they gain competitive position and ideological vindication without bearing costs. Underrepresented applicants and state universities are structural victims (high d): they bear the costs of lost remedial pathways and lost institutional autonomy. The federal judiciary sits near symmetric via institutional fallback â it wields authority but does not directly collect the transfer. Civil rights advocates are excluded from the framework entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by distinguishing the genuine coordination function (preventing state racial discrimination) from the extraction function (blocking remediation under inherited disadvantage). A pure rope analysis would ignore the asymmetric victim structure; a pure snare analysis would ignore the real anti-discrimination coordination. Tangled rope captures both: the same structure that prevents racial classification by oppressors also prevents racial classification by remedial state actors, with asymmetric impact determined by pre-existing social position. The founding problem (Jim Crow segregation) is dead, confirming mandatrophy via the R5 genealogy interview.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_naturalness,
    'Is the colorblind reading a discovered feature of constitutional text or a constructed interpretation that naturalizes a particular distribution of opportunity?',
    'Historical linguistics analysis of the Fourteenth Amendment''s original public meaning; comparative constitutional analysis of how other jurisdictions interpret equality guarantees.',
    'If constructed, the reading is susceptible to false-summit detection and may compute as more extractive than a natural-law reading would; if discovered, it approaches mountain status within the legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_naturalness, conceptual, 'Whether the colorblind reading is natural law-like discovery or constructed interpretation').

omega_variable(
    remedial_reading_structural_flip,
    'If the remedial reading were adopted as the dominant framework, would the constraint reclassify as scaffold (temporary remedial coordination) or rope (genuine coordination without extraction)?',
    'Comparative analysis of jurisdictions where remedial race-conscious policies carry sunset clauses versus permanent frameworks.',
    'A scaffold classification would validate the coordination-with-sunset model; permanent remedial frameworks may exhibit their own extraction dynamics through extended identity politics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_reading_structural_flip, conceptual, 'How sibling reading adoption would change constraint classification').

omega_variable(
    stratification_under_formal_equality,
    'Does formally race-neutral admissions under conditions of inherited structural inequality reproduce racial stratification or produce equitable opportunity?',
    'Longitudinal empirical studies of applicant pools and outcomes before and after race-conscious admissions bans in multiple jurisdictions.',
    'If stratification increases, the colorblind reading''s coordination story is empirically undermined and extraction is higher than authored; if outcomes equalize, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_under_formal_equality, empirical, 'Empirical effect of colorblind policies on racial stratification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_colorblind_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ep_colorblind_tr_t9, equal_protection_kernel__colorblind_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement(ep_colorblind_tr_t18, equal_protection_kernel__colorblind_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(ep_colorblind_tr_t27, equal_protection_kernel__colorblind_reading, theater_ratio, 27, 0.28).
narrative_ontology:measurement(ep_colorblind_tr_t36, equal_protection_kernel__colorblind_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(ep_colorblind_tr_t45, equal_protection_kernel__colorblind_reading, theater_ratio, 45, 0.35).

% Extraction over time
narrative_ontology:measurement(ep_colorblind_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ep_colorblind_be_t9, equal_protection_kernel__colorblind_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(ep_colorblind_be_t18, equal_protection_kernel__colorblind_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(ep_colorblind_be_t27, equal_protection_kernel__colorblind_reading, base_extractiveness, 27, 0.62).
narrative_ontology:measurement(ep_colorblind_be_t36, equal_protection_kernel__colorblind_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(ep_colorblind_be_t45, equal_protection_kernel__colorblind_reading, base_extractiveness, 45, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ep_colorblind_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ep_colorblind_su_t9, equal_protection_kernel__colorblind_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(ep_colorblind_su_t18, equal_protection_kernel__colorblind_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(ep_colorblind_su_t27, equal_protection_kernel__colorblind_reading, suppression_requirement, 27, 0.72).
narrative_ontology:measurement(ep_colorblind_su_t36, equal_protection_kernel__colorblind_reading, suppression_requirement, 36, 0.78).
narrative_ontology:measurement(ep_colorblind_su_t45, equal_protection_kernel__colorblind_reading, suppression_requirement, 45, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equal_protection_kernel, which decomposes into three structurally distinct constraints per the epsilon-invariance principle: colorblind_reading (anti-classification), remedial_reading (race-conscious remediation permitted), and antisubordination_reading (subordination-targeted). Each reading has a different epsilon, beneficiary/victim structure, and classification. The colorblind reading forecloses both siblings within a single interpretive framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
