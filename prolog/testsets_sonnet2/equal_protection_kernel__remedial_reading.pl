% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Equal Protection Clause — Remedial/Diversity Reading (Race-Conscious Admissions)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the remedial/diversity reading of the Equal
 *   Protection Clause's kernel: race-conscious state action is
 *   constitutionally permitted when narrowly tailored either to remedy
 *   documented historical exclusion or to achieve a compelling diversity
 *   interest in education. From Bakke (1978) through Grutter (2003) to the
 *   doctrine's substantial narrowing/overturn in SFFA v. Harvard (2023), this
 *   reading governed selective university admissions. The constraint is
 *   authored here as a Tangled Rope: it has a genuine coordination function
 *   (correcting for documented historical exclusion, achieving pedagogical
 *   diversity benefits) but also produces asymmetric extraction (a
 *   concentrated cost borne by specific displaced applicants) sustained by
 *   active judicial and institutional enforcement (strict scrutiny review,
 *   narrow-tailoring requirements). This is ONE of three sibling readings of
 *   the same equal-protection kernel — the colorblind reading and the
 *   antisubordination reading are separate constraint stories with their own
 *   ε values, beneficiary/victim sets, and classifications; this story does
 *   not average over them or hedge between them.
 *
 * KEY AGENTS:
 *   - historically_underrepresented_minority_applicants: Primary beneficiary (moderate/constrained) — gains admission probability under the doctrine
 *   - universities_pursuing_diversity_rationale: Agenda-setter (institutional/mobile) — designs and administers the race-conscious process, retains discretion
 *   - marginally_rejected_nonpreferred_applicants: Primary payer (moderate/constrained) — bears concentrated displacement cost
 *   - state_legislatures_and_courts: Analytical/administering seat (institutional) — sets and re-sets doctrinal boundaries
 *   - civil_rights_advocacy_organizations: Excluded voice (organized) — argues for stronger remedial readings but does not control doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.42).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.35).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial/Diversity Reading (Race-Conscious Admissions)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '5be0c264-7fb2-402e-a88f-469ce391eda2').
narrative_ontology:cs_kernel_codification('5be0c264-7fb2-402e-a88f-469ce391eda2', fixed_text).
narrative_ontology:cs_authority_grounding('5be0c264-7fb2-402e-a88f-469ce391eda2', lineage).
narrative_ontology:cs_interpretation_layer_present('5be0c264-7fb2-402e-a88f-469ce391eda2').
narrative_ontology:cs_reading_relation('5be0c264-7fb2-402e-a88f-469ce391eda2', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('5be0c264-7fb2-402e-a88f-469ce391eda2', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('5be0c264-7fb2-402e-a88f-469ce391eda2', foundational, purpose_matters_more_than_classification_form).
narrative_ontology:cs_axiom_status(purpose_matters_more_than_classification_form, holdable).
narrative_ontology:cs_axiom_grounding('5be0c264-7fb2-402e-a88f-469ce391eda2', purpose_matters_more_than_classification_form, deontological).
narrative_ontology:cs_axiom('5be0c264-7fb2-402e-a88f-469ce391eda2', secondary, compelling_diversity_interest_justifies_race_conscious_means).
narrative_ontology:cs_axiom_status(compelling_diversity_interest_justifies_race_conscious_means, overridden).
narrative_ontology:cs_axiom_grounding('5be0c264-7fb2-402e-a88f-469ce391eda2', compelling_diversity_interest_justifies_race_conscious_means, instrumental).
narrative_ontology:cs_reference_frame('5be0c264-7fb2-402e-a88f-469ce391eda2', post_reconstruction_remedial_purpose).
narrative_ontology:cs_drift_state('5be0c264-7fb2-402e-a88f-469ce391eda2', post_sffa_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5be0c264-7fb2-402e-a88f-469ce391eda2', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_underrepresented_minority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_pursuing_diversity_rationale).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, marginally_rejected_nonpreferred_applicants).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_diversity_interest_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, narrow_tailoring_remedial_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply to selective universities where race is weighed as a plus factor among many admissions considerations. Benefit from an admissions process that treats their group membership as evidence of overcome structural disadvantage, increasing admission odds relative to a strictly race-blind baseline. Their exit option from the constraint is nonexistent — they cannot opt out of being classified by the university's holistic process; their leverage is political and legal advocacy for the doctrine's survival.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_underrepresented_minority_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Design and administer holistic admissions processes, decide how heavily to weight race, and must document a compelling interest (educational benefits of diversity or remedying identified historical exclusion) with narrow tailoring to survive strict scrutiny. Retain broad discretion in practice because the standard is deferential to institutional judgment about educational mission, and can restructure processes (proxies, essays, geographic weighting) if the explicit racial classification is struck down — giving them the most exit flexibility of any seat.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_pursuing_diversity_rationale, agenda_setter,
    institutional, generational, mobile, national).

% Applicants — disproportionately Asian-American and white in the contemporary record — who would have been admitted under a strictly race-blind sorting of the same applicant pool but are displaced by the plus-factor weighting given to other applicants. Bear a concentrated, identifiable cost (loss of a specific seat) for a diffuse, institution-level benefit. Cannot exit the constraint except by applying elsewhere or foregoing the specific institution; the harm is a one-time, high-stakes event with no remedy after the decision is made.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, marginally_rejected_nonpreferred_applicants, payer,
    moderate, biographical, constrained, national).

% Adjudicate the boundaries of the doctrine — how much documentation of historical exclusion suffices, how 'narrow tailoring' is measured, whether diversity interests remain compelling. Their rulings (e.g., shifting standards across decades) determine whether the remedial reading remains viable or is displaced by a rival reading of the same clause.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_legislatures_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, state_legislatures_and_courts, agenda_setter).

% Argue for stronger remedial and antisubordination readings — that the current doctrine's narrow-tailoring requirement and documentation burden are themselves obstacles to genuine remedy. They participate in litigation as amici but do not control the doctrine's actual content, which is set by courts responding primarily to litigants on both sides of specific cases.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows universities to pursue a compelling institutional interest — either remedying documented historical exclusion of specific groups or securing the pedagogical benefits of a diverse student body — without those two goals being derailed by a rule that treats any racial classification as per se unconstitutional. Solves the problem of how the state can act to counteract effects of historical discrimination without every remedial classification being invalidated as symmetrical to the discrimination it responds to.
% TRANSFER_FUNCTION: Moves a scarce admissions seat (and the downstream credential, network, and earnings premium it confers) from an applicant who would have been admitted under a race-blind sort to an applicant from a group the institution has determined is underrepresented relative to its diversity or remedial rationale.
% ABSENT_VOICES: Rejected applicants rarely litigate individually — they lack the resources and standing infrastructure that organized plaintiffs' groups (e.g. Students for Fair Admissions) have built; the actual harmed individuals are usually represented only in aggregate statistical form in litigation, not as named parties with direct voice in doctrinal design. Historically excluded communities also have limited direct voice — the compelling-interest standard is adjudicated by courts and administered by university officials, not by affected communities themselves.
% DISAPPEARANCE_RATIONALE: If this reading were overturned (as it substantially was in the 2023 SFFA decisions), universities would face significant restructuring of admissions criteria, litigation over 'diversity by other means' proxies, likely declines in enrollment of some historically underrepresented groups at selective institutions, and a shift in institutional risk calculus toward race-neutral means of pursuing similar goals — a real rearrangement of both admissions practice and downstream demographic composition.
% FOUNDING_PROBLEM: Post-Reconstruction and post-Civil-Rights-era recognition that formally equal treatment applied to a population with unequal starting conditions (segregation, exclusion, resource deprivation) would reproduce those inequalities; the doctrine was built to let the state take group membership into account as a remedy rather than treating equal protection as satisfied by mere facial neutrality.
% FOUNDING_PROBLEM_CORROBORATION: Historically underrepresented communities and civil rights organizations attest the founding problem (durable effects of historical exclusion) remains live, citing persistent wealth, wealth-adjacent, and educational-access gaps. Colorblind-reading proponents and some social scientists attest that the specific mechanism — race-conscious admissions at selective universities — has drifted from remedying documented exclusion toward pursuing diversity as an independent institutional good untethered to any specific finding of past discrimination by the university itself, a shift acknowledged even by defenders of the doctrine (e.g., Bakke's rejection of pure remediation in favor of the diversity rationale) and by the Supreme Court's own doctrinal history moving from Bakke to Grutter to SFFA.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) is moderate: the doctrine transfers a real, identifiable good (an admissions seat) from a specific class of rejected applicants to another class, but the magnitude at the individual level is bounded (one seat, one institution, one cycle) and the aggregate volume affected by race-conscious weighting was, empirically, a minority of admissions decisions even at institutions using it. Suppression (0.35) reflects that strict scrutiny itself functions as an internal check — courts actively police against overbroad quotas — so the doctrine is more constrained than a pure racial-preference regime would be, but active enforcement (narrow-tailoring litigation, documentation requirements) is still required to keep it operating within constitutional bounds. Theater ratio rose slowly over the interval (0.15 to 0.28) reflecting increasing institutional emphasis on holistic-review documentation and 'diversity statement' formalism as a hedge against litigation, without a proportional increase in demonstrable remedial outcomes tied to documented historical exclusion specifically (the doctrine drifted toward the diversity rationale, which requires less specific historical documentation than pure remediation).
 *
 * PERSPECTIVAL GAP:
 *   From the university's seat, the arrangement is coordination: a lawful, court-sanctioned mechanism for achieving compelling institutional interests via holistic review, subject to real internal constraint. From the rejected nonpreferred applicant's seat, the same structure operates as an enforced, non-negotiable transfer of a specific opportunity, imposed without their consent and without any individualized finding that they personally benefited from the historical exclusion being remedied. The engine's per-seat computation should reflect this asymmetry: the agenda_setter and beneficiary seats likely compute closer to rope/coordination; the payer seat likely computes closer to snare-adjacent extraction, which is precisely the divergence that motivates classifying the whole arrangement as tangled_rope rather than either pure rope or pure snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically underrepresented minority applicants are beneficiaries with constrained exit — they cannot choose whether the process classifies them, but the classification works in their favor on net, so directionality sits toward the low-d end. Marginally rejected nonpreferred applicants are the structural target: a specific, identifiable displacement with no institutional recourse once the decision is made, placing them toward the high-d end despite their nominal 'moderate' power. Universities hold the actual administrative discretion and the widest exit options (they can restructure admissions criteria entirely), making them the agenda-setter seat whose interest in maintaining the doctrine is institutional and reputational rather than a direct extraction in the ordinary sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (durable effects of documented historical exclusion) is contested as either live or substantially transformed. The doctrine's own history — moving from Bakke's explicit rejection of pure quota remediation, to Grutter's diversity-interest rationale (which does not require documenting specific institutional discrimination), to SFFA's rejection of the diversity rationale altogether — is itself the record of a mandatrophy question being litigated in real time: does the arrangement still serve the founding remedial problem, or has it drifted into serving an institutional interest (educational diversity broadly conceived) that is only loosely coupled to the original historical-exclusion rationale? This story treats that drift as live and contested rather than resolved, and documents it structurally in the omega variables and the founding_problem_status field rather than pre-judging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_rationale_vs_documented_remediation,
    'Has the doctrine drifted from remedying DOCUMENTED historical exclusion (a narrower, evidence-bound justification) toward pursuing DIVERSITY as an independent institutional good that does not require any specific finding of past discriminatory exclusion by the admitting institution?',
    'Comparative doctrinal analysis of Bakke, Grutter, Fisher, and SFFA opinions; empirical audit of how many institutions using race-conscious admissions actually documented specific historical exclusion versus asserting generalized diversity benefits.',
    'If the drift is substantial, the remedial reading as originally conceived (tied to documented exclusion) has been partially supplanted by a weaker diversity-interest reading with a thinner evidentiary foundation, which would justify a higher extraction/suppression score for the diversity-rationale component specifically and could be split into its own constraint story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_rationale_vs_documented_remediation, empirical, 'Whether the remedial reading has drifted from documented-exclusion remediation toward a generalized diversity rationale.').

omega_variable(
    kernel_reading_dominance,
    'Among the three sibling readings of the equal-protection kernel (remedial, colorblind, antisubordination), which reading has effectively captured the Supreme Court''s controlling doctrine at any given time, and what determines which reading a given panel or era adopts?',
    'Track the sequence of controlling opinions (Bakke plurality, Grutter majority, SFFA majority) and the composition/reasoning shifts between them; identify whether the shift reflects genuine reconsideration of constitutional meaning or exogenous changes in court composition.',
    'If reading-dominance tracks court composition rather than principled doctrinal evolution, this substantially undermines the corroboration value of any single reading''s founding-problem status claim, since the doctrine''s content becomes contingent on personnel rather than settled interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Whether kernel-reading dominance reflects doctrinal reasoning or exogenous judicial composition changes.').

omega_variable(
    remedial_scope_ambiguity,
    'Is the beneficiary class properly defined as ''members of historically excluded groups'' (group-based) or as ''individuals who can show personal disadvantage traceable to historical exclusion'' (individual-based) — and does the remedial reading as practiced actually operate on the narrower individual basis it formally claims to require?',
    'Audit university admissions files (where available through litigation discovery, e.g. SFFA v. Harvard record) for whether race was used as a group-level proxy or individualized assessment of disadvantage was actually performed.',
    'If race functioned mostly as a group-level proxy rather than an individualized narrow-tailoring exercise, the doctrine''s actual operation was less narrowly tailored than its formal legal standard requires, which would push the extraction and suppression metrics higher than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_scope_ambiguity, empirical, 'Whether the remedial reading''s narrow-tailoring requirement was honored in practice or functioned as group-level proxy classification.').


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
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__remedial_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_kernel__remedial_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.34).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__remedial_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(equa_be_t2018, equal_protection_kernel__remedial_reading, base_extractiveness, 2018, 0.41).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.37).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.34).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__remedial_reading, suppression_requirement, 2013, 0.36).
narrative_ontology:measurement(equa_su_t2018, equal_protection_kernel__remedial_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the equal_protection_kernel, each authored as an independent constraint per the ε-invariance principle. The colorblind_reading treats any racial classification as categorically forbidden (near-mountain-like prohibition claim with its own distinct beneficiary/victim structure inverted relative to this story). The antisubordination_reading treats the clause as targeting caste-like hierarchy specifically, producing a narrower and differently-scoped beneficiary set than this remedial/diversity reading. All three share the same underlying constitutional text but instantiate structurally distinct constraints with different ε values, different stakeholders, and different classifications — they are linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
