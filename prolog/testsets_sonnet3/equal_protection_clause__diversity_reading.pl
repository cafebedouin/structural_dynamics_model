% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
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
 *   human_readable: Equal Protection Clause — Diversity Rationale for Race-Conscious Admissions
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This constraint instantiates the diversity reading of the Equal
 *   Protection Clause kernel as articulated in Bakke (1978), refined in
 *   Grutter v. Bollinger (2003), and ultimately curtailed in Students for
 *   Fair Admissions v. Harvard/UNC (2023). The reading holds that
 *   race-conscious admissions survive strict scrutiny not because they remedy
 *   historical group subordination (the remedial reading) and not despite the
 *   Constitution's demand for colorblindness (the colorblind reading), but
 *   because a racially diverse student body produces pedagogical benefits
 *   that accrue to ALL students, including the racial majority. This is a
 *   structurally distinct claim from its sibling readings: the compelling
 *   interest is defined around the institution's own educational mission, not
 *   around any group's claim to redress or any individual's claim to
 *   colorblind treatment. Under this reading, minority students' racial
 *   identity is valued instrumentally — as a contributor to the diversity
 *   good consumed by the whole student body — rather than as the basis of an
 *   independent claim to repair. ε is authored moderate (0.42) reflecting
 *   narrow-tailoring requirements (holistic, individualized review; no
 *   quotas) that genuinely constrain how much race-consciousness can operate,
 *   in contrast to a hypothetical remedial-reading constraint that would
 *   authorize more direct, targeted correction and hence a different ε
 *   profile. This is intentionally NOT the same constraint as the colorblind
 *   or remedial readings — those are separate files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - elite_universities: agenda_setter, designs and defends the diversity rationale operationally
 *   - white_and_nonminority_students: primary named beneficiary under this reading's own doctrinal logic
 *   - instrumentalized_minority_admits: payer/beneficiary, valued as means to an institutional end
 *   - high_achieving_asian_american_applicants: payer, bears the competitive cost of compositional targets
 *   - federal_judiciary: observer/adjudicator, enforces narrow tailoring and ultimately curtailed the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.42).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.38).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Clause — Diversity Rationale for Race-Conscious Admissions").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '94472a34-b6a2-4099-87de-305dbf70ab85').
narrative_ontology:cs_kernel_codification('94472a34-b6a2-4099-87de-305dbf70ab85', fixed_text).
narrative_ontology:cs_authority_grounding('94472a34-b6a2-4099-87de-305dbf70ab85', lineage).
narrative_ontology:cs_interpretation_layer_present('94472a34-b6a2-4099-87de-305dbf70ab85').
narrative_ontology:cs_reading_relation('94472a34-b6a2-4099-87de-305dbf70ab85', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('94472a34-b6a2-4099-87de-305dbf70ab85', equal_protection_clause__colorblind_reading, influences).
narrative_ontology:cs_axiom('94472a34-b6a2-4099-87de-305dbf70ab85', foundational, diversity_is_compelling_educational_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_educational_interest, overridden).
narrative_ontology:cs_axiom_grounding('94472a34-b6a2-4099-87de-305dbf70ab85', diversity_is_compelling_educational_interest, instrumental).
narrative_ontology:cs_axiom('94472a34-b6a2-4099-87de-305dbf70ab85', secondary, race_conscious_means_permissible_if_narrowly_tailored_to_institutional_benefit).
narrative_ontology:cs_axiom_status(race_conscious_means_permissible_if_narrowly_tailored_to_institutional_benefit, overridden).
narrative_ontology:cs_axiom_grounding('94472a34-b6a2-4099-87de-305dbf70ab85', race_conscious_means_permissible_if_narrowly_tailored_to_institutional_benefit, conventional).
narrative_ontology:cs_reference_frame('94472a34-b6a2-4099-87de-305dbf70ab85', bakke_powell_diversity_compromise).
narrative_ontology:cs_drift_state('94472a34-b6a2-4099-87de-305dbf70ab85', post_sffa_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('94472a34-b6a2-4099-87de-305dbf70ab85', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, white_and_nonminority_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, elite_universities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, employers_seeking_diverse_graduates).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, high_achieving_asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, instrumentalized_minority_admits).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, instrumentalized_minority_admits).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, educational_diversity_compelling_interest_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers holistic admissions processes that consider race as one factor among many, framed as serving the university's own pedagogical mission of producing a diverse learning environment. Litigates to preserve discretion, sets the narrow-tailoring boundaries in practice, and controls how much race-consciousness actually operates behind opaque holistic review.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, elite_universities, agenda_setter,
    institutional, generational, arbitrage, national).

% Attend institutions that market cross-racial exposure as part of the educational product — improved classroom discussion, cross-cultural competence, preparation for a diverse workforce. Bear little to no direct admissions cost from the policy and are named by the doctrine itself as primary beneficiaries of the diversity rationale, since the compelling interest is diversity's value TO the institution's whole student body, not remediation owed to any group.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, white_and_nonminority_students, beneficiary,
    organized, biographical, mobile, national).

% Admitted partly because their group presence supplies the educational externality the doctrine values — they are the raw material of diversity rather than the rights-holders the interest is defined around. They gain admission and its downstream benefits, but carry the stigma cost of 'diversity admit' suspicion and the doctrine does not center their own claim to redress; their inclusion is justified by what it does for others' education.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, instrumentalized_minority_admits, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, instrumentalized_minority_admits, beneficiary).

% Compete in an admissions pool where holistic review has been documented (in litigation discovery) to score personal-rating factors in ways that disadvantage this group relative to academic metrics, effectively capping representation to preserve space for the diversity mix the university wants. Exit means applying elsewhere, but the credential value is concentrated at the excluding institutions, so exit is costly rather than free.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, high_achieving_asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Receive a pre-sorted graduate pool credentialed as having navigated a diverse educational environment, useful for their own diversity commitments and client-facing representation needs, without bearing any of the admissions competition costs directly.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, employers_seeking_diverse_graduates, beneficiary,
    organized, generational, mobile, national).

% Groups arguing that the diversity rationale is a legal workaround that lets universities pursue what is functionally racial balancing without having to prove historical discrimination or defend an actual remedial claim; they are heard in litigation but the doctrine's framing (diversity as institutional pedagogical interest) structurally excludes their preferred remedial framing from being the operative legal theory.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, civil_rights_litigants, excluded,
    organized, generational, constrained, national).

% Adjudicates strict scrutiny challenges, requiring narrow tailoring and periodic reassessment of whether race-conscious means are still necessary; can uphold, narrow, or overturn the diversity rationale as a compelling interest.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows universities to assemble a student body whose racial composition supports the pedagogical goods the institution claims flow from cross-racial exposure — richer classroom discussion, reduced stereotyping, broader alumni networks — without requiring proof that any individual applicant was a victim of discrimination.
% TRANSFER_FUNCTION: Moves admissions slots at competitive margins from applicants who score highly on conventional academic metrics (concentrated among certain groups) toward applicants whose racial identity contributes to the institution's desired composition, while moving the resulting 'diverse experience' good to the entire enrolled student body, especially the racial majority.
% ABSENT_VOICES: Civil rights litigants pressing a colorblind or remedial framing are present in courtrooms but structurally excluded from the doctrine's own terms, which define the compelling interest as the institution's pedagogical benefit rather than any group's claim to redress; the instrumentalized minority admits whose presence supplies the diversity good are rarely asked whether they consent to being valued primarily for that function.
% DISAPPEARANCE_RATIONALE: If the diversity rationale disappeared, universities would need either a remedial theory (requiring proof of specific historical harm) or race-neutral proxies (class, geography, first-generation status) to pursue similar compositional goals — admissions formulas, litigation strategy, and institutional diversity offices would all restructure immediately, as they did after Students for Fair Admissions v. Harvard curtailed the doctrine.
% FOUNDING_PROBLEM: Justice Powell's Bakke opinion sought a rationale for race-conscious admissions that could survive strict scrutiny after quota systems were struck down — diversity-as-educational-benefit was the doctrinal vehicle that let race-consciousness continue without requiring universities to admit to or prove specific discriminatory histories.
% FOUNDING_PROBLEM_CORROBORATION: Universities and their amici attest the pedagogical diversity interest remains live and empirically supported by educational-benefits research. Critics including the plaintiffs in SFFA v. Harvard/UNC and dissenting economists in the discovery record attest the doctrine functions as a legal proxy for racial balancing that the remedial framework could not otherwise justify, and the Supreme Court's 2023 majority opinion itself found the interest too amorphous to survive strict scrutiny as previously applied — a rare instance of the adjudicating body corroborating the critique from outside the beneficiary set.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) sits at a moderate level because narrow-tailoring doctrine (individualized holistic review, no explicit quotas, periodic strict-scrutiny reassessment) genuinely constrains how much race-consciousness the arrangement can carry, distinguishing it from a more extractive quota-based or explicitly remedial regime. Suppression (0.38) is moderate: the doctrine does not forcibly suppress alternative admissions approaches so much as it channels universities toward diversity-framing litigation strategy because that framing survived scrutiny where others did not. Theater ratio rises over the measured interval (0.25 to 0.40) reflecting growing evidence, surfaced in SFFA discovery, that 'holistic review' operated partly as a euphemism for racial balancing rather than genuine individualized assessment — a Goodhart-style drift where the proxy (holistic diversity factors) increasingly substituted for the stated function (individualized merit assessment).
 *
 * PERSPECTIVAL GAP:
 *   From the university agenda-setter seat, this looks like Rope: a voluntarily chosen, carefully tailored institutional policy producing genuine pedagogical goods. From the instrumentalized minority admit's seat, and from the excluded civil rights litigants pressing the remedial framing, the same structure computes as extraction riding on a coordination story — their inclusion is valued for what it produces for others, not for their own claim to repair. The engine computes these divergently from the same structural data; the claimed_type here (tangled_rope) already reflects the authoring seat's judgment that both stories are simultaneously true.
 *
 * DIRECTIONALITY LOGIC:
 *   White and nonminority students are declared primary beneficiaries because the doctrine's own compelling-interest logic defines the good (diverse learning environment) as flowing to the whole student body, with the racial majority as its largest recipient class. Instrumentalized minority admits are dual-positioned: they benefit from admission and its life outcomes, but the doctrine's justification structure treats their racial identity as a means to others' educational benefit rather than centering their own claim — this dual position is the structural signature this reading's expected delta anticipated. High-achieving Asian American applicants are targets: litigation discovery evidence (personal rating score disparities) supports directionality toward the target end despite these applicants sharing 'minority' status, because the diversity-composition logic caps their representation to preserve slots for the desired mix.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (finding a scrutiny-surviving rationale for race-consciousness after Bakke struck down quotas) was contested rather than clearly live or dead by 2023: universities maintained the pedagogical-benefit research was robust, while the Supreme Court majority found the asserted interests too amorphous to satisfy strict scrutiny's measurability requirements — a genealogy question resolved, in this instance, by the adjudicating body itself rather than remaining a partisan dispute. This is precisely the kind of divergence the classification exists to surface: a coordination story (educational diversity benefits everyone) riding alongside asymmetric instrumentalization (minority admits valued as means), sustained by active enforcement (litigation-tested narrow-tailoring requirements) — the tangled_rope classification captures both the genuine coordination function and the asymmetric extraction the same structure carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_rationale_genuine_vs_pretextual,
    'Is the diversity rationale a genuine, independently defensible educational interest, or a doctrinally necessary fiction constructed to preserve race-conscious admissions after quota-based and remedial justifications were foreclosed by prior case law?',
    'Examination of whether universities pursue racial diversity specifically (versus viewpoint, socioeconomic, or geographic diversity) when race-neutral alternatives achieve similar compositional outcomes at comparable cost; SFFA discovery record on personal-rating score patterns is direct evidence.',
    'If genuine, this reading''s coordination function is real and the tangled_rope classification appropriately credits a real (if imperfect) coordination good. If pretextual, the coordination story is closer to cover for a de facto remedial or balancing policy the courts would not otherwise permit, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_rationale_genuine_vs_pretextual, conceptual, 'Whether the diversity interest is independently real or a doctrinal workaround.').

omega_variable(
    kernel_reading_selection_evidence,
    'What features of the case record and institutional practice justified authoring this reading (diversity) rather than the remedial or colorblind reading as the operative structural claim for this constraint file?',
    'The diversity reading is directly textually grounded in Powell''s controlling Bakke opinion and its adoption in Grutter — this is the doctrine as actually operative in admissions law from 1978-2023, distinguishing it from remedial theories that courts largely rejected and colorblind theories that only became controlling doctrine in 2023.',
    'Confirms this file''s ε (moderate, narrow-tailoring-constrained) and permanent time horizon are correctly reading-indexed to the diversity doctrine specifically, not blended with the remedial reading''s likely-higher ε or the colorblind reading''s likely-near-zero ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Documentation of why this reading was selected as the operative structural claim, per Rule 2''s routing requirement.').

omega_variable(
    instrumentalization_harm_measurability,
    'Is the instrumentalization of minority admits (valuing them as diversity-contributors rather than independent rights-holders) a measurable dignitary harm, or an unavoidable feature of any group-conscious policy that does not itself constitute extraction?',
    'Survey and qualitative research on minority students'' own experience of ''diversity admit'' framing and stigma; comparison with outcomes under race-neutral holistic review regimes post-SFFA.',
    'If measurable and substantial, supports the victim classification for instrumentalized_minority_admits and the tangled_rope''s asymmetric-extraction leg; if minimal, the dual beneficiary/payer role should weight further toward beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_harm_measurability, empirical, 'Whether instrumentalization constitutes a measurable harm to minority admits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__diversity_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.32).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__diversity_reading, theater_ratio, 2013, 0.36).
narrative_ontology:measurement(equa_tr_t2019, equal_protection_clause__diversity_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__diversity_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.36).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__diversity_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(equa_be_t2019, equal_protection_clause__diversity_reading, base_extractiveness, 2019, 0.41).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__diversity_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.34).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__diversity_reading, suppression_requirement, 2013, 0.36).
narrative_ontology:measurement(equa_su_t2019, equal_protection_clause__diversity_reading, suppression_requirement, 2019, 0.37).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the colloquial label 'equal protection and race-conscious admissions' per the ε-invariance principle. The colorblind_reading (near-zero ε, mountain-adjacent claim that all racial classification is per se suspect) and remedial_reading (higher ε, group-subordination-remediation claim with different beneficiary/victim structure and an expected sunset-oriented time horizon) are separate files. This file's diversity_reading sits structurally between them: moderate ε reflecting narrow-tailoring constraints, permanent (non-sunset) time horizon reflecting the doctrine's claim to ongoing pedagogical value rather than remedial completion. The 2023 SFFA decision is modeled here as the moment this reading's founding problem became contested/superseded in favor of the colorblind reading gaining controlling authority — an inter-reading dynamic best captured by linking the files rather than blending their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
