% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Membership by Reproductive Biology (Biology Reading)
 *   domain: social_ontology/legal_classification/identity_politics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested sex_gender_category
 *   kernel: category membership is fixed by chromosomal/anatomical sex at
 *   birth, immutable regardless of subsequent medical transition or
 *   self-identification. Under this reading, single-sex spaces, competitive
 *   sport categories, and legal sex records use birth biology as the sole
 *   determinant. The coordination function (legible boundary for safeguarding
 *   and fairness contexts) is real and cited by a broad base of advocates and
 *   institutions; the extraction is that this rigid criterion excludes trans
 *   women from 'woman' category membership in every context (not only
 *   physiology-sensitive ones), assigns trans men to a category inconsistent
 *   with their identity and presentation, and forces a binary determination
 *   onto intersex bodies that do not naturally supply one — often via
 *   non-consensual infant intervention. Enforcement has intensified over the
 *   measured interval as legal and sporting disputes have proliferated and
 *   verification infrastructure (genetic testing, birth-registry litigation)
 *   has hardened. This is a sibling of hybrid_reading and identity_reading —
 *   see kernel_context.
 *
 * KEY AGENTS:
 *   - cis_women_sex_based_protection_advocates: organized beneficiary group whose safeguarding/fairness rationale anchors the reading
 *   - womens_sports_governing_bodies and single_sex_space_administrators: institutional agenda-setters who administer and enforce the birth-sex criterion
 *   - trans_women, trans_men, intersex_individuals: powerless, identity-locked or trapped payers who bear the classification's exclusionary costs
 *   - medical_and_legal_verification_bodies: institutional adjudicators of contested edge cases
 *   - identity_reading_advocates: excluded from shaping the rule wherever this reading is administratively entrenched
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.71).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Membership by Reproductive Biology (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/legal_classification/identity_politics").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '957fda73-44d8-4c56-8bed-e8a4caae8b22').
narrative_ontology:cs_kernel_codification('957fda73-44d8-4c56-8bed-e8a4caae8b22', distributed).
narrative_ontology:cs_authority_grounding('957fda73-44d8-4c56-8bed-e8a4caae8b22', distributed).
narrative_ontology:cs_reading_relation('957fda73-44d8-4c56-8bed-e8a4caae8b22', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('957fda73-44d8-4c56-8bed-e8a4caae8b22', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('957fda73-44d8-4c56-8bed-e8a4caae8b22', foundational, birth_anatomy_is_dispositive_and_immutable).
narrative_ontology:cs_axiom_status(birth_anatomy_is_dispositive_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('957fda73-44d8-4c56-8bed-e8a4caae8b22', birth_anatomy_is_dispositive_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('957fda73-44d8-4c56-8bed-e8a4caae8b22', foundational, self_identification_cannot_alter_category_membership).
narrative_ontology:cs_axiom_status(self_identification_cannot_alter_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('957fda73-44d8-4c56-8bed-e8a4caae8b22', self_identification_cannot_alter_category_membership, deontological).
narrative_ontology:cs_reference_frame('957fda73-44d8-4c56-8bed-e8a4caae8b22', birth_registered_binary_sex).
narrative_ontology:cs_drift_state('957fda73-44d8-4c56-8bed-e8a4caae8b22', contemporary_legal_sports_disputes_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('957fda73-44d8-4c56-8bed-e8a4caae8b22', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_sex_based_protection_advocates).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, single_sex_space_administrators).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, womens_sports_governing_bodies).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_dimorphism_is_biologically_real).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, reproductive_biology_grounds_material_sex_based_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize politically and legally to keep category membership tied to birth sex, arguing that single-sex spaces, sports categories, and safeguarding protections exist to address risks and disparities rooted in reproductive biology and average sexed physical difference. They can exit particular disputes (litigation, advocacy campaigns) without losing their category membership itself, which is never contested.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_sex_based_protection_advocates, beneficiary,
    organized, generational, mobile, national).

% Prisons, domestic violence shelters, changing facilities, and hospital wards that administer eligibility criteria for sex-segregated spaces. They set and enforce entry rules based on birth-registered sex or anatomy, absorbing legal and reputational risk either way, and can adjust policy language faster than the people it is applied to can adjust their lives.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, single_sex_space_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Set eligibility rules for competitive categories, citing average physiological differences from puberty onward as the basis for a protected female category. They administer testing and verification processes, and benefit from a stable, legible category that underwrites the existence of women's competitive sport as a funded, sponsored tier.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, womens_sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, womens_sports_governing_bodies, beneficiary).

% Have transitioned or are transitioning gender presentation and often hormonal profile, but are categorically excluded from the 'woman' classification under this reading because chromosomal and birth-anatomical facts are treated as fixed and dispositive. Exit means either detransition (identity-incompatible) or living permanently outside the category they identify with, with attendant exclusion from single-sex spaces, sports, and legal recognition. Bears the constraint's core cost: their identity claim is structurally unrecognizable under this reading regardless of medical status.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Retain legal and administrative classification as female under birth-anatomy criteria despite male gender identity and presentation, which can force inclusion in women's spaces, sports, or records against their identity and sometimes against their safety, and denies them male category membership regardless of transition status.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men, payer,
    powerless, biographical, identity_locked, national).

% Are born with sex characteristics that do not fit typical binary chromosomal or anatomical patterns. A biology-only reading requires administrators to force a binary determination onto bodies that do not naturally supply one, often via infant surgical intervention, chromosomal testing, or arbitrary administrative assignment — bearing costs the framework's premise (clean biological binary) does not acknowledge exists.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Courts, birth registries, and sports federations that must adjudicate contested cases — genetic testing panels, birth certificate disputes, chromosomal verification in athletics. They administer the enforcement machinery that gives the biology reading practical force and bear the reputational and legal cost of edge-case rulings.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_and_legal_verification_bodies, agenda_setter,
    institutional, biographical, arbitrage, national).

% Argue self-identification should determine category membership and are structurally locked out of shaping eligibility rules wherever the biology reading is administratively entrenched (sports federations, some legal jurisdictions, some single-sex services) — their framework is treated as inadmissible input to the boundary-setting process in those venues, even though they are vocal participants in the broader public debate.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, identity_reading_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, verifiable, low-ambiguity criterion for allocating access to sex-segregated resources (competitive sport tiers, shelters, prisons, medical services) where administrators need a bright-line rule and where average population-level physiological differences are argued to create real safety, fairness, or privacy interests for the group defined by birth sex.
% TRANSFER_FUNCTION: Moves recognition, access, and legal standing within the 'woman' or 'man' category toward those whose birth-registered chromosomal/anatomical sex matches their claimed category, and away from trans and intersex people whose identity or bodily reality does not fit the binary birth-sex criterion — regardless of medical transition status or lived gender presentation.
% ABSENT_VOICES: Trans women, trans men, and intersex individuals are the parties most structurally affected by where the line is drawn, yet in venues where the biology reading is administratively entrenched (sports federation rulebooks, certain legal statutes), their competing framework for category membership is not treated as an admissible input — it is litigated against the standing rule rather than negotiated as a co-equal claim.
% DISAPPEARANCE_RATIONALE: Advocates for the biology reading argue that abandoning birth-sex criteria would dissolve the legal and administrative basis for single-sex spaces and competitive categories, rearranging safeguarding and sports policy substantially. Advocates for the identity reading argue the sex-segregated infrastructure would persist under different eligibility criteria with no operational collapse. Because the disagreement is about what would happen, not merely about values, this remains genuinely contested rather than resolvable from either side's premises alone.
% FOUNDING_PROBLEM: Sex-segregated spaces, records, and competitive categories were established to address real or perceived disparities and interests tied to reproductive biology and average sexed physical difference (privacy, safety from sex-based violence, fairness in physical competition), at a time when no significant population sought category membership on grounds other than birth sex.
% FOUNDING_PROBLEM_CORROBORATION: Sports scientists and some legal scholars outside directly benefiting advocacy groups attest that average post-pubertal physiological differences relevant to specific competitive disciplines are empirically real and persist regardless of social transition, corroborating a live founding problem for that narrow domain. Independent human rights bodies and medical ethics scholars outside the advocacy coalitions supporting either reading attest that the same rigid criterion, applied uniformly across shelters, prisons, and legal recognition rather than narrowly to physiology-sensitive contexts, extends well past the originally cited safety/fairness rationale into domains where the founding problem does not clearly apply — supporting a 'contested' rather than uniformly 'live' or 'dead' status.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, contested).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that the coordination function (a legible criterion for a narrow set of physiology-sensitive contexts) is real but the criterion is applied far beyond that narrow domain — to legal recognition, general-purpose single-sex spaces, and record-keeping where the cited safety/fairness rationale does not clearly transfer. Suppression (0.71) is high because enforcement increasingly depends on litigation, chromosomal verification regimes, and administrative gatekeeping to hold the line against both identity-reading advocacy and lived gender presentation that does not match the assigned category. Accessibility collapse (0.62) reflects that once the birth-sex criterion is administratively entrenched, alternative criteria become very difficult for excluded parties to access through ordinary channels (requires litigation or legislative change). Resistance (0.74) reflects substantial organized pushback from trans advocacy, some medical and legal scholarship, and international human rights bodies. Theater ratio (0.28, rising) captures growing symbolic/procedural verification activity (testing panels, compliance audits) relative to the narrower set of contexts where the underlying physiological rationale is actually load-bearing.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats, this reads as protective coordination: a stable criterion safeguarding privacy, safety, and fair competition for a group defined by birth sex. From the trans and intersex payer seats, the identical structure operates as categorical exclusion enforced by testing regimes and litigation, with the safeguarding rationale extended well past the contexts where it was originally argued to apply. The engine computes these as different seat-level classifications from the same structural data — the divergence is not an error, it is what the story is measuring.
 *
 * DIRECTIONALITY LOGIC:
 *   cis_women_sex_based_protection_advocates and the institutional agenda-setters sit near the beneficiary end: the criterion is stable, legible, and administratively convenient for them, and it does not threaten their own category membership. trans_women, trans_men, and intersex_individuals sit near the full-target end: the constraint's entire operative content is directed at determining their status, and none of them can exit the classification system without exiting their own identity or bodily reality — hence identity_locked/trapped exit options rather than mobile ones. Verification bodies are structurally load-bearing administrators rather than beneficiaries or victims in the direct sense; they carry institutional power but are not personally advantaged by any given ruling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (safety/fairness/privacy interests tied to average sexed physical difference in specific, mostly physiology-sensitive contexts) remains partly live — sports science corroboration for competitive-fairness contexts is real and comes from parties outside direct advocacy. But applying the same rigid criterion to contexts without a clear physiological stake (general legal recognition, non-competitive single-sex services) extends the founding rationale past where independent corroboration supports it. This is why founding_problem_status is authored as 'contested' rather than 'live' or 'dead' — the mismatch is domain-specific, not uniform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_binary_naturalness_vs_construction,
    'Is the treatment of sex as a clean binary determined solely by birth chromosomes/anatomy a description of an underlying natural fact, or a constructed simplification that the classification system imposes onto a more continuous/variable biological reality (as evidenced by intersex variation)?',
    'Assess whether the empirical prevalence and clinical reality of intersex variation is better modeled as noise around a true binary or as evidence that biological sex is not cleanly bimodal at the population level; also assess whether the specific criterion chosen (chromosomes vs. gonads vs. hormones vs. genitalia at birth) is itself an administratively convenient but non-unique choice among several biologically real markers.',
    'If sex is better modeled as non-bimodal, the biology reading''s claim to be a direct transcription of natural fact (rather than an administrative choice among several coherent criteria) weakens, and the constraint''s beneficiary/victim structure becomes harder to justify as naturally emergent rather than constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_binary_naturalness_vs_construction, conceptual, 'Whether treating sex as a strict binary is a natural fact or a constructed simplification.').

omega_variable(
    sibling_reading_which_framing_governs,
    'This kernel has three declared readings (biology, hybrid, identity) that a single natural-language debate (''what determines womanhood'') conflates. Which reading governs in a given administrative venue is itself contested, and the answer changes who counts as victim and who counts as beneficiary.',
    'Track which reading is operative in which jurisdiction/institution over time (sports federations vs. national legal systems vs. individual employers) and whether venues are converging toward one reading or remaining split.',
    'If most venues converge toward hybrid_reading or identity_reading, this biology_reading constraint''s real-world scope narrows even though its internal ε remains stable; if venues remain split, the three readings persist as parallel, competing constraint structures indefinitely with no dominant resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_which_framing_governs, conceptual, 'Which of the three sibling readings is administratively operative, and where.').

omega_variable(
    scope_of_founding_rationale,
    'Does the founding safeguarding/fairness rationale for birth-sex criteria genuinely extend to all administrative contexts (legal recognition, general single-sex facilities) or only to a narrow set of physiology-sensitive contexts (elite competitive sport, some medical contexts)?',
    'Compare outcome data and expert corroboration across domains: is there independent, non-advocacy evidence of a safety or fairness interest specific to each domain where the criterion is applied, or does the rationale only clearly hold in a subset?',
    'If the rationale holds only narrowly, extending the biology criterion universally is extraction beyond the coordination function, supporting reclassification toward snare in broad-application contexts even while a narrower tangled_rope or rope classification might hold for the physiology-sensitive subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_founding_rationale, empirical, 'Whether the safeguarding/fairness rationale extends beyond narrow physiology-sensitive contexts to all applications of the criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__biology_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__biology_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__biology_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(sex__tr_t32, sex_gender_category__biology_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__biology_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__biology_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__biology_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__biology_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(sex__be_t32, sex_gender_category__biology_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__biology_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__biology_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__biology_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__biology_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(sex__su_t32, sex_gender_category__biology_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__biology_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.1).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the sex_gender_category kernel, each instantiating a structurally distinct constraint with its own ε and beneficiary/victim structure: biology_reading (this story — birth chromosomes/anatomy determine membership; trans people categorically excluded regardless of transition), hybrid_reading (biology plus medical-transition status determines membership; narrower, transition-status-conditioned victim set), and identity_reading (self-identification alone determines membership; cis women's sex-based safeguarding claims become the contested victim set instead). The three are linked via affects_constraints rather than merged, per the ε-invariance principle — measuring 'who counts as a woman' three different ways yields three different ε values, hence three constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
