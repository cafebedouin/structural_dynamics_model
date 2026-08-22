% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Biology Determination of Female Category Membership
 *   domain: political philosophy/bioethics/gender studies/law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'woman/female
 *   category': the sex-biology reading, under which category membership is
 *   determined by chromosomal sex, reproductive anatomy, and developmental
 *   biology. The standing arrangement under contest — the referent of epsilon
 *   — is the existing regime that keys access to female-designated provisions
 *   (refuges, custodial placement, sporting categories, medical cohorts) to
 *   biological sex. Per the epsilon-invariance principle, the colloquial
 *   label 'what makes someone a woman' decomposes into three structurally
 *   distinct constraints sharing this kernel: this reading (victims: trans
 *   women excluded, intersex/DSD individuals misclassified; beneficiaries:
 *   natal females), the gender_identity_reading (which inverts the victim
 *   set), and the hybrid_contextual_reading (which fragments the victim set
 *   by context). All three assess the SAME referent with reading-indexed
 *   values: this reading authors epsilon at 0.44 because it weights the
 *   protective delivery heavily and the exclusion costs moderately; the
 *   gender_identity_reading authors the same referent far higher because it
 *   counts identity-denial as the central harm; the hybrid reading splits
 *   epsilon across contexts. The values diverge because the readings weigh
 *   harms differently, not because the referent differs. The claim/metric gap
 *   is deliberate: the reading CLAIMS tangled_rope (genuine coordination it
 *   defends as necessary) while the metrics describe real, actively enforced
 *   exclusion costs — the engine measures the divergence per seat.
 *
 * KEY AGENTS:
 *   - natal_females_seeking_sex_based_protections: Primary beneficiary (organized/constrained) — the protected class the criterion subsidizes
 *   - female_prisoners_and_shelter_residents: Captive beneficiary (powerless/trapped) — highest-stakes protection seat, no exit from placement regimes
 *   - womens_category_athletes: Beneficiary (organized/constrained, global scope) — category-integrity seat bearing occasional verification burdens
 *   - trans_women_excluded_from_female_provisions: Primary target (moderate/trapped) — bears the exclusion costs the criterion imposes
 *   - intersex_dsd_individuals_subject_to_testing: Secondary target (powerless/trapped) — bears the bright-line rule's misclassification and testing costs
 *   - sports_governing_bodies: Agenda setter (institutional/arbitrage) — writes and rewrites eligibility criteria
 *   - prison_and_shelter_administrators: Agenda setter (institutional/constrained) — applies the criterion to placement decisions daily
 *   - legislators_and_courts: Agenda setter (institutional/mobile) — sets and adjudicates the statutory definition
 *   - academic_bioethicists_and_jurists: Analytical observer (analytical/analytical) — sees the full structure without administrative power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.44).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.48).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Biology Determination of Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political philosophy/bioethics/gender studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '86db872f-d808-49d8-84ae-51676c8e0190').
narrative_ontology:cs_kernel_codification('86db872f-d808-49d8-84ae-51676c8e0190', distributed).
narrative_ontology:cs_authority_grounding('86db872f-d808-49d8-84ae-51676c8e0190', distributed).
narrative_ontology:cs_reading_relation('86db872f-d808-49d8-84ae-51676c8e0190', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('86db872f-d808-49d8-84ae-51676c8e0190', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('86db872f-d808-49d8-84ae-51676c8e0190', foundational, gametic_biology_determines_female_membership).
narrative_ontology:cs_axiom_status(gametic_biology_determines_female_membership, holdable).
narrative_ontology:cs_axiom_grounding('86db872f-d808-49d8-84ae-51676c8e0190', gametic_biology_determines_female_membership, empirically_contingent).
narrative_ontology:cs_axiom('86db872f-d808-49d8-84ae-51676c8e0190', secondary, sex_based_protection_requires_biologically_stable_boundary).
narrative_ontology:cs_axiom_status(sex_based_protection_requires_biologically_stable_boundary, holdable).
narrative_ontology:cs_axiom_grounding('86db872f-d808-49d8-84ae-51676c8e0190', sex_based_protection_requires_biologically_stable_boundary, instrumental).
narrative_ontology:cs_reference_frame('86db872f-d808-49d8-84ae-51676c8e0190', natural_kind_biological_binary).
narrative_ontology:cs_drift_state('86db872f-d808-49d8-84ae-51676c8e0190', contemporary_self_identification_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('86db872f-d808-49d8-84ae-51676c8e0190', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, womens_category_athletes).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, female_prisoners_and_shelter_residents).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_excluded_from_female_provisions).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_dsd_individuals_subject_to_testing).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, human_sexual_dimorphism_doctrine).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, gamete_based_binary_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on biologically-keyed eligibility for refuges, custodial placement, sporting categories, and medical cohorts. They cannot individually exit the categorization regime — their protection exists only while the criterion holds — and they organize legally and politically to defend it. What flows to them is protected access; what they bear is the defense burden of keeping the criterion in place.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, national).

% Are housed, searched, and accommodated under sex-based placement rules inside institutions they cannot leave. They hold the highest-stakes dependence on the criterion: their day-to-day physical environment is set by whichever membership rule their institution applies, and they have no market or geographic exit from custody or crisis accommodation.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, female_prisoners_and_shelter_residents, beneficiary,
    powerless, immediate, trapped, national).

% Compete in a female category whose eligibility is biologically determined. Their stake is competitive integrity and the record structure of their sport; they occasionally bear verification burdens when eligibility is challenged. Their career horizon is bounded, and their recourse is their federation's rules rather than exit from competitive sport.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, womens_category_athletes, beneficiary,
    organized, biographical, constrained, global).

% Are denied access to female-designated spaces, categories, and services under the biological criterion. They hold legal-advocacy capacity but limited structural power against the institutions that set placement and eligibility rules. Exit from the constraint's effects is unavailable: the categorization is imposed by statute and institutional policy wherever they live, and their stake in the question is bound up with identity they cannot set down.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_excluded_from_female_provisions, payer,
    moderate, biographical, trapped, national).

% Have bodies that do not cleanly match the binary the criterion presupposes. They bear eligibility testing, disqualification, documentation disputes, and medical scrutiny — the bright-line rule's misclassification costs concentrate on them precisely where biology is supposed to be most determinate. Sporting governance is international, so they face the rule wherever they compete.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, intersex_dsd_individuals_subject_to_testing, payer,
    powerless, biographical, trapped, global).

% Write and rewrite eligibility rules for the female category. They have repeatedly re-specified the criterion — from mass chromosome screening, to its abolition, to targeted testosterone and DSD frameworks — demonstrating freedom to move between criteria regimes that no participant seat possesses. Whatever rule they set, they administer it globally.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Apply the membership criterion to placement decisions daily. Unlike federations, they cannot redefine membership — they must operate whatever definition statute and case law supply, and they face litigation from contending parties whichever way they decide individual cases. Their discretion is operational, not definitional.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, prison_and_shelter_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Enact and adjudicate the statutory definition of the category. They can rewrite the rule — some jurisdictions have moved toward self-ID, others have hardened biological definitions — but each move carries electoral, constitutional, and cross-jurisdictional cost. They are the only seat that can replace the criterion outright.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legislators_and_courts, agenda_setter,
    institutional, generational, mobile, national).

% Analyze the structure of the category, publish competing frameworks, and supply the conceptual vocabulary all contending parties use. They hold no administrative power over placement or eligibility and collect nothing from the criterion's operation; their seat is the analytical vantage from which the full three-reading structure is visible.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, academic_bioethicists_and_jurists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single administrable criterion — chromosomal sex, reproductive anatomy, developmental biology — by which shelters, prisons, sports federations, and medical research programs allocate female-designated provisions, avoiding case-by-case adjudication of membership claims.
% TRANSFER_FUNCTION: Moves access to female-designated spaces, competition categories, and data cohorts toward natal females; moves the costs of exclusion and verification onto trans women and intersex/DSD individuals whose bodies or identities do not match the criterion.
% ABSENT_VOICES: Trans women were absent from the administrative rooms where most facility-placement and service policies were first drafted; intersex individuals were absent from the standard-setting that produced sex-verification protocols later applied to them without consent; gender-nonconforming cis women who bear collateral verification burdens rarely hold a seat in eligibility proceedings that concern them.
% DISAPPEARANCE_RATIONALE: If the biological determinant vanished overnight, every institution keying provisions to it would need a replacement — self-ID, context-indexed rules, or de-segregation — simultaneously rearranging custodial placement, shelter admission, elite sport eligibility, and decades of sex-stratified medical data collection. Nothing about the current arrangement survives the criterion's removal unchanged.
% FOUNDING_PROBLEM: Male violence against women and the demand for protected single-sex provision; preserving meaningful female competition categories given average physiological performance differences; and collecting valid sex-stratified medical evidence. The category boundary was built to serve these problems.
% FOUNDING_PROBLEM_CORROBORATION: Criminal-justice statistics on sexual and domestic violence recorded by agencies outside the beneficiary coalition, sports-physiology literature on performance gaps, and epidemiological findings on sex-differentiated disease all attest the underlying hazards independently of the arrangement's defenders. What remains disputed — and what the sibling readings contest — is whether the biological determinant is the necessary response to those hazards; the corroboration covers the founding problem, not the reading's answer to it.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).
:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.44: the reading's own lights acknowledge real exclusion costs borne by trans women and testing costs borne by intersex/DSD individuals, while holding the protective delivery dominant; the value is reading-indexed over the fixed referent, not tuned to any predicted output. Suppression 0.48: enforcement is structural, not internalized — statutes, facility placement rules, and eligibility protocols actively police the boundary, but alternatives short of the foreclosed one remain (unisex provision, male-category access, third-space proposals), so accessibility_collapse is low at 0.35. Theater_ratio 0.35: a growing share of activity is culture-war symbolism (declarations, pledges, symbolic bans) while core operations (placement, eligibility, cohort assembly) remain functional. Resistance 0.70: the constraint meets heavy, organized, two-sided contest — litigation, legislation, protest — among the highest-resistance profiles in the corpus. Measurements run on ONE shared seven-point grid (interval roughly 1995-2025). The suppression_requirement series is deliberately non-monotonic: mass chromosome screening in elite sport (high enforcement infrastructure) was dismantled around t=10, then enforcement rebuilt in new form as targeted DSD regulations and facility-placement litigation — this is an enforcement-machinery substitution, not decay, which is why the scalar suppression ends slightly above its trough. Extractiveness rises as the excluded class became mobilized and the exclusion became salient, plateauing at the current litigation equilibrium. Theater rises monotonically with the symbolization of the dispute.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats must compute differently. Trans women (trapped, moderate power, national scope) sit near the full-target end: the constraint forecloses their access and they cannot exit the categorization regime that imposes it. Intersex/DSD individuals (trapped, powerless, global scope via sport) bear the rule's failure at its own edge cases. The beneficiary seats sit near the subsidy end — with an important intra-class gradient: female prisoners (powerless, trapped, immediate horizon) hold the highest-stakes dependence on the criterion, while elite athletes (global scope, biographical horizon) hold a weaker, integrity-based stake. Agenda setters experience administration, not extraction: sports federations (arbitrage-grade freedom — they have repeatedly re-specified criteria) face the constraint differently from prison administrators (constrained — they must apply whatever definition the law supplies). Adherents of this reading additionally exhibit ideological identity fusion — the biological frame functions as a worldview making concession to context-relativity feel like betrayal — which inflates the resistance the constraint meets and would change the picture if the identity frame broke.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed: the derivation chain captures the relationships from the declared structure. Beneficiary declarations (three natal-female seats) drive d toward the beneficiary end, damped further by organized power; victim declarations with trapped exit drive d toward the full-target end, and national-to-global spatial scope scales effective extraction upward modestly for the targets — verification is hardest exactly where the excluded class is weakest. The vindicated propositions (sexual dimorphism doctrine, gamete-binary sufficiency) collect no rents and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against rope: declaring victims and active enforcement blocks pure-coordination certification despite the genuine coordination function — the same structure that delivers protection extracts exclusion, which is the tangled_rope signature. Against snare: the founding problem is live and externally corroborated (violence statistics, performance physiology, sex-differentiated epidemiology attested by agencies outside the beneficiary coalition), so the coordination story is not mere cover. Mandatrophy is NOT resolved — the mandate has not outlived its function. The lifecycle risk to watch is the theater trajectory (0.12 to 0.35): if symbolic maintenance continues displacing operational function while the rule persists, piton drift becomes the long-run hazard; the current plateau in extractiveness suggests the system has reached a contested equilibrium rather than continuing accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (sex_biology_reading) of the shared kernel woman_female_category; what structurally changes if a sibling reading is adopted instead?',
    'Track which determinant each jurisdiction''s statutes, facility policies, and federation rules actually adopt; the adopted reading determines the operative victim and beneficiary sets.',
    'Under gender_identity_reading the victim set inverts (natal females lose guaranteed access; trans women gain it); under hybrid_contextual_reading the victim set fragments by context, with extraction concentrated wherever the context assigns the biological test. This story''s epsilon, victims, and beneficiaries are valid only for the sex-biology instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the woman/female category kernel.').

omega_variable(
    exclusion_harm_weighting,
    'Is denial of access to female-designated provisions a welfare harm of the kind this counting framework weighs — and how heavily — relative to the physical-safety and category-integrity interests the reading prioritizes?',
    'Not resolvable by data alone: it depends on whether gender identity constitutes a welfare-relevant interest comparable to bodily-safety interests. Comparative outcome studies can bound the physical-safety side; the identity-affirmation side is a framing commitment.',
    'This is the precise location of disagreement between the sibling readings. Weighting exclusion costs heavily pushes effective extraction toward snare levels; discounting them keeps the constraint in tangled_rope territory. The epsilon authored here reflects this reading''s own weighting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusion_harm_weighting, conceptual, 'Where the readings disagree: the normative weight of identity-affirmation harm versus protection harm.').

omega_variable(
    intersex_edge_case_load,
    'How much of the measured extraction is generated by the bright-line rule''s treatment of intersex/DSD individuals whose biology does not cleanly match the binary the rule presupposes?',
    'Epidemiological and administrative counts of classification disputes: sports eligibility cases, facility-placement challenges, documentation contests involving DSD conditions.',
    'A heavy edge-case load strains the reading on its own terms — the criterion fails precisely where it claims biological determinacy is strongest — raising epsilon and pressuring the foundational axiom; a light load supports the criterion''s administrability claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_edge_case_load, empirical, 'Extraction contribution of binary-rule misclassification at biological edge cases.').

omega_variable(
    protective_efficacy_uncertainty,
    'Does biologically-keyed single-sex provision actually deliver the safety and category-integrity outcomes that justify its exclusion costs?',
    'Comparative outcome studies across jurisdictions operating different determinants: assault incident rates in custodial and shelter settings, competitive-fairness measures in female categories, quality of sex-stratified medical evidence.',
    'If protective efficacy is low, the coordination half of the constraint weakens and the structure slides toward snare; if high, the rope-side reading strengthens. Current comparative data is thin and politicized, leaving this open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_efficacy_uncertainty, empirical, 'Whether the coordination function delivers the outcomes that offset exclusion costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfcat_sbr_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(wfcat_sbr_tr_t5, woman_female_category__sex_biology_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(wfcat_sbr_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(wfcat_sbr_tr_t15, woman_female_category__sex_biology_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(wfcat_sbr_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(wfcat_sbr_tr_t25, woman_female_category__sex_biology_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement(wfcat_sbr_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(wfcat_sbr_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(wfcat_sbr_be_t5, woman_female_category__sex_biology_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(wfcat_sbr_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(wfcat_sbr_be_t15, woman_female_category__sex_biology_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(wfcat_sbr_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(wfcat_sbr_be_t25, woman_female_category__sex_biology_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(wfcat_sbr_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(wfcat_sbr_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(wfcat_sbr_su_t5, woman_female_category__sex_biology_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(wfcat_sbr_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(wfcat_sbr_su_t15, woman_female_category__sex_biology_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(wfcat_sbr_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(wfcat_sbr_su_t25, woman_female_category__sex_biology_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement(wfcat_sbr_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'who counts as female/woman' decomposes into three stories sharing the kernel woman_female_category. This story (sex_biology_reading) is the upstream member in most jurisdictions' current practice — its criterion is cited as the settled baseline that the gender_identity_reading attacks and the hybrid_contextual_reading partially incorporates. Each member has its own epsilon over the shared referent: this reading 0.44 (protective weighting), the identity reading substantially higher (identity-harm weighting), the hybrid reading fragmented by context. Family members are linked via affects_constraints; orphaning any one would break contamination-propagation analysis across the definitional contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
