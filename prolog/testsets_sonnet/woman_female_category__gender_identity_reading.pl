% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Woman/Female Category Membership — Gender Identity Reading
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates the gender-identity reading of the contested
 *   woman/female category kernel: category membership is determined by
 *   internal self-identification, independent of chromosomal sex,
 *   reproductive anatomy, or developmental biology. Under this reading,
 *   institutions that adopt self-identification as the operative standard for
 *   admission to female-designated spaces (shelters, prisons, sports,
 *   changing facilities) and for legal sex-marker changes extend real
 *   dignitary and administrative benefit to transgender individuals while
 *   imposing costs — access-based, safety-based, and
 *   competitive-fairness-based — on natal women who occupied those spaces
 *   under a biological-exclusion premise. This is ONE of three sibling
 *   readings of the same kernel; the sex-biology reading and the
 *   hybrid-contextual reading are separate constraint stories with their own
 *   ε, beneficiary/victim sets, and classifications. They are not alternate
 *   observables of this constraint — adopting a different reading changes who
 *   benefits and who is harmed, which is exactly the ε-invariance principle's
 *   signal that these are distinct constraints linked by kernel membership,
 *   not measurement variants of one constraint.
 *
 * KEY AGENTS:
 *   - transgender_women_seeking_recognition: Primary beneficiary (moderate/identity_locked) — gains recognition and space access under this reading
 *   - natal_women_in_single_sex_spaces: Primary payer (moderate/constrained) — loses the biological-exclusion premise the space was organized around
 *   - female_athletes_in_sex_segregated_sport: Payer (moderate/trapped) — bears competitive cost specific to sport eligibility
 *   - detained_female_prisoners: Payer (powerless/trapped) — bears the most acute, least voluntary version of the cost
 *   - gender_identity_advocacy_organizations: Agenda-setter (organized/mobile) — drives adoption of this reading through litigation and legislation
 *   - womens_rights_and_sex_based_rights_organizations: Excluded (organized/constrained) — objects to adoption but is frequently not consulted
 *   - legislators_and_courts: Observer (institutional/analytical) — adjudicates between this and sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.52).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Woman/Female Category Membership — Gender Identity Reading").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'c183ce3f-5b0a-41f9-96b9-9d93be64835a').
narrative_ontology:cs_kernel_codification('c183ce3f-5b0a-41f9-96b9-9d93be64835a', distributed).
narrative_ontology:cs_authority_grounding('c183ce3f-5b0a-41f9-96b9-9d93be64835a', distributed).
narrative_ontology:cs_reading_relation('c183ce3f-5b0a-41f9-96b9-9d93be64835a', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('c183ce3f-5b0a-41f9-96b9-9d93be64835a', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('c183ce3f-5b0a-41f9-96b9-9d93be64835a', foundational, self_identification_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('c183ce3f-5b0a-41f9-96b9-9d93be64835a', self_identification_sufficient_for_category_membership, deontological).
narrative_ontology:cs_axiom('c183ce3f-5b0a-41f9-96b9-9d93be64835a', secondary, biological_criteria_are_illegitimate_basis_for_category_exclusion).
narrative_ontology:cs_axiom_status(biological_criteria_are_illegitimate_basis_for_category_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('c183ce3f-5b0a-41f9-96b9-9d93be64835a', biological_criteria_are_illegitimate_basis_for_category_exclusion, deontological).
narrative_ontology:cs_reference_frame('c183ce3f-5b0a-41f9-96b9-9d93be64835a', self_identification_as_sufficient_criterion).
narrative_ontology:cs_drift_state('c183ce3f-5b0a-41f9-96b9-9d93be64835a', post_2020_institutional_contestation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c183ce3f-5b0a-41f9-96b9-9d93be64835a', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_men_and_nonbinary_people_seeking_recognition).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, natal_women_in_single_sex_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, female_athletes_in_sex_segregated_sport).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, detained_female_prisoners).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, domestic_violence_shelter_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition as women based on internal gender identity rather than natal sex, including access to female-designated spaces, categories, and protections. Report severe dignity and safety harms when recognition is denied; exit from the identity claim is not a live option for them, and their access to single-sex spaces depends entirely on this reading being adopted by the institution in question.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition, beneficiary,
    moderate, biographical, identity_locked, national).

% Seek recognition of gender identity independent of natal sex for legal documentation, restrooms, and social treatment. Their claim rests on the same self-identification principle; they are typically less visible in single-sex space disputes than trans women but share the underlying beneficiary structure of this reading.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_men_and_nonbinary_people_seeking_recognition, beneficiary,
    powerless, biographical, identity_locked, national).

% Use female-designated changing rooms, shelters, prisons, and sports categories on the premise that these spaces exclude males. Under this reading, anyone self-identifying as a woman qualifies for entry regardless of natal sex, which they experience as a loss of the privacy, safety, or competitive-fairness rationale the space was built on. Exit means avoiding the space entirely or accepting mixed-sex conditions; they cannot opt out of the policy change once adopted by the institution.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, natal_women_in_single_sex_spaces, payer,
    moderate, biographical, constrained, national).

% Compete in categories segregated to offset average male athletic advantage. Under this reading, eligibility is determined by identity rather than developmental biology, so athletes retaining male puberty-conferred advantage may compete in the female category. Athletes cannot individually alter federation eligibility rules; their only exit is leaving the sport.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, female_athletes_in_sex_segregated_sport, payer,
    moderate, biographical, trapped, national).

% Housed in sex-segregated custodial facilities with no control over facility assignment policy. Under this reading, trans women are housed according to self-identified gender, which incarcerated women experience as an imposed cohabitation risk they cannot decline, appeal individually, or exit.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, detained_female_prisoners, payer,
    powerless, immediate, trapped, national).

% Seek refuge from male violence in shelters premised on sex-based exclusion of men. Under this reading, admission is determined by self-identified gender, which some residents experience as reintroducing the risk category the shelter exists to exclude; they typically have no alternative shelter option in crisis.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, domestic_violence_shelter_residents, payer,
    powerless, immediate, trapped, local).

% Advocate for and draft the legal and institutional frameworks that codify self-identification as the operative standard, and pursue litigation and legislation to have institutions adopt it. They administer the campaign for this reading's adoption and can adjust strategy or scope without themselves bearing the space-access costs.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Argue that sex-based protections require a biological definition to function and that this reading's adoption erodes those protections without their consent. In many institutional and legislative processes their objections are treated as bigotry rather than a competing rights claim, and they report being excluded from consultation processes that adopted this reading.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, womens_rights_and_sex_based_rights_organizations, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes between competing readings of the category, hear evidence and testimony from all sides, and produce rulings or statutes that can adopt, reject, or hybridize this reading with the sex-biology or contextual readings.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable rule — self-identification — for determining category membership across legal documents, institutional admission, and social address, avoiding case-by-case biological verification and reducing the dignitary harm of forced disclosure or inspection.
% TRANSFER_FUNCTION: Moves access to female-designated spaces, categories, and resources (changing rooms, shelters, prisons, sports divisions, legal sex markers) from natal-sex-defined eligibility to self-identification-defined eligibility, transferring access from natal women as a class to anyone self-identifying as women, and transferring the burden of contested entry onto natal-women-only spaces.
% ABSENT_VOICES: Detained women and shelter residents affected by facility-assignment policy are rarely direct parties to the litigation or legislative processes that adopt this reading; their experience surfaces mainly through advocacy reporting after policy adoption, not through direct consultation.
% DISAPPEARANCE_RATIONALE: Transgender advocacy organizations would say the world rearranges catastrophically for trans individuals — loss of legal recognition, renewed forced disclosure, exclusion from documentation matching lived identity. Sex-based rights organizations would say the world reverts to a workable and long-standing biological standard with no loss of any genuine right. The two camps do not agree on the counterfactual, which is itself part of what the kernel contest is about.
% FOUNDING_PROBLEM: Transgender individuals faced severe social stigma, legal non-recognition, and exclusion from services and documentation matching their lived gender, producing high rates of harassment, violence, and administrative harm when treated strictly according to natal sex.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations and allied legal scholars attest the founding problem remains live and severe. Sex-based rights organizations and some detained-women and shelter advocacy groups — outside the beneficiary set — attest that the self-identification solution as currently implemented creates a new, distinct harm class (loss of sex-based safeguarding) rather than resolving the original one, and argue a contextual or biological standard would better serve both groups' legitimate interests.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) reflecting the accumulating institutional footprint of self-identification-based eligibility as more jurisdictions and organizations adopt it — a genuine coordination gain for transgender individuals paired with a real, uncompensated cost transfer onto natal-sex-defined spaces. Suppression at 0.52 reflects that adoption of this reading, once institutionalized (e.g., in prison policy or shelter admission), is not something the payer class can individually opt out of and is often enforced through anti-discrimination liability rather than negotiated consent. Theater ratio is modest (0.28) because the coordination function — reducing dignitary harm from non-recognition — is real, not merely performative, even though the metric rises somewhat as institutional compliance activity (trainings, policy statements) grows alongside substantive changes. Resistance is high (0.72) because sex-based rights organizations and affected women mount sustained public, legal, and legislative opposition to this reading's adoption — this is a live, contested constraint, not a settled one.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (transgender individuals and advocacy organizations), this reading is experienced as overdue recognition correcting an unjust exclusion — closer to rope. From the payer seats (natal women in single-sex spaces, especially those with no institutional exit such as prisoners and shelter residents), the same structural mechanism is experienced as an imposed, unconsented cost transfer — closer to snare. The engine computes these divergent seat classifications from the authored power/exit/beneficiary-victim data; this story does not adjudicate which seat is correct, only that the divergence is structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals are the structural beneficiaries: the reading directly grants what they seek (recognition, access, documentation) and their exit from the identity claim itself is not available (identity_locked), which the derivation correctly reads as high stakes rather than as mobility. Natal women, female athletes, detained women, and shelter residents are the structural payers: the reading imposes a cost transfer through the same category-membership mechanism, and their exit options range from constrained (can avoid some spaces) to trapped (prisoners, shelter residents, who cannot exit the institution at all). Advocacy organizations on the beneficiary side hold organized power and mobile exit (litigation strategy, venue-shopping, coalition-building) that individual beneficiaries and payers alike lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — severe stigma and non-recognition harming transgender individuals — remains genuinely live by most accounts, which argues against calling this arrangement a pure zombie mandate. But the founding_problem_status is authored as contested rather than simply live because sex-based rights organizations argue the self-identification solution, as currently implemented in some institutional contexts, has generated a new and distinct harm class (loss of sex-based safeguarding for vulnerable natal women) that was not part of the original founding problem and is not addressed by continuing to expand this reading's scope. Classifying this as tangled_rope rather than snare preserves the fact that it does solve a real coordination problem for transgender individuals; classifying it as tangled_rope rather than rope preserves the fact that identifiable victims bear concentrated, non-consensual costs through the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_authority,
    'Which body or process has legitimate authority to select among the gender_identity_reading, sex_biology_reading, and hybrid_contextual_reading for a given institutional context (sport, custody, shelter, legal documentation)?',
    'Track which institutions adopt which reading and through what process (legislative, judicial, administrative, or unilateral policy change) and whether affected payer classes were structurally represented in that process.',
    'If adoption consistently occurs through processes that exclude the payer class (natal women in single-sex contexts) from consultation, that strengthens the case that this reading''s institutionalization is procedurally as well as substantively contested, independent of which reading is normatively correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'Who legitimately decides which kernel reading governs a given institutional context.').

omega_variable(
    dignitary_harm_vs_safety_harm_commensurability,
    'Are the dignitary/recognition harms borne by transgender individuals under the sex-biology reading commensurable with the safety/privacy/fairness harms borne by natal women under this reading, such that one can be traded off against the other in policy design?',
    'Comparative harm studies examining actual incidence rates, severity, and reversibility of each harm class under each reading''s adoption, conducted by researchers without institutional stake in either advocacy position.',
    'If the harms are genuinely incommensurable (different in kind, not just degree), no single reading can be shown to minimize total harm, and the choice among readings becomes irreducibly a values question (preference-type) rather than resolvable by further evidence (empirical-type) — which would argue for the hybrid_contextual_reading''s context-splitting strategy over either uniform extreme.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignitary_harm_vs_safety_harm_commensurability, preference, 'Whether the two harm classes produced by opposing kernel readings can be weighed on a common scale.').

omega_variable(
    identity_lock_authenticity_ambiguity,
    'Is the identity_locked exit_options classification for transgender individuals purely a description of the irreversibility of gender identity, or does it also encode a contested claim about the ontological status of gender identity that the sex_biology_reading would dispute?',
    'Distinguish the exit_options atom (a claim about whether THIS agent can practically exit the identity claim) from the underlying ontological dispute (whether gender identity is a fact independent of biology) — the former can be authored without resolving the latter, but authors should be alert to whether classification choices smuggle in the ontological claim.',
    'If exit_options classification itself is read as adjudicating the ontological dispute, this story''s structural data would be doing normative work beyond what it claims to do, undermining the ε-invariance discipline of keeping readings structurally separate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_authenticity_ambiguity, conceptual, 'Whether the identity_locked exit designation for beneficiaries smuggles in a contested ontological premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__gender_identity_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__gender_identity_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__gender_identity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__gender_identity_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_female_category__gender_identity_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(woma_be_t8, woman_female_category__gender_identity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(woma_be_t12, woman_female_category__gender_identity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_female_category__gender_identity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_female_category__gender_identity_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(woma_su_t8, woman_female_category__gender_identity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(woma_su_t12, woman_female_category__gender_identity_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(woma_su_t16, woman_female_category__gender_identity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.1).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the woman_female_category kernel. sex_biology_reading defines membership by chromosomal/anatomical/developmental criteria and inverts the beneficiary/victim structure found here. hybrid_contextual_reading splits the criterion by institutional context (biological for medical/sports/safety, identity-based for social/legal recognition) and is expected to show a lower ε on both directions of harm than either uniform extreme, at the cost of requiring context-sensitive administration. All three stories share the same kernel_id but are authored as independent constraints with independent ε values per the ε-invariance principle — do not average or reconcile their metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
