% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender-Identity-Based Woman Category Membership
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the gender_identity_reading of the
 *   woman_category kernel: the rule that 'woman' means any person who
 *   identifies as a woman, regardless of sex assigned at birth. This reading
 *   has been adopted in varying degrees across Western legal systems (gender
 *   recognition laws, anti-discrimination guidance, sports policies,
 *   institutional self-ID policies) since roughly 2000. The constraint claims
 *   to be a coordination mechanism for inclusion (rope) but operates with
 *   substantial asymmetric extraction: natal women lose sex-based
 *   protections, female athletes lose fair competition, and women in
 *   vulnerable settings lose single-sex provisions. The engine will compute
 *   per-seat classifications from the structural data; the authored claim
 *   (tangled_rope) and metrics are independent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.65).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender-Identity-Based Woman Category Membership").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, 'efd1c4fe-6098-4369-8f87-d9a6815e0f61').
narrative_ontology:cs_kernel_codification('efd1c4fe-6098-4369-8f87-d9a6815e0f61', distributed).
narrative_ontology:cs_authority_grounding('efd1c4fe-6098-4369-8f87-d9a6815e0f61', distributed).
narrative_ontology:cs_reading_relation('efd1c4fe-6098-4369-8f87-d9a6815e0f61', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('efd1c4fe-6098-4369-8f87-d9a6815e0f61', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('efd1c4fe-6098-4369-8f87-d9a6815e0f61', foundational, gender_identity_determines_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('efd1c4fe-6098-4369-8f87-d9a6815e0f61', gender_identity_determines_category_membership, deontological).
narrative_ontology:cs_axiom('efd1c4fe-6098-4369-8f87-d9a6815e0f61', foundational, sex_based_protections_are_discriminatory).
narrative_ontology:cs_axiom_status(sex_based_protections_are_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('efd1c4fe-6098-4369-8f87-d9a6815e0f61', sex_based_protections_are_discriminatory, deontological).
narrative_ontology:cs_reference_frame('efd1c4fe-6098-4369-8f87-d9a6815e0f61', self_determination_framework).
narrative_ontology:cs_drift_state('efd1c4fe-6098-4369-8f87-d9a6815e0f61', contemporary_gender_identity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('efd1c4fe-6098-4369-8f87-d9a6815e0f61', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_women_losing_sex_based_protections).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, women_needing_single_sex_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, female_athletes).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_identity_determines_category_membership).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, sex_based_protections_are_discriminatory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to women's legal categories, single-sex spaces, sports, and protections through self-identification. Their inclusion is the constraint's stated coordination function. Exit from the gender identity claim is structurally identity-locked — the identity claim constitutes the self.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Lose sex-based legal protections, single-sex spaces, fair sports competition, and female-specific data collection when category membership shifts from biology to identity. Cannot exit the material consequences of female biology (reproduction, physical dimorphism) but can exit the political coalition defending sex-based rights at significant social cost.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_women_losing_sex_based_protections, payer,
    organized, generational, constrained, global).

% Face direct competitive displacement in female sports categories when eligibility is determined by gender identity rather than biology. Some benefit from inclusion rhetoric; most bear the cost of lost fairness. Exit from elite sport is possible but ends athletic career.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, female_athletes, beneficiary).

% Women in prisons, shelters, rape crisis centers, and intimate care settings who lose the guarantee of male-free spaces. Often economically dependent on the institution, geographically immobile, and unable to consent to or avoid mixed-sex provision. Exit is structurally trapped.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, women_needing_single_sex_spaces, payer,
    powerless, immediate, trapped, local).

% Drive the legal and policy campaign to replace sex-based category membership with gender-identity-based membership across jurisdictions. Control the framing of the constraint as pure inclusion/anti-discrimination. Can shift focus to other issues; not personally dependent on any single policy outcome.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Argue that sex-based protections are necessary for female people as a biological class and that gender-identity-based categories erase the material basis of women's oppression. Their objection is structurally excluded from the gender-identity reading's framework as bigotry. Exit from the debate means conceding the category change.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_advocates, excluded,
    organized, generational, constrained, national).

% Implement eligibility policies for female competition categories. Caught between inclusion mandates (IOC, human rights bodies) and fairness evidence. Policy changes are enforced on athletes; the bodies themselves bear reputational and legal risk but cannot exit the governance role.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% Enact and adjudicate gender-recognition laws, anti-discrimination statutes, and institutional policies that codify gender-identity-based category membership. Their authority makes the constraint legally binding. Exit would require legislative reversal or constitutional amendment.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Scholars, bioethicists, and policy analysts who study the constraint's effects across domains without direct stake in its enforcement. See the full structure of competing readings and their empirical consequences.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified category membership rule based on self-identification, enabling trans inclusion across legal, social, and institutional domains without case-by-case adjudication of authenticity or medical history.
% TRANSFER_FUNCTION: Moves access rights to women's categories (sports, single-sex spaces, legal protections, identity documents, female-specific data) from natal-women-only to all who identify as women; moves the cost of lost sex-based specificity onto natal women, female athletes, and women in vulnerable settings.
% ABSENT_VOICES: Natal women who oppose the loss of sex-based protections but are characterized as bigoted for objecting; intersex people whose specific needs are subsumed under the gender identity framework without distinct accommodation; detransitioners whose experience challenges the identity-only model; female athletes in non-elite categories with no institutional voice.
% DISAPPEARANCE_RATIONALE: The constraint actively structures access to single-sex spaces, sports categories, legal protections, and identity documents across multiple jurisdictions. Its removal would cause immediate reorganization: sports federations would revert to sex-based eligibility, prisons and shelters would restore male-free provisions, gender recognition certificates would require medical or legal sex-change processes, and equality law would re-center sex as a protected characteristic.
% FOUNDING_PROBLEM: The exclusion and misgendering of transgender people under sex-based category systems; the inability of trans women to access women's spaces, protections, and legal recognition without invasive medical gatekeeping; the denial of self-determination in gender classification.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations (Stonewall, GLAAD, NCTE), human rights bodies (UN Independent Expert on SOGI, WPATH), and major medical associations (AMA, APA) attest the founding problem is live — trans exclusion persists in many jurisdictions and the harms of misgendering and exclusion are documented. Sex-based rights advocates (Women's Declaration International, Fair Play For Women) and some feminist organizations contest this, arguing the founding problem is substantially solved by anti-discrimination law and gender recognition acts, and the arrangement now creates new harms to natal women. Legislative testimony and independent policy analysis from outside the beneficiary set support the shifted-function reading.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the high-stakes collision in sports and single-sex spaces where access rights for trans women directly displace sex-based protections for natal women — a zero-sum transfer. Moderate in identity documents (administrative convenience) but high where material resources and bodily vulnerability are at stake. Suppression (0.7) is high because the constraint's persistence depends on actively suppressing the sex-biology reading: dissenting voices are deplatformed, researchers face institutional pressure, and legal challenges are framed as hate speech. Theater ratio (0.4) is moderate: the inclusion coordination function is genuine but an increasing share of enforcement energy defends the identity-only boundary against any sex-based qualification. Accessibility collapse (0.55) is moderate: sex-based alternatives persist in some jurisdictions and domains but are collapsing in institutional policy. Resistance (0.75) is high and rising: organized opposition from women's rights groups, athlete coalitions, and some medical bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seats (gender_identity_advocates, sports_governing_bodies, legal_institutions), the constraint appears as overdue justice — a coordination innovation that finally includes a marginalized group. From the payer seats (natal_women, female_athletes, women_needing_single_sex_spaces), the same structure operates as enforced extraction — their sex-based rights are displaced without consent or compensation. The analytical_observer sees both: a genuine coordination problem (trans inclusion) solved by a mechanism that creates a new, asymmetric extraction problem (displacement of sex-based protections). The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and gender identity advocates are structural beneficiaries (d near 0.0): the constraint subsidizes their access claims and centers their self-definition. Natal women, female athletes, and women needing single-sex spaces are structural targets (d near 1.0): they bear the extraction through lost protections, fairness, and safety. Their exit options differ — natal women are constrained (organized but socially penalized for exit), female athletes are constrained (career-dependent), women in vulnerable settings are trapped (institutionally dependent). Sports governing bodies and legal institutions are agenda_setters with institutional power but constrained exit (locked into governance roles). Sex-based rights advocates are excluded (d near 1.0 but with no seat at the table). The derivation chain from beneficiary/victim declarations + exit options produces this directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trans exclusion from women's categories) remains live — trans people still face exclusion in many jurisdictions. However, the constraint has expanded beyond solving exclusion into mandating identity-only criteria that displace sex-based protections entirely. This mission creep — from 'include trans women' to 'sex is irrelevant to womanhood' — is the mandatrophy signature. The original coordination function could have been solved by targeted accommodations (e.g., trans-specific pathways) without eliminating the sex-based category. The current arrangement extracts from natal women to validate an identity claim that does not require their displacement. The constraint persists because the agenda_setters control the framing and the payers are structurally fragmented and socially penalized for resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/institutional barriers to sex-based advocacy) or internalized (self-censorship, ideological capture, identity fusion with the gender-identity framework)?',
    'Post-policy-change trajectory: if suppression of sex-based advocacy persists after legal barriers are removed (e.g., after a court ruling protecting gender-critical speech), the internalized component is confirmed. Survey experiments measuring willingness to voice dissent under anonymity vs. attribution.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — natal women carry the suppression with them into putatively free spaces. This would increase effective extraction for payer seats and strengthen the snare classification tendency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the gender-identity constraint').

omega_variable(
    coordination_extraction_boundary,
    'Is a gender-identity-only category membership rule structurally necessary for trans inclusion, or can trans inclusion be achieved through targeted accommodations that preserve sex-based protections for natal women?',
    'Natural experiment from jurisdictions that adopt gender identity recognition WITH sex-based carve-outs (e.g., UK Equality Act 2010 framework): if trans inclusion outcomes hold while sex-based protections persist, the functions are separable. Comparative policy analysis of trans wellbeing metrics across regimes.',
    'If separable, the identity-only rule is pure extraction riding on a real coordination function (tangled_rope with separable components). If inseparable, part of the measured extraction is the necessary price of the coordination itself — the constraint cannot be decomposed without losing the inclusion benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable').

omega_variable(
    kernel_reading_foreclosure,
    'Does the gender_identity_reading logically foreclose the sex_biology_reading within any single legal/policy framework, or can both readings coexist as parallel category systems (e.g., gender identity for social/legal gender, sex for sports/spaces/medicine)?',
    'Analyze jurisdictions attempting dual systems (e.g., ''gender'' for documents, ''sex'' for sports). If the gender identity reading''s logic (identity determines category) necessarily expands to colonize all domains where ''woman'' is used, foreclosure is structural. If stable dual systems exist, coexistence is possible.',
    'If foreclosure is structural, the kernel is a winner-take-all contest — the engine''s cs_foreclosure_detection will flag this. If coexistence is stable, the kernel supports a multi-reading equilibrium and the constraint family is less zero-sum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether gender_identity_reading forecloses sex_biology_reading in a single framework').

omega_variable(
    sports_fairness_empirical_uncertainty,
    'What is the magnitude of retained male performance advantage after testosterone suppression in trans women athletes, and does it constitute ''meaningful'' unfairness in female competition?',
    'Longitudinal performance studies of trans women athletes pre/post transition; meta-analysis of physiological data (muscle mass, bone density, hemoglobin, cardiorespiratory capacity) after 12+ months testosterone suppression; competition outcome analysis in categories with trans inclusion.',
    'If advantage is large and persistent, the extraction from female athletes is structurally severe and the constraint''s claimed coordination function (fair inclusion) fails its own terms — strengthening snare classification. If advantage is negligible or mitigable, the coordination function holds and extraction is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sports_fairness_empirical_uncertainty, empirical, 'Empirical uncertainty about male performance advantage retention in trans women athletes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_category__gender_identity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(woma_tr_t2005, woman_category__gender_identity_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(woma_tr_t2010, woman_category__gender_identity_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(woma_tr_t2015, woman_category__gender_identity_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(woma_tr_t2020, woman_category__gender_identity_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(woma_tr_t2025, woman_category__gender_identity_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_category__gender_identity_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(woma_be_t2005, woman_category__gender_identity_reading, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(woma_be_t2010, woman_category__gender_identity_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(woma_be_t2015, woman_category__gender_identity_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(woma_be_t2020, woman_category__gender_identity_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(woma_be_t2025, woman_category__gender_identity_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_category__gender_identity_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(woma_su_t2005, woman_category__gender_identity_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(woma_su_t2010, woman_category__gender_identity_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(woma_su_t2015, woman_category__gender_identity_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(woma_su_t2020, woman_category__gender_identity_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(woma_su_t2025, woman_category__gender_identity_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the gender_identity_reading of the woman_category kernel. It differs from sex_biology_reading in ε (0.65 vs ~0.15 for sex_biology_reading's own victims) because the extraction falls on different populations. It differs from intersex_accommodation_reading in that intersex_accommodation_reading preserves sex as a biological category while accommodating boundary cases, whereas gender_identity_reading eliminates sex as a category criterion entirely. The three readings form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__gender_identity_reading, organized, 0.85).
constraint_indexing:directionality_override(woman_category__gender_identity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
