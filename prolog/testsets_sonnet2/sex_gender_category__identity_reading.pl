% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Self-Identification Standard for Sex/Gender Category Membership
 *   domain: social ontology / identity politics / legal classification
 *
 * SUMMARY:
 *   This story instantiates the identity_reading of the contested
 *   sex_gender_category kernel: category membership (for legal recognition,
 *   single-sex space access, sport eligibility, and carceral housing) is
 *   determined by an individual's declared gender identity rather than by
 *   reproductive biology (biology_reading) or by a combined
 *   biology-plus-transition gatekeeping process (hybrid_reading). Under this
 *   reading, trans women are included in the 'woman' category by
 *   self-declaration alone. The structural delta from the sibling readings is
 *   real and load-bearing: the victim set expands to include trans women who
 *   face misogyny once recognized as women, cis women lose an exclusive
 *   biological claim to sex-based protections, boundary-enforcement costs for
 *   the classification itself are low (no medical or judicial gatekeeping
 *   apparatus is needed), but conflict over shared physical space (shelters,
 *   prisons, changing rooms, sport) rises because the category boundary that
 *   governed access to those spaces no longer tracks the criterion those
 *   spaces were built around.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.42).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.38).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Self-Identification Standard for Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social ontology / identity politics / legal classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, 'fb4f9668-b882-46b6-ab27-8f1b64493d0a').
narrative_ontology:cs_kernel_codification('fb4f9668-b882-46b6-ab27-8f1b64493d0a', distributed).
narrative_ontology:cs_authority_grounding('fb4f9668-b882-46b6-ab27-8f1b64493d0a', distributed).
narrative_ontology:cs_reading_relation('fb4f9668-b882-46b6-ab27-8f1b64493d0a', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('fb4f9668-b882-46b6-ab27-8f1b64493d0a', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('fb4f9668-b882-46b6-ab27-8f1b64493d0a', foundational, gender_identity_is_the_authoritative_criterion_of_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_is_the_authoritative_criterion_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('fb4f9668-b882-46b6-ab27-8f1b64493d0a', gender_identity_is_the_authoritative_criterion_of_category_membership, deontological).
narrative_ontology:cs_axiom('fb4f9668-b882-46b6-ab27-8f1b64493d0a', secondary, medical_or_judicial_gatekeeping_of_category_membership_is_illegitimate).
narrative_ontology:cs_axiom_status(medical_or_judicial_gatekeeping_of_category_membership_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('fb4f9668-b882-46b6-ab27-8f1b64493d0a', medical_or_judicial_gatekeeping_of_category_membership_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('fb4f9668-b882-46b6-ab27-8f1b64493d0a', birth_registered_sex_administrative_default).
narrative_ontology:cs_drift_state('fb4f9668-b882-46b6-ab27-8f1b64493d0a', post_self_id_legislative_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fb4f9668-b882-46b6-ab27-8f1b64493d0a', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, nonbinary_individuals).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, female_athletes).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, domestic_violence_shelter_residents).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, detained_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women_facing_misogyny).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, trans_women_facing_misogyny).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition as women in the category they identify with, without requiring medical transition, surgery, or judicial process. Access to women's facilities, sports categories, and legal sex markers follows from declared identity. Exit from this framework is not really available to them personally — their inclusion in the category is what they are seeking, not a position they could trade away.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Use single-sex spaces (shelters, changing rooms, prisons, hospital wards) whose original rationale was reproductive-biology-based vulnerability and privacy. Under self-identification, category membership no longer tracks that biological criterion, so the space's boundary function is redefined without their consent. Their exit option is to avoid the space entirely or organize politically to contest the redefinition; they cannot opt out of the classification system that governs the spaces they need.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_in_sex_segregated_spaces, payer,
    organized, generational, constrained, national).

% Compete in a category originally organized around average performance differentials rooted in sex-linked physiology. Under self-identification, category eligibility follows declared identity rather than physiological baseline, changing the competitive field. Their options are to compete under the new standard, exit the sport, or lobby for policy carve-outs; the classification itself is not theirs to set.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, female_athletes, payer,
    moderate, biographical, constrained, national).

% Enter shelters fleeing male violence, often from men; the shelter's single-sex model was built on excluding males, including those who have transitioned, as a residual precaution regardless of individual intent. Under self-identification, admission follows declared identity. Residents in crisis have essentially no capacity to relocate, appeal, or select an alternative shelter model — they take what is offered.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, domestic_violence_shelter_residents, payer,
    powerless, immediate, trapped, local).

% Held in women's carceral facilities with no choice over housing policy. Under self-identification, housing assignment follows declared gender identity rather than birth sex or genital status, changing who they are housed alongside. They have no exit at all — placement is imposed by the institution.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, detained_women, payer,
    powerless, immediate, trapped, national).

% Once recognized as women, become subject to the same structures of sex-based discrimination, harassment, and violence directed at women as a class, while also facing distinct transphobic violence. The identity reading extends women's-category protections to them but does not remove the additional layer of anti-trans hostility they uniquely face; they cannot exit either category.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women_facing_misogyny, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, trans_women_facing_misogyny, payer).

% Advocate for, draft, and lobby to enshrine self-identification as the legal and institutional standard for sex/gender category membership. Shape policy language, litigate test cases, and set organizational compliance norms (e.g., in healthcare, employment, sport). Institutionally mobile — can pursue the standard across multiple jurisdictions and venues regardless of any single loss.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Argue that sex-based protections require a biological definition to function, and that self-identification dissolves the class the protections were built to serve. Frequently characterized as illegitimate voices in policy consultations and excluded from some drafting processes on the grounds that their position is itself discriminatory; their objections are treated as out of bounds rather than engaged on the merits in many venues.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, womens_sex_based_rights_organizations, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes between the competing readings — hear challenges to self-ID policies in sport, prisons, shelters, and employment; issue rulings that can affirm, narrow, or reject the identity standard in specific institutional contexts. Their rulings are fragmented across jurisdictions and domains rather than settling the underlying kernel.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, low-friction administrative rule for who counts as a woman (or man) for legal and institutional purposes, avoiding the need for medical certification, judicial hearings, or third-party gatekeeping — a genuine coordination gain for anyone whose lived identity does not match birth-registered sex.
% TRANSFER_FUNCTION: Moves the benefit of low-friction category access to self-identifying individuals, and moves the cost of redefined space/category boundaries onto cis women who relied on the prior biological boundary for privacy, safety, or fair competition — without their assent to the redefinition.
% ABSENT_VOICES: Sex-based-rights organizations for women, and detained/sheltered women with no institutional voice at all, are frequently excluded from the consultations that set self-ID policy; their objections are treated in many venues as illegitimate rather than substantively addressed.
% DISAPPEARANCE_RATIONALE: If the self-identification standard vanished overnight, trans people would lose accrued legal recognition, name/marker changes, and facility access built under this standard — a real rearrangement for them. Sex-based-rights advocates would say the world reverts to a status quo ante that functioned for decades on biological criteria. The two constituencies disagree sharply on whether reversion is a rearrangement or a restoration, which is exactly the kernel dispute this story is one reading of.
% FOUNDING_PROBLEM: Trans and nonbinary people faced exclusion, harassment, and legal nonrecognition under systems that required medical proof, surgical status, or judicial approval before any change in legal sex — a slow, expensive, sometimes impossible gatekeeping process that left many people legally misclassified for life.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and many public health bodies attest the gatekeeping problem was real and remains partly live in jurisdictions with medical requirements. Independent observers outside the advocacy movement — including some feminist legal scholars sympathetic to trans rights — corroborate that gatekeeping was excessive, while also documenting that the self-ID solution created a distinct second-order problem (space/category conflict) that the original gatekeeping model was in part designed to manage; this second problem is attested by shelter operators and sport governing bodies, not only by opponents.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, contested).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).
:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.18 to 0.42) as self-ID policy is adopted across more institutional domains (legal registries first, then sport and detention, which generate the most contested and highest-stakes conflicts). Suppression tracks upward similarly (0.15 to 0.38) as institutions increasingly treat objections to self-ID as impermissible rather than debatable — captured in the exclusion of sex-based-rights organizations from many consultations. Theater ratio stays comparatively low (0.10 to 0.22): most institutional activity under this reading is substantive policy change, not performance, though some 'inclusive policy' announcements function more as signaling than operational change in facilities that see little actual gender-diverse traffic. Accessibility collapse is moderate (0.35): once adopted, self-ID becomes the default administrative posture and alternatives (case-by-case assessment, opt-in single-sex-by-birth-sex spaces) become harder to access, but they have not vanished entirely — some jurisdictions retain carve-outs. Resistance is high (0.72), reflecting sustained, organized political and legal contestation from sex-based-rights groups, sport federations, and some shelter operators.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and nonbinary individuals are the structural beneficiaries: the constraint removes a gatekeeping burden they previously bore, at low direct enforcement cost to the institutions adopting it. Cis women in sex-segregated spaces, female athletes, shelter residents, and detained women are the payers: the classification boundary they relied on for its excluding function (privacy from and separation from those with male physiology) is redefined without their participation, and their exit options are constrained-to-trapped depending on the domain — a shelter resident in crisis or a detained woman has essentially no exit at all. Trans women themselves also appear as partial payers in the misogyny they encounter once recognized as women, which the identity reading extends to them as a category-membership consequence, not a separate harm; this is the expected 'expanded victim set' delta named in the kernel context. Gender identity advocacy organizations are the agenda-setters: organized, mobile, and generationally invested in the standard's adoption. Sex-based-rights organizations are the excluded voice: engaged in the dispute but frequently denied standing in the policy venues that decide it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — burdensome, sometimes impossible medical/judicial gatekeeping that left trans people legally unrecognized — was real and is corroborated by sources outside the advocacy movement, so this is not classified as a pure snare riding on a fake coordination story. But the self-ID solution generates a second-order coordination problem (space/category access) that the original gatekeeping model existed partly to manage, and that second problem is not resolved by this reading, only relocated onto a different population (cis women in constrained-exit positions). Tangled Rope, not rope, because the coordination gain for one group and the extraction cost for another run through the identical structure — the same self-declaration rule that grants access is what removes the prior boundary protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_selection_ambiguity,
    'Which of the three kernel readings (biology, hybrid, identity) should govern a given institutional domain — and does the correct reading vary by domain (e.g., legal registry vs. detention housing vs. elite sport) rather than being uniform across all of them?',
    'Comparative institutional analysis of outcomes across jurisdictions that apply different readings to different domains (e.g., self-ID for legal sex marker but biology-based eligibility for elite sport) to determine whether domain-differentiated reading selection resolves more of the conflict than a uniform kernel reading.',
    'If domain-differentiated reading selection resolves most of the conflict, this suggests the kernel dispute is partly a category error — treating a domain-varying question as if it required one uniform answer — which would lower the stakes of the abstract kernel contest relative to domain-specific policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_selection_ambiguity, conceptual, 'Whether the kernel readings should be applied uniformly or domain-by-domain.').

omega_variable(
    expanded_victim_set_weighting,
    'How should the misogyny trans women experience once included in the ''woman'' category be weighted against the space-access costs borne by cis women, when both are authored as victim populations under this single reading?',
    'Longitudinal tracking of documented harm incidence and severity in both populations under jurisdictions that have adopted self-ID, compared to pre-adoption baselines and to hybrid/biology-reading jurisdictions.',
    'If the harms are comparable in scale, the tangled_rope classification (genuine coordination + genuine extraction through the same structure) is well-supported; if one population''s harm is much larger, the classification may need revisiting toward snare (if cis women''s costs dominate and are structurally suppressed) or toward rope (if trans women''s inclusion benefit dominates and the space-access costs are marginal).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expanded_victim_set_weighting, empirical, 'Relative severity of harms to the two victim populations under this reading.').

omega_variable(
    self_identification_verifiability,
    'Is a purely subjective self-identification criterion, with no external verification step, structurally distinguishable from an unverifiable claim for institutional purposes — and does that unverifiability itself constitute a form of accessibility collapse for institutions trying to design safeguards?',
    'Analysis of documented cases of bad-faith invocation of self-ID claims for access to single-sex spaces, weighed against the base rate of such invocation relative to genuine trans population size, in jurisdictions with data collection.',
    'A high bad-faith invocation rate would strengthen the payer-side extraction reading; a low rate would support characterizing most cited space-access conflicts as rare edge cases inflated in political discourse rather than structural features of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_identification_verifiability, empirical, 'Whether unverifiable self-identification enables exploitation at a structurally significant rate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sex__tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__identity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sex__be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__identity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(sex__su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__identity_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the sex_gender_category kernel. sex_gender_category__biology_reading authors category membership by reproductive biology (low ε, minimal contested boundary, likely mountain/rope for cis-only populations but exclusionary of trans populations). sex_gender_category__hybrid_reading authors a medical-gatekeeping combination (moderate ε, moderate enforcement cost, different victim/beneficiary balance than either pure reading). Each reading has a distinct beneficiary/victim structure and a distinct ε; they are linked here for contamination/family analysis, not averaged or reconciled into one classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
