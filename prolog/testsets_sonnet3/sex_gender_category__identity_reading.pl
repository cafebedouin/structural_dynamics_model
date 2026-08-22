% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   This story authors the identity-reading of a contested kernel: category
 *   membership in 'woman'/'man' is determined by subjective
 *   self-identification, with no medical or biological precondition. Under
 *   this reading, trans women are included in the category 'woman' without
 *   qualification, which resolves the founding exclusion problem (gatekept,
 *   unaffordable, or inaccessible medical transition requirements) but
 *   reallocates the boundary-drawing power away from cis women's exclusive
 *   claim to sex-based protections. Enforcement costs at the level of
 *   individual boundary policing are low (no document review, no medical
 *   verification required) but conflict intensity over single-sex space and
 *   sport access is high, exactly as the expected structural delta specifies.
 *   This is ONE of three sibling readings of the same kernel
 *   (biology_reading, hybrid_reading); each is authored as its own constraint
 *   with its own epsilon, beneficiary/victim structure, and classification —
 *   this file does not average across them or describe the contest itself.
 *
 * KEY AGENTS:
 *   - trans_women: primary beneficiary (moderate/constrained) — gains recognition without medical gatekeeping
 *   - cis_women_in_single_sex_spaces: primary payer (moderate/trapped) — loses exclusive natal-sex claim to protected spaces
 *   - identity_affirming_institutions: agenda_setter (institutional/mobile) — writes and administers the policy, bears little direct cost
 *   - female_athletes_in_sex_segregated_sport: concentrated payer (powerless/trapped) — bears performance-category cost with no voice in federation policy
 *   - domestic_violence_shelter_residents: concentrated payer (powerless/trapped) — bears safeguarding-relevant cost at moment of acute vulnerability
 *   - gender_critical_feminists: excluded voice (moderate/constrained) — raises the competing interest but is frequently kept out of the policy-setting conversation
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
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Self-Identification Standard for Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '3d8398d9-b045-4e10-a5c3-5c40378c4df5').
narrative_ontology:cs_kernel_codification('3d8398d9-b045-4e10-a5c3-5c40378c4df5', distributed).
narrative_ontology:cs_authority_grounding('3d8398d9-b045-4e10-a5c3-5c40378c4df5', distributed).
narrative_ontology:cs_reading_relation('3d8398d9-b045-4e10-a5c3-5c40378c4df5', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('3d8398d9-b045-4e10-a5c3-5c40378c4df5', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3d8398d9-b045-4e10-a5c3-5c40378c4df5', foundational, gender_identity_is_the_operative_criterion_of_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_is_the_operative_criterion_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('3d8398d9-b045-4e10-a5c3-5c40378c4df5', gender_identity_is_the_operative_criterion_of_category_membership, deontological).
narrative_ontology:cs_axiom('3d8398d9-b045-4e10-a5c3-5c40378c4df5', secondary, medical_or_legal_gatekeeping_of_category_membership_is_illegitimate).
narrative_ontology:cs_axiom_status(medical_or_legal_gatekeeping_of_category_membership_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('3d8398d9-b045-4e10-a5c3-5c40378c4df5', medical_or_legal_gatekeeping_of_category_membership_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('3d8398d9-b045-4e10-a5c3-5c40378c4df5', gatekept_medical_transition_model).
narrative_ontology:cs_drift_state('3d8398d9-b045-4e10-a5c3-5c40378c4df5', contemporary_self_id_legislative_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3d8398d9-b045-4e10-a5c3-5c40378c4df5', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, nonbinary_people).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, identity_affirming_institutions).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_in_single_sex_spaces).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, female_athletes_in_sex_segregated_sport).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, domestic_violence_shelter_residents).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_identity_as_the_relevant_category_of_womanhood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition as women without requiring medical transition, surgery, or diagnosis as a precondition. Access to single-sex spaces, sport categories, and legal documentation follows from declared identity. Exit from the arrangement is not really available to them in any meaningful sense — the alternative (classification by natal sex) is the thing they are seeking relief from, not a live option they could return to if the standard were withdrawn.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, constrained, national).

% Gain recognition as men on the same self-identification basis; largely a secondary beneficiary of the same rule, though most of the contested space-access conflict in this reading centers on trans women and female-designated spaces.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, beneficiary,
    moderate, biographical, constrained, national).

% Gain legal recognition of a category outside the binary that self-identification frameworks were extended to accommodate; benefit from the same principle that category membership follows declared identity rather than biological criteria.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, nonbinary_people, beneficiary,
    powerless, biographical, constrained, national).

% Universities, HR departments, medical bodies, and civil-rights organizations that write and enforce self-ID policy into law, employment practice, and service provision. They administer the classification rule, train staff on compliance, and adjudicate disputes. They bear little direct cost from the rule and gain legitimacy, funding, and alignment with prevailing professional norms by adopting it; they can revise policy language relatively cheaply if institutional consensus shifts.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, identity_affirming_institutions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, identity_affirming_institutions, beneficiary).

% Lose the ability to invoke natal sex as a categorical criterion for excluding male-bodied individuals from changing rooms, shelters, prisons, and rape crisis services, because the self-identification standard makes gender identity the operative criterion instead. Those who object publicly report reputational and professional costs; the practical exit is avoiding the shared space altogether, which is itself a loss of the resource the space was meant to provide. Exit from the classification dispute itself is not available — the categories are set by law and institutional policy, not by individual negotiation.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_in_single_sex_spaces, payer,
    moderate, biographical, trapped, national).

% Compete in categories that, under this reading, include athletes who transitioned after puberty and retain performance-relevant physiological advantages. Their only exit is leaving the sport or the category; the classification rule is set by federations responding to legal and institutional pressure, not by athlete vote.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, female_athletes_in_sex_segregated_sport, payer,
    powerless, biographical, trapped, national).

% Are often fleeing male violence and seeking single-sex refuge; under this reading, shelter admission criteria follow declared gender identity rather than natal sex, which some residents experience as reintroducing the presence of male-socialized bodies into the exact space meant to exclude them. Residents in crisis have essentially no capacity to litigate or relocate.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, domestic_violence_shelter_residents, payer,
    powerless, immediate, trapped, local).

% Adjudicate disputes between the self-identification standard and single-sex exemptions carved into equality and safeguarding law; produce rulings and statutes that either harden or soften the self-ID standard's reach into contested spaces.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% Argue that sex-based protections require a biological definition of woman and that self-identification erases the basis on which those protections were won; frequently characterized as bigoted within identity-affirming institutions and excluded from platforms, professional advancement, and coalition spaces where the self-ID standard is treated as settled.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-friction, dignity-preserving path for trans and nonbinary people to have their lived gender recognized in law, documentation, and everyday categorization without requiring medical gatekeeping, diagnosis, or surgery as preconditions — solving the real problem that medical-transition requirements excluded people who could not access, afford, or medically tolerate transition procedures.
% TRANSFER_FUNCTION: Moves the power to define category membership in sex-segregated spaces and competitions from a biology-based criterion (verifiable by birth records or medical history) to a self-report criterion, which shifts the incidence of contested access away from trans people seeking recognition and onto cis women and girls who can no longer invoke natal sex to exclude self-identified members from spaces designed around sex-based vulnerability or performance categories.
% ABSENT_VOICES: Gender-critical feminists and safeguarding-focused shelter staff who argue the standard reintroduces exactly the risk single-sex provision was designed to manage are frequently excluded from the institutional conversation that sets policy, characterized as motivated by animus rather than as raising a genuine competing interest; residents of shelters and young female athletes, who bear concentrated costs, are rarely direct parties to the legal and institutional debates that set the category rule.
% DISAPPEARANCE_RATIONALE: If self-identification as the sole category criterion were withdrawn overnight, institutions would revert to biology-based or hybrid gatekeeping criteria; trans people without medical documentation would lose legal recognition in the affected categories; single-sex space and sport disputes would be resolved by a different criterion, changing outcomes for a subset of contested cases in both directions.
% FOUNDING_PROBLEM: Medical and legal gatekeeping requirements (diagnosis, surgery, hormone therapy, judicial sign-off) excluded many trans people from legal recognition entirely — people who could not access care, could not afford it, were medically ineligible, or lived in jurisdictions with no pathway at all remained legally classified by natal sex indefinitely, with attendant discrimination and documentation mismatch harms.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and the institutions that adopted self-ID policy attest the founding problem (gatekeeping-based exclusion) remains largely solved by the current standard. Independent of the beneficiary set, some clinicians, family law practitioners, and single-sex service providers attest a different problem has emerged in its place — contested access to spaces whose sex-based rationale did not depend on legal gatekeeping difficulty but on natal-sex-linked vulnerability or performance differences; this second group is not drawn from either side's core beneficiaries and its testimony is the main outside corroboration available.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 at interval end) rather than low or severe: the standard delivers genuine coordination value (recognition without gatekeeping) for a beneficiary group with real unmet need, but imposes a concentrated, non-consensual cost on cis women and girls in a small number of high-stakes, sex-segregated contexts (shelters, sport, prisons) where natal-sex-linked vulnerability or performance differential is the entire rationale for segregation. Suppression is authored moderate-low (0.38): the standard is enforced primarily through institutional policy, professional norm enforcement, and reputational sanction against dissent rather than criminal coercion, but it is real — objectors report career and platform costs. Accessibility collapse is authored low-moderate (0.3): the hybrid and biology readings remain live legal and institutional alternatives being actively fought over in courts and legislatures, so alternatives have not collapsed; resistance is authored high (0.72) precisely because this is an actively, publicly contested standard, not a settled one.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (trans and nonbinary people), the standard reads as rope or scaffold — necessary transitional coordination correcting an exclusionary gatekeeping regime, low suppression, minimal victims. From the concentrated payer seats (shelter residents, female athletes), the same structure reads as tangled_rope shading toward snare — real coordination function for someone else, layered onto an involuntary transfer of protective capacity away from them, sustained by institutional and reputational enforcement against dissent. The engine computes both seat-level readings from the same structural data; this divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans and nonbinary people are the structural beneficiaries under this reading: the rule removes a gatekeeping burden that previously fell on them, and their exit from the arrangement is effectively unavailable because the alternative is the exclusion they are seeking relief from. Cis women in single-sex spaces, female athletes, and shelter residents are the structural payers: the rule removes a categorical tool (natal sex) they previously could invoke, and their exit options are trapped or highly constrained — leaving the sport, avoiding the shelter, or accepting the shared space are not meaningful substitutes for the protection the segregation was designed to provide. Identity-affirming institutions are agenda-setters with mobile exit: they administer the rule, gain legitimacy and alignment benefits from adopting it, and can revise policy language at comparatively low cost if institutional consensus shifts, which is structurally different from the trapped exit facing the concentrated payer groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusionary medical gatekeeping barring legal recognition — is genuinely contested as either live or dead depending on jurisdiction and the specific gatekeeping regime being compared against; it is not simply dead-but-persisting (which would indicate pure mandatrophy) nor simply live-and-unaddressed (which would indicate pure rope). The self-ID standard is authored as tangled_rope rather than snare because a real, ongoing coordination function persists (recognition without medical barriers) alongside the asymmetric cost now falling on the concentrated payer groups; classifying it as pure extraction would erase the genuine unmet need that motivated the standard, while classifying it as pure coordination would erase the non-consensual reallocation of protective capacity documented in the victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_id_verification_versus_intent_ambiguity,
    'Is the self-identification standard, as actually administered, a genuine no-verification declaration standard, or does it retain informal verification/plausibility checks that partially reintroduce the hybrid model under a different name?',
    'Audit of actual institutional practice (shelter intake procedures, sport federation eligibility panels, legal document processors) to determine whether declared identity alone suffices or whether informal gatekeeping persists.',
    'If informal verification persists widely, the identity_reading as authored here (pure self-declaration, low accessibility_collapse) is empirically less common than believed, and many real-world cases are actually operating under a de facto hybrid_reading despite formal self-ID law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_id_verification_versus_intent_ambiguity, empirical, 'Whether self-ID is administered as pure declaration or with informal gatekeeping.').

omega_variable(
    which_reading_the_kernel_should_take,
    'This story is one reading (identity_reading) of the sex_gender_category kernel; the sibling readings (biology_reading, hybrid_reading) are separate constraints with different beneficiary/victim structures. Which reading a given legal jurisdiction or institution SHOULD adopt is not resolved by this story and is not a fact this story asserts.',
    'This is a normative/political question, not an empirical one resolvable by data internal to any single reading; it is resolved (provisionally, per-jurisdiction) by legislative and judicial process, and remains genuinely contested across jurisdictions.',
    'Adopting a different reading changes which agents are beneficiaries versus victims, changes the epsilon value, and changes the classification — this is exactly why the readings are authored as separate constraint files rather than as parameters of one constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_kernel_should_take, preference, 'The inter-reading choice is a values question routed to omega, not adjudicated within this file.').

omega_variable(
    performance_advantage_persistence_ambiguity,
    'For the sport-access cost specifically: to what degree do physiological advantages from male puberty persist after hormone therapy, and does this vary enough by sport/event that a uniform self-ID standard for competitive categories is the wrong grain of policy regardless of which reading is adopted?',
    'Sport-specific physiological studies tracking performance-relevant markers pre- and post-hormone therapy across different suppression durations and event types.',
    'If advantage persistence is sport-specific and substantial in some events but negligible in others, a uniform category rule (under any reading) may be the wrong instrument, and the victim set for female_athletes_in_sex_segregated_sport should be scoped to specific events rather than sport-wide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_advantage_persistence_ambiguity, empirical, 'Sport-specific physiological uncertainty affecting the scope of the sport-access cost.').


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
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sex__su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__identity_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.1).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the sex_gender_category kernel, decomposed per the ε-invariance principle: identity_reading (this file), biology_reading, and hybrid_reading each instantiate a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, because the natural-language label 'category membership' covers three different structural claims that a single observable cannot average over. Each file's beneficiary and victim sets differ: identity_reading's victim set (cis women in single-sex spaces, female athletes, shelter residents) does not appear in biology_reading, whose victim set instead centers on trans people facing legal misclassification and documentation mismatch.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
