% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Woman Category Membership via Self-Declared Gender Identity
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint models the gender_identity_reading of the contested
 *   'gendered category membership' kernel: category membership in 'woman' is
 *   grounded in subjective self-declared identity, not in birth-assigned
 *   biological markers (the biological_sex_reading, a separate constraint) or
 *   in sustained social performance and recognition (the social_role_reading,
 *   also separate). Under this reading, trans women are included in the
 *   category 'woman' by declaration, sex-segregated institutional spaces
 *   functionally become gender-segregated, and the coordination function
 *   (dignified, low-friction recognition avoiding invasive verification)
 *   coexists with a real transfer: institutional access and categorical
 *   protections move toward self-identifying claimants and away from parties
 *   who relied on the prior criterion for safety or fairness claims specific
 *   to sex. ε is authored moderate because the gatekeeping/adjudication costs
 *   and the transfer to specific payer groups (not merely diffuse social
 *   friction) are real and rising as the reading has been adopted more
 *   broadly in law and institutional policy, but the underlying coordination
 *   function (recognition, dignity, reduction of forced-disclosure harm) is
 *   also genuine and substantial, which is why this is authored tangled_rope
 *   rather than snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.38).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Woman Category Membership via Self-Declared Gender Identity").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '6f24b620-9f59-4eb6-8ec3-5c8819d8e893').
narrative_ontology:cs_kernel_codification('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', distributed).
narrative_ontology:cs_authority_grounding('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', distributed).
narrative_ontology:cs_reading_relation('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', foundational, self_declared_identity_is_constitutive_of_category_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_is_constitutive_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', self_declared_identity_is_constitutive_of_category_membership, deontological).
narrative_ontology:cs_axiom('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', secondary, invasive_verification_of_gender_claims_constitutes_harm).
narrative_ontology:cs_axiom_status(invasive_verification_of_gender_claims_constitutes_harm, holdable).
narrative_ontology:cs_axiom_grounding('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', invasive_verification_of_gender_claims_constitutes_harm, instrumental).
narrative_ontology:cs_created_at('6f24b620-9f59-4eb6-8ec3-5c8819d8e893', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, female_athletes_in_open_categories).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_self_identification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek recognition as women in law, social space, and institutional categorization on the basis of self-declared gender identity rather than birth-assigned sex. Access to sex-segregated facilities, sports categories, and legal sex markers under this reading depends on this category membership being honored without additional gatekeeping. For many, identity is not a strategic choice but a lived, non-negotiable self-understanding; exit from the claim is not experienced as available.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Lobby for policy, institutional guidance, and legal reform enshrining self-identification as the operative criterion for category membership. Set the terms of what counts as legitimate inclusion, shape institutional training and compliance standards, and benefit organizationally (funding, mandate, legitimacy) from the doctrine's adoption and defense.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations, beneficiary).

% Use shelters, changing rooms, prisons, and support groups organized around sex-based vulnerability (to male violence, to dysphoria around bodily exposure). Under this reading, objecting to the presence of a self-identified trans woman in these spaces is treated as exclusionary bigotry rather than a legitimate boundary claim, regardless of the objector's own history or vulnerability. Their exit options are limited to the specific institution's alternatives, which may not exist locally.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_in_sex_segregated_spaces, payer,
    powerless, biographical, trapped, local).

% Compete in categories historically segregated by sex to offset average performance differences rooted in developmental biology. Under self-ID inclusion, they may lose competitive opportunities, podium positions, or scholarships to competitors who transitioned after male puberty. Their only exit is to stop competing or accept the outcome; there is no parallel category that preserves their prior competitive position.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, female_athletes_in_open_categories, payer,
    powerless, biographical, trapped, national).

% Argue that sex is the material basis for the specific vulnerabilities and exclusions the segregated spaces exist to address, and that self-ID collapses this basis. Frequently excluded from institutional consultation processes, deplatformed from professional and academic venues, or labeled as engaged in hate speech when raising the objection inside the frameworks this reading has captured.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, excluded,
    organized, biographical, constrained, national).

% Courts, sports governing bodies, and healthcare systems must adjudicate competing claims under this reading — determining what counts as valid self-declaration, whether any threshold (duration, documentation, hormonal status) applies, and how to resolve conflicts with the other kernel readings. Their rulings actively shape which version of the reading becomes operative law or policy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, medical_and_legal_gatekeeping_institutions, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, medical_and_legal_gatekeeping_institutions, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, humane, low-friction criterion (self-declared identity) for who belongs in the 'woman' category, avoiding invasive verification, avoiding the psychological harm of forced disclosure or examination, and extending recognition and safety to a population with documented high rates of violence and discrimination.
% TRANSFER_FUNCTION: Moves institutional recognition, physical access to sex-segregated spaces, and eligibility in female sport and legal-sex-marked contexts from a criterion requiring evidence of birth sex or transition status toward a criterion requiring only self-declaration — shifting the cost of adjudicating disputed membership from the person claiming membership onto whoever previously relied on the excluded criterion for their own safety or fairness claims.
% ABSENT_VOICES: Detransitioners whose self-declared identity changed over time are rarely centered in either advocacy or critique; women's sex-based-rights organizations report exclusion from consultation processes that produced the policy; survivors of male violence with specific trauma responses to male bodies in intimate spaces are frequently characterized as bigoted rather than heard as a distinct interest group.
% DISAPPEARANCE_RATIONALE: If self-declaration were withdrawn as the operative criterion overnight, current legal sex changes, sports eligibility rulings, and institutional access policies built on it would need re-adjudication under a different criterion (biological or social-role), materially changing outcomes for trans women's access and for the two payer groups' competitive and safety positions — this is not a constraint whose removal would leave the world as it is.
% FOUNDING_PROBLEM: Historically, transgender people faced institutional non-recognition, forced sterilization or invasive proof requirements to change legal sex, exclusion from any coherent category, and resulting exposure to violence and discrimination when neither treated as their natal sex nor as their identified gender.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and many clinicians attest the founding problem (non-recognition, forced proof, exposure to violence) remains substantially live. Independent human-rights bodies and some detransition researchers corroborate elevated violence rates against trans people as an ongoing empirical fact. Gender-critical feminist organizations and some sports governing bodies dispute that self-declaration (as opposed to legal recognition on other terms, e.g. verified transition status) is the necessary or sufficient remedy, arguing the current arrangement has moved from solving the founding problem to displacing a different population's sex-based claims — this dispute is unresolved and sits outside the advocacy organizations that benefit from the current criterion.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).
:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising over the interval: as self-ID has been adopted into more institutional policy (sport, prisons, shelters, legal sex markers), the transfer from the payer groups (cis women in vulnerable segregated contexts, female athletes) has grown more concrete and consequential rather than remaining abstract. Suppression (0.38) reflects the increasingly real professional and social cost of contesting the reading in institutional and public settings (deplatforming, employment consequences, characterization as bigotry) but is not total — organized resistance (gender-critical feminist organizations) persists and has won some legal contests, which is why resistance is authored high (0.62) rather than near-zero. Theater ratio is modest (0.22): most institutional adoption reflects genuine policy commitment rather than pure performance, though some corporate/institutional diversity messaging around the reading is more symbolic than operationally consequential.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are the structural beneficiaries of this specific reading: it grants them the category membership they seek without requiring proof beyond declaration, and their exit option is authored identity_locked because for most, gender identity is not a strategic position to be abandoned for tactical advantage. Gender identity advocacy organizations are agenda-setters and secondary beneficiaries: they set policy terms and gain institutional standing and mandate. Cis women in sex-segregated spaces and female athletes in open categories are payers: the criterion shift moves a real cost onto them specifically because their prior claims were themselves grounded in sex, not gender identity, and this reading does not preserve a parallel category for that sex-based claim. Both payer groups are authored powerless/trapped because the relevant spaces (shelters, changing rooms, competitive categories) typically have no accessible alternative once the criterion changes locally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (non-recognition and forced invasive proof requirements harming trans people) is genuinely live by most independent accounts, which blocks a simple 'this is pure mandatrophy' reading. But the founding_problem_status is authored contested because the specific remedy — unconditional self-declaration as the operative criterion for all sex-segregated contexts, rather than legal recognition on other terms — is disputed by parties outside the benefiting coalition, including some human-rights-oriented feminist organizations and sports scientists. The tangled_rope classification exists precisely to prevent this dispute from being flattened into either 'pure discriminatory backlash against a settled coordination norm' or 'pure ideological capture with no genuine underlying problem' — both a real coordination function and a real, growing, unevenly distributed cost coexist in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_declaration_sufficiency_ambiguity,
    'Is unconditional self-declaration a sufficient and stable criterion for category membership across all institutional contexts (sport, prisons, shelters, legal documents), or does its sufficiency vary by the specific interest the segregated context was designed to protect?',
    'Comparative institutional analysis: track outcomes and dispute rates across jurisdictions/institutions that adopt pure self-ID versus those that layer additional criteria (duration, hormonal status, legal documentation) for specific high-stakes contexts (sport, carceral settings).',
    'If sufficiency varies sharply by context, this reading may need to be further decomposed into context-specific constraints (e.g. self-ID for legal name/pronoun recognition versus self-ID for elite sport eligibility) rather than treated as one uniform criterion — an ε-invariance concern for future decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_declaration_sufficiency_ambiguity, conceptual, 'Whether self-declaration functions as one uniform criterion or needs further context-specific decomposition.').

omega_variable(
    committer_structure_kernel_location,
    'This constraint is one reading (gender_identity_reading) of the gendered_category_membership kernel, alongside biological_sex_reading and social_role_reading. Where is the actual disagreement between readings located — is it a disagreement about facts (what sex/gender IS), about which criterion best serves an agreed-upon value (safety, dignity, fairness), or about which populations'' interests should be weighted more heavily when criteria conflict?',
    'This is not resolvable by additional data alone; it requires philosophical/political adjudication of competing value frameworks (a preference-type question), though the biological_sex_reading and social_role_reading files should be examined for where their axioms diverge from this reading''s axioms to locate the precise structural fork.',
    'If the disagreement is purely factual, one reading could in principle be shown false; if it is a values disagreement about weighting competing legitimate interests, no reading forecloses the others and coexistence (as authored in cs_structure.reading_relations) is the structurally honest outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_location, preference, 'Where the disagreement between kernel readings is actually located: fact, value-weighting, or interest-priority.').

omega_variable(
    trans_women_beneficiary_vs_victim_dual_status,
    'Trans women are authored as beneficiaries of this reading''s inclusion criterion, but under the biological_sex_reading and in contested public discourse they are also frequently subject to exclusion, violence, and non-recognition themselves — is ''beneficiary'' the complete structural description, or are trans women simultaneously beneficiaries of this specific reading AND victims of the broader kernel contest itself?',
    'Track outcomes for trans individuals specifically under jurisdictions where this reading has been rolled back or contested, distinguishing harm from the kernel contest itself (being caught between readings) from harm attributable to this reading''s operation.',
    'If trans women are also significantly harmed by the instability of the kernel contest itself, that harm belongs to the kernel-level dispute rather than to this single reading''s ε, and should not be double-counted here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_women_beneficiary_vs_victim_dual_status, conceptual, 'Whether trans women''s beneficiary status under this reading captures their full structural position or elides kernel-contest-level harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__gender_identity_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__gender_identity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__gender_identity_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__gender_identity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__gender_identity_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__gender_identity_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__gender_identity_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__gender_identity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__gender_identity_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__gender_identity_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__gender_identity_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__gender_identity_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gendered_category_membership kernel (per the ε-invariance principle, decomposed rather than averaged): biological_sex_reading grounds membership in immutable birth markers (forecloses this reading's core premise — the two cannot jointly hold in a single framework, since one asserts self-declaration is constitutive and the other asserts it is irrelevant to the operative criterion); social_role_reading grounds membership in sustained social performance and recognition (coexists_with this reading — a person could satisfy both self-declaration and sustained social role simultaneously, and many actual policy regimes blend the two without logical contradiction). Each reading carries its own ε, beneficiaries, victims, and stakeholder set; this file does not aggregate or average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
