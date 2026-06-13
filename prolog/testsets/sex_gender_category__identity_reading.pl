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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Sex/Gender Category Membership via Self-Identified Gender (Identity Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   The identity reading instantiates sex/gender category membership via
 *   self-identification without medical gatekeeping or biological
 *   verification. Under this reading, trans women are included in the legal
 *   'woman' category upon self-declaration, which expands access to
 *   sex-segregated spaces (bathrooms, shelters, prisons, sports) and
 *   sex-based legal protections. The constraint benefits trans women and
 *   gender-self-determination advocates by reducing gatekeeping barriers and
 *   providing formal legal recognition; it imposes costs on cis women (loss
 *   of exclusive category access) and sex-based-protection-dependent
 *   populations (expanded space boundaries). The reading vindicates a
 *   doctrine of gender autonomy and self-determination, which stands in
 *   tension with the biology_reading and creates contention that requires
 *   active enforcement.
 *
 * KEY AGENTS:
 *   - trans_women: Beneficiaries via self-identification; identity-locked exit (reverting to legal male classification is identity-incompatible)
 *   - cis_women: Organized payers; some are also agenda-setters in the dispute; constrained exit (cannot leave the category but contest its boundary)
 *   - sex_based_protection_dependent_populations: Powerless payers; trapped exit; experience space boundary expansion as loss of safety/privacy
 *   - gender_self_determination_advocates: Organized beneficiaries; mobile exit (can exit via geography); institutional access
 *   - enforcement_infrastructure: Institutional agenda-setters; administer the boundary via documents and policy; analytical exit only
 *   - biology_reading_advocates: Organized opposition; excluded from beneficiary set; mobile exit (geographic/cultural relocation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Sex/Gender Category Membership via Self-Identified Gender (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '227655d3-55ef-4148-ad54-cc22cf1acc0c').
narrative_ontology:cs_kernel_codification('227655d3-55ef-4148-ad54-cc22cf1acc0c', distributed).
narrative_ontology:cs_authority_grounding('227655d3-55ef-4148-ad54-cc22cf1acc0c', extraction).
narrative_ontology:cs_reading_relation('227655d3-55ef-4148-ad54-cc22cf1acc0c', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('227655d3-55ef-4148-ad54-cc22cf1acc0c', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('227655d3-55ef-4148-ad54-cc22cf1acc0c', foundational, gender_identity_self_determination_foundational).
narrative_ontology:cs_axiom_status(gender_identity_self_determination_foundational, holdable).
narrative_ontology:cs_axiom_grounding('227655d3-55ef-4148-ad54-cc22cf1acc0c', gender_identity_self_determination_foundational, deontological).
narrative_ontology:cs_axiom('227655d3-55ef-4148-ad54-cc22cf1acc0c', secondary, gatekeeping_reduction_via_declaration).
narrative_ontology:cs_axiom_status(gatekeeping_reduction_via_declaration, holdable).
narrative_ontology:cs_axiom_grounding('227655d3-55ef-4148-ad54-cc22cf1acc0c', gatekeeping_reduction_via_declaration, instrumental).
narrative_ontology:cs_reference_frame('227655d3-55ef-4148-ad54-cc22cf1acc0c', identity_autonomous_determination).
narrative_ontology:cs_drift_state('227655d3-55ef-4148-ad54-cc22cf1acc0c', contemporary_enforcement_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('227655d3-55ef-4148-ad54-cc22cf1acc0c', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_self_determination_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, sex_based_protection_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, institutional_sex_based_service_administrators).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_identity_self_determination_doctrine).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, inclusive_non_discrimination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain formal legal recognition in the 'woman' category via self-identification, which flows into access to women's spaces (bathrooms, shelters, prisons, sports), legal documentation (driver's license, passport, vital records), and protection under sex-discrimination law. The constraint's enforcement makes their self-identified category official and enforceable. Exit would mean either reverting to legal male classification (identity-incompatible) or accepting non-recognition in any category.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Lose the exclusive legal claim to sex-based spaces and protections (bathrooms, shelters, prisons, sports, women's colleges, domestic violence services). The category boundary has expanded to include trans women via self-identification rather than biological or medical criteria, which some cis women experience as a loss of exclusive access and a dilution of sex-based protection frameworks. They maintain formal legal status in the category but the material scope of exclusion from cis males has contracted. Many cis women are agenda-setters in the dispute, advocating for either maintenance of biological criteria or hybrid criteria.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, cis_women, agenda_setter).

% Vulnerable populations whose access to women's shelters, prisons, bathrooms, and medical services depended on the category boundary being drawn at biology. The expansion of the category via self-identification means their protected spaces now admit trans women, which some in this group perceive as a loss of safety (from male violence or male-attracted persons) or a breach of medical privacy. Their exit options are trapped: they cannot leave the situation and have limited alternatives for protected space.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, sex_based_protection_dependent_populations, payer,
    powerless, biographical, trapped, national).

% Benefit from the normative and legal instantiation of self-identification as the legitimate basis for category membership. This reading vindicates their broader doctrine of gender autonomy and forms the foundation for non-discrimination law, healthcare access, and social recognition. They are organized advocates with substantial institutional access and can exit by geography (moving to jurisdictions where the identity reading is not enforced).
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_self_determination_advocates, beneficiary,
    organized, generational, mobile, global).

% Government agencies, courts, and institutional policy-setters (prisons, schools, hospitals, sports bodies) that must enforce the boundary via issuing documents, managing space allocation, adjudicating disputes, and implementing policy. They administer the constraint but do not systematically benefit or pay from it; they experience the constraint as an administrative obligation and ongoing contention. Their exit would require legislative or constitutional change.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, enforcement_infrastructure, agenda_setter,
    institutional, generational, analytical, national).

% Would gain legal recognition in the 'man' category via self-identification under this reading's logic, but are largely absent from the public dispute about the boundary. Some cis women's organizations and some cis men's advocates would contest the symmetric application of the reading to trans men, which would implicate exclusion of trans men from women's spaces and inclusion in men's spaces. They would have objections to the boundary's application but are not centered in the dispute.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, excluded,
    moderate, biographical, identity_locked, national).

% Administrators of women's shelters, prisons, bathrooms, changing facilities, and sex-segregated services must navigate the boundary expansion in real time. They face operational costs (redesigning bathroom/shower facilities, managing space conflicts, navigating privacy and safety disputes among users) and reputational risk from both constituencies. They cannot exit without state mandate change.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, institutional_sex_based_service_administrators, payer,
    institutional, biographical, constrained, national).

% Advocates for the competing biology_reading who argue that reproductive biology is the legitimate basis for category membership. They would argue that the identity_reading is illegitimate and would restore the biology_reading if they prevailed. They are excluded from the beneficiary set of this reading and organize in opposition to it. They could exit via geographic relocation or cultural/institutional exit if the identity_reading were universally enforced.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, biology_reading_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, trans_women).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal category for sex-based rights, protections, and services, with membership determined by self-identification rather than medical gatekeeping or biological criteria. Solves the problem of legal recognition for trans people and reduces administrative burden of requiring medical documentation or proof of transition.
% TRANSFER_FUNCTION: Moves access to women's spaces, sex-discrimination protections, and legal documentation from an exclusive cis-women's category to an expanded category that includes trans women via self-identification. Also transfers the legitimacy of the boundary from biology to subjective identity, which affects the foundational framing of sex-based protection systems.
% ABSENT_VOICES: Trans men largely absent from the public dispute, though the reading's logic applies symmetrically to them. Intersex and gender-non-binary persons are also largely absent, though some advocacy exists for three-or-more-category solutions. Cis women who benefit from sex-based protections but were not organized in the dispute are partially excluded from the boundary-setting process.
% DISAPPEARANCE_RATIONALE: If the identity reading disappeared and were replaced by biological criteria, legal recognition of trans women would collapse, sex-based service access would revert to biological boundaries, institutional policy would reverse, and social conflict would shift from questions of inclusion to questions of proof. The constraint shapes the entire structure of sex-based law and policy.
% FOUNDING_PROBLEM: Trans people lacked legal recognition, faced medical gatekeeping for identity verification, had no reliable pathway to change legal documentation, and experienced systematic exclusion from services and protections keyed to sex category. The constraint was built to solve identity recognition and reduce gatekeeping barriers.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocates and gender-self-determination organizations attest the founding problem is live and the reading solves it. Cis women's organizations and biology-reading advocates contest whether the founding problem justifies the boundary expansion or whether it creates new problems (loss of exclusive sex-based protections). Sex-based protection researchers and shelter administrators attest to operational complications arising from the expanded boundary. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval, indicating increasing asymmetry: early in implementation, the beneficiary gains (legal recognition, space access) feel coordinated and the costs to cis women diffuse; as the boundary becomes settled and enforcement intensifies, the costs become more concentrated and visible to the payer constituencies. Suppression rises from 0.54 to 0.72, tracking increasing enforcement infrastructure to manage space conflicts and respond to resistance. Theater rises modestly from 0.28 to 0.41, indicating that some share of enforcement effort becomes performative (affirmation language, symbolic gestures) rather than functional boundary management. Resistance is high and stable (0.74) throughout, indicating sustained opposition from cis women's organizations and biology-reading advocates. Accessibility for alternatives stays moderate (0.58) because biological criteria remain technically available (via hidden documentation, jurisdictional exit, institutional workarounds) but are socially and legally costly to invoke. The coercion grid shows dramatic level-specificity: individual-level suppression is consistently high (~0.72–0.78) because everyday space conflicts activate at person-level; class-level resistance is highest (~0.76–0.80) because organized constituencies (cis women, sex-based service users) can mobilize collectively. Structural-level metrics are lower because the law itself is formalized and does not require constant restatement. At the organizational level, stakes rise sharpest (0.55→0.64 in stakes_inflation) as institutions manage real operational complications.
 *
 * PERSPECTIVAL GAP:
 *   Trans women compute this as rope or even approaching mountain (a natural boundary recognition) from their seat because the constraint removes medical gatekeeping and affirms their category membership. Cis women compute it as tangled_rope or snare from their seat because it expands the category boundary and constrains their control over women's spaces without compensation. Sex-based-protection-dependent populations compute it as snare (coercive expansion of space boundaries reducing their safety options). Enforcement infrastructure is split: some administrators see rope (functional coordination, necessary legal duty), others see tangled_rope or snare (managing operational contradictions and daily conflicts). The engine's per-seat computation should reflect these divergences; the authored claim (tangled_rope) captures the structural reality that both coordination (trans recognition) and extraction (cis women's loss of exclusivity) are present and enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are structural beneficiaries (identity-locked exit; escape is identity-incompatible; high directionality toward beneficiary, d near 0.0 or negative). Cis women are structural payers (constrained exit; they remain in the category but with expanded membership; directionality shifted toward target, d near 0.6–0.7). Sex-based-protection-dependent populations are trapped payers (powerless; no exit; highest directionality toward target, d near 0.9). Gender-self-determination advocates are moderate beneficiaries (organized; mobile exit available; directionality toward beneficiary, d near 0.15–0.25). No directionality overrides needed; the derivation chain (beneficiary/victim + exit + power) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trans recognition, reduced gatekeeping) is live but contested. The constraint's persistence depends on active enforcement (institutional policy, legal documentation systems, space management) rather than on participant preference. A genuine rope would solve the problem and then relax—beneficiaries and payers would both accept the outcome. This constraint requires continuous enforcement against sustained resistance, which is the signature of tangled_rope: it coordinates trans recognition (a real function) while extracting exclusive access from cis women (asymmetric cost). The theater ratio rising slightly indicates some performative maintenance (affirmation language, symbolic policy revisions) but the constraint's core function remains operational. No mandatrophy signal yet, but if enforcement machinery were removed and the constraint persisted via theatrical maintenance alone, it would degrade into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_verification_circularity,
    'How is self-identified gender verified without reinstituting gatekeeping? Is mere declaration sufficient, or does enforcement require supporting documentation?',
    'Examine jurisdiction-level policy variations: where jurisdictions permit declaration without documentation versus where supporting letters or counselor attestation are required. Track whether documentation requirements gradually reintroduce gatekeeping pressures.',
    'If enforcement requires supporting documentation, the constraint reintroduces medical/psychological gatekeeping despite the reading''s claim of identity-only determination. If enforcement permits declaration alone, the constraint''s enforceability against strategic or bad-faith claims becomes contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_verification_circularity, empirical, 'Whether self-identification can be enforced without creating new gatekeeping mechanisms.').

omega_variable(
    exclusive_vs_shared_rights_framing,
    'Is the constraint better understood as expanding the beneficiary set (trans women added to existing protections) or as creating shared access to category-keyed resources where shared access was not previously the norm?',
    'Distinguish empirically: if sex-based protections can accommodate shared category membership without architectural change, it is expansion; if shared membership requires redesigning shelters, prisons, or facilities, it is a foundational shift in the resource model.',
    'If expansion, the extraction narrative (cis women losing exclusive access) is partially inaccurate and the constraint might be more rope-like than tangled_rope. If foundational shift, the constraint genuinely extracts cis women''s previous resource allocation and is more extractive than the expansion frame suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusive_vs_shared_rights_framing, conceptual, 'Whether the constraint adds beneficiaries to existing resources or transforms the resource-sharing model.').

omega_variable(
    sex_based_vs_gender_based_protection_distinction,
    'Can sex-based protections (keyed to biological sex) coexist with gender-based protections (keyed to gender identity category) in the same legal system, or does adopting the identity reading require replacing all sex-based protections with gender-based ones?',
    'Examine jurisdictions that have formally adopted the identity reading: do they maintain separate sex-based protections or consolidate into gender-based frameworks? Track legal reinterpretation of existing sex-discrimination statutes.',
    'If sex-based and gender-based protections can coexist, the constraint''s extraction is reduced because the payer constituencies (sex-based-protection-dependent populations) retain some protections. If the reading requires replacing sex-based with gender-based, extraction increases because the payer constituencies lose the foundation of their protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_vs_gender_based_protection_distinction, empirical, 'Whether the identity reading is compatible with maintaining separate sex-based legal protections.').

omega_variable(
    space_conflict_as_mechanism,
    'Are the high suppression and resistance metrics driven primarily by trans women seeking space access or by cis women''s resistance to inclusion? Is the suppression mechanism structural enforcement of the boundary or enforcement of silence about the conflict?',
    'Examine enforcement patterns: are most enforcement actions Trans women entering spaces (positive enforcement) or cis women''s complaints being suppressed (negative enforcement)? Track narrative control: who is allowed to speak about conflicts in public forums?',
    'If suppression is positive enforcement (trans women''s access actively defended), the constraint is maintaining its functional coordination. If suppression is silencing (cis women''s concerns suppressed), the constraint is degrading toward snare (pure extraction without coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_conflict_as_mechanism, empirical, 'Whether suppression enforces the constraint''s coordination function or suppresses the conflict it generates.').

omega_variable(
    kernel_reading_vs_contingent_policy_distinction,
    'Is this constraint instantiating a kernel reading (a fundamental commitment about what determines category membership) or a contingent policy choice (one jurisdiction''s administrative decision that could be reversed without touching foundational law)?',
    'Examine constitutional/statutory grounding: is the identity criterion embedded in constitutional definitions of sex, in statutory sex-discrimination law, in administrative policy, or in case law? Track whether reversing the constraint would require constitutional amendment versus administrative rule change.',
    'If a kernel reading, the constraint carries the full weight of foundational disagreement with the biology and hybrid readings. If a contingent policy, it is more vulnerable to reversal and may not trigger the same level of enforcement stability. Classification boundary shifts accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_contingent_policy_distinction, conceptual, 'Whether the constraint is a kernel-level commitment or a high-level policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__identity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(sex__tr_t5, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(sex__tr_t15, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(sex__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__identity_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(sex__be_t5, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__identity_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(sex__be_t15, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__identity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(sex__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__identity_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(sex__su_t5, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__identity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(sex__su_t15, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__identity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(sex__su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(sex__grid_01, sex_gender_category__identity_reading, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(sex__grid_02, sex_gender_category__identity_reading, accessibility_collapse(class), 25, 0.7).
narrative_ontology:measurement(sex__grid_03, sex_gender_category__identity_reading, accessibility_collapse(individual), 0, 0.71).
narrative_ontology:measurement(sex__grid_04, sex_gender_category__identity_reading, accessibility_collapse(individual), 25, 0.76).
narrative_ontology:measurement(sex__grid_05, sex_gender_category__identity_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(sex__grid_06, sex_gender_category__identity_reading, accessibility_collapse(organizational), 25, 0.58).
narrative_ontology:measurement(sex__grid_07, sex_gender_category__identity_reading, accessibility_collapse(structural), 0, 0.35).
narrative_ontology:measurement(sex__grid_08, sex_gender_category__identity_reading, accessibility_collapse(structural), 25, 0.42).
narrative_ontology:measurement(sex__grid_09, sex_gender_category__identity_reading, resistance(class), 0, 0.76).
narrative_ontology:measurement(sex__grid_10, sex_gender_category__identity_reading, resistance(class), 25, 0.8).
narrative_ontology:measurement(sex__grid_11, sex_gender_category__identity_reading, resistance(individual), 0, 0.78).
narrative_ontology:measurement(sex__grid_12, sex_gender_category__identity_reading, resistance(individual), 25, 0.82).
narrative_ontology:measurement(sex__grid_13, sex_gender_category__identity_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(sex__grid_14, sex_gender_category__identity_reading, resistance(organizational), 25, 0.75).
narrative_ontology:measurement(sex__grid_15, sex_gender_category__identity_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(sex__grid_16, sex_gender_category__identity_reading, resistance(structural), 25, 0.64).
narrative_ontology:measurement(sex__grid_17, sex_gender_category__identity_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(sex__grid_18, sex_gender_category__identity_reading, stakes_inflation(class), 25, 0.75).
narrative_ontology:measurement(sex__grid_19, sex_gender_category__identity_reading, stakes_inflation(individual), 0, 0.74).
narrative_ontology:measurement(sex__grid_20, sex_gender_category__identity_reading, stakes_inflation(individual), 25, 0.79).
narrative_ontology:measurement(sex__grid_21, sex_gender_category__identity_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(sex__grid_22, sex_gender_category__identity_reading, stakes_inflation(organizational), 25, 0.64).
narrative_ontology:measurement(sex__grid_23, sex_gender_category__identity_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(sex__grid_24, sex_gender_category__identity_reading, stakes_inflation(structural), 25, 0.51).
narrative_ontology:measurement(sex__grid_25, sex_gender_category__identity_reading, suppression(class), 0, 0.65).
narrative_ontology:measurement(sex__grid_26, sex_gender_category__identity_reading, suppression(class), 25, 0.73).
narrative_ontology:measurement(sex__grid_27, sex_gender_category__identity_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement(sex__grid_28, sex_gender_category__identity_reading, suppression(individual), 25, 0.78).
narrative_ontology:measurement(sex__grid_29, sex_gender_category__identity_reading, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(sex__grid_30, sex_gender_category__identity_reading, suppression(organizational), 25, 0.62).
narrative_ontology:measurement(sex__grid_31, sex_gender_category__identity_reading, suppression(structural), 0, 0.38).
narrative_ontology:measurement(sex__grid_32, sex_gender_category__identity_reading, suppression(structural), 25, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel has three ε-invariant readings, each a separate constraint story: biology_reading (membership by reproductive biology), hybrid_reading (membership by biology + medical gatekeeping), identity_reading (this story—membership by self-identification). The readings are structurally distinct (different ε values, different beneficiary/victim sets, different enforcement mechanisms) and should not be combined into one story with a measurement parameter. Each reading affects the others via the network: the identity_reading's adoption constrains the plausibility of the biology_reading and creates institutional pressure on the hybrid_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
