% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Sex/Gender Category Membership by Identity Self-Identification
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   The identity reading of sex/gender categorization holds that membership
 *   in the category 'woman' is determined solely by subjective gender
 *   identity, without medical gatekeeping or biological prerequisites. Under
 *   this reading, trans women are women; institutional spaces coded as
 *   female-only become accessible to anyone declaring female identity; legal
 *   documents reflect stated identity without proof. This reading directly
 *   competes with the biology reading (sex category anchored in reproductive
 *   biology) and the hybrid reading (medical transition as requirement). The
 *   identity reading benefits trans women by eliminating gatekeeping and
 *   recognizing their identity; it imposes costs on cis women by making the
 *   category 'woman' non-exclusive and undermining sex-based protections; it
 *   benefits gender identity advocates by advancing their framing; and it
 *   imposes costs on sex-based rights advocates by treating their advocacy as
 *   hostile. The constraint is substantially extractive because the benefits
 *   to trans women and advocates are real but concentrated, while the costs
 *   to cis women are diffuse but substantial (loss of exclusive category,
 *   shared spaces, narrowed scope for sex-based organizing). The
 *   extractiveness increases over the measured interval as institutional
 *   adoption spreads.
 *
 * KEY AGENTS:
 *   - trans_women: primary beneficiary (identity recognition, space access, gatekeeping elimination) — moderate power, identity-locked exit
 *   - cis_women: primary payer (exclusive category loss, shared space, sex-based protection erosion) — organized power, constrained exit
 *   - gender_identity_advocates: beneficiary coalition (policy legitimacy, institutional framing adoption) — organized power, mobile exit
 *   - sex_based_rights_advocates: payer coalition (framing marginalized, advocacy legible as bigotry) — organized power, constrained exit
 *   - institutional_policy_makers: agenda setter (adopt and enforce identity rules) — institutional power, constrained exit once adopted
 *   - excluded_sex_essentialism_advocates: outside the conversation (objections treated as transphobic, not substantive) — moderate power, constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.71).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.49).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Sex/Gender Category Membership by Identity Self-Identification").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '6617a36a-ea8b-4555-bd98-a2d9a45a0447').
narrative_ontology:cs_kernel_codification('6617a36a-ea8b-4555-bd98-a2d9a45a0447', distributed).
narrative_ontology:cs_authority_grounding('6617a36a-ea8b-4555-bd98-a2d9a45a0447', extraction).
narrative_ontology:cs_reading_relation('6617a36a-ea8b-4555-bd98-a2d9a45a0447', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('6617a36a-ea8b-4555-bd98-a2d9a45a0447', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('6617a36a-ea8b-4555-bd98-a2d9a45a0447', foundational, gender_identity_is_ontologically_primary).
narrative_ontology:cs_axiom_status(gender_identity_is_ontologically_primary, holdable).
narrative_ontology:cs_axiom_grounding('6617a36a-ea8b-4555-bd98-a2d9a45a0447', gender_identity_is_ontologically_primary, deontological).
narrative_ontology:cs_axiom('6617a36a-ea8b-4555-bd98-a2d9a45a0447', foundational, medical_gatekeeping_is_illegitimate_access_barrier).
narrative_ontology:cs_axiom_status(medical_gatekeeping_is_illegitimate_access_barrier, holdable).
narrative_ontology:cs_axiom_grounding('6617a36a-ea8b-4555-bd98-a2d9a45a0447', medical_gatekeeping_is_illegitimate_access_barrier, empirically_contingent).
narrative_ontology:cs_reference_frame('6617a36a-ea8b-4555-bd98-a2d9a45a0447', gender_identity_legal_recognition).
narrative_ontology:cs_drift_state('6617a36a-ea8b-4555-bd98-a2d9a45a0447', institutional_adoption_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6617a36a-ea8b-4555-bd98-a2d9a45a0447', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, sex_based_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, trans_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain formal recognition and legal status as women through identity declaration alone, without medical requirements or gatekeeping. Access women's spaces (bathrooms, shelters, prisons, sports) based on stated identity. Avoid outing or disclosure requirements that create safety risks. Their exit from this category is psychologically and socially costly — the framework affirms their self-understanding as women.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Lose the exclusive category 'woman' and the sex-based protections anchored to biological materiality (exclusion from male violence, male space intrusion, reproductive autonomy). Share women's spaces with trans women, including bathrooms, shelters, prisons, sports, locker rooms. The boundary of the category expands beyond their control; they cannot exit the category (natal sex is fixed) but the category's scope is contested. Sex-based organizing and advocacy becomes legible as transphobic rather than as self-defense.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, biographical, constrained, national).

% Advocate for sex-based (not gender-identity-based) rights, arguing that the biological realities of sex should remain the basis for protections addressing male violence, reproductive autonomy, and female-only space. Their framing of 'woman' as a sex-based category is treated as hostile or exclusionary under the identity reading; institutional and social pressure narrows the space for this advocacy. They remain in the debate but at higher speaking cost.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, sex_based_rights_advocates, payer,
    organized, biographical, constrained, national).

% Advance the principle that gender identity should be the organizing axis for sex/gender categories in law and institutional practice. Gain institutional legitimacy and policy wins (name changes, document recognition, space access, sports eligibility rules). Their exit from this advocacy is feasible but carries reputational and career costs for institutional actors and professionals committed to the framework.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_advocates, beneficiary,
    organized, biographical, mobile, national).

% Adopt and enforce the identity reading in institutional rules: adopt legal name changes without medical gatekeeping, revise bathroom/shelter/prison/sports eligibility to require identity declaration only, rewrite anti-discrimination law to protect gender identity. Face ongoing legal challenges, legislative pressure to reverse, and operational costs of managing conflicting space-use demands. Cannot easily exit once adopted; institutional inertia locks them in.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, institutional_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Argue that sex (biological category) is immutable and ontologically prior, and that gender identity cannot override sex-based classification. Are increasingly absent from institutional conversations, labeled as transphobic, and face professional consequences for public advocacy. Their objections to the identity reading are treated as bigotry rather than as substantive claims about category ontology.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, excluded_sex_essentialism_advocates, excluded,
    moderate, biographical, constrained, national).

% Gain formal recognition as men through identity declaration, with corresponding access to male-coded spaces and legal status. Simultaneously lose access to women-only protections and resources (domestic violence shelters, women's health services, reproductive rights framing) if they exit the 'woman' category. The constraint creates gains for their male identity recognition but costs for their biological reproductive reality.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, trans_men, payer).

% Adjudicate disputes over space access, rights protections, and legal category membership as the identity reading spreads. Courts, human rights commissions, and executive agencies have conflicting mandates (gender identity protection vs. sex-based rights). Their institutional position is to interpret the constraint operationally even as its legitimacy is contested.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, gender_identity_advocates).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified legal and social category 'woman' that includes trans women, reducing the requirement for parallel legal schemas and simplifying recognition procedures; eliminates medical gatekeeping requirements that created barriers and surveillance for trans people; coordinates institutional practice (bathrooms, shelters, legal documents) under a single principle (self-identification).
% TRANSFER_FUNCTION: Transfers definitional power from biological criteria to subjective identity declaration; shifts the burden of proof from trans women (proving medical transition) to sex-based rights advocates (proving that biological sex remains relevant); moves exclusive access to 'woman' category and women's protections from cis women as a group to all who declare female gender identity.
% ABSENT_VOICES: Sex-essentialism advocates are increasingly structurally excluded from institutional policymaking; their objections are categorized as bigotry rather than as substantive claims about category boundary. Cis women without institutional platforms are also partly absent: working-class and racialized women experience space-access costs and safety concerns that elite-led institutional adoption of the identity reading does not address directly.
% DISAPPEARANCE_RATIONALE: Sex-based advocates argue the world would rearrange back to sex-based categories if identity-based classification disappeared, because the biological realities of reproduction, male violence, and physical difference would reassert; they claim this is restoration of natural/prior order. Identity-based advocates argue the world would still acknowledge trans people's identity and would face pressure to recognize them under some framework, because trans identity itself would not disappear. The disappearance itself is contested as either restoration or loss depending on one's reading of what 'natural' or 'prior' order is.
% FOUNDING_PROBLEM: Trans people faced legal non-recognition, medical gatekeeping (required psychiatric diagnosis, surgical proof, years of transition), and exclusion from legal categories matching their lived identity. Cis women needed protection from male violence and reproductive exploitation anchored in biological sex. These two problems are read by different seats as either compatible or fundamentally opposed.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocates and human rights organizations document medical gatekeeping burdens and legal non-recognition costs in historical and contemporary accounts. Cis women advocates and sex-based rights organizations document male violence patterns, reproductive harms, and the erosion of sex-based protections as a consequence. No independent external corroboration exists that both problems have the same solution; each side's account of the other side's problem is dismissed as inaccurate.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, contested).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness measurement (0.68 at interval end) reflects the asymmetric distribution of gains and costs: trans women and advocates benefit substantially; cis women and sex-based advocates bear substantial costs; the constraint persists because institutional adoption spreads (policy wins accumulate), but resistance remains high (0.73) because the cost-bearing groups actively contest the boundary redefinition. Suppression is substantial (0.71) because the constraint's persistence depends on actively treating sex-based objections as bigotry (institutional norm-setting, professional consequences for dissent, content moderation) — the enforced suppression is the mechanism by which the identity reading spreads, not external constraint. Theater is moderate (0.42): institutions deploy language about 'inclusion,' 'safety,' and 'rights,' but the actual operational effect is boundary renegotiation that reallocates access to spaces and protections; the gap between the stated purpose (inclusion/safety) and the structural effect (exclusive-category loss) grows over the interval as adoption spreads. Accessibility collapse is low-moderate (0.49) because alternatives to the identity reading remain available (biology reading, hybrid reading) and are held by substantial constituencies; the identity reading does not foreclose alternatives, but institutional pressure narrows the legitimacy space for voicing them. The measurement trajectory shows extractiveness rising as institutional adoption spreads (t=0 to t=20), with theater rising more gradually and suppression rising steadily — the pattern suggests the constraint is becoming more entrenched and more dependent on active suppression of the competing reading.
 *
 * PERSPECTIVAL GAP:
 *   From the trans women/advocates seat, the identity reading is a liberation constraint — it eliminates medical gatekeeping, affirms identity, and secures legal recognition. From the cis women/sex-based advocates seat, the identity reading is an extraction constraint — it redefines their category without consent, reallocates access to protected spaces, and narrows the scope for sex-based organizing. From the institutional policy-maker seat, the identity reading is a coordination problem: it simplifies legal procedure (one criterion: identity declaration) but creates operational costs (managing space-access conflicts, navigating legal challenges, mediating between constituencies). The seats compute different types because they face genuinely different structural positions: beneficiaries get affirmation; payers get dilution of category and loss of exclusive protections; the agenda-setter gets policy adoption but institutional friction. This divergence is NOT an error — it is the expected structure of a tangled-rope constraint: genuine coordination benefit (elimination of gatekeeping) bundled with asymmetric extraction (exclusive-category loss).
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and advocates are beneficiaries: d approaches 0.2–0.3 (low directionality toward cost). Cis women and sex-based advocates are payers: d approaches 0.7–0.8 (high directionality toward extraction). Institutional policy-makers sit near 0.5 (symmetric): they benefit from procedural simplification but bear institutional friction. Regulatory authorities are analytical (d=0.5, no stake). The beneficiary/victim declarations feed the directionality derivation: trans women gain legal recognition without cost (low d); cis women lose exclusive category without compensation and cannot exit sex itself (high d); advocates have organizational choice and career mobility (lower d for advocates than for the populations they advocate for). The identity-locked exit for trans women is structural: transitioning gender identity is not optional, so even though the identity reading benefits them, they cannot refuse it without betraying their identity — the constraint captures identity itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trans people face non-recognition and medical gatekeeping) is LIVE for trans people and DEAD for cis women (who did not create that problem and do not benefit from its solution). The founding-problem status is CONTESTED because the two seats disagree on whether the identity reading solves one problem or creates another. The identity reading solves a real coordination problem (elimination of medical gatekeeping simplifies legal procedure), but the coordination bundle includes substantial asymmetric extraction (exclusive-category loss). This is the canonical tangled-rope structure: the coordination function is real (gatekeeping elimination) and the extraction is real (category dilution), and they ride on the same mechanism. No party can separate them without institutional redesign. The mandatrophy risk is moderate: the founding problem could become dead (trans recognition achieves cultural saturation, gatekeeping is eliminated) while the extraction persists (sex-based protections remain eroded). At that point the identity reading becomes a pure extraction mechanism — a snare masquerading as coordination. The measurement trajectory (rising extractiveness, stable theater) suggests the constraint is moving toward that state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_verification_mechanism,
    'Under the identity reading, what mechanism verifies gender identity declaration? Is any verification necessary, or is all verification treated as gatekeeping?',
    'Operational observation of institutional practice: do institutions ask any questions, require any documentation, permit reversals, or accept all declarations as valid without further inquiry?',
    'If verification is required, the constraint carries hidden gatekeeping machinery and becomes more snare-like (extraction mechanism disguised as identity recognition). If verification is absent, the constraint is more purely coordinating (identity declaration alone) but faces conflict over space access (how to manage the unknown-identity problem in bathrooms/shelters). The extraction shifts from gatekeeping burden to space-access conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_verification_mechanism, empirical, 'Whether ''identity declaration'' has gatekeeping buried inside it.').

omega_variable(
    sex_based_rights_separability,
    'Are sex-based protections (exclusion from male violence, reproductive autonomy, female-only space) structurally inseparable from ''woman'' as a legal category, or can they be protected through a parallel sex-based mechanism?',
    'Legislative or institutional redesign creating parallel protections: e.g., legal category ''woman'' includes trans women (identity-based), while ''female sex'' remains a protected axis for reproduction, male-violence exclusion, and female-only spaces. If both can coexist without category collapse, the identity reading''s extraction is reduced.',
    'If separable, the identity reading becomes a pure rope (genuine coordination benefit without extraction). If inseparable, the reading is tangled-rope or snare (extraction is necessary to the coordination). Separability also depends on political will: even if technically feasible, institutional and political pressure may make parallel protection unthinkable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_rights_separability, conceptual, 'Whether sex-based and identity-based category systems can coexist without zero-sum conflict.').

omega_variable(
    cis_women_consent_boundary,
    'Is the loss of exclusive ''woman'' category membership a cost imposed on cis women without consent, or a renegotiation that cis women have the power to refuse?',
    'Political process: does cis-women-only coalitional power exist to maintain category exclusivity, or are institutional and social forces arrayed such that consent is not meaningfully available? Comparative analysis of jurisdictions where identity-based classification was adopted over explicit cis-women''s objection vs. with their explicit agreement.',
    'If cis women have genuine exit/refusal power and choose to accept the boundary renegotiation, the constraint is less extractive (cooperative boundary shift). If refusal power does not exist (institutional/social forces override their objection), the constraint is more extractive (imposed boundary dilution). The extraction is partly a function of voice and choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_consent_boundary, empirical, 'Whether the category renegotiation carries cis-women consent or imposes on them without it.').

omega_variable(
    reading_boundary_foreclosure,
    'Does the identity reading logically foreclose the biology reading, or can both coexist as legitimate framings in different institutional contexts?',
    'Theoretical analysis: if identity and biology are ontologically distinct (one psychological, one material) and both are true simultaneously (a trans woman has female identity AND male reproductive biology), can law acknowledge both without contradiction? Or does one reading necessarily erase the other?',
    'If foreclosure is unavoidable (one reading necessarily eliminates the other''s truth-claims), the identity reading is a replacement, not an addition, and the cost to biology-reading advocates is total. If coexistence is possible, the constraint is more purely a boundary renegotiation (institutional pluralism) than a zero-sum replacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_foreclosure, conceptual, 'Whether the identity and biology readings are logically incompatible or can coexist in institutional pluralism.').

omega_variable(
    space_access_conflict_cost,
    'What is the actual incidence and cost of space-access conflicts (bathrooms, shelters, prisons, sports) when the identity reading is operationalized? Is it a rare edge case or a systematic design problem?',
    'Empirical observation: data from jurisdictions with identity-based access rules on incident frequency, complaint rates, resolution mechanisms, and safety outcomes in high-conflict spaces (women''s shelters, prisons). Comparative analysis with baseline data from pre-identity-reading contexts.',
    'If conflicts are rare and resolvable, the extractiveness estimate is overestimated (theater accounts for conflict avoidance). If conflicts are systematic and unresolved, the extractiveness is underestimated and the constraint may be sliding toward snare-type operation (the conflict becomes the suppression mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_access_conflict_cost, empirical, 'Whether space-access conflict is a marginal cost or a systematic design failure of the identity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sex__tr_t3, sex_gender_category__identity_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(sex__tr_t6, sex_gender_category__identity_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex__be_t3, sex_gender_category__identity_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sex__be_t6, sex_gender_category__identity_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__identity_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(sex__su_t3, sex_gender_category__identity_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(sex__su_t6, sex_gender_category__identity_reading, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__identity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel decomposes into three structurally distinct constraint stories: the identity_reading (this file, high extractiveness, identity-coordination type), the biology_reading (low extractiveness, mountain-adjacent, natural-law framing), and the hybrid_reading (moderate extractiveness, enforcement-mechanism type, medical gatekeeping). Each reading has a different ε, different beneficiary/victim structure, and different justification narrative. The three stories are linked by this affects_constraints relation to signal kernel kinship. The identity reading influences the other two by setting institutional policy precedent and by reframing category boundary in ways that change the operating environment for biology and hybrid readings. See commentary.kernel_context for the decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
