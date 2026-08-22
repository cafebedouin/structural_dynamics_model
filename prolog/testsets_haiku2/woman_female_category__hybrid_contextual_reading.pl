% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Category Membership: Sex/Gender Boundary
 *   domain: political_philosophy/bioethics/law
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'woman/female category': the hybrid contextual reading, which holds that
 *   category membership varies by domain—biological sex for medical,
 *   reproductive, and sports contexts; gender identity for legal recognition
 *   and social inclusion. This reading differs structurally from the sibling
 *   sex-biology reading (category = chromosomal/reproductive sex always) and
 *   the gender-identity reading (category = self-identification always) by
 *   claiming both readings are partially correct, each in its proper domain.
 *   The extraction measured here is the cost imposed on trans people and
 *   gender-identity/sex-category advocates who navigate contradictory
 *   category rules across institutional domains. The beneficiary is the set
 *   of institutional actors (medical systems, sports federations, legal
 *   systems, legislatures) that use the hybrid reading to appear
 *   compromise-oriented while deferring hard category choices to
 *   domain-specific rules, thereby shifting classification burden to subject
 *   populations.
 *
 * KEY AGENTS:
 *   - Institutional conflict-minimizers (hospital systems, sports federations, legislatures, courts) — agenda setters who set and enforce category rules
 *   - Trans women in medical contexts — classified by sex category despite gender identity, pay the cost of institutional compartmentalization
 *   - Trans men in legal contexts — classified by gender identity, excluded from sex-category legal recognition when required, navigate documentation conflicts
 *   - Sex-category advocates (political and intellectual actors) — beneficiaries in medical/sports domains, payers in legal/social domains
 *   - Gender-identity advocates (political and intellectual actors) — beneficiaries in legal/social domains, payers in medical/sports domains
 *   - Medical practitioners — observers navigating contradictory documentation and clinical workflow ambiguity
 *   - Sports governance bodies — observers implementing domain-specific sex-category rules with uncertain measurement standards
 *   - Nonbinary and intersex people — excluded from positive category membership under both readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.58).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.62).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Category Membership: Sex/Gender Boundary").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, 'ed583983-e0ed-4ccf-8186-3935974f28eb').
narrative_ontology:cs_kernel_codification('ed583983-e0ed-4ccf-8186-3935974f28eb', distributed).
narrative_ontology:cs_authority_grounding('ed583983-e0ed-4ccf-8186-3935974f28eb', distributed).
narrative_ontology:cs_reading_relation('ed583983-e0ed-4ccf-8186-3935974f28eb', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed583983-e0ed-4ccf-8186-3935974f28eb', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('ed583983-e0ed-4ccf-8186-3935974f28eb', foundational, category_membership_context_dependent).
narrative_ontology:cs_axiom_status(category_membership_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('ed583983-e0ed-4ccf-8186-3935974f28eb', category_membership_context_dependent, conventional).
narrative_ontology:cs_axiom('ed583983-e0ed-4ccf-8186-3935974f28eb', foundational, institutional_domain_specificity_coherent).
narrative_ontology:cs_axiom_status(institutional_domain_specificity_coherent, holdable).
narrative_ontology:cs_axiom_grounding('ed583983-e0ed-4ccf-8186-3935974f28eb', institutional_domain_specificity_coherent, empirically_contingent).
narrative_ontology:cs_reference_frame('ed583983-e0ed-4ccf-8186-3935974f28eb', institutional_compartmentalization).
narrative_ontology:cs_drift_state('ed583983-e0ed-4ccf-8186-3935974f28eb', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ed583983-e0ed-4ccf-8186-3935974f28eb', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, domain_gatekeepers).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_medical_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_men_legal_recognition).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_category_advocates).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_identity_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sex_category_advocates).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, gender_identity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations (hospitals, sports federations, legislatures, courts) that set and enforce category membership rules. They adopt the hybrid contextual framework claiming it balances safety, fairness, and inclusion across multiple domains. They collect legitimacy from appearing compromise-oriented while actually deferring hard choices to domain-specific rules, shifting the burden of proof and classification to the subjects.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers, agenda_setter,
    institutional, generational, arbitrage, national).

% In medical contexts (gynecology, sex-specific disease screening, reproductive endocrinology), are classified by sex (therefore excluded from 'woman' category) despite gender identity. Must either accept sex-category treatment that conflicts with their identity, seek specialized providers at high cost, or forgo care. Exit options are limited by insurance coverage and provider availability.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_medical_contexts, payer,
    moderate, biographical, constrained, national).

% In legal recognition contexts (identification documents, marriage law, sex-segregated institutional access), are classified by gender identity (therefore excluded from 'man' category when legal systems retain sex-category requirements). Must navigate mismatches between their legal gender and sex-category requirements in some jurisdictions, or accept institutional barriers to legal recognition. Trapped by jurisdictional variation and documentation requirements.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_men_legal_recognition, payer,
    moderate, biographical, constrained, national).

% Political and intellectual actors arguing that woman/female must be anchored in biological sex for coherence in medical, sports, and reproductive policy. They pay the cost of being positioned as exclusionary in gender-recognition contexts while benefiting from institutional adoption of sex-category definitions in their preferred domains. Have institutional platforms and funding.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_category_advocates, payer,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sex_category_advocates, beneficiary).

% Political and intellectual actors arguing that woman/female must be anchored in gender identity for consistency in social and legal recognition. They pay the cost of being positioned as erasing sex-based category concerns in medical and sports contexts while benefiting from institutional adoption of gender-identity definitions in their preferred domains. Have institutional platforms and funding.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_advocates, payer,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, gender_identity_advocates, beneficiary).

% Clinicians and medical organizations that must implement category rules when treating patients. The hybrid contextual framework creates documentation and clinical workflow ambiguity: they must navigate category rules that differ from legal documents, manage patient self-identification alongside medical assessments, and document sex-category data for epidemiological purposes while respecting gender identity for social purposes.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, medical_practitioners, observer,
    institutional, biographical, analytical, national).

% Federations and leagues enforcing eligibility categories. The hybrid framework delegates the category boundary to biological sex in sports on fairness/safety grounds, creating pressure to define and measure sex category (testosterone, chromosomes, reproductive anatomy) while other institutional domains use gender identity. Must navigate athlete self-identification, medical transition status, and testing protocols without clear consensus on causal mechanisms.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governance_bodies, observer,
    institutional, generational, analytical, global).

% Courts and legislatures that must apply category rules in marriage, family law, sex-segregated institutional access, and anti-discrimination law. The hybrid framework creates jurisdictional variation and appeals: some legal systems recognize the gender-identity reading (category = gender identity), others the sex-biology reading (category = chromosomal/reproductive sex), and still others attempt the hybrid itself. Enforcement inconsistency is structural.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, legal_systems, observer,
    institutional, generational, analytical, national).

% Employers, educational institutions, housing providers, and other organizations that implement category rules for social and legal purposes. The hybrid framework creates policy ambiguity: they must decide whether to use legal gender identity (accessible via document change in most jurisdictions) or sex category (accessed via medical history or undisclosed attributes) for bathrooms, hiring, housing, and record-keeping.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, social_recognition_gatekeepers, observer,
    institutional, biographical, analytical, national).

% People who do not identify with either pole of the woman/man binary (nonbinary, agender, genderqueer individuals) and biological sex categories that don't fit the binary (intersex people whose chromosomal, anatomical, or hormonal profiles don't align with XX or XY definitions). The hybrid contextual framework provides no positive category membership for them; they are structurally excluded from both reading's attempted solutions.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, excluded_category_agents, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves institutional conflict by deferring category membership to domain-specific rules: biological sex for medical risk assessment and sports fair-play assurance, gender identity for legal recognition and social inclusion. Avoids requiring a single unified definition of woman/female that would inevitably privilege one reading's core claim.
% TRANSFER_FUNCTION: Moves institutional legitimacy and operational discretion from standardized, unified category rules to domain-specific definitions. Institutions claiming to balance competing values transfer the cost of classification inconsistency to subject populations (trans people navigate contradictory category rules across contexts).
% ABSENT_VOICES: People who operate outside the woman/man binary—nonbinary and genderqueer individuals—have no category membership under either pole and are excluded from the negotiation. Intersex people whose biological traits don't align with binary sex categories are similarly excluded from both readings. These groups would argue for positive category membership or explicit non-binary recognition, but institutional processes have historically centered cisgender people in both sex-category and gender-identity frameworks.
% DISAPPEARANCE_RATIONALE: The hybrid contextual rule is the mechanism holding medical systems, sports governance, legal systems, and social institutions together in the face of a genuine category dispute. If it vanished, institutions would collapse back into choosing a single reading (all sex-category or all gender-identity), triggering regulatory realignment, litigation waves, and institutional restructuring across healthcare, athletics, legal recognition, and social policy.
% FOUNDING_PROBLEM: From the early 2010s onward, rising numbers of transgender people seeking legal and social recognition created jurisdictional conflict: medical systems operating on biological sex categories (for reproductive risk assessment, sex-specific disease screening) faced legal systems adopting gender-identity categories (for identification documents, anti-discrimination law). A unified category definition that satisfied both proved impossible; the hybrid contextual reading emerged as an institutional attempt to satisfy both domains simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Medical organizations (WHO, AMA) attest that sex-category data remains essential for disease screening and hormonal assessment. Legal and human-rights advocates attest that gender identity recognition is essential for legal equality and social inclusion. Sports bodies attest that sex-category criteria are necessary for fair competition. No party outside the institutional consensus attests that the founding problem has been solved; trans people and nonbinary people continue to report category-related institutional barriers and documentation conflicts.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.58) because the constraint's primary extraction is the displacement of classification burden to subject populations; it is not as extractive as pure capture (snare) because institutional actors genuinely face coordination pressure from conflicting domain needs. Suppression is moderately high (0.62) because the constraint requires active enforcement of domain-specific boundary rules and suppression of cross-domain category consistency demands—subjects are discouraged from asking why category membership changes across contexts. Theater_ratio is rising (0.25→0.51 over the interval) because institutional commitment to the hybrid reading as genuine compromise is weakening as domain conflicts become more salient: institutions increasingly invoke 'balanced approach' language while narrowing gender-identity recognition in legal contexts and tightening sex-category enforcement in medical contexts. Accessibility_collapse is moderate-low (0.44) because alternatives remain conceptually accessible—the sex-biology reading and gender-identity reading both have coherent institutional defenders and are not suppressed as heresy. Resistance is high (0.71) because both sex-category advocates and gender-identity advocates actively resist the hybrid reading and push for their preferred single reading; the resistance is organized and funded.
 *
 * PERSPECTIVAL GAP:
 *   The institutional conflict-minimizers (agenda-setters) perceive the hybrid reading as genuine compromise and coordination achievement—they genuinely face pressure from medical systems requiring sex category data and legal systems requiring gender-identity recognition. From their seat, the constraint solves a real coordination problem. Trans people and activists perceive the hybrid reading as institutional cost-shifting—a performance of balance that actually imposes contradictory classification demands on those who navigate multiple institutional contexts. From their seats (payers), the constraint extracts without coordinating. The engine computes these divergent classifications from the structural data: the agenda-setter seat benefits from the arrangement (institutional legitimacy, operational flexibility) and has arbitrage exit options (can leave implementation to subordinate institutions), deriving low d and therefore lower per-seat extraction. The trans payer seats face constrained exit options and bear the costs of category contradictions, deriving high d and higher per-seat extraction. The political advocate seats (sex-category and gender-identity) are powerful and mobile, benefiting in their preferred domains while paying in others—dual-role stakeholders with more balanced d.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional conflict-minimizers (agenda-setters) derive d = 0.2 (near beneficiary): they set the rules, collect institutional legitimacy, have arbitrage exit options (can shift implementation burden), and benefit from the appearance of compromise. Trans women in medical contexts derive d = 0.85 (near target): they must navigate sex-category classifications that contradict their legal and social gender identity, have constrained exit options (cannot easily switch to gender-identity-affirming medical systems in most jurisdictions), and bear the cost of institutional compartmentalization. Trans men in legal recognition derive d = 0.80: they face legal systems that subordinate their sex-category to gender-identity-based recognition demands when legal systems attempt to harmonize on gender identity, opposite to trans women's cost in medical contexts. Sex-category and gender-identity advocates derive d = 0.55 (symmetric): they benefit in their preferred domains and pay in others, with mobile exit options (can shift institutional affiliation or funding). Medical practitioners and sports bodies derive d = 0.50 (symmetric): they face genuine coordination pressure from conflicting domain needs and have analytical rather than material stakes. The directionality variation drives the per-seat type divergence: institutional actors compute the constraint as tangled_rope (genuine coordination, moderate extraction); trans payers compute it as closer to snare (exploitation with institutional coordination cover); advocates compute it as constrained rope (cooperation within domains, extraction across domains).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint displays mandatrophy candidacy (founding problem status = contested, disappearance verdict = world_rearranges). The founding problem was genuine: medical systems and legal systems did face category-membership conflicts in the 2010s. But attestation of current problem-status comes only from institutional gatekeepers; trans people and gender-identity/sex-category advocates attest the founding problem is partially solved but the constraint persists as institutional cost-shifting. The rising theater_ratio (0.25→0.51) suggests the hybrid reading itself is increasingly performative: institutional rhetoric claims domain-specificity and balance, but enforcement increasingly privileges sex-category in medical/sports contexts and gender identity in legal/social contexts without acknowledging the asymmetry. This tracks the mandatrophy pattern—a coordination claim that solved a real problem initially (medical and legal systems did need to coexist) but now persists as institutional theater and extraction covering narrowed recognition and tightened boundaries. The three omegas on domain boundary construction, sex-category measurement instability, and institutional capture mechanism all address mandatrophy resolution: if the domain boundaries are constructed (not natural), if sex-category measurement is unstable (not medically sound), or if the hybrid reading is institutional capture rather than genuine compromise, the constraint reclassifies toward piton (inertial performance) and mandatrophy is confirmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_determinism,
    'Are the domain boundaries that separate medical/sports contexts (demanding sex-category) from legal/social contexts (demanding gender-identity) natural features of how institutions operate, or socially constructed choices that institutions could revise?',
    'Historical and comparative institutional analysis: did the boundary between medical and legal category rules predate the gender-identity/sex-category dispute, or did it emerge as a compromise mechanism during the dispute itself?',
    'If boundaries are natural, the hybrid reading is a genuine coordination achievement. If constructed, the hybrid reading is a performance that transfers conflict to subject populations rather than resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_determinism, conceptual, 'Whether domain-specific categorization reflects institutional necessity or institutional choice.').

omega_variable(
    sex_category_measurement_instability,
    'What counts as biological sex for medical and sports purposes: chromosomes, reproductive anatomy, hormonal profile, developmental biology, gamete production capacity, or some weighted combination? And does that measurement method actually predict medical or fair-competition outcomes?',
    'Medical and sports-science research examining whether specified sex-category criteria (e.g., testosterone levels, chromosome type) predict actual health outcomes or athletic performance variance. Natural experiments from sports federations changing eligibility criteria.',
    'If sex-category measurement is unstable (different medical contexts use different criteria) or predictively weak (measurement does not predict stated outcomes), the medical/sports domain distinction collapses and the hybrid reading''s domain-specificity is false. If stable and predictive, the reading''s domain distinction is structurally warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sex_category_measurement_instability, empirical, 'Whether biological sex measurement is coherent and outcome-predictive across medical and sports domains.').

omega_variable(
    institutional_capture_mechanism,
    'Is the hybrid contextual reading genuinely a compromise, or is it a mechanism by which institutional actors minimize their own decision-burden by deferring conflicts to subject populations?',
    'Analysis of institutional decision-making: do institutions author domain-specific rules based on evidence of differential impact, or do they adopt the reading because it allows them to claim balance without resolving the underlying dispute?',
    'If genuine compromise, the constraint is tangled-rope (coordination plus selective extraction). If institutional cost-shifting, the constraint is closer to snare (the institutional benefit is administrative simplicity achieved by displacing classification burden to trans subjects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_mechanism, empirical, 'Whether institutional domain-specificity reflects problem-solving or cost-shifting.').

omega_variable(
    binary_category_necessity,
    'Do the medical, legal, and sports domains that invoke woman/female categories actually require binary categorization, or do they require specific data (reproductive anatomy, chromosomes, hormonal profiles, legal documents) that could be reported without embedding them in a binary category?',
    'Institutional redesign experiments: could medical systems report relevant sex-based data without requiring patients to fit into woman/man categories? Could sports use performance-based criteria or hormone-level criteria without binary sex categories? Could legal systems recognize gender identity without requiring sex-category erasure?',
    'If binary categorization is not structurally necessary, the constraint is entirely institutional choice and the extraction (forcing trans people into contradictory category rules) is purely discretionary. This would increase the extracted-ness assessment and shift the constraint closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binary_category_necessity, conceptual, 'Whether binary woman/man categories are functionally necessary or institutionally chosen.').

omega_variable(
    kernel_reading_disjunction,
    'Is this hybrid contextual reading genuinely a third position that stabilizes the kernel (woman/female category), or is it a performance of compromise that covertly privileges one of the sibling readings depending on which institutional domain you examine?',
    'Analysis of which reading''s logic actually dominates institutional outcomes: if medical contexts reliably enforce sex-category membership and legal contexts reliably enforce gender-identity membership, the hybrid reading is stable. If institutional enforcement is inconsistent or one reading dominates across domains, the hybrid reading is theater.',
    'If the hybrid reading is theater (covert privileging of one sibling), the theater_ratio and mandatrophy scores increase, and the constraint reclassifies toward piton. If the hybrid reading genuinely stabilizes the kernel, the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disjunction, empirical, 'Whether the hybrid reading is a genuine third position or covert mono-reading theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__hybrid_contextual_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__hybrid_contextual_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__hybrid_contextual_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.51).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(woma_be_t4, woman_female_category__hybrid_contextual_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(woma_be_t8, woman_female_category__hybrid_contextual_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(woma_be_t16, woman_female_category__hybrid_contextual_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(woma_su_t4, woman_female_category__hybrid_contextual_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(woma_su_t8, woman_female_category__hybrid_contextual_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(woma_su_t16, woman_female_category__hybrid_contextual_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading constraint family. The woman/female category kernel is contested across three structurally distinct readings: (1) sex_biology_reading (category = chromosomal/reproductive sex always, negligible ε in medical/sports contexts where it is descriptively accurate, higher ε in legal/social contexts where it conflicts with identity-based recognition), (2) gender_identity_reading (category = gender self-identification always, negligible ε in legal/social contexts, higher ε in medical/sports contexts where it may conflict with reproductive-system-relevant data needs), and (3) hybrid_contextual_reading (this story—category membership varies by domain, moderate ε across all contexts because institutional compartmentalization imposes costs on all parties). The three readings have fundamentally different ε referents and victim sets. The hybrid reading attempts to avoid the zero-sum nature of the other two by claiming both are partially correct in different domains, but this creates the institutional cost-shifting that is the extraction measured here. All three readings share the kernel (contested woman/female category) and should be linked bidirectionally via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
