% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   human_readable: Woman/Female Category Membership — Gender-Identity Reading (Self-Identification Constitutive)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The colloquial question 'what determines membership in the woman/female
 *   category' decomposes, per the epsilon-invariance principle, into three
 *   structurally distinct readings of one kernel. This file instantiates the
 *   gender_identity_reading (membership by internal self-identification,
 *   independent of biological sex);
 *   woman_female_category__sex_biology_reading and
 *   woman_female_category__hybrid_contextual_reading instantiate the siblings
 *   as separate constraints. All three stories share one epsilon referent —
 *   the standing category-governance arrangement (biology-anchored legal sex
 *   with layered, gatekept recognition mechanisms) — and author different
 *   values through their own lights: this reading authors epsilon = 0.72,
 *   reading the arrangement's certification layer as dignity/recognition
 *   extraction from transgender people, with the harm locus concentrated on
 *   trans women's access to female-only spaces and services; the sex-biology
 *   reading would author low epsilon on the same arrangement (it endorses the
 *   anchor and reads the recognition overlays as the extraction); the hybrid
 *   reading sits intermediate. The sex-biology anchor is the upstream
 *   baseline the other two readings contest, so this story links downstream
 *   of it in the family network. The story's constraint is this reading's
 *   rule — self-identification as constitutive of membership — while its
 *   metrics describe the standing arrangement the reading assesses; the
 *   interval maps t=0 to 1951 (first statutory recognition paths) through
 *   t=75 (2026).
 *
 * KEY AGENTS:
 *   - transgender_women: Primary target (moderate/identity_locked) — bears the certification burden, documentation outing, and conditional access to female-only spaces; holds a dual position through the arrangement's protection layer
 *   - trans_men_and_nonbinary_people: Secondary target (moderate/identity_locked) — same certification machinery; nonbinary people fall outside the binary categories entirely, receiving neither recognition nor a category to be excluded from
 *   - medical_gatekeeping_professions: Primary beneficiary (institutional/constrained) — collects assessment jurisdiction, fees, casework authority, and professional role-security from the certification requirement
 *   - cisgender_women: Beneficiary constituency (organized/identity_locked) — hold the anchor's determinacy benefit and many defend it as protective
 *   - national_recognition_legislatures: Agenda setter (institutional/mobile) — enact and maintain the gatekept statutes and could dismantle the certification layer by statute
 *   - gender_critical_womens_organizations: Excluded voice (organized/mobile) — excluded at the level of premise rather than procedure; this reading's framework cannot admit their core claim
 *   - human_rights_courts: Analytical observer (institutional/analytical) — adjudicate the arrangement's boundaries and absorb its drift through case law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.72).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.68).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Woman/Female Category Membership — Gender-Identity Reading (Self-Identification Constitutive)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '0838cb9f-0fdb-47ae-a4e4-0c948ddda6af').
narrative_ontology:cs_kernel_codification('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', formalized).
narrative_ontology:cs_authority_grounding('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', extraction).
narrative_ontology:cs_interpretation_layer_present('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af').
narrative_ontology:cs_reading_relation('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', foundational, gender_identity_constitutes_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', gender_identity_constitutes_category_membership, deontological).
narrative_ontology:cs_axiom('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', secondary, category_membership_context_invariant).
narrative_ontology:cs_axiom_status(category_membership_context_invariant, holdable).
narrative_ontology:cs_axiom_grounding('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', category_membership_context_invariant, deontological).
narrative_ontology:cs_reference_frame('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', self_identification_constitutive_membership).
narrative_ontology:cs_drift_state('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', contemporary_post_depathologization, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('0838cb9f-0fdb-47ae-a4e4-0c948ddda6af', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, trans_men_and_nonbinary_people).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, medical_gatekeeping_professions).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, trans_men_and_nonbinary_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a recognition regime that conditions membership in the woman/female category on medical certification: diagnosis letters, assessment panels, evidence of persistence. They receive real protections through the arrangement's anti-discrimination and recognition layer where it operates, while bearing its gatekeeping costs: documents that out them, exclusion or conditional access to female-only spaces and services, assessment fees and multi-year waiting lists. Exit is unavailable — the category question follows them through every institution, and their identity is the very thing the arrangement adjudicates.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_women, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, transgender_women, beneficiary).

% Face the same certification machinery to have assigned sex corrected or category membership recognized. Nonbinary people additionally fall outside the arrangement's binary categories entirely, receiving neither recognition nor a category whose protections they could claim. They bear documentation and assessment costs and receive whatever protection the recognition layer extends to them.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, trans_men_and_nonbinary_people, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, trans_men_and_nonbinary_people, beneficiary).

% Psychiatric and psychological professions, gender clinics, and assessment panels whose gatekeeping role the arrangement sustains: diagnosis letters, panel approvals, and evidence requirements are their jurisdiction, revenue, and professional authority. Depathologization pressures have eroded the mandate's justification, but their institutional position is bound up with the assessment function, and adaptation has been slow.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, medical_gatekeeping_professions, beneficiary,
    institutional, generational, constrained, global).

% Hold secure membership in the category under the arrangement's biology anchor and receive its determinacy: a stable, administrable classification that institutions coordinate around. Many defend the anchor as protective of women's provisions. This reading's assessment holds the determinacy is real but purchased with the certification costs borne by transgender people, and that the protective framing does more rhetorical than structural work.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women, beneficiary,
    organized, biographical, identity_locked, global).

% Enact and maintain the gatekept recognition statutes: they set the evidence requirements, waiting periods, and panel structures, and they can amend or replace them — as the reform attempts and enactments of the last two decades demonstrate. They are the seat that could dismantle the certification layer by ordinary statute.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, national_recognition_legislatures, agenda_setter,
    institutional, generational, mobile, national).

% Organize for the biology anchor and against identity-based membership, contesting this reading's premise in legislation, litigation, and public discourse. Within this reading's framework their core claim — that the category is biology-determined — is inadmissible, so their strongest objections operate outside the conversation this story's framework recognizes: the exclusion is premise-level rather than procedural.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_womens_organizations, excluded,
    organized, biographical, mobile, national).

% Adjudicate the arrangement's boundaries: they have struck down the harshest certification requirements while upholding anchored classification elsewhere, and their case law is the interpretive layer through which the arrangement's drift is absorbed without formal revision of its kernel.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, human_rights_courts, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, medical_gatekeeping_professions).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, administrable legal classification of category membership that documentation systems, institutions, and anti-discrimination law can coordinate around, plus a certified change path for people whose identity diverges from assignment.
% TRANSFER_FUNCTION: Moves recognition authority from individuals to medical and administrative gatekeepers (diagnosis letters, panel approvals, evidence requirements); moves the dignity, documentation, and assessment costs of classification onto people whose identity diverges from assigned sex; moves fees, casework authority, and professional role-security to the gatekeeping professions; delivers category determinacy to members whose identity matches their assignment.
% ABSENT_VOICES: Gender-critical women's organizations hold the strongest structural objection to this reading's premise and are excluded at the level of premise rather than procedure — this reading's framework cannot admit their core claim, so their objections register only as contest, never as admissible argument; whether that exclusion reflects the premise's structure or the reading's blind spot is carried in the omega set. Trans people in gatekept jurisdictions without advocacy infrastructure — incarcerated, poor, non-anglophone — are absent from the consultation and litigation record that shapes the arrangement's drift.
% DISAPPEARANCE_RATIONALE: Documentation registries, prison placement policy, sports eligibility rules, medical recording, and anti-discrimination enforcement all key off the arrangement's classification; overnight removal would force every institution to re-derive membership rules at once. The legislative upheaval accompanying each self-ID reform wave is a small-scale preview of that rearrangement.
% FOUNDING_PROBLEM: Governing legal category membership when identity and assigned sex diverge: the postwar settlement built a narrow, medically-certified change path — diagnosis, evidence of persistence, and across much of the arrangement's history sterilization or treatment requirements — to admit the 'genuine' case while protecting the category's biological anchor.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: ECHR case law (Goodwin v. UK, 2002) attests both the founding problem and the gatekept answer's inadequacy; the WHO ICD-11 depathologization attests that the certification layer's medical justification was abandoned by its own discipline; the legislative records of the Argentine (2012) and Irish (2015) reforms attest the problem's persistence under gatekeeping. The gatekeeping professions attest the underlying problem is real while disputing this reading's diagnosis that their role constitutes the extraction.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72) because the arrangement conditions category membership on medical certification whose justifying framework — pathologization of gender diversity — its own profession abandoned (WHO ICD-11), while the machinery persists; the extraction axis is dignity and recognition: documents that out people, exclusion or conditional access to female-only spaces, assessment fees and multi-year waiting lists. Suppression is high (0.68) because the arrangement actively denies self-declared membership and enforces the anchor through documentation systems and institutional policy; the alternative (self-declaration) is foreclosed within anchored jurisdictions and available only through costly jurisdictional exit — suppression here is structural (legal and administrative barriers), and it is authored as a raw structural property, unscaled by power or scope. Theater is moderate (0.45): panels and evidence requirements perform a rigor whose clinical question has largely dissolved. Accessibility_collapse (0.60) reflects an alternative that is real but expensive: self-ID statutes exist in reachable jurisdictions. Resistance (0.65) reflects organized trans advocacy, strategic litigation, and the reform wave — coalition power among a class with moderate individual resources is the arrangement's principal source of resistance, and it has legislatively won in several jurisdictions. The measurement series run on one shared time grid (t = 0, 10, 20, 30, 45, 55, 65, 75) and show one full cycle: certification build-up and hardening, the Goodwin/self-ID reform trough, and partial re-hardening in anchored jurisdictions; the oscillation is legislative-contest dynamics, not intermittent reinforcement. Claim and metrics are independent authored facts: the tangled_rope claim is this reading's structural judgment that the arrangement's recognition layer is genuine coordination worth keeping while its certification layer is extraction worth removing; the engine computes per-seat types from the structural data, and divergence between this claim and any computed type is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the gatekeeping professions' position the arrangement is a clinical-administrative necessity — safeguarding requires assessment; from the transgender seats the same structure is dignity extraction wearing a protection veneer; from the cisgender-women constituency it is determinacy worth defending. Two organized actors at equal nominal power — trans advocacy organizations and gender-critical women's organizations — sit at opposite directionalities, differentiated entirely by their structural relationship to the anchor rather than by global standing. The rival readings invert this gap: the sex-biology reading would author the same arrangement with cisgender women as the protected class and self-identification as the extraction; that inversion, not any neutral description, is what the family's shared referent is designed to expose.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification professions are the structural beneficiary: the certification requirement is their jurisdiction, revenue, and authority, so their directionality sits near the beneficiary end. Cisgender women receive the anchor's determinacy benefit and, per this reading, do not bear its costs — low d. Transgender women and trans men/nonbinary people are dual-listed (protection layer received, gatekeeping costs borne); the structural derivation from dual beneficiary/victim listing plus identity-locked exit would produce a near-symmetric d, which this reading holds is wrong — the extraction dominates the protection layer — hence the moderate-atom override to 0.78. The organized-atom override to 0.2 covers the anchor-constituency seats (cisgender women and gender-critical organizations) whose defense of the anchor places them near the beneficiary end despite collecting none of the extraction; without the override the derivation would leave the unlisted gender-critical organizations at a symmetric fallback that misdescribes their position.
 *
 * MANDATROPHY ANALYSIS:
 *   The standing arrangement is a compound: a live recognition function (documentation, anti-discrimination protection, a change path) wrapped around a dead mandate (medical certification of identity, abandoned by the profession's own depathologization). The mandatrophy declaration marks the dead mandate; the classification stays tangled_rope rather than piton because the extraction has a concentrated receiver — the certification professions, named in the receipt surface — and rather than snare because the coordination function is genuine and this reading's remedy is surgical: remove the certification layer, keep recognition. The mislabeling risks run both ways: calling the whole arrangement a snare would license discarding the recognition layer this reading exists to secure; calling it a rope would launder the certification extraction. The founding problem (governing identity/sex divergence) remains live, but the founding answer's medical justification does not — that split is the arrangement's mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the gender_identity_reading of the woman_female_category kernel; how would the sibling readings (sex_biology_reading, hybrid_contextual_reading) restructure the same story?',
    'Generate the sibling stories: the sex-biology reading would declare no transgender victims and seat cisgender women as the protected class with low epsilon on the anchor; the hybrid reading would partition the contexts and author intermediate epsilon.',
    'The disagreement is located entirely in the membership criterion: identity-constitutive vs biology-constitutive vs context-partitioned. Victim sets, epsilon values, and types all flip across the family while the referent arrangement stays fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one kernel, three readings, shared referent, reading-indexed values.').

omega_variable(
    epsilon_referent_framing,
    'Is epsilon correctly authored on the standing category-governance arrangement (biology-anchored legal sex with layered gatekept recognition) rather than on this reading''s endorsed self-identification rule?',
    'Apply the epsilon-referent rule for kernel readings: the referent is the arrangement under contest that the story assesses, never the reading''s endorsed alternative; authoring on the endorsed rule would collapse every advocacy reading in the kernel toward epsilon near zero and destroy the family''s divergence signal.',
    'If the referent were switched to the endorsed self-ID rule, this story would become a low-epsilon rope self-assessment and the manifest''s high-epsilon expectation would be unmeetable; the shared-referent design is what lets the three readings disagree numerically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_framing, conceptual, 'Framing under-determination: what epsilon is about for an advocacy reading of a contested membership kernel.').

omega_variable(
    protection_extraction_dominance,
    'For transgender individuals holding dual position under the standing arrangement, does the recognition and anti-discrimination protection layer outweigh the gatekeeping costs, or does the extraction dominate?',
    'Comparative welfare and dignity-harm measurement across gatekept and self-ID jurisdictions: documentation outing rates, assessment waiting-list harms, exclusion incidents, versus measured protection uptake and discrimination-remedy access.',
    'If protection dominates, the dual-listed seats derive near-symmetric directionality and the arrangement tilts toward rope; if extraction dominates (this reading''s position, encoded in the moderate-atom override at 0.78), the tangled_rope classification holds with the trans seats near full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_extraction_dominance, empirical, 'Whether the arrangement''s benefit layer or its gatekeeping layer dominates for the dual-positioned population.').

omega_variable(
    foreclosure_of_hybrid_partition,
    'Is the hybrid contextual partition (biology for medical/sports/safety contexts, identity for social/legal recognition) genuinely incompatible with this reading''s unqualified identity premise, or is it a holdable qualification?',
    'Test whether a single framework can coherently hold ''membership is identity-determined, full stop'' alongside ''membership tracks biology in specified contexts''; if the unqualified premise entails context-invariance, the partition is ruled out and the forecloses edge stands.',
    'If the partition is holdable as a qualification rather than a contradiction, the relation to hybrid_contextual_reading should be influences rather than forecloses, changing the kernel''s computed foreclosure graph and the engine''s terminal-state analysis for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_of_hybrid_partition, conceptual, 'Whether this reading''s context-invariant premise logically eliminates the hybrid sibling or merely pressures it.').

omega_variable(
    gatekeeping_mandate_viability,
    'Is the certification layer''s mandate dead (its medical justification abandoned by depathologization) or live (safeguarding needs independently justify assessment)?',
    'Outcome comparison across jurisdictions that removed certification requirements versus those retaining them: if safety and provision-integrity outcomes hold without assessment, the mandate is dead; documented safeguarding failures traceable to removal would support a live mandate.',
    'If the mandate is live, theater_ratio drops substantially, the mandatrophy declaration weakens, and the arrangement tilts toward a genuine rope with residual costs; if dead (this reading''s position), the theater measurement stands and the gatekeeping layer is extraction riding a dead justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_mandate_viability, empirical, 'Whether the certification requirement retains an independent justification or persists as procedural performance.').

omega_variable(
    reform_wave_trajectory,
    'Is the post-2015 self-ID reform wave a permanent erosion of the anchored arrangement or one phase of a re-hardening cycle?',
    'Track legislative and litigation outcomes over the next decade: continued adoption and entrenchment of declarative systems versus statutory reversals and judicial re-anchoring.',
    'Permanent erosion would date a tangled_rope-to-scaffold transition (the arrangement''s gatekeeping layer carrying a de facto sunset); re-hardening would confirm the cyclical pattern and keep the arrangement in steady tangled_rope operation with the trans seats near full target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_wave_trajectory, empirical, 'Whether the enforcement trajectory is decay or cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__gender_identity_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(woma_tr_t45, woman_female_category__gender_identity_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(woma_tr_t55, woman_female_category__gender_identity_reading, theater_ratio, 55, 0.39).
narrative_ontology:measurement(woma_tr_t65, woman_female_category__gender_identity_reading, theater_ratio, 65, 0.42).
narrative_ontology:measurement(woma_tr_t75, woman_female_category__gender_identity_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(woma_be_t30, woman_female_category__gender_identity_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(woma_be_t45, woman_female_category__gender_identity_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(woma_be_t55, woman_female_category__gender_identity_reading, base_extractiveness, 55, 0.66).
narrative_ontology:measurement(woma_be_t65, woman_female_category__gender_identity_reading, base_extractiveness, 65, 0.69).
narrative_ontology:measurement(woma_be_t75, woman_female_category__gender_identity_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(woma_su_t30, woman_female_category__gender_identity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(woma_su_t45, woman_female_category__gender_identity_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(woma_su_t55, woman_female_category__gender_identity_reading, suppression_requirement, 55, 0.62).
narrative_ontology:measurement(woma_su_t65, woman_female_category__gender_identity_reading, suppression_requirement, 65, 0.65).
narrative_ontology:measurement(woma_su_t75, woman_female_category__gender_identity_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what makes someone a woman/female' covers three structurally distinct membership rules that cannot share one epsilon: this reading (identity-constitutive, context-invariant), the sex-biology reading (biology-constitutive), and the hybrid contextual reading (context-partitioned). Per the epsilon-invariance principle they are authored as three stories in one constraint family, all linked via affects_constraints, all sharing the standing arrangement as referent with reading-indexed epsilon values (0.72 here; low expected from the sex-biology seat, intermediate from the hybrid seat). The sex-biology story is the upstream baseline; the other two contest it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, moderate, 0.78).
constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
