% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Gender Identity Category Membership (Dignity-Harm Reading)
 *   domain: political_philosophy/bioethics/law
 *
 * SUMMARY:
 *   The gender-identity reading of woman/female-category membership defines
 *   category inclusion by internal gender identity independent of biological
 *   sex. This reading institutionalizes recognition of trans women as women
 *   and trans men as men based on self-identification. The constraint's
 *   operation creates asymmetric harm: it benefits transgender individuals
 *   seeking identity-based legal recognition and inflicts dignity/category
 *   costs on cisgender women whose prior understanding was grounded in
 *   biological boundaries. The reading is actively enforced through legal
 *   machinery (ID issuance, healthcare routing, access to female-designated
 *   services); persistence depends on institutional authority suppressing
 *   alternative definitions (the sex-biology reading and hybrid-contextual
 *   reading). The claim/metric gap is deliberate: this is authored as
 *   tangled_rope (genuine coordination function + asymmetric extraction)
 *   while the sex-biology reading would author it as snare or
 *   no-constraint-at-all. The engine measures the divergence across readings;
 *   do not reconcile claims between readings.
 *
 * KEY AGENTS:
 *   - transgender_women_seeking_recognition: powerless beneficiaries, identity-locked, existential recognition at stake
 *   - cisgender_women_defending_female_category: moderate-power payers, constrained exit, category-boundary costs
 *   - legal_authority_gender_classifier: institutional agenda-setter, national scope, administers the definition
 *   - sex_biology_reading_advocates: excluded organized opposition, constrained exit, biology-criterion argument
 *   - hybrid_contextual_reading_advocates: excluded organized alternative, constrained exit, context-dependent argument
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.72).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity Category Membership (Dignity-Harm Reading)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '1f6c0094-ddb4-4b8a-a920-49282ce36f95').
narrative_ontology:cs_kernel_codification('1f6c0094-ddb4-4b8a-a920-49282ce36f95', distributed).
narrative_ontology:cs_authority_grounding('1f6c0094-ddb4-4b8a-a920-49282ce36f95', extraction).
narrative_ontology:cs_interpretation_layer_present('1f6c0094-ddb4-4b8a-a920-49282ce36f95').
narrative_ontology:cs_reading_relation('1f6c0094-ddb4-4b8a-a920-49282ce36f95', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('1f6c0094-ddb4-4b8a-a920-49282ce36f95', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('1f6c0094-ddb4-4b8a-a920-49282ce36f95', foundational, internal_gender_identity_categorical_ground).
narrative_ontology:cs_axiom_status(internal_gender_identity_categorical_ground, holdable).
narrative_ontology:cs_axiom_grounding('1f6c0094-ddb4-4b8a-a920-49282ce36f95', internal_gender_identity_categorical_ground, deontological).
narrative_ontology:cs_axiom('1f6c0094-ddb4-4b8a-a920-49282ce36f95', foundational, identity_supersedes_biology_for_category).
narrative_ontology:cs_axiom_status(identity_supersedes_biology_for_category, holdable).
narrative_ontology:cs_axiom_grounding('1f6c0094-ddb4-4b8a-a920-49282ce36f95', identity_supersedes_biology_for_category, deontological).
narrative_ontology:cs_reference_frame('1f6c0094-ddb4-4b8a-a920-49282ce36f95', pre_institutional_gender_recognition).
narrative_ontology:cs_drift_state('1f6c0094-ddb4-4b8a-a920-49282ce36f95', contemporary_legal_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f6c0094-ddb4-4b8a-a920-49282ce36f95', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_recognition).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, transgender_women_excluded_from_female_category).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women_contesting_category_definition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_men_birth_assigned_female).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women_defending_female_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition as women/female based on internal gender identity independent of birth-assigned sex. The constraint as the gender-identity reading frames it recognizes their category membership on that basis, enabling access to female-designated spaces, legal documents, healthcare routing, and social standing. The benefit is existential recognition and dignity — the constraint names them as they understand themselves.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_women_seeking_recognition, beneficiary,
    powerless, biographical, identity_locked, national).

% Bear costs measured as loss of categorical exclusivity and exclusion-based protection in spaces defined as female-only (bathrooms, shelters, sports, prisons). Under this reading, they lose the right to define the category by biological boundaries. They experience this as forced inclusion of trans women in spaces they understand as biology-based. Their exit is constrained: they cannot leave womanhood (it is identity-constitutive), and they cannot enforce exclusion once the reading's definition is institutionalized.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women_defending_female_category, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, cisgender_women_defending_female_category, payer).

% Administers the legal definition of woman/female category membership through statute, regulation, and case law. Under the gender-identity reading, they enforce recognition of gender identity as the criterion, issuing legal documents (birth certificates, IDs, marriage certificates), routing healthcare, and adjudicating access to female-designated services. This reading requires active enforcement: legal machinery must recognize self-identification and reject biology-only criteria.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_authority_gender_classifier, agenda_setter,
    institutional, generational, analytical, national).

% Argue that the category should be defined by chromosomal sex, reproductive anatomy, and developmental biology (XX/XY, gamete production capacity). They are excluded from the decision-making process that institutionalizes the gender-identity reading; they would contest the definition but find themselves outside the frame that the institutional authority has adopted. Their exclusion is the institutional choice to enforce one reading over another.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_biology_reading_advocates, excluded,
    organized, biographical, constrained, national).

% Propose that category membership varies by context: biological sex for medical/sports/safety purposes, gender identity for social/legal recognition. They are structurally excluded from this reading's framework because the gender-identity reading treats the criterion as universal and context-independent. Their alternative is not entertained as live within the institutional authority's operative definition.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, hybrid_contextual_reading_advocates, excluded,
    organized, biographical, constrained, national).

% Benefit incidentally: under this reading they are recognized as men based on gender identity, not female despite birth assignment. The reading's criterion (internal gender identity independent of biological sex) applies to them as well, securing their category placement and associated recognition.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_men_birth_assigned_female, beneficiary,
    powerless, biographical, identity_locked, national).

% Occupy the complementary male category under this reading, defined by the same criterion: internal gender identity independent of biological sex. They are largely unaffected by the female-category dispute, though their position is symmetrically dependent on the same criterion. They are analytical to the immediate constraint because the extractiveness and enforcement focus on the female category.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_men_institutional_position, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, legal_authority_gender_classifier).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal-institutional mapping from internal gender identity to category membership (woman/female), enabling consistent recognition across healthcare systems, legal documents, and social services. Solves a coordination problem: what criterion determines category assignment when biological, social, and identity axes diverge?
% TRANSFER_FUNCTION: Transfers categorical recognition and the access/protections attached to that category from biology-based frameworks to identity-based frameworks. Under this reading, trans women receive legal female-category status; cisgender women whose understanding was biology-based lose the exclusivity they may have relied on. The constraint moves standing and institutional recognition from one definition regime to another.
% ABSENT_VOICES: Sex-biology-reading advocates and hybrid-contextual-reading advocates are structurally excluded from the decision-making process that institutionalizes the gender-identity reading. They would argue for biological criteria or context-dependent mixed criteria but are not seated in the authority structure that makes the definition binding. Their argument that the category should remain grounded in reproducible biological facts is not heard in the institutional process.
% DISAPPEARANCE_RATIONALE: If the gender-identity reading constraint vanished overnight and legal authority reverted to biology-only definition, trans women would lose legal female-category status; legal documents would revert to birth-assignment categories; healthcare routing would reorganize around biological criteria; female-designated spaces would reorganize around biology-only access. Millions of legal statuses would flip; institutional categories and access structures would reorganize entirely around a different definitional criterion.
% FOUNDING_PROBLEM: Medical, legal, and social recognition systems were built on biological sex as an immutable categorical marker, but millions of people experience their gender identity as discordant from birth-assigned sex. The founding problem is the incongruence: systems designed for biology-sex-as-default encounter people for whom that framework misnames their social and legal standing.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals, medical professionals treating gender dysphoria, and legal advocates attest the founding problem is live: misalignment between internal gender identity and institutional category assignment causes documented psychological, social, and medical harms. This reading's response (adopt gender-identity criteria) is contested by advocates of the alternative readings, but the existence of the foundational incongruence is not disputed across any reading.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) is high because the reading's operation inflicts measurable harms on the payer seats (loss of category exclusivity, forced inclusion in spaces previously defined by biological boundaries) without their consent, and these harms are not compensated by equivalent benefits — cisgender women's dignity/safety concerns are overridden, not balanced. Suppression (0.72) is high because enforcement of the identity-based criterion requires active institutional suppression of alternative readings: the legal authority must reject biology-based criteria and exclude those arguments from the decision-making process. Theater (0.41, moderate) reflects that some enforcement activity genuinely coordinates recognition across systems, but increasing institutional effort goes to suppressing alternative definitions rather than solving coordination problems. Accessibility collapse (0.63) is moderate: alternatives (biology-based definition, hybrid-contextual definition) exist and are argued, but are systematically excluded from institutionalization — they collapse as institutionally available options, not as logically or empirically available positions. Resistance (0.79) is high: the payer seats and excluded seats mount substantial resistance through litigation, legislative advocacy, and social opposition — the constraint persists against real, organized resistance. The measurement series trace extractiveness and theater rising over the interval, then plateauing as institutional enforcement stabilizes; suppression requirement rises then stabilizes. This pattern reflects the lifecycle of a contested constraint: initial expansion of enforcement to suppress alternatives, then stabilization as the enforcement regime hardens.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute substantially different constraint types from the engine. From the trans-women-seeking-recognition seat (beneficiary, powerless, identity-locked), the constraint appears as rope or scaffold: genuine coordination solving a real incongruence between identity and institutional category, offering access and recognition. From the cisgender-women-defending-category seat (payer, moderate, constrained), the same structure appears as tangled_rope or snare: forced inclusion without consent, extraction of category-boundary protection without compensation. From the sex-biology-reading advocates' seat (excluded, organized, constrained), the constraint appears as snare or scaffolding-toward-harm: an institutional choice to suppress an alternative definition and enforce one reading over another, with lasting consequences for the definition itself. The engine's per-seat computation of these divergent types — not reconciled to a single story-level answer — is exactly the measurement the classification system exists to perform. Do not tune the metrics to produce uniform types across seats; the divergence is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women (beneficiary seat) occupy powerless position with identity-locked exit — their directionality is near 0.0 (full beneficiary): the constraint subsidizes their legal recognition at massive existential value. Cisgender women (payer seat) occupy moderate power with constrained exit — their directionality is near 0.85 (near-target): they bear category-boundary costs without consent, constrained by the fact that womanhood is identity-constitutive (cannot exit by leaving the category). Legal authority (agenda-setter) occupies institutional power and enforces the definition — directionality depends on whether enforcement is extractive (profiting from the controversy) or coordinative (genuinely trying to solve incongruence); under this reading's framing, authority directionality sits near 0.5 (symmetric) because their role is administering a contested definition, not collecting rents. Sex-biology and hybrid-contextual advocates (excluded) sit outside the constraint's operative boundaries and are not assigned directionality within this reading (their directionality belongs to the rival readings). The metric profile creates substantial seat divergence: the beneficiary seat experiences the constraint as liberation (d ≈ 0.0, χ inverted to subsidy); the payer seat experiences it as forced inclusion and category erosion (d ≈ 0.85, χ highly extractive). The engine computes per-seat types from this structural divergence; different seats should reach different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The gender-identity reading's claim as tangled_rope (coordination + asymmetric extraction) requires both a genuine coordination function AND identifiable victims with asymmetric costs. Coordination function: The underlying incongruence (identity-biology misalignment) is real; institutional category systems need one criterion; identity-based criteria do genuinely coordinate recognition where biology-based criteria produced institutional misgendering. Asymmetric extraction: Cisgender women whose understanding of the female category was grounded in biological distinctiveness lose exclusivity and force-accept inclusion of trans women — this is not reciprocal; they do not gain equivalent recognition in return. Trans women gain what they lack under alternative readings (legal female status); cisgender women lose what they had (category boundary control). Victims are not speculative — they are named stakeholders (cisgender_women_defending_female_category) with articulated costs (loss of exclusivity, forced inclusion, category redefinition without consent). Active enforcement is required: the legal authority must suppress the sex-biology and hybrid-contextual readings institutionally and issue legal documents under the identity-based criterion. The classification prevents false-labeling this as pure rope (which would require beneficiaries and payers to be symmetrically positioned) or as pure coordination (which would require no victims). It also prevents false-labeling this as pure snare (the coordination function is genuine, not cover story). Mandatrophy is not present: the founding problem (identity-biology incongruence) is live across all readings; the founding_problem_status is live, not dead. The constraint is not a degraded function persisting through inertia — it is an active contestation over definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_evidence_asymmetry,
    'What makes gender identity a reliable basis for category membership when it is internal and unobservable, compared to biological sex which is measurable?',
    'Clinical validation research on the stability and authenticity of gender identity across populations; comparison with reliability of biological sex markers in edge cases (hormonal disorders, chromosomal variation); precedent from other legal identity categories (race, nationality) that use self-identification.',
    'If gender identity is shown to have high test-retest stability and low rate of strategic misrepresentation, the evidentiary gap closes and the identity-based reading''s legitimacy strengthens. If the gap persists or identity-instability is high, the reading''s institutional reliability is compromised and hybrid or contextual criteria gain traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_evidence_asymmetry, empirical, 'Comparative evidentiary reliability of identity-based vs. biology-based criteria.').

omega_variable(
    harm_asymmetry_across_readings,
    'Under this reading, cisgender women lose category-boundary control and forced-inclusion costs are inflicted; under the sex_biology_reading, trans women lose category membership and exclusion costs are inflicted. Which configuration produces greater aggregate harm, or is the harm incomparable?',
    'Comparative documentation of psychological, social, and institutional harms to each set under each reading; examination of whether harms are comparable or incommensurable (category-boundary loss vs. identity-recognition loss may be different types of harm with no common measure).',
    'If harms are comparable and the identity-based reading produces greater aggregate harm, the sex_biology_reading''s ethical case strengthens. If harms are incommensurable, the choice between readings cannot be resolved by harm-minimization and becomes a question of values (recognition vs. protection). If the identity-based reading produces lesser aggregate harm, its institutional case strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_asymmetry_across_readings, preference, 'Comparative harm across readings — empirically complex, value-dependent.').

omega_variable(
    institutional_foreclosure_vs_coexistence,
    'Can institutional authority maintain multiple definitional criteria simultaneously (hybrid-contextual reading), or does legal/institutional structure force binary choice?',
    'Examination of existing multi-criteria institutional systems (e.g., passport recognition under one criterion for some purposes, another for others; medical vs. legal sex distinctions); test whether legal authority can sustain context-dependent definitions without contradicting coherence requirements.',
    'If multi-criteria institutional systems are viable and stable, the hybrid_contextual_reading is institutionally available and this reading''s mandate to suppress it becomes harder to justify. If institutional law requires binary/universal criteria, the choice between readings is forced and the exclusion of alternatives becomes structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_foreclosure_vs_coexistence, empirical, 'Whether institutional architecture permits multi-criteria definitions or enforces binary choice.').

omega_variable(
    committer_frame_kernel_identity,
    'Is the woman/female category a persistent kernel (a commitment whose definition is contested but whose category persists), or is it a constructed category subject to fundamental reimagining?',
    'Historical genealogy of the category''s institutional evolution; examination of whether alternatives (eliminating sex/gender categories entirely, radically reimagining their meaning) are live readings or structurally foreclosed by the kernel structure itself.',
    'If the category is a true kernel whose definition is contested but whose existence is fixed, each reading (gender-identity, sex-biology, hybrid) is a legitimate instantiation. If the category is radically constructed and subject to elimination, the contest itself may be a false reframing and an abolitionist reading would become the relevant alternative. If category-elimination is foreclosed, this reading and its siblings are the complete set of live instantiations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_identity, conceptual, 'Whether woman/female is a persistent kernel or a radically contingent category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__gender_identity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__gender_identity_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__gender_identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__gender_identity_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(woma_be_t25, woman_female_category__gender_identity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(woma_be_t30, woman_female_category__gender_identity_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(woma_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(woma_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(woma_su_t25, woman_female_category__gender_identity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(woma_su_t30, woman_female_category__gender_identity_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The woman_female_category kernel instantiates three separate constraint stories, one per reading. Each story carries a distinct ε value, victim/beneficiary structure, and type claim because the reading's operative criterion determines what counts as extraction. The gender_identity_reading (this file) authors ε=0.68 based on dignity/recognition harms to payers and suppression of alternative readings. Sibling readings author different ε values from their own standpoints. All three are linked via network.affects_constraints because adoption of one reading structurally influences the others — e.g., institutional enforcement of gender-identity criteria raises the cost and reduces the institutional viability of the sex-biology reading. The three stories are not observations of one category from different angles; they are three different categories (each with its own definition, its own institutional machinery, its own harm/benefit structure) that happen to share a natural-language label. The constraint-family relationship preserves the ε-invariance principle: each reading gets its own ε, independently authored, without averaging or hedging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
