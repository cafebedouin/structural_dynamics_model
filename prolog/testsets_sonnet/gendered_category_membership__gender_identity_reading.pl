% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership via Self-Declared Identity
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'gendered_category_membership': category membership grounded in
 *   subjective identity and self-declaration. Under this reading, trans women
 *   are included in the 'woman' category by self-ID, sex-segregated
 *   institutions are re-read as gender-segregated, and objection by incumbent
 *   category members is framed as exclusionary conduct rather than a
 *   legitimate competing claim. This is a distinct constraint from the
 *   biological_sex_reading (which grounds membership in immutable biological
 *   markers and would classify the same admission as a category error) and
 *   the social_role_reading (which grounds membership in sustained social
 *   performance and recognition, a different and more gradualist criterion).
 *   The three readings are not the same constraint measured three ways — each
 *   has a different beneficiary/victim structure and a different ε, and are
 *   linked here only via network.affects_constraints, per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (moderate/identity_locked) — depend on self-ID for recognition and access
 *   - nonbinary_people: Beneficiary (powerless/identity_locked) — self-ID is often their only legal recognition path
 *   - cis_women_in_sex_segregated_spaces: Primary payer (moderate/trapped) — bear redefinition of protective single-sex spaces
 *   - female_athletes: Payer (moderate/constrained) — bear competitive category redefinition
 *   - abuse_survivors_requiring_single_sex_spaces: Payer (powerless/trapped) — bear loss of sex-specific safety criteria
 *   - detransitioners: Excluded (powerless/trapped) — testimony complicates the framework and is structurally sidelined
 *   - gender_identity_advocacy_organizations: Agenda-setter (organized/mobile) — administers and propagates the self-ID standard
 *   - family_courts_and_prison_administrators: Agenda-setter (institutional/constrained) — implements the standard under mandate, absorbing adjudication friction
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
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Self-Declared Identity").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '5939dda8-4494-44e5-b75c-ebd2cde0b50a').
narrative_ontology:cs_kernel_codification('5939dda8-4494-44e5-b75c-ebd2cde0b50a', distributed).
narrative_ontology:cs_authority_grounding('5939dda8-4494-44e5-b75c-ebd2cde0b50a', distributed).
narrative_ontology:cs_reading_relation('5939dda8-4494-44e5-b75c-ebd2cde0b50a', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('5939dda8-4494-44e5-b75c-ebd2cde0b50a', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('5939dda8-4494-44e5-b75c-ebd2cde0b50a', foundational, subjective_identity_is_constitutive_of_category_membership).
narrative_ontology:cs_axiom_status(subjective_identity_is_constitutive_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('5939dda8-4494-44e5-b75c-ebd2cde0b50a', subjective_identity_is_constitutive_of_category_membership, deontological).
narrative_ontology:cs_axiom('5939dda8-4494-44e5-b75c-ebd2cde0b50a', secondary, self_declaration_requires_no_external_corroboration).
narrative_ontology:cs_axiom_status(self_declaration_requires_no_external_corroboration, holdable).
narrative_ontology:cs_axiom_grounding('5939dda8-4494-44e5-b75c-ebd2cde0b50a', self_declaration_requires_no_external_corroboration, conventional).
narrative_ontology:cs_reference_frame('5939dda8-4494-44e5-b75c-ebd2cde0b50a', proof_based_category_gatekeeping).
narrative_ontology:cs_drift_state('5939dda8-4494-44e5-b75c-ebd2cde0b50a', contemporary_self_id_policy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5939dda8-4494-44e5-b75c-ebd2cde0b50a', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, nonbinary_people).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, female_athletes).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, detransitioners).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, abuse_survivors_requiring_single_sex_spaces).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, self_determination_of_identity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek recognition as women in law, medicine, and social space without requiring surgical or hormonal proof. Self-declaration grants access to women's shelters, sports categories, prisons, and social recognition. Exit from the constraint is not meaningfully available — their claim to inclusion depends entirely on the category holding as declared, and rejection of the framework means being classified against their identity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Use single-sex shelters, changing rooms, prisons, and sports categories originally organized around reproductive biology, often for safety or fairness reasons tied to average physical differences or histories of male violence. Under the self-ID standard, the space is redefined by gender identity rather than sex, and objecting isframed as exclusionary or transphobic. Exit means abandoning the sex-segregated protection or accepting its redefinition; there is no institutional channel to preserve a sex-based category without being cast as a perpetrator of exclusion.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_in_sex_segregated_spaces, payer,
    moderate, biographical, trapped, national).

% Compete in categories designed to offset average male physical advantage. Self-ID admission of trans women into female categories can alter competitive outcomes. Their options are competing at a disadvantage, exiting the sport, or organizing to contest category rules — each costly and reputationally risky given the framing of objection as bigotry.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, female_athletes, payer,
    moderate, biographical, constrained, national).

% Having once self-identified into a gender category and later reverted, their experience complicates the claim that self-declared identity is a stable, sufficient ground for category membership. Their testimony is frequently excluded from advocacy narratives and institutional policy discussions, since it undermines the framework's premise of settled, authoritative self-knowledge.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, detransitioners, excluded,
    powerless, biographical, trapped, national).

% Depend on shelters and services segregated by sex, often specifically to be free of the presence of biological males following trauma. Under gender self-identification, admission criteria shift to gender identity, and objecting to the presence of trans women in these spaces is treated as prejudice rather than as a trauma-informed access concern. They typically lack the institutional standing to contest policy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, abuse_survivors_requiring_single_sex_spaces, payer,
    powerless, biographical, trapped, local).

% Draft model legislation, lobby institutions, and set professional and legal standards embedding self-ID as the governing criterion for gender category membership. They administer the framework's spread through policy capture, media framing, and professional gatekeeping (medical associations, HR bodies, sports federations), and derive institutional funding, standing, and legitimacy from the framework's adoption.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations, beneficiary).

% Seek recognition outside the binary categories altogether; self-ID frameworks are often the only legal mechanism through which their identity is recognized at all. Highly dependent on the framework's continuation, with essentially no alternative avenue for legal or social recognition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, nonbinary_people, beneficiary,
    powerless, biographical, identity_locked, national).

% Must apply self-ID policy to concrete placement and custody decisions — prison housing, child custody framing, medical consent — often under legal mandate, absorbing the practical friction between competing claims (safety, identity recognition, single-sex protections) without discretion to resolve the underlying category dispute themselves.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, family_courts_and_prison_administrators, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively tractable criterion (self-declaration) for sorting individuals into gender categories across law, medicine, sport, and social space, avoiding the need for institutions to adjudicate contested biological or performative tests case by case.
% TRANSFER_FUNCTION: Moves social and institutional recognition, legal category access, and physical-space access toward self-declared identity claimants; moves the costs of category ambiguity and adjudication risk onto incumbent category members (particularly cis women in protective or competitive single-sex contexts) and onto those whose self-declared identity later reverts.
% ABSENT_VOICES: Detransitioners are structurally excluded from advocacy-shaped policy conversations because their testimony complicates the framework's central premise. Abuse survivors reliant on single-sex spaces are rarely consulted directly in policy design; their objections are frequently reframed as bigotry rather than as safety needs, which suppresses their participation in the debate.
% DISAPPEARANCE_RATIONALE: Advocacy organizations and trans/nonbinary beneficiaries would say the world rearranges catastrophically — legal recognition, safety, and access collapse without the self-ID standard. Cis women's groups, female athletes, and abuse-survivor advocates would say much of the world reverts to a prior, more contested but more legible sex-based system, and some contested harms (safety incidents, competitive disputes) would diminish. Whether this is 'rearrangement' or 'restoration' is precisely the disputed question the kernel contest is about.
% FOUNDING_PROBLEM: Trans and nonbinary individuals faced systemic non-recognition, exclusion from services, and violence when categorization required medical or legal proof (surgery, hormone therapy, court orders) that many could not obtain, afford, or safely pursue.
% FOUNDING_PROBLEM_CORROBORATION: Advocacy organizations and many trans individuals attest the non-recognition problem remains live and severe. Independent sources outside the advocacy structure — some feminist legal scholars, sports governance bodies commissioning physiological review panels, and detransition researchers — attest that the self-ID solution has generated a distinct, unresolved secondary problem (competing rights claims in single-sex spaces) rather than resolving the founding problem cleanly; no source entirely outside all interested parties has adjudicated which reading should govern.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) rather than high because the coordination function is genuine — reducing case-by-case adjudication burden and providing recognition to people who could not obtain medical/legal proof — but it is coupled with real asymmetric cost transfer onto incumbent category members in protective and competitive contexts, hence tangled_rope rather than rope or snare. Suppression (0.38) reflects the social and institutional cost of objecting (reputational risk, professional consequence, exclusion from advocacy-shaped policy fora) rather than direct physical coercion; it is real but not absolute — objection occurs and is documented, so this is not near-total suppression. Accessibility collapse is moderate (0.35): alternative framings (biological_sex_reading, social_role_reading) remain visible and contested in public discourse, they have not been fully foreclosed, which is itself evidence this is a live kernel contest rather than a settled fact. Resistance is elevated (0.62), reflecting substantial organized pushback from women's groups, sports bodies, and some feminist scholars — this is a hotly contested reading, not an uncontested natural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and nonbinary people are structural beneficiaries: the category shift grants them recognition and access they otherwise lack, and their exit option is identity_locked (rejecting the framework means being classified against their own identity, which is not a live alternative for them). Cis women in protective spaces, female athletes, and abuse survivors are payers: the redefinition of the category they relied on transfers cost onto them, and their exit options range from constrained (athletes, who can contest through governance channels at reputational cost) to trapped (abuse survivors, who typically lack institutional standing to contest placement decisions at all). Detransitioners are excluded rather than merely payers — their structural position is that their testimony is not solicited by the agenda-setting bodies, which is a distinct harm from bearing a cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — systemic non-recognition and exclusion of trans people under proof-based (medical/legal) gatekeeping — is genuinely live for many trans and nonbinary individuals, which is why this is not classified as a pure snare: there is a real coordination function being solved. But the founding_problem_status is authored as contested, not simply live, because credible sources outside the advocacy structure (sports governance physiological panels, some feminist legal scholars, detransition researchers) document that the self-ID solution has generated a distinct secondary harm (competing rights claims in single-sex spaces) that the original proof-based framework was specifically designed around. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine coordination gain (recognition without medical gatekeeping) as pure extraction; classifying it as tangled_rope rather than rope prevents treating documented cost transfer onto cis women, female athletes, and abuse survivors as costless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_declaration_epistemic_sufficiency,
    'Is subjective self-declaration a sufficient and stable ground for category membership that carries material consequences for third parties (space access, competitive categorization, custody placement), or does it require some external corroboration to bear that institutional weight?',
    'Longitudinal data on detransition rates and stability of self-declared identity over time; comparative institutional outcomes between jurisdictions using pure self-ID versus corroborated (e.g. sustained social role, medical documentation) standards.',
    'If self-declaration proves highly stable over time and across contexts, the coordination case for this reading strengthens and the extraction framing (cost to third parties) weakens. If a substantial minority of self-declarations revert, the case for requiring some external corroboration (moving toward the social_role_reading) strengthens, and continuing to treat self-ID as institutionally sufficient looks more like unearned extraction from incumbent category members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_declaration_epistemic_sufficiency, empirical, 'Whether self-declaration alone can bear the institutional weight this reading assigns it.').

omega_variable(
    kernel_reading_selection_authority,
    'Which body or process has legitimate authority to decide which of the three readings (biological_sex, gender_identity, social_role) governs a given institutional context (sport, prisons, shelters, law), and is that decision itself contestable on democratic or scientific grounds?',
    'Track which institutions (legislatures, courts, professional bodies, sports federations) explicitly adjudicate between readings versus which adopt one reading by advocacy pressure or administrative default without an adjudicative process.',
    'If the gender_identity_reading is being adopted primarily through administrative and advocacy channels rather than through explicit democratic or scientific adjudication, that raises the suppression and legitimacy-deficit concerns documented in this story''s resistance metric; if it is being adopted through transparent contested processes with input from all reading''s constituencies, the same outcome looks more like a defensible policy resolution than an imposed extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'Who has legitimate authority to select among kernel readings, and by what process.').

omega_variable(
    cis_women_as_perpetrator_framing,
    'Is the framing of cis women''s objections to gender self-ID in single-sex spaces as ''exclusionary'' or ''transphobic'' a fair characterization of a genuine competing rights claim, or does it function to suppress a legitimate objecting party by recasting their position as moral failing rather than interest conflict?',
    'Discourse analysis of how objections are treated in media, institutional policy documents, and legal proceedings; comparison to how other genuine rights conflicts (e.g. religious liberty vs. anti-discrimination) are typically adjudicated without moralized framing of one side.',
    'If the perpetrator framing is found to function primarily as a suppression mechanism rather than a substantive moral assessment, the suppression metric in this story is likely understated rather than overstated, and the classification should be reviewed toward higher suppression / lower accessibility_collapse resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_as_perpetrator_framing, conceptual, 'Whether moralized framing of objectors functions as suppression rather than fair characterization.').


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

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__gender_identity_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kernel 'gendered_category_membership.' biological_sex_reading grounds membership in immutable birth markers and would classify trans-inclusive self-ID as a category error rather than coordination; social_role_reading grounds membership in sustained social performance and recognition, which would require an ongoing history of gendered social role rather than declaration alone, and produces a different and generally lower ε (recognition earned through observable, verifiable performance reduces both gatekeeping cost and third-party contestation compared to pure self-declaration). Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure; they are not the same constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
