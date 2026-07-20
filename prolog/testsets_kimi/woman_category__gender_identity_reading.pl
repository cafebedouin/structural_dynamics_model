% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Womanhood as Internal Gender Identity (Self-Identification Reading)
 *   domain: political_philosophy_law_social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the gender_identity_reading of the contested
 *   kernel woman_category: the claim that 'woman' is defined by internal
 *   gender identity regardless of assigned sex at birth. The reading has been
 *   codified in law, institutional policy, and social practice across
 *   multiple jurisdictions. It coordinates recognition for transgender women
 *   but generates asymmetric extraction in high-stakes domainsâsports
 *   eligibility, sex-segregated shelters, prisons, and intimate
 *   spacesâwhere access rights collide with exclusion rights. The sibling
 *   readings (sex_biology_reading, intersex_accommodation_reading) are
 *   structurally foreclosed by this reading's core premise that biology is
 *   irrelevant to category membership.
 *
 * KEY AGENTS:
 *   - transgender_women: Primary beneficiary (moderate/identity_locked) â gain categorical recognition and access.
 *   - natal_females: Primary target (organized/constrained) â lose sex-based protections and segregated provisions.
 *   - gender_identity_advocacy_networks: Agenda setter (institutional/arbitrage) â lobbies for and enforces the reading.
 *   - state_administrators: Agenda setter (institutional/mobile) â codifies and administers the definitional rule.
 *   - womens_sex_based_rights_groups: Excluded voice (organized/constrained) â opposes the reading, structurally suppressed.
 *   - academic_researchers: Analytical observer (moderate/constrained) â produces evidence under institutional pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.72).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.68).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Womanhood as Internal Gender Identity (Self-Identification Reading)").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy_law_social_policy").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '646ce6ed-939f-408f-97f0-0f447e5ad09e').
narrative_ontology:cs_kernel_codification('646ce6ed-939f-408f-97f0-0f447e5ad09e', formalized).
narrative_ontology:cs_authority_grounding('646ce6ed-939f-408f-97f0-0f447e5ad09e', lineage).
narrative_ontology:cs_interpretation_layer_present('646ce6ed-939f-408f-97f0-0f447e5ad09e').
narrative_ontology:cs_reading_relation('646ce6ed-939f-408f-97f0-0f447e5ad09e', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('646ce6ed-939f-408f-97f0-0f447e5ad09e', woman_category__intersex_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('646ce6ed-939f-408f-97f0-0f447e5ad09e', foundational, self_identification_defines_womanhood).
narrative_ontology:cs_axiom_status(self_identification_defines_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('646ce6ed-939f-408f-97f0-0f447e5ad09e', self_identification_defines_womanhood, deontological).
narrative_ontology:cs_axiom('646ce6ed-939f-408f-97f0-0f447e5ad09e', foundational, sex_based_distinctions_are_discriminatory).
narrative_ontology:cs_axiom_status(sex_based_distinctions_are_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('646ce6ed-939f-408f-97f0-0f447e5ad09e', sex_based_distinctions_are_discriminatory, deontological).
narrative_ontology:cs_reference_frame('646ce6ed-939f-408f-97f0-0f447e5ad09e', self_determination_framework).
narrative_ontology:cs_drift_state('646ce6ed-939f-408f-97f0-0f447e5ad09e', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('646ce6ed-939f-408f-97f0-0f447e5ad09e', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_females).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition as women through self-identification alone, obtaining identity documents, access to sex-segregated spaces, and inclusion in sports categories aligned with their gender identity. Their lived identity is ratified by state and institutional policy, though this ratification remains contested in practice.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear the dissolution of sex-based protections in law and policy; face new competition in women's sports, potential loss of privacy in intimate spaces such as shelters and changing rooms, and difficulty maintaining female-only associations without legal penalty. They cannot exit the category of female, nor can they easily construct parallel institutions outside the gender identity framework.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_females, payer,
    organized, generational, constrained, national).

% Draft model legislation, lobby governments and institutions, and enforce compliance through human rights complaints, institutional training, and legal pressure. They set the discursive and policy agenda for what counts as legitimate womanhood, and derive institutional funding, status, and career trajectories from the constraint's expansion.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocacy_networks, agenda_setter,
    institutional, generational, arbitrage, global).

% Codify and enforce the definitional rule through identity-document regimes, anti-discrimination law, and public-sector equality duties. They administer the boundary between recognized and unrecognized category membership, facing political pressure from both advocacy networks and opposition movements.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, state_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Argue for retention of biological sex as the basis for the category woman and for preservation of sex-segregated provisions. They are frequently excluded from policy consultations, labeled discriminatory by institutional actors, and denied standing in equality proceedings; their alternative categorization scheme is structurally suppressed.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, womens_sex_based_rights_groups, excluded,
    organized, generational, constrained, national).

% Study the biological, psychological, and social dimensions of sex and gender. Some produce evidence supporting the gender identity reading, while others document performance differentials in sport or clinical differences in medicine; many operate under institutional pressure to align conclusions with the self-identification framework.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, academic_researchers, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves categorical ambiguity for individuals whose gender identity does not align with their sex assigned at birth, supplying a uniform, easily administered rule for membership in the category 'woman' across legal, sporting, and social institutions.
% TRANSFER_FUNCTION: Moves legal recognition, identity-document legitimacy, and access to sex-segregated spaces and opportunities from a sex-based criterion to a self-declaration-based criterion; natal females lose exclusive categorical claims while transgender women gain inclusion.
% ABSENT_VOICES: Natal females who rely on sex-segregated spaces for safety and fairness, and researchers who treat biological sex as non-trivial for medicine and sport, are regularly excluded from policy-making or dismissed as acting in bad faith.
% DISAPPEARANCE_RATIONALE: If the self-identification definitional rule vanished, legal gender recognition regimes, sports eligibility criteria, and equality-law interpretations would revert to sex-based sorting; the institutional infrastructure built to administer and enforce gender-identity-based categorization would lose its primary function.
% FOUNDING_PROBLEM: Transgender individuals faced categorical exclusion and misidentification in legal and social systems that sorted them strictly by birth anatomy, producing lack of documentation, barriers to services, and discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Trans-led advocacy organizations and human rights institutions attest to the founding problem from within the beneficiary set. Independent medical and sports-science researchers corroborate the existence of categorical difficulties but dispute that self-identification without gatekeeping is the appropriate remedy; state equality bodies frequently adopt the advocacy framing without independent corroboration.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the reading's application to sports and sex-segregated spaces extracts material opportunities and safety guarantees from natal females. Suppression (0.68) is high because the reading's persistence depends on active institutional suppression of sex-based categorization (sanctions for misgendering, exclusion of dissenting researchers, legal penalties for sex-exclusive association). Theater ratio (0.40) reflects substantial performative maintenance: institutional diversity training, pronoun regimes, and declarative policies that often exceed functional protection. Accessibility collapse (0.45) is incompleteâsex-based alternatives remain widely held but are institutionally suppressed rather than naturally collapsed. Resistance (0.75) is elevated due to sustained opposition from sex-based rights movements, medical dissent, and policy counter-mobilization. Temporal measurements show monotonic increase in all three tracked metrics as the reading moved from marginal academic position to institutional orthodoxy between 2000 and 2024.
 *
 * PERSPECTIVAL GAP:
 *   The transgender women seat experiences the constraint as recognition, legitimacy, and inclusion; the natal females seat experiences the identical constraint as categorical dissolution and extraction of sex-based safeguards. The agenda-setter seats see a civil-rights coordination mechanism solving historical exclusion, while the excluded sex-based rights seat sees an illegitimate redefinition that dissolves material protections. The engine will compute these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women are declared beneficiaries with identity_locked exit: their structural relationship to the constraint is beneficiary-tending but their exit is fused to identity, which prevents full mobility and may moderate the beneficiary subsidy. Natal females are declared victims with constrained exit: they cannot leave the category or create parallel institutions, placing them near the full-target end. Gender identity advocacy networks and state administrators are agenda setters with arbitrage/mobile exit, placing them near the beneficiary end though they do not personally occupy the category. Womens sex-based rights groups are excluded observers with constrained exit, experiencing high directionality as targets of suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâexclusion of transgender individuals from categorical recognitionâwas genuine, but the constraint has undergone scope creep. It now operates as a scaffold that never sunsetted, extending from identity-document coordination (where extraction is lowest) into sports, shelters, and prisons (where extraction is highest). This prevents mislabeling the constraint as pure coordination (it extracts) or pure extraction (it does coordinate recognition in low-stakes domains). The R5 genealogy interview flags contested status, and the temporal series shows extraction accumulation consistent with a tangled rope that thickened over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_specific_epsilon_decomposition,
    'The constraint''s extractiveness varies sharply by application domain (moderate in identity-document policy, high in sports and sex-segregated spaces). Does this variance indicate a single constraint with contextual intensity, or a family of domain-specific constraints that should be decomposed per the epsilon-invariance principle?',
    'Comparative classification analysis: if domain-specific stories produce non-overlapping epsilon ranges and distinct beneficiary-victim structures, decompose the kernel into per-domain constraints.',
    'If decomposable, the current high-epsilon aggregate masks a low-epsilon coordination function in identity documents and a high-epsilon extraction function in sports and spaces; failure to decompose contaminates the corpus with unstable epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specific_epsilon_decomposition, conceptual, 'Whether domain variance requires epsilon-invariant decomposition').

omega_variable(
    victim_set_framing_ambiguity,
    'Source material identifies people who identify as women as the victim set, yet structural analysis places transgender women in the beneficiary seat and natal females in the payer seat. Are transgender women simultaneously beneficiaries of categorical inclusion and victims of backlash or conflation costs that shift their effective directionality?',
    'Post-implementation outcome tracking: measure material harms (violence, healthcare delay, social stigma) experienced by transgender women after categorical inclusion, compared to pre-reform baselines.',
    'If inclusion produces net material harm via backlash, the beneficiary seat would recompute toward symmetric or target directionality, converting the constraint from tangled rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_framing_ambiguity, empirical, 'Whether transgender women bear hidden extraction costs despite beneficiary framing').

omega_variable(
    foreclosure_or_coexistence,
    'Does the gender identity reading logically foreclose the sex biology and intersex accommodation readings in all possible institutional frameworks, or can polycentric or jurisdictionally differentiated arrangements allow both definitional schemas to remain live?',
    'Comparative jurisdictional analysis: examine legal systems that maintain both sex-based and identity-based registers in different policy domains.',
    'If coexistence is structurally possible, the authored forecloses relations should be downgraded to influences, altering the kernel''s contamination propagation topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_or_coexistence, conceptual, 'Whether gender identity reading logically excludes biological readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wcgir_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wcgir_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(wcgir_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(wcgir_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(wcgir_tr_t16, woman_category__gender_identity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(wcgir_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(wcgir_tr_t24, woman_category__gender_identity_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(wcgir_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(wcgir_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(wcgir_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(wcgir_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(wcgir_be_t16, woman_category__gender_identity_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(wcgir_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(wcgir_be_t24, woman_category__gender_identity_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(wcgir_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(wcgir_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(wcgir_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(wcgir_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(wcgir_su_t16, woman_category__gender_identity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(wcgir_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(wcgir_su_t24, woman_category__gender_identity_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% The natural-language category 'woman' decomposes into three structurally distinct constraints under the epsilon-invariance principle: the gender identity reading (self-identification, high extraction in segregated spaces), the sex biology reading (anatomical/chromosomal, high extraction for trans women), and the intersex accommodation reading (biological spectrum, moderate extraction for atypical bodies). Each carries a non-overlapping epsilon and victim/beneficiary structure, warranting separate stories linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
