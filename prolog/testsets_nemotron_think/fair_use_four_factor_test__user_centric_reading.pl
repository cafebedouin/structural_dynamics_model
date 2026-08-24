% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The fair use doctrine (17 U.S.C. §107) establishes a four-factor
 *   balancing test for unauthorized uses of copyrighted works. This
 *   constraint story captures the USER-CENTRIC READING: fair use as an
 *   affirmative user right, where the four factors are weighed to preserve
 *   public access and cultural production. From this reading, extraction is
 *   low (ε=0.22) because the constraint primarily enables rather than
 *   extracts — it transfers decision rights to users for socially valuable
 *   uses. Rights holders are the victims (reduced licensing control), but the
 *   constraint's coordination function (solving the copyright paradox) is
 *   genuine and dominant. This reading coexists with two sibling readings of
 *   the same kernel: the creator-centric reading (fair use as narrow
 *   exception preserving incentives) and the transformative-use reading
 *   (transformativeness dominates). All three are live judicial positions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.22).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.35).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'd30cff02-8cab-4352-b97a-74e3fcb3e948').
narrative_ontology:cs_kernel_codification('d30cff02-8cab-4352-b97a-74e3fcb3e948', formalized).
narrative_ontology:cs_authority_grounding('d30cff02-8cab-4352-b97a-74e3fcb3e948', lineage).
narrative_ontology:cs_interpretation_layer_present('d30cff02-8cab-4352-b97a-74e3fcb3e948').
narrative_ontology:cs_reading_relation('d30cff02-8cab-4352-b97a-74e3fcb3e948', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('d30cff02-8cab-4352-b97a-74e3fcb3e948', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('d30cff02-8cab-4352-b97a-74e3fcb3e948', foundational, fair_use_as_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('d30cff02-8cab-4352-b97a-74e3fcb3e948', fair_use_as_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('d30cff02-8cab-4352-b97a-74e3fcb3e948', foundational, public_access_as_copyright_purpose).
narrative_ontology:cs_axiom_status(public_access_as_copyright_purpose, holdable).
narrative_ontology:cs_axiom_grounding('d30cff02-8cab-4352-b97a-74e3fcb3e948', public_access_as_copyright_purpose, instrumental).
narrative_ontology:cs_reference_frame('d30cff02-8cab-4352-b97a-74e3fcb3e948', statutory_four_factor_balancing).
narrative_ontology:cs_drift_state('d30cff02-8cab-4352-b97a-74e3fcb3e948', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d30cff02-8cab-4352-b97a-74e3fcb3e948', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, researchers_scholars).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, individual_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, commercial_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, copyright_purpose_is_public_access).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, fair_use_as_affirmative_right).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, cultural_production_requires_breathing_room).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on fair use for everyday activities: quoting, sharing, remixing, criticism, education, accessibility. The four-factor test protects these uses when they serve public access. Exit is constrained — users cannot individually negotiate licenses for every snippet or transformative use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    organized, biographical, constrained, national).

% Universities, schools, and training programs depend on fair use for teaching materials, course reserves, distance learning, and student projects. They have institutional legal counsel and collective licensing options (exit: mobile), but fair use remains the primary enabler of pedagogical freedom.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    institutional, generational, mobile, national).

% Preservation, digitization, interlibrary loan, and access for patrons with disabilities all rely on fair use. Their mission is public access; fair use is the legal infrastructure. Exit is constrained by mission — they cannot simply license everything at scale.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_archives, beneficiary,
    institutional, generational, constrained, national).

% Artists, writers, musicians, filmmakers, and content creators who build on existing culture. They benefit as users (quotation, parody, remix, critique) but also pay as rights holders when their own works are used. Dual position: fair use protects their creative intake but limits their control over output.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_producers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, cultural_producers, payer).

% Text and data mining, citation, reproduction for analysis, and scholarly sharing depend on fair use. Their work advances knowledge; the four-factor test's purpose prong favors nonprofit research. Exit is constrained by the nature of scholarship — you cannot license every data point.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, researchers_scholars, beneficiary,
    moderate, biographical, constrained, global).

% Corporate copyright owners (studios, labels, publishers, software companies) who hold large portfolios. They experience fair use as loss of licensing revenue and control. They have arbitrage-grade exit: they can lobby for legislative narrowing, pursue strategic litigation, deploy technological protection measures, and shift business models.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, rights_holders, payer,
    institutional, generational, arbitrage, global).

% Working artists, writers, musicians without institutional backing. They lose potential licensing income when uses are deemed fair, but lack resources to monitor, enforce, or litigate. Trapped: they cannot practically exit the copyright system, and fair use erodes the narrow revenue streams they depend on.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, individual_creators, payer,
    powerless, biographical, trapped, national).

% Book, journal, and media publishers who invest in creation and distribution. Fair use limits their ability to monetize every downstream use (course packs, library lending, search snippets). Constrained exit: they can shift to licensed models but face market pressure from free/fair alternatives.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_publishers, payer,
    powerful, biographical, constrained, global).

% Federal courts (especially appellate) articulate and apply the four-factor test. Their decisions define the boundary. They do not collect rents from fair use; they administer the balancing. Analytical exit: they interpret law, not make policy, but their interpretations become de facto policy.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Congress enacted the four-factor test in §107 and can amend it. They respond to lobbying from rights holders and public interest groups. Analytical exit: they set the statutory framework but rarely revisit fair use directly; the courts do the operational work.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legislators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the copyright paradox: how to grant creators exclusive rights to incentivize production, while preventing those rights from blocking the very cultural exchange and follow-on creativity that sustains a living culture. The four-factor test coordinates this balance case-by-case rather than by rigid rule.
% TRANSFER_FUNCTION: Moves decision-making authority over certain unauthorized uses from rights holders to users — specifically, the power to copy, distribute, perform, display, or create derivative works without permission or payment when the four factors weigh in favor. The transfer is from rights holder control to user liberty, calibrated by purpose, nature, amount, and market effect.
% ABSENT_VOICES: Individual creators in the global south and developing economies who lack representation in US/EU policy debates; users in jurisdictions without fair use/fair dealing equivalents who face stricter enforcement; future generations whose cultural raw material is shaped by today's fair use boundaries. These voices are structurally excluded from the courtroom and legislative hearings where the test is calibrated.
% DISAPPEARANCE_RATIONALE: If fair use vanished overnight, every quotation, parody, critique, educational excerpt, search index, and transformative remix would require a license. The transaction costs would paralyze cultural production, education, journalism, and digital innovation. Rights holders would capture all downstream value; users would lose the breathing room that makes culture cumulative. The world would rearrange into a permission culture.
% FOUNDING_PROBLEM: Copyright's exclusive rights, if absolute, would prevent the very follow-on creativity, criticism, education, and cultural dialogue that a healthy expressive ecosystem requires. The founding problem is how to prevent copyright from becoming a barrier to the progress it is constitutionally designed to promote.
% FOUNDING_PROBLEM_CORROBORATION: Supreme Court in Campbell v. Acuff-Rose (1994) and Google v. Oracle (2021) affirmed fair use as safeguarding copyright's constitutional purpose. Legislative history of the 1976 Act shows Congress intended fair use as a flexible doctrine, not a narrow exception. Scholars across the ideological spectrum (Litman, Samuelson, Tushnet, Lessig) attest the problem persists — digital technology has only amplified the tension.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness reflects the user-centric framing: the constraint's primary operation is enabling, not taking. The 0.22 epsilon represents the marginal loss of licensing revenue to rights holders from uses deemed fair — real but bounded. Suppression (0.35) reflects that fair use requires judicial enforcement to exist at all (it's a defense, not a right you exercise without risk), but the suppression is on rights holders' ability to block, not on users. Theater is low (0.18) because the four-factor test genuinely coordinates — courts actually balance, they don't just perform. Accessibility collapse (0.42) is moderate: without fair use, many uses would be impossible to license, but some would find alternatives. Resistance (0.48) is moderate: rights holders litigate aggressively, but users and institutions also defend the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different types for different seats. From the public_user seat (beneficiary, organized, constrained exit), the constraint computes as rope — genuine coordination with minimal extraction. From the individual_creator seat (payer, powerless, trapped), it may compute as tangled_rope or snare — coordination exists but extraction is felt acutely with no exit. From the rights_holder seat (payer, institutional, arbitrage exit), it computes as rope with a side payment — they lose some control but retain the overall system. The user-centric reading claims 'rope' overall; the engine's per-seat divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users, educational institutions, libraries, and researchers are structural beneficiaries (d ~ 0.15-0.25): the constraint subsidizes their access. Cultural producers are near-symmetric (d ~ 0.5): they benefit as users, pay as rights holders. Rights holders, individual creators, and commercial publishers are targets (d ~ 0.7-0.85): they bear the cost of lost control/revenue. Courts and legislators are analytical/agenda-setting (d ~ 0.0): they administer, not collect. The derivation follows from beneficiary/victim declarations plus exit options: institutional rights holders have arbitrage exit (lobbying, TPMs, litigation), pushing their effective extraction down; individual creators are trapped, pushing theirs up.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (copyright blocking the progress it aims to promote) remains live — digital technology has intensified, not resolved, the tension. The constraint has not atrophied; its coordination function has expanded with new technologies (search, TDM, AI training). Mandatrophy is not resolved. The user-centric reading sees the doctrine as more necessary now, not less.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the user-centric reading of the four-factor test a distinct constraint with its own ε, or merely a rhetorical framing of the same legal doctrine?',
    'Compare case outcomes and litigation behavior under each reading: if courts applying the user-centric frame systematically reach different results (more findings of fair use, broader scope) than courts applying the creator-centric frame, the readings instantiate different constraints.',
    'If distinct constraints, each gets its own ε, stakeholder structure, and classification. If same constraint, the ε variance is measurement noise and the kernel should be modeled as one story with perspectival variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of the fair use kernel are structurally distinct constraints or interpretive variants of one constraint.').

omega_variable(
    epsilon_measurement_ambiguity,
    'Does the low ε (0.22) authored here reflect the constraint''s actual extraction on users, or the reading''s normative commitment to low extraction?',
    'Empirical study of licensing markets: what fraction of uses currently deemed fair would actually be licensed if fair use disappeared? If the licensing market would capture most of these uses at non-trivial rates, the reading''s low ε understates the constraint''s actual transfer magnitude.',
    'If ε is higher under empirical measurement, the user-centric reading''s claim of ''rope'' may not hold — the constraint could be tangled_rope (coordination + asymmetric extraction) from a neutral measurement stance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epsilon_measurement_ambiguity, empirical, 'Whether the authored extractiveness reflects the reading''s normative frame or the constraint''s empirical transfer magnitude.').

omega_variable(
    beneficiary_structure_contest,
    'Are public/educational users the primary beneficiaries, or do commercial technology platforms (search engines, AI training, social media) capture the majority of fair use''s economic value?',
    'Economic analysis of fair use-dependent industries: measure revenue attributable to fair use for public/educational users vs. commercial platforms. The Authors Guild v. Google and recent AI training litigation test this boundary.',
    'If commercial platforms are the primary beneficiaries, the user-centric reading''s beneficiary declaration misidentifies the coordination function — the constraint coordinates platform business models, not public access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_contest, empirical, 'Whether the declared beneficiaries match the actual capture of fair use''s economic gains.').

omega_variable(
    transformative_use_boundary,
    'Where does transformative use end and market substitution begin? The user-centric reading treats transformativeness as a purpose-factor amplifier; the transformative-use reading makes it dominant.',
    'Track appellate decisions: if transformativeness increasingly displaces the other three factors (especially market harm), the readings diverge structurally. The Warhol Foundation v. Goldsmith (2023) decision suggests the Supreme Court may be narrowing the transformative-use reading.',
    'If the boundary shifts toward the transformative-use reading, the user-centric reading''s ε may rise (more uses deemed fair = more extraction from rights holders). If it shifts toward creator-centric, ε falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_boundary, conceptual, 'The structural boundary between the user-centric and transformative-use readings hinges on how transformativeness weights the four factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_user_centric_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_use_user_centric_tr_t1985, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(fair_use_user_centric_tr_t1994, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(fair_use_user_centric_tr_t2005, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(fair_use_user_centric_tr_t2015, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(fair_use_user_centric_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(fair_use_user_centric_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.15).
narrative_ontology:measurement(fair_use_user_centric_be_t1985, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(fair_use_user_centric_be_t1994, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1994, 0.2).
narrative_ontology:measurement(fair_use_user_centric_be_t2005, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(fair_use_user_centric_be_t2015, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(fair_use_user_centric_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_user_centric_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(fair_use_user_centric_su_t1985, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(fair_use_user_centric_su_t1994, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1994, 0.32).
narrative_ontology:measurement(fair_use_user_centric_su_t2005, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(fair_use_user_centric_su_t2015, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(fair_use_user_centric_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, information_standard).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__user_centric_reading, 0.02).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fair_use_four_factor_test kernel. The user-centric reading (this file) claims low ε and names users as beneficiaries. The creator-centric reading claims higher ε and names creators as beneficiaries. The transformative-use reading claims moderate ε but different factor weighting. All three share the same statutory text (§107) but instantiate different constraints with different ε, stakeholders, and classifications. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, powerless, 0.85).
constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, powerful, 0.75).
constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, analytical, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
