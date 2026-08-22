% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_transformative_right_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use Statutory Exception — Transformative Right Reading
 *   domain: intellectual_property_law/legal_interpretation
 *
 * SUMMARY:
 *   Fair use is a statutory exception to copyright that permits certain
 *   reuses without the copyright holder's permission. This story instantiates
 *   the 'transformative right' reading: fair use exists fundamentally to
 *   enable transformative creativity and cultural production; courts have a
 *   duty to recognize transformative uses broadly and facilitate innovation;
 *   the test focuses on whether the new use adds new meaning or message, not
 *   on whether a licensing market could theoretically exist. This reading
 *   contrasts with the 'market licensing' reading (fair use exists only where
 *   no licensing market exists, and licensing harm is the primary factor) and
 *   the 'narrow defense' reading (copyright is property; fair use is a narrow
 *   affirmative defense to preserve market value). The transformative reading
 *   claims low extractiveness for qualifying transformative uses (courts
 *   should recognize them; licensing markets are not dispositive) while
 *   maintaining moderate extractiveness overall because courts' application
 *   of the standard is contested and because copyright holders constantly
 *   litigate to narrow the doctrine.
 *
 * KEY AGENTS:
 *   - Transformative reusers (remix artists, samplers, commentators) — benefit from the exception; exit if licensing is required.
 *   - Copyright holders (studios, publishers, record labels) — excluded from deciding transformativeness; excluded stakeholders; bear costs of uncompensated reuse under this reading.
 *   - Courts (interpreters of the doctrine) — agenda setters; decide what counts as transformative.
 *   - Scholarship and criticism (academic institutions) — beneficiaries; depend on quotation and fair use for core function.
 *   - Congress (codifier of the statutory standard) — observer; wrote § 107 but courts interpret it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.38).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.42).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use Statutory Exception — Transformative Right Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '3701d49d-400c-44eb-8158-490bfa0e2c45').
narrative_ontology:cs_kernel_codification('3701d49d-400c-44eb-8158-490bfa0e2c45', fixed_text).
narrative_ontology:cs_authority_grounding('3701d49d-400c-44eb-8158-490bfa0e2c45', lineage).
narrative_ontology:cs_interpretation_layer_present('3701d49d-400c-44eb-8158-490bfa0e2c45').
narrative_ontology:cs_reading_relation('3701d49d-400c-44eb-8158-490bfa0e2c45', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_reading_relation('3701d49d-400c-44eb-8158-490bfa0e2c45', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('3701d49d-400c-44eb-8158-490bfa0e2c45', foundational, transformative_uses_require_protection).
narrative_ontology:cs_axiom_status(transformative_uses_require_protection, holdable).
narrative_ontology:cs_axiom_grounding('3701d49d-400c-44eb-8158-490bfa0e2c45', transformative_uses_require_protection, deontological).
narrative_ontology:cs_axiom('3701d49d-400c-44eb-8158-490bfa0e2c45', foundational, courts_must_balance_innovation_against_copyright_control).
narrative_ontology:cs_axiom_status(courts_must_balance_innovation_against_copyright_control, holdable).
narrative_ontology:cs_axiom_grounding('3701d49d-400c-44eb-8158-490bfa0e2c45', courts_must_balance_innovation_against_copyright_control, deontological).
narrative_ontology:cs_axiom('3701d49d-400c-44eb-8158-490bfa0e2c45', secondary, licensing_markets_not_dispositive_to_fair_use).
narrative_ontology:cs_axiom_status(licensing_markets_not_dispositive_to_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('3701d49d-400c-44eb-8158-490bfa0e2c45', licensing_markets_not_dispositive_to_fair_use, empirically_contingent).
narrative_ontology:cs_reference_frame('3701d49d-400c-44eb-8158-490bfa0e2c45', copyright_act_balanced_incentive_plus_public_interest).
narrative_ontology:cs_drift_state('3701d49d-400c-44eb-8158-490bfa0e2c45', contemporary_copyright_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3701d49d-400c-44eb-8158-490bfa0e2c45', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_reusers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, remix_artists).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, scholarship_and_criticism).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, technology_platforms_and_creators).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, copyright_as_incentive_structure_not_absolute_property).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformativeness_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_coordination_via_exception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, filmmakers, musicians, and creators who build on or incorporate existing copyrighted works as part of their creative process. They depend on fair use to make commentary, satire, remix, and derivative works without licensing from upstream copyright holders. Their exit option is abandoning the work or seeking licenses (often expensive or refused); the constraint permits them to proceed without permission when transformation is genuine.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_reusers, beneficiary,
    moderate, biographical, constrained, global).

% Academic institutions, critics, and researchers who cite, quote, and analyze copyrighted works as essential to scholarship. They need to reproduce excerpts and images to build arguments and advance knowledge. Licensing every excerpt would be economically prohibitive and would give copyright holders veto power over critical analysis.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, scholarship_and_criticism, beneficiary,
    organized, generational, constrained, global).

% Publishers, theaters, documentarians, and cultural institutions that produce works engaging with existing material. They use fair use to include clips, sample dialogue, and visual references without licensing. Without fair use, cultural conversation would be filtered through copyright holders' commercial interests.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_producers, beneficiary,
    moderate, biographical, constrained, global).

% The collective interest in maintaining a functioning public domain and preventing enclosure of cultural material through aggressive copyright enforcement. Fair use doctrine preserves this public good by carving out space where works can be studied, built upon, and circulated even after copyright protection attaches.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain_access, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__transformative_right_reading, public_domain_access).

% Publishers, film studios, music labels, and rights holders who depend on licensing revenue and exclusive market control. Under this reading, they are excluded from the fair use determination unless they can prove harm to their licensing market. They argue fair use undermines their ability to monetize derivative uses and that transformativeness is an undefined standard that lets reusers escape paying for permission.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders, excluded,
    institutional, generational, trapped, global).

% Interpret the fair use doctrine and decide whether a specific use qualifies. Under this reading, courts have discretion to facilitate innovation by recognizing transformative uses and applying fair use broadly; they are tasked with balancing copyright's incentive function against the public interest in cultural production.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Wrote the Copyright Act and the fair use provision (17 U.S.C. § 107). They codified transformativeness as a legal standard and set the framework courts interpret. They could amend the statute to narrow or eliminate fair use, but have not; their silence is read by this reading as endorsement of the doctrine.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, congress_and_legislators, observer,
    institutional, generational, analytical, national).

% Social media platforms, video hosts, and technology companies that enable user-generated content. They rely on fair use and the DMCA safe harbor to operate platforms where users remix, parody, and create derivatives. Without fair use, platform liability for user uploads would be crippling.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, technology_platforms_and_creators, beneficiary,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cultural production and knowledge-building by guaranteeing that creators can quote, sample, critique, and build upon existing works without needing permission from upstream copyright holders. This enables cumulative creativity and critical discourse to function without centralized gatekeepers.
% TRANSFER_FUNCTION: Transfers the power to prevent certain reuses from copyright holders to courts and creators: copyright holders lose the exclusive right to control transformative uses and derivative works that transform the original's purpose or meaning. Courts gain authority to determine transformativeness; reusers gain freedom from licensing requirements for qualifying uses.
% ABSENT_VOICES: Copyright holders excluded from the fair use determination itself (their interests are considered via the four-factor test, but they do not decide what counts as transformative). Collective management organizations and licensing bodies would argue for mandatory licensing regimes covering all sampling and quotation rather than fair use exceptions. Jurisdictions outside the U.S. with different copyright frameworks (where fair use is narrower or absent) have different stakeholders in this dispute.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished and courts adopted the market_licensing_reading instead, scholarship would require licenses for every quotation, remix culture would collapse, and criticism would be filtered through copyright holders' commercial interests. Digital culture, academic publishing, and documentary film would reorganize around licensing infrastructure or self-censor.
% FOUNDING_PROBLEM: Copyright's scope threatened to encroach on speech, scholarship, and cultural participation: the Statute of Anne (1710) and the Copyright Clause (1787) recognized that copyright must balance authors' incentives against the public interest in innovation and knowledge. Early judicial recognition of fair use (e.g., Folsom v. Marsh, 1841) created space for legitimate reuse; modern courts extended the doctrine to recognize transformative uses explicitly.
% FOUNDING_PROBLEM_CORROBORATION: Courts cite the incentive-versus-public-interest balance in fair use opinions (Sony, Campbell, Harper & Row). Technology scholars and digital humanities researchers outside the copyright-holder sector attest that cultural production genuinely depends on fair use space. Legislative silence on narrowing fair use despite decades of copyright-holder lobbying is read as corroboration that the problem the doctrine solves remains recognized as legitimate.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).
:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) rather than low because, despite the reading's pro-fair-use framing, courts frequently rule against fair use claimants and copyright holders successfully litigate to establish licensing markets as dispositive (Harper & Row v. Nation; Andy Warhol Foundation v. Goldsmith). The doctrine is contested: courts do not consistently recognize transformativeness; the four-factor test gives weight to market harm; high-profile cases go against the reusers. Suppression is moderate (0.42) because the doctrine itself—when applied favorably—suppresses copyright holders' ability to enforce exclusivity, but enforcement is patchy and contested. Theater is low-moderate (0.28) because the doctrine is legitimately applied sometimes (real transformative uses do qualify) and sometimes theater (courts invoke transformativeness while ruling against reusers on other factors). The measurement series traces rising extractiveness and theater over time as copyright litigation has grown more sophisticated and copyright holders have learned to argue licensing-market harm more effectively (post-Campbell era intensification). Accessibility collapse is moderate (0.62) because the fair use doctrine is codified in statute and case law, but its boundaries are ambiguous—creators cannot be certain whether their use qualifies until litigation occurs.
 *
 * PERSPECTIVAL GAP:
 *   The courts and copyright holders should experience dramatically different types. From the courts' perspective (agenda-setter seat), the constraint is coordinating genuine innovation and cultural freedom against copyright overreach—a rope balancing competing interests. From copyright holders' perspective (excluded, but powerful), the constraint is an unpredictable carve-out that extracts their exclusive control over derivative markets without compensation—a snare. From transformative reusers' perspective (beneficiaries, moderate power), the constraint is coordination that permits their creative practice, but uncertainty about how courts will apply it creates ongoing suppression (they cannot publish derivative works without legal risk). The engine should compute these per-seat divergences from the structural data: different power levels, different exit options, different roles relative to the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative reusers and cultural producers are structural beneficiaries (d toward 0.0): they gain the freedom to create without licensing, face constrained exit options (they would need to license if fair use fails), and rely on courts' favorable interpretation. Copyright holders are structurally ambiguous: they appear as excluded stakeholders (not in the determination, though considered), but they bear real costs (loss of licensing rents, derivative-market control). Courts are the agenda-setter (high power, institutional, analytical time horizon) with d near 0.5 (symmetric: they balance both interests, constrained by the statute but with discretion to interpret it). Scholarship and criticism are beneficiaries with modest power and constrained exit (licensing is economically infeasible for academic quotation). The constraint's effective extraction depends on how courts apply it: if courts recognize transformativeness broadly, χ is low (beneficiaries extract value via the exception); if courts narrow fair use and recognize licensing markets, χ rises (copyright holders' exclusivity is preserved and reusers must license). The measured 0.38 reflects the contested middle ground: courts recognize some transformative uses (extractiveness lower than market_licensing_reading), but copyright holders have successfully argued licensing-market harm in major cases (extractiveness higher than the transformative-right-reading's most optimistic claim).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (copyright's scope threatens speech and cultural participation) is live and actively litigated. Courts consistently cite the need to balance incentive and innovation in fair use opinions. The doctrine has not become obsolete; rather, it is under constant pressure from copyright holders who argue licensing markets are dispositive and transformativeness is too vague. The measured extraction trajectory (rising slightly to 0.38) reflects not mandatrophy (the problem has gone away) but rather enforcement intensity (copyright holders' litigation sophistication has increased). The theater ratio stays low-moderate (0.28) because real transformative uses do qualify and are recognized (not all theater), but the doctrine's boundaries remain ambiguous and courts' application is inconsistent. No mandatrophy signal; the doctrine remains contested and enforceable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_definition_ambiguity,
    'What constitutes a sufficiently ''transformative'' use for fair use purposes? Does transformation require adding new meaning or expression, or does any new use qualify?',
    'Survey courts'' decisions on transformativeness across multiple genres (visual art, music sampling, literary adaptation) to identify consistent criteria. The Andy Warhol Foundation v. Goldsmith decision (2023) provides a recent court-drawn boundary; analyze how future courts apply it.',
    'If transformativeness is narrowly defined (purely new meaning or commentary required), fewer uses qualify and extractiveness rises (copyright holders'' licensing control is preserved). If broadly defined (any new context or purpose counts), more uses qualify and extractiveness falls (reusers'' freedom increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_definition_ambiguity, empirical, 'The boundary condition that determines what counts as a transformative use.').

omega_variable(
    licensing_market_dispositive_vs_factor,
    'Should the existence of a licensing market be dispositive to fair use (automatic denial), or merely one factor among four?',
    'Legislative amendment clarifying the statutory test, or Supreme Court decision establishing hierarchical weight of the four-factor test. Harper & Row v. Nation and Andy Warhol Foundation decisions show courts treating licensing markets heavily; congressional testimony and law review scholarship outside copyright-holder interests provide corroboration on whether the current approach matches the founding intent.',
    'If licensing markets are dispositive, the market_licensing_reading dominates and this reading''s extractiveness rises sharply (fair use becomes narrow exception). If merely a factor, this reading''s extractiveness remains moderate (courts balance licensing harm against transformative function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_dispositive_vs_factor, conceptual, 'Whether licensing-market existence is the controlling principle or one element in fair use analysis.').

omega_variable(
    reading_framework_contention,
    'Do the three sibling readings (transformative_right, market_licensing, narrow_defense) represent genuine alternative interpretations of the same statutory kernel, or do they describe different facts (e.g., different case types)?',
    'Meta-analysis of fair use cases: do courts applying the market_licensing logic and courts applying the transformative_right logic reach different outcomes on factually identical scenarios? If yes, genuine reading contention (same kernel, different readings). If no, the ''readings'' may actually be case-type-specific differences (e.g., commercial uses vs. nonprofit scholarship).',
    'If genuine reading contention, the three stories should coexist as different stakeholder frameworks and the network should link them as coexisting alternatives. If case-type-specific, decompose into separate constraints per case type (statute has different effective force in different domains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framework_contention, conceptual, 'Whether sibling readings are genuinely alternative interpretations of the kernel or case-type variations.').

omega_variable(
    copyright_holder_exclusion_legitimacy,
    'Is the exclusion of copyright holders from the transformativeness determination legitimate, or does it unfairly bias fair use toward reusers?',
    'Constitutional analysis of copyright holders'' due process and property rights (Fifth Amendment takings). Comparative study: do countries with mandatory licensing instead of fair use show different innovation patterns? Copyright economics research on whether exclusion leads to welfare loss.',
    'If exclusion is seen as unfair or unconstitutional, pressure increases to replace fair use with licensing-market-based alternatives, pulling courts toward the market_licensing_reading. If exclusion is legitimate (copyright holders'' interests are considered via factor 4, but they don''t veto), this reading''s exclusion is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_holder_exclusion_legitimacy, preference, 'Whether copyright holders'' structural exclusion from fair use determination is fair policy.').

omega_variable(
    digital_era_scaling_uncertainty,
    'Does fair use''s doctrine scale to the digital era''s ease of copying, remixing, and distribution? Do transformativeness standards developed for print, film, and music sampling apply unchanged to AI-generated works and algorithmic remix?',
    'Case law and legislative action on AI-generated fair use claims (currently emerging). Study transformativeness doctrine''s application to new genres (deepfake, generative AI, algorithmic sampling). Track whether courts recognize these as transformative or as mere substitution.',
    'If transformativeness scaling fails (courts reject digital-era transformations as non-transformative), extractiveness rises and fewer digital reuses qualify as fair. If doctrine scales successfully, extractiveness stays moderate and innovation protection extends to new media.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_era_scaling_uncertainty, empirical, 'Whether the transformativeness doctrine applies coherently to digital-era creation practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t5, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(fair_tr_t5, observed).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(fair_tr_t10, observed).
narrative_ontology:measurement(fair_tr_t15, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(fair_tr_t15, observed).
narrative_ontology:measurement(fair_tr_t25, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(fair_tr_t25, observed).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(fair_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t5, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(fair_be_t5, observed).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(fair_be_t10, observed).
narrative_ontology:measurement(fair_be_t15, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(fair_be_t15, observed).
narrative_ontology:measurement(fair_be_t25, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(fair_be_t25, observed).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(fair_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t5, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(fair_su_t5, observed).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(fair_su_t10, observed).
narrative_ontology:measurement(fair_su_t15, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(fair_su_t15, observed).
narrative_ontology:measurement(fair_su_t25, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(fair_su_t25, observed).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(fair_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_licensing_markets__mandatory_licensing_alternative).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fair use statutory exception kernel (17 U.S.C. § 107). The transformative_right_reading instantiates the pro-innovation, pro-reuser interpretation where courts facilitate transformative uses and licensing markets are not dispositive. The market_licensing_reading restricts fair use by making licensing-market existence the primary factor. The narrow_defense_reading treats copyright as absolute property and fair use as a narrow escape hatch. Each story carries its own ε (this one: 0.38, reflecting contested application of the doctrine), beneficiary/victim structure (this reading: beneficiaries are reusers and cultural producers; copyright holders are excluded but bear costs), and structural data. The three stories coexist as different readings held by different parties and courts; they are not progressions or phases of a single constraint. Link all three via network.affects_constraints to enable analysis of how the reading contest shapes fair use doctrine evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
