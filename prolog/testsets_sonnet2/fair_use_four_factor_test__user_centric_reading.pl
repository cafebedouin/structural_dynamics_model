% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Fair Use as Affirmative User Right (User-Centric Reading)
 *   domain: legal/cultural production
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested fair-use kernel: the
 *   user-centric reading, which treats fair use not as a narrow carve-out
 *   from an otherwise absolute property right (the creator_centric_reading)
 *   and not as a doctrine dominated by the transformativeness inquiry (the
 *   transformative_use_reading), but as an affirmative right belonging to
 *   users — students, libraries, critics, the public — that the four
 *   statutory factors exist to protect. Under this reading, courts weigh
 *   purpose (especially public-benefit, nonprofit, and educational purpose)
 *   and market effect (discounted heavily when the use serves public access
 *   rather than substituting for the original) as the dominant factors, with
 *   amount-used and nature-of-work playing supporting roles. Extraction on
 *   unauthorized use is authored low: the doctrine's own framework, on this
 *   reading's terms, treats such use as socially sanctioned rather than as a
 *   taking from rights holders. This is a distinct constraint from its
 *   siblings, not the same doctrine measured differently — each reading
 *   produces a genuinely different beneficiary/victim structure and a
 *   genuinely different ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.22).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.15).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/cultural production").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '42e51f70-8139-4337-802c-90e8975e3569').
narrative_ontology:cs_kernel_codification('42e51f70-8139-4337-802c-90e8975e3569', fixed_text).
narrative_ontology:cs_authority_grounding('42e51f70-8139-4337-802c-90e8975e3569', practice).
narrative_ontology:cs_interpretation_layer_present('42e51f70-8139-4337-802c-90e8975e3569').
narrative_ontology:cs_reading_relation('42e51f70-8139-4337-802c-90e8975e3569', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('42e51f70-8139-4337-802c-90e8975e3569', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('42e51f70-8139-4337-802c-90e8975e3569', foundational, fair_use_is_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_is_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('42e51f70-8139-4337-802c-90e8975e3569', fair_use_is_affirmative_user_right, conventional).
narrative_ontology:cs_axiom('42e51f70-8139-4337-802c-90e8975e3569', secondary, public_access_purpose_discounts_market_harm).
narrative_ontology:cs_axiom_status(public_access_purpose_discounts_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('42e51f70-8139-4337-802c-90e8975e3569', public_access_purpose_discounts_market_harm, instrumental).
narrative_ontology:cs_reference_frame('42e51f70-8139-4337-802c-90e8975e3569', campbell_public_purpose_framework).
narrative_ontology:cs_drift_state('42e51f70-8139-4337-802c-90e8975e3569', post_google_v_oracle_2021, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42e51f70-8139-4337-802c-90e8975e3569', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educators_and_students).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, follow_on_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, general_public).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, individual_rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, small_press_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Copy, quote, and screen portions of copyrighted works for teaching and research without seeking permission or paying licensing fees, relying on fair use to make classroom and scholarly use lawful. Their access to cultural and scientific material would shrink significantly without the doctrine's protection.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educators_and_students, beneficiary,
    moderate, biographical, mobile, national).

% Preserve, digitize, and lend materials — including orphan works and out-of-print texts — under fair use rationale, treating preservation and public access as the core justification for reduced permission-seeking.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_and_archives, beneficiary,
    organized, generational, mobile, national).

% Incorporate clips, quotations, and excerpts into commentary, criticism, and nonfiction works, relying on the user-right reading to license-free use where paying for every excerpt would be prohibitive or would let rights holders veto criticism.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, documentarians_and_critics, beneficiary,
    moderate, biographical, constrained, national).

% Build new cultural works — fan works, remixes, parody, commentary — that draw on existing copyrighted material. Under this reading their access to raw cultural material is treated as a right to be preserved, not a privilege rights holders may withhold.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, follow_on_creators, beneficiary,
    powerless, biographical, constrained, national).

% Benefits diffusely from wider circulation of ideas, cheaper access to knowledge, and a richer public domain of derivative commentary and criticism that this reading treats as the doctrine's central purpose.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, general_public, beneficiary,
    powerless, civilizational, constrained, national).

% Authors, photographers, and musicians whose works are used without permission or payment under a fair-use finding. They bear lost licensing revenue and reduced control over how and where their work circulates; their only recourse is costly, uncertain litigation with the four factors weighted toward the user.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, individual_rights_holders, payer,
    moderate, biographical, constrained, national).

% Rely on licensing revenue from excerpting and anthology rights; when courts read fair use expansively to favor public-access uses, licensing markets for their catalogs shrink and they cannot absorb the litigation costs to test close cases the way major rights-holding conglomerates can.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, small_press_publishers, payer,
    moderate, biographical, constrained, national).

% Apply and articulate the four-factor test case by case, deciding how much weight purpose-and-character, nature-of-the-work, amount-used, and market-effect carry. Under this reading, courts treat factor one (purpose, especially public-benefit and educational purpose) and factor four (market effect, discounted when the use serves public access) as anchors, shaping how much unauthorized use survives judicial review.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts_and_doctrine_architects, agenda_setter,
    institutional, generational, analytical, national).

% Represent rights holders in negotiating blanket licenses; would prefer a narrower fair-use doctrine that channels more uses into paid licensing markets they administer. Their preferred remedy — mandatory licensing for institutional and educational users — is largely foreclosed by the user-centric reading's presumption against required payment.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, collective_licensing_organizations, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows society to use small, purposeful portions of copyrighted works for teaching, criticism, preservation, and commentary without a permission market that would otherwise block or price out socially valuable uses — solving the transaction-cost and holdout problems that a pure permission regime would create.
% TRANSFER_FUNCTION: Moves the economic value of unlicensed excerpting and access from rights holders (who would otherwise collect licensing fees) to educational institutions, libraries, critics, and the broader public who consume the resulting cultural and educational output without payment.
% ABSENT_VOICES: Individual authors and small rights holders with weak bargaining power relative to large educational institutions and technology platforms rarely appear before the courts shaping doctrine; collective licensing organizations that would represent their interests are structurally sidelined by a reading that presumes against required payment.
% DISAPPEARANCE_RATIONALE: If this reading of fair use disappeared and were replaced by a strict permission-and-payment regime, licensing markets for excerpts, classroom materials, and commentary clips would expand dramatically; libraries, schools, and documentarians would face new transaction costs and some uses would simply not occur; rights holders would see new revenue streams open.
% FOUNDING_PROBLEM: Copyright's exclusive rights, if absolute, would let rights holders block or price out socially valuable uses — teaching, criticism, scholarship, preservation — that depend on quoting, excerpting, or reproducing existing work, and would let them use licensing leverage to suppress unflattering commentary or slow the spread of knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Library associations, educational institutions, and public-interest legal scholars attest the founding problem remains live and that a user-right reading is necessary to prevent copyright from functioning as a veto over criticism and access. Rights-holder groups and licensing collectives dispute the framing, arguing the doctrine has drifted from a narrow, incentive-preserving exception into a broad license-substitute that erodes the very production incentives copyright exists to protect — a dispute this story does not resolve but documents as the founding-problem contest.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.22) because the user-centric reading's own internal logic treats unauthorized use protected by the doctrine as legitimate exercise of a public right, not extraction from rights holders — the reading does not concede that unlicensed use is a wrong being excused. Suppression is low (0.15) because the doctrine is permissive rather than coercive: it does not compel rights holders to do anything, it withholds a cause of action. Resistance is moderate-high (0.55) because rights holders and licensing collectives actively litigate and lobby against the doctrine's expansive application, seeking narrower judicial interpretation or legislative reform (e.g., extended collective licensing mandates). Theater ratio is low but has drifted upward slightly (0.10 to 0.20) as institutional fair-use compliance offices and formal 'fair use checklists' have proliferated in universities and libraries — some of this activity is genuine risk assessment, some is defensive paperwork with limited bearing on actual four-factor outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Educators, libraries, documentarians, follow-on creators, and the general public are declared beneficiaries: the doctrine, on this reading, exists FOR them, and they benefit directly from reduced permission costs, so their derived directionality sits near the beneficiary end. Individual rights holders and small press publishers are declared victims: they bear the lost licensing revenue and diminished control that the user-right reading treats as the acceptable cost of a functioning public sphere, so their derived directionality sits toward the target end, especially given their constrained exit — an author cannot opt out of having their work fair-used by a documentarian who successfully invokes the doctrine. Courts are the agenda-setting institutional seat: their case-by-case weighting IS the constraint's operative mechanism, and their exit option is properly analytical since they administer rather than experience the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The user-centric reading resists mislabeling as pure extraction (from the rights holder's vantage) or pure coordination (from the pure public-goods vantage) by insisting the four-factor weighing is real and outcome-determinative, not a rubber stamp: some uses lose under this reading too (e.g., wholesale reproduction with no public-benefit purpose and clear market substitution). The doctrine's mandate — preserving public access and cultural production against overbroad property claims — remains live rather than obsolete: digital reproduction and AI-era content aggregation have if anything intensified the founding problem (permission-market friction blocking valuable public-facing use), which is why founding_problem_status is authored contested rather than dead. If the transformativeness inquiry (sibling reading) came to dominate court practice entirely, this reading's public-access framing would be structurally weakened without being logically foreclosed — that is an influences relationship, not a forecloses one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_right_vs_narrow_exception_framing,
    'Is fair use structurally an affirmative right held by users (this reading), or a narrow, defeasible exception carved out of an otherwise-absolute property right (the creator_centric_reading)? US case law and scholarship do not speak with one voice on this — the Supreme Court has at times described fair use as a ''right'' (Campbell v. Acuff-Rose) and at other times treated it as a defense to be construed narrowly.',
    'Doctrinal history analysis of how courts characterize the burden of proof and rhetorical framing across circuits and eras; a right-framing typically shifts persuasive weight toward the user even holding the four factors'' substance constant.',
    'If courts systematically shift toward the narrow-exception framing, effective ε for unauthorized use under real-world adjudication rises even without a change in statutory text — the reading this story authors would itself be drifting toward its sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_right_vs_narrow_exception_framing, conceptual, 'Whether the right-vs-exception framing is doctrinally settled or genuinely contested across the case law this reading draws on.').

omega_variable(
    public_benefit_discount_scope,
    'How far does the public-access discount on market-effect analysis (factor four) extend before it collapses into a wholesale override of rights-holder compensation for any use claiming public benefit?',
    'Track case outcomes where public-benefit purpose is claimed but a licensing market for the specific use plainly exists (e.g., textbook excerpting where clearance services operate) — a finding of fair use in such cases would indicate the discount has expanded beyond genuine market-failure correction.',
    'A narrow discount (applied only where licensing markets are genuinely absent or prohibitively costly) keeps this reading''s low ε defensible; an expansive discount (applied even where functioning licensing markets exist) would mean the reading is authoring extraction lower than the doctrine''s actual operation warrants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_benefit_discount_scope, empirical, 'Whether the public-benefit discount on market harm is bounded by market-failure or has become a general override.').

omega_variable(
    sibling_reading_dominance_shift,
    'Which reading actually dominates current appellate practice — is the transformative_use_reading''s focus on new-meaning-added the operative test in most circuits, making this user-centric framing more aspirational than descriptive of contemporary adjudication?',
    'Systematic review of post-Google v. Oracle (2021) circuit court opinions for which factor combination is dispositive in outcome-determinative cases.',
    'If transformativeness dominates in practice, this reading describes a minority or historical judicial posture rather than the live operative test, which would not change this story''s authored ε (a reading-indexed value) but would bear on how much real-world weight to give this reading relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_dominance_shift, empirical, 'Whether the user-centric framing or the transformative-use framing is the dominant judicial posture in current practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(fair_tr_t2018, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2018, 0.19).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.15).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(fair_be_t2018, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__user_centric_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the fair_use_four_factor_test kernel, decomposed per the ε-invariance principle: creator_centric_reading (property-right-primary, high ε on unauthorized use), transformative_use_reading (transformativeness-dominant, ε contingent on new-meaning finding), and this user_centric_reading (user-right-primary, low ε, public-access beneficiary set). Each reading shares the same statutory four-factor text but reads the weighting and the doctrine's purpose differently, producing genuinely different beneficiary/victim structures and different ε — not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
