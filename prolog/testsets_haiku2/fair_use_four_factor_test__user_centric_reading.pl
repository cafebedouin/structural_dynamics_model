% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The fair use doctrine permits the use of copyrighted material without
 *   permission or license fees under specific circumstances—criticism,
 *   commentary, news reporting, teaching, scholarship, parody. The
 *   four-factor test (purpose and character of use, nature of the copyrighted
 *   work, amount and substantiality of the portion used, effect on the market
 *   for the original) guides courts in determining whether a particular use
 *   qualifies as fair. This story instantiates the USER-CENTRIC READING: fair
 *   use is an affirmative right of users—educational institutions, the
 *   public, cultural producers, and critics—to access and build upon
 *   copyrighted material. The reading centers public benefit, cultural
 *   production, and access over creator compensation and control. It is one
 *   of three structurally distinct readings of the same four-factor kernel
 *   (the creator-centric reading emphasizes preservation of copyright
 *   incentives; the transformative-use reading prioritizes novelty of
 *   expression over market protection). The user-centric reading posits low
 *   baseline extraction because it treats unauthorized use as within-rights
 *   rather than as compensable harm.
 *
 * KEY AGENTS:
 *   - Educational institutions: benefit from fair use to incorporate copyrighted materials into curricula without licensing fees
 *   - Public libraries: rely on fair use to preserve, archive, and provide public access to cultural materials
 *   - Noncommercial cultural producers: depend on fair use to create derivative works, remixes, and fan works without licensing
 *   - Criticism/scholarship community: exercise fair use to quote, excerpt, and analyze copyrighted works in published criticism
 *   - Copyright holders: bear the cost of foregone licensing fees and loss of control over derivative uses
 *   - Courts: interpret and apply the four-factor test, setting precedent for fair use scope under this reading
 *   - Commercial licensing ecosystem: excluded from transactions that fall under fair use; would expand revenue if doctrine narrowed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.35).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.42).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '131a2048-aef1-48f5-994c-277aa6681bde').
narrative_ontology:cs_kernel_codification('131a2048-aef1-48f5-994c-277aa6681bde', fixed_text).
narrative_ontology:cs_authority_grounding('131a2048-aef1-48f5-994c-277aa6681bde', lineage).
narrative_ontology:cs_interpretation_layer_present('131a2048-aef1-48f5-994c-277aa6681bde').
narrative_ontology:cs_reading_relation('131a2048-aef1-48f5-994c-277aa6681bde', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('131a2048-aef1-48f5-994c-277aa6681bde', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('131a2048-aef1-48f5-994c-277aa6681bde', foundational, fair_use_affirmative_right_users).
narrative_ontology:cs_axiom_status(fair_use_affirmative_right_users, holdable).
narrative_ontology:cs_axiom_grounding('131a2048-aef1-48f5-994c-277aa6681bde', fair_use_affirmative_right_users, deontological).
narrative_ontology:cs_axiom('131a2048-aef1-48f5-994c-277aa6681bde', foundational, public_benefit_primacy_over_creator_incentive).
narrative_ontology:cs_axiom_status(public_benefit_primacy_over_creator_incentive, holdable).
narrative_ontology:cs_axiom_grounding('131a2048-aef1-48f5-994c-277aa6681bde', public_benefit_primacy_over_creator_incentive, deontological).
narrative_ontology:cs_reference_frame('131a2048-aef1-48f5-994c-277aa6681bde', affirmative_user_right_framework).
narrative_ontology:cs_drift_state('131a2048-aef1-48f5-994c-277aa6681bde', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('131a2048-aef1-48f5-994c-277aa6681bde', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_libraries).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, noncommercial_cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, criticism_commentary_scholarship_community).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schools, universities, and educational nonprofits rely on fair use to incorporate copyrighted materials into curricula without licensing every excerpt. Under this reading, they are entitled to use copyrighted works for teaching, research, and classroom discussion. They benefit from the affirmative right to use without negotiating with every copyright holder.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    organized, generational, mobile, national).

% Operate under fair use doctrine to make copyrighted materials accessible to the public through lending, archiving, and preservation. Under this reading, they have an affirmative right to serve the public interest by distributing knowledge and culture, including through digital and interlibrary loan systems.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_libraries, beneficiary,
    moderate, generational, constrained, regional).

% Artists, musicians, writers, and creators who build on existing culture—fan fiction authors, remix artists, documentary filmmakers, sampling musicians—depend on fair use to create derivative works without licensing fees or permission. They operate outside commercial markets and would be unable to create at all if fair use collapsed.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, noncommercial_cultural_producers, beneficiary,
    powerless, biographical, mobile, global).

% Critics, academics, journalists, and bloggers quote, excerpt, and analyze copyrighted works to discuss, evaluate, and critique them. Under this reading, they have an affirmative right to quote copyrighted material in the course of critical commentary without negotiating licensing.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, criticism_commentary_scholarship_community, beneficiary,
    moderate, biographical, mobile, global).

% Publishers, studios, music labels, and individual creators hold exclusive rights to reproduction and distribution. Under this reading, fair use carves out zones where they cannot collect licensing fees or control use, even when the use is profitable or competes with potential licensing markets. They bear the cost of foregone licensing revenue.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% Rights-clearing houses, licensing agencies, and collective societies (ASCAP, PROs) are structurally excluded from transactions that fall under fair use. They would collect fees from educational use, library archiving, and quotation in criticism if fair use were narrowed; the doctrine prevents this revenue from materializing.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_licensing_ecosystem, excluded,
    powerful, biographical, trapped, global).

% Federal courts interpret and apply the four-factor test. Under this reading, they weigh factors in a manner that prioritizes the public benefit and transformative nature of use, subordinating market harm when the use serves educational, critical, or cultural-preservation functions. They set precedent that determines the doctrine's practical scope.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts_applying_four_factor_test, agenda_setter,
    institutional, generational, analytical, national).

% Could amend copyright statute to narrow or eliminate fair use; has repeatedly declined to do so and has extended copyright terms in ways that presume fair use will operate as a counterbalance. Observes the doctrine in operation and retains authority to alter it.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legislature, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action coordination problem: enables educational and cultural institutions to access and use copyrighted material for socially beneficial purposes without transaction costs of individual licensing negotiations with multiple rights holders. Creates a common pool of accessible culture and knowledge.
% TRANSFER_FUNCTION: Transfers foregone licensing revenue and control over derivative uses FROM copyright holders TO educational institutions, libraries, noncommercial producers, and the general public. The transfer is a redistribution of access rights and economic value from rights holders to public beneficiaries.
% ABSENT_VOICES: Commercial licensing intermediaries (rights-clearing houses, collective societies) are structurally excluded—they would argue for narrower fair use to expand licensing opportunities and fee collection. Creators whose primary revenue model depends on licensing (screenwriters, illustrators, music publishers licensing to educational markets) are not directly represented in court arguments and would argue for stricter market-harm analysis.
% DISAPPEARANCE_RATIONALE: If fair use disappeared overnight, educational institutions would need to license every excerpt used in teaching; libraries could not operate preservation programs or provide interlibrary loans; noncommercial creators would be unable to create derivative works; critics and scholars would require permission to quote. Digital culture, remix, and fan communities would collapse unless licensing became free and frictionless—which is structurally impossible at scale. The copyright licensing system would need to undergo radical redesign to replace fair use's gatekeeping function.
% FOUNDING_PROBLEM: Copyright law grants exclusive rights to reproduction and distribution. Without fair use, socially beneficial uses—teaching, criticism, preservation, commentary, parody—would require licensing negotiations with rights holders. Educational and nonprofit use would become prohibitively expensive or impossible. Public understanding and cultural conversation would be chilled by licensing requirements and the threat of infringement liability.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions, libraries, and scholars consistently testify that fair use is essential to their operations and that licensing every excerpt would be economically impossible and pedagogically harmful. Courts have repeatedly affirmed that fair use serves foundational constitutional interests in free speech and the development of new culture. Copyright scholars and First Amendment experts (outside the benefiting parties) attest that without fair use, speech and culture would be substantially constrained.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.35, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-low (0.35 at interval end) because, under this reading, fair use is not extraction at all—it is the assertion of an affirmative user right. From the copyright holder's perspective, the measured extractiveness reflects foregone licensing revenue and loss of control; from the user's perspective, extractiveness is zero (the right is granted, not constrained). The shared time grid shows extractiveness plateauing after year 20, reflecting relative stability of the doctrine (few major statutory changes; courts oscillate but do not systematically narrow or expand fair use over this interval). Suppression is moderate (0.42) because the doctrine's scope is contested: copyright holders continuously argue for narrower interpretation; courts resist but sometimes tighten market-harm analysis; the suppression required to hold fair use in place consists of litigation costs, appeals defending precedent, and legislative resistance to copyright-holder demands for fair-use narrowing. Theater ratio is low (0.28), indicating that fair use is primarily functional (genuinely permitting use) rather than performative; the modest theater component reflects periodic symbolic 'tough' court decisions that narrow particular applications while leaving the doctrine's overall scope intact.
 *
 * PERSPECTIVAL GAP:
 *   The copyright holder and the educational institution should compute to very different types. From the copyright holder's seat (powerful, generational, constrained exit, global scope), the constraint extracts: licensing revenue is foregone, derivative-market control is lost, and enforcement machinery is required to police the boundary. From the educational institution's seat (organized, generational, mobile exit, national scope), the constraint is coordinate: it solves the transaction-cost problem of licensing every excerpt and ensures access to culture for teaching. From the public library seat (moderate power, generational, constrained exit, regional scope), it is a beneficiary Rope—the right is affirmed and exercised without being extracted from elsewhere. The engine computes these divergences from the power and exit atoms. The user-centric reading authored these stakeholder positions to reflect the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are the directional target (high d, near 1.0): they bear the cost of foregone licensing revenue, have constrained exit (cannot opt out of copyright law), and are powerful enough to negotiate but not to override the doctrine. Educational institutions and public libraries are directional beneficiaries (low d, near 0.0): they collect the right to use without negotiating, bear no direct loss, and have mobile exit (they could license on a case-by-case basis, but fair use makes that unnecessary). Noncommercial cultural producers are also beneficiaries but with more constrained exit (they depend on fair use to operate at all—identity-locked, in a sense: their entire creative practice assumes fair use exists). Courts are analytical (d = 0.5, neither collecting nor paying, but interpreting the boundary). The commercial licensing ecosystem is excluded: they would participate if fair use narrowed, giving them higher d, but are currently locked out of these transactions entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling socially beneficial use despite exclusive copyright rights) remains live: courts regularly hear fair-use defenses; educational institutions continue to rely on fair use; new forms of derivative culture (remix, sampling, fan works) repeatedly raise the question of whether the doctrine accommodates new uses. The disappearance verdict (world_rearranges) is supported by empirical fact: each litigation cycle demonstrates that culture would reorganize around licensing if fair use collapsed. The doctrine is not performing a vestigial function; it is actively and continuously contested. This prevents piton classification: the constraint is held in place not by institutional inertia or theatrical maintenance, but by active litigation, legislative resistance to copyright-holder demands, and the genuine utility of the doctrine to multiple organized stakeholder groups. A piton would show rising theater_ratio and flat or declining resistance; this story shows stable theater_ratio and sustained resistance from copyright holders and licensing intermediaries. The classification is Rope (coordination with asymmetric impact) or Tangled Rope (coordination with extraction), depending on whether the market-harm factor is weighted heavily enough that fair use resembles an exception to property rights (creator-centric reading) or an affirmative right (this user-centric reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_weighting,
    'How should the ''effect on the market for the original'' factor be weighted relative to other factors? Should actual market harm be required, or is potential market harm sufficient? Should licensing revenue foregone by the copyright holder count as harm to the original market?',
    'Systematic analysis of court decisions over a decade, coding how much weight courts assign to market-harm analysis. Comparison of outcomes when market harm is potential vs. actual, direct vs. indirect. Empirical measurement of how often courts subordinate market harm to other factors.',
    'High weighting of market harm and potential harm favors the creator-centric reading and narrows fair use. Low weighting of licensing revenue and emphasis on actual market substitution favors the user-centric reading and expands fair use. This is the primary axis of contestation between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_weighting, empirical, 'The weight assigned to market-harm analysis in four-factor balancing.').

omega_variable(
    transformativeness_primacy,
    'Should the transformative character of a use (does it add new meaning, expression, or message?) be treated as a secondary consideration within the four-factor framework, or as a dominant factor that can outweigh market harm?',
    'Longitudinal analysis of case law: does transformative-use doctrine consistently override market-harm findings, or do courts balance transformation against other factors? Measurement of outcome rates when transformation is high but market harm is also substantial.',
    'If transformativeness is dominant, the transformative-use reading increasingly displaces both creator-centric and user-centric readings. If it is merely one factor, the user-centric reading remains coherent as a distinct frame that emphasizes public benefit and access alongside transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_primacy, conceptual, 'Whether transformative character should dominate fair-use determination or remain one of several factors.').

omega_variable(
    beneficiary_structure_under_readings,
    'Under the user-centric reading, is the primary beneficiary the using party (the educator, critic, library) or the general public? If the public, how is public benefit measured—is it cultural preservation, access to knowledge, support for creative freedom, or all three?',
    'Textual analysis of court opinions adopting user-centric language: which beneficiary is named and prioritized? Examination of how ''public benefit'' is operationalized in court decisions. Analysis of which stakeholders are treated as having standing in fair-use disputes.',
    'If the using party (educator, critic) is the primary beneficiary, fair use is a Rope coordinating their interests with the public. If the general public is the primary beneficiary, fair use is extraction from copyright holders for public gain, a Tangled Rope. The victimhood of copyright holders (all holders, or only commercial publishers?) determines whether suppression is required to enforce the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_under_readings, conceptual, 'Who is the structural beneficiary under the user-centric reading: the using party, the general public, or both.').

omega_variable(
    sibling_reading_coexistence,
    'Are the three readings—creator-centric, transformative-use, user-centric—genuinely coexistent as live positions, or does one reading logically foreclose the others within the framework of copyright law?',
    'Examination of whether courts have simultaneously applied elements of all three readings in a single decision, or whether different courts, jurisdictions, or eras adopt distinct readings. Analysis of whether a judge can hold creator-centric and user-centric premises without contradiction, or whether they are mutually exclusive within a single framework.',
    'If coexistent, the three readings are distinct constraint stories linked by network edges (affecting_constraints). If one forecloses others, the framework is binary or unary, and the stories reduce in number. Currently the evidence suggests coexistence (different circuits, different judges, different doctrinal areas), but systematic precedent analysis is needed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the three fair-use readings are genuinely coexistent or whether one forecloses the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fair_tr_t5, fair_use_four_factor_test__user_centric_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__user_centric_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__user_centric_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__user_centric_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__user_centric_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fair_be_t5, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 40, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fair_su_t5, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(fair_su_t30, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, information_standard).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% The fair_use_four_factor_test kernel has three structurally distinct readings, each instantiating a different constraint with different epsilon values, beneficiary structures, and classifications. The user-centric reading (this story) treats fair use as an affirmative right preserving public access, low epsilon on unauthorized use, public/educational beneficiaries. The creator-centric reading treats fair use as a narrow exception, high epsilon on uses that reduce creator incentives. The transformative-use reading emphasizes novelty of expression over market protection. Each reading is a complete constraint story with its own ε, stakeholder seats, and claim-to-metric divergence. They affect each other through the shared kernel (courts' interpretation of the four factors influences which reading dominates at any moment) but are not merged into a single multi-reading story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
