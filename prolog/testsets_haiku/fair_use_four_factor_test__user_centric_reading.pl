% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use as Affirmative User Right (Four-Factor Test)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   Fair use doctrine in US copyright law permits unauthorized copying and
 *   derivative use for specified purposes—criticism, commentary, teaching,
 *   research, and transformative creation—without infringing copyright. The
 *   user-centric reading of the four-factor test treats fair use as an
 *   affirmative user right that preserves public access and enables cultural
 *   production. Copyright holders bear the extraction cost: they lose
 *   licensing revenue and cannot fully control derivative-use markets. Courts
 *   applying this reading weight the four factors to prioritize public
 *   benefit and transformativeness over copyright holder compensation. The
 *   constraint is claimed as rope (genuine coordination that solves the
 *   permission-barrier problem) and the authored metrics describe a
 *   coordination mechanism with moderate extractiveness, moderate suppression
 *   (copyright holders must actively litigate to narrow fair use), and low
 *   theater (the doctrine's functional purposes are real, though political
 *   contestation around it is substantial). This story instantiates ONE
 *   reading of a contested kernel (fair_use_four_factor_test); sibling
 *   readings (creator-centric and transformative-use) generate separate
 *   constraints with different ε values and stakeholder structures.
 *
 * KEY AGENTS:
 *   - public_and_educational_users (beneficiary, powerless-to-organized, constrained exit) — access copyrighted materials for teaching and research
 *   - commercial_copyright_holders (payer, powerful, mobile) — lose licensing revenue and derivative-market control under user-centric reading
 *   - courts_interpreting_fair_use (agenda_setter, institutional, analytical) — apply four-factor test case-by-case; interpretation doctrines instantiate the reading
 *   - library_institutions (beneficiary + agenda_setter, institutional, mobile) — defend fair use through preservation, digitization, and litigation; institutional guardians of access rights
 *   - cultural_producers (beneficiary, moderate power, constrained exit) — create transformative works that depend on broad fair use protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.38).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.42).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (Four-Factor Test)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'c31bf4b8-7ce1-4307-be38-8a3fc402b370').
narrative_ontology:cs_kernel_codification('c31bf4b8-7ce1-4307-be38-8a3fc402b370', fixed_text).
narrative_ontology:cs_authority_grounding('c31bf4b8-7ce1-4307-be38-8a3fc402b370', lineage).
narrative_ontology:cs_interpretation_layer_present('c31bf4b8-7ce1-4307-be38-8a3fc402b370').
narrative_ontology:cs_reading_relation('c31bf4b8-7ce1-4307-be38-8a3fc402b370', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c31bf4b8-7ce1-4307-be38-8a3fc402b370', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('c31bf4b8-7ce1-4307-be38-8a3fc402b370', foundational, fair_use_as_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('c31bf4b8-7ce1-4307-be38-8a3fc402b370', fair_use_as_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('c31bf4b8-7ce1-4307-be38-8a3fc402b370', foundational, public_access_and_cultural_production_priority).
narrative_ontology:cs_axiom_status(public_access_and_cultural_production_priority, holdable).
narrative_ontology:cs_axiom_grounding('c31bf4b8-7ce1-4307-be38-8a3fc402b370', public_access_and_cultural_production_priority, deontological).
narrative_ontology:cs_reference_frame('c31bf4b8-7ce1-4307-be38-8a3fc402b370', public_benefit_default_fair_use).
narrative_ontology:cs_drift_state('c31bf4b8-7ce1-4307-be38-8a3fc402b370', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c31bf4b8-7ce1-4307-be38-8a3fc402b370', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_and_educational_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, student_researchers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, library_institutions).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, commercial_copyright_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, copyright_holders_via_licensing_industry).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders_via_licensing_industry).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, public_access_doctrine).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, educational_equity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access copyrighted materials for research, education, criticism, and commentary without license. Students quote from textbooks; scholars analyze literary works; documentary filmmakers incorporate archival footage. Fair use as an affirmative right means they can make these uses without asking permission or paying fees, provided the four-factor test weighs in their favor. Their ability to participate in cultural production and knowledge-building depends on this protection.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_and_educational_users, beneficiary,
    organized, generational, mobile, national).

% Cite, quote, and analyze copyrighted texts without licensing fees. A strict reading of copyright would require seeking permission and paying for every academic quotation; fair use as an affirmative right means educational research is presumptively protected when transformation and limited copying serve teaching purposes. Their constraint to tight budgets and institutional resources makes license fees prohibitive.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, student_researchers, beneficiary,
    powerless, biographical, constrained, national).

% Preserve, digitize, and provide access to cultural materials without seeking permission for every work. Under a user-centric reading, libraries can interpret fair use broadly to justify preservation activities, interlibrary loan, and controlled digital lending—extending public access to materials that would otherwise be locked behind licensing agreements. They also defend fair use doctrine through litigation and advocacy, positioning themselves as institutional guardians of user rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, library_institutions, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, library_institutions, agenda_setter).

% Create new works that build on, remix, or reference existing copyrighted material—meme creators, fan artists, remix musicians, documentary filmmakers. Under a user-centric reading, their transformative uses are presumptively fair, because the four-factor test prioritizes how the new work adds value over whether it harms the original creator's market. They depend on broad fair use protection to avoid cease-and-desist letters and litigation costs.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_producers, beneficiary,
    moderate, biographical, constrained, national).

% Bear reduced licensing revenue and derivative-work control under a user-centric fair use reading. A broad interpretation of fair use—prioritizing public access and transformative use—means they cannot charge licensing fees or control downstream uses as extensively. Publishers, film studios, and music labels argue that fair use in this reading erodes their incentive to create and their ability to monetize secondary markets.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_copyright_holders, payer,
    powerful, generational, mobile, global).

% Apply the four-factor test case-by-case to determine what uses qualify as fair. Under a user-centric reading, courts weight the four factors to prioritize public benefit and transformativeness; they construe the test as a gateway mechanism for protecting user rights, not as a narrow exception that defaults to copyright holder control. Their interpretation doctrines—how they frame the test, which factors they emphasize, how they weigh market harm—collectively instantiate the reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts_interpreting_fair_use, agenda_setter,
    institutional, generational, analytical, national).

% May license uses that would otherwise qualify as fair use, deriving licensing revenue from a narrower fair use doctrine (one that reserves more uses for licensing). Under a user-centric reading, licensing demand shrinks because more uses fall outside the licensing market. They also benefit from the fact that fair use creates a licensing ceiling: creators can still license works for uses that fair use doesn't cover.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders_via_licensing_industry, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, copyright_holders_via_licensing_industry, beneficiary).

% Enacts copyright law and could amend Section 107 (fair use statute) to narrow or broaden the doctrine. Currently, the statutory framework states that fair use is determined by four factors, leaving interpretation to courts. Legislative observation is passive; fair use doctrine is judge-made law within the statutory framework. Congress could act but has not fundamentally rewritten fair use in decades.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, congress_and_legislative_authority, observer,
    institutional, generational, analytical, national).

% International agreements like TRIPS and the WIPO Copyright Treaty impose constraints on exceptions to copyright (including fair use), but the US courts interpret fair use broadly under a user-centric reading, which may tension with international harmonization expectations. These treaties would prefer more uniform, restrictive fair use doctrines. They are excluded from US fair use doctrine deliberation, though they create structural pressure on how broad fair use can be.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, international_copyright_treaties, excluded,
    institutional, generational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, commercial_copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cultural production, knowledge-building, and public access by establishing that copyrighted materials can be used for specified purposes—teaching, criticism, commentary, research—without requiring licensing from every copyright holder. Solves the collective-action problem of permission-seeking: if every use required a license, education and scholarship would be severely constrained by transaction costs and refusals.
% TRANSFER_FUNCTION: Moves the ability to use copyrighted materials from copyright holders (who would otherwise control all derivative and transformative uses) to users (educators, scholars, cultural producers). The transfer is functional, not economic: no money changes hands, but the right to use is allocated to the user class. This reduces copyright holders' licensing revenue and their control over derivative markets.
% ABSENT_VOICES: International copyright regimes (TRIPS, WIPO treaties) that prefer narrower fair use exceptions; smaller independent creators who might benefit from stricter copyright enforcement; for-hire creators in publishing and music who depend on derivative-work licensing for income. These parties would argue for a creator-centric reading but are not substantially represented in US fair use doctrine development (which remains judge-centric and academic-community-influenced).
% DISAPPEARANCE_RATIONALE: If fair use as an affirmative user right disappeared—if the four-factor test became a narrow exception or were eliminated—education would reorganize: schools would license every textbook excerpt; academic publishing would contract around licensing costs; documentary filmmaking would require clearance for every archival clip; library preservation would become legally impossible without creator permission. The cultural production ecosystem would reallocate toward creators who could afford licensing and away from public/educational use.
% FOUNDING_PROBLEM: Copyright law's default-to-permission rule creates a permission barrier that blocks legitimate educational and transformative uses, especially by resource-constrained users (students, libraries, nonprofit organizations). Fair use was developed to carve out a presumptive user right for uses that serve public benefit, preserve access, and enable new creation without requiring the transaction cost of licensing.
% FOUNDING_PROBLEM_CORROBORATION: Library associations, educational institutions, and digital rights advocates (outside the copyright-holder industry) attest the founding problem remains live: licensing costs for educational materials are prohibitive; copyright holders do refuse educational licenses; fair use protections are necessary to enable scholarship and teaching. Courts have similarly affirmed this in fair use cases (Harper & Row, Campbell v. Acuff-Rose) that protect educational and transformative users. The copyright-holder industry contests this framing, arguing that licensing mechanisms are efficient and voluntary; their testimony comes from the beneficiary side of the dispute and should be weighted accordingly.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38) because the user-centric reading grants users an affirmative right, reducing copyright holders' extractive power, but copyright holders retain substantial market control outside fair use. Suppression is moderate-low (0.42) because the doctrine is embedded in statute (Section 107) and case law; copyright holders must actively litigate to narrow it, and courts have increasingly protected user rights under transformative-use doctrine. Theater is low (0.22) because fair use has genuine functional purposes (enabling education, criticism, adaptation) and courts are sincerely applying the four-factor test, not performing a ritualized permission-granting ceremony. The measurement series show slight extractiveness creep from 1976–2000 (as licensing industries developed and challenged fair use in Harper & Row and RIAA cases) and stabilization from 2000–2024 (as Campbell v. Acuff-Rose and its transformative-use doctrine entrenched). Suppression requirement is stable across the interval (courts continuously defend fair use against copyright-holder litigation). Theater ratio is stable (the four-factor test is genuinely applied, not theatrical; contestation is genuine, not performative).
 *
 * PERSPECTIVAL GAP:
 *   The user-beneficiary seats (students, educators, libraries, cultural producers) perceive the constraint as a floor of protection—a right they can rely on to access and build upon copyrighted materials. The copyright-holder seat perceives it as a ceiling—an unpredictable limitation on their exclusive rights that reduces licensing revenue and derivative-market control. Courts perceive it as a balancing doctrine that must be applied case-by-case, giving them institutional power to shape fair use's scope. The engine computes per-seat directionality from these structural positions: users sit at low d (fair use benefits them), copyright holders at high d (it extracts from them), courts at analytical d. The divergence is the structure: the same rule allocates opposite benefits/costs to different seats based on power and position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (public users, educators, libraries, cultural producers) are powerless-to-moderate, constrained by licensing costs and permission barriers—fair use reduces their exit cost. Victims (copyright holders) are powerful, with arbitrage and mobile exit (they can license or litigate); fair use reduces their monopoly control, but they retain substantial bargaining power. Courts are institutional agenda-setters with analytical exit—they shape the doctrine through interpretation. The directionality profile: users derive near-zero or negative d (fair use is a subsidy reducing their constraint cost); copyright holders derive high d (fair use extracts licensing revenue and monopoly control); courts derive near-0.5 d (they are the mechanism implementing the constraint, neither pure target nor pure beneficiary). The base_properties.beneficiaries and victims declarations track this structure directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permission barriers blocking education and cultural production) is LIVE: libraries still cannot freely digitize; students still cannot license every quotation; documentarians still cannot clear every archival clip without fair use protection. The disappearance verdict is WORLD_REARRANGES (education would reorganize if fair use vanished). The founding_problem_status is LIVE, not dead or obsolete—fair use doctrine continues to solve the core permission-barrier problem it was designed for. There is no mandatrophy signal (founding problem dead, constraint persisting). The constraint is classified as rope, not piton, because it carries a live coordination function and is actively enforced (courts continuously apply the four-factor test in new cases). The contestation is not over whether the founding problem exists (all parties acknowledge copyright's default-to-permission rule and its costs) but over how much user benefit should be protected (user-centric vs. creator-centric reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_weight_contestation,
    'Is transformativeness the primary weight in the four-factor test, or is it one factor among four with equal standing?',
    'Case-law trend analysis: if courts consistently weight transformativeness as dispositive (Campbell v. Acuff-Rose pattern), the user-centric reading holds; if courts give equal weight to all four factors with market harm weighing heavily (Harper & Row pattern), a creator-centric reading dominates.',
    'If transformativeness is primary, fair use protection expands for remix, adaptation, and critical uses (user-centric). If four factors are co-equal and market harm is controlling, fair use narrows (creator-centric). This shift determines how much cultural production and derivative use is presumptively protected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_weight_contestation, empirical, 'Which factor dominates in four-factor fair use balancing?').

omega_variable(
    kernel_contestation_user_vs_creator_framing,
    'Is fair use fundamentally an affirmative user right, or is it a narrow exception to the copyright holder''s exclusive rights?',
    'This is a kernel contest: the two readings (user-centric vs. creator-centric) assign opposite foundational roles to fair use within copyright law. The resolution is a commitment-system reading choice, not an empirical discovery. The user-centric reading frames fair use as the doctrine that preserves public-domain-like access; the creator-centric reading frames it as a carve-out from property rights that should be construed narrowly. No amount of case-law data resolves which framing is authoritative—each reading produces different conclusions from the same law.',
    'The user-centric reading derives lower extractiveness (0.38) by treating unauthorized uses as presumptively fair when they serve public benefit. A creator-centric reading would derive higher extractiveness (0.65+) by treating unauthorized uses as presumptively infringing unless the four-factor test clearly permits them. The choice of reading determines whether the constraint is rope (coordination with public benefit) or snare (extraction behind a judicial carve-out).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contestation_user_vs_creator_framing, conceptual, 'Kernel contest: is fair use an affirmative user right or a narrow exception to copyright?').

omega_variable(
    market_harm_assessment_ambiguity,
    'How should courts assess ''market harm'' in the fourth factor when a use does not compete with the original work''s market but does harm licensing revenue?',
    'Case law distinguishes between harm to the original work''s market (not fair use) and harm to licensing markets for derivative uses (fair use is not automatic, but users have strong interests). The question is whether courts weight licensing-revenue loss as a controlling harm. Empirical indicator: if Campbell v. Acuff-Rose''s distinction between market substitution and derivative-market harm is consistently applied, licensing-revenue loss is secondary; if courts treat any licensing-revenue loss as market harm, creator interests dominate.',
    'If licensing-revenue loss is treated as market harm, fair use protection shrinks and copyright holders can prevent transformative uses by offering licenses. If licensing-revenue loss is distinguished from market substitution, fair use protects transformative uses even when they foreclose licensing opportunities. This directly affects cultural producers (remixers, documentarians) and libraries (preservation digitization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_assessment_ambiguity, empirical, 'Whether licensing-revenue harm counts as copyright-relevant market harm in fair use analysis.').

omega_variable(
    reading_sibling_coexistence_stability,
    'Can the user-centric, transformative-use, and creator-centric readings coexist as live doctrinal positions within a single legal system, or does one reading''s dominance foreclose the others?',
    'Observational: if courts cite all three readings across different fact patterns without explicitly choosing one, coexistence is holding. If one reading becomes dominant and cited readings are confined to narrow holdings, foreclosure is occurring. Alternatively, a legislative amendment to Section 107 would foreclose readings incompatible with the new statutory language.',
    'Coexistence means fair use doctrine is genuinely contested and outcomes are doctrine-dependent; a user-centric court and a creator-centric court could reach opposite conclusions on identical facts. Foreclosure of user-centric or transformative-use readings would shift the constraint toward snare classification (copyright holder protection, user limitation). Foreclosure of the creator-centric reading would expand the rope classification toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_coexistence_stability, conceptual, 'Stability of multiple competing fair use readings within US copyright doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement_basis(fair_tr_t1976, projected).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement_basis(fair_tr_t1990, observed).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(fair_tr_t2000, observed).
narrative_ontology:measurement(fair_tr_t2012, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2012, 0.21).
narrative_ontology:measurement_basis(fair_tr_t2012, observed).
narrative_ontology:measurement(fair_tr_t2018, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement_basis(fair_tr_t2018, observed).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(fair_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.28).
narrative_ontology:measurement_basis(fair_be_t1976, projected).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement_basis(fair_be_t1990, observed).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement_basis(fair_be_t2000, observed).
narrative_ontology:measurement(fair_be_t2012, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2012, 0.37).
narrative_ontology:measurement_basis(fair_be_t2012, observed).
narrative_ontology:measurement(fair_be_t2018, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement_basis(fair_be_t2018, observed).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(fair_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.35).
narrative_ontology:measurement_basis(fair_su_t1976, projected).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(fair_su_t1990, observed).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement_basis(fair_su_t2000, observed).
narrative_ontology:measurement(fair_su_t2012, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2012, 0.43).
narrative_ontology:measurement_basis(fair_su_t2012, observed).
narrative_ontology:measurement(fair_su_t2018, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(fair_su_t2018, observed).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(fair_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__user_centric_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_licensing_markets).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, library_preservation_digitization).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, educational_access_to_copyrighted_material).

% DUAL FORMULATION NOTE:
% The fair_use_four_factor_test kernel has three structurally distinct readings, each instantiating a different constraint with different ε values and beneficiary/victim structures. The user-centric reading (this story) treats fair use as an affirmative right protecting public access and cultural production; epsilon is moderate (~0.38). The creator-centric reading treats fair use as a narrow exception to copyright; epsilon is high (~0.70). The transformative-use reading privileges transformativeness in the balancing; epsilon is low (~0.25). All three readings operate simultaneously in US copyright jurisprudence—courts cite all three in different fact patterns. The readings influence each other: the transformative-use reading builds on the user-centric reading's public-benefit framing; both are opposed by the creator-centric reading. The three constraints are linked via network.affects_constraints in all three files to preserve the family relationship and enable cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
