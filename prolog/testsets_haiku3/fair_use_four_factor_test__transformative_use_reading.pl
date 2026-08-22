% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative Use Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The fair use doctrine in U.S. copyright law (Section 107, codified 1976)
 *   permits limited copying of copyrighted material without permission for
 *   purposes like criticism, commentary, news reporting, teaching,
 *   scholarship, or parody. Courts must weigh four statutory factors: (1)
 *   purpose and character of use, (2) nature of the copyrighted work, (3)
 *   amount/substantiality of portion taken, (4) effect on the market for the
 *   original. The transformative-use reading interprets this doctrine such
 *   that when a secondary use adds new meaning, new aesthetic dimension, new
 *   critique, or new expression, it should dominate the four-factor balancing
 *   and shield the use from infringement liability even if the market harm to
 *   the original is substantial. This reading became judicial consensus from
 *   Campbell v. Acuff-Rose (1994) through contemporary cases. Remix culture,
 *   sampling-based music production, fan fiction communities, parody
 *   industries, and platforms like YouTube depend on this reading's
 *   protection. Original copyright holders and defenders of narrower fair use
 *   argue transformation has been weaponized to subordinate creator
 *   incentives and licensing revenue. The constraint describes the doctrine
 *   itself — the standing arrangement under contest. The transformative-use
 *   reading is one canonical interpretation of how that doctrine should
 *   operate. Alternative readings (creator-centric: narrow fair use to
 *   protect incentives; user-centric: treat fair use as an affirmative right,
 *   not exception) are separate constraints operating on the same kernel.
 *
 * KEY AGENTS:
 *   - remix_culture_practitioners: Derivative creators (samplers, remixers, parody makers, fan fiction writers) — moderately powered but widely distributed; beneficiaries under transformation doctrine
 *   - ugc_platforms: YouTube, TikTok, remix platforms — institutionally powerful agenda-setters; set content policy and determine transformation; beneficiaries and enforcers
 *   - original_copyright_holders: Film studios, music labels, publishers — institutionally powerful, constrained exit (doctrine binds them); victims whose licensing revenue and veto rights erode
 *   - courts: Federal judiciary interpreting Section 107; institutional power, generational time horizon; set the binding rule for what counts as transformation
 *   - original_creators_individual_level: Occupy dual role — benefit when their own work gets fair-use protection, pay when others remix theirs uncompensated
 *   - commercial_licensing_operators: Music sync, film rights clearance — powerful but constrained by doctrine shrinking licensing demand; victims
 *   - cultural_commons_advocates: EFF, Creative Commons, scholars — organized but excluded from rule-making; shape discourse
 *   - copyright_fundamentalists: Scholarly opponents of broad transformation — excluded from doctrine; prefer narrower reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.48).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.35).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '7613d6a3-a0bb-46e6-b4bb-67555c19f744').
narrative_ontology:cs_kernel_codification('7613d6a3-a0bb-46e6-b4bb-67555c19f744', fixed_text).
narrative_ontology:cs_authority_grounding('7613d6a3-a0bb-46e6-b4bb-67555c19f744', lineage).
narrative_ontology:cs_interpretation_layer_present('7613d6a3-a0bb-46e6-b4bb-67555c19f744').
narrative_ontology:cs_reading_relation('7613d6a3-a0bb-46e6-b4bb-67555c19f744', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('7613d6a3-a0bb-46e6-b4bb-67555c19f744', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('7613d6a3-a0bb-46e6-b4bb-67555c19f744', foundational, transformation_dominates_four_factor_balancing).
narrative_ontology:cs_axiom_status(transformation_dominates_four_factor_balancing, holdable).
narrative_ontology:cs_axiom_grounding('7613d6a3-a0bb-46e6-b4bb-67555c19f744', transformation_dominates_four_factor_balancing, empirically_contingent).
narrative_ontology:cs_axiom('7613d6a3-a0bb-46e6-b4bb-67555c19f744', secondary, market_harm_subordinated_under_transformation).
narrative_ontology:cs_axiom_status(market_harm_subordinated_under_transformation, holdable).
narrative_ontology:cs_axiom_grounding('7613d6a3-a0bb-46e6-b4bb-67555c19f744', market_harm_subordinated_under_transformation, deontological).
narrative_ontology:cs_reference_frame('7613d6a3-a0bb-46e6-b4bb-67555c19f744', statutory_fair_use_four_factors_1976).
narrative_ontology:cs_drift_state('7613d6a3-a0bb-46e6-b4bb-67555c19f744', contemporary_platform_remix_era_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7613d6a3-a0bb-46e6-b4bb-67555c19f744', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, downstream_creative_industries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders_derivative_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, original_creators_individual_level).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_creators_individual_level).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, commercial_derivative_market_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, musicians, filmmakers, and creators who build derivative works from existing copyrighted material. Transformative-use framing permits their work to be legally defensible when it adds new meaning, critique, or aesthetic dimension. They benefit from the ability to reference, sample, remix, and recontextualize without seeking permission or paying licensing fees.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Technology platforms hosting user-generated content (YouTube, TikTok, Twitch, platforms for mashups and remixes). Under transformative-use doctrine, they can permit user uploads of derivative works and defend against takedown notices by arguing transformation. They benefit from network effects and engagement driven by remix culture, and from reduced liability under the doctrine. They set content policy frames and determine what counts as transformation in practice.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, beneficiary).

% Film studios, music labels, publishers, and other copyright holders whose work is remixed, sampled, or used as the basis for derivative works. When transformation doctrine shields derivative works from infringement liability, they lose licensing revenue, control over derivative uses, and the ability to prevent uses they deem damaging to their brand or creative intent. Their exit options are constrained by the judicial doctrine itself: they cannot change the rule unilaterally, and statutory licensing is limited.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders_derivative_markets, payer,
    powerful, generational, constrained, global).

% Federal courts interpreting Section 107 of the Copyright Act in the United States. The transformative-use reading has become dominant in case law since Harper & Row v. Nation Enterprises and Sony v. Universal, with decisive turns in Campbell v. Acuff-Rose (2 Live Crew parody) and subsequent cases. They interpret what qualifies as transformation, adjudicate fair-use claims, and in doing so, set the binding rule for the constraint's operation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_applying_transformation_doctrine, agenda_setter,
    institutional, generational, analytical, national).

% Individual original creators (musicians, visual artists, filmmakers) who both produce original work they want protected AND consume and remix existing cultural material. They benefit when their own work is shielded by fair use as it transforms others' work, but pay when others transform theirs without licensing. They occupy a dual position: beneficiary and victim depending on the direction of the remix.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_creators_individual_level, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, original_creators_individual_level, payer).

% Organizations and theorists (EFF, Creative Commons, academic scholars) who argue for expansive fair use doctrine as necessary to preserve cultural freedom and remix capacity. They analyze court decisions, file amicus briefs, and advocate for transformation as the dominant interpretive lens. They have no direct economic stake but shape the discourse around what counts as legitimate transformation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, cultural_commons_advocates, observer,
    organized, generational, analytical, global).

% Companies that license derivative rights (music sync licensing, film rights clearance houses, stock music operators). When transformative-use doctrine shields derivative works from licensing requirements, their market shrinks. Original creators channel licensing revenue through these operators; if fair use expands, licensing volume contracts. They lobby for narrower transformation thresholds.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, commercial_derivative_market_operators, payer,
    powerful, biographical, constrained, global).

% Legal scholars and policy advocates who argue fair use is being misapplied to subordinate original creator rights and that transformation should be narrowly construed. They contest the transformation doctrine in scholarship and policy debates but are structurally excluded from the constraint's operation: courts do not consult them, and their preferred reading (creator-centric) is minority doctrine. Their exclusion means the contest plays out in appellate litigation and legislation, not in the doctrine's daily application.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_fundamentalist_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the tension between copyright's incentive function (protecting original creators' economic interests) and the public's cultural production capacity (enabling downstream creators to build on existing works). Transformation doctrine coordinates by permitting uses that add new meaning without requiring permission or payment, contingent on the use being substantially transformative.
% TRANSFER_FUNCTION: Moves the right to control derivative works from original copyright holders to downstream creators and platforms, contingent on judicial finding of transformation. Original holders lose licensing revenue and veto rights when a derivative work qualifies as fair use. The transfer is conditional and non-monetized: downstream creators gain freedom to use but do not pay the original holders.
% ABSENT_VOICES: Copyright fundamentalists and creator-centric legal scholars are structurally excluded from doctrine-setting: they do not participate in appellate decisions and their preferred reading is minority doctrine. Individual original creators who are harmed by uncompensated derivatives have limited standing to contest transformation findings (they can litigate but lose more often). Commercial licensing intermediaries are excluded from the doctrine-development process itself, though they have lobbying access.
% DISAPPEARANCE_RATIONALE: If transformation doctrine vanished and courts returned to literal literalist fair-use analysis (weighing all four factors equally, prioritizing original creator consent and market harm), remix culture would face massive liability exposure. Sampling-based music production, fan fiction, video remix platforms, and parody industries would restructure: either licensing would expand dramatically (raising transaction costs), or derivative production would migrate to jurisdictions with broader fair use, or it would contract sharply. Original copyright holders would regain licensing control and revenue.
% FOUNDING_PROBLEM: Before transformation became dominant, fair use was unpredictable. Courts weighted the four statutory factors (purpose, nature, amount/substantiality, market effect) with no clear hierarchy, sometimes finding literal copying to be fair use (satire, news), sometimes finding it to be infringement. Downstream creators faced chilling effects: they could not reliably know if a derivative work would survive suit. The doctrine was unclear; the constraint aims to clarify what downstream uses are permitted.
% FOUNDING_PROBLEM_CORROBORATION: The transformation doctrine's proponents (courts, scholars like Wendy Gordon, EFF) attest the founding problem remains live — without clarity on transformation, creators still face unpredictability. Copyright holders and fundamentalist scholars attest the problem has been over-solved — transformation doctrine now shields uses the original Copyright Act did not contemplate as fair. Legislative testimony and economic analysis from outside the benefiting parties (original holder licensing associations) support the claim that transformation doctrine has shifted from clarifying to expanding fair use beyond historical intent.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at endpoint) because the constraint extracts from original copyright holders (they lose licensing control and revenue) but does so contingent on judicial finding of genuine transformation, which requires case-by-case analysis. It is not pure extraction — the transfer is conditional on added meaning, and some uses do not qualify. Suppression is relatively low (0.35) because the constraint does not depend on coercive enforcement — it works through courts weighing factors. Resistance is high (0.71) because copyright holders actively litigate against fair-use defenses and lobby for narrower transformation thresholds; they have resources and institutional access. Theater ratio is modest (0.22) because the doctrine's function (clarifying what derivative uses are permitted) is real, though some share of court and platform activity defends the transformation umbrella itself rather than performing the coordination function. The measurement series shows extractiveness rising from 1976 (low clarity, narrow fair use) through the mid-2010s (transformation doctrine established, platform-enabled remix flourishing) and plateauing around 2019 as the doctrine stabilized and began facing legislative/institutional pushback (leading to slight compression by 2024). Theater tracks the same arc, reflecting institutional theater around transformation claims rising as the doctrine becomes more contested.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (remix practitioners, platforms) compute the constraint as genuine coordination — it resolves unpredictability and enables cultural production. From their perspective, the constraint is a rope: it coordinates by establishing clear rules that both protect derivative creators and permit platforms to operate at scale. The payer seats (original holders, licensing operators) compute it as extraction subordinating legitimate creator incentives to downstream profit. They perceive suppression of their market power and contestation of their licensing model as coercive, even though the legal mechanism is judicial interpretation, not administrative force. The court seat computes it as neutral interpretation of statutory language, though courts have shifted doctrine over time. The individual original-creator seat experiences both benefit and harm depending on which direction the remix flows, creating internal conflict within that stakeholder role.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix practitioners are low-d (beneficiaries: constraint removes barriers to their work). UGC platforms are near-zero-d (they set the agenda and collects value from remix culture enablement). Original copyright holders are high-d (constraint extracts licensing revenue and control from them). Individual original creators are near-symmetric (d ≈ 0.5) because they both benefit and pay. Courts are at d ≈ 0.4 (they interpret and maintain the doctrine; they benefit from the legitimacy of predictable fair-use doctrine, but they also bear reputational cost when accused of subordinating creator rights). Licensing operators are high-d (targets of the extraction via licensing-demand compression). The engine derives d from beneficiary/victim declarations and exit options: beneficiary + mobile or arbitrage exit = low d; victim + constrained exit = high d; dual role with contingency = moderate d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unpredictable fair use before transformation doctrine) is contested: does it persist, or has transformation over-solved it? Transformation proponents say prediction improved and cultural production flourished. Copyright holders say transformation over-interpreted fair use and dismantled legitimate licensing markets. The constraint does not exhibit mandatrophy in the simple sense — the founding problem has not wholly disappeared and the arrangement persists for reasons that still map to the original problem (coordination). However, the doctrine is increasingly under strain from legislative challenges (CASE Act, Copyright Office positions questioning transformation's breadth) and platform liability questions (whether platforms can claim transformation for user uploads). This is not mandatrophy (function-death with inertial persistence) so much as doctrine-contestation: the function is partially disputed, and the constraint may be moving toward reclassification as future legislation or case law shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_ambiguity,
    'What degree and kind of modification constitutes sufficient transformation to shield a use from liability?',
    'Examination of circuit splits in case law (Second Circuit, Ninth Circuit, others have diverged on what counts as transformation); legislative or judicial codification of a threshold (e.g., ''substantial new expression'' vs. minimal alteration). Natural experiments from other jurisdictions with different transformation standards.',
    'A high, narrow threshold would push ε closer to 0.35 (licenses required for most derivatives); a low, broad threshold would push ε toward 0.55+ (most derivatives shielded). The current ambiguity means transformation findings are contested and appeal rates are high. Courts'' case-by-case application creates uncertainty for both beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_threshold_ambiguity, empirical, 'The boundary of what counts as transformation is not fixed; courts apply it contextually, creating ex-post unpredictability.').

omega_variable(
    platform_liability_deflection,
    'When a platform hosts user-uploaded derivatives, does the user''s transformation claim shield the platform from contributory infringement, or is platform liability separate?',
    'Appellate clarification of platform liability under Sony safe harbor and DMCA § 512; legislative amendment clarifying when platforms can claim user transformation as a defense to their own liability.',
    'If platform liability remains robust (platforms liable even for transformative user uploads), platform gatekeeping increases (higher legal risk); if transformation shields platforms, platform-enabled remix culture expands. Current doctrine is mixed and contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_liability_deflection, conceptual, 'Whether transformation doctrine applies to platforms'' facilitation or only to end-user creation.').

omega_variable(
    commerciality_and_transformation_interaction,
    'When a derivative work adds transformation but is commercially exploited, does the commercial dimension override the transformation finding?',
    'Case-law evolution and legislative guidance. Current doctrine suggests commercial reuse can coexist with transformation (Campbell v. Acuff-Rose); some critics argue commerciality should reverse transformation findings.',
    'If commerciality voids transformation, licensing markets expand and ε drops toward 0.35; if transformation persists despite commerciality, platform-enabled UGC monetization remains protected and ε stays near 0.48. This is the site of maximum current contestation (cases like Oracle v. Google turn on this axis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerciality_and_transformation_interaction, empirical, 'Whether commercial derivative works can be shielded by transformation doctrine.').

omega_variable(
    kernel_reading_coexistence_stability,
    'Will the three fair-use readings (creator-centric, transformative-use, user-centric) continue to coexist as live positions in doctrine, or will one foreclose the others as legislative and case law evolve?',
    'Observation of statutory amendments, shifts in appellate consensus, and international legal harmonization (EU Copyright Directive, UK law). If a single reading gains sufficient institutional consensus to dominate, the kernel resolves toward that reading and others become overridden rather than coexisting.',
    'If transformation remains ascendant, this constraint''s classification is stable (rope-ish). If creator-centric reading gains legislative backing, ε could shift toward snare (transformation reinterpreted narrowly). If user-centric reading becomes dominant, ε might shift toward rope (expansion of fair use from exception to right).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_stability, conceptual, 'The long-term fate of the three competing fair-use doctrinal readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1976, 0.05).
narrative_ontology:measurement_basis(fair_tr_t1976, observed).
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.08).
narrative_ontology:measurement_basis(fair_tr_t1994, observed).
narrative_ontology:measurement(fair_tr_t2005, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement_basis(fair_tr_t2005, observed).
narrative_ontology:measurement(fair_tr_t2013, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2013, 0.19).
narrative_ontology:measurement_basis(fair_tr_t2013, observed).
narrative_ontology:measurement(fair_tr_t2019, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2019, 0.21).
narrative_ontology:measurement_basis(fair_tr_t2019, observed).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(fair_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1976, 0.22).
narrative_ontology:measurement_basis(fair_be_t1976, observed).
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.31).
narrative_ontology:measurement_basis(fair_be_t1994, observed).
narrative_ontology:measurement(fair_be_t2005, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement_basis(fair_be_t2005, observed).
narrative_ontology:measurement(fair_be_t2013, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2013, 0.47).
narrative_ontology:measurement_basis(fair_be_t2013, observed).
narrative_ontology:measurement(fair_be_t2019, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2019, 0.49).
narrative_ontology:measurement_basis(fair_be_t2019, observed).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.48).
narrative_ontology:measurement_basis(fair_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement_basis(fair_su_t1976, observed).
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.32).
narrative_ontology:measurement_basis(fair_su_t1994, observed).
narrative_ontology:measurement(fair_su_t2005, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement_basis(fair_su_t2005, observed).
narrative_ontology:measurement(fair_su_t2013, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement_basis(fair_su_t2013, observed).
narrative_ontology:measurement(fair_su_t2019, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2019, 0.35).
narrative_ontology:measurement_basis(fair_su_t2019, observed).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.35).
narrative_ontology:measurement_basis(fair_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_incentive_structure).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, platform_content_moderation_liability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair_use_four_factor_test kernel. The transformative-use reading (this file) interprets transformation as dominant in four-factor balancing; it enables remix culture and platform-hosted UGC. Sibling readings include creator_centric_reading (narrow transformation, prioritize creator incentives) and user_centric_reading (transformation as one factor, fair use as affirmative right). Each reading is a distinct constraint with different ε values, beneficiary/victim sets, and classification implications. They coexist as live positions in contemporary copyright doctrine, held by different coalitions (courts, scholars, platforms, creators). The three form a constraint family linked by network.affects_constraints; each describes the same statutory text (Section 107) under different interpretive frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
