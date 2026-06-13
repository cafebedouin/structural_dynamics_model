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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative-Use Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The transformative-use reading of the fair use doctrine holds that when a
 *   use of copyrighted material adds new meaning, expression, or
 *   critique—making a fundamentally new work—it presumptively qualifies as
 *   fair use even if market harm to the original work occurs. This reading
 *   dominates current U.S. fair use jurisprudence and enables a vast
 *   ecosystem of remix culture, sampling, user-generated content, and
 *   platform-mediated cultural production. The reading is one interpretation
 *   of a contested kernel: the fair use statute's four-factor balancing test.
 *   Other readings weight creator rights and market harm more heavily
 *   (creator-centric reading) or emphasize user access and public benefit
 *   (user-centric reading). This constraint story instantiates only the
 *   transformative-use reading: what makes it structurally distinct, who
 *   benefits, who bears costs, and why the claim/metric relationship reveals
 *   underlying asymmetry.
 *
 * KEY AGENTS:
 *   - Remix culture practitioners: benefit from expanded fair use protection; depend on transformativeness doctrine; constrained exit (cannot stop creating).
 *   - User-generated content platforms: benefit structurally and financially from expanded fair use; set the operational framework; institutional power; mobile exit (could operate under licensing but prefer expansion).
 *   - Original copyright holders (studios, labels, publishers): pay the cost of narrowed copyright; constrained exit (cannot stop copyright creation); powerful but litigation-bound.
 *   - Courts (applying fair use): agenda-setter role; interpret transformativeness through case law; filter expectations downstream.
 *   - Licensing intermediaries: pay indirect cost; trapped exit (shrinking market for permissions).
 *   - Small independent creators: excluded from case-law-setting; lack resources for litigation; trapped exit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.48).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.42).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative-Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'a3e9307c-39dc-49b7-a2c2-0d3d0599a12a').
narrative_ontology:cs_kernel_codification('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', fixed_text).
narrative_ontology:cs_authority_grounding('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', lineage).
narrative_ontology:cs_interpretation_layer_present('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a').
narrative_ontology:cs_reading_relation('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', foundational, transformation_dominates_four_factors).
narrative_ontology:cs_axiom_status(transformation_dominates_four_factors, holdable).
narrative_ontology:cs_axiom_grounding('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', transformation_dominates_four_factors, instrumental).
narrative_ontology:cs_axiom('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', foundational, market_harm_subordinate_to_transformation).
narrative_ontology:cs_axiom_status(market_harm_subordinate_to_transformation, holdable).
narrative_ontology:cs_axiom_grounding('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', market_harm_subordinate_to_transformation, empirically_contingent).
narrative_ontology:cs_reference_frame('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', fair_use_preserves_copyright_incentives_through_transformativeness).
narrative_ontology:cs_drift_state('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', contemporary_platform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a3e9307c-39dc-49b7-a2c2-0d3d0599a12a', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, musicians, and creators who build derivative works that add new meaning, expression, or critique to existing copyrighted materials. They benefit from the transformative-use doctrine's expansion of fair use protection, which reduces legal risk for their work. They depend on the doctrine's existence; without it, their practices face cease-and-desist letters and litigation. Exit would mean abandoning creative practices built on remixing and recontextualization.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Tech platforms (YouTube, TikTok, social media services) that host and monetize user-created derivative content. They benefit structurally from the transformative-use doctrine because it widens the fair use shelter for the content their users upload, reducing takedown liability and legal costs. They set the operational framework within which transformativeness claims are made and enforced (content moderation, fair use policy, takedown response). They have arbitrage: could operate under pure licensing but prefer the expansive fair use umbrella.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, agenda_setter).

% Music labels, film studios, publishers, and individual creators whose works are remixed, sampled, or recontextualized without license or compensation. They bear the cost of the transformative-use doctrine: their copyright is narrowed by fair use claims they cannot fully control. They argue that transformation claims allow commercial exploitation of their work and that market harm is subordinated even when remix creators profit. Exit is constrained: they cannot stop copyright creation itself, only litigate fair use claims one by one.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders, payer,
    powerful, generational, constrained, global).

% Federal judges who interpret and apply the fair use doctrine, including the four-factor test. Under the transformative-use reading, they emphasize factor one (purpose and character of use) and its transformativeness inquiry, treating market harm (factor four) as less dispositive when transformation is substantial. They set the operative definition of transformativeness through case law, which filters down to platform policy and creator expectations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_applying_fair_use, agenda_setter,
    institutional, generational, analytical, national).

% Rights clearance agencies, music rights organizations, and licensing platforms that exist to facilitate paid-for permissions. They face reduced demand as the transformative-use doctrine expands fair use, making permission unnecessary for works the doctrine shelters. They bear indirect cost: shrinking revenue from licensing fees and reduced leverage to broker permissions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_intermediaries, payer,
    moderate, biographical, trapped, global).

% Individual creators without institutional backing who own copyrights but do not have resources to monitor infringement or litigate fair use disputes. They would argue for stronger copyright protection and narrower fair use, but their voice is rarely represented in court cases or policy discussions dominated by large studios and platform economics. Their work is often the source material for remixes that benefit under this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, small_independent_creators, excluded,
    powerless, biographical, trapped, global).

% Legal academics and IP economists who analyze fair use doctrine and produce empirical and theoretical work on its effects. They serve as analytical seat: some defend transformative-use reading as enabling cultural innovation, others argue it subordinates creator incentives and market harm.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fair use coordinates legitimate use of copyrighted material without license, settling the boundary between copyright protection and cultural freedom by applying a four-factor balancing test. The transformative-use reading emphasizes transformativeness as the primary coordination signal: when a use adds new meaning or expression, it is presumed legitimate even if market harm occurs.
% TRANSFER_FUNCTION: The doctrine transfers latitude from copyright holders to remix creators and platforms: it redistributes the right to use existing works for new expressive purposes, reducing copyright holders' ability to control derivative uses and extract licensing fees. The transfer is conditional on the court finding sufficient transformation.
% ABSENT_VOICES: Small independent creators and unrepresented copyright owners lack standing in fair use litigation and are excluded from the case-law-setting process. They would argue for narrower fair use and stronger copyright protection, but their voices are drowned by institutional players (studios, platforms). Rights clearance agencies and licensing professionals are also effectively excluded from the policy-making process that shapes fair use doctrine.
% DISAPPEARANCE_RATIONALE: If the transformative-use reading vanished overnight and courts reverted to a narrower fair use doctrine subordinating transformation in favor of creator rights and market harm, remix culture would face immediate legal pressure. Platforms would need to implement more aggressive takedown and licensing, creators would face cease-and-desist letters, and licensing intermediaries would regain bargaining power. The economics of user-generated content platforms would shift toward licensing or more restrictive copyright enforcement.
% FOUNDING_PROBLEM: Copyright law in the digital age was written for static publication and distribution; fair use had to accommodate dynamic, real-time cultural production where sampling, remixing, and recontextualization are central creative practices. The transformative-use reading emerged to recognize that adding new meaning or expression to existing works serves the copyright system's ultimate goal: promoting cultural innovation.
% FOUNDING_PROBLEM_CORROBORATION: The doctrine's proponents (courts in landmark cases like Campbell v. Acuff-Rose, scholars like Professor Lawrence Lessig, and platform advocates) attest the founding problem is ongoing: remix and UGC are now central to cultural production and the doctrine must accommodate them. Opposing voices (copyright holders, small creators, music licensing organizations) attest the founding problem has been over-solved: fair use has expanded beyond its historical bounds and now subordinates creator rights and licensing revenue. The dispute is documented in case law divergence (Sony v. Universal vs. Harper & Row vs. Google Books showing shifting treatment of market harm) and in empirical studies on licensing uptake post-transformative-use jurisprudence.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.48) because the doctrine produces genuine coordination benefits (clarifying fair use boundaries, reducing frivolous takedowns, enabling cultural innovation) alongside asymmetric cost distribution (copyright holders lose licensing revenue, platforms gain unchecked user-content value capture). Suppression is lower than extractiveness (0.42) because the doctrine does not rely on legal coercion to persist—courts enforce it through interpretation, not penalty. Resistance is substantial (0.58) because copyright holders and licensing organizations actively litigate against fair use claims, producing a continuous stream of contested cases. Theater is moderate-low (0.28): the transformativeness inquiry is a genuine legal test, but increasing framing of every derivative work as 'transformative' to shelter commercial UGC platforms suggests rising performativity. The measurement series show extractiveness rising and suppression declining from t0 to t15, then stabilizing: the doctrine's scope has expanded but enforcement has become normalized (suppression requirement decreases as the doctrine becomes institutional common sense). The temporal pattern reflects the constraint's lifecycle: initial contestation → consolidation → institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (remix practitioners, platforms) perceive the constraint as enabling legitimate cultural freedom and innovation with minimal coercive overhead. The payer seats (copyright holders, licensing intermediaries) perceive the constraint as actively extractive: it subordinates their market rights and licensing bargaining power in favor of platforms' uncompensated content capture. The courts (agenda-setter) perceive it as a neutral balancing test, but their case-law choices have consistently tilted transformativeness upward and market harm downward. This divergence is structural and derives from the asymmetry in the constraint's benefits and costs: remix practitioners gain legal protection; platforms gain user-generated content value; copyright holders lose licensing revenue and derivative-use control. The engine computes each seat's type from this asymmetry: platform as low-d beneficiary, copyright holder as high-d payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix practitioners (organized power) have low directionality: the constraint subsidizes their practice by expanding fair use shelter. User-generated content platforms (institutional power) have low-to-moderate directionality: they benefit from expanded fair use and have alternative paths (licensing) available, giving them arbitrage. Original copyright holders (powerful power) have high directionality: the constraint extracts their licensing revenue and derivative-use control, and their exit is constrained (they cannot stop copyright creation or litigation, only defend incrementally). Licensing intermediaries (moderate power) have moderate-to-high directionality: the constraint shrinks their market and they have trapped exit. Small independent creators (powerless power) have high directionality when their works are remixed without compensation; they are identity-locked (cannot stop their work from existing in the cultural commons) and bear diffuse cost with no mechanism to collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic in the classical sense—the founding problem (accommodating remix culture in digital-age copyright) remains live and substantially unsolved by any competing mechanism. However, a secondary founding problem (ensuring copyright holder incentives and licensing markets) has been substantially solved by the doctrine's institutional acceptance, and the doctrine persists partly to maintain the subordination of that problem. This is not mandatrophy but rather asymmetric problem-solving: the doctrine solves remix culture's legal access problem while orphaning copyright incentive problems. The rising extractiveness and theater ratio at t0–t15, then plateau, suggest the doctrine has reached a steady-state coordination/extraction equilibrium where transformativeness claims are routinely granted (institutionalization), suppression requirement drops (normalized common sense), and theater rises (framing every derivative work as transformative becomes the path of least resistance). The divergence between claimed type (tangled_rope) and the coordinate seats' empirical experience (platforms: rope; copyright holders: snare) is the diagnostic signal here: the doctrine performs coordination for some seats while functioning as extraction for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_ambiguity,
    'What quantum of transformation is sufficient to trigger the fair use shelter under this reading? At what point does a derivative work add ''new meaning'' rather than merely exploit the original?',
    'Systematic empirical study of court decisions coding transformation threshold across genre and context; algorithmic analysis of judicial language around transformation in successful vs. unsuccessful fair use claims.',
    'A low transformation threshold (nearly any recontextualization counts) would increase extractiveness and reduce suppression, making the constraint more purely extractive for copyright holders. A high threshold (requiring substantial creative addition) would increase suppression requirement and reduce extractiveness, moving the constraint toward rope. Current jurisprudence sits between: parody is clearly transformative, sampling is contested, remix framing ranges from transformative to imitative depending on judicial panel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_threshold_ambiguity, conceptual, 'Ambiguity in what constitutes sufficient transformation to invoke fair use shelter.').

omega_variable(
    market_harm_subordination_boundaries,
    'Does the transformative-use reading actually subordinate market harm analysis, or does it merely reframe which markets count? If a remix creator profits commercially from a transformative use, should that profit count as market harm to the original work?',
    'Analysis of case law on commerciality and market harm post-transformative-use doctrine; comparison of damages awards in cases where transformation is high vs. low; empirical study of licensing revenue impact on copyright holder earnings when fair use is expansive.',
    'If market harm is truly subordinated (commerciality of the derivative use is not dispositive), the constraint is more extractive for copyright holders. If market harm remains a strong factor (commercial transformative uses are still subject to damages), the constraint is more balanced. Current practice: courts often find market harm is not fatal to fair use even in commercial transformative uses, supporting extraction interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_subordination_boundaries, empirical, 'Whether the transformative-use reading''s subordination of market harm is empirically as complete as doctrine claims.').

omega_variable(
    platform_capture_vs_cultural_benefit,
    'Does the transformative-use reading''s expansion of fair use serve cultural production broadly, or does it primarily capture value for commercial platforms (YouTube, TikTok) that host user-generated content?',
    'Economic analysis of platform profit vs. creator compensation under expansive fair use; comparison of licensing revenue for independent creators vs. platform-hosted creators; study of how transformative-use defense is deployed by platforms vs. grassroots creators in litigation.',
    'If the reading primarily benefits platforms, it is a false-summit mountain: framed as cultural freedom but functioning as extraction from copyright holders for platform benefit. If it benefits grassroots remix creators and independent cultural workers substantially, it is a genuine rope. Evidence suggests platforms disproportionately benefit; this would support snare classification for original copyright holders despite the rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_vs_cultural_benefit, empirical, 'Whether the transformative-use reading''s stated cultural benefits accrue to cultural workers or primarily to commercial platforms.').

omega_variable(
    reading_contest_institutional_lock,
    'Is the current dominance of the transformative-use reading in U.S. courts and policy a stable equilibrium, or is it contingent on institutional configurations that could shift?',
    'Historical analysis of fair use doctrine shifts; monitoring of recent judicial appointments and shifts in statutory fair use interpretation; comparative law analysis of how other jurisdictions balance transformativeness with creator rights.',
    'If institutional lock-in is high, the transformative-use reading will persist regardless of measured extractiveness or creator harm, and the constraint approaches piton. If institutional lock-in is low, shifting litigation and legislative pressure could shift the reading toward creator-centric or user-centric alternatives, fundamentally altering beneficiary and victim sets. Current signal: increasing judicial criticism of expansive fair use and platform capture suggests some institutional vulnerability, but doctrine remains institutionalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_institutional_lock, conceptual, 'Stability of the transformative-use reading''s institutional dominance.').

omega_variable(
    kernel_contest_reading_implications,
    'What structural differences distinguish the transformative-use reading from the creator-centric and user-centric readings? Specifically, where do the axioms diverge?',
    'Comparative case-law analysis tracking how courts frame transformation, market harm, and creator incentives across the three readings; mapping of academic and policy literatures endorsing each reading; analysis of legislation and regulatory guidance shaped by each reading.',
    'If the readings are truly distinct (coexisting positions held by different coalitions), the constraint family will show three separate stories with different ε, beneficiary/victim sets, and directionality profiles. If the readings are merely discursive framings of the same structural constraint, the family should show high correlation in metrics despite different framing language. Preliminary analysis: the readings are structurally distinct; they have genuinely different beneficiary/victim asymmetries and different suppression/resistance profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_implications, conceptual, 'Structural distinctness of the three fair use reading interpretations within the four-factor kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t5, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(fair_tr_t5, observed).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(fair_tr_t10, observed).
narrative_ontology:measurement(fair_tr_t15, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(fair_tr_t15, observed).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(fair_tr_t20, observed).
narrative_ontology:measurement(fair_tr_t25, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(fair_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t5, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(fair_be_t5, observed).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(fair_be_t10, observed).
narrative_ontology:measurement(fair_be_t15, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(fair_be_t15, observed).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(fair_be_t20, observed).
narrative_ontology:measurement(fair_be_t25, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(fair_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t5, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(fair_su_t5, observed).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(fair_su_t10, observed).
narrative_ontology:measurement(fair_su_t15, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(fair_su_t15, observed).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(fair_su_t20, observed).
narrative_ontology:measurement(fair_su_t25, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(fair_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_licensing_markets).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, platform_content_moderation_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested fair use four-factor test kernel. The transformative-use reading emphasizes transformation as dominant factor; the creator-centric reading emphasizes copyright property rights and market harm; the user-centric reading emphasizes public access and cultural freedom. Each reading has distinct ε, beneficiary/victim sets, and institutional support. They are not alternative framings of one constraint but three distinct constraints sharing a common kernel. The network links establish that each reading's classification and metrics are interdependent: if the transformative-use reading dominates case law, it influences (constrains and enables) the operation of the other two readings' operative scope in courts and policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
