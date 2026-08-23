% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test â Transformative Use Reading
 *   domain: legal/cultural_production
 *
 * SUMMARY:
 *   This constraint story instantiates the transformative use reading of the
 *   fair use four-factor test in U.S. copyright law. The reading elevates the
 *   first factorâpurpose and character of useâsuch that adding 'new
 *   meaning, message, or purpose' dominates the analysis and subordinates the
 *   fourth factor (market harm). It is one reading of the contested fair-use
 *   kernel, alongside the creator-centric reading (narrow exception
 *   preserving incentives) and the user-centric reading (affirmative right
 *   preserving access). Structurally, the reading coordinates a vast
 *   ecosystem of remix culture and platform-hosted UGC while extracting
 *   licensing value from original creators and commercial rights holders.
 *
 * KEY AGENTS:
 *   - Remix culture producers: Primary beneficiaries (moderate power, constrained exit) â gain expressive freedom at the cost of legal uncertainty.
 *   - Tech platforms enabling UGC: Structural beneficiaries (institutional power, constrained exit) â monetize content without licensing infrastructure.
 *   - Original creators: Primary payers (moderate power, constrained exit) â lose exclusivity and licensing revenue to transformative appropriation.
 *   - Commercial rights holders: Deep payers (powerful, constrained exit) â see derivative and sync licensing markets eroded by broad transformativeness findings.
 *   - Federal judiciary: Agenda-setter (institutional, analytical exit) â controls doctrinal threshold through precedent and factor weighting.
 *   - Public domain advocates: Observers (organized, mobile exit) â promote broad fair use but do not bear direct costs or gains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.55).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.62).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test â Transformative Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '797cf797-9c9e-42c8-8791-a652f3c47f04').
narrative_ontology:cs_kernel_codification('797cf797-9c9e-42c8-8791-a652f3c47f04', fixed_text).
narrative_ontology:cs_authority_grounding('797cf797-9c9e-42c8-8791-a652f3c47f04', lineage).
narrative_ontology:cs_interpretation_layer_present('797cf797-9c9e-42c8-8791-a652f3c47f04').
narrative_ontology:cs_reading_relation('797cf797-9c9e-42c8-8791-a652f3c47f04', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('797cf797-9c9e-42c8-8791-a652f3c47f04', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('797cf797-9c9e-42c8-8791-a652f3c47f04', foundational, transformativeness_as_speech_protective_priority).
narrative_ontology:cs_axiom_status(transformativeness_as_speech_protective_priority, holdable).
narrative_ontology:cs_axiom_grounding('797cf797-9c9e-42c8-8791-a652f3c47f04', transformativeness_as_speech_protective_priority, conventional).
narrative_ontology:cs_axiom('797cf797-9c9e-42c8-8791-a652f3c47f04', foundational, market_harm_subordination_doctrine).
narrative_ontology:cs_axiom_status(market_harm_subordination_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('797cf797-9c9e-42c8-8791-a652f3c47f04', market_harm_subordination_doctrine, conventional).
narrative_ontology:cs_reference_frame('797cf797-9c9e-42c8-8791-a652f3c47f04', campbell_speech_protective_balance).
narrative_ontology:cs_drift_state('797cf797-9c9e-42c8-8791-a652f3c47f04', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('797cf797-9c9e-42c8-8791-a652f3c47f04', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, commercial_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create derivative works, parodies, remixes, and reaction content relying on the transformative use reading as a legal shield against infringement claims. Their creative practice depends on courts finding new meaning or message in their appropriations; they cannot purchase licenses for the volumes of source material they use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_producers, beneficiary,
    moderate, biographical, constrained, national).

% Host and monetize massive volumes of user-generated content. The transformative use reading reduces aggregate licensing exposure and supports safe-harbor defenses. They capture ad revenue against content that often incorporates third-party works without payment; their business model scales only if transformativeness findings remain broad and routine.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc, beneficiary,
    institutional, generational, constrained, global).

% Produce expressive works that may be incorporated into others without license or compensation when courts find the secondary use transformative. They cannot contract out of fair use; their exclusive rights are narrowed by a judicial standard they do not control and whose threshold shifts unpredictably.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_creators, payer,
    moderate, biographical, constrained, national).

% Invest in catalogs and derivative licensing markets. The transformative use reading erodes predictable revenue streams for sampling, sync, and adaptation rights by allowing uncompensated uses that may compete with licensed markets. They litigate to raise the transformation threshold but face precedent trending against market-harm emphasis.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, commercial_rights_holders, payer,
    powerful, generational, constrained, global).

% Interprets 17 U.S.C. Â§ 107 through case law, with the transformative use reading elevating the first factor and compressing the fourth. Circuit courts apply Campbell-era precedent with varying emphasis; the judiciary controls the doctrinal threshold through opinion-writing and selective citation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% File amicus briefs and publish scholarship arguing for broad fair use to promote access to knowledge and cultural participation. They do not directly collect or pay under the constraint but observe and seek to influence its doctrinal trajectory.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, public_domain_advocates, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for follow-on speech, commentary, and parody without requiring individual licensing negotiations that would be infeasible at internet scale; coordinates a shared understanding of when appropriation is permissible.
% TRANSFER_FUNCTION: Moves licensing value and monetization opportunity from original creators and rights holders to secondary users and platforms by permitting uncompensated incorporation of existing works when courts find new meaning or purpose.
% ABSENT_VOICES: Individual artists without litigation resources whose works are appropriated but who cannot afford to test the transformation threshold; international creators operating under moral-rights traditions that reject the transformativeness logic; small licensing collectives excluded from platform-deal negotiations.
% DISAPPEARANCE_RATIONALE: If the transformative use reading vanished overnight, platform content-moderation and licensing strategies would shift toward blanket or automated licensing; remix communities would face chilling effects; courts would revert to market-harm-centric balancing; and the UGC economy would contract or restructure around rights-clearance systems.
% FOUNDING_PROBLEM: Copyright exclusivity threatened to chill criticism, parody, and follow-on creativity that depends on quoting or referencing existing works; the four-factor test was designed to prevent copyright from becoming a veto on speech.
% FOUNDING_PROBLEM_CORROBORATION: First Amendment legal historians and free-speech scholars corroborate the chilling-effect rationale. Rights-holder economists and creator guilds contest that the current reading still solves that problem, arguing it has instead created a market failure in which platforms externalize licensing costs onto creators.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.55) is moderate but rising: the doctrine permits large-scale uncompensated use by platforms and remixers, transferring licensing value away from creators. Suppression (0.62) is higher than extraction because the reading's persistence depends on judicial precedent actively suppressing alternative market-harm-centric interpretations. Theater ratio (0.42) reflects that an increasing share of judicial balancing is performativeâcourts recite all four factors while effectively letting transformativeness dictate the outcome. Accessibility collapse (0.48) captures partial collapse of licensing alternatives; resistance (0.60) reflects sustained litigation and lobbying by content industries. Measurements share a single time grid aligned to the interval to prevent temporal misclassification.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal judiciary) experiences the constraint as a flexible standard that advances speech values, while payer seats (original creators and commercial rights holders) experience it as a ratchet that narrows their property rights. Beneficiary seats (remixers and platforms) experience it as essential infrastructure. The engine computes this divergence from structural data: same legal text, opposite directionality depending on whether the agent is shielded or exposed by the doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix culture producers and tech platforms are structural beneficiaries (low d): the constraint subsidizes their activity by reducing licensing liability. Original creators and commercial rights holders are structural targets (high d): the constraint extracts from their exclusive rights. The federal judiciary sits near symmetric (moderate d) because it both administers the test and is bound by its own precedent. Public domain advocates are analytical observers with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy interview, this constraint could be misread as a pure Snare (platforms extracting from creators) or a pure Rope (coordination of speech). The Tangled Rope classification is warranted because: (1) a genuine coordination function existsâremix and commentary are speech forms that licensing markets cannot practicably clear; (2) asymmetric extraction is presentâplatforms capture ad revenue against unlicensed appropriations; and (3) active enforcement is requiredâjudicial precedent must continually reassert the dominance of transformativeness over market harm. The metrics and claim are authored independently; the engine will verify whether the structural data support the claimed type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_indeterminacy,
    'Is transformativeness a judicially manageable standard, or does its indeterminacy mask policy-driven outcomes that systematically favor platforms over creators?',
    'Empirical study of fair use outcomes controlling for litigant resources, platform status, and use type; comparison of inter-circuit variance in transformativeness findings.',
    'If the standard is indeterminate and outcomes track platform status, the coordination story weakens and the extraction story strengthens, shifting computed classification toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_threshold_indeterminacy, empirical, 'Indeterminacy of the transformativeness standard').

omega_variable(
    market_harm_circularity,
    'Does subordinating market harm to transformativeness create circularity where any ''new meaning'' defeats a licensing market that would otherwise exist?',
    'Economic analysis of foregone licensing revenue in categories where transformative appropriation is now routine (sampling, reaction video, appropriation art).',
    'If the doctrine circularly defines markets out of existence, extraction is higher than the base metric suggests; if markets genuinely fail to form for creative reasons, coordination is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_circularity, conceptual, 'Circularity between transformativeness and market harm').

omega_variable(
    reading_family_boundary,
    'Has the transformative use reading drifted so far toward user-centric outcomes that it has effectively merged with the user-centric reading, collapsing the kernel''s internal structure?',
    'Doctrinal mapping of recent appellate decisions to see whether transformativeness is treated as a balancing factor or as an outcome-determinative safe harbor.',
    'If merged, the kernel decomposes into two rather than three live readings; the present constraint would need re-labeling and re-linking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_family_boundary, conceptual, 'Drift toward merger with user-centric reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_trans_tr_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_use_trans_tr_t6, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(fair_use_trans_tr_t12, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(fair_use_trans_tr_t18, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(fair_use_trans_tr_t24, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(fair_use_trans_tr_t30, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_use_trans_be_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fair_use_trans_be_t6, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(fair_use_trans_be_t12, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(fair_use_trans_be_t18, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(fair_use_trans_be_t24, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(fair_use_trans_be_t30, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_trans_su_t0, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fair_use_trans_su_t6, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(fair_use_trans_su_t12, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(fair_use_trans_su_t18, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(fair_use_trans_su_t24, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(fair_use_trans_su_t30, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, identity_coordination).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, user_centric_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the fair_use_four_factor_test kernel family. The kernel decomposes into three structurally distinct readings because each reading assigns a different epsilon, beneficiary set, and directional logic to the same statutory text. The transformative use reading is linked to its siblings as coexisting doctrinal positions within the same legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
