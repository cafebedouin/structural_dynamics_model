% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Fair Use Four-Factor Test — Transformative Use Dominant Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story authors the transformative-use reading of the fair use
 *   four-factor test: the doctrinal current, traceable to Campbell v.
 *   Acuff-Rose (1994) and its progeny, in which a finding that a secondary
 *   use adds 'new expression, meaning, or message' substantially discounts
 *   the fourth factor's inquiry into market harm. This reading is distinct
 *   from the creator-centric reading (fair use as a narrow exception
 *   preserving creator incentives, weighting market harm heavily) and the
 *   user-centric reading (fair use as an affirmative public right, weighting
 *   public access and cultural production over any single factor). All three
 *   are readings of the same kernel — the statutory four-factor test at 17
 *   U.S.C. §107 — but they instantiate structurally different constraints
 *   with different beneficiary/victim sets and different epsilon values. This
 *   story's epsilon is authored for the transformative-use reading's own
 *   operation: moderate extraction, rising over the thirty-year interval as
 *   courts (and increasingly, well-resourced secondary users like AI training
 *   operators) leaned harder on transformativeness to license de facto free
 *   use of others' works, until Warhol v. Goldsmith (2023) began pulling
 *   market harm back toward parity.
 *
 * KEY AGENTS:
 *   - remix_culture_creators: beneficiary of loose transformation threshold (moderate power, constrained exit)
 *   - ugc_technology_platforms: institutional beneficiary and de facto co-architect of the doctrine's expansion (institutional power, arbitrage exit)
 *   - original_rightsholders_with_derivative_markets: bears subordinated market-harm analysis (organized power, constrained exit)
 *   - midlist_authors_and_photographers: diffuse, powerless victims of aggregate appropriation (trapped exit)
 *   - federal_judiciary: agenda-setting seat elaborating and recalibrating the doctrine case by case
 *   - ai_training_data_operators: newest and most aggressive beneficiary class, pressing transformativeness to its outer limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.46).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.38).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test — Transformative Use Dominant Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '8caf54fb-3fe1-4234-8ad8-1ff28723936d').
narrative_ontology:cs_kernel_codification('8caf54fb-3fe1-4234-8ad8-1ff28723936d', fixed_text).
narrative_ontology:cs_authority_grounding('8caf54fb-3fe1-4234-8ad8-1ff28723936d', lineage).
narrative_ontology:cs_interpretation_layer_present('8caf54fb-3fe1-4234-8ad8-1ff28723936d').
narrative_ontology:cs_reading_relation('8caf54fb-3fe1-4234-8ad8-1ff28723936d', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8caf54fb-3fe1-4234-8ad8-1ff28723936d', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('8caf54fb-3fe1-4234-8ad8-1ff28723936d', foundational, new_meaning_subordinates_market_substitution).
narrative_ontology:cs_axiom_status(new_meaning_subordinates_market_substitution, holdable).
narrative_ontology:cs_axiom_grounding('8caf54fb-3fe1-4234-8ad8-1ff28723936d', new_meaning_subordinates_market_substitution, conventional).
narrative_ontology:cs_axiom('8caf54fb-3fe1-4234-8ad8-1ff28723936d', secondary, purpose_and_character_is_the_primary_factor).
narrative_ontology:cs_axiom_status(purpose_and_character_is_the_primary_factor, holdable).
narrative_ontology:cs_axiom_grounding('8caf54fb-3fe1-4234-8ad8-1ff28723936d', purpose_and_character_is_the_primary_factor, conventional).
narrative_ontology:cs_reference_frame('8caf54fb-3fe1-4234-8ad8-1ff28723936d', campbell_transformative_purpose_standard).
narrative_ontology:cs_drift_state('8caf54fb-3fe1-4234-8ad8-1ff28723936d', post_warhol_goldsmith_2023, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8caf54fb-3fe1-4234-8ad8-1ff28723936d', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_technology_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_and_commentary_producers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_rightsholders_with_derivative_markets).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, midlist_authors_and_photographers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ai_training_data_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces parodies, mashups, commentary videos, and appropriation art that repurposes existing copyrighted material with new meaning or message. Under the transformative-use reading, a court finding sufficient transformation can excuse substantial market substitution analysis, letting the work proceed without license. Exit from the doctrine means either not creating the work, obtaining a license (often unavailable or prohibitively priced for individual creators), or risking litigation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_creators, beneficiary,
    moderate, biographical, constrained, national).

% Hosts billions of user uploads that recontextualize copyrighted content — reaction videos, remixes, memes, AI-training corpora built from scraped works. Benefits enormously from a doctrine that treats transformation as near-dispositive because it reduces the platform's own licensing exposure and litigation risk for user content. Lobbies for expansive transformativeness readings in litigation and legislative comment, and can route infringement risk through DMCA safe harbors while the transformative-use standard does the substantive legal work of narrowing the fourth factor.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_technology_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_technology_platforms, agenda_setter).

% Relies on quoting, excerpting, and recontextualizing copyrighted footage, images, and text to produce criticism, scholarship, and journalism. The transformative-use reading is often the only route to using necessary source material without ruinous licensing costs. Without this reading's dominance, many documentaries about controversial or well-funded subjects would be effectively unmakeable.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_and_commentary_producers, beneficiary,
    moderate, biographical, constrained, national).

% Owns copyrights whose value depends partly on licensing derivative and adaptation markets (sequels, remixes, merchandising, AI-training licenses). When courts find a secondary use transformative, the fourth factor's market-harm analysis is frequently subordinated even where the secondary use competes with or forecloses a market the rightsholder was actively developing or could plausibly develop. Legal recourse exists but is expensive and outcome-uncertain given how malleable transformativeness has become post-Warhol/Campbell.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_rightsholders_with_derivative_markets, payer,
    organized, biographical, constrained, national).

% Operates clearance and licensing businesses (stock photo agencies, sync licensing houses, rights clearance services) whose entire commercial function is priced against the assumption that reuse requires permission. Every expansion of the transformative-use doctrine shrinks the addressable market for licensing services, since more uses fall inside the fair-use safe zone without payment. Cannot exit the doctrine's effects; can only lobby for narrower transformativeness tests or diversify into non-licensing services.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_intermediaries, payer,
    moderate, biographical, constrained, national).

% Individual creators without the resources of major studios or large publishers, whose single-work income depends on licensing fees for reuse, adaptation, and AI-training rights. When large platforms or well-resourced secondary users successfully argue transformation, individual creators bear concentrated income loss that is rarely worth individually litigating. Cannot practically pursue costly infringement suits against institutional users; effectively trapped by the asymmetry between the cost of enforcement and the value of any single use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, midlist_authors_and_photographers, payer,
    powerless, biographical, trapped, national).

% Applies and elaborates the four-factor test case by case, deciding how much weight transformativeness carries relative to market harm. Post-Campbell v. Acuff-Rose, transformativeness became the analytical center of gravity; post-Warhol v. Goldsmith (2023), courts began recalibrating toward greater market-harm weight, showing the reading itself is judicially contested and actively shifting rather than settled doctrine.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Trains large models on copyrighted text, images, and code at scale, arguing that using works as statistical training signal is quintessentially transformative because the output does not reproduce the expressive content of any single input. This is the most aggressive contemporary extension of the transformative-use reading and the site of its highest-stakes ongoing litigation, where the fourth factor's market-harm analysis (licensing markets for AI training data) is most sharply contested.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ai_training_data_operators, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable legal standard letting courts distinguish socially valuable reuse (criticism, parody, scholarship, follow-on innovation) from mere substitution, without requiring case-by-case legislative enumeration of every permitted use.
% TRANSFER_FUNCTION: Shifts the economic value of secondary uses from rightsholders and licensing intermediaries to secondary users (platforms, remixers, documentarians, AI developers) whenever a court characterizes the use as sufficiently transformative to subordinate market-harm analysis under the fourth factor.
% ABSENT_VOICES: Individual midlist creators whose works are used piecemeal in aggregate training corpora or viral remix content are rarely parties to the landmark litigation that sets transformativeness doctrine — the shaping cases involve well-resourced parties (major studios, large publishers, big tech) on at least one side, so the doctrine's contours are set without the class of creators who bear its most diffuse costs.
% DISAPPEARANCE_RATIONALE: If the transformative-use reading's dominance disappeared and courts reverted to weighing all four factors evenly with market harm restored to primacy, large swaths of remix culture, documentary practice, and AI training would face immediate licensing demands or injunctions; platforms would need new legal theories or licensing infrastructure; and licensing intermediaries would see restored demand. The doctrine's current shape actively organizes multi-billion-dollar industries (UGC platforms, generative AI) around its presumption of permissibility.
% FOUNDING_PROBLEM: Copyright's exclusive rights, applied literally, would criminalize criticism, parody, scholarship, and news reporting that must quote or reference the work being discussed — fair use exists to prevent copyright from swallowing free expression and follow-on creativity.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside both plaintiff and defendant interests (e.g., in amicus briefs before the Warhol decision) attest the founding free-expression problem remains live for core cases (parody, criticism) but argue the transformative-use reading has been stretched by well-resourced secondary users — especially AI operators — well beyond the doctrine's original justification into a general license to appropriate at scale; the Supreme Court's 2023 Warhol v. Goldsmith opinion itself, authored by justices with no stake in either party's commercial outcome, explicitly criticized lower courts for letting transformativeness swallow the market-harm factor.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).
:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.46) is moderate rather than high because the transformative-use reading genuinely does the coordination work of freeing socially valuable speech (parody, criticism, scholarship) from licensing paralysis — this is not pure extraction cover. But it is authored as tangled_rope rather than rope because the same structural mechanism that frees legitimate transformative speech also lets well-resourced secondary users (platforms, AI operators) subordinate market-harm findings in cases where real substitutionary harm exists, concentrating loss on midlist creators without meaningful individual recourse. Suppression (0.38) reflects that alternatives are not fully foreclosed — rightsholders can litigate, license markets persist for uses courts find non-transformative — but the doctrine's post-Campbell drift raised the practical bar for a market-harm claim to survive a transformativeness finding, requiring active judicial correction (Warhol) to partially reverse. Theater ratio rises modestly (0.12→0.28) as more litigation and commentary invoke 'transformativeness' rhetoric even in contexts (e.g., wholesale AI training corpora) where the underlying transformation is contested, suggesting some proxy-goal substitution of the label for the substantive inquiry the four factors originally required.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix creators, documentarians, and UGC platforms sit near the beneficiary end: the doctrine's dominant weighting of transformativeness directly reduces their licensing exposure and litigation risk. Original rightsholders with derivative markets and licensing intermediaries sit near the target end: the same weighting subordinates the analysis that would otherwise protect their markets. Midlist authors and photographers are pushed furthest toward the target end by the trapped exit_options override logic — even though structurally similar to organized rightsholders, their inability to litigate individually against institutional secondary users (especially AI training operators) means the derived directionality undersells their exposure without an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that literal copyright enforcement would criminalize criticism, parody, and scholarship — remains partly live (courts still need a mechanism for these categories) but the doctrine's dominant form has drifted to cover uses (large-scale AI training, algorithmic remix at platform scale) that the 1994 Campbell court did not contemplate and that stress the free-expression rationale to its limit. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: it would be wrong to call the whole arrangement a snare (it does perform real, valuable coordination for parody/criticism/scholarship), and it would be wrong to call it a pure rope (it now channels asymmetric extraction from diffuse individual creators to institutional secondary users without their meaningful participation or consent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warhol_recalibration_trajectory,
    'Does the Warhol v. Goldsmith (2023) reassertion of market-harm weight represent a durable doctrinal correction back toward parity among the four factors, or a narrow, fact-specific holding that leaves transformative-use dominance intact for most cases?',
    'Track post-Warhol circuit court applications of the four-factor test over the next decade: if market-harm findings begin routinely defeating transformativeness claims outside the narrow Warhol fact pattern (licensing markets for the same use), the correction is durable; if courts distinguish Warhol narrowly and transformativeness continues to dominate elsewhere, the reading''s pre-2023 dominance persists.',
    'A durable correction would shift this constraint''s classification toward a more balanced reading (converging with the user-centric reading''s weighting) and lower measured extractiveness; a narrow holding would leave the transformative-use reading''s current extraction profile intact or rising, especially as AI-training litigation tests the doctrine''s outer edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warhol_recalibration_trajectory, empirical, 'Whether Warhol durably rebalances the four factors or is confined to its facts.').

omega_variable(
    transformation_threshold_indeterminacy,
    'Is ''sufficient transformation'' a coherent, administrable legal standard, or does its inherent vagueness let courts reach whatever market-harm conclusion they prefer under transformativeness cover?',
    'Empirical coding of appellate opinions for inter-judge and inter-circuit consistency in transformativeness findings on comparable facts; high variance across similar fact patterns would indicate the standard functions as a discretionary lever rather than a predictable rule.',
    'If the standard is genuinely indeterminate, the doctrine''s coordination function (predictable ex ante guidance for creators and platforms) is substantially undermined, pushing this constraint''s classification closer to snare (extraction dressed as principled balancing); if reasonably consistent, the tangled_rope classification (real coordination plus asymmetric extraction) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_threshold_indeterminacy, conceptual, 'Whether the transformativeness standard is administrable or a discretionary cover for outcome-driven balancing.').

omega_variable(
    ai_training_transformation_classification,
    'Is using copyrighted works as statistical training signal for generative AI models genuinely transformative in the Campbell sense (new purpose, new meaning), or is it a novel category the four-factor test was never designed to evaluate, being litigated under a label that doesn''t structurally fit?',
    'Ongoing federal litigation (e.g., authors'' guild and visual-artist suits against major AI developers) will produce appellate rulings that either extend transformativeness doctrine to training-data use or carve out a distinct analytical framework for machine-learning ingestion.',
    'If courts extend transformativeness to cover AI training wholesale, this reading''s beneficiary set expands dramatically and its extraction from individual creators rises sharply, potentially warranting a decomposed sibling story (ε-invariance principle) rather than folding AI training into this constraint''s existing metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_transformation_classification, empirical, 'Whether AI training data use is properly analyzed under this reading''s transformativeness framework or requires its own constraint story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fair_tr_t2006, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement(fair_tr_t2012, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2012, 0.23).
narrative_ontology:measurement(fair_tr_t2018, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(fair_be_t2006, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2006, 0.34).
narrative_ontology:measurement(fair_be_t2012, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement(fair_be_t2018, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(fair_su_t2006, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2006, 0.34).
narrative_ontology:measurement(fair_su_t2012, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(fair_su_t2018, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fair_use_four_factor_test kernel, decomposed per the epsilon-invariance principle because the same statutory text produces structurally distinct beneficiary/victim/extraction profiles depending on which factor dominates the balancing. The creator_centric_reading authors higher extraction from users/platforms with rightsholders as beneficiaries; the user_centric_reading authors lower extraction with public-access framing dominant and no single factor privileged; this transformative_use_reading sits between them, with extraction rising over time as the doctrine's practical application drifted toward favoring institutional secondary users. All three share the same underlying case law corpus but are authored as separate constraints, not as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
