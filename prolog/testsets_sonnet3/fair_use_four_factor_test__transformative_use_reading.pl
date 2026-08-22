% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Fair Use Doctrine — Transformative Use Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story authors ONE reading of the fair use four-factor test kernel:
 *   the transformative-use reading, under which courts (following Campbell v.
 *   Acuff-Rose 1994 through the Cariou/Warhol Foundation line) treat the
 *   first factor's transformativeness inquiry as dominant, subordinating the
 *   fourth factor's market-harm analysis whenever a secondary use is found to
 *   add 'new expression, meaning, or message.' This is structurally distinct
 *   from the creator-centric reading (fair use as narrow exception preserving
 *   creator incentives, market harm dispositive) and the user-centric reading
 *   (fair use as affirmative public right, factors weighed for cultural
 *   access) — those are separate constraints with their own ε and stakeholder
 *   structure, not alternate measurements of this one. The transformative-use
 *   reading has moderate, context-dependent extraction: it enables enormous
 *   coordination value for remix culture, education, and platform-scale UGC,
 *   but it also erodes licensing markets for working creators whose harm the
 *   doctrine's own weighting scheme discounts.
 *
 * KEY AGENTS:
 *   - remix_culture_creators: primary beneficiary (powerless/mobile) — legally shielded low-budget derivative work
 *   - ugc_tech_platforms: primary beneficiary and repeat-litigation agenda-shaper (institutional/arbitrage) — builds moderation policy and case law around the doctrine
 *   - original_rights_holders: primary target (powerful/constrained) — bears foregone licensing revenue
 *   - licensing_dependent_creators: secondary target (moderate/trapped) — most acute victim, least resourced to respond
 *   - federal_judiciary: agenda_setter (institutional/analytical) — elaborates the doctrine case by case
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
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Doctrine — Transformative Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'f4ebe1db-1857-4718-b687-8e3c6858f988').
narrative_ontology:cs_kernel_codification('f4ebe1db-1857-4718-b687-8e3c6858f988', fixed_text).
narrative_ontology:cs_authority_grounding('f4ebe1db-1857-4718-b687-8e3c6858f988', lineage).
narrative_ontology:cs_interpretation_layer_present('f4ebe1db-1857-4718-b687-8e3c6858f988').
narrative_ontology:cs_reading_relation('f4ebe1db-1857-4718-b687-8e3c6858f988', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4ebe1db-1857-4718-b687-8e3c6858f988', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('f4ebe1db-1857-4718-b687-8e3c6858f988', foundational, transformativeness_subordinates_market_harm).
narrative_ontology:cs_axiom_status(transformativeness_subordinates_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('f4ebe1db-1857-4718-b687-8e3c6858f988', transformativeness_subordinates_market_harm, conventional).
narrative_ontology:cs_axiom('f4ebe1db-1857-4718-b687-8e3c6858f988', secondary, new_meaning_test_governs_factor_one).
narrative_ontology:cs_axiom_status(new_meaning_test_governs_factor_one, holdable).
narrative_ontology:cs_axiom_grounding('f4ebe1db-1857-4718-b687-8e3c6858f988', new_meaning_test_governs_factor_one, instrumental).
narrative_ontology:cs_reference_frame('f4ebe1db-1857-4718-b687-8e3c6858f988', campbell_transformative_purpose_standard).
narrative_ontology:cs_drift_state('f4ebe1db-1857-4718-b687-8e3c6858f988', post_goldsmith_2023, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f4ebe1db-1857-4718-b687-8e3c6858f988', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_tech_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_and_commentary_producers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_dependent_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, stock_content_libraries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build parody, commentary, mashup, and meme content on top of existing copyrighted works. Under the transformative-use reading, courts weigh whether the new work adds meaning or message over whether it substitutes economically for the original, which shields a wide range of low-budget derivative work from infringement liability. Exit from the doctrine means either licensing (often unaffordable) or not creating at all.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_creators, beneficiary,
    powerless, biographical, mobile, global).

% Host billions of user uploads that recontextualize copyrighted material — reaction videos, edits, AI-training datasets scraped from copyrighted corpora. A transformativeness-dominant test lets platforms build content moderation and DMCA response policy around 'does this add new meaning' rather than negotiating blanket licenses, which is enormously cheaper than the alternative and shapes case law through repeated litigation and lobbying that push transformativeness further to the fore.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_tech_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_tech_platforms, agenda_setter).

% Rely on quoting film clips, news footage, and images for criticism, education, and historical documentation. The transformative-use reading is what makes their work legally viable at all — clearing rights for every clip at market rate would be prohibitively expensive and often impossible when rights holders refuse licensing for critical uses.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_and_commentary_producers, beneficiary,
    moderate, biographical, constrained, national).

% Musicians, photographers, novelists, and studios whose works are repurposed without payment when courts find sufficient transformativeness. They bear lost licensing revenue and diminished control over how their works appear in public, and their only recourse is expensive litigation with uncertain outcomes because the transformativeness inquiry is famously indeterminate. Market-harm evidence that would once have been dispositive is subordinated to a qualitative judgment about 'new meaning.'
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_rights_holders, payer,
    powerful, biographical, constrained, global).

% Photographers, illustrators, and session musicians whose income model depends on per-use licensing fees. When downstream uses are deemed transformative, the licensing market for their work erodes even though direct substitution never happens the way it does for pirated distribution. They cannot restructure their business around a doctrine that subordinates the harm they can actually document.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_dependent_creators, payer,
    moderate, biographical, trapped, national).

% Aggregate and license stock photography, footage, and music at scale. Transformative-use rulings that excuse use of licensable stock content without payment directly cannibalize their catalog revenue, and they lack the individual leverage of a major studio to litigate every instance.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, stock_content_libraries, payer,
    organized, generational, constrained, global).

% Applies and elaborates the four-factor test case by case, with the transformativeness inquiry under the first factor increasingly treated as dispositive since Campbell v. Acuff-Rose and its progeny (Cariou, Warhol Foundation, Google v. Oracle line). Each ruling reshapes how much weight market harm retains, effectively legislating doctrine through interpretation without formal statutory amendment.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Songwriter guilds, photographer associations, and independent publishers argue that transformativeness has swallowed the market-harm factor entirely, effectively rewriting fair use into a free license for anyone who can articulate a 'new meaning' argument. They lobby for statutory clarification but have limited direct influence on case-by-case judicial doctrine formation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, creator_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, ugc_tech_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows cultural production — criticism, commentary, parody, remix, education, and technological innovation (search indexing, AI training) — to proceed without a case-by-case licensing negotiation for every quotation or transformation of a prior work, solving a real transaction-cost problem that would otherwise choke commentary and derivative culture.
% TRANSFER_FUNCTION: Moves the economic value of certain secondary uses from original rights holders (who forgo licensing revenue) to downstream creators and platforms (who capture the value of the new work without paying for the underlying material), the size of the transfer determined by how far the transformativeness threshold has crept from Campbell's original 'add new expression' formulation.
% ABSENT_VOICES: Individual working photographers, session musicians, and freelance illustrators — the highest-volume victims of eroded licensing markets — are rarely parties to the landmark cases that set doctrine; the doctrine is shaped in litigation between well-resourced platforms/institutions (Google, Warhol Foundation) and well-resourced plaintiffs (Oracle, Lynn Goldsmith), leaving the diffuse harm to smaller rights holders largely unlitigated and unrepresented in the case law that governs them.
% DISAPPEARANCE_RATIONALE: If courts abandoned transformativeness as the dominant factor and returned to market-harm-centric balancing, large swaths of remix culture, meme commentary, and platform-hosted derivative content would face renewed infringement exposure overnight; platforms would need to rebuild licensing infrastructure or aggressive takedown policies, and rights holders would regain substantial negotiating leverage over secondary uses that currently proceed unlicensed.
% FOUNDING_PROBLEM: Copyright's exclusive rights, applied literally, would prohibit quotation, criticism, and parody essential to free expression and cultural commentary — the four-factor test (and later the transformativeness gloss) was built to carve space for socially valuable secondary use without gutting the incentive function of copyright.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and platform policy teams (interested parties) attest the doctrine still serves core free-expression functions. Independent copyright scholars outside both the platform and rights-holder camps (e.g. academic critiques of the post-Warhol Foundation v. Goldsmith landscape) and the Supreme Court's own 2023 Goldsmith opinion attest that transformativeness expanded well beyond the original commentary/parody rationale into a general-purpose license for reuse, suggesting the founding problem has been substantially answered but the doctrine's scope has continued to grow past it.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.48, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.22 (Campbell, 1994) to 0.48 (post-Goldsmith, 2024) as the transformativeness inquiry has been applied to progressively less commentary-focused uses (Cariou's minimal alteration standard, pre-Goldsmith AI training arguments) — this is a real accumulation, not noise, and the Supreme Court's 2023 Warhol Foundation v. Goldsmith decision represents a partial correction that the post-2019 uptick and subsequent plateau reflect. Suppression (0.42) is moderate: the doctrine does not suppress alternatives by force, but litigation cost functions as a suppression mechanism — a rights holder who cannot afford to litigate the transformativeness question functionally loses the factor regardless of its merits. Theater ratio (0.28) captures that a meaningful share of transformativeness argumentation in litigation is now performative doctrinal signaling rather than substantive analysis of what new meaning was actually added.
 *
 * PERSPECTIVAL GAP:
 *   From the tech-platform/remix-creator seat, transformative use looks like durable, well-functioning coordination that lets valuable secondary culture exist without transaction-cost paralysis. From the licensing-dependent-creator seat, the same doctrine looks like an ever-expanding license to use their work without payment, dressed in First Amendment language. Both readings are structurally accurate from their respective positions; the engine computes the divergence from the beneficiary/victim/enforcement data rather than from either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix creators, platforms, and commentary/documentary producers are beneficiaries: the doctrine subsidizes their production by shielding them from licensing negotiation (d low, near full beneficiary). Original rights holders and licensing-dependent creators are targets: the doctrine extracts licensing revenue they would otherwise capture, and their exit options are constrained (litigation is expensive, non-litigation means simply absorbing the loss) or trapped (individual creators with no bargaining leverage). Stock content libraries sit as organized-but-structurally-targeted: they have some collective resources but face the same erosion. Tech platforms occupy a dual role — beneficiary of the legal shield AND an active agenda-setter shaping how far transformativeness expands through repeat litigation, which the secondary_role marks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting criticism, parody, and education from copyright's literal reach) remains genuinely live for the beneficiary set closest to the doctrine's original Campbell rationale (parody, commentary, documentary use) — classifying this as pure extraction would erase a real and still-functioning coordination function. But the doctrine's application has drifted well past that founding case toward general-purpose reuse licensing for commercial platforms, which is why tangled_rope (not rope) is the structurally accurate claim: coordination and extraction now run through the same mechanism, and disentangling them requires exactly the kind of transformativeness-threshold litigation the doctrine itself generates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_threshold_indeterminacy,
    'Where does the transformativeness inquiry stop tracking Campbell''s original ''new expression, meaning, or message'' rationale and start functioning as a general license for any reuse a court finds sufficiently reinterpretive?',
    'Systematic coding of post-Campbell circuit court opinions for the marginal degree of alteration found sufficient to satisfy factor one, tracked against whether factor four (market harm) was still independently dispositive in the outcome.',
    'If the threshold has drifted to near-zero alteration required, the doctrine functions closer to a snare against rights holders with a coordination-function veneer; if the threshold remains anchored to genuine recontextualization, tangled_rope with moderate epsilon is the accurate read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_threshold_indeterminacy, empirical, 'How far the transformativeness threshold has drifted from its founding rationale.').

omega_variable(
    goldsmith_correction_durability,
    'Does the Supreme Court''s 2023 Warhol Foundation v. Goldsmith decision represent a durable doctrinal correction restoring market-harm weight, or a narrow fact-specific ruling that lower courts will distinguish away, leaving the transformativeness-dominant trend intact?',
    'Track post-2023 circuit court application of Goldsmith across non-visual-art fair use disputes (music sampling, AI training, software) to see whether market harm regains independent weight or Goldsmith is cabined to close licensing-market-substitute facts.',
    'A durable correction would flatten or reverse the extractiveness trajectory; a narrow reading preserves the current tangled_rope trajectory toward higher extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(goldsmith_correction_durability, empirical, 'Whether Goldsmith durably rebalances the four-factor test or is distinguished away.').

omega_variable(
    kernel_reading_dominance,
    'Which of the three kernel readings (creator-centric, transformative-use, user-centric) will dominant appellate doctrine ultimately settle into, if any single reading prevails at all?',
    'Longitudinal tracking of Supreme Court and circuit split resolution on fair use doctrine; a genuine resolution would require either statutory amendment or a decisive, widely-followed Supreme Court synthesis (which Goldsmith only partially attempted).',
    'If the user-centric reading gains doctrinal ascendance, the beneficiary set for transformative use expands further (public access framing); if the creator-centric reading regains dominance, the extraction this story measures would sharply decline as market-harm analysis reasserts primacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Which sibling reading of the fair use kernel will become doctrinally dominant, and whether the contest resolves at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(fair_tr_t2006, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2006, 0.17).
narrative_ontology:measurement(fair_tr_t2013, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2013, 0.21).
narrative_ontology:measurement(fair_tr_t2019, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(fair_be_t2006, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2006, 0.33).
narrative_ontology:measurement(fair_be_t2013, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(fair_be_t2019, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2019, 0.44).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(fair_su_t2006, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2006, 0.35).
narrative_ontology:measurement(fair_su_t2013, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2013, 0.38).
narrative_ontology:measurement(fair_su_t2019, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'fair use doctrine' per the epsilon-invariance principle: creator_centric_reading (narrow exception, market-harm dispositive, low epsilon from the creator-protection lens), transformative_use_reading (this story — moderate epsilon, transformativeness-dominant balancing), and user_centric_reading (affirmative public right, epsilon assessed from the access/cultural-production lens). Each reading shares the same kernel (the statutory four-factor test) but produces a structurally distinct constraint with its own beneficiary/victim set and classification, because the readings disagree about which factor should dominate the balancing — not merely about whether the outcome is good.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
