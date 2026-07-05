% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Fair Use Doctrine — Transformative Use Dominance Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates one reading of the fair use four-factor test
 *   kernel: the transformative-use-dominant reading, under which courts
 *   (post-Campbell v. Acuff-Rose, 1994) increasingly weighted whether a
 *   secondary use added new expression, meaning, or message so heavily that
 *   the fourth factor — effect on the market for the original — was
 *   frequently subordinated or discounted even where an active licensing
 *   market existed. This reading structurally favors remix culture,
 *   documentary and commentary production, and — most consequentially in its
 *   recent extension — AI training-data aggregation, at the expense of
 *   individual rightsholders whose works are reused without compensation once
 *   a court finds sufficient 'transformation.' The 2023 Warhol Foundation v.
 *   Goldsmith decision partially checked this reading's expansion by
 *   re-elevating market-harm and commercial-purpose scrutiny, which the
 *   measurement dip at 2023 reflects — the doctrine did not return to a
 *   market-harm-dominant reading, but the transformative-use reading's
 *   dominance became less absolute.
 *
 * KEY AGENTS:
 *   - remix_and_parody_creators: primary beneficiary (moderate/mobile) — builds new works on existing ones under discounted market-harm scrutiny
 *   - ugc_hosting_platforms: primary institutional beneficiary and agenda-setter (institutional/arbitrage) — shapes doctrine development through litigation strategy
 *   - ai_training_data_aggregators: powerful emergent beneficiary (powerful/arbitrage) — stretches the reading to non-human, non-expressive 'transformation'
 *   - midlist_photographers_and_illustrators: primary victim (powerless/trapped) — bears uncompensated reuse of licensable work
 *   - federal_judiciary: agenda-setter and adjudicator (institutional/analytical) — actively revises the doctrine's balance case by case
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
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Doctrine — Transformative Use Dominance Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'caaf4dbb-1136-4133-810e-4580467b6fc9').
narrative_ontology:cs_kernel_codification('caaf4dbb-1136-4133-810e-4580467b6fc9', fixed_text).
narrative_ontology:cs_authority_grounding('caaf4dbb-1136-4133-810e-4580467b6fc9', lineage).
narrative_ontology:cs_interpretation_layer_present('caaf4dbb-1136-4133-810e-4580467b6fc9').
narrative_ontology:cs_reading_relation('caaf4dbb-1136-4133-810e-4580467b6fc9', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('caaf4dbb-1136-4133-810e-4580467b6fc9', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('caaf4dbb-1136-4133-810e-4580467b6fc9', foundational, new_meaning_or_message_justifies_subordinating_market_harm).
narrative_ontology:cs_axiom_status(new_meaning_or_message_justifies_subordinating_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('caaf4dbb-1136-4133-810e-4580467b6fc9', new_meaning_or_message_justifies_subordinating_market_harm, instrumental).
narrative_ontology:cs_axiom('caaf4dbb-1136-4133-810e-4580467b6fc9', foundational, transformativeness_is_the_central_four_factor_inquiry).
narrative_ontology:cs_axiom_status(transformativeness_is_the_central_four_factor_inquiry, overridden).
narrative_ontology:cs_axiom_grounding('caaf4dbb-1136-4133-810e-4580467b6fc9', transformativeness_is_the_central_four_factor_inquiry, conventional).
narrative_ontology:cs_reference_frame('caaf4dbb-1136-4133-810e-4580467b6fc9', campbell_transformativeness_primacy).
narrative_ontology:cs_drift_state('caaf4dbb-1136-4133-810e-4580467b6fc9', post_warhol_foundation_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('caaf4dbb-1136-4133-810e-4580467b6fc9', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_and_parody_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_hosting_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_and_commentary_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ai_training_data_aggregators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, midlist_photographers_and_illustrators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, music_sample_rightsholders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_market_intermediaries).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, copyright_purpose_is_promoting_progress_not_rewarding_labor).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, transformation_of_meaning_or_message_justifies_unlicensed_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build new works — parody, mashup, commentary video, appropriation art — from existing copyrighted material. Under the transformative-use reading, courts weigh whether the new work adds new expression, meaning, or message; if it does, market harm to the original is discounted heavily even where a licensing market plainly exists. This lets them proceed without clearing rights they could rarely afford to clear.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_and_parody_creators, beneficiary,
    moderate, biographical, mobile, national).

% Host billions of user uploads that recombine copyrighted footage, music, and images. The transformative-use reading lets platforms defend a large share of that content as fair use in take-down disputes and litigation, reducing licensing overhead and moderation liability. Platforms also shape which transformativeness arguments get litigated by choosing which claims to fight versus settle, giving them agenda-setting influence over how the doctrine develops in practice.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_hosting_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_hosting_platforms, agenda_setter).

% Use archival footage, news clips, and copyrighted images to build critical or historical works. The transformative-use reading is often decisive for their ability to complete projects without exhausting budgets on rights clearance, since courts ask whether the new context recontextualizes rather than substitutes for the original.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_and_commentary_producers, beneficiary,
    moderate, biographical, constrained, national).

% Train large models on scraped copyrighted text, images, and code, arguing the resulting model's outputs are non-substitutive and the training process itself transforms the works' purpose from expression to statistical pattern-extraction. This is the doctrine's most consequential and most contested current extension, decoupling transformativeness from any human authorial recontextualization.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ai_training_data_aggregators, beneficiary,
    powerful, generational, arbitrage, global).

% License individual images and illustrations as a primary income source. When a court finds a commercial reuse of their work 'transformative' because it recontextualizes purpose or meaning, the fourth factor (market harm) is subordinated even though a real licensing market for exactly that reuse existed and was foreclosed. They cannot afford to litigate against better-resourced users and have no meaningful exit from a doctrine applied ex post to their existing portfolio.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, midlist_photographers_and_illustrators, payer,
    powerless, biographical, trapped, national).

% Hold rights in recordings and compositions sampled by other artists. Under the transformative-use reading, a sample recontextualized into a new work with a different message can defeat a claim even where a clearance market for samples is well established industry practice. They bear the cost of an unlicensed transfer dressed as cultural production.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, music_sample_rightsholders, payer,
    moderate, biographical, constrained, national).

% Operate stock-image, sync-licensing, and rights-clearance businesses whose entire value proposition is pricing and administering exactly the reuses the transformative-use reading tends to excuse. Rising transformativeness findings shrink the addressable market for clearance transactions they were built to intermediate.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_market_intermediaries, payer,
    moderate, biographical, constrained, national).

% Applies and develops the four-factor test case by case, deciding how much weight transformativeness carries against market harm. Post-Campbell and pre-Warhol Foundation case law drifted toward broad transformativeness findings; the 2023 Warhol Foundation v. Goldsmith decision constrained that drift by requiring closer scrutiny of commercial purpose and licensing-market overlap, showing the doctrine's balance is actively contested and judicially revisable.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Monitor how courts apply the doctrine and periodically consider statutory clarification, particularly regarding AI training. They have not codified a transformativeness standard, leaving the reading's boundaries to accumulate through litigation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, legislators_and_copyright_office, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows new expressive and technological works to build on existing culture without requiring case-by-case licensing negotiation for every incorporation, recontextualization, or commentary — solving a real bottleneck where transaction costs of clearance would otherwise choke off transformative cultural production.
% TRANSFER_FUNCTION: Moves the economic value of secondary uses — image licensing fees, sample clearance fees, training-data compensation — from original rightsholders to downstream users and the platforms and firms that aggregate downstream use, whenever a court or platform classifies the reuse as sufficiently transformative to discount market harm.
% ABSENT_VOICES: Individual photographers, illustrators, and session musicians rarely appear as parties in the landmark cases that set transformativeness doctrine — those cases are typically litigated by well-resourced platforms, studios, or foundations against other well-resourced parties (e.g., Google Books, Warhol Foundation), leaving the doctrine's practical boundaries set without the participation of the smallest rightsholders it most affects.
% DISAPPEARANCE_RATIONALE: If the transformative-use reading were abandoned in favor of a market-harm-dominant framework, platforms and remix creators would face substantially higher licensing exposure, AI training practices reliant on the transformativeness argument would lose their principal legal defense, and licensing intermediaries would recapture a wider share of the reuse market — a legally and economically consequential rearrangement, not a return to an unaffected status quo.
% FOUNDING_PROBLEM: Rigid application of exclusive rights would prevent criticism, parody, scholarship, and news reporting from engaging with copyrighted works at all, and would impose prohibitive transaction costs on culturally valuable secondary uses that the copyright system's stated purpose — promoting progress — depends on permitting.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court in Campbell v. Acuff-Rose (1994) and again in Warhol Foundation v. Goldsmith (2023) attests the founding problem — the need to permit genuine transformation — remains live but has narrowed the transformativeness reading's dominance, expressly cautioning against letting transformativeness swallow the market-harm factor; this corroboration comes from the adjudicating body itself rather than from beneficiary platforms, and represents a partial course-correction rather than an outside confirmation that the wide reading is still warranted.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.46 rather than high, because the reading genuinely serves a coordination function (permitting culturally valuable secondary use without prohibitive transaction costs) alongside its extractive effect on rightsholders whose licensing markets are discounted. Suppression (0.38) is lower than in a typical snare because the reading operates through discretionary judicial balancing rather than blanket prohibition — individual rightsholders retain the formal right to litigate, even though the practical asymmetry in litigation resources functions as a suppressive barrier. Theater ratio rose through the 2000s-2010s as 'transformativeness' rhetoric increasingly served as a doctrinal label applied to justify outcomes reached on other grounds (a Goodhart-style drift), then partially receded after Warhol Foundation's tightening. The dip and rebound at 2023-2024 reflect the Supreme Court's partial correction followed by continued lower-court and platform pressure to re-expand the reading, particularly around AI training.
 *
 * PERSPECTIVAL GAP:
 *   From the UGC platform and remix-creator seats, this reading looks like a rope: it solves a genuine coordination problem (letting culture build on culture) with the courts as neutral arbiters. From the midlist photographer or session musician seat, the identical doctrine looks like a tangled rope shading toward snare: a court-sanctioned mechanism for taking their licensable work without compensation, dressed in the vocabulary of cultural progress. The engine computes these divergent seat classifications from the same structural data — that divergence is the point of the classification, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (remix creators, UGC platforms, documentarians, AI aggregators) sit near the low-d end: the reading subsidizes their activity by discounting the market-harm factor that would otherwise require licensing. Victims (individual photographers/illustrators, sample rightsholders, licensing intermediaries) sit near the high-d end: they bear a transfer that the transformativeness finding renders uncompensated, and their exit options are trapped or constrained because litigation costs and portfolio lock-in prevent meaningful avoidance. The federal judiciary is an agenda-setter with analytical exit — it is not extracting rents itself but is the mechanism through which the extraction is authorized or checked case by case, which is why Warhol Foundation could partially reverse the trajectory without abolishing the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permitting genuine transformation without prohibitive transaction costs) remains partly live — commentary, parody, and criticism plainly still need the doctrine — but the AI-training extension represents a use of the same 'transformativeness' vocabulary for a purpose (mass extraction of training value from copyrighted corpora) that bears little resemblance to the recontextualization the doctrine was built around. This is not full mandatrophy (the founding problem is not fully dead), but it is a live site of doctrinal capture where the same label is being stretched to cover a structurally different transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_threshold_ambiguity,
    'Where is the line between ''adds new meaning or message'' (transformative) and ''merely repackages for a different market'' (derivative, requiring license) — and who gets to draw it in practice?',
    'Track post-Warhol Foundation circuit court applications to see whether the tightened standard produces a stable, predictable line or continues to vary by judge and forum; a stable line would reduce the reading''s current unpredictability-driven extraction.',
    'A narrow, predictable threshold would shrink this reading''s beneficiary set toward genuine recontextualization cases and reduce extraction from rightsholders; a wide, unpredictable threshold preserves the current moderate-to-high extraction by letting well-resourced users litigate favorable findings case by case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_threshold_ambiguity, empirical, 'Whether post-Warhol transformativeness doctrine stabilizes into a predictable line or remains forum-dependent.').

omega_variable(
    ai_training_transformation_ontology,
    'Is training a statistical model on copyrighted works structurally analogous to the human recontextualization (parody, commentary, criticism) the transformative-use reading was built around, or is it a categorically different act that has adopted the same vocabulary for different structural reasons?',
    'Pending AI-training fair use litigation (e.g., various generative-AI copyright suits) will produce appellate rulings on whether ''transformative purpose'' in the training-data sense satisfies the same doctrinal test as transformative expression; a circuit split or Supreme Court ruling would resolve this.',
    'If courts find AI training is NOT transformative in the doctrinally relevant sense, this reading''s beneficiary set shrinks sharply (ai_training_data_aggregators exit the beneficiary list) and the reading''s extractiveness score would fall; if courts extend the reading to cover it, extraction concentrates further on rightsholders whose works are used as training inputs at scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_transformation_ontology, conceptual, 'Whether machine-training transformation is the same structural claim as human-authorial transformation, or a distinct extension riding on the same label.').

omega_variable(
    reading_kernel_disagreement_location,
    'Where exactly do the transformative-use, creator-centric, and user-centric readings diverge — is it in how each weighs the four factors relative to each other, or in a deeper disagreement about whether fair use is an exception to a property right versus an affirmative right in its own terms?',
    'Doctrinal and jurisprudential analysis comparing how each reading''s proponents characterize the underlying legal nature of fair use (exception vs. right) versus how they weight the four statutory factors — these may be independent axes of disagreement rather than a single spectrum.',
    'If the disagreement is purely about factor-weighting, the readings could in principle converge through incremental case law; if it is about the underlying legal characterization (exception vs. right), the readings are foreclosing rather than merely weighting differently, which would change the reading_relations declared here from coexists_with toward something closer to contested foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_disagreement_location, conceptual, 'Whether the three kernel readings diverge on factor-weighting alone or on the deeper legal characterization of fair use.').


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
narrative_ontology:measurement(fair_tr_t2018, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(fair_tr_t2023, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.28).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(fair_be_t2006, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2006, 0.38).
narrative_ontology:measurement(fair_be_t2012, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement(fair_be_t2018, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement(fair_be_t2023, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2023, 0.41).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.2).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(fair_su_t2006, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2006, 0.29).
narrative_ontology:measurement(fair_su_t2012, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2012, 0.34).
narrative_ontology:measurement(fair_su_t2018, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(fair_su_t2023, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2023, 0.36).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, user_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fair_use_four_factor_test kernel. creator_centric_reading treats fair use as a narrow property-right exception with market-harm dominant; user_centric_reading treats fair use as an affirmative public right with transformativeness as a floor rather than a balancing factor; this reading (transformative_use_reading) treats transformativeness as the dominant balancing lever within an exception framework. Each reading carries a distinct ε, beneficiary set, and victim set per the ε-invariance principle — they are not measurement perspectives on one constraint but three structurally distinct constraints sharing a contested textual/doctrinal kernel (17 U.S.C. § 107).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
