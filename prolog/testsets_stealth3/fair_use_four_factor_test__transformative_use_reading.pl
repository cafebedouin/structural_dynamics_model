% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Transformative-Use Dominance in Fair Use Adjudication
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the fair-use kernel: the
 *   transformative-use reading, under which a use that adds new meaning,
 *   message, or purpose dominates the four statutory factors, and evidence of
 *   licensing-market harm is subordinated whenever such new meaning is found.
 *   The standing arrangement under contest is the actual operation of that
 *   doctrine in US adjudication from the Leval article (interval origin)
 *   through the post-Warhol recalibration (interval end). The claim and the
 *   metrics are independent authored facts: the reading is CLAIMED as
 *   tangled_rope — a genuine coordination function (an escape valve for
 *   unpriceable expressive reuse) joined to asymmetric incidence
 *   (uncompensated appropriation concentrated on long-tail rights holders,
 *   monetized gains concentrating at platforms) held in place by active
 *   case-by-case enforcement — while the metrics describe moderately
 *   extractive, moderately suppressive, increasingly theatrical operation.
 *   Sibling readings (creator_centric_reading, user_centric_reading) are
 *   separate constraints with their own epsilon and beneficiary structures;
 *   nothing here averages across them.
 *
 * KEY AGENTS:
 *   - federal_courts: agenda-setting seat (institutional/constrained) — defines the transformation threshold case by case; allocates who may reuse and who must sue
 *   - ugc_platforms: primary beneficiary with agenda-setting reach (institutional/arbitrage) — hosts and monetizes reuse, funds doctrine-shaping litigation
 *   - remix_creators: beneficiary (moderate/mobile) — practice converted from infringement risk into protected expression
 *   - documentary_filmmakers: organized beneficiary (organized/constrained) — archival reuse underwritten by recognized fair-use practices
 *   - independent_photographers_illustrators: primary target (powerless/trapped) — absorb uncompensated appropriation with no realistic per-instance remedy
 *   - stock_photo_agencies: target (organized/constrained) — licensing revenue eroded by uses treated as transformative
 *   - music_publishers: powerful target (powerful/constrained) — litigate and lobby against expansive readings
 *   - foreign_creators_under_platform_policy: excluded seat (powerless/trapped) — governed by US-derived norms without a voice in them
 *   - copyright_law_scholars: analytical observer (analytical/analytical) — maps drift, designs alternatives, binds no one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.58).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.54).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Transformative-Use Dominance in Fair Use Adjudication").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '383821e2-ad3d-496f-9a75-839e5cb3bcea').
narrative_ontology:cs_kernel_codification('383821e2-ad3d-496f-9a75-839e5cb3bcea', formalized).
narrative_ontology:cs_authority_grounding('383821e2-ad3d-496f-9a75-839e5cb3bcea', lineage).
narrative_ontology:cs_interpretation_layer_present('383821e2-ad3d-496f-9a75-839e5cb3bcea').
narrative_ontology:cs_reading_relation('383821e2-ad3d-496f-9a75-839e5cb3bcea', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('383821e2-ad3d-496f-9a75-839e5cb3bcea', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('383821e2-ad3d-496f-9a75-839e5cb3bcea', foundational, transformativeness_dominates_four_factors).
narrative_ontology:cs_axiom_status(transformativeness_dominates_four_factors, holdable).
narrative_ontology:cs_axiom_grounding('383821e2-ad3d-496f-9a75-839e5cb3bcea', transformativeness_dominates_four_factors, instrumental).
narrative_ontology:cs_axiom('383821e2-ad3d-496f-9a75-839e5cb3bcea', secondary, market_harm_subordinated_when_new_meaning_added).
narrative_ontology:cs_axiom_status(market_harm_subordinated_when_new_meaning_added, holdable).
narrative_ontology:cs_axiom_grounding('383821e2-ad3d-496f-9a75-839e5cb3bcea', market_harm_subordinated_when_new_meaning_added, instrumental).
narrative_ontology:cs_reference_frame('383821e2-ad3d-496f-9a75-839e5cb3bcea', transformation_centered_balancing).
narrative_ontology:cs_drift_state('383821e2-ad3d-496f-9a75-839e5cb3bcea', post_warhol_recalibration, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('383821e2-ad3d-496f-9a75-839e5cb3bcea', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, independent_photographers_illustrators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, stock_photo_agencies).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, music_publishers).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, leval_transformative_standard).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, campbell_transformativeness_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Section 107 case by case, deciding which uses count as adding new meaning or purpose and how much weight market-harm evidence carries. Their opinions define the operative threshold that determines who may reuse without a license and who must sue to stop a use. Bound by precedent, appellate review, and the possibility of congressional codification; they collect nothing and pay nothing directly, but they allocate who does.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Host billions of user uploads that incorporate copyrighted material and monetize them through advertising. Rely on transformative-use defenses, alongside notice-and-takedown safe harbors, to operate without licensing the underlying catalog. Fund landmark litigation that shapes the doctrine, publish transparency reports framing enforcement choices, and can shift terms of service or corporate domicile across jurisdictions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, agenda_setter).

% Sample, mash up, and recontextualize existing audiovisual and musical works. The doctrine converts what would otherwise be actionable copying into protected expression, and their audience and distribution run through platforms whose policies assume that protection. Making non-appropriative work remains possible, but their established practice and reputation are built on reuse.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_creators, beneficiary,
    moderate, biographical, mobile, global).

% Quote archival footage, photographs, and music under published fair-use best practices, with errors-and-omissions insurers accepting documented fair-use determinations in place of licenses. Archival licensing at scale would price most historical and critical documentary out of existence; their trade associations codified the practices that courts now recognize.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers, beneficiary,
    organized, biographical, constrained, national).

% License commercial imagery and find their catalogs reproduced in editorial, blog, and machine-learning contexts that courts or platforms treat as transformative. Recovery requires per-instance litigation or takedown demands; watermarking and fingerprinting mitigate but do not prevent uncompensated reproduction. Their licensing revenue declines as reuse expands faster than enforcement capacity.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, stock_photo_agencies, payer,
    organized, biographical, constrained, global).

% Individual image-makers whose work circulates online and is reused in compilations, commentary, and training corpora found transformative. Litigating a single instance costs more than the typical license fee, so uncompensated use is absorbed silently. Works already in circulation cannot be recalled, and mid-career switches to non-reproducible income are limited; collective organizing efforts are recent and thin.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, independent_photographers_illustrators, payer,
    powerless, biographical, trapped, global).

% Control composition and sound-recording rights and confront sampling and synchronization uses that defendants frame as transformative commentary. They litigate aggressively, secured historical carve-outs treating sound recordings differently from other works, and fund legislative and scholarly campaigns for a narrower doctrine. Their leverage is real but bounded by the platform economy's dependence on user uploads.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, music_publishers, payer,
    powerful, biographical, constrained, global).

% Creators outside the United States whose works move through platforms that apply US-derived fair-use norms worldwide regardless of local law. Their national systems may allow only narrow fair dealing or none, yet platform-level enforcement decisions are made under American doctrine. They hold no seat in US adjudication and no practical ability to opt their catalogs out of the arrangement.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, foreign_creators_under_platform_policy, excluded,
    powerless, biographical, trapped, global).

% Map doctrinal movement across case cohorts, publish empirical studies of litigation outcomes and licensing-market effects, and design alternatives such as extended collective licensing and registration-based clearances. They take no side in particular disputes and their proposals rarely bind anyone, but their analyses supply the evidentiary record other seats argue over.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_law_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the transaction-cost deadlock of expressive reuse: requiring a negotiated license for every quotation, parody, documentary reference, or search index would make much criticism and commentary impossible to produce at any price. The doctrine supplies a judicially administered escape valve so that uses serving comment, criticism, teaching, and research can proceed without prior permission.
% TRANSFER_FUNCTION: Moves uncompensated use-value of existing works from rights holders to second-comers and to the platforms hosting the resulting output, and moves enforcement burden onto rights holders, who must sue instance by instance to stop uses they regard as harmful.
% ABSENT_VOICES: Rights holders without litigation capacity, foreign creators governed by platform policies derived from US doctrine, and designers of collective-licensing alternatives are all outside the adversarial process that produces the case law. The apparent stability of the doctrine reflects who can afford to litigate, not agreement among everyone affected.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, documentary production, parody, search indexing, and the user-upload economies of major platforms would face mass liability; licensing demand and clearance costs would surge; platforms would restructure around licensed catalogs or heavy filtering; and a large body of existing practice would become infringing retroactively.
% FOUNDING_PROBLEM: Reconcile statutory exclusive rights with free-expression values and with categories of reuse — criticism, comment, news reporting, teaching, scholarship — whose value cannot be captured by a licensing price set before the reuse exists.
% FOUNDING_PROBLEM_CORROBORATION: Supreme Court opinions across the spectrum attest the expressive safety-valve rationale, while the Warhol majority's renewed attention to licensing markets and the Copyright Office's recent proceedings attest that the parties dispute whether transformativeness dominance still serves the founding problem. Industry-funded economic studies and independent scholarship corroborate opposing assessments; no attester outside the disputing camps certifies the status as settled in either direction.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.58 at interval end) because the doctrine transfers use-value without compensation in a substantial subset of cases while simultaneously delivering a real coordination service; incidence is sharply uneven, falling hardest on rights holders least able to litigate. Suppression (0.54) is structural rather than penal: rights holders cannot exclude uses once courts deem them transformative and must pursue remedies instance by instance, while would-be reusers face default liability wherever the doctrine does not reach. Theater ratio (0.46) is the fastest-moving signal: courts routinely recite all four factors while factor one effectively decides, and the post-Warhol period adds performative re-balancing — the ritual of weighing persists as the substance narrows. Accessibility collapse (0.60) reflects that once the doctrine is understood, negotiated licensing for critical reuse is largely foreclosed as unnecessary or unavailable, though collective-licensing proposals survive as unrealized alternatives. Resistance (0.62) is sustained: industry lobbying, adverse funded scholarship, and decade-scale litigation campaigns. The temporal series share one grid; the late-interval dip in extractiveness tracks the Warhol-era re-weighting of market harm, while theater continues rising — the balancing ritual outliving the balance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setting seats compute differently from the same doctrine. From the independent photographer's position the arrangement operates as uncompensated taking with no exit and no remedy; from the platform's position it is the load-bearing wall of a content economy; from the bench it is pragmatic line-drawing between expression and incentive; from the scholarly seat it is measurable drift. The engine computes these divergent classifications from the structural data — power, exit, and declared position — and the divergence itself is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (remix_creators, ugc_platforms, documentary_filmmakers) derive low directionality — the arrangement subsidizes them — with platforms pushed furthest toward the beneficiary end by arbitrage-grade exit. Declared victims (independent_photographers_illustrators, stock_photo_agencies, music_publishers) derive high directionality, amplified for the powerless-and-trapped photographers and damped somewhat for powerful publishers whose litigation capacity recovers part of the transfer. Federal courts sit outside the beneficiary/victim declarations; their directionality falls to the canonical fallback, which is appropriate since they allocate rather than collect. No directionality overrides are authored: overrides key on power atoms, and 'institutional' spans both courts and platforms, whose structural relationships are opposed — differentiating them through overrides would misattribute the platform's position to the bench. The beneficiary/victim declarations carry the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the doctrine as pure extraction (the creator-centric temptation) misses the genuine coordination function: without the escape valve, criticism and documentary work die in clearance costs, and the founding problem — reconciling exclusivity with unpriceable expressive reuse — remains live. Reading it as pure coordination (the reading's own self-description) misses the asymmetric incidence: the long-tail rights holders who bear the transfer are precisely those least positioned to object, and the monetized gains pool at seats with arbitrage exit. The founding-problem interview records status 'contested' rather than 'dead', so the dead-plus-world_rearranges mismatch flag should not fire; the rising theater series is the symptom to watch, since a four-factor ritual performed while one factor decides is the classic signature of a mandate maintained after its function has migrated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story is one reading of the fair_use_four_factor_test kernel; how would the structural picture change under the sibling readings, and what does this file''s epsilon fail to capture about them?',
    'Author creator_centric_reading and user_centric_reading as separate constraint stories with their own beneficiary/victim structures and epsilon values; compare computed classifications across the family.',
    'Under the creator-centric reading the same case law presents a different victim set (appropriating users rather than rights holders) and a different epsilon; under the user-centric reading the doctrine reads as under-inclusive rather than over-reaching. Cross-reading averaging would corrupt every classification in the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: one-of-three readings; siblings are distinct constraints, not hedges on this one.').

omega_variable(
    transformation_threshold_location,
    'Where exactly do courts draw the line at which added meaning flips market-harm evidence from controlling to subordinate?',
    'Outcome coding of the case-law corpus across media types and use contexts, tracking which fact patterns receive the subordination treatment.',
    'A low threshold widens the victim set (more rights holders absorbed uncompensated) and raises effective extraction; a high threshold narrows both. The victim set is threshold-dependent, so this omega bounds the whole classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_threshold_location, empirical, 'The operative threshold determining who pays is unstable and case-contingent.').

omega_variable(
    platform_capture_degree,
    'Has platform-funded litigation moved the doctrine''s center beyond where unassisted adjudication would have placed it?',
    'Compare outcomes and doctrinal language in platform-funded landmark cases against contemporaneous cases litigated by unrepresented or thinly resourced parties.',
    'Substantial capture would justify raising the platform seat''s directionality above what its beneficiary declaration alone derives, and would support reading the doctrine''s trajectory as steered rather than emergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_degree, empirical, 'Degree to which the agenda-setting seat is co-held by the largest beneficiary.').

omega_variable(
    warhol_recalibration_stability,
    'Does the post-Warhol re-weighting of licensing-market harm stabilize the transformative reading, or begin an oscillation between expansive and contractive phases?',
    'Track lower-court application from 2024 forward: whether market-harm evidence regains decisive weight only in visual-art cases or across the doctrine.',
    'Stabilization supports the authored flat-to-declining extraction tail; oscillation would indicate an intermittent dynamic in which each swing itself redistributes value between seats, warranting cyclical measurement treatment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warhol_recalibration_stability, empirical, 'Whether the late-interval dip is a new equilibrium or a phase boundary.').

omega_variable(
    licensing_alternative_viability,
    'Could collective or micro-licensing infrastructure actually deliver the coordination the doctrine currently delivers, at tolerable cost?',
    'Performance data from functioning collective-licensing markets (music mechanicals, reprographic rights organizations) extrapolated to visual and audiovisual reuse contexts.',
    'If viable alternatives exist, part of the measured extraction is avoidable rent rather than necessary coordination cost, pushing the classification toward the extractive end; if not, more of the transfer is the irreducible price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_alternative_viability, conceptual, 'Whether the doctrine''s coordination function has a workable replacement, bounding how much of the transfer is dispensable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t5, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(fair_tr_t5, observed).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(fair_tr_t10, observed).
narrative_ontology:measurement(fair_tr_t15, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(fair_tr_t15, observed).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(fair_tr_t20, observed).
narrative_ontology:measurement(fair_tr_t25, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(fair_tr_t25, observed).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(fair_tr_t30, observed).
narrative_ontology:measurement(fair_tr_t34, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 34, 0.46).
narrative_ontology:measurement_basis(fair_tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t5, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(fair_be_t5, observed).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(fair_be_t10, observed).
narrative_ontology:measurement(fair_be_t15, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(fair_be_t15, observed).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(fair_be_t20, observed).
narrative_ontology:measurement(fair_be_t25, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(fair_be_t25, observed).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(fair_be_t30, observed).
narrative_ontology:measurement(fair_be_t34, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 34, 0.58).
narrative_ontology:measurement_basis(fair_be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t5, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(fair_su_t5, observed).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(fair_su_t10, observed).
narrative_ontology:measurement(fair_su_t15, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(fair_su_t15, observed).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(fair_su_t20, observed).
narrative_ontology:measurement(fair_su_t25, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(fair_su_t25, observed).
narrative_ontology:measurement(fair_su_t30, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(fair_su_t30, observed).
narrative_ontology:measurement(fair_su_t34, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 34, 0.54).
narrative_ontology:measurement_basis(fair_su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, dmca_safe_harbor_regime).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'fair use'. The single label conflates three structurally distinct arrangements — the creator-centric, transformative-use, and user-centric readings of the same statutory kernel — each with its own epsilon, beneficiary structure, and failure modes. This file authors the transformative-use reading only. Family links run through network.affects_constraints in all members; the upstream reading with greater institutional entrenchment exerts legitimacy pressure on the siblings without resolving the contest. The transformative reading additionally couples to the DMCA safe-harbor regime, whose platform-side operation presupposes fair-use defenses for user uploads.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
