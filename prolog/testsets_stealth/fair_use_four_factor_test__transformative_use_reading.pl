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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Transformative-Use Dominance in Fair Use Balancing
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   Since Campbell v. Acuff-Rose (1994), the question 'does the use add new
 *   meaning or purpose?' has organized American fair-use adjudication: courts
 *   locate transformativeness first, and a strong finding pulls the remaining
 *   §107 factors — including market harm — toward tolerance. The arrangement
 *   is administered entirely through litigation: no agency sets rates, no
 *   statute defines the threshold, and the boundary moves opinion by opinion.
 *   It operates as a genuine coordination device — an escape valve that keeps
 *   copyright from choking criticism, scholarship, indexing, and remix —
 *   while simultaneously transferring control over expressive raw material
 *   from rights holders to secondary users and the platforms that host them,
 *   uncompensated, wherever a judge finds new meaning. claimed_type is
 *   authored from this dual structure; the metric scores are authored
 *   independently as descriptive measurements of the doctrine's actual
 *   operation, and the engine computes each seat's classification from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda setter (institutional/constrained) — administers §107 case by case; the Supreme Court and Second Circuit set the transformativeness standard; collects no revenue but its doctrinal craft and prestige ride on the test's administrability
 *   - ugc_platforms: primary beneficiary (powerful/arbitrage) — hosts user uploads incorporating copyrighted works without advance clearance; monetizes the resulting engagement at global scale
 *   - search_engine_operators: secondary beneficiary (powerful/arbitrage) — indexes, thumbnails, and surfaces copyrighted works; transformative-use findings excuse reproduction that licensing could never cover
 *   - remix_artists: beneficiary (powerless/constrained) — mashup, video-essay, and fan-work creators who gain expressive outlet without clearance costs; dependent on platform terms and unable to self-fund defense
 *   - documentary_filmmakers: beneficiary (moderate/constrained) — incorporate archival footage under fair-use best-practice codes; residual insurance and clearance friction
 *   - music_publishers: primary payer (organized/constrained) — control sync and master licenses; lose licensing revenue when samples are found transformative; respond through litigation and lobbying
 *   - individual_visual_artists: payer (powerless/trapped) — photographers and illustrators whose images circulate and are reused without license; enforcement costs exceed likely recovery
 *   - stock_photo_agencies: payer (organized/constrained) — licensing businesses whose editorial demand erodes under transformative-use findings
 *   - unrepresented_small_creators: excluded (powerless/trapped) — creators shaped by doctrine made in litigation between well-resourced parties; their compensated-licensing preference has no seat
 *   - ip_legal_scholarship: analytical observer (analytical/analytical) — maps the doctrine's drift from outside; no material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.55).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.48).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Transformative-Use Dominance in Fair Use Balancing").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'd30878e9-8738-4ed3-93a1-211ac34b9116').
narrative_ontology:cs_kernel_codification('d30878e9-8738-4ed3-93a1-211ac34b9116', fixed_text).
narrative_ontology:cs_authority_grounding('d30878e9-8738-4ed3-93a1-211ac34b9116', lineage).
narrative_ontology:cs_interpretation_layer_present('d30878e9-8738-4ed3-93a1-211ac34b9116').
narrative_ontology:cs_reading_relation('d30878e9-8738-4ed3-93a1-211ac34b9116', fair_use_four_factor_test__creator_centric_reading, influences).
narrative_ontology:cs_reading_relation('d30878e9-8738-4ed3-93a1-211ac34b9116', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('d30878e9-8738-4ed3-93a1-211ac34b9116', foundational, expressive_addition_subordinates_market_harm).
narrative_ontology:cs_axiom_status(expressive_addition_subordinates_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('d30878e9-8738-4ed3-93a1-211ac34b9116', expressive_addition_subordinates_market_harm, instrumental).
narrative_ontology:cs_axiom('d30878e9-8738-4ed3-93a1-211ac34b9116', foundational, transformation_finding_gates_factor_weighing).
narrative_ontology:cs_axiom_status(transformation_finding_gates_factor_weighing, holdable).
narrative_ontology:cs_axiom_grounding('d30878e9-8738-4ed3-93a1-211ac34b9116', transformation_finding_gates_factor_weighing, conventional).
narrative_ontology:cs_reference_frame('d30878e9-8738-4ed3-93a1-211ac34b9116', transformativeness_primacy_balancing).
narrative_ontology:cs_drift_state('d30878e9-8738-4ed3-93a1-211ac34b9116', post_warhol_2023, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d30878e9-8738-4ed3-93a1-211ac34b9116', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, search_engine_operators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_artists).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, music_publishers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, individual_visual_artists).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, stock_photo_agencies).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, leval_transformative_purpose_test).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, first_amendment_safety_valve_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides fair-use questions case by case under 17 U.S.C. §107. The Supreme Court and the Second Circuit supply the working definition of transformativeness that lower courts apply. The bench collects no money from the doctrine's operation, but its institutional role — managing the boundary between property and expression — depends on the test staying administrable. Changing the standard requires new precedent, which arrives slowly and only through litigated cases.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Host billions of user uploads that quote, sample, or remix copyrighted works. Fair-use defenses, layered on notice-and-takedown safe harbors, let them host first and filter later instead of clearing rights in advance. Revenue flows from advertising and subscriptions on the resulting engagement. They can relocate infrastructure, restructure products, negotiate selective blanket licenses, and fund landmark test litigation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, beneficiary,
    powerful, generational, arbitrage, global).

% Copy, index, thumbnail, and excerpt copyrighted works at query time. Judicial findings that indexing and thumbnailing serve a different purpose than the originals removed the licensing bottleneck that would otherwise make search-scale reproduction impossible. Operations span jurisdictions, giving wide latitude to structure around adverse rulings.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, search_engine_operators, beneficiary,
    powerful, generational, arbitrage, global).

% Make mashups, video essays, reaction formats, and fan works built on existing recordings and footage. Unlicensed use is the medium: clearance for every fragment would be unaffordable. Their protection is defensive — a doctrine they invoke when challenged, not one they can enforce. They depend on platform upload rules and cannot fund sustained litigation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists, beneficiary,
    powerless, biographical, constrained, global).

% Incorporate archival news footage, home video, and popular culture into nonfiction film. Best-practice codes built on transformative-use reasoning let insurers and broadcasters accept uncleared clips that serve commentary. Residual clearance and errors-and-omissions friction remains, but the doctrine widened what can be made at independent budgets.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Own composition catalogs and administer synchronization licenses for recorded music. When courts find a sample transformative, the sync-licensing fee the publisher would have charged goes unpaid and the use proceeds anyway. They contest unfavorable applications through litigation and legislative lobbying, and adapt commercially through direct-licensing deals and takedown enforcement. Leaving the jurisdiction is not an option; the catalog is the business.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, music_publishers, payer,
    organized, generational, constrained, global).

% Photographers, illustrators, and painters whose images spread online and reappear in collages, editorial layouts, and training corpora without license. Recent precedent strengthened their argument that a reuse harms the specific licensing market for their work, but bringing a claim costs more than most recoveries return. There is no practical way to keep their work out of circulation while remaining working artists.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, individual_visual_artists, payer,
    powerless, biographical, trapped, global).

% Operate licensing businesses whose inventory is photographic and illustrative work. Editorial and commentary reuses found transformative reduce demand for paid licenses in exactly the segments the agencies price highest. They respond with joint litigation, pricing adjustments, and metadata enforcement; the underlying circulation of images is not something they can exit.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, stock_photo_agencies, payer,
    organized, biographical, constrained, global).

% Creators on both sides of the line — small rights holders whose work gets reused, and small makers who fear suits — whose interests are set by precedents produced in appeals between platforms, labels, estates, and foundations. Their preferred resolution, a compensated statutory licensing scheme, appears nowhere in the docketed conversation. They neither litigate nor lobby at scale.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, unrepresented_small_creators, excluded,
    powerless, biographical, trapped, global).

% Academic lawyers and economists who trace the doctrine's movement, quantify its effects on licensing markets and cultural production, and argue its direction from both sides. No material stake in outcomes; influence runs through amicus briefs, treatises, and the clerks and judges who read them.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ip_legal_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the standing conflict between copyright exclusivity and free expression by giving courts an administrable first question — whether the use adds new meaning or purpose — so that criticism, parody, scholarship, indexing, and remix can proceed without case-by-case legislative authorization or universal licensing negotiation.
% TRANSFER_FUNCTION: Moves control over, and potential licensing revenue from, copyrighted works from rights holders to secondary users and the platforms hosting them, whenever a court finds the use transformative — an uncompensated transfer of expressive raw material whose incidence tracks where the transformation threshold sits.
% ABSENT_VOICES: Small creators and non-litigating rights holders: the doctrine is made in appeals between well-resourced parties, so the people whose everyday conduct the standard governs have no seat in the cases that set it. Licensing intermediaries whose markets are displaced are likewise absent from the proceedings that displace them, and advocates of a compensated compulsory-licensing scheme stand entirely outside the judicial frame.
% DISAPPEARANCE_RATIONALE: If transformative-use dominance vanished overnight, fair use would collapse toward a narrowly and evenly weighed exception: platforms would need licenses for user-generated content at scale or retreat to takedown-only models, remix and video-essay production would shrink or move behind paywalls, sync-licensing and stock-licensing demand would partially revive, and the litigation economy around §107 would reorganize around proposals for statutory licensing.
% FOUNDING_PROBLEM: The 1976 Copyright Act codified fair use as an open-ended four-factor test to preserve criticism and commentary that rigid property rules would chill. By the early 1990s lower courts were applying the factors unpredictably, and Campbell v. Acuff-Rose (1994) elevated transformativeness to give judges a manageable first question and to protect parody from categorical market-harm presumptions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Supreme Court's Warhol majority (2023) expressly recalibrated transformativeness against the reading that a finding of added meaning settles the inquiry, and Judge Pierre Leval — the doctrine's own author — has warned against its mechanical application. Critical scholarship from copyright academics unaffiliated with platform or remix interests attests the mandate has outrun its origin. No source outside the dispute attests that the founding problem is fully dead, and none attests that the solution still fits it; the status is genuinely contested.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (ε = 0.55) is moderate and threshold-dependent: uncompensated appropriation is real but conditional on a judicial finding, and the 2023 Warhol decision restored market-specific analysis, pulling ε back from its 2021 peak. Suppression (0.48) aggregates two opposite chills — rights holders deterred from enforcing, secondary creators deterred by litigation risk — and is carried structurally by the fact that no one can opt out of someone else's fair-use defense. Theater (0.50) reflects Goodhart drift: the four-factor balancing is increasingly announced after the transformativeness finding has done the decisive work, though Warhol partially restored genuine multi-factor reasoning. Accessibility collapse is low-moderate (0.40) because licensing, permission, and original creation remain available alternatives; resistance is substantial (0.62), funded by organized content industries across three decades of litigation. All temporal series share one seven-point grid (1990–2023); the trajectory rises through the Google Books and Oracle era and partially recedes post-Warhol — a non-monotonic drift, not a cycle. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the platform and remixer seats the doctrine is breathing room — the difference between building at all and negotiating every fragment. From the individual-artist and publisher seats the same doctrine is uncompensated taking dressed in expressive language, with the burden of proof and the cost of appeal falling on them. The judiciary experiences neither: it holds the test as craft, and its stake is administrative manageability rather than outcome. The engine derives these divergences from the declared roles, power levels, and exit options; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (ugc_platforms, search_engine_operators, remix_artists, documentary_filmmakers) derive low directionality — the doctrine subsidizes them, amplified for the platforms by arbitrage-grade exit. Declared victims (music_publishers, individual_visual_artists, stock_photo_agencies) derive high directionality, with trapped individual artists sitting nearer the full-target end than organized publishers, who can litigate, lobby, and adapt their business models. The federal judiciary is neither beneficiary nor victim: it administers the arrangement and accrues craft prestige from its centrality, but collects no rents and bears legitimacy costs when the doctrine overreaches. The structural derivation cannot express that position, so an explicit override sets the institutional seat to d = 0.42 — slightly beneficiary-side of symmetric. Scholarship is analytical and feeds no directional arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination would erase the uncompensated transfer that individual artists and publishers bear; reading it as pure taking would erase the escape valve that keeps criticism, parody, and indexing lawful without legislative action. The tangled-rope classification holds both facts: a live coordination function (the founding problem — uncertainty chilling commentary — is not dead) and asymmetric extraction riding the same structure, held in place by active judicial enforcement. It is not a piton: the function is performed, not merely performed-at, even though roughly half of the balancing activity is now theater. The mandate has not plainly outlived its function, so no mandatrophy resolution is declared; the contested status is carried by the R5 fields and the threshold omega instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'This constraint is the transformative_use_reading of the fair_use_four_factor_test kernel; which reading governs is itself the live contest — what would instantiate differently under the creator_centric_reading or the user_centric_reading?',
    'Track Supreme Court and circuit weighting of the four factors over successive terms: sustained elevation of market-specific harm evidence signals migration toward the creator-centric reading; explicit rights-framing of fair use signals the user-centric reading.',
    'Under the creator-centric reading, extraction rises for appropriative uses and the victim set stabilizes around licensing-market displacement; under the user-centric reading, extraction falls and the beneficiary set expands to downstream culture generally. The metrics in this file are valid only for the transformative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'Committer structure: this story is one of three readings of the §107 kernel; sibling readings are separate constraints with different ε and different victim sets.').

omega_variable(
    transformation_threshold_location,
    'Where does ''adds new meaning or purpose'' sit — how much expressive addition suffices to subordinate market harm?',
    'Comparative coding of granted and denied fair-use motions across circuits before and after Warhol, scoring degree of expressive addition against outcome and against the market-specific analysis Warhol requires.',
    'A high threshold shrinks the victim set toward verbatim-substitution cases and lowers ε; a low threshold makes individual_visual_artists systematic victims and pushes ε toward the range where the coordination story reads as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_threshold_location, conceptual, 'Threshold indeterminacy is the mechanism behind the shifting victim set declared in the expected structural delta.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression borne by rights holders (deterred enforcement, uncapturable reuse) or by secondary creators (deterred production under litigation risk) — and in what proportion?',
    'Litigation-rate and registration studies bracketing Warhol, plus survey data on projects abandoned over clearance risk, disaggregated by seat.',
    'If suppression is mostly rights-holder-side, the per-seat asymmetry hardens and the tangled-rope reading strengthens; if it is mostly secondary-creator-side, the coordination function is weaker than authored and a rope classification becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'The aggregate suppression scalar conflates two opposite chills acting on different seats.').

omega_variable(
    platform_capture_drift,
    'Does the doctrine''s application systematically favor repeat-player platform litigants over individual creators on both sides — has platform-scale litigation leverage captured the transformativeness inquiry?',
    'Outcome comparison of fair-use rulings in platform-defendant cases versus individual-defendant cases, controlling for use type and circuit.',
    'Confirmed capture would shift effective extraction onto unrepresented_small_creators and individual_visual_artists, trending the constraint away from hybrid coordination toward a structure whose coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_drift, empirical, 'Repeat-player asymmetry in how the doctrine develops and is applied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1990, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement_basis(fair_tr_t1990, observed).
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement_basis(fair_tr_t1994, observed).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement_basis(fair_tr_t2000, observed).
narrative_ontology:measurement(fair_tr_t2008, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2008, 0.45).
narrative_ontology:measurement_basis(fair_tr_t2008, observed).
narrative_ontology:measurement(fair_tr_t2015, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2015, 0.58).
narrative_ontology:measurement_basis(fair_tr_t2015, observed).
narrative_ontology:measurement(fair_tr_t2021, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2021, 0.62).
narrative_ontology:measurement_basis(fair_tr_t2021, observed).
narrative_ontology:measurement(fair_tr_t2023, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2023, 0.5).
narrative_ontology:measurement_basis(fair_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement_basis(fair_be_t1990, observed).
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.38).
narrative_ontology:measurement_basis(fair_be_t1994, observed).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(fair_be_t2000, observed).
narrative_ontology:measurement(fair_be_t2008, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement_basis(fair_be_t2008, observed).
narrative_ontology:measurement(fair_be_t2015, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement_basis(fair_be_t2015, observed).
narrative_ontology:measurement(fair_be_t2021, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement_basis(fair_be_t2021, observed).
narrative_ontology:measurement(fair_be_t2023, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement_basis(fair_be_t2023, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__transformative_use_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, user_centric_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'fair use' decomposes, per the ε-invariance principle, into three structurally distinct constraints — competing readings of the §107 kernel. This file authors the transformative_use_reading only. Its ε (0.55) is moderate and threshold-dependent; the creator_centric_reading would author higher ε for appropriative uses with a stabilized victim set anchored in licensing-market displacement; the user_centric_reading would author lower ε with an expanded beneficiary set covering downstream culture generally. The statutory text is upstream of all three readings; each reading is a separate story, linked here through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
