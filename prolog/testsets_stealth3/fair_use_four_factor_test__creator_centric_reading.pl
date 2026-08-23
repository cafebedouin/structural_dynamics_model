% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use as Narrow Exception to Property Right — Creator-Centric Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This file instantiates the creator-centric reading of the contested
 *   fair-use kernel: fair use as a narrow exception carved from an
 *   exclusivity baseline, with the four statutory factors weighed to preserve
 *   creator incentives. Under this reading the arrangement's gains
 *   concentrate on rights holders — majors, estates, collecting societies,
 *   and the platform enforcement infrastructure that monetizes policing —
 *   while its costs fall on transformative creators, documentary filmmakers,
 *   critical scholars, and libraries, plus the unseated constituencies
 *   (orphan-work users, future audiences) whose objection never reaches a
 *   courtroom. The claim/metric gap is deliberate: the reading CLAIMS the
 *   arrangement is a legitimate incentive-preserving balance, while the
 *   authored metrics describe substantially extractive, actively enforced
 *   operation — the engine measures that divergence rather than the author
 *   reconciling it. Sibling readings are separate constraint files, not
 *   hedges inside this one. KEY AGENTS (by structural relationship): -
 *   major_rights_holders: Primary beneficiary (institutional/arbitrage) —
 *   collects licensing, settlement, and damages flows; co-administers
 *   enforcement - federal_courts: Agenda setter (institutional/constrained) —
 *   weighs the four factors; recent practice re-centers market harm -
 *   platform_enforcement_infrastructure: Secondary beneficiary
 *   (institutional/mobile) — operates automated takedown and matching -
 *   transformative_creators: Primary target (moderate/constrained) — bears
 *   takedown risk and foregone expression - documentary_filmmakers: Target
 *   (organized/constrained) — clearance-cost bearer -
 *   academic_and_critical_authors: Dual-positioned target
 *   (moderate/constrained) — pays for access, collects protection for own
 *   works - libraries_and_archives: Target with residual reliance
 *   (organized/constrained) — depends on the exceptions it is losing -
 *   orphan_works_users: Excluded voice (powerless/trapped) -
 *   future_creators_and_audiences: Excluded voice (powerless/trapped,
 *   civilizational horizon) - legal_academics: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.65).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use as Narrow Exception to Property Right — Creator-Centric Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '70cd3568-c46a-4697-887d-928f44141a6d').
narrative_ontology:cs_kernel_codification('70cd3568-c46a-4697-887d-928f44141a6d', fixed_text).
narrative_ontology:cs_authority_grounding('70cd3568-c46a-4697-887d-928f44141a6d', lineage).
narrative_ontology:cs_interpretation_layer_present('70cd3568-c46a-4697-887d-928f44141a6d').
narrative_ontology:cs_reading_relation('70cd3568-c46a-4697-887d-928f44141a6d', fair_use_four_factor_test__user_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('70cd3568-c46a-4697-887d-928f44141a6d', fair_use_four_factor_test__transformative_use_reading, forecloses).
narrative_ontology:cs_axiom('70cd3568-c46a-4697-887d-928f44141a6d', foundational, exclusivity_as_default_property_baseline).
narrative_ontology:cs_axiom_status(exclusivity_as_default_property_baseline, holdable).
narrative_ontology:cs_axiom_grounding('70cd3568-c46a-4697-887d-928f44141a6d', exclusivity_as_default_property_baseline, conventional).
narrative_ontology:cs_axiom('70cd3568-c46a-4697-887d-928f44141a6d', foundational, incentive_preservation_bounds_permitted_unlicensed_use).
narrative_ontology:cs_axiom_status(incentive_preservation_bounds_permitted_unlicensed_use, holdable).
narrative_ontology:cs_axiom_grounding('70cd3568-c46a-4697-887d-928f44141a6d', incentive_preservation_bounds_permitted_unlicensed_use, instrumental).
narrative_ontology:cs_axiom('70cd3568-c46a-4697-887d-928f44141a6d', secondary, unauthorized_use_presumptively_infringing).
narrative_ontology:cs_axiom_status(unauthorized_use_presumptively_infringing, holdable).
narrative_ontology:cs_axiom_grounding('70cd3568-c46a-4697-887d-928f44141a6d', unauthorized_use_presumptively_infringing, conventional).
narrative_ontology:cs_reference_frame('70cd3568-c46a-4697-887d-928f44141a6d', creator_incentive_primacy_framework).
narrative_ontology:cs_drift_state('70cd3568-c46a-4697-887d-928f44141a6d', post_warhol_doctrine, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('70cd3568-c46a-4697-887d-928f44141a6d', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, major_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, legacy_media_estates).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, collecting_societies).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, academic_and_critical_authors).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, libraries_and_archives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, platform_enforcement_infrastructure).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, incentive_theory_of_copyright).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, property_baseline_conception_of_expression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Studios, major publishers, record labels, and large catalog owners. Treat exclusive control of their catalogs as the baseline from which any unlicensed use must justify itself. Fund test-case litigation, operate takedown and claims pipelines, and price licenses for reuse. Settlement payments, damages awards, and licensing fees land here first. They can reprice catalogs, shift enforcement intensity across works, or move titles between licensing channels.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, major_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, major_rights_holders, agenda_setter).

% Estates and heirs controlling posthumous rights in music, literary, and photographic catalogs. License aggressively and pursue claims long after the creating artist's death. Their multi-generation horizon and enforcement posture shape the clearance market every other user faces.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legacy_media_estates, beneficiary,
    institutional, generational, arbitrage, global).

% Blanket-licensing bodies collecting for the combined repertoires of many rights holders. Each narrowing of unlicensed routes pushes more users toward blanket licenses; they administer royalty distribution back to members and lobby on enforcement questions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, collecting_societies, beneficiary,
    institutional, generational, arbitrage, global).

% Article III courts and appellate panels that weigh the four statutory factors case by case. Recent Supreme Court practice has re-centered the market-effect factor, restoring weight to right-holder-facing considerations. Bound by precedent and statute; they cannot leave the docket, but they can move doctrine.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Platforms running automated content identification and notice-and-takedown systems. They monetize fingerprint matching for rights holders, avoid liability by over-complying with takedown notices, and rarely adjudicate any defense before removal. Mobile across jurisdictions and product lines.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, platform_enforcement_infrastructure, beneficiary,
    institutional, generational, mobile, global).

% Remix artists, video essayists, fan creators, and sample-based musicians whose work builds on existing recordings and footage. Licenses for critical or negative uses are typically unavailable or priced beyond reach, so their realistic options are self-censorship, small-audience distribution under the radar, or carrying takedown and damages risk.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Nonfiction filmmakers quoting archival footage and music. Insurers and broadcasters demand cleared rights even where a defense might eventually succeed, pushing projects toward paid licenses; community-written best-practice statements are their main partial shelter.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, documentary_filmmakers, payer,
    organized, biographical, constrained, national).

% Scholars, critics, and biographers who quote and reproduce excerpts for analysis. They also hold copyrights in their own works, so they stand on both sides of the line: paying for access to others' material while collecting protection for their own. Permissions queues and fear of claims shape what gets published.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, academic_and_critical_authors, payer,
    moderate, biographical, constrained, global).

% Research libraries, national archives, and preservation nonprofits that copy, lend, and digitize for access and preservation. They depend on the exceptions remaining usable; litigation over digitization and lending has made them cautious, and they lack revenue to absorb statutory damages.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, libraries_and_archives, payer,
    organized, generational, constrained, national).

% Publishers, museums, and filmmakers who want to use works whose rights holders cannot be located. The risk asymmetry — small value of the use versus outsized damages if an owner surfaces — blocks the use entirely. They hold no seat in any balancing; their objection is recorded only in failed legislation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, orphan_works_users, excluded,
    powerless, biographical, trapped, national).

% Creators and audiences not yet born who inherit whatever reusable commons survives the current enforcement posture. They bear costs as foregone works and foregone access but cannot appear in any proceeding; their interest is voiced only by proxy.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, future_creators_and_audiences, excluded,
    powerless, civilizational, trapped, universal).

% Copyright scholars mapping how factor weights move across decisions and circuits. They publish doctrinal histories and empirical studies of enforcement practice; their seat is analytical, with no stake in particular licenses or claims.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legal_academics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, major_rights_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable incentive-and-clearance system for creative markets: exclusivity makes copying excludable where reproduction cost is near zero, and the four-factor test marks which unlicensed uses fall inside the exception, giving publishers, insurers, and platforms a predictable boundary to clear against.
% TRANSFER_FUNCTION: Moves control over reuse of expressive works, and the money attached to it — license fees, settlement payments, statutory damages — from users of existing works toward rights holders; moves legal risk and foregone expression onto transformative users, libraries, and the reading public.
% ABSENT_VOICES: Orphan-work users and future creators and audiences would object but have no seat: factor-balancing happens between litigating parties with resources, so the people blocked from using unlocatable works and the generations inheriting a thinner reusable commons enter only through proxy arguments made by others.
% DISAPPEARANCE_RATIONALE: If the narrow-exception, incentive-first operation vanished overnight — say, fair use flipped to a broad user right — clearance markets, errors-and-omissions insurance, platform takedown pipelines, and rights-holder revenue models would reorganize within years; a wave of previously blocked reuse (documentaries, remixes, digital archives) would surface, and licensing prices would reprice against a smaller captive market.
% FOUNDING_PROBLEM: Reconcile exclusive rights in expression with criticism, commentary, and learning: Folsom v. Marsh (1841) framed the problem as preventing copyright from suppressing the very discourse it was enacted to stimulate; the creator-centric reading states the same problem from the incentive side — protecting the motive to create against erosion by unauthorized appropriation.
% FOUNDING_PROBLEM_CORROBORATION: Mixed, and partly outside the beneficiary set: empirical economics corroborates that some incentive effect exists but disputes that current enforcement breadth is necessary for it; historical scholarship on the Statute of Anne and Folsm v. Marsh corroborates the anti-suppression genealogy; library and archive associations attest the access half. The claim that today's enforcement level is required is attested mainly by rights holders themselves.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because unauthorized use faces presumptive hostility: licenses for critical and negative uses are unavailable or punitive, and the market-effect factor's restored weight makes any commercial footprint decisive. Suppression (0.65) reflects the takedown-first machinery and statutory-damages exposure rather than criminal coercion — the mechanism is risk asymmetry, not prohibition. Theater ratio (0.35) is moderate-low but rising: the incentive function is real, yet a growing share of enforcement activity is automated over-blocking that defends exclusivity rather than weighing factors. Accessibility collapse (0.55): alternatives persist (licensing, best-practice statements, jurisdictional variation) but collapse almost entirely for critical, negative, or orphan-work uses. Resistance (0.58): sustained advocacy, filmmaker best-practices movements, archive litigation, and recurring reform bills. The measurement series run on one shared time grid (1976, 1984, 1994, 1998, 2005, 2013, 2023) with every tracked metric authored at every point; the 1994 dip in extractiveness records the transformativeness turn widening user space, and the post-1998 rise records the enforcement ratchet (notice-and-takedown, automated filtering) culminating in the 2023 re-centering of market harm. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute a benign or functional arrangement: from the rights-holder position the system is the incentive bargain operating as designed, and from the bench it is factor-weighing, not extraction. From the payer seats the same structure operates as presumptive hostility to their work — clearance costs, takedown risk, self-censorship. The dual-positioned seats diverge internally: academic authors pay for access while collecting protection for their own output; libraries defend the exceptions they simultaneously lose. The engine computes these per-seat differences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (majors, estates, collecting societies, platform infrastructure) derive directionality near the beneficiary end; declared victims (transformative creators, filmmakers, scholars, libraries) derive near the full-target end, amplified by constrained exits — licensing is the only sanctioned route and it is closed for critical uses. Platform beneficiaries hold a conditional benefit (safe-harbor compliance plus matching revenue) that sits them slightly off the pure-beneficiary pole. Academic authors and libraries are genuinely dual-positioned — producers and preservationists as well as users — so a pure victim derivation slightly overstates their target-ness; no directionality overrides are authored because override granularity is per-power-atom and would smear heterogeneous seats sharing atoms (two moderates, two organized, five institutional), leaving the divergence to per-seat computation and this commentary instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling exclusivity with criticism and learning — is contested rather than dead: the incentive rationale is empirically disputed but not refuted, so the mismatch consumer reads contested-status against a world_rearranges verdict and correctly declines to flag a zombie mandate. The classification prevents mislabeling in both directions: calling this a snare would deny the real incentive-and-clearance coordination function that creative markets genuinely rely on; calling it a rope would ignore the asymmetric, actively enforced transfer from users to holders. The rising theater series (0.12 to 0.35) is the watch item: if judicial factor-weighing atrophies fully into automated enforcement, the arrangement drifts toward theatrical maintenance of a doctrine nobody applies — and the rising base_extractiveness series already supplies the accumulation hypothesis for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the fair_use_four_factor_test kernel — does the creator-centric instantiation (fair use as narrow exception weighed for creator incentives) correctly locate the disagreement, and what would the sibling readings change?',
    'Comparative classification across the sibling files: if user_centric_reading and transformative_use_reading compute materially different victim sets and epsilon over the same doctrine, the kernel decomposes as modeled; if they converge, the readings are stylistic variants of one constraint.',
    'If the readings converge, this file''s high epsilon double-counts a shared structure and the family should merge; if they diverge as expected, per-reading classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the fair-use kernel; siblings would flip the victim/beneficiary structure and the market-harm factor''s weight.').

omega_variable(
    incentive_necessity_empirics,
    'Does preserving creator incentives actually require the enforcement breadth this reading prescribes?',
    'Natural experiments: term extensions, piracy shocks, open-access and collective-licensing experiments; econometric studies of creative output under varied protection levels.',
    'If incentives survive narrower protection, the coordination cover thins and the arrangement drifts toward pure extraction; if incentives are fragile, the coordination function is genuine and part of the measured burden prices real risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_necessity_empirics, empirical, 'Whether the incentive rationale supports the reading''s prescribed enforcement breadth.').

omega_variable(
    chilling_effect_counterfactual,
    'How large is the victim set''s unobservable component — expression never attempted because clearance risk precedes creation?',
    'Retrospective interview and survey methods (documentary clearance studies, abandoned-project audits); comparison of reuse output across jurisdictions and periods with broader exceptions.',
    'If chilling is large, effective burden exceeds measured litigation transfers and the victim set understates the damage; if small, the reading''s cost profile rests on measurable transfers alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_counterfactual, empirical, 'Measurability of the foregone-expression component of the victim set.').

omega_variable(
    warhol_revival_durability,
    'Is the post-2023 re-centering of the market-effect factor a durable restoration of the creator-centric frame or a transient correction before legislative or doctrinal rebalancing?',
    'Track lower-court application of the 2023 decision, certiorari patterns, and any legislative movement on the statutory factors through the next decade.',
    'Durable restoration validates the rising extractiveness series'' endpoint; reversal would date the peak at 2023 and flatten the trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warhol_revival_durability, empirical, 'Durability of the market-harm factor''s restored weight.').

omega_variable(
    kernel_codification_framing,
    'Commitment-system framing under-determination: is the kernel the statutory text of the fair-use section (fixed_text) or the common-law balancing practice itself (implicit)?',
    'Ask whether doctrinal change outruns textual amendment without interpretive strain: the doctrine moved substantially across two generations under an unchanged two-sentence statute, which supports fixed_text with a strong interpretive layer.',
    'Under a practice-based framing, the authority grounding shifts and the drift vector reads as ordinary common-law evolution rather than interpretive absorption of drift; the foreclosure edges between readings would soften accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Whether the kernel is the enacted text or the accumulated balancing practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fu_ccr_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement_basis(fu_ccr_tr_t1976, observed).
narrative_ontology:measurement(fu_ccr_tr_t1984, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1984, 0.14).
narrative_ontology:measurement_basis(fu_ccr_tr_t1984, observed).
narrative_ontology:measurement(fu_ccr_tr_t1994, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1994, 0.13).
narrative_ontology:measurement_basis(fu_ccr_tr_t1994, observed).
narrative_ontology:measurement(fu_ccr_tr_t1998, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement_basis(fu_ccr_tr_t1998, observed).
narrative_ontology:measurement(fu_ccr_tr_t2005, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement_basis(fu_ccr_tr_t2005, observed).
narrative_ontology:measurement(fu_ccr_tr_t2013, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2013, 0.32).
narrative_ontology:measurement_basis(fu_ccr_tr_t2013, observed).
narrative_ontology:measurement(fu_ccr_tr_t2023, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2023, 0.35).
narrative_ontology:measurement_basis(fu_ccr_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(fu_ccr_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement_basis(fu_ccr_be_t1976, observed).
narrative_ontology:measurement(fu_ccr_be_t1984, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1984, 0.56).
narrative_ontology:measurement_basis(fu_ccr_be_t1984, observed).
narrative_ontology:measurement(fu_ccr_be_t1994, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement_basis(fu_ccr_be_t1994, observed).
narrative_ontology:measurement(fu_ccr_be_t1998, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1998, 0.56).
narrative_ontology:measurement_basis(fu_ccr_be_t1998, observed).
narrative_ontology:measurement(fu_ccr_be_t2005, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement_basis(fu_ccr_be_t2005, observed).
narrative_ontology:measurement(fu_ccr_be_t2013, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement_basis(fu_ccr_be_t2013, observed).
narrative_ontology:measurement(fu_ccr_be_t2023, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2023, 0.72).
narrative_ontology:measurement_basis(fu_ccr_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(fu_ccr_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement_basis(fu_ccr_su_t1976, observed).
narrative_ontology:measurement(fu_ccr_su_t1984, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1984, 0.42).
narrative_ontology:measurement_basis(fu_ccr_su_t1984, observed).
narrative_ontology:measurement(fu_ccr_su_t1994, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1994, 0.41).
narrative_ontology:measurement_basis(fu_ccr_su_t1994, observed).
narrative_ontology:measurement(fu_ccr_su_t1998, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1998, 0.52).
narrative_ontology:measurement_basis(fu_ccr_su_t1998, observed).
narrative_ontology:measurement(fu_ccr_su_t2005, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(fu_ccr_su_t2005, observed).
narrative_ontology:measurement(fu_ccr_su_t2013, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2013, 0.62).
narrative_ontology:measurement_basis(fu_ccr_su_t2013, observed).
narrative_ontology:measurement(fu_ccr_su_t2023, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2023, 0.65).
narrative_ontology:measurement_basis(fu_ccr_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'fair use' decomposes into three structurally distinct arrangements per the epsilon-invariance principle — the creator-centric narrow exception (this file), the transformativeness-dominant balancing, and the affirmative user right. Measuring the doctrine against creator incentives yields high extraction on unauthorized use with rights holders as beneficiaries; measuring it against public access yields a different victim set and a different epsilon. Each reading is a separate file linked here. Lineage: the creator-centric frame is the statutory-era baseline from which the transformative reading drifted (Campbell 1994) and to which Warhol (2023) partially returned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
