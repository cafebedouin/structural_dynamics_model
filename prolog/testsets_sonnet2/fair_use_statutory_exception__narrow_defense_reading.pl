% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrowly-Construed Affirmative Defense (Property-First Reading)
 *   domain: legal/economic/informational
 *
 * SUMMARY:
 *   This story instantiates the narrow-defense reading of the fair use kernel
 *   under 17 U.S.C. § 107: copyright is property, fair use is an affirmative
 *   defense the defendant must plead and prove, and the fourth statutory
 *   factor — effect on the market for or value of the copyrighted work —
 *   functions as the practical center of gravity, with commercial character
 *   of the use treated as strongly probative of market harm. Under this
 *   reading, transformativeness is one factor among several rather than the
 *   organizing question, and any use touching a plausible (even hypothetical)
 *   licensing market weighs against the defendant. The 1998-2008 acceleration
 *   in the extraction trajectory tracks the post-DMCA,
 *   post-Napster-litigation period in which rightsholders and collecting
 *   societies actively litigated to establish market-harm-centric doctrine
 *   and notice-and-takedown enforcement infrastructure matured. This is a
 *   distinct constraint from the market_licensing_reading (which asks a
 *   narrower empirical question — does a licensing market exist at all) and
 *   from the transformative_right_reading (which inverts the burden and the
 *   organizing question entirely, asking whether the use serves cultural
 *   production). Each reading has its own epsilon; they are linked as
 *   siblings of the same kernel, not measured as one constraint from
 *   different angles.
 *
 * KEY AGENTS:
 *   - incumbent_rightsholders: primary beneficiary (institutional/arbitrage) — collects licensing revenue and litigation leverage from narrow construal
 *   - licensing_intermediaries: secondary beneficiary (organized/arbitrage) — entire business model depends on narrow reading
 *   - independent_commentators: primary target (powerless/trapped) — bears chilling effect and self-censorship
 *   - documentary_filmmakers, digital_archivists, remix_and_sampling_artists: secondary targets (moderate-to-powerless) — bear clearance costs, insurance burden, or existential litigation risk
 *   - courts_applying_the_four_factor_test: agenda-setter (institutional/analytical) — administers the doctrine
 *   - transformative_use_advocates: excluded voice (organized/constrained) — argues the sibling reading, structurally outweighted in doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.71).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.62).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrowly-Construed Affirmative Defense (Property-First Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "legal/economic/informational").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '2a4776b5-c20f-46bf-baf9-db3a7c091eea').
narrative_ontology:cs_kernel_codification('2a4776b5-c20f-46bf-baf9-db3a7c091eea', fixed_text).
narrative_ontology:cs_authority_grounding('2a4776b5-c20f-46bf-baf9-db3a7c091eea', lineage).
narrative_ontology:cs_interpretation_layer_present('2a4776b5-c20f-46bf-baf9-db3a7c091eea').
narrative_ontology:cs_reading_relation('2a4776b5-c20f-46bf-baf9-db3a7c091eea', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_reading_relation('2a4776b5-c20f-46bf-baf9-db3a7c091eea', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_axiom('2a4776b5-c20f-46bf-baf9-db3a7c091eea', foundational, copyright_is_property_fair_use_is_exception).
narrative_ontology:cs_axiom_status(copyright_is_property_fair_use_is_exception, holdable).
narrative_ontology:cs_axiom_grounding('2a4776b5-c20f-46bf-baf9-db3a7c091eea', copyright_is_property_fair_use_is_exception, conventional).
narrative_ontology:cs_axiom('2a4776b5-c20f-46bf-baf9-db3a7c091eea', foundational, market_effect_factor_is_practically_dispositive).
narrative_ontology:cs_axiom_status(market_effect_factor_is_practically_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('2a4776b5-c20f-46bf-baf9-db3a7c091eea', market_effect_factor_is_practically_dispositive, instrumental).
narrative_ontology:cs_axiom('2a4776b5-c20f-46bf-baf9-db3a7c091eea', secondary, commercial_character_strongly_probative_of_harm).
narrative_ontology:cs_axiom_status(commercial_character_strongly_probative_of_harm, holdable).
narrative_ontology:cs_axiom_grounding('2a4776b5-c20f-46bf-baf9-db3a7c091eea', commercial_character_strongly_probative_of_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('2a4776b5-c20f-46bf-baf9-db3a7c091eea', property_first_exclusive_rights_framework).
narrative_ontology:cs_drift_state('2a4776b5-c20f-46bf-baf9-db3a7c091eea', post_digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a4776b5-c20f-46bf-baf9-db3a7c091eea', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_commentators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, digital_archivists).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, remix_and_sampling_artists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, streaming_and_platform_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, streaming_and_platform_intermediaries).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_as_property_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_harm_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold large catalogs of copyrighted works and license derivative, excerpt, and reuse rights as a revenue stream. Litigate aggressively to establish precedent that any use touching a plausible licensing market defeats fair use, and lobby for statutory and judicial doctrine that treats the four-factor test's fourth factor (market effect) as dispositive. Benefit directly whenever a court treats an unauthorized use as displacing a licensable market, whether or not that market currently exists in practice.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders, agenda_setter).

% Collecting societies, stock-footage clearinghouses, and rights-clearance firms whose entire business model depends on fair use being read narrowly enough that most secondary uses require a paid license. Their revenue rises directly with the scope of what counts as infringing rather than excused.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries, beneficiary,
    organized, biographical, arbitrage, national).

% Bloggers, critics, and educators who quote, excerpt, or embed copyrighted material to make an argument or teach a point. Cannot afford litigation to test whether their use qualifies as fair use under a narrow-defense standard, so most self-censor, seek licenses they cannot afford, or take down content on receipt of a takedown notice regardless of the notice's legal merit. Their only 'exit' is not making the commentary at all.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_commentators, payer,
    powerless, immediate, trapped, global).

% Rely on archival footage, news clips, and cultural artifacts to construct historical and journalistic works. Under the narrow reading, insurance carriers and distributors require errors-and-omissions coverage that itself requires clearance opinions treating almost all uncleared use as risky, so films are cut, delayed, or abandoned rather than defended in court. They can negotiate licenses if well-funded, but the negotiation leverage sits entirely with the rightsholder.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Libraries, universities, and nonprofit archives that preserve and provide access to cultural and scholarly material at scale. A narrow defense reading treats mass digitization and text-and-data-mining as presumptively infringing unless a licensing market is shown absent, which incumbents dispute reflexively to preserve future licensing optionality even where no current market exists. Archivists face existential litigation risk for core preservation activity.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, digital_archivists, payer,
    moderate, civilizational, constrained, global).

% Musicians, video essayists, and meme-culture creators whose work is built from recombining existing copyrighted material. Under a reading where commercial context and potential licensing markets are near-determinative, almost any monetized platform use is presumptively unlicensed infringement, and transformative intent is treated as secondary to market substitution analysis. Exit means working unmonetized, anonymously, or not at all.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, remix_and_sampling_artists, payer,
    powerless, biographical, trapped, global).

% Platforms benefit from a narrow reading insofar as it lets them outsource enforcement risk to automated takedown systems and claim safe-harbor compliance, but they also pay in the form of litigation exposure and the cost of building rights-clearance infrastructure. Their interest is mixed: predictable narrow rules reduce their own liability even as they suppress the user-generated content that drives engagement.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, streaming_and_platform_intermediaries, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, streaming_and_platform_intermediaries, payer).

% Adjudicate fair use case by case under 17 U.S.C. § 107, treating it as an affirmative defense the defendant must plead and prove. Under the narrow-defense reading, courts weight commercial character and market-effect heavily, often collapsing the multi-factor balancing into a de facto presumption against the defendant once any commercial dimension or hypothetical licensing market is shown.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts_applying_the_four_factor_test, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars, digital rights organizations, and creator coalitions who argue fair use should be read as an affirmative user right central to First Amendment values and cultural production, not a narrow carve-out from property. Their position is structurally present in academic literature and amicus briefs but is systematically outweighted in doctrine that treats copyright-as-property and market-harm as the organizing frame.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_use_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable rule for allocating the economic value of creative works between original creators/rightsholders and downstream users, in principle protecting incentives to create by ensuring creators can capture value from licensing markets rather than having that value captured for free by secondary users.
% TRANSFER_FUNCTION: Moves the burden of proof and the economic risk of ambiguity from rightsholders to users: any use with plausible commercial dimension or hypothetical licensing market is presumed infringing until the user affirmatively proves otherwise, shifting litigation costs, insurance costs, and self-censorship costs onto commentators, archivists, and remix artists while channeling licensing revenue toward rightsholders and clearance intermediaries.
% ABSENT_VOICES: Transformative-use advocates, library and archive coalitions, and the broader public interested in low-cost access to commentary and derivative culture are present in academic and amicus contexts but structurally outweighted in the doctrinal frame that treats copyright as property first and fair use as an exception construed against the user. Individual would-be commentators who never make the work because of chilling effect are, by construction, invisible to the record entirely.
% DISAPPEARANCE_RATIONALE: If the narrow-defense reading were replaced by a use-facilitating standard tomorrow, licensing intermediaries would lose substantial revenue, incumbent rightsholders would lose leverage in clearance negotiations, and documentary filmmakers, archivists, and remix artists would face a materially lower barrier to legal use — insurance and clearance-industry practices built around the current presumption would have to reorganize around a different default.
% FOUNDING_PROBLEM: Fair use was codified in 1976 to preserve the flexible, judge-made doctrine allowing socially valuable uses (criticism, scholarship, news reporting, parody) without categorical infringement liability, and to prevent copyright's exclusive rights from swallowing the public domain of ideas, facts, and cultural reuse that copyright was never meant to lock down.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder and licensing-industry amici attest the doctrine functions correctly by protecting licensing markets that would otherwise be undermined by free-riding. Library associations, First Amendment scholars, and courts in cases like Google v. Oracle and Authors Guild v. Google (outside the rightsholder-benefiting coalition) have found substantial uses fair despite commercial context, corroborating that the founding purpose — enabling socially valuable secondary use — remains live and is in active tension with, not settled by, the narrow-defense reading.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) is authored high because the narrow-defense reading systematically resolves ambiguity against secondary users whenever a commercial dimension or hypothetical licensing market can be alleged, which is nearly always possible to allege even where no actual market exists. Suppression (0.62) reflects both litigation risk and the chilling effect of uncertainty — the doctrine does not need to be enforced against every use to suppress most uses, because the cost of being wrong (statutory damages up to $150,000 per willful infringement) makes risk-averse self-censorship rational even for defensible uses. Accessibility collapse (0.58) is moderate rather than extreme: some uses remain clearly fair (parody, limited quotation in criticism) even under the narrow reading, so alternatives have not collapsed completely, but the margin of clearly-safe use has narrowed substantially since 1976. Resistance (0.55) reflects active, organized pushback from library associations, digital rights groups, and courts in high-profile cases (Google Books, Oracle v. Google) that have declined to apply the narrowest version of the doctrine — this is a live, contested doctrine, not a settled one.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent rightsholder seat, this doctrine is coordination: it protects the economic incentive structure that funds creative production by ensuring value flows back to creators through licensing. From the independent commentator or remix artist seat, the identical structure operates as extraction: a de facto requirement to pay for or forgo speech and cultural production that would, under a use-facilitating reading, be uncontroversially permitted. The engine should compute these as different per-seat classifications from the same structural data — the tangled_rope claim asserts both the coordination function (real: incentive protection is not fictional) and the asymmetric extraction (real: the burden and cost fall overwhelmingly on low-power secondary users) are simultaneously present, which is exactly what tangled_rope requires and what a pure snare or pure rope classification would miss.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent rightsholders and licensing intermediaries derive low d (near full beneficiary) because they collect licensing revenue and litigation leverage directly from the narrow construal and have arbitrage-grade exit (they choose which uses to pursue and can license selectively). Independent commentators and remix artists derive high d (near full target) because they are trapped — their only true exit is not creating the work — and they bear the cost of legal uncertainty without the resources to test it. Documentary filmmakers and digital archivists sit closer to the target end but with somewhat more mobility (grant funding, institutional legal counsel) than individual commentators, hence 'constrained' rather than 'trapped' exit options and moderate rather than powerless power atoms. Streaming platforms are directionally mixed — they benefit from predictable safe-harbor rules but pay in litigation exposure — captured with a secondary payer role rather than an override, since the structural derivation from dual roles already captures the ambivalence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing copyright's exclusive rights from swallowing socially valuable secondary use) remains live by the corroboration of courts outside the rightsholder coalition (Google v. Oracle, Authors Guild v. Google) that have found substantial commercial uses fair despite the narrow reading's presumptions — this blocks a clean mandatrophy verdict (the founding problem is not dead) while the doctrine's operational tilt toward market-harm primacy shows the classification correctly refuses to treat the arrangement as either pure coordination (it demonstrably burdens powerless users asymmetrically) or pure extraction (it demonstrably protects real incentive structures and has been checked by courts in high-profile cases). Tangled rope holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_reading_vs_statutory_text_ambiguity,
    'Does the statutory text of 17 U.S.C. § 107 itself compel the narrow-defense, market-harm-primacy reading, or is this reading a judicially and industry-constructed gloss on a text that is genuinely open to the transformative-right reading?',
    'Close doctrinal history of pre-1976 case law codified by the statute, legislative history of the House and Senate reports accompanying the Copyright Act of 1976, and comparison of circuit splits on how heavily to weight factor four versus factor one (purpose and character of use, including transformativeness).',
    'If the statutory text is genuinely indeterminate between readings, this constraint is better understood as one contestable interpretive tradition among coequal alternatives rather than the ''correct'' reading of a determinate text — which affects how much authority-grounding weight the narrow reading''s precedent should carry going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_reading_vs_statutory_text_ambiguity, conceptual, 'Whether the narrow-defense reading is compelled by statutory text or is a constructed interpretive gloss.').

omega_variable(
    hypothetical_versus_actual_licensing_market,
    'Should a licensing market that does not currently exist but could be created count against fair use under factor four, or should only actual, extant markets count?',
    'Track post-2020 circuit decisions on whether courts credit rightsholder claims of hypothetical foregone licensing revenue absent evidence of an actual functioning market for that specific use.',
    'If hypothetical markets count, the narrow reading''s extractiveness is structurally unbounded (any use is potentially licensable in principle); if only actual markets count, extractiveness is bounded by rightsholders'' actual commercial practice, meaningfully narrowing the constraint''s reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_versus_actual_licensing_market, empirical, 'Whether hypothetical licensing markets suffice to defeat fair use under the narrow reading.').

omega_variable(
    reading_selection_framing_ambiguity,
    'Is the choice to treat this as the ''narrow defense'' reading rather than folding it into the market_licensing_reading a genuine structural distinction, or are these two readings actually the same underlying doctrine described at different grain?',
    'Compare case outcomes: does the narrow-defense reading ever produce a different result than the market_licensing_reading would on identical facts (e.g., a case with no plausible market but strong commercial character)? If outcomes never diverge, the readings may be redundant framings of one constraint rather than two.',
    'If the readings are outcome-equivalent, this story and market_licensing_reading should be merged rather than treated as siblings, changing the network topology of the kernel family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_framing_ambiguity, conceptual, 'Whether the narrow-defense and market-licensing readings are structurally distinct or the same doctrine at different resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement(fair_tr_t1988, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1988, 0.15).
narrative_ontology:measurement(fair_tr_t1998, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1998, 0.19).
narrative_ontology:measurement(fair_tr_t2008, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(fair_tr_t2016, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.42).
narrative_ontology:measurement(fair_be_t1988, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1988, 0.48).
narrative_ontology:measurement(fair_be_t1998, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(fair_be_t2008, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(fair_be_t2016, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.38).
narrative_ontology:measurement(fair_su_t1988, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1988, 0.44).
narrative_ontology:measurement(fair_su_t1998, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1998, 0.53).
narrative_ontology:measurement(fair_su_t2008, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2008, 0.57).
narrative_ontology:measurement(fair_su_t2016, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, transformative_right_reading).

% DUAL FORMULATION NOTE:
% This constraint, market_licensing_reading, and transformative_right_reading are three readings of the same statutory kernel (fair_use_statutory_exception, 17 U.S.C. § 107). They share the same text and the same four-factor structure but differ in which factor is treated as organizing and in the burden allocation implied. This story (narrow_defense_reading) authors the highest epsilon of the three, reflecting property-first framing with market-harm primacy; market_licensing_reading authors a narrower, more empirically-gated epsilon keyed to actual market existence; transformative_right_reading authors substantially lower epsilon, reflecting an affirmative-right framing that facilitates rather than narrowly excuses secondary use. Each is a distinct constraint with its own stakeholders and its own claimed type — not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
