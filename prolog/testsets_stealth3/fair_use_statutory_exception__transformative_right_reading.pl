% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use Doctrine as Judicially Administered — Transformative-Right Reading
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the transformative_right_reading —
 *   of the contested fair_use_statutory_exception kernel; the narrow_defense
 *   and market_licensing readings are separate constraint stories and are not
 *   described here. The constraint under assessment is the standing
 *   arrangement: fair use as judicially administered under 17 U.S.C. Sec.
 *   107, together with the enforcement machinery (DMCA takedowns, automated
 *   matching, upload filtering) that surrounds it. Epsilon's referent is that
 *   standing arrangement, assessed by this reading's own lights: courts have
 *   not discharged the facilitation mandate this reading holds the doctrine
 *   exists for, and the enforcement layer taxes transformative production
 *   even where doctrine ultimately exempts the use. The reading bifurcates
 *   use classes — transformative uses sit near the exempt pole, substitutive
 *   uses near the condemned pole — and that bifurcation is documented as an
 *   omega (epsilon_bifurcation_decomposition) rather than averaged silently
 *   into the epsilon value, per the epsilon-invariance principle. KEY AGENTS
 *   (by structural relationship): - large_rights_holders: Primary beneficiary
 *   (institutional/arbitrage) — collects licensing and settlement revenue
 *   leveraged off doctrinal uncertainty; shapes doctrine through selective
 *   litigation - federal_judiciary: Agenda setter (institutional/analytical)
 *   — administers the four-factor test case-by-case; precedent accumulates in
 *   its hands - transformative_creators: Primary target
 *   (organized/constrained) — professional documentarians, parodists,
 *   biographers, critics bearing litigation risk and licensing demands -
 *   independent_digital_creators: Secondary target (powerless/trapped) —
 *   platform-hosted remixers and commentators absorbing automated takedowns
 *   without counsel - platform_intermediaries: Dual-positioned
 *   beneficiary/payer (institutional/arbitrage) — hosts transformative
 *   content under the doctrine's cover while funding filter infrastructure -
 *   libraries_educators_archives: Incidental beneficiary
 *   (organized/constrained) — preservation, teaching, and access uses
 *   sheltered by the doctrine - general_audience: Near-symmetric
 *   beneficiary/payer (powerless/mobile) — receives transformative works;
 *   loses access where enforcement suppresses them -
 *   unrepresented_remixer_communities: Excluded voice (powerless/trapped) —
 *   fan-fiction, AMV, and meme communities outside the doctrinal conversation
 *   - copyright_legal_scholars: Analytical observer (analytical/analytical) —
 *   maps the doctrine's operation from outside the benefiting parties
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.58).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.62).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use Doctrine as Judicially Administered — Transformative-Right Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'b7daf461-f2af-4b91-9761-a1b90feeac78').
narrative_ontology:cs_kernel_codification('b7daf461-f2af-4b91-9761-a1b90feeac78', fixed_text).
narrative_ontology:cs_authority_grounding('b7daf461-f2af-4b91-9761-a1b90feeac78', lineage).
narrative_ontology:cs_interpretation_layer_present('b7daf461-f2af-4b91-9761-a1b90feeac78').
narrative_ontology:cs_reading_relation('b7daf461-f2af-4b91-9761-a1b90feeac78', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7daf461-f2af-4b91-9761-a1b90feeac78', fair_use_statutory_exception__market_licensing_reading, forecloses).
narrative_ontology:cs_axiom('b7daf461-f2af-4b91-9761-a1b90feeac78', foundational, transformative_reuse_is_user_entitlement).
narrative_ontology:cs_axiom_status(transformative_reuse_is_user_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('b7daf461-f2af-4b91-9761-a1b90feeac78', transformative_reuse_is_user_entitlement, instrumental).
narrative_ontology:cs_axiom('b7daf461-f2af-4b91-9761-a1b90feeac78', foundational, licensing_markets_not_dispositive).
narrative_ontology:cs_axiom_status(licensing_markets_not_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('b7daf461-f2af-4b91-9761-a1b90feeac78', licensing_markets_not_dispositive, empirically_contingent).
narrative_ontology:cs_axiom('b7daf461-f2af-4b91-9761-a1b90feeac78', secondary, fair_use_burden_shared).
narrative_ontology:cs_axiom_status(fair_use_burden_shared, holdable).
narrative_ontology:cs_axiom_grounding('b7daf461-f2af-4b91-9761-a1b90feeac78', fair_use_burden_shared, conventional).
narrative_ontology:cs_reference_frame('b7daf461-f2af-4b91-9761-a1b90feeac78', innovation_facilitation_mandate).
narrative_ontology:cs_drift_state('b7daf461-f2af-4b91-9761-a1b90feeac78', contemporary_ai_training_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7daf461-f2af-4b91-9761-a1b90feeac78', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, large_rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, platform_intermediaries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, libraries_educators_archives).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, general_audience).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, independent_digital_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, platform_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, general_audience).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, campbell_transformativeness_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, first_amendment_safety_valve_theory).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, constitutional_progress_clause_purpose).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Media conglomerates, publishers, record labels, and studios maintain licensing divisions and enforcement teams. They decide which unlicensed uses to challenge, settle or license where the counterparty can pay, and their litigation selections accumulate into doctrine; lobbying shapes the statutory frame between amendments. They also invoke the same doctrine for their own promotional, archival, and review uses, so the arrangement both funds and constrains them.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, large_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, large_rights_holders, agenda_setter).

% Federal courts decide fair use claim-by-claim under the four statutory factors, weighing purpose, nature, amount, and market effect. Each ruling binds future cases; the courts cannot decline the questions brought to them, have no alternative forum to delegate the doctrine to, and inherit whatever enforcement-driven disputes arrive at their doors.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Documentary filmmakers, biographers, parodists, critics, and sampling musicians build work on existing copyrighted material. Where permission is refused or priced beyond reach, their path runs through the doctrine: they clear clips, carry errors-and-omissions insurance, delay releases pending opinion letters, and occasionally litigate. Guilds and filmmaker coalitions publish best-practices documents that insurers and broadcasters consult. Leaving their subject matter is not an option; the material is the work.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, payer,
    organized, biographical, constrained, national).

% Platform-hosted video essayists, reaction channels, remixers, and streamers rely on copyrighted material for their output. Automated identification systems match their uploads against rights-holder catalogs; disputes run through portal forms, and losing a channel ends their income. Few retain counsel; most absorb muted segments, demonetization, or removal without appeal.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, independent_digital_creators, payer,
    powerless, immediate, trapped, global).

% Video platforms, search engines, and hosting services intermediate billions of uploads that are either infringing or exempt. They profit from the volume the doctrine shelters, negotiate bulk licenses where certainty is worth paying for, and spend on fingerprinting infrastructure and policy teams. Their scale makes them repeat players in litigation and frequent targets of it.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, platform_intermediaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, platform_intermediaries, payer).

% Libraries, universities, and archives copy for preservation, lend digitized collections, stream films for coursework, and build text corpora for scholarship. Their budgets cannot absorb per-item licensing at commercial rates; the doctrine is what makes several core functions affordable. They coordinate through consortium statements and shared litigation counsel.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, libraries_educators_archives, beneficiary,
    organized, generational, constrained, national).

% Readers, viewers, teachers, and researchers consume the criticism, parody, scholarship, and search results the doctrine permits. Where enforcement removes a work or a clip, they lose access without any forum in which to register the loss; where the doctrine holds, they receive material no license would have cleared at consumer prices. Their consumption choices shift between platforms but not out of culture.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, general_audience, beneficiary,
    powerless, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, general_audience, payer).

% Fan-fiction writers, anime music video editors, meme-makers, and noncommercial modders create outside both the licensing economy and the litigation system. Takedowns reach them through automated notices; they have no trade association, no insurer, and no seat in congressional hearings or judicial proceedings. Their works circulate informally or vanish, and their objections surface only as silence.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, unrepresented_remixer_communities, excluded,
    powerless, immediate, trapped, global).

% Academic copyright specialists publish on the doctrine's operation, testify in hearings, and file amicus briefs. They observe the full structure — statutory text, case law, enforcement practice — without collecting from or paying into it, and their analyses supply the evidentiary base the other seats argue with.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_legal_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, large_rights_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a judge-administered, case-by-case channel through which unlicensed reuse — quotation, parody, criticism, scholarship, preservation, indexing — occurs without bilateral permission negotiation, routing the transaction-cost problem of clearing millions of incidental and cumulative uses away from rights-holder consent and into doctrinal adjudication.
% TRANSFER_FUNCTION: Moves adjudication authority over reuse from rights-holder consent to court determination; moves licensing and settlement revenue toward rights holders wherever doctrinal uncertainty deters free use; and, where the doctrine holds, moves expressive and scholarly output from creators to the public without payment.
% ABSENT_VOICES: Noncommercial remixer communities, individual takedown recipients who never respond, and audiences of suppressed works would object to the enforcement layer but hold no seat in hearings, negotiations, or most litigation; their objections surface only statistically, as abandonment and silent noncompliance.
% DISAPPEARANCE_RATIONALE: Overnight repeal would force every quotation, parody, course reserve, preservation copy, search index, and video essay through licensing or cessation; scholarly communication, criticism, and platform content economies would reorganize around clearance departments and per-use fees, and a large share of current unlicensed output would simply stop.
% FOUNDING_PROBLEM: Reconciling exclusive rights in expression with the fact that criticism, scholarship, and successive technologies (the photocopier, the VCR, search, machine learning) require unlicensed reuse of protected material — Folsom v. Marsh posed the problem in 1841 and the 1976 codification wrote the reconciliation into statute.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and the Supreme Court's own repeated reliance on the doctrine (Campbell, Google v. Oracle) attest the problem's persistence; parallel statutory exceptions abroad (UK fair dealing, EU quotation and text-and-data-mining exceptions) corroborate that the underlying tension is structural rather than local; even rights-holder industries invoke the doctrine for their own review, promotion, and archival uses. No seat attests the problem is solved.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the standing arrangement genuinely enables large volumes of unlicensed reuse (real coordination), but uncertainty is monetized — errors-and-omissions insurance, clearance delays, licensing demands on uses this reading holds should be free, and settlement leverage against smaller counterparties. Suppression (0.62) is structural: litigation-cost asymmetry and automated enforcement close alternatives for all but the best-resourced reusers; the suppression_requirement series is authored because enforcement capacity is the traced dynamic — it hardened across the interval (DMCA 1998, Content ID circa 2007, EU Article 17 in 2019) rather than staying static. Theater (0.36) reflects ritualized four-factor 'balancing' and nominal fair-use consideration inside takedown workflows that proceed regardless. Accessibility collapse (0.45) and resistance (0.6) sit where a contested legal construct sits: alternatives (licensing, avoidance) remain partly available, and resistance is continuous — best-practices coalitions, amicus campaigns, litigation. The measurement series share one grid (t=0..30 in five-year steps, mapping 1994's Campbell v. Acuff-Rose to 2024); base_extractiveness dips mid-interval as transformativeness ascends in doctrine (Campbell through the Google Books ruling) and recovers as enforcement scale and AI-era rights assertions re-tighten the arrangement. Coalition note: the payer seats' resistance runs through organized channels (guilds, library consortia, platform policy teams) rather than individual exit, which is why resistance registers at 0.6 despite powerless individual members.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical doctrine. From the federal_judiciary seat the arrangement is balanced case-by-case judgment — no extraction visible from the bench, only factors weighed. From the transformative_creators seat the same arrangement is a tollbooth of insurance, delay, and opinion letters. From large_rights_holders it is a boundary-policing instrument that monetizes ambiguity. From independent_digital_creators it is an automated gate with no human in it. The engine computes these divergences from power, exit, and directional position; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   large_rights_holders sit nearest the beneficiary pole (low d): the arrangement subsidizes their licensing leverage and settlement revenue. transformative_creators and independent_digital_creators sit nearest the target pole (high d): they pay in risk, delay, and lost output, with exit constrained or trapped. platform_intermediaries derive near-beneficiary d from their declared beneficiary role, but their dual position (filter spending, licensing outlays, litigation exposure) pulls toward symmetry — handled through secondary_role rather than a directionality_override, because overrides key on the power atom and would misfire across the story's several institutional seats. general_audience sits near symmetric: it receives works no license would clear at consumer prices and loses access where enforcement bites. libraries_educators_archives lean beneficiary. federal_judiciary is the administering seat — neither subsidized nor taxed by the doctrine's operation. Scope amplification applies modestly: the arrangement operates globally through platform enforcement while its doctrinal core is national.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the standing arrangement as tangled_rope prevents two symmetrical mislabels. Reading it as pure rope (this reading's own aspirational framing) would erase the extraction layer — monetized uncertainty, enforcement asymmetry — that this reading itself protests. Reading it as snare (the market_licensing neighbor's implication) would erase the genuine coordination function: the doctrine clears millions of uses no licensing regime could process. The R5 interview supports the hybrid: the founding problem (exclusive rights versus cumulative culture) is live, corroborated from outside the benefiting parties, and overnight disappearance would rearrange the world — so the mismatch consumer finds status=live crossed with verdict=world_rearranges, no zombie flag. Mandatrophy is not resolved: the arrangement has not outlived its function; it is functioning and contested simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the transformative_right_reading of the fair_use_statutory_exception kernel; how would the same standing arrangement classify under the narrow_defense_reading or the market_licensing_reading?',
    'Author the two sibling stories against the identical referent and stakeholder surface; compare per-seat classifications and epsilon across the three files.',
    'Under narrow_defense, epsilon rises uniformly (every unlicensed use is a potential market intrusion and the defense is construed narrowly); under market_licensing, epsilon tracks licensing-market existence rather than use character, and the bifurcation this reading draws would vanish. Cross-reading divergence is the corpus datum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings; this file holds one.').

omega_variable(
    epsilon_bifurcation_decomposition,
    'The reading assigns low epsilon to transformative uses and high epsilon to substitutive uses; does the single-story epsilon (0.58 over the standing arrangement) correctly consolidate that bifurcation, or does the epsilon-invariance principle demand two stories?',
    'Decompose into a transformative-channel story (low epsilon, rope-candidate) and a substitutive-suppression story (high epsilon, snare-candidate), link them via network.affects_constraints, and compare classifications against this consolidated story.',
    'If decomposition holds, this story''s tangled_rope verdict is an artifact of averaging two structurally distinct arrangements; the pair would classify independently and the consolidated file becomes an index entry rather than a measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_bifurcation_decomposition, conceptual, 'Whether the reading''s internal use-class bifurcation survives consolidation into one epsilon.').

omega_variable(
    speculative_licensing_market_reality,
    'Do the licensing markets courts weigh actually exist, or would they emerge only if fair use were narrowed — the Campbell objection to hypothetical markets?',
    'Market studies of existing clearance markets per use class; natural experiments where licensing was attempted at scale (news aggregation, stock footage, sample clearance houses) and where it collapsed.',
    'Robust existing markets raise epsilon for those use classes and strengthen the market_licensing sibling; purely speculative markets leave this reading''s non-dispositivity axiom intact and keep epsilon anchored to enforcement friction rather than forgone sales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_licensing_market_reality, empirical, 'Empirical status of the licensing markets the arrangement''s opponents cite.').

omega_variable(
    chilling_structural_vs_internalized,
    'How much of the measured suppression is structural (takedown automation, litigation cost, insurance requirements) versus internalized (self-censorship habits that persist when enforcement eases)?',
    'Compare creation and clearance-seeking rates across jurisdictions and periods with differing enforcement intensity; survey abandoned-project testimony among creators who exited.',
    'An internalized component means effective suppression exceeds the structural measure and would persist through doctrinal liberalization — courts could facilitate fully and still find production depressed; the omega bounds how far enforcement reform alone closes the gap this reading protests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_structural_vs_internalized, empirical, 'Structural vs internalized share of the arrangement''s suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_tr_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(fair_use_tr_tr_t0, observed).
narrative_ontology:measurement(fair_use_tr_tr_t5, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(fair_use_tr_tr_t5, observed).
narrative_ontology:measurement(fair_use_tr_tr_t10, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(fair_use_tr_tr_t10, observed).
narrative_ontology:measurement(fair_use_tr_tr_t15, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(fair_use_tr_tr_t15, observed).
narrative_ontology:measurement(fair_use_tr_tr_t20, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(fair_use_tr_tr_t20, observed).
narrative_ontology:measurement(fair_use_tr_tr_t25, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement_basis(fair_use_tr_tr_t25, observed).
narrative_ontology:measurement(fair_use_tr_tr_t30, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(fair_use_tr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fair_use_tr_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(fair_use_tr_be_t0, observed).
narrative_ontology:measurement(fair_use_tr_be_t5, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(fair_use_tr_be_t5, observed).
narrative_ontology:measurement(fair_use_tr_be_t10, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(fair_use_tr_be_t10, observed).
narrative_ontology:measurement(fair_use_tr_be_t15, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(fair_use_tr_be_t15, observed).
narrative_ontology:measurement(fair_use_tr_be_t20, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(fair_use_tr_be_t20, observed).
narrative_ontology:measurement(fair_use_tr_be_t25, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(fair_use_tr_be_t25, observed).
narrative_ontology:measurement(fair_use_tr_be_t30, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(fair_use_tr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_tr_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(fair_use_tr_su_t0, observed).
narrative_ontology:measurement(fair_use_tr_su_t5, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(fair_use_tr_su_t5, observed).
narrative_ontology:measurement(fair_use_tr_su_t10, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(fair_use_tr_su_t10, observed).
narrative_ontology:measurement(fair_use_tr_su_t15, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(fair_use_tr_su_t15, observed).
narrative_ontology:measurement(fair_use_tr_su_t20, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(fair_use_tr_su_t20, observed).
narrative_ontology:measurement(fair_use_tr_su_t25, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(fair_use_tr_su_t25, observed).
narrative_ontology:measurement(fair_use_tr_su_t30, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(fair_use_tr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, dmca_notice_and_takedown_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into a three-member constraint family by reading: transformative_right_reading (this file), narrow_defense_reading, and market_licensing_reading. Each member authors its own epsilon over the same standing arrangement — this reading indexes epsilon to the facilitation shortfall and enforcement friction it protests; the siblings index it to market-value preservation and licensing-market displacement respectively. The members are linked pairwise via network.affects_constraints; the dmca_notice_and_takedown_regime edge records the causal dependency whereby enforcement machinery, not statutory text, supplies most of the arrangement's operative suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
