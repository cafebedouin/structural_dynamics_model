% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV-VI Reciprocal Pairing (Grand Bargain Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   Under the grand-bargain reading of the NPT Article IV-VI pairing, the
 *   treaty establishes a conditional reciprocal obligation:
 *   non-nuclear-weapon states (NNWS) restrain from weapons development
 *   (Article IV) in exchange for nuclear-weapon states' (WS) commitment to
 *   pursue disarmament (Article VI). This reading treats Article VI not as
 *   aspirational but as enforceable, and interprets NNWS restraint as
 *   contingent on measurable WS disarmament progress. When WS fail to disarm,
 *   the grand-bargain reading holds that NNWS lose the legitimacy of Article
 *   IV restraint and gain grounds for withdrawal or reinterpretation. The
 *   constraint is a tangled rope: genuine coordination function (both sides
 *   benefit from reduced proliferation risk) paired with asymmetric
 *   extraction (NNWS surrender a security option while WS retain full
 *   arsenals under a broken promise). The measurement series documents a
 *   56-year trajectory of rising extractiveness and theater—disarmament
 *   rhetoric intensifies while arsenal reductions stall, suggesting the
 *   functional core (non-proliferation coordination) persists but is
 *   increasingly decoupled from the stated reciprocal obligation.
 *
 * KEY AGENTS:
 *   - Nuclear weapons states (institutional agenda-setters; define disarmament timelines and verification standards; retain arsenals)
 *   - Non-nuclear-weapon states (organized payers; surrender weapons option; receive broken disarmament promise)
 *   - Threshold proliferators (identity-locked to non-proliferation; bear strategic restraint cost)
 *   - TPNW signatories (excluded; represent alternative reading)
 *   - Disarmament advocacy movements (payers bearing credibility cost of unfulfilled promises)
 *   - Verification bodies (observers; document Article VI breaches without enforcement authority)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV-VI Reciprocal Pairing (Grand Bargain Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'f9aa044b-5376-48a0-8ab3-b84b1831f8ce').
narrative_ontology:cs_kernel_codification('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', fixed_text).
narrative_ontology:cs_authority_grounding('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', extraction).
narrative_ontology:cs_interpretation_layer_present('f9aa044b-5376-48a0-8ab3-b84b1831f8ce').
narrative_ontology:cs_reading_relation('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', foundational, article_vi_disarmament_conditional_enforceability).
narrative_ontology:cs_axiom_status(article_vi_disarmament_conditional_enforceability, holdable).
narrative_ontology:cs_axiom_grounding('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', article_vi_disarmament_conditional_enforceability, conventional).
narrative_ontology:cs_axiom('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', foundational, nnws_restraint_contingent_on_ws_progress).
narrative_ontology:cs_axiom_status(nnws_restraint_contingent_on_ws_progress, holdable).
narrative_ontology:cs_axiom_grounding('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', nnws_restraint_contingent_on_ws_progress, instrumental).
narrative_ontology:cs_reference_frame('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', reciprocal_disarmament_conditionality).
narrative_ontology:cs_drift_state('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', post_cold_war_stagnation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f9aa044b-5376-48a0-8ab3-b84b1831f8ce', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapons_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, disarmament_advocacy_movements).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, threshold_and_latent_proliferators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret the NPT framework; set verification protocols and disarmament timelines through the Review Conferences; retain arsenals justified under Article VI's indefinite non-binding commitment. They frame Article VI compliance as a gradual, security-contingent process whose pace is determined by international stability assessments. They benefit from the arrangement by maintaining arsenals under treaty cover while avoiding binding timelines.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_states, beneficiary).

% Forfeit the right to develop nuclear weapons (Article IV restraint); receive (theoretically) Article VI disarmament progress in exchange. They pay the non-proliferation cost by closing a security option; they benefit from a reduced-arsenal world and non-nuclear-weapon-state security assurances. Their exit options are treaty withdrawal (legally available, politically costly) or Article IV expansion (reinterpreting Article IV to permit peaceful nuclear development toward weapons-latency).
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapons_states, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapons_states, beneficiary).

% Bear the moral and political cost of advocacy under a reading that promises disarmament but does not deliver; expend credibility on Review Conference process that produces non-binding recommendations. Their exit is to shift advocacy to the Treaty on the Prohibition of Nuclear Weapons (TPNW) framework, which they increasingly view as an alternative kernel.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, disarmament_advocacy_movements, payer,
    moderate, biographical, mobile, global).

% Face strategic pressure to develop or maintain nuclear capacity (security dilemma, regional rivalries) while committed to non-proliferation under NPT. They are identity-locked to the non-proliferation commitment by alliance security guarantees and political framing, even as domestic factions view Article IV as illegitimately restricting justified security options. They bear the cost of strategic restraint.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, threshold_and_latent_proliferators, payer,
    powerful, biographical, identity_locked, regional).

% Implement and report on NPT compliance; have limited authority over nuclear weapons states' arsenals or disarmament reporting; produce assessments that reveal Article VI non-compliance. Their constraint-relative role is to document the breach, not to adjudicate it or compel remedy.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, verification_and_monitoring_bodies, observer,
    institutional, generational, constrained, global).

% Administer the treaty framework; convene Review Conferences; maintain the official record of state compliance and reservations. They operate within the constraint rather than control it; their role is performative administration of a rule set written by others.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, treaty_depositary_states, agenda_setter,
    institutional, generational, trapped, global).

% Have adopted an alternative reading (abolitionist) that frames Article VI as illegitimate and Article IV as incompatible with weapons-prohibition commitments. They are formally outside the NPT reciprocity frame and would object to the grand-bargain reading on the grounds that it perpetuates the legitimacy of perpetual nuclear deterrence.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, tpnw_signatories, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a multi-level coordination problem: (1) prevents horizontal proliferation by binding NNWS to non-weaponization; (2) commits WS to gradual disarmament as the quid pro quo; (3) establishes verification reciprocity — WS arsenal transparency contingent on NNWS restraint compliance. The grand-bargain reading treats the coordination as explicitly conditional rather than aspirational.
% TRANSFER_FUNCTION: Transfers a strategic option (nuclear weapons development) from NNWS to WS; in exchange, transfers a disarmament commitment and verification transparency from WS to NNWS. When Article VI disarmament fails to materialize, the transfer becomes asymmetric: NNWS surrender a security option in exchange for a broken promise, while WS retain full arsenals.
% ABSENT_VOICES: TPNW signatories and abolitionist advocacy movements are structurally excluded from the NPT review process and decision-making; they would argue that the grand-bargain reading perpetuates the legitimacy of nuclear deterrence and makes Article IV a permanent restraint on NNWS rather than a transitional mechanism. Proliferators and would-be nuclear states lack standing in the formal process, though their preferences shape the underlying strategic dynamics.
% DISAPPEARANCE_RATIONALE: If the Article IV-VI pairing (as interpreted under the grand-bargain reading) disappeared overnight, NNWS would rapidly reposition: some would pursue nuclear development under Article IV's peaceful-use provisions, others would withdraw from the treaty, alliances would reorganize around nuclear-umbrella restructuring, and regional security competitions would intensify. WS would lose treaty cover for their arsenals and face immediate pressure for verifiable disarmament. The arrangement structures the entire post-1968 non-proliferation order.
% FOUNDING_PROBLEM: The Cold War nuclear standoff created a double security dilemma: (1) non-nuclear states feared WS would use weapons or nuclear blackmail absent a formal restraint; (2) WS feared horizontal proliferation would destabilize deterrence and increase accident risk. The NPT grand bargain offered to solve both: NNWS would forgo the security option of weaponization in exchange for WS commitment to negotiate disarmament and provide security assurances.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapons states and non-aligned NNWS attest the founding security dilemma was real and remains partially live (proliferation risk, deterrence stability). Disarmament advocates and TPNW signatories attest the founding problem is solved by alternative frameworks (TPNW, enhanced conventional deterrence, UN collective security) and that the NPT perpetuates an illegitimate dual system. Independent strategic analysts (e.g., International Crisis Group, Stockholm International Peace Research Institute) document that Article VI disarmament has stalled while Article IV restraint has held, suggesting the bargain is broken and the founding rationale has eroded.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint transfers a permanent strategic restraint from NNWS to WS while WS retain the option of weaponization. The transfer is unequal: NNWS lose an exit route; WS gain an arena to exercise arms-control diplomacy while maintaining deterrent arsenals. Suppression (0.72) is substantial because the arrangement's persistence depends on active exclusion of exit routes—NNWS cannot legally pursue weapons without breaching the treaty, and the Review Conference process provides a valve for discontent while binding no WS commitment. Theater (0.48) is moderate-high: the disarmament machinery (subsidiary bodies, technical working groups, five-yearly reviews) is real but produces non-binding recommendations; the machinery functions as a legitimacy mechanism for WS retention of arsenals rather than as an enforcement path for Article VI. Accessibility collapse (0.52) is moderate because NNWS retain legal exit (withdrawal) and reinterpretation (Article IV expansion for peaceful nuclear fuel cycles), though both exits carry high political costs. Resistance (0.71) is substantial: NNWS coalitions voice objection at review conferences, TPNW mobilization offers an alternative framework, and threshold states chafe under the restraint—the constraint meets real opposition, even if enforcement-backed exit remains limited. The measurement trajectory shows extractiveness rising from 0.42 to 0.68 over the interval: early disarmament momentum (1970s–1980s) created genuine reciprocal hope; Cold War end promised rapid WS reduction (1989–2000, mid-trajectory plateau); post-2000 stagnation, 9/11, and nuclear modernization reversed disarmament direction while NNWS remained bound, driving extractiveness upward. Theater rises from 0.25 to 0.48: proportionally more Review Conference activity addresses non-compliance complaints and procedural workarounds, relatively less achieves binding WS commitments. The shared time grid (six measurement points, each metric at every point) ensures the temporal analysis does not fabricate type transitions.
 *
 * PERSPECTIVAL GAP:
 *   The WS and organized-NNWS seats compute radically different constraint types from identical structural data. From the WS seat, the constraint is genuine coordination—they set and enforce the rules, the non-proliferation outcome holds, and their exercise of disarmament diplomacy (even non-binding) is treated as good-faith reciprocal effort. The computed type from that seat is rope-grade coordination with modest extraction overhead. From the organized-NNWS seat and especially the threshold-proliferator seat, the same constraint computes as snare-grade extraction: the reciprocal promise is broken, restraint is enforced by legal architecture and alliance commitments, and the WS retain full arsenal flexibility. The engine computes per-seat; the divergence is the measurement the corpus captures. The claimed type (tangled rope) reflects the author's (the reading's) analytical stance: the coordination is real, the extraction is real, both operate through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   From the WS seat (institutional, arbitrage-mobile, beneficiary): the constraint coordinates non-proliferation (a genuine coordination problem WS solved) and simultaneously extracts the benefit of indefinite deterrent retention. Directionality approaches 0.2–0.3 (net beneficiary; they set the terms and collect the restraint rent). From the NNWS organized-seat (organized, constrained exit, payer): the constraint promises disarmament but delivers restraint; directionality approaches 0.8–0.85 (net target; they surrender an exit option in exchange for a non-binding commitment). From the threshold-proliferator seat (powerful, identity-locked, payer): directionality is near the target end (0.75+) because identity-lock prevents exit despite strategic pressure to pursue weapons—the constraint operates against their revealed preference but is internalized as legitimate through alliance commitments. From the disarmament-advocacy seat (moderate, mobile): directionality is near 0.65–0.70 (a payer bearing credibility cost, but with exit available to TPNW framing). The engine derives these d values automatically from the beneficiary/victim declarations and exit-option asymmetry; the commentary documents why the grand-bargain reading produces such divergent seat experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   The grand-bargain reading embodies a mandatrophy candidate: the founding problem (Cold War nuclear security dilemma) is substantially resolved (Soviet Union dissolved, multipolar deterrence replaced bipolar standoff, conventional military dominance reduces WS reliance on nuclear deterrent), but the arrangement persists because the mandate has outlived its stated function while producing durable extraction. The constraint's persistence is justified under a shifted narrative—non-proliferation is now framed as inherent good rather than reciprocal quid pro quo—but the NNWS restraint cost remains locked in. At the 2010–2024 interval, the founding problem status crosses from 'live' to 'contested': WS argue the problem persists (regional proliferation risk, deterrence stability); NNWS and advocates argue it is solved and Article IV has become a permanent, asymmetric restraint mechanism. Theater rise (0.25 → 0.48) documents this shift: the review machinery becomes a conflict-management forum rather than a disarmament-implementation mechanism. The six_questions.founding_problem_status field declares this contested status; the measurement trajectory provides evidence for the mandatrophy reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforceability_ambiguity,
    'Is Article VI disarmament a legally enforceable obligation, or is it an aspirational commitment whose timeline is determined solely by WS security calculations?',
    'International Court of Justice advisory opinion on treaty interpretation (formally requested by NNWS coalition or UNGA), or emergence of binding enforcement mechanisms through treaty amendment or subsidiary protocol.',
    'If enforceable, the grand-bargain reading is structurally sound and NNWS have grounds to withdraw or reinterpret Article IV upon WS failure to disarm. If aspirational only, the grand-bargain reading collapses and Article IV becomes a unilateral NNWS restraint unanchored to reciprocal WS obligation, restructuring the constraint as a snare. This is the most critical omega for the reading''s sustainability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_enforceability_ambiguity, conceptual, 'The status of Article VI as binding vs. aspirational.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding Cold War security dilemma been substantially resolved such that the NPT''s rationale no longer applies, or does horizontal proliferation risk persist at founding-level severity?',
    'Longitudinal analysis of proliferation attempts, failed weapons programs, and regional security dynamics (20-year retrospect); NNWS surveys of security preference shifts; empirical assessment of conventional deterrence sufficiency replacing nuclear deterrence.',
    'If founding problem is dead, mandatrophy is confirmed and the constraint becomes pure extraction defended by institutional inertia and legal path-dependency—theater_ratio interpretation shifts from ''conflict-management machinery'' to ''pure theater.'' If founding problem remains live, the extraction is genuinely payment for coordination service and the theater is legitimate process overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding Cold War security dilemma remains live or has been substantially resolved.').

omega_variable(
    verification_reciprocity_breakdown,
    'Is the measured suppression (0.72) primarily structural (treaty-enforcement mechanisms) or increasingly internalized through identity-lock and alliance politics, such that NNWS would maintain Article IV restraint even absent formal treaty architecture?',
    'Post-exit observation from a major NNWS that withdraws from the NPT: do they pursue weapons, or do alliance commitments and internalized non-proliferation norms hold restraint despite legal exit?',
    'If suppression is primarily structural, the constraint is treaty-contingent and removable. If internalized, the constraint''s persistence is decoupled from the treaty mechanism itself and operates through identity-fusion—the suppression measurement underestimates persistence, and the constraint is more piton-like than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_reciprocity_breakdown, empirical, 'Whether suppression operates structurally or through internalized identity-lock.').

omega_variable(
    article_iv_peaceful_use_expansion,
    'Does Article IV''s ''inalienable right'' to peaceful nuclear technology create a latent avenue for reinterpreting Article IV as permitting dual-use development (enrichment, reprocessing) that approaches weapons capability without technical breach?',
    'Emerging practice: NNWS reinterpretation of Article IV in Review Conferences or IAEA governance; emergence of openly declared ''breakout'' capacity positioned as peaceful; legal challenge to IAEA authority to restrict dual-use technology transfer.',
    'If Article IV is reinterpreted to permit dual-use development, the constraint collapses from an extraction mechanism into a capability-approaching arrangement. NNWS exit via Article IV expansion becomes plausible and the WS enforcement machinery must shift from suppression to active destruction or containment of facilities. The constraint type would shift toward snare as WS resistance to exit intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_iv_peaceful_use_expansion, empirical, 'Whether Article IV peaceful-use language can be reinterpreted to permit dual-use development.').

omega_variable(
    kernel_alternative_tpnw_displacement,
    'Will the Treaty on the Prohibition of Nuclear Weapons (TPNW) functionally displace the NPT as the primary nuclear governance kernel, delegitimizing the grand-bargain reading by offering an alternative coordination frame without indefinite WS arsenal retention?',
    'TPNW entry-into-force and state adherence rate; emergence of TPNW-based security architecture among non-aligned states; formal NPT Review Conference rejection or non-consensus outcome driven by TPNW-identified NNWS.',
    'If TPNW displaces NPT as the governance frame, the grand-bargain reading loses its authority structure—the kernel (Articles IV-VI) becomes vestigial rather than lived. The constraint persists but transforms from a coordination mechanism to a zombie arrangement defended only by WS enforcement and existing alliance commitments. The abolitionist reading would become institutionally primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_alternative_tpnw_displacement, conceptual, 'Whether the TPNW will functionally displace the NPT as the governing kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, iaea_verification_authority_asymmetry).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, regional_security_dilemma_proliferation_pressure).

% DUAL FORMULATION NOTE:
% The npt_article_iv_vi_pairing kernel is decomposed into three constraint stories corresponding to three live readings: (1) grand_bargain (this file): Article IV-VI as reciprocal, enforceable obligations; NNWS restraint conditional on WS disarmament. (2) nonproliferation_primary: Article IV as primary, Article VI as aspirational; WS arsenal retention justified for security stability. (3) abolitionist: Article VI as disarmament mandate; Article IV illegitimate under perpetual-deterrence regime. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, different types. ε-invariance principle: these are not one constraint viewed from three angles—they are three structurally distinct constraints with different empirical referents. The grand-bargain reading has higher extractiveness (0.68) and suppression (0.72) because the reference constraint is the asymmetric restraint arrangement; the nonproliferation_primary reading has lower extractiveness (treats WS retention as stability public good) because its reference constraint is horizontal-proliferation prevention; the abolitionist reading has highest extractiveness (treats indefinite WS retention as illegitimate extraction) because its reference is the weapons-prohibition commitment. All three are linked via network.affects_constraints for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
