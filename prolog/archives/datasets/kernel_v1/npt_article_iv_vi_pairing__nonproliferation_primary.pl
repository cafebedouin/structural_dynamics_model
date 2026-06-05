% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV-VI Pairing (Nonproliferation Primary Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (1968) pairs two articles with
 *   fundamentally asymmetric legal force: Article IV guarantees non-nuclear
 *   weapon states (NNWS) an 'inalienable right' to peaceful nuclear
 *   technology while subjecting that right to IAEA verification; Article VI
 *   commits all parties to 'negotiate in good faith' on disarmament timelines
 *   but contains no enforcement mechanism, verification procedure, or binding
 *   deadline. The nonproliferation_primary reading instantiates one specific
 *   interpretation of this pairing: the two-tier order is structural and
 *   permanent. Article IV enforcement (horizontal nonproliferation
 *   prevention) is the binding constraint; Article VI aspiration (vertical
 *   disarmament pressure) is a political gesture without treaty force. This
 *   reading prioritizes prevention of weapons spread over elimination of
 *   existing arsenals. Under this reading, NWS arsenals are categorically
 *   excluded from the treaty's enforcement architecture, and NNWS bear
 *   permanent restraint obligations while disarmament timeline remains
 *   indefinitely open. The constraint exhibits high theater (IAEA inspections
 *   perform real verification work but apply asymmetrically) and rising
 *   extraction (as civilian nuclear technology diffuses, the cost of Article
 *   IV compliance for NNWS increases while NWS disarmament obligations remain
 *   legally toothless). This reading competes with two others: the
 *   grand_bargain reading (which interprets the pairing as a tit-for-tat
 *   exchange with an expectation that NWS will eventually disarm within a
 *   bounded timeframe, possibly 25-50 years) and the abolitionist reading
 *   (which holds that Article VI creates a binding obligation that
 *   non-performance constitutes treaty breach and legal remedy). The
 *   nonproliferation_primary reading produces the most stable and lowest-cost
 *   outcome for NWS, explaining why it has become institutionally dominant
 *   since the 1995 NPT indefinite extension.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (P5 + additional NWS): Primary beneficiaries (institutional/arbitrage) — maintain arsenals without treaty constraints; Article VI non-enforcement costs them nothing
 *   - Non-Nuclear Weapon States (majority of treaty parties): Primary victims (powerless/trapped) — accept indefinite restraint on nuclear capacity; Article IV verification costs them sovereignty and resources
 *   - Disarmament & Civil Society Advocates: Secondary victims (organized/constrained) — organized actors whose pressure for Article VI enforcement is neutralized by institutional dominance of nonproliferation_primary reading
 *   - IAEA & Treaty Administration: Institutional actor (institutional/arbitrage) — maintains verification apparatus for Article IV while having zero mandate for Article VI; sustains self through institutional inertia
 *   - Threshold & Regional Powers: Mixed victims (moderate/constrained) — face coordination benefit (alliance security assurances) AND extraction (technology restrictions) with high exit costs but not total suppression
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the two-tier structure as immutable law of security competition rather than contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.58).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV-VI Pairing (Nonproliferation Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '50d78761-7a5b-4289-9fd2-0f6c6590e927').
narrative_ontology:cs_kernel_codification('50d78761-7a5b-4289-9fd2-0f6c6590e927', formalized).
narrative_ontology:cs_authority_grounding('50d78761-7a5b-4289-9fd2-0f6c6590e927', extraction).
narrative_ontology:cs_interpretation_layer_present('50d78761-7a5b-4289-9fd2-0f6c6590e927').
narrative_ontology:cs_reading_relation('50d78761-7a5b-4289-9fd2-0f6c6590e927', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('50d78761-7a5b-4289-9fd2-0f6c6590e927', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('50d78761-7a5b-4289-9fd2-0f6c6590e927', foundational, nws_arsenal_maintenance_permanent).
narrative_ontology:cs_axiom_status(nws_arsenal_maintenance_permanent, holdable).
narrative_ontology:cs_axiom_grounding('50d78761-7a5b-4289-9fd2-0f6c6590e927', nws_arsenal_maintenance_permanent, conventional).
narrative_ontology:cs_axiom('50d78761-7a5b-4289-9fd2-0f6c6590e927', foundational, article_vi_non_justiciable_aspiration).
narrative_ontology:cs_axiom_status(article_vi_non_justiciable_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('50d78761-7a5b-4289-9fd2-0f6c6590e927', article_vi_non_justiciable_aspiration, conventional).
narrative_ontology:cs_reference_frame('50d78761-7a5b-4289-9fd2-0f6c6590e927', nws_security_interest_perpetual_arsenal_maintenance).
narrative_ontology:cs_drift_state('50d78761-7a5b-4289-9fd2-0f6c6590e927', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50d78761-7a5b-4289-9fd2-0f6c6590e927', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, security_alliance_leaders).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocates).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, global_nuclear_risk_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR WEAPON STATES (SNARE) — Permanently locked into restraint. Article IV ratification requires acceptance of IAEA inspection (costs, sovereignty limitation); Article VI is aspirational text with zero enforcement mechanism. Exit options: none without abandoning treaty (security isolation) or acquiring weapons (international isolation). The constraint extracts compliance while offering no reciprocal disarmament timeline.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THRESHOLD STATES & REGIONAL POWERS (TANGLED ROPE) — Face coordination benefit (NWS security assurances reduce regional arms races) AND asymmetric extraction (Article IV restrictions limit civilian nuclear capacity, Article VI non-enforcement creates credibility gap). Exit cost is high but not total: some states pursue nuclear capacity despite treaty (Iran, North Korea), others invest in conventional deterrence. Mixed structural relationship.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NUCLEAR WEAPON STATES (ROPE) — Experience the constraint as pure coordination. Article IV locks non-competitors into restraint; Article VI is unenforceable so arsenals remain unconstrained. NWS benefit from horizontal proliferation prevention without bearing vertical disarmament burden. This is a coordination mechanism that solves the legitimate problem of preventing weapons spread while allowing NWS to maintain security arsenals.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISARMAMENT & CIVIL SOCIETY ORGANIZATIONS (TANGLED ROPE) — Organized actors that see both coordination (the treaty prevents some proliferation) and extraction (Article VI aspiration creates false hope, neutralizing disarmament pressure). The constraint coordinates horizontal nonproliferation AND simultaneously extracts legitimacy from the disarmament movement by appearing to address vertical disarmament while enforcing no actual reduction. Exit cost is moderate: can exit the treaty consensus but sacrifice advocacy platform.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TREATY ADMINISTRATION & IAEA (PITON) — Institutional actor maintaining the verification theater. The IAEA conducts inspections (Article IV enforcement) while having zero mandate or capacity to verify Article VI compliance. The entire institutional apparatus is performing a function (monitoring nonproliferation) while the core asymmetry (NWS arsenals excluded) persists unchallenged. Theater ratio high: the inspections are real work, but they apply only to half the bargain. The institution sustains itself through inertia — the NPT regime is the default international framework for nuclear governance.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REALIST SECURITY STRUCTURALISM (MOUNTAIN) — From a civilizational scale, the two-tier order (NWS with arsenals, NNWS without) is an immutable feature of security competition: rational actors cannot commit to disarmament under anarchy; Article VI aspiration reflects this structural constraint rather than legal obligation. However, the engine will identify this as a false summit: the mountain framing naturalizes what is a contingent political choice (excluding NWS arsenals from treaty enforcement) rather than a law of physics or logic.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__nonproliferation_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, TR),
    TR >= 0.70.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts asymmetric restraint from NNWS while excluding NWS arsenals from treaty enforcement. This is substantial but not maximal (true snare would be 0.72+) because (1) horizontal nonproliferation does solve a genuine coordination problem with global security benefit, and (2) NNWS retain limited exit options (some successfully pursue weapons, others build conventional deterrence). The rising trajectory (0.42 → 0.58 over 56 years) reflects that as civilian nuclear technology diffuses and becomes cheaper, the cost of Article IV compliance for NNWS increases (safeguards are more intrusive, more expensive, more sovereignty-limiting on larger programs) while NWS disarmament obligations remain legally empty — the asymmetry deepens over time. Suppression (0.72): High. NNWS face multiple barriers to exit: international isolation if they withdraw, security vulnerability if they abandon the treaty's alliance security assurances, resource costs of sanctions if they pursue weapons, and epistemic lock-in (disarmament narratives make weapons acquisition culturally illegitimate for most NNWS). The rising trajectory (0.55 → 0.72) reflects intensifying suppression as the treaty regime hardens and enforcement mechanisms for Article IV deepen while Article VI remains permanently hollow. Theater ratio (0.65): Moderate-high. IAEA inspections (Article IV enforcement) are real technical work with genuine verification function — this is not purely performative. However, the theater is high because the inspections apply asymmetrically (NNWS only) and the core asymmetry (NWS arsenal exclusion) is maintained through institutional choreography and legal reinterpretation, not through any enforcement mechanism. The rising trajectory (0.45 → 0.65) reflects increasing performative content as the gap between Article IV enforcement and Article VI non-enforcement becomes more visible and requires more institutional justification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival disagreement. NWS see it as Rope (pure coordination; prevents weapons spread without constraining their arsenals). NNWS see it as Snare (permanent restraint with zero reciprocal disarmament obligation). Threshold states see Tangled Rope (coordination benefit from reduced regional arms races alongside extraction from technology restrictions). Civil society sees Tangled Rope (coordination benefit from prevented proliferation alongside extraction from false hope on disarmament). The IAEA sees Piton (their verification work is real but applies only half the treaty; they maintain the institution through inertia). The analytical observer at civilizational scale risks Mountain (seeing the two-tier structure as inevitable under anarchy), but this is a false summit: the structure is a 1968 political choice, not a law of physics. The engine's false summit detector will flag this.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural relationship to the constraint. NWS beneficiaries with arbitrage exits (can withdraw, can maintain arsenals, can form alternative security arrangements) have low d → low f(d) → negative or minimal χ. NNWS victims with trapped exits (cannot withdraw without isolation, cannot pursue weapons without sanctions, cannot refuse inspection without treaty violation) have high d → high f(d) → high χ. Threshold states and organized advocates occupy middle positions with constrained exits — they can exit at high cost but not without significant penalty. The directionality asymmetry is the core engine of the constraint: structural position determines experienced extractiveness, which produces the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint successfully classifies as Tangled Rope at multiple perspectives (NNWS, threshold states, civil society, disarmament organizations) and this classification withstands cross-examination. The core mandatrophy question — does the constraint solve a genuine coordination problem (preventing horizontal proliferation) while imposing asymmetric extraction (Article IV binding, Article VI non-binding)? — has a clear answer: YES. The coordination function is real: the NPT has correlated with slower proliferation rates and prevented estimated 20-30 weapons-capable states from pursuing weapons programs. The extraction is also real: NNWS bear verification costs, sovereignty limitations, technology restrictions, and restraint obligations that NWS do not bear. The constraint is not 'coordination disguised as extraction' (which would be Snare) nor 'extraction disguised as coordination' (which would also be Snare); it is genuine hybrid where coordination and extraction coexist and are structurally coupled. The coupling is the critical insight: Article IV enforcement (horizontal nonproliferation) is causally dependent on Article VI weakness (vertical disarmament non-enforcement). NWS will not accept intrusive verification of their own arsenals; therefore Article VI must remain aspirational. NNWS will only accept restraint (Article IV) if they receive something in exchange; therefore Article VI must exist as a promise, even if unenforceable. The two-tier structure is not accidental — it is the price NWS extracted for participating in a nonproliferation regime at all. The mandatrophy resolution confirms that this is the legitimate reading of the constraint's structure: it is Tangled Rope, not Snare. However, the false summit perspective (the realist mountain) must be explicitly flagged as a naturalization of what is actually a contingent political choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforceability_threshold,
    'What constitutes ''good faith'' disarmament under Article VI? Is any treaty timeline or verification mechanism enforceable, or is Article VI aspirational by design?',
    'Analysis of negotiation records (Uruguay 1968) + state practice in claims enforcement; comparison with binding vs aspirational treaty language in parallel instruments (CTBT, CWC verification mechanisms)',
    'If Article VI is enforceable: constraint reclassifies to snare from NWS perspective (they bear compliance burden). If aspirational by design: snare from NNWS perspective confirmed (extraction without reciprocity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_enforceability_threshold, conceptual, 'Whether Article VI imposes enforceable disarmament obligation or is aspirational language').

omega_variable(
    horizontal_vs_vertical_proliferation_coupling,
    'Does Article IV enforcement (preventing horizontal proliferation) causally depend on Article VI weakness (vertical disarmament non-enforcement), or could the treaty enforce both simultaneously?',
    'Counterfactual analysis: model NPT compliance if Article VI carried inspection provisions, verification timelines, and enforcement teeth equivalent to Article IV. Historical analysis of negotiation tradeoffs: what explicit quid pro quo was offered to NNWS for accepting Article IV restrictions?',
    'If coupled: the two-tier extraction is structural; weakening Article VI suppression is impossible without renegotiating Article IV. If decoupled: the asymmetry is a political choice, not a necessity; the constraint is reformable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_vs_vertical_proliferation_coupling, empirical, 'Causal coupling between horizontal proliferation prevention and vertical disarmament non-enforcement').

omega_variable(
    nnws_exit_capacity_evolution,
    'As civilian nuclear technology diffuses and enrichment/reprocessing become cheaper, do NNWS exit costs for NPT withdrawal decline, changing the constraint''s suppression topology?',
    'Trend analysis of NNWS withdrawal threats and actual withdrawals (North Korea 2003, Iran threats, others); cost-benefit modeling of NPT compliance vs independent nuclear capacity as technology costs decline; correlation between technology diffusion and treaty stress',
    'If exit costs decline: suppression metric should be revised downward; constraint reclassifies toward lower-suppression types (tangled_rope toward rope, snare toward tangled_rope) as technology democratizes. If exit costs remain high: suppression persistent; extraction mechanism remains structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_exit_capacity_evolution, empirical, 'Whether declining technology costs reduce NNWS exit suppression from NPT').

omega_variable(
    reading_committer_ambiguity,
    'This reading (nonproliferation_primary) naturalizes NWS arsenal exclusion as structural necessity. Do the sibling readings (grand_bargain, abolitionist) merely disagree about implementation, or do they hold fundamentally incompatible premises about the treaty''s binding force?',
    'Analysis of sibling reading axioms: if grand_bargain axiom is ''temporal delay in disarmament is acceptable'' and this reading''s axiom is ''permanent arsenal maintenance is acceptable'', these coexist. If abolitionist axiom is ''all weapon states must disarm on fixed timeline'' and this reading''s axiom is ''Article VI is non-justiciable'', these foreclose each other.',
    'If foreclosure: kernel structure is bipolar (choose one reading''s framework). If coexistence: kernel structure is multipolar (competing readings held by different parties). Affects downstream political feasibility analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Foreclosure vs coexistence between nonproliferation_primary and sibling readings').

omega_variable(
    mandatrophy_extraction_vs_coordination,
    'The constraint solves the genuine coordination problem of preventing weapons spread (horizontal nonproliferation). Does the extraction (asymmetric burden on NNWS) exceed what coordination requires, or is it the necessary price for NWS participation?',
    'Counterfactual modeling: what minimal NWS incentive structure would support horizontal nonproliferation without Article IV restrictions on NNWS civilian capacity? Empirical comparison: correlation between NWS arsenal size and their willingness to enforce Article IV vs Article VI.',
    'If extraction exceeds coordination cost: mandatrophy confirmed; constraint is partly extractive predation disguised as coordination. If extraction equals NWS participation cost: coordination is efficient; mandatrophy resolution supports tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_extraction_vs_coordination, empirical, 'Whether Article IV-VI extraction exceeds coordination necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_a4_a6_theater_1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.45).
narrative_ontology:measurement(npt_a4_a6_theater_1995, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 27, 0.58).
narrative_ontology:measurement(npt_a4_a6_theater_2024, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 56, 0.65).

% Extraction over time
narrative_ontology:measurement(npt_a4_a6_extractiveness_1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_a4_a6_extractiveness_1995, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 27, 0.52).
narrative_ontology:measurement(npt_a4_a6_extractiveness_2024, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 56, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_a4_a6_suppression_1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(npt_a4_a6_suppression_1995, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 27, 0.68).
narrative_ontology:measurement(npt_a4_a6_suppression_2024, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 56, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_asymmetry).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, uranium_enrichment_technology_diffusion).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, alliance_extended_deterrence).

% DUAL FORMULATION NOTE:
% The NPT article pairing decomposes into three structurally distinct constraints: (1) Article IV enforcement (horizontal nonproliferation prevention, ε=0.35, Rope from most perspectives), (2) Article VI aspiration (vertical disarmament pressure, ε=0.72, Snare from NNWS perspective), and (3) the article pairing itself (the two-tier order structure, ε=0.58, Tangled Rope). This story addresses the pairing constraint. The individual articles are separate constraints with their own ε values and network linkages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, institutional, 0.08).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
