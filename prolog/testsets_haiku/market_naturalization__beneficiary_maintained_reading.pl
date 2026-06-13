% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance Actively Defended by Incumbent Capital
 *   domain: political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'market_naturalization': the reading that market dominance is actively
 *   defended by incumbent capital holders who have invested in maintaining
 *   barriers, suppressing alternatives, and capturing regulatory processes.
 *   It assumes capital holders are the primary active agents sustaining
 *   dominance structure. Sibling readings (lapsed_alternative_reading,
 *   hybrid_reading) locate agency and causation differently: lapsed reads
 *   dominance as persisting mostly through inertia absent active defense;
 *   hybrid reads dominance as a combination of inherited structural
 *   advantages plus ongoing maintenance. This reading treats dominance as a
 *   sustained snare—high extractiveness, high suppression, identifiable
 *   beneficiary class—in contrast to lapsed (which would be a piton with
 *   diminishing theater) and hybrid (which would be tangled_rope mixing
 *   structural coordination with active extraction). The claim/metric
 *   alignment is intentional here: this reading CLAIMS snare and the metrics
 *   DESCRIBE snare; the reading is that the beneficiary class is actively
 *   defending the structure, not passively collecting from it.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: Institutional power, long time horizon, arbitrage-level exit; they set the agenda on dominance maintenance and collect rents. Structurally beneficiary and agenda-setter.
 *   - potential_market_entrants: Powerless, trapped by capital requirements and IP barriers; they would object if visible but are structurally excluded by the barrier machinery. Primary victims.
 *   - labor_suppliers: Moderate power but constrained by monopsony; they lose through suppressed wage competition. Secondary victims.
 *   - smaller_competitors: Moderate power but constrained by acquisition threat; they lose through predatory pricing and exclusive dealing. Secondary victims.
 *   - consumers: Organized, mobile, but constrained to incumbent-curated options; they benefit from coordination (one trusted brand, integrated services) and pay through higher prices than genuine competition would support.
 *   - antitrust_authorities: Observers who see the structure but lack political mandate or doctrinal framework to dismantle it. They enforce at margins but don't restructure dominance.
 *   - economic_theorists: Excluded from enforcement discourse; they would argue for treating dominance as a policy choice rather than market outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.81).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, snare).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance Actively Defended by Incumbent Capital").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, 'c3c26bf6-3a31-441d-84ee-edf6e67bb8d0').
narrative_ontology:cs_kernel_codification('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', distributed).
narrative_ontology:cs_authority_grounding('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', extraction).
narrative_ontology:cs_reading_relation('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', foundational, incumbent_capital_actively_defends_dominance).
narrative_ontology:cs_axiom_status(incumbent_capital_actively_defends_dominance, holdable).
narrative_ontology:cs_axiom_grounding('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', incumbent_capital_actively_defends_dominance, empirically_contingent).
narrative_ontology:cs_axiom('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', foundational, dominance_extraction_exceeds_coordination_cost).
narrative_ontology:cs_axiom_status(dominance_extraction_exceeds_coordination_cost, holdable).
narrative_ontology:cs_axiom_grounding('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', dominance_extraction_exceeds_coordination_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', competitive_markets_absent_active_defense).
narrative_ontology:cs_drift_state('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', contemporary_consolidated_markets, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c3c26bf6-3a31-441d-84ee-edf6e67bb8d0', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, potential_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, labor_suppliers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, smaller_competitors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) and rising over the 44-year measurement interval (0.42 in 1980 to 0.78 in 2024) because rents increase as dominance consolidates and competitive pressure declines. Suppression is even higher (0.81) because the constraint's persistence depends on actively blocking alternatives through legal action, acquisition, pricing strategy, and regulatory capture—not on participant preference. Theater is moderate (0.42) because the constraint is partly justified by genuine coordination benefits (brand trust, scale economies) but increasingly defended through mechanisms disconnected from consumer value (patent thicketing, exclusive dealing). The upward trajectory of all three metrics over 44 years indicates a constraint whose extraction and suppression intensify as capital consolidates—the constraint is not stabilizing but strengthening. The measurement grid is shared across all three metrics at each time point, so comparison is directly valid. Accessibility_collapse is moderate (0.48) because alternatives to incumbent-dominated markets DO exist in principle (open entry is logically possible) but become increasingly difficult in practice; resistance is high (0.72) because firms and workers and potential competitors are constantly pushing back against dominance, even if they lack the power to restructure it unilaterally. This reading treats the constraint as a snare: extraction requires active enforcement, beneficiary is identifiable, and persistence depends on suppressing real alternatives, not on participant rational preference.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent_capital_holders seat (institutional, arbitrage exit, agenda-setter) should perceive this constraint as rope—genuine coordination they've built and maintained, rightly rewarded through dominance. The potential_market_entrants seat (powerless, trapped) should perceive snare—a closed door maintained for others' benefit. The antitrust_authorities seat (observer, analytical) should perceive the constraint's actual structure: coordination function is real but increasingly secondary to rent defense; the dominance structure is maintained by incumbent action but justified through coordination rhetoric. The engine computes these seat-specific perceptions from the structural data. This story does not reconcile them—the gap IS the measurement the constraint story exists to produce.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent_capital_holders seat should compute high d (target-ish, despite being the beneficiary, because their dominance is actively defended by their own capital investment—they are the ones enforcing the constraint, making them the structural agenda_setter). Potential entrants should compute very high d (full targets; they are structurally excluded and pay in opportunity cost). Labor suppliers should compute moderately high d (targets of wage suppression through monopsony). Consumers should compute near-symmetric d because they benefit from coordination but pay through reduced choice and higher prices—the benefit and cost are both real. The directionality derivation flow: beneficiary+victim declarations feed into the beneficiary/target computation; powerless agents with trapped exit and victim declaration → high d; institutional agents with arbitrage exit and beneficiary+agenda_setter roles → lower d but still elevated because they are actively defending the constraint (the agenda-setter role drives up d; beneficiary status only partially offsets it). No overrides are used here; the structural declarations are sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (how to incentivize risky capital investment through protection from imitation) is ALIVE in the beneficiary-maintained reading because incumbents claim that continuous innovation requires continuous dominance defense. However, the corroboration comes exclusively from incumbent firms—economic historians and competition scholars attest the founding problem is substantially SOLVED by existing IP law, first-mover advantages, and patent/copyright systems; the measured persistence of dominance is rent maintenance, not incentive for innovation. This is a mandatrophy signal: the constraint persists but the founding problem it claims to solve is dead for outside corroborators. The hybrid_reading would treat this differently (lapsed structural elements still solving something), and the lapsed_alternative_reading would treat mandatrophy as the core insight (dominance persists despite its founding problem being dead). This reading acknowledges mandatrophy but interprets it as evidence that incumbent capital has successfully converted an incentive structure into a permanent rent stream. Antitrust intervention would need to sever that conversion, which explains the high suppression score—defending against intervention is part of incumbent dominance maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_beneficiary_vs_lapsed,
    'Is market dominance actively maintained by incumbent capital holders'' ongoing investment and enforcement (this reading), or is it a lapsed closure persisting mostly through inertia and historical accident (lapsed_alternative_reading)?',
    'Empirical measurement of incumbent defensive expenditure: capital spend on IP litigation, predatory pricing instances, acquisition of rivals, regulatory lobbying, and innovation redirected to barrier-building vs. consumer-value. Comparison to counterfactual: what would market structure be if these expenditures ceased? If structure remains stable without active defense, the reading is lapsed; if rapid reorganization follows, this reading is correct.',
    'If lapsed, the constraint might be amenable to simple regulatory withdrawal (no active defense to overcome). If actively maintained, fixing requires either destroying the accumulated capital advantages or constantly constraining incumbent defensive action. This reading assumes active maintenance; the corpus measures the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_beneficiary_vs_lapsed, empirical, 'Whether incumbent dominance is actively maintained or merely lapsed.').

omega_variable(
    reading_contest_beneficiary_vs_hybrid,
    'Does incumbent dominance arise purely from active defense by capital holders (this reading), or is it a hybrid of lapsed structural elements inherited from earlier regulation plus ongoing maintenance by incumbents (hybrid_reading)?',
    'Historical decomposition: identify which features of incumbent dominance come from formal legal structures (IP law, patent terms, regulatory carve-outs, licensing requirements) that persist regardless of incumbent action, vs. which features require incumbent capital to maintain (litigation, pricing strategy, acquisition, innovation direction). If legal structures alone would sustain dominance, the reading is hybrid; if dominance requires ongoing active defense, this reading is correct.',
    'This reading asserts capital holders are the primary active agents defending dominance. The hybrid reading claims the legal and structural framework does most of the work. The policy implication differs: this reading suggests antitrust enforcement can disrupt dominance by preventing active defense; the hybrid reading suggests legal/structural reform is necessary regardless of incumbent action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_beneficiary_vs_hybrid, empirical, 'Whether active incumbent defense or hybrid structural + active elements sustain dominance.').

omega_variable(
    moral_status_of_rent_defense,
    'Is incumbent capital''s defense of market dominance morally justified as a rightful protection of investment returns, or is it morally unjustified rent-seeking that exploits legal structures to extract above-competitive returns?',
    'Normative philosophical analysis (not empirical resolution): does the right to enjoy returns on invested capital extend to preventing new competition through barriers, or does it terminate at maintaining competitive advantages through superior service? Different philosophical frameworks (property-rights liberalism vs. competitive-equality republicanism) resolve this differently. No empirical fact settles it.',
    'If justified, enforcement against incumbent defense is seen as illegitimate expropriation; if unjustified, enforcement is seen as correcting a structural illegitimacy. This reading presents defense as unjustified rent-seeking; sibling readings frame it as rightful capital protection. The corpus does not decide moral status but records the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_of_rent_defense, preference, 'Moral justification of incumbent capital''s dominance defense.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of market alternatives structural (external barriers: capital requirements, legal obstacles, infrastructure control) or partially internalized (potential entrants believe dominance is natural/inevitable and so don''t attempt entry, or believe they cannot win)?',
    'Post-barrier empirical tests: after removing capital requirements through state funding, providing patent licensing, or reducing legal barriers, do new entrants emerge? If yes, suppression was substantially structural; if no, suppression is partially internalized (entrants continue to believe competition is futile even with barriers removed). Observation of actual entry attempts when barriers are reduced is the diagnostic.',
    'Structural suppression is addressed by removing barriers; internalized suppression persists after barrier removal and requires consciousness-raising or cultural shift. This reading measures 0.81 suppression at interval end; the mechanism affects what remedies work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is structural or partially internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_naturalization__beneficiary_maintained_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(mark_tr_t1995, market_naturalization__beneficiary_maintained_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(mark_tr_t2005, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(mark_tr_t2015, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(mark_tr_t2024, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(mark_be_t1995, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(mark_be_t2005, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(mark_be_t2015, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement(mark_be_t2024, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(mark_su_t1995, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(mark_su_t2005, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(mark_su_t2015, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(mark_su_t2024, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2024, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, labor_monopsony_enforcement).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, patent_thicket_defense).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, regulatory_capture_institutional).

% DUAL FORMULATION NOTE:
% This story is one of three decomposed readings of the kernel 'market_naturalization.' The sibling readings (lapsed_alternative_reading, hybrid_reading) model alternative explanations for persistent incumbent dominance. This reading (beneficiary_maintained_reading) asserts incumbent capital is the primary active agent. The network edges indicate structural influence: all three kernel readings affect institutional constraints like labor monopsony and patent defense, which are downstream mechanisms through which dominance is enforced. The three kernel readings should be cross-read to understand the contested boundary between active incumbent defense and structural inertia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
