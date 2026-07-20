% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance Actively Defended by Incumbent Capital Holders
 *   domain: political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the beneficiary_maintained_reading of the
 *   market_naturalization kernel, which treats market dominance not as a
 *   spontaneous equilibrium but as a politically and legally defended
 *   structure sustained by identifiable incumbent capital holders. The
 *   constraint encompasses the full apparatus of barrier maintenance:
 *   lobbying, regulatory capture, patent thicketing, platform access control,
 *   strategic acquisition of challengers, and ideological production that
 *   reframes contingent power as natural efficiency. Sibling readings include
 *   lapsed_alternative_reading (dominance as requiring no active maintenance)
 *   and hybrid_reading (mixed lapsed and active elements). This reading
 *   asserts that dominance persists only through continuous investment in
 *   suppression and that the beneficiaries of that investment are
 *   structurally identifiable.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: Primary beneficiary and agenda setter (institutional/global/arbitrage) â controls rules and collects rents
 *   - excluded_competitors: Primary target (moderate/constrained) â bears extraction through blocked entry
 *   - consumer_public: Diffuse payer (organized/constrained) â bears extraction through supracompetitive prices
 *   - competition_authorities: Analytical observer (institutional/analytical) â investigates but faces capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.82).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.79).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance Actively Defended by Incumbent Capital Holders").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, 'dd06ba62-0749-4c79-b09d-0e23d44841e1').
narrative_ontology:cs_kernel_codification('dd06ba62-0749-4c79-b09d-0e23d44841e1', distributed).
narrative_ontology:cs_authority_grounding('dd06ba62-0749-4c79-b09d-0e23d44841e1', extraction).
narrative_ontology:cs_interpretation_layer_present('dd06ba62-0749-4c79-b09d-0e23d44841e1').
narrative_ontology:cs_reading_relation('dd06ba62-0749-4c79-b09d-0e23d44841e1', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('dd06ba62-0749-4c79-b09d-0e23d44841e1', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('dd06ba62-0749-4c79-b09d-0e23d44841e1', foundational, dominance_is_policed_not_spontaneous).
narrative_ontology:cs_axiom_status(dominance_is_policed_not_spontaneous, holdable).
narrative_ontology:cs_axiom_grounding('dd06ba62-0749-4c79-b09d-0e23d44841e1', dominance_is_policed_not_spontaneous, empirically_contingent).
narrative_ontology:cs_axiom('dd06ba62-0749-4c79-b09d-0e23d44841e1', foundational, market_power_requires_continuous_maintenance).
narrative_ontology:cs_axiom_status(market_power_requires_continuous_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('dd06ba62-0749-4c79-b09d-0e23d44841e1', market_power_requires_continuous_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('dd06ba62-0749-4c79-b09d-0e23d44841e1', market_outcome_as_natural_equilibrium).
narrative_ontology:cs_drift_state('dd06ba62-0749-4c79-b09d-0e23d44841e1', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dd06ba62-0749-4c79-b09d-0e23d44841e1', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, excluded_competitors).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumer_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control dominant positions across technology, pharmaceutical, financial, and platform markets. Actively defend dominance through lobbying, regulatory capture, patent thicketing, strategic acquisitions, and narrative control that presents consolidated outcomes as natural or efficient. Can redeploy capital globally to jurisdictions with weaker enforcement and maintain revolving-door relationships with regulatory agencies.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Startups and mid-size firms facing structural barriers including predatory pricing, exclusive dealing, platform access denial, and regulatory moats designed by incumbents. Innovation at the margins is tolerated until it threatens dominant positions, at which point acquisition or destruction follows. Entry is technically legal but economically irrational due to anticipated retaliation.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, excluded_competitors, payer,
    moderate, biographical, constrained, national).

% Pays supracompetitive prices and accepts reduced choice in consolidated markets. Diffuse costs prevent organized resistance; individual exit is undermined by network effects, switching costs, and regulatory structures. Occasionally mobilizes through consumer advocacy but faces sophisticated counter-mobilization by incumbent interests.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumer_public, payer,
    organized, biographical, constrained, national).

% Possess legal mandate to challenge market dominance but face resource asymmetries, revolving doors with incumbent firms, and ideological capture by frameworks treating dominance as presumptively efficient. Interventions are episodic and often arrive after market structure has consolidated, functioning more as retrospective justification than prevention.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates among incumbent capital holders to maintain stable oligopolistic returns and prevent destructive price competition, while presenting consolidated outcomes as efficient market clearance.
% TRANSFER_FUNCTION: Moves consumer surplus and excluded-competitor profits to incumbent capital holders through supracompetitive pricing, rent extraction, and foreclosure of innovative challengers.
% ABSENT_VOICES: Potential entrants who rationally anticipate suppression and never attempt market entry; consumer advocates and heterodox economists systematically marginalized by incumbent-funded think tanks, academic capture, and closed regulatory proceedings.
% DISAPPEARANCE_RATIONALE: Without active defense, barriers to entry would erode through competitive challenge and political reform, incumbent market shares would face sustained assault, prices would fall toward cost, and the distribution of surplus would shift dramatically from incumbents to consumers and entrants.
% FOUNDING_PROBLEM: Historical instability of unregulated markets and destructive competition that industrial consolidation was initially thought to remedy.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside incumbent-funded networks attest that while early industrial consolidation addressed genuine coordination failures in specific sectors, the problem was substantially resolved by the mid-twentieth century; heterodox political economists and legal historians attest the founding rationale has been dead for decades and now serves primarily as intellectual cover for extraction.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because incumbent returns are substantially decoupled from competitive risk and maintained by political expenditure rather than productive superiority. Suppression is high (0.79) because the constraint's persistence depends on active exclusion of rivals through legal, technical, and ideological means. Theater ratio is moderate (0.45) because incumbents must continuously perform innovation, consumer benefit, and free-market rhetoric while the underlying structure hardens against entry. Accessibility collapse (0.68) reflects that while alternatives are imaginable, they are structurally foreclosed by network effects, switching costs, and regulatory moats. Resistance (0.55) captures episodic antitrust mobilization and scholarly critique that has not yet overcome incumbent counter-mobilization.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent seat experiences the constraint as legitimate coordination that stabilizes investment and prevents destructive competition; the excluded-competitor and consumer seats experience the same structure as actively enforced extraction. The engine computes this divergence from structural data: the agenda setter has arbitrage-grade exit and collects rents, while payers face constrained or trapped exit and bear diffuse costs. The claimed type is tangled_rope rather than snare because there is a genuine coordination function among incumbents (stable oligopoly avoids mutually destructive price wars) even as the same structure asymmetrically extracts from non-incumbents.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders are declared beneficiaries with global scope and arbitrage exit, placing d near the full-beneficiary end (effectively subsidized by the constraint). Excluded competitors are declared victims with constrained exit, placing d near the full-target end. Consumer public is a victim with constrained exit but organized power, placing d at moderately high target. The engine will amplify effective extraction for excluded competitors and consumers while damping or inverting it for incumbents. No override is needed because the structural derivation chain produces accurate directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than rope prevents mistaking incumbent rent extraction for pure coordination: the rope classification would require identifiable beneficiaries and no victims, but excluded competitors and consumers are structurally harmed. Classifying it as tangled_rope rather than snare prevents ignoring the genuine coordination among incumbents (mutual recognition of spheres, avoidance of destructive competition) that would persist in some form even if extraction were reduced. The mandatrophy risk here is that the constraint might be read as a lapsed structure (piton or mountain) when in fact it requires active, continuous defense; the measurements show rising extractiveness and suppression over the interval, contradicting any lapsed-reading classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the market_naturalization kernel decompose into structurally distinct constraints under different readings, or do the readings represent perspectival framings of the same mechanism?',
    'Cross-reading comparison: if the lapsed_alternative_reading shows negligible extractiveness and no active enforcement while this reading shows high extractiveness and active suppression, the kernel decomposes into distinct constraints; if metrics converge, the divergence is perspectival.',
    'If structurally distinct, the kernel decomposition is validated and each reading warrants its own constraint story; if perspectival, the readings should collapse into a single constraint with observer-indexed classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Committer position within the market_naturalization kernel and sibling-reading structural relationship').

omega_variable(
    sectoral_scope_of_defense,
    'Does active defense of dominance characterize all concentrated markets, or only specific sectors with particular barrier technologies such as network effects, intellectual property regimes, and regulatory capture?',
    'Sectoral comparative analysis measuring enforcement expenditure, entry rates, and profit persistence across industries with different technological and regulatory profiles.',
    'If sector-specific, the constraint''s scope should narrow and its classification may shift from a universal tangled_rope to a family of sector-specific constraints; if universal, the current framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_scope_of_defense, empirical, 'Whether active defense is universal or sector-specific').

omega_variable(
    ideological_vs_structural_suppression,
    'Is the suppression of alternatives primarily structural (legal barriers, enforcement costs, platform control) or ideological (the naturalization narrative internalized by regulators, courts, and consumers)?',
    'Natural experiment across jurisdictions with similar legal structures but weaker neoclassical economic ideology; measure entry rates, enforcement outcomes, and judicial reasoning.',
    'If primarily ideological, directionality for regulators and consumers shifts toward identity_locked and effective extraction rises; if structural, suppression is external and exit options are higher than they appear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_vs_structural_suppression, conceptual, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_nat_ben_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(market_nat_ben_tr_t6, market_naturalization__beneficiary_maintained_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(market_nat_ben_tr_t12, market_naturalization__beneficiary_maintained_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(market_nat_ben_tr_t18, market_naturalization__beneficiary_maintained_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(market_nat_ben_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(market_nat_ben_tr_t30, market_naturalization__beneficiary_maintained_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(market_nat_ben_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(market_nat_ben_be_t6, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(market_nat_ben_be_t12, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(market_nat_ben_be_t18, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 18, 0.73).
narrative_ontology:measurement(market_nat_ben_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(market_nat_ben_be_t30, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(market_nat_ben_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(market_nat_ben_su_t6, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(market_nat_ben_su_t12, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(market_nat_ben_su_t18, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(market_nat_ben_su_t24, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(market_nat_ben_su_t30, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 30, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
