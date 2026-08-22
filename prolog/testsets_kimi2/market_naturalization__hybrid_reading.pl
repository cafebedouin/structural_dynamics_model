% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance as Hybrid of Lapsed and Actively Maintained Closure
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the
 *   market_naturalization kernel: market dominance is neither purely a lapsed
 *   historical closure nor purely actively defended extraction, but a durable
 *   combination of both. Some agents collect rents from lapsed structural
 *   advantages such as regulatory grandfathering and irreversible network
 *   effects that require no current maintenance; others actively suppress
 *   alternatives through lobbying, litigation, and strategic exclusion. The
 *   constraint's extractiveness varies by sector and historical moment,
 *   producing a mixed beneficiary-and-victim structure that resists
 *   purification into either rope or snare.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders (agenda_setter/powerful/arbitrage) â actively maintain dominance through political and legal investment
 *   - legacy_rent_recipients (beneficiary/moderate/constrained) â collect returns from historically lapsed advantages without enforcing closure
 *   - excluded_challengers (payer/powerless/constrained) â blocked by active and passive barriers, bearing foregone opportunity costs
 *   - captive_suppliers (payer/moderate/constrained) â locked into incumbent ecosystems through sunk costs and exclusivity
 *   - antitrust_authorities (observer/institutional/analytical) â intermittently investigate and regulate dominance with mixed success
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.55).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.6).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance as Hybrid of Lapsed and Actively Maintained Closure").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '9724d16c-9c81-43c1-bfd1-b9fb34c74cc3').
narrative_ontology:cs_kernel_codification('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', distributed).
narrative_ontology:cs_authority_grounding('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', diffuse_epistemic).
narrative_ontology:cs_reading_relation('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', foundational, market_dominance_is_hybrid).
narrative_ontology:cs_axiom_status(market_dominance_is_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', market_dominance_is_hybrid, empirically_contingent).
narrative_ontology:cs_axiom('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', secondary, lapsed_and_active_are_co_constitutive).
narrative_ontology:cs_axiom_status(lapsed_and_active_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', lapsed_and_active_are_co_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', institutional_embeddedness).
narrative_ontology:cs_drift_state('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', neoliberal_hegemony_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9724d16c-9c81-43c1-bfd1-b9fb34c74cc3', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, legacy_rent_recipients).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, excluded_challengers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, captive_suppliers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control large shares of concentrated markets and invest in maintaining dominance through lobbying, litigation, mergers, and exclusivity agreements. They capture super-normal profits and can redeploy capital across jurisdictions, but benefit from localized or sectoral closure.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_capital_holders, agenda_setter,
    powerful, generational, arbitrage, global).

% Hold valuable market positions secured by historical accidents such as early licensing, grandfathered regulations, or irreversible network adoption. They collect ongoing returns without investing in active exclusion and cannot easily transfer their advantage to other domains.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, legacy_rent_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Entrepreneurs and firms attempting to enter markets dominated by incumbents. They face patent thickets, platform exclusivity, predatory pricing, or insurmountable lapsed barriers like entrenched user habits and relationship lock-in, bearing the cost of foregone opportunity.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, excluded_challengers, payer,
    powerless, biographical, constrained, national).

% Small and mid-sized firms that must transact with dominant buyers. They accept unfavorable contractual terms because the incumbent controls the only viable distribution channel or because switching costs and relationship-specific investments make exit economically damaging.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, captive_suppliers, payer,
    moderate, biographical, constrained, regional).

% Public agencies tasked with preserving competition. They evaluate mergers and conduct cases but operate with limited resources and face political pressure not to disrupt nationally championed firms, producing inconsistent enforcement across jurisdictions and eras.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, antitrust_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In sectors with genuine scale economies and network effects, dominance can coordinate dispersed actors onto a single efficient standard, reducing transaction costs and fragmented investment. Under the hybrid reading, this coordination function is partially real but entangled with historically lapsed closures that no longer solve an active coordination problem.
% TRANSFER_FUNCTION: Moves surplus from excluded challengers and captive suppliers to incumbent capital holders and legacy rent recipients, through prices above competitive levels, terms below competitive alternatives, and foregone innovation rents.
% ABSENT_VOICES: Potential entrants who were never aware of the foregone market opportunity because the dominance structure preempted their formation; workers in displaced sectors who lack voice in antitrust proceedings; and alternative organizational forms such as cooperatives or public options that are screened out by the naturalization narrative.
% DISAPPEARANCE_RATIONALE: If the dominance arrangement vanished overnight, lapsed barriers would not immediately reconstitute; markets would experience entry surges, price compression, and supply-chain reorganization. Some scale-dependent sectors might fragment temporarily before reorganizing, but the overall distribution of surplus and market structure would shift substantially.
% FOUNDING_PROBLEM: Historical market fragmentation with incompatible standards, high search costs, and underinvestment in shared infrastructure that justified single-firm or oligopoly coordination.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and heterodox political economists attest that foundational fragmentation problems were resolved decades ago in many sectors, making the original coordination rationale at best partially live. Incumbent-funded industry associations and neoclassical efficiency theorists contest this, asserting the problem remains active. Independent comparative-industry studies from outside the beneficiary set support the atrophy reading.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the hybrid reading divides the constraint into genuinely lapsed components with lower marginal extraction and actively maintained components with higher extraction. Suppression is moderate-high (0.60): while some alternatives have simply lapsed, others are actively suppressed, and the combined effect is a substantial closure of opportunity. Theater ratio is moderate (0.45): incumbents perform competitive rhetoric and innovation signaling while maintaining exclusionary structures, and legacy beneficiaries treat their advantages as earned rather than historical windfalls. Accessibility collapse is substantial (0.65) because dominance is culturally naturalized, making alternatives hard to imagine or reconstruct. Resistance is moderate (0.50): challengers and authorities push back intermittently, but incumbents divide and dilute opposition while legacy beneficiaries remain politically invisible.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent_capital_holders seat experiences the constraint as necessary market order and efficient coordination; the excluded_challengers seat experiences it as an artificial barrier sustained by political investment. The legacy_rent_recipients may not perceive the constraint as a constraint at all, treating their returns as the natural background of a well-functioning market. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   incumbent_capital_holders are directional beneficiaries with low d: they subsidize and enforce the constraint and collect the active extraction. legacy_rent_recipients are also beneficiaries but with constrained exit and moderate power, sitting at moderate-low d. excluded_challengers are directional targets with high d: they face both active and lapsed barriers and have the least power. captive_suppliers are moderate-high d targets whose exit is blocked by relationship-specific investments rather than direct market barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mandatrophy mislabeling by distinguishing lapsed from active components. A pure lapsed reading would mislabel active suppression as benign historical residue; a pure beneficiary-maintained reading would mislabel genuinely efficient scale economies and historical lock-in as pure extraction. The hybrid reading captures that the constraint's mandate (market coordination) is partially dead (lapsed elements) and partially captured (active maintenance), producing a tangled_rope classification that preserves the analytical distinction between coordination residue and rent-seeking overlay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_active_proportion,
    'What proportion of current market dominance in a given sector is attributable to lapsed historical closures versus active incumbent maintenance?',
    'Sectoral decomposition via historical institutional analysis, enforcement-expenditure tracking, and counterfactual simulation of entry conditions absent active exclusion.',
    'Would reweight the constraint between piton-like lapsed inertia, tangled-rope hybridity, and snare-like active extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_active_proportion, empirical, 'Boundary ambiguity between lapsed and active components of dominance').

omega_variable(
    naturalization_epiphenomenality,
    'Does the narrative of market dominance as natural function as an independent ideological constraint, or is it merely epiphenomenal to the underlying lapsed and active mechanisms?',
    'Discourse analysis measuring how naturalization narratives correlate with regulatory forbearance, judicial deference, and public acceptance of concentration.',
    'If naturalization is an independent constraint, effective extraction is higher than the structural measure suggests; if epiphenomenal, the authored metrics suffice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalization_epiphenomenality, conceptual, 'Whether naturalization narrative is an independent constraint or byproduct').

omega_variable(
    hybrid_reading_temporal_stability,
    'Can the hybrid reading maintain analytical coherence when the lapsed-to-active boundary shifts sectorally and temporally?',
    'Cross-sector comparative studies tracking when lapsed advantages convert to actively maintained ones and vice versa over institutional evolution.',
    'If the boundary is unstable, the hybrid reading may dissolve into one of its sibling readings, collapsing the mixed beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_temporal_stability, conceptual, 'Temporal stability of the hybrid analytical frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__hybrid_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__hybrid_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__hybrid_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__hybrid_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mark_be_t8, market_naturalization__hybrid_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(mark_be_t16, market_naturalization__hybrid_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(mark_be_t24, market_naturalization__hybrid_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(mark_be_t32, market_naturalization__hybrid_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mark_su_t8, market_naturalization__hybrid_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(mark_su_t16, market_naturalization__hybrid_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(mark_su_t24, market_naturalization__hybrid_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(mark_su_t32, market_naturalization__hybrid_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid reading of the market_naturalization kernel, decomposed from the ambiguous natural-language concept of market dominance into a structurally precise claim. Sibling readings handle the pure-type claims: lapsed_alternative claims low extractiveness with no active enforcement; beneficiary_maintained claims high extractiveness with no lapsed residue. The hybrid reading's moderate extractiveness and mixed mechanism are epsilon-invariant and distinct from both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
