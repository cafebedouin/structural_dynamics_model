% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__georgist_reading, []).

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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Georgist Reading of Land Price Formation
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   This constraint story instantiates the Georgist reading of the price
 *   formation kernel: the claim that housing and land prices analytically
 *   separate into an unearned land rent component (arising from fixed supply
 *   and community-created location value) and an earned improvement component
 *   (returning labor and capital). Under this reading, the institutional
 *   framework that enforces this separation operates as a tangled rope: it
 *   genuinely coordinates productive investment in improvements, but
 *   simultaneously enables asymmetric extraction by allowing private
 *   landowners to capture location rents. The land component presents as a
 *   false-summit mountainâfixed supply appears as natural law, yet
 *   identifiable beneficiaries (landowners) collect from its operation. The
 *   improvement component functions as rope. The overall constraint requires
 *   active enforcement through property rights and eviction law.
 *
 * KEY AGENTS:
 *   - landowners: Primary beneficiary (powerful/arbitrage) â captures location rent without production
 *   - state_property_regime: Agenda-setter (institutional/analytical) â enforces title and eviction
 *   - urban_tenants: Primary target (powerless/constrained) â pays embedded land rent
 *   - productive_investors: Secondary target (moderate/constrained) â land rent reduces returns on capital
 *   - community_at_large: Excluded voice (organized/constrained) â creates value but excluded from capture
 *   - georgist_economists: Analytical observer (analytical/analytical) â identifies the structural separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.72).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.55).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading of Land Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '3452cf7c-3c85-4feb-81a7-a9eecd897b14').
narrative_ontology:cs_kernel_codification('3452cf7c-3c85-4feb-81a7-a9eecd897b14', formalized).
narrative_ontology:cs_authority_grounding('3452cf7c-3c85-4feb-81a7-a9eecd897b14', lineage).
narrative_ontology:cs_interpretation_layer_present('3452cf7c-3c85-4feb-81a7-a9eecd897b14').
narrative_ontology:cs_reading_relation('3452cf7c-3c85-4feb-81a7-a9eecd897b14', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3452cf7c-3c85-4feb-81a7-a9eecd897b14', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('3452cf7c-3c85-4feb-81a7-a9eecd897b14', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('3452cf7c-3c85-4feb-81a7-a9eecd897b14', foundational, land_rent_is_unearned_surplus).
narrative_ontology:cs_axiom_status(land_rent_is_unearned_surplus, holdable).
narrative_ontology:cs_axiom_grounding('3452cf7c-3c85-4feb-81a7-a9eecd897b14', land_rent_is_unearned_surplus, empirically_contingent).
narrative_ontology:cs_axiom('3452cf7c-3c85-4feb-81a7-a9eecd897b14', secondary, community_should_recover_location_value).
narrative_ontology:cs_axiom_status(community_should_recover_location_value, holdable).
narrative_ontology:cs_axiom_grounding('3452cf7c-3c85-4feb-81a7-a9eecd897b14', community_should_recover_location_value, deontological).
narrative_ontology:cs_reference_frame('3452cf7c-3c85-4feb-81a7-a9eecd897b14', georgist_analytic_frame).
narrative_ontology:cs_drift_state('3452cf7c-3c85-4feb-81a7-a9eecd897b14', contemporary_financialized_housing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3452cf7c-3c85-4feb-81a7-a9eecd897b14', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, urban_tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_investors).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, classical_rent_theory).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, land_labor_capital_trichotomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to land locations and capture the scarcity premium created by community growth, public infrastructure, and agglomeration economies; receive rents and capital gains from the land component of housing prices without contributing labor or capital to improvements on the land.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, beneficiary,
    powerful, generational, arbitrage, national).

% Maintains the legal framework of private property in land, including title registration, eviction enforcement, and land-use regulation; assigns location rents to private title holders rather than to the community through its enforcement apparatus.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, state_property_regime, agenda_setter,
    institutional, civilizational, analytical, national).

% Pay monthly housing costs that embed a substantial land rent component; must occupy locations proximate to employment and services; face displacement or homelessness if they cannot pay the rent extracted by title holders.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, urban_tenants, payer,
    powerless, immediate, constrained, local).

% Firms and developers who must acquire land access before undertaking productive investment; the land rent component reduces the return on earned capital and labor, transferring surplus to landowners who did not produce the location value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_investors, payer,
    moderate, biographical, constrained, regional).

% The collective body of residents and public infrastructure investments that generate location value; excluded from receiving the rent their presence and taxes create, which is instead captured by private title holders.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, community_at_large, excluded,
    organized, generational, constrained, local).

% Analytical observers who identify the separation of land rent from improvement value in price formation; maintain the analytical framework that distinguishes unearned location rents from earned returns to labor and capital.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of labor and capital toward improvements (buildings, infrastructure, maintenance) by allowing producers to recover invested capital and labor through market prices that are analytically separable from location rents.
% TRANSFER_FUNCTION: Moves location value created by community presence, public investment, and natural scarcity from tenants and productive investors to private landowners, while permitting producers to retain returns on constructed improvements.
% ABSENT_VOICES: The community at large that generates location value is excluded from price formation and rent distribution; tenant organizers and labor advocates who would challenge the rent component are marginalized in housing policy discourse dominated by property owners and financial intermediaries.
% DISAPPEARANCE_RATIONALE: If the price formation mechanism that separates land rent from improvements vanished overnight, land rent could no longer be independently captured by title holders; housing costs would restructure around improvement values alone, landowners would lose the scarcity premium, and productive investment would be freed from the land rent burdenâthe urban political economy would rearrange.
% FOUNDING_PROBLEM: Classical political economy needed to distinguish the earned returns to labor and capital from the unearned returns to land monopoly, in order to explain persistent poverty amid progress and to design a non-distorting tax system that would not penalize production.
% FOUNDING_PROBLEM_CORROBORATION: Georgist economists and some classical political economists attest the problem is still live. Neoclassical and institutional economists attest the land/capital distinction has been conflated or superseded by broader factor-market analysis; urban economists and housing researchers outside the Georgist tradition increasingly document rising land rent shares, corroborating the empirical phenomenon but not necessarily the Georgist policy prescription.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__georgist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__georgist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.72 (high) because the Georgist reading identifies land rent as a substantial, growing share of housing pricesâvalue transferred without productive contribution. Suppression is 0.55 because the arrangement depends on active enforcement of property rights and the exclusion of common or collective alternatives. Theater_ratio is 0.60 because the narrative of property rights as natural and earned masks the extraction of community-created value. Accessibility_collapse is 0.60: once inside the Georgist frame, alternatives like land value taxation appear logical, but within the dominant institutional frame, private land ownership appears inevitable. Resistance is 0.45: tenant movements and Georgist advocacy provide moderate resistance, but landowning interests dominate politically. The temporal series show extraction and theater rising as financialization and urban agglomeration amplify land rents over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The landowner seat perceives the constraint as legitimate property rights protecting their asset (possibly computing as rope or mountain), while the tenant and productive-investor seats experience the same structure as extraction of their earned product. The community seat experiences the constraint as alienation of collectively created value. The engine computes this divergence from the structural data rather than from any authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners are declared beneficiaries with arbitrage-grade exit, placing them near the beneficiary end of directionality (low d, subsidized by the constraint). Urban tenants are declared victims with constrained exit, placing them near the full-target end (high d). Productive investors are victims with constrained exit but greater power than tenants. The state property regime sits as agenda-setter with analytical exit, directionality toward enforcement maintenance rather than personal extraction. The excluded community drifts toward high d because it bears the costs without a seat at the table.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) by preserving the genuine coordination function for improvements: buildings do get built and capital does flow to construction through the price signal. It prevents mislabeling as pure coordination (rope) by naming the asymmetric land rent capture that rides on the same structure. If the improvement coordination were absent, the constraint would be snare; if the rent capture were absent, it would be rope. Both are present, so tangled_rope is the structurally honest classification that captures the hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_component_naturalness,
    'Does the land rent component of price formation reflect an irreducible natural scarcity (mountain), or is the measured extraction entirely a product of institutional property arrangements?',
    'Cross-jurisdictional comparison of land rent levels under different property regimes (community land trusts, public leasehold, full private fee simple) controlling for location quality.',
    'If land rent persists even under non-private regimes as a location premium, the natural scarcity component is genuine and the constraint''s extraction is partially inherent; if rent collapses when private capture is removed, the ''mountain'' component was a false summit and the constraint is closer to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_component_naturalness, empirical, 'Ambiguity between natural scarcity and institutional rent capture in land component').

omega_variable(
    price_separability_in_practice,
    'Can market price formation actually separate land rent from improvement value, or do transaction structures fuse them into an inseparable bundle?',
    'Hedonic pricing studies and assessment practices that independently value land and structures; reliability of such separation in dense urban markets.',
    'If inseparable, the Georgist analytical distinction cannot be operationalized as a constraint, undermining the reading''s policy program (land value taxation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_separability_in_practice, empirical, 'Empirical separability of land and improvement values in market prices').

omega_variable(
    kernel_reading_sibling_divergence,
    'How would the classification of price formation change under sibling readings of the same kernel?',
    'Comparative analysis of the naturalist, institutional, and financialization readings as separate constraint stories; evaluation of whether the kernel should be decomposed into distinct constraints per the epsilon-invariance principle.',
    'If sibling readings produce structurally divergent epsilon values and stakeholder configurations, the kernel is confirmed as a constraint family; if they converge, the kernel may support a single reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Structural divergence across kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__georgist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__georgist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__georgist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.54).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__georgist_reading, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__georgist_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__georgist_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__georgist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__georgist_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__georgist_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__georgist_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__georgist_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__georgist_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
