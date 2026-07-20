% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Georgist Reading: Land Rent vs. Improvement Value in Price Formation
 *   domain: political_economy/housing/institutional_analysis
 *
 * SUMMARY:
 *   The Georgist reading of price formation holds that housing and land
 *   markets commingle two structurally distinct values: land rent (a
 *   locational premium arising from scarcity and community activity, unearned
 *   by the titleholder) and improvement value (the product of labor and
 *   capital investment in buildings and infrastructure). Under this reading,
 *   the current institutional arrangementâwhere titles to land are traded
 *   bundled with improvements, and finance appraises them as a single
 *   assetâfunctions as a tangled rope. It carries a genuine coordination
 *   function for productive investment in improvements, but simultaneously
 *   enforces an asymmetric transfer of land rent from tenants, wage-earners,
 *   and productive investors to rentier landowners. The constraint persists
 *   through active legal enforcement of property titles, eviction, and a
 *   financial apparatus that treats land as a speculative asset. This story
 *   authors the Georgist reading as one constraint in a contested kernel;
 *   sibling readings (naturalist, institutional, financialization) are
 *   modeled as separate files.
 *
 * KEY AGENTS:
 *   - rentier_landowners: Primary beneficiary (powerful/arbitrage) â captures land rent without production
 *   - tenants: Primary target (powerless/constrained) â pays commingled rent
 *   - productive_investors: Secondary target (moderate/constrained) â pays speculative land prices
 *   - wage_labor: Secondary target (powerless/constrained) â bears rent through depressed housing purchasing power
 *   - state_property_regime: Agenda setter (institutional/analytical) â enforces titles and commingled appraisal
 *   - community_land_trusts: Excluded alternative (moderate/constrained) â represents the decommodified path
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.72).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.65).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading: Land Rent vs. Improvement Value in Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '0e51bb34-3cb3-4f46-b1c1-34c44cb182b6').
narrative_ontology:cs_kernel_codification('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', formalized).
narrative_ontology:cs_authority_grounding('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', lineage).
narrative_ontology:cs_interpretation_layer_present('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6').
narrative_ontology:cs_reading_relation('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', foundational, land_rent_is_unearned_surplus).
narrative_ontology:cs_axiom_status(land_rent_is_unearned_surplus, holdable).
narrative_ontology:cs_axiom_grounding('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', land_rent_is_unearned_surplus, empirically_contingent).
narrative_ontology:cs_axiom('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', foundational, producer_owns_improvement_value).
narrative_ontology:cs_axiom_status(producer_owns_improvement_value, holdable).
narrative_ontology:cs_axiom_grounding('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', producer_owns_improvement_value, deontological).
narrative_ontology:cs_reference_frame('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', classical_rent_theory).
narrative_ontology:cs_drift_state('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', financialized_urban_contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0e51bb34-3cb3-4f46-b1c1-34c44cb182b6', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, rentier_landowners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_investors).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, wage_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to land and capture locational value increases and ground rent without contributing labor or capital to improvements. They benefit from the institutional conflation of land rent with building value in mortgage appraisal, sale prices, and tax treatment. Exit is liquid: they can sell titles and redeploy capital across asset markets.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, rentier_landowners, beneficiary,
    powerful, generational, arbitrage, national).

% Pay monthly housing costs that embed both land rent and improvement charges in a single bundled price. They cannot decline participation in the location market and lack the savings or political power to recapture land rent through policy. Their payments transfer surplus to title-holders.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants, payer,
    powerless, immediate, constrained, local).

% Supply capital and labor to buildings and infrastructure but must pay speculative land prices or ground rents to access sites. The constraint extracts from them because land price inflation raises project costs beyond the value of their productive contribution, while they retain returns only on the improvement component.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_investors, payer,
    moderate, biographical, constrained, regional).

% Earns income from labor but must pay for residential access through rents or mortgages whose land component is unearned by the titleholder. In the Georgist frame, wages are depressed by the need to pay rent for access to location, creating a structural transfer from labor to landownership.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, wage_labor, payer,
    powerless, immediate, constrained, local).

% Separate land ownership from improvement ownership to remove land from speculative markets and capture land rent for community benefit. They are structurally excluded from mainstream housing finance, appraisal standards, and tax codes that treat land and buildings as a single bundle.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, community_land_trusts, excluded,
    moderate, generational, constrained, local).

% Maintains the title registry, eviction enforcement, and property law that legally underwrite the commingled price. It could in principle assess land and improvement values separately for taxation but currently administers them as a bundled unit, enforcing the extraction structure.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, state_property_regime, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, rentier_landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuine coordination of productive investment in buildings, infrastructure, and capital improvements on land; the improvement component of price signals where and how much to build and maintains incentives for upkeep.
% TRANSFER_FUNCTION: Moves the locational premium (land rent) from tenants, wage-earners, and productive investors to rentier landowners and title-holders, without reciprocal production from the recipient.
% ABSENT_VOICES: Georgist policy advocates and community land trusts who would separate land rent from improvements through land value taxation or social ownership; they are excluded from mainstream housing finance, appraisal, and zoning discourse.
% DISAPPEARANCE_RATIONALE: If the commingled price formation vanished and land rent were fully socialized while improvements remained privately traded, land speculation would collapse, housing prices would decouple from location rent, urban development would shift toward highest-use rather than highest-speculation, and the political economy of landownership would reorganize.
% FOUNDING_PROBLEM: How to allocate fixed land locations among competing uses while incentivizing productive capital investment in improvements.
% FOUNDING_PROBLEM_CORROBORATION: Classical political economists (Smith, Ricardo, Mill) corroborate the distinction between land rent and improvement value from outside the modern landed-interest beneficiary set. Modern Georgist economists and some urban planning scholars attest the continued relevance of the separation, while real estate lobbies and neoclassical finance economists contest it.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.74 at interval end) because landowners capture a rising share of value that the Georgist frame identifies as independent of their productive contribution. Suppression (0.65) reflects the active enforcement of title, eviction, and mortgage appraisal norms that prevent the separation of land from improvement markets. Theater ratio (0.48) captures the ideological performance that conflates 'property rights' with 'production,' masking the unearned component. The measurement series run on a shared time grid showing accumulation of extraction as urban agglomeration and financialization deepen. Accessibility collapse (0.58) indicates that while alternatives like community land trusts and land-value taxation exist, they are marginalized by finance norms and tax codes. Resistance (0.48) is moderateâtenant organizing and Georgist policy advocacy persist but face concentrated landed power.
 *
 * PERSPECTIVAL GAP:
 *   The rentier landowner seat should compute as near-beneficiary (low d): the constraint subsidizes their wealth accumulation and they enjoy arbitrage-grade exit. The tenant and wage-labor seats should compute as near-target (high d): they pay the transfer and have constrained exit, amplifying effective extraction. The state seat sits near symmetric or analytical: it enforces but does not personally capture the rent. Productive investors experience a mixed directionalityâthey benefit from improvement returns but lose to land speculationâso structural derivation should place them near the middle. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rentier_landowners) receive the land rent component without producing; their low exit cost (arbitrage) and high power damp their effective extraction. Victims (tenants, productive_investors, wage_labor) bear the payment and have constrained exit, amplifying their effective extraction. The directionality derivation should map these structural positions without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as pure extraction (snare) by preserving the genuine coordination function of improvement investment: buildings must still be built, capital must still be allocated to construction. Conversely, it prevents mislabeling as pure coordination (rope) by insisting on the asymmetric rent transfer. If the improvement coordination were absent, the constraint would be a pure snare; if the rent extraction were eliminated (e.g., via full land value taxation), it would collapse toward a rope for improvements and the land scarcity would revert to a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_supply_vs_property_institution,
    'Does land rent exist independently of the institution of private property in land, or is the extraction entirely an artifact of that institution?',
    'Comparative analysis of non-private land tenure systems (community land trusts, customary tenure, public leasehold) to observe whether locational surplus persists without private rent capture.',
    'If extraction vanishes without private title, the land component is not a mountain but an institutional snare, and the Georgist reading strengthens toward a prescriptive policy mandate rather than a descriptive observation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_supply_vs_property_institution, empirical, 'Whether land rent is natural scarcity or institutionally produced').

omega_variable(
    separability_in_assessment,
    'Can land rent and improvement value be operationally separated in complex urban housing markets, or does heterogeneity and bundled finance make the Georgist analytical separation analytically valid but practically infeasible?',
    'Hedonic pricing studies and mass appraisal pilot programs that independently assess land and building values at scale.',
    'If inseparable, the Georgist reading remains a theoretical critique; if separable, it becomes a directly actionable tax base and planning instrument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_in_assessment, empirical, 'Operational separability of land and improvement values').

omega_variable(
    committer_sibling_positioning,
    'How does the Georgist reading''s claim that land rent is unearned surplus structurally interact with the naturalist reading''s claim that price formation is a natural equilibrium reflecting objective scarcity?',
    'Analysis of whether the Georgist empirical claim (rent is unearned surplus measurable independent of owner effort) falsifies the naturalist equilibrium claim or merely adds a normative redistribution layer atop it.',
    'Determines whether the two readings are incommensurable paradigms or compatible descriptions at different analytical levels, with consequences for how the constraint family is networked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_sibling_positioning, conceptual, 'Structural relationship between Georgist and naturalist framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfk_georgist_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pfk_georgist_tr_t10, price_formation_kernel__georgist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(pfk_georgist_tr_t20, price_formation_kernel__georgist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(pfk_georgist_tr_t30, price_formation_kernel__georgist_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(pfk_georgist_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(pfk_georgist_tr_t50, price_formation_kernel__georgist_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(pfk_georgist_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pfk_georgist_be_t10, price_formation_kernel__georgist_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(pfk_georgist_be_t20, price_formation_kernel__georgist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pfk_georgist_be_t30, price_formation_kernel__georgist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(pfk_georgist_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(pfk_georgist_be_t50, price_formation_kernel__georgist_reading, base_extractiveness, 50, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(pfk_georgist_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pfk_georgist_su_t10, price_formation_kernel__georgist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(pfk_georgist_su_t20, price_formation_kernel__georgist_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(pfk_georgist_su_t30, price_formation_kernel__georgist_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(pfk_georgist_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(pfk_georgist_su_t50, price_formation_kernel__georgist_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% The kernel price_formation_kernel decomposes into four sibling constraints because the colloquial label 'price formation' conflates structurally distinct claims: natural equilibrium, institutional construction, financialized asset dynamics, and the Georgist land/improvement separation. Each reading carries a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
