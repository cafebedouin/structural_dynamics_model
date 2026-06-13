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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Land Rent Extraction Under Georgist Price Formation
 *   domain: political_economy/housing/institutional
 *
 * SUMMARY:
 *   The Georgist reading of price formation asserts that housing prices
 *   decompose into two components: land rent (unearned, driven by location
 *   scarcity and surrounding development) and improvement value (earned,
 *   produced by labor and capital investment). Under this reading, landowners
 *   extract unearned increments through price appreciation and rent
 *   collection, while labor bears the cost through housing affordability
 *   collapse and wage compression. The constraint is hybrid: the land
 *   component (fixed supply, location scarcity) behaves as a mountain, but
 *   rent extraction (organized by property rights and enforced through debt
 *   and eviction) operates as a snare targeting the housing-insecure. The
 *   improvement component operates as rope (genuine coordination of building,
 *   production). The Georgist reading directly contests three sibling
 *   readings: the naturalist reading (price is equilibrium, not extraction),
 *   the institutional reading (price is construction via policy, not
 *   decomposition), and the financialization reading (price is credit-driven,
 *   not land-driven).
 *
 * KEY AGENTS:
 *   - landowner_class: institutional agenda-setter and beneficiary; captures unearned location value; organizes politically to defend property-rights framing and tax treatment that shields land appreciation; directionality near full beneficiary (d ≈ 0.1–0.2)
 *   - labor_class: organized payer and victim; wages compressed by rent burden; structured resistance through tenant unions and housing movements; constrained exit (labor markets are geographically tied); directionality near partial target (d ≈ 0.65–0.75)
 *   - housing_insecure_renters: powerless payer; trapped by local labor/housing markets; highest rent burden, least negotiating power; endure full extractive force; directionality near full target (d ≈ 0.85–0.95)
 *   - land_speculator_networks: institutional beneficiary; mobile arbitrage capital; pure rent capture without improvement production; global scope; directionality near beneficiary (d ≈ 0.15–0.25)
 *   - improvement_developers: powerful but dual-positioned; produce genuine improvement value but must pay landowner capture; caught between labor (whom they employ/house) and landowners (whom they pay); directionality near symmetric (d ≈ 0.45–0.55)
 *   - tax_policy_makers: institutional agenda-setter; authority to restructure tax treatment and separate land value; currently captured by landowner organizing; analytical observer seat with constrained agency; directionality varies with capture state (d ≈ 0.40–0.50)
 *   - georgist_reform_movement: organized but excluded; would restructure beneficiary/victim if admitted; excluded by institutional capture; advocates land value tax, public capture of location increment; directionality displaced by exclusion
 *   - economic_analysts: analytical observer; surface extractive dynamics through data; provide corroboration for Georgist framing but do not benefit/pay directly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Land Rent Extraction Under Georgist Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing/institutional").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280').
narrative_ontology:cs_kernel_codification('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', distributed).
narrative_ontology:cs_authority_grounding('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', extraction).
narrative_ontology:cs_reading_relation('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', foundational, land_value_is_unearned_increment).
narrative_ontology:cs_axiom_status(land_value_is_unearned_increment, holdable).
narrative_ontology:cs_axiom_grounding('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', land_value_is_unearned_increment, empirically_contingent).
narrative_ontology:cs_axiom('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', foundational, land_scarcity_is_structural_prior).
narrative_ontology:cs_axiom_status(land_scarcity_is_structural_prior, holdable).
narrative_ontology:cs_axiom_grounding('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', land_scarcity_is_structural_prior, empirically_contingent).
narrative_ontology:cs_axiom('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', secondary, public_commons_creation_entails_rent_capture_right).
narrative_ontology:cs_axiom_status(public_commons_creation_entails_rent_capture_right, holdable).
narrative_ontology:cs_axiom_grounding('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', public_commons_creation_entails_rent_capture_right, deontological).
narrative_ontology:cs_reference_frame('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', georgist_land_value_separation).
narrative_ontology:cs_drift_state('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', contemporary_neoliberal_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30ad6eb8-1f16-4bf4-ac9b-9c7cddf3b280', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowner_class).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, land_speculator_networks).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_class).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, housing_insecure_renters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).

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
 *   Extractiveness is high (0.68 at interval end) and rising over 0-30 (0.45 → 0.68) because land values are capturing an increasing share of housing costs as supply constraints tighten and surrounding development concentrates value. The riser plateaus at t=30-40, suggesting a saturation point where rent extraction has compressed labor income and affordability severely enough that further extraction is resisted. Suppression is high (0.71) and rising (0.50 → 0.71 over 0-30, then flat) because the constraint's persistence depends on actively enforcing property rights against squatting/rent strikes, excluding alternative housing models (cooperative, public), and suppressing Georgist reform through political capture. Theater is moderate (0.42) because the constraint sustains a narrative of earned improvement value and efficient markets that partially obscures the rent-extraction mechanism — enough performative legitimacy to suppress organized resistance but not enough to hide extraction completely (hence theater_ratio lower than pure piton, higher than pure snare). Accessibility_collapse rises from 0.55–0.64 individually and 0.65–0.75 structurally, indicating that housing alternatives have progressively closed as landowners and speculators consolidate holdings and prices rise beyond wage growth. Stakes_inflation rises sharply at individual level (0.40 → 0.62) and class level (0.45 → 0.65), modeling the precarity cost escalation: being evicted, displaced, or housing-insecure carries growing life-disruption risk as gentrification accelerates. Resistance at organizational level is substantial (0.62–0.65) because tenant unions and housing movements remain active and organized; individual resistance is suppressed (0.35 → 0.32, declining) because isolated renters have minimal leverage.
 *
 * PERSPECTIVAL GAP:
 *   The landowner seat (beneficiary, institutional, arbitrage exit) and the housing-insecure renter seat (payer, powerless, trapped exit) compute entirely differently from the same price vector. For the landowner, rising land values are natural and earned (improvement and location desirability); the constraint is experienced as reward for holding good property. For the renter, rising land values are extraction (landlord capture of value the renter's labor and presence helped create); the same price movement is experienced as theft. The improvement_developer seat sits between: they profit from rising land values (they sell improvements at higher land-implied prices) but also bear landowner capture (they must pay escalating land acquisition costs), so they have structural interest in both denying the Georgist decomposition (to avoid public land value tax) and in appearing to align with labor (who are their workers/tenants). The engine computes per-seat classifications from the structural data: landowners compute as beneficiary-aligned (low χ effective extraction), renters as target-aligned (high χ), developers as hybrid. The commentary must explain why the same metric (0.68 extractiveness) produces such different seat experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are landowner_class and land_speculator_networks — they capture unearned increments through title-holding and market timing, bearing zero improvement cost. Their directionality derives downward from beneficiary status: landowner_class d ≈ 0.15–0.20 (powerful, wide exit options, direct capture), speculator_networks d ≈ 0.15–0.25 (institutional, mobile, capture). Victims are labor_class and housing_insecure_renters — they pay rent to access location value they did not produce and cannot exit without losing income/relationships. Directionality derives upward from victim status: labor_class d ≈ 0.65–0.75 (organized, constrained exit, indirect target), housing_insecure_renters d ≈ 0.85–0.95 (powerless, trapped, direct full target). Improvement_developers sit near symmetric (d ≈ 0.45–0.55): they benefit from rising land values (sold at higher implicit prices) but bear landowner capture (land acquisition cost) and employ labor (bearing part of rent-burden cost). Tax_policy_makers are captured: they have authority to restructure but are organized against by landowner lobbying; directionality near beneficiary at the institutional level (d ≈ 0.40–0.50, structured asymmetry) but this is analytically separable into 'power when uncaptured' vs. 'actual position when captured.' The engine derives d from beneficiary/victim + exit + power; Georgist analysis adds the claim that 'property rights themselves are constructed and defend extraction' (a second-order structural claim about how d itself is made/maintained). No directionality override is needed; the derivation chain (victims = labor/renters + trapped/constrained exit + weak organizing → high d) produces the correct direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (location scarcity allocation) is live and unchanged — locations are still scarce, still need allocation. But the constraint's mandate (efficient price-discovery mechanism) has partially decayed into pure extraction as land becomes financialized and treated as investment asset rather than location for habitation. Georgist analysis identifies mandatrophy: the beneficiaries (landowners, speculators) benefit from the mandate decay (less price pressure to find alternatives, more rent extraction possible) while the victims (labor, renters) bear the cost. The constraint persists because: (1) landowners have organized political power to defend property-rights framing; (2) improvement developers have structural interest in denying the decomposition (to avoid land value tax); (3) tax_policy_makers are captured by (1) and (2). A genuine Georgist remedy (land value tax, public capture of location increment) would restructure the constraint from snare/tangled-rope into pure rope (genuine coordination of scarcity allocation without extraction), but the path to that restructuring is politically blocked. This is not a case where the original mandate is obsolete — it is a case where the beneficiaries have captured the authority structure and use it to extract beyond the mandate's stated purpose. Mandatrophy is PARTIAL: enough coordination function remains (prices do allocate scarce locations) to sustain the framing, but enough extraction has been added to warrant the snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_scarcity_vs_institutional_construction,
    'Is housing-price inflation fundamentally driven by location scarcity (Georgist claim) or by institutional policy choices (institutional_reading claim) or by credit expansion (financialization_reading claim)?',
    'Comparative institutional analysis: examine jurisdictions that have decoupled institutional constraints (removed zoning restrictions, opened credit easily) but kept land scarcity the same, and vice versa; measure whether prices follow land scarcity or institutional policy. Natural experiments from de-regulated housing markets (e.g., Auckland 2016–2022, Japan''s low credit/high-density model) provide evidence. Decompose price growth into land-component and improvement-component using hedonic methods; time-series analysis of the separation''s stability.',
    'If land scarcity dominates and prices follow it even under different institutions, the Georgist reading is vindicated and land value tax is the primary lever. If institutions dominate and prices follow policy (zoning relaxation → supply increase → prices fall despite same scarcity), then institutional_reading is vindicated and zoning/lending reform is primary. If credit dominates and prices follow debt-to-income ratios regardless of supply/institutions, then financialization is vindicated and credit controls are primary. The three readings are empirically testable but so far inconclusive across jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_scarcity_vs_institutional_construction, empirical, 'Which structural driver (scarcity, institution, credit) dominates price formation.').

omega_variable(
    landowner_improvement_conflation,
    'Do landowners and improvement developers genuinely conflate land value with improvement value, or do they consciously separate them and choose the conflation strategically?',
    'Interviews and internal documents from real estate development firms, landowner organizations, and tax-policy advocacy groups; analysis of their private arguments vs. public rhetoric; examination of how they price and market land separately from improvements in internal deals vs. public facing value propositions.',
    'If conflation is genuine (cognitive), the Georgist separation is a value-added analytical contribution and resistance from beneficiaries is cognitive capture. If conflation is strategic (they know the separation and exploit public confusion), then the constraint''s persistence is more consciously maintained and the suppression mechanism is more overt. Either way, the framing question remains unresolved, but the character of the denial shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(landowner_improvement_conflation, conceptual, 'Whether the land/improvement distinction is cognitively unavailable or strategically obscured.').

omega_variable(
    sibling_reading_empirical_falsifiability,
    'Which of the four readings (naturalist, institutional, financialization, Georgist) is empirically falsifiable, and do they forecast different observable futures?',
    'Construct specific predictions from each reading: if naturalist is true, relaxing zoning (increasing supply) should reduce prices; if institutional is true, changing tax treatment should shift distribution without price movement; if financialization is true, tightening credit should deflate prices regardless of supply/institutions; if Georgist is true, removing land value taxation should increase extraction and require labor-income redistribution to maintain affordability. Run these predictions against historical and ongoing policy experiments. Identify points of divergence and test each.',
    'If all four readings survive empirical testing (make compatible predictions), then price formation is genuinely multi-causal and all four are partial truths. If readings falsify each other empirically, the contest resolves into testable hierarchy. The Georgist reading survives strongest if land-value separation persists under institutional and credit variations, suggesting the land component is the irreducible structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_falsifiability, empirical, 'Cross-reading empirical differentiation and falsifiability.').

omega_variable(
    georgist_remedy_political_feasibility,
    'Is the political impossibility of implementing land value tax a structural feature of democratic governance, or a contingent outcome of current landowner power?',
    'Historical analysis of land value tax adoption (e.g., Denmark''s partial implementation, Singapore''s element, Taiwan''s experience); modeling of coalition dynamics required for land value tax passage in current polities; identification of institutional designs that might break landowner veto (constitutional constraints, supermajority requirements, federalism structures). Compare countries where land value tax persists vs. were abandoned.',
    'If feasible with political will, then the constraint''s current form is contingent on beneficiary power, not structural necessity, and could be restructured. If the constraint is politically sticky regardless of political system, then Georgist remedy remains a limiting case. Either way, the persistence mechanism is revealed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(georgist_remedy_political_feasibility, preference, 'Whether the Georgist remedy is politically impossible or contingently blocked.').

omega_variable(
    committer_frame_kernel_contest,
    'This constraint (georgist_reading) asserts a specific decomposition of price formation. How should the engine weigh this reading against the sibling readings when they produce different ε values and different structural classifications?',
    'The four readings are under a single kernel (price_formation_kernel), which means they are alternative interpretations of the same underlying phenomenon. Committer-frame analysis: the kernel itself is contested; there is no neutral arbiter within the dispute that all four readings would accept. The engine should treat the four stories as a constraint family: publish all four, compute per-reading classifications, and let downstream analysis assess which reading best explains observed outcomes (prices, rent burdens, land concentration, housing supply, wage patterns). The Georgist reading claims ε=0.68 (high extraction) while the naturalist would claim ε→0.0 (no extraction, pure equilibrium). These are not measurement error — they are structural disagreements about what counts as extraction. The framework routes this through committer_frame omegas (this one) and through the cs_structure.reading_relations (forecloses/coexists/influences declarations).',
    'This omega documents that the Georgist reading is ONE contestable reading of a multi-interpretable kernel, not THE TRUE READING. The engine does not resolve the contest; instead, it computes what each reading implies structurally and makes the contrast visible. Downstream analysis (policy, advocacy, research) operates with full knowledge that the readings are incompatible and produces different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, conceptual, 'Kernel contest structure: this is one reading of a multi-interpretable price-formation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__georgist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__georgist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__georgist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__georgist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(pric_tr_t25, price_formation_kernel__georgist_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__georgist_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(pric_tr_t35, price_formation_kernel__georgist_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__georgist_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__georgist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__georgist_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__georgist_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(pric_be_t25, price_formation_kernel__georgist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__georgist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(pric_be_t35, price_formation_kernel__georgist_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__georgist_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__georgist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__georgist_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__georgist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(pric_su_t25, price_formation_kernel__georgist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__georgist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(pric_su_t35, price_formation_kernel__georgist_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__georgist_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, housing_affordability_crisis__structural_extraction).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, tax_treatment_of_property__rent_subsidy).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel price_formation_kernel. All four readings (georgist_reading, naturalist_reading, institutional_reading, financialization_reading) describe the same observable phenomenon (housing-price growth, rent burdens, land-value acceleration) but decompose the causal structure differently and propose different remedies. The Georgist reading asserts the land component is a mountain (supply-fixed, location-based, natural scarcity) but rent EXTRACTION is a snare (organized through property rights, defended politically, extracting from labor). The improvement component is rope (genuine coordination of building production). The sibling readings dispute this decomposition: naturalist claims the entire price vector is equilibrium, institutional claims it is constructed by policy, financialization claims it is credit-driven. Each reading generates different metrics and different type classifications per seat. All four stories should be generated and linked; the four-story family enables downstream analysis to compare reading performance against observed outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
