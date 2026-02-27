% ============================================================================
% CONSTRAINT STORY: jp_eez_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jp_eez_enforcement, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jp_eez_enforcement
 *   human_readable: Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)
 *   domain: geopolitical/maritime_sovereignty
 *
 * SUMMARY:
 *   Japan's enforcement of its Exclusive Economic Zone (EEZ) around the
 *   Senkaku/Diaoyu Islands represents a hybrid constraint combining genuine
 *   maritime coordination (UNCLOS framework, shipping safety, fisheries
 *   management) with asymmetric geopolitical extraction (sovereignty
 *   assertion, resource monopoly, exclusion of Chinese claimants). The
 *   constraint exhibits tangled_rope structure: Japan's Coast Guard provides
 *   measurable coordination benefits (navigational clarity, resource
 *   predictability) while simultaneously extracting de facto control over
 *   contested waters through enforcement mechanisms (vessel interdiction,
 *   crew detention, economic penalties) that target Chinese fishing
 *   operations. The constraint's extractiveness has increased over the
 *   measurement interval (0.35 → 0.58) as Chinese maritime activity has
 *   intensified and Japan has escalated enforcement response. Theater ratio
 *   remains moderate (0.55), reflecting that enforcement actions are
 *   functionally coercive rather than purely performative—actual
 *   confiscations and detentions occur—but increasingly coupled with symbolic
 *   assertions (naming, administrative claims) that exceed enforcement
 *   capacity. Suppression is high (0.72) because Chinese actors have limited
 *   alternatives: international law provides no dispute resolution mechanism
 *   with binding authority on both parties, UNCLOS allows unilateral EEZ
 *   claim but does not resolve overlapping claims, and exit options
 *   (alternative fishing grounds, negotiated access) are politically
 *   foreclosed by sovereignty dispute.
 *
 * KEY AGENTS:
 *   - Japan Maritime Authority (Japan Coast Guard): Primary beneficiary (institutional/arbitrage) — enforces EEZ claims, maintains control, shapes maritime order
 *   - Japan Fishing Industry: Beneficiary (institutional/arbitrage) — gains exclusive access to high-productivity waters
 *   - Chinese Fishing Crews: Primary victim (powerless/trapped) — face interdiction, confiscation, economic penalties; dependent on these grounds; no exit mechanism
 *   - Chinese Maritime Authority: Secondary victim/organized rival (organized/constrained) — constrained by UNCLOS recognition of Japan's rights while asserting competing claims; cannot openly challenge without escalation
 *   - Third-Party Maritime Users: Incidental victim (moderate/constrained) — benefit from navigational order but constrained by unpredictable sovereignty disputes; risk of inadvertent incursion into contested zones
 *   - Analytical Observer: Sees coordination + extraction (analytical/analytical) — international law provides framework (rope) but enforcement asymmetry creates extraction (snare); tangled_rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_eez_enforcement, 0.58).
domain_priors:suppression_score(jp_eez_enforcement, 0.72).
domain_priors:theater_ratio(jp_eez_enforcement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_eez_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(jp_eez_enforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jp_eez_enforcement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jp_eez_enforcement, tangled_rope).
narrative_ontology:human_readable(jp_eez_enforcement, "Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)").
narrative_ontology:topic_domain(jp_eez_enforcement, "geopolitical/maritime_sovereignty").

domain_priors:requires_active_enforcement(jp_eez_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japan_fishing_industry).
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japan_maritime_authority).
narrative_ontology:constraint_victim(jp_eez_enforcement, chinese_fishing_fleets).
narrative_ontology:constraint_victim(jp_eez_enforcement, third_party_maritime_access).
narrative_ontology:constraint_victim(jp_eez_enforcement, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE FISHING CREWS (SNARE) — Face Coast Guard interdiction, confiscation of vessels, detention, and fines. Economic dependence on these fishing grounds and lack of alternative livelihoods trap them. No mechanism to challenge sovereignty claims. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(jp_eez_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THIRD-PARTY MARITIME USERS (TANGLED ROPE) — Gain access to navigational standards and maritime order from Japan's enforcement (coordination function), but constrained by unpredictable sovereignty claims and risk of incidental interdiction. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: JAPAN MARITIME AUTHORITY (ROPE) — Enforces EEZ; experiences constraint as coordination of maritime order, resource management, and territorial assertion. High arbitrage capacity (can choose enforcement intensity). d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(jp_eez_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JAPAN FISHING INDUSTRY (ROPE) — Beneficiary of exclusive access to high-productivity fishing grounds. Experiences constraint as resource coordination and monopoly protection. d≈0.12, f(d)≈-0.08, σ=0.9 → χ≈-0.04. Net beneficiary through protected access.
constraint_indexing:constraint_classification(jp_eez_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CHINESE MARITIME AUTHORITY (TANGLED ROPE) — Organized actor; experiences constraint as asymmetric enforcement against its interests. China benefits from regional maritime order but not from this specific EEZ claim. Constrained by international law (UNCLOS) but asserts competing claims. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Views EEZ enforcement through international law framework (UNCLOS provides coordination function: predictable maritime zones) and geopolitical extraction (Japan asserts claims contested by China; extracts de facto control). Coordination (UNCLOS structure) + extraction (contested sovereignty) = Tangled Rope. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_eez_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jp_eez_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jp_eez_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jp_eez_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jp_eez_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value through three mechanisms: (1) monopoly on fishing access (Chinese crews excluded from high-productivity zone), (2) capture of rent through enforcement (penalties, confiscation), (3) geopolitical dominance (de facto control over disputed territory). However, extraction is not maximal (0.70+) because: Japan cannot fully suppress Chinese activity (crews continue fishing, face repeated interdiction cycle), UNCLOS provides some legitimate foundation (reduces naked extraction), and coordination benefits (maritime safety, predictable zones) are genuine. Suppression (0.72): High. Chinese fishing crews have minimal alternatives: alternative grounds are lower-yield or occupied by other claimants, international law provides no recourse, domestic political pressure on China prevents negotiated access, and economic dependence on Senkaku fisheries is significant for coastal communities. The only escape route is offshore migration, costly and long-term. Theater ratio (0.55): Moderate. Enforcement includes performative elements (administrative designation of zones, symbolic naming, ceremonial patrols) but is functionally coercive (actual confiscations, criminal detention). This is higher than pure coordination (arXiv model, ~0.10) but lower than purely symbolic constraint (judicial review theater, ~0.80). The constraint is real but partly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   MAXIMAL PERSPECTIVAL VARIANCE: Japanese institutional actors (Maritime Authority, fishing industry) see rope—a coordination mechanism providing order and resource access. Chinese crews see snare—extraction with no exit. Chinese Maritime Authority sees tangled_rope—benefits from regional order but suffers from this specific asymmetric enforcement. Third parties see tangled_rope—both coordination and extraction. The analytical observer sees tangled_rope with risk of false mountain classification (naturalizing geopolitical claims as immutable law). This gap reflects the fundamental asymmetry: Japan's enforcement rests on institutional capacity (Coast Guard, administrative machinery, international legitimacy) while victims lack equivalent organizing power. The gap is NOT a measurement artifact—it reflects real structural differences in exit options and power relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Japan Maritime Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction; institution benefits from the constraint. Chinese fishing crews: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options, full target. Chinese Maritime Authority: Victim (constrained EEZ access) + organized + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; organized power partially offsets victimization. Third-party users: Victim (restricted access) + moderate + constrained → d≈0.68, f(d)≈1.02. Moderate-high extraction; benefit from coordination but constrained by disputes. Scope modifier σ(regional)=0.9 dampens effective extraction relative to global constraints, but regional scope makes containment harder (sovereignty disputes travel poorly).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA STRUCTURAL DECOMPOSITION: The temptation is to misclassify Japan's EEZ enforcement as a MOUNTAIN (natural law of maritime delimitation per UNCLOS) or as a pure ROPE (coordination mechanism). The mandatrophy is resolved by recognizing that: (1) UNCLOS is a rope—it provides genuine coordination (predictable zones, resource allocation, dispute mechanism). (2) Japan's ENFORCEMENT of contested claims is tangled_rope—it adds asymmetric extraction (Chinese exclusion, rent capture, dominance assertion) on top of the coordination framework. (3) The constraint is NOT a mountain because: the ε value (0.58) is driven by geopolitical choice (enforcement intensity, approach to ambiguity), not by physical law; accessibility_collapse is zero (Chinese crews can technically still fish, they just risk confiscation); resistance to change is non-zero (negotiated settlement is possible). (4) The constraint IS genuinely extractive because suppression (0.72) reflects limited alternatives and asymmetric enforcement targeting a politically weaker opponent. The Tangled Rope classification correctly captures both functions: Japan provides maritime order (coordination) while extracting geopolitical dominance and resource monopoly (extraction). Extractiveness rising from 0.35 to 0.58 over the interval reflects escalating enforcement intensity, not discovery of a pre-existing natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unclos_validity_threshold,
    'Does UNCLOS Article 121 (rocks vs islands) correctly classify the Senkaku/Diaoyu feature, or is the classification itself a contested geopolitical extraction mechanism?',
    'International Court of Justice interpretation; geological/hydrographic analysis accepted by both claimant states; precedent from similar cases (Scarborough Shoal, South China Sea)',
    'If UNCLOS classification is accepted: coordination function dominates, tangled_rope softens toward rope for all institutional perspectives. If classification is rejected as colonially-imposed: extraction function dominates, tangled_rope hardens toward snare for affected groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unclos_validity_threshold, conceptual, 'Whether UNCLOS classification is valid or extractive').

omega_variable(
    enforcement_asymmetry_sustainability,
    'Can Japan sustain unilateral Coast Guard enforcement against increasing Chinese maritime presence without escalation to direct military confrontation?',
    'Time-series analysis of incidents, response patterns, and escalation risks; strategic modeling of asymmetric maritime confrontation; willingness-to-use-force indicators from both states',
    'If sustainable: snare classification for Chinese crews persists indefinitely. If unsustainable: constraint degrades to piton (performative enforcement without functional control) or transforms into military standoff (reclassification needed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_sustainability, empirical, 'Whether unilateral enforcement can be sustained without escalation').

omega_variable(
    alternative_bilateral_framework,
    'Could shared EEZ governance or co-management agreements replace unilateral enforcement while preserving resource access for both parties?',
    'Analysis of precedent agreements (joint development zones, fishing agreements); bilateral negotiation outcomes; pressure from third-party maritime users for predictability',
    'If viable: constraint could transform from tangled_rope toward scaffold (temporary enforcement with negotiated sunset). If infeasible: extraction mechanisms lock in, increasing suppression and deepening snare classification for Chinese actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_bilateral_framework, preference, 'Whether co-management alternatives are politically viable').

omega_variable(
    international_law_enforcement_authority,
    'Does Japan''s enforcement rest on legitimate UNCLOS authority or on de facto military capability divorced from legal mandate?',
    'International law analysis by neutral tribunal; acceptance of UNCLOS interpretation by third-party maritime states; consistency with enforcement against other EEZ incursions (by allied states, global actors)',
    'If legitimate: rope classification strengthens (coordination function validated). If illegitimate: snare and piton classifications dominate (extraction without legal basis, maintained by force).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_law_enforcement_authority, conceptual, 'Whether EEZ enforcement rests on legal or military authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jp_eez_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jpeez_tr_t0, jp_eez_enforcement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jpeez_tr_t5, jp_eez_enforcement, theater_ratio, 5, 0.5).
narrative_ontology:measurement(jpeez_tr_t10, jp_eez_enforcement, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(jpeez_be_t0, jp_eez_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jpeez_be_t5, jp_eez_enforcement, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(jpeez_be_t10, jp_eez_enforcement, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jp_eez_enforcement, resource_allocation).
narrative_ontology:affects_constraint(jp_eez_enforcement, south_china_sea_claims).
narrative_ontology:affects_constraint(jp_eez_enforcement, unclos_interpretation_authority).
narrative_ontology:affects_constraint(jp_eez_enforcement, chinese_fishing_industry_constraints).

% DUAL FORMULATION NOTE:
% This constraint decomposes into two structurally distinct claims: (1) UNCLOS maritime delimitation (genuine coordination, ε≈0.05, rope), (2) Japan's enforcement against Chinese vessels (geopolitical extraction, ε≈0.58, tangled_rope). The constraint family includes parallel EEZ enforcement mechanisms in the South China Sea, each with region-specific ε values and institutional contexts. All family members share the fundamental tension between UNCLOS coordination and geopolitical extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jp_eez_enforcement, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
