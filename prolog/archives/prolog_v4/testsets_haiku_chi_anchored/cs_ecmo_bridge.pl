% ============================================================================
% CONSTRAINT STORY: cs_ecmo_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cs_ecmo_bridge, []).

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
 *   constraint_id: cs_ecmo_bridge
 *   human_readable: ECMO Bridge to Transplant
 *   domain: medical_technology/organ_transplant
 *
 * SUMMARY:
 *   ECMO bridge to transplant (extracorporeal membrane oxygenation)
 *   represents a technological intervention that extends the viability window
 *   for patients with end-stage lung disease awaiting transplantation. The
 *   constraint exhibits a core tension: ECMO is genuinely life-saving
 *   coordination technology (keeps patients alive pending organ
 *   availability), yet its implementation creates extraction through
 *   differential access and implicit waitlist prioritization. The mechanism
 *   is tangled — high-volume transplant centers with ECMO capacity benefit
 *   from extended viability windows and increased transplant success, while
 *   patients without geographic or financial access to ECMO-capable centers
 *   experience suppressed alternatives and unequal allocation of scarce donor
 *   organs. The extractiveness has increased over the measurement interval
 *   (0.28 → 0.52) as ECMO has become integrated into allocation protocols,
 *   creating implicit priority for patients at centers with ECMO
 *   infrastructure. Theater_ratio remains low (0.38) because the therapeutic
 *   mechanism is genuine — ECMO functions to extend viability — but
 *   allocation bias is becoming more visible in the data.
 *
 * KEY AGENTS:
 *   - Patients Without ECMO Access: Primary victims (powerless/trapped) — end-stage lung disease patients at non-ECMO centers or in underserved regions; trapped by geography and institutional capacity
 *   - High-Volume Transplant Centers: Primary beneficiaries (institutional/arbitrage) — Mayo, UPMC, Stanford; capture volume and outcomes concentration through ECMO access
 *   - Regional Transplant Programs: Secondary actors (moderate/constrained) — face resource barriers and ICU capacity constraints; benefit from ECMO but constrained by infrastructure investment requirements
 *   - ECMO Device Manufacturers: Secondary beneficiaries (organized/mobile) — Abiomed, Maquet, LivaNova; benefit from market creation but have exit mobility (product portfolio diversification)
 *   - UNOS Allocation System: Institutional actor (institutional/arbitrage) — manages waitlist allocation; benefits from ECMO as a tool for urgency scoring but maintains performative allocation bias
 *   - Analytical Observer: Bioethics perspective (analytical/analytical) — sees hybrid coordination-extraction structure; identifies equity concerns in access and allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cs_ecmo_bridge, 0.52).
domain_priors:suppression_score(cs_ecmo_bridge, 0.65).
domain_priors:theater_ratio(cs_ecmo_bridge, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cs_ecmo_bridge, extractiveness, 0.52).
narrative_ontology:constraint_metric(cs_ecmo_bridge, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cs_ecmo_bridge, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cs_ecmo_bridge, tangled_rope).
narrative_ontology:human_readable(cs_ecmo_bridge, "ECMO Bridge to Transplant").
narrative_ontology:topic_domain(cs_ecmo_bridge, "medical_technology/organ_transplant").

domain_priors:requires_active_enforcement(cs_ecmo_bridge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, lung_transplant_recipients).
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, transplant_centers).
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, ecmo_device_manufacturers).
narrative_ontology:constraint_victim(cs_ecmo_bridge, non_bridge_candidates).
narrative_ontology:constraint_victim(cs_ecmo_bridge, waitlist_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENTS WITHOUT ECMO ACCESS (SNARE) — Trapped in end-stage lung disease without institutional resources to access ECMO bridge. Geographic, financial, and institutional barriers prevent exit. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73. High effective extraction from those denied access.
constraint_indexing:constraint_classification(cs_ecmo_bridge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL TRANSPLANT PROGRAMS (TANGLED ROPE) — Constrained by ICU bed capacity, ECMO specialist availability, and procurement logistics. Benefits from ECMO as a coordination mechanism (keeps patients alive pending donor organ), but also faces extraction through resource concentration at high-volume centers. d≈0.62, f(d)≈0.82, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-VOLUME TRANSPLANT CENTERS (ROPE) — Institutional actors (Mayo, UPMC, Stanford) benefit from ECMO access through coordination: extends patient viability window, increases successful transplant outcomes, and concentrates transplant volume. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Negative effective extraction = net beneficiaries.
constraint_indexing:constraint_classification(cs_ecmo_bridge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ECMO DEVICE MANUFACTURERS (ROPE) — Organized commercial actors (Abiomed, Maquet, LivaNova) benefit from coordination: ECMO creates a market for equipment, consumables, and training. Suppression is low because manufacturers have market mobility (can exit or shift product lines). d≈0.15, f(d)≈0.01, σ=1.2 → χ≈0.01. Near-zero extraction; essentially pure coordination.
constraint_indexing:constraint_classification(cs_ecmo_bridge, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNOS WAITLIST ALLOCATION SYSTEM (PITON) — The UNOS allocation protocol now includes ECMO status as a clinical urgency modifier, but the mechanism is largely performative. Center-based variation in ECMO initiation creates implicit allocation bias; sicker patients at ECMO centers get priority even with identical physiologic urgency. theater_ratio≈0.38 reflects moderate performative content — allocation appears needs-based but functions as capacity-based. Institutional inertia maintains the system despite recognition of its limitations.
constraint_indexing:constraint_classification(cs_ecmo_bridge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (BIOETHICS VIEW) (TANGLED ROPE) — From a global health equity perspective, ECMO bridge represents genuine coordination (extends the therapeutic window, increases total lives saved) coupled with asymmetric extraction (access concentrated at wealthy centers, institutional capacity barriers exclude the majority of end-stage lung patients globally). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.41. The constraint is hybrid: solves a real coordination problem while extracting from those without institutional access.
constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cs_ecmo_bridge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cs_ecmo_bridge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cs_ecmo_bridge, TR),
    TR >= 0.70.

:- end_tests(cs_ecmo_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The base extraction reflects that ECMO access is concentrated at high-volume transplant centers, creating an implicit allocation advantage for patients at these institutions. The trajectory (0.28 → 0.52 over interval) shows increasing extraction as ECMO has become routinized and integrated into clinical urgency scoring. Suppression (0.65): High. Geographic barriers (limited ECMO-capable centers), financial barriers (ECMO support costs $150k-200k per patient), specialized personnel requirements (ECMO specialists in ICU), and implicit institutional bias suppress alternatives for non-bridge candidates. Patients without ECMO access have constrained alternatives; many die awaiting transplant because they cannot access the extended viability window. Theater_ratio (0.38): Moderate-low. The therapeutic mechanism is genuine — ECMO does extend viability — so performative content is not dominant. However, allocation bias is increasing: center-based variation in ECMO initiation creates implicit urgency modifier that appears needs-based but functions as capacity-based. The theater reflects growing recognition that allocation appears equitable (UNOS scoring includes ECMO status) while functioning as institutional access bias.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival divergence. High-volume transplant centers see Rope (pure coordination) — ECMO extends the therapeutic window and increases their success rates. ECMO manufacturers see Rope (market creation with low suppression) — they have portfolio mobility and can exit individual markets. Patients without ECMO access see Snare (extraction with no alternatives) — they are trapped by geography and institutional capacity. UNOS system sees Piton (performative allocation) — the scoring mechanism appears needs-based but functions as capacity-based. Regional programs see Tangled Rope (mixed benefit and constraint) — they benefit from ECMO as a coordination tool but face extraction through resource concentration at high-volume centers. The analytical observer sees Tangled Rope (genuine coordination with unequal distribution) — ECMO solves a real problem but the solution is accessible only to the well-resourced. This perspectival gap arises from the fact that ECMO is location-dependent: its benefit is concentrated at centers with infrastructure and expertise, making the same technological intervention appear as pure coordination to those with access and as extraction to those without.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients without ECMO access: Victims + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. No institutional affiliation, no geographic access to ECMO-capable centers, no ability to exit end-stage lung disease except through death or (rarely) long-distance relocation for transplant evaluation. High-volume transplant centers: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. They control ECMO infrastructure and capture the allocation advantage. They can exit by shifting focus or redistributing resources, but ECMO amplifies their existing advantage (volume → outcomes → referral network → more volume). Regional programs: Victims and partial beneficiaries + constrained → d≈0.62, f(d)≈0.82. Constrained because they face capital and expertise barriers to ECMO implementation but also benefit from ECMO as a coordination mechanism for selected patients. ECMO manufacturers: Beneficiaries + mobile → d≈0.15, f(d)≈0.01. Low extraction because they have market mobility and can diversify product portfolios. UNOS system: Institutional actor managing constraint → d≈0.05 (beneficiary role, arbitrage through allocation authority). Theater_ratio (0.38) indicates performative allocation, not high extraction. Analytical observer: analytical → d≈0.50, f(d)≈0.65. Symmetric position capturing both coordination benefit and extraction mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecmo_survival_attribution,
    'Does ECMO bridge increase net lives saved, or does it redistribute a fixed donor pool toward institutional-access patients?',
    'Cohort study comparing survival outcomes for ECMO bridge vs non-bridge patients with identical LAS scores; analysis of whether ECMO increases donor utilization or merely reallocates from lower-access populations',
    'If net-positive: Rope classification strengthens (pure coordination benefit). If zero-sum reallocation: Snare classification strengthens (extraction from non-bridge candidates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecmo_survival_attribution, empirical, 'Whether ECMO bridge increases total lives saved or reallocates fixed donor pool').

omega_variable(
    access_barrier_essentiality,
    'Are geographic and financial barriers to ECMO access intrinsic to the technology or contingent on infrastructure investment and policy?',
    'Comparative analysis of ECMO access models across healthcare systems (US centers of excellence vs European distributed access vs manufacturer-supported programs in lower-income regions); correlation between infrastructure investment and access equity',
    'If intrinsic: suppression is natural (mountain-like floor). If contingent: suppression is policy-chosen, raising mandatrophy on whether the system should be restructured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_essentiality, conceptual, 'Whether access barriers are intrinsic to ECMO technology or policy-contingent').

omega_variable(
    waitlist_priority_mechanism,
    'Does ECMO status in UNOS urgency scoring reflect objective physiologic deterioration or institutional capacity at transplant centers?',
    'Analysis of UNOS allocation data: comparison of LAS score change rates for ECMO vs non-ECMO patients; assessment of whether ECMO initiation predicts worse outcomes absent the therapy (true salvage) or merely concentrates marginal patients',
    'If objective physiologic: allocation remains needs-based. If capacity-driven: waitlist system exhibits implicit bias, and theater_ratio should increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(waitlist_priority_mechanism, empirical, 'Whether ECMO urgency modifier reflects physiologic deterioration or center capacity').

omega_variable(
    long_term_graft_outcomes,
    'Do lungs retrieved after prolonged ECMO support have inferior long-term graft survival compared to standard donor lungs?',
    'Survival curve analysis of transplant recipients stratified by ECMO bridge duration and donor lung quality metrics; comparison of chronic rejection rates and graft survival at 3-5 years',
    'If inferior: ECMO bridge trades short-term survival for long-term failure (hidden extraction cost). If equivalent: coordination benefit is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_graft_outcomes, empirical, 'Whether ECMO bridge affects long-term graft outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cs_ecmo_bridge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecmo_tr_t0, cs_ecmo_bridge, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ecmo_tr_t5, cs_ecmo_bridge, theater_ratio, 5, 0.32).
narrative_ontology:measurement(ecmo_tr_t10, cs_ecmo_bridge, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(ecmo_be_t0, cs_ecmo_bridge, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ecmo_be_t5, cs_ecmo_bridge, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(ecmo_be_t10, cs_ecmo_bridge, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cs_ecmo_bridge, resource_allocation).
narrative_ontology:affects_constraint(cs_ecmo_bridge, organ_transplant_scarcity).
narrative_ontology:affects_constraint(cs_ecmo_bridge, waitlist_allocation_urgency).
narrative_ontology:affects_constraint(cs_ecmo_bridge, transplant_center_concentration).

% DUAL FORMULATION NOTE:
% ECMO bridge is downstream of organ scarcity but represents a distinct structural constraint. Organ scarcity (ε≈0.70, mountain-like) is the upstream immutable constraint; ECMO bridge (ε≈0.52, tangled_rope) represents a technological response that creates its own extraction mechanism through differential access. The two constraints are linked: ECMO appears as pure coordination IF donor organs were abundant, but under scarcity, it becomes a mechanism for institutional access bias.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cs_ecmo_bridge, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
