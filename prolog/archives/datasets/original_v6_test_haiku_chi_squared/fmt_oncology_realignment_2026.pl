% ============================================================================
% CONSTRAINT STORY: fmt_oncology_realignment_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fmt_oncology_realignment_2026, []).

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
 *   constraint_id: fmt_oncology_realignment_2026
 *   human_readable: FMT Immunotherapy Realignment in Oncology (2026)
 *   domain: health/medical/oncology/immunotherapy
 *
 * SUMMARY:
 *   In late January 2026, Phase I and Phase II clinical trial data
 *   demonstrated that oral fecal microbiota transplantation (FMT) pills
 *   significantly reduce immunotherapy toxicity and improve cancer response
 *   rates when co-administered with checkpoint inhibitor monotherapies
 *   (anti-PD-1, anti-CTLA-4). This represents a fundamental realignment in
 *   the immunotherapy ecosystem: the standard-of-care treatment paradigm
 *   shifts from 'optimize checkpoint inhibitor dosing and toxicity
 *   management' to 'co-treat with microbiota-based interventions to improve
 *   both safety and efficacy.' The constraint captures the structural tension
 *   between immediate exploitation of the new therapeutic pathway (extraction
 *   of value by manufacturers, early-adopter centers, and
 *   treatment-responsive patients) and the longer-term ecosystem cost
 *   (delayed access for patients outside early-adopter centers, disruption of
 *   established immunotherapy protocols, manufacturing bottlenecks, vendor
 *   lock-in risk). The realignment exhibits characteristics of a Tangled
 *   Rope: there is genuine coordination function (microbiota composition does
 *   improve immune tolerance and response), but this coordination is bundled
 *   with asymmetric extraction (manufacturing control, treatment access
 *   inequality, protocol disruption). Theater is present but moderate (58%):
 *   some of the regulatory and reimbursement delay reflects genuine safety
 *   evaluation, but much reflects institutional inertia and risk-aversion
 *   theater.
 *
 * KEY AGENTS:
 *   - Metastatic cancer patients (powerless/trapped): Primary victims — trapped in standard protocols while efficacy of FMT co-treatment is proven but not yet available at their centers; face 6-18 month delays in access depending on geographic location and insurance coverage
 *   - FMT manufacturing companies & clinical trial sponsors (institutional/arbitrage): Primary beneficiaries — experience the constraint as coordination opportunity; capture manufacturing advantage, clinical partnership premiums, and first-mover brand equity in microbiota-based oncology
 *   - Regional oncology programs (moderate/constrained): Secondary actors — constrained by capital costs, sourcing complexity, and protocol integration barriers; also benefit from improved patient outcomes and competitive advantage if they adopt early
 *   - Traditional immunotherapy ecosystem (organized/constrained): Secondary actors — organized checkpoint inhibitor manufacturers, toxicity management specialists, clinical guideline committees; experience extraction threat (value proposition undermined) and coordination benefit (better outcomes) simultaneously
 *   - Regulatory & reimbursement infrastructure (institutional/arbitrage): Institutional actor — manages FDA approval pathways and insurance coverage via theater-heavy processes (18-36 month cycles); functionally beneficiary but operationally slow
 *   - Open microbiota standards initiative (organized/mobile): Organized coalition — working toward decentralized manufacturing standards and open-source microbiota characterization; sees constraint as temporary (Scaffold with clear 5-10 year sunset)
 *   - Analytical observer (analytical/analytical): Civilizational perspective — risks naturalizing the realignment as 'revealing a law of tumor immunology' rather than 'capturing an institutional opportunity'; engine will flag as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fmt_oncology_realignment_2026, 0.52).
domain_priors:suppression_score(fmt_oncology_realignment_2026, 0.68).
domain_priors:theater_ratio(fmt_oncology_realignment_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fmt_oncology_realignment_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(fmt_oncology_realignment_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fmt_oncology_realignment_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fmt_oncology_realignment_2026, tangled_rope).
narrative_ontology:human_readable(fmt_oncology_realignment_2026, "FMT Immunotherapy Realignment in Oncology (2026)").
narrative_ontology:topic_domain(fmt_oncology_realignment_2026, "health/medical/oncology/immunotherapy").

domain_priors:requires_active_enforcement(fmt_oncology_realignment_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fmt_oncology_realignment_2026, fmt_manufacturing_companies).
narrative_ontology:constraint_beneficiary(fmt_oncology_realignment_2026, gastroenterology_specialists).
narrative_ontology:constraint_beneficiary(fmt_oncology_realignment_2026, early_adopter_oncology_centers).
narrative_ontology:constraint_victim(fmt_oncology_realignment_2026, cancer_patients_delayed_access).
narrative_ontology:constraint_victim(fmt_oncology_realignment_2026, traditional_immunotherapy_ecosystem).
narrative_ontology:constraint_victim(fmt_oncology_realignment_2026, emerging_market_oncology_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENTS WITHOUT EARLY ACCESS (SNARE) — Trapped in standard immunotherapy protocols with known toxicity profiles while early-adopter centers deploy FMT co-treatment. No exit from geographic location, insurance coverage, or clinical availability. Bears full cost of delayed access during the critical window when oral FMT efficacy is proven but not yet standard-of-care. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL ONCOLOGY PROGRAMS (TANGLED ROPE) — Constrained by capital equipment costs, microbiota sourcing/manufacturing complexity, and integration barriers into existing immunotherapy protocols. Also benefit from improved patient outcomes, reduced adverse event hospitalizations, and competitive advantage if they adopt early. Mixed experience: both extraction (cost barriers, vendor lock-in risk) and genuine coordination (outcome improvement). d≈0.58, f(d)≈0.70, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FMT MANUFACTURERS & SPONSORS (ROPE) — Experience the constraint as pure coordination: the January 2026 Phase I/II data creates urgency to establish manufacturing capacity, clinical protocols, and regulatory pathways. Early-mover advantage in manufacturing and clinical partnerships is substantial but the constraint is not extractive — they solve a real problem (immunotherapy toxicity). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADITIONAL IMMUNOTHERAPY ECOSYSTEM (TANGLED ROPE) — Organized actors (checkpoint inhibitor manufacturers, clinical protocol committees, toxicity management specialists) experience this as both coordination failure and extraction threat. Benefit from robust patient outcomes, reduced adverse event costs, and new treatment paradigms (genuine coordination function). But also face extraction as FMT co-treatment undermines the 'immunotherapy monotherapy' value proposition and creates new vendor dependencies. Constrained by sunk investment in existing protocols and physician training. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY & REIMBURSEMENT INFRASTRUCTURE (PITON) — FDA approval pathways, insurance coverage determination, and clinical guideline committees operate on 18-36 month cycles (theater_ratio=0.58: substantial performative review). The formal process of evaluating FMT co-treatment efficacy and safety is functional but slow; much discussion focuses on precedent and risk-mitigation theater rather than direct outcome evaluation. Institutional inertia keeps this constraint alive despite new evidence. d≈0.05, f(d)≈-0.10, σ=1.1 → χ≈-0.03 (structural beneficiary but classified as piton via theater gate).
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN MICROBIOTA STANDARDS INITIATIVE (SCAFFOLD) — Organized consortium working on open-source microbiota characterization standards, decentralized manufacturing protocols, and global access frameworks sees the constraint as temporary coordination failure with a clear sunset. The goal is to transition from proprietary FMT pills to standardized, decentralized microbiota-based interventions (donor screening, fermentation, formulation) that reduce vendor lock-in and manufacturing bottlenecks. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.18. Has clear sunset: 5-10 years to establish global manufacturing capacity and open standards.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (FALSE SUMMIT) — From a civilizational/universal perspective, cancer immunotherapy toxicity is an immutable physical constraint: checkpoint inhibitor therapy activates immune cells that can attack normal tissue (CTLA-4, PD-1 blockade). Microbiota composition affects immune tolerance and Treg differentiation by fundamental molecular biology. This perspective risks classifying the realignment as 'revealing a natural law of immuno-oncology' rather than 'capturing an institutional opportunity.' However, base metrics (ε=0.52, suppression=0.68, theater=0.58) indicate contingent extraction, not immutable constraint. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fmt_oncology_realignment_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fmt_oncology_realignment_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fmt_oncology_realignment_2026, TR),
    TR >= 0.70.

:- end_tests(fmt_oncology_realignment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The January 2026 trial data creates an immediate access inequality between early-adopter centers (mostly in high-income regions, academic medical centers) and peripheral sites. Manufacturing constraints (donor screening, microbiota characterization, formulation stability, cold chain logistics) create temporary scarcity that FMT manufacturers can extract value from through pricing power and partnership leverage. However, extractiveness is not as severe as a pure Snare (0.66+) because the therapeutic benefit is real and the constraint is not purely coercive — patients do benefit from access, and regional programs do improve outcomes. The 0.52 value reflects 'genuine innovation bundled with immediate inequality.' Suppression (0.68): Moderate-high. Significant barriers to rapid diffusion include: (1) manufacturing bottleneck (capacity ramp limited by GMP compliance, donor sourcing), (2) regulatory uncertainty (FDA 'live biotherapeutic product' guidance is recent and strict), (3) clinical protocol integration complexity (requires gastroenterology and oncology coordination), (4) reimbursement uncertainty (insurance coverage timelines are slow). However, suppression is not total (0.90+) because academic research institutions and some early-adopter centers are already implementing protocols, and the scientific pathway is clear. Theater ratio (0.58): Moderate. Regulatory review is partly functional (genuine safety and efficacy evaluation) but partly performative (risk-aversion theater, precedent-driven delays). Clinical guideline committee discussions will be substantive but also include 'we must wait for more data' theater. The theater is increasing over time as more clinical data accumulate but regulatory processes remain slow.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is stark and follows from exit option differentiation. Patients without early access (trapped) see a Snare: they are excluded from a proven benefit for reasons beyond their control. Regional oncology programs (constrained) see Tangled Rope: they are both constrained by costs and beneficiary from outcomes. FMT manufacturers (arbitrage) see Rope: they solve a coordination problem. The traditional immunotherapy ecosystem (constrained) sees Tangled Rope: they benefit from better outcomes but suffer extraction as their value proposition is disrupted. The open standards coalition (mobile) sees Scaffold: they have agency and can exit the proprietary manufacturing constraint through standards development. The regulatory system (arbitrage) structurally sees Piton: it maintains a performative review ritual despite clear efficacy data. The analytical observer risks seeing Mountain: 'tumor microbiota interactions are a law of immunobiology.' The gap between the Snare perspective (patients) and the Rope perspective (manufacturers) is maximal — the same constraint is simultaneously a pure extraction mechanism (no exit, full cost) and a pure coordination mechanism (solving a real problem) depending on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Metastatic patients without early access: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. FMT manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Regional oncology programs: Mixed (victim of costs + beneficiary of outcomes) + constrained → d≈0.58, f(d)≈0.70. Moderate-to-high directionality reflecting constraint cost. Traditional immunotherapy ecosystem: Mixed (victim of disruption + beneficiary of outcomes) + constrained → d≈0.62, f(d)≈0.82. Higher directionality reflecting disruption threat. Open standards coalition: Beneficiary of solving problem + mobile → d≈0.35, f(d)≈0.30. Lower directionality because coalition has agency and clear path forward. Regulatory system: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Piton classification comes from theater gate (0.58), not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Is this a Rope (coordination mechanism enabling better cancer treatment) or a Snare (access inequality and vendor lock-in)?' The data resolves this as follows: (1) From the perspective of patients with access and regional programs with capital: Rope. The coordination function is genuine. (2) From the perspective of patients without access and the ecosystem disruption: Snare. The extraction is real. (3) The engine's tangled_rope classification at the primary analytical level (ε=0.52, suppression=0.68, χ at moderate institutional level) correctly captures that both are true simultaneously. The constraint is a Tangled Rope because it has both genuine coordination (improved efficacy, reduced toxicity) AND asymmetric extraction (access inequality, vendor lock-in, ecosystem disruption). The mandatrophy resolves by refusing the binary choice: the constraint IS both Rope (coordination view) and Snare (extraction view), depending on structural position. The resolution is perspectival, not analytical — all perspectives are valid readings of the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    responder_microbiota_causality,
    'Is the improved immunotherapy response in FMT-treated patients causally driven by microbiota composition, or by patient selection bias (only treatment-responsive patients receive FMT co-treatment in early-adopter centers)?',
    'Randomized controlled trial with prospective microbiota sampling; blinded outcome assessment; correlation between specific taxa abundance and response vs non-response; cross-trial meta-analysis for consistency of responder phenotype',
    'If causal: FMT co-treatment is genuine innovation (Rope, Scaffold from many perspectives). If selection bias: realignment is temporary (Piton classification), and the extraction window closes as patient selection criteria become explicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responder_microbiota_causality, empirical, 'Whether FMT efficacy is causally driven by microbiota or patient selection bias').

omega_variable(
    manufacturing_bottleneck_permanence,
    'Are manufacturing constraints for oral FMT pills (donor screening, quality control, stability, cold chain) inherent to the product or temporary until capacity scales?',
    'Analysis of manufacturing cost curves, regulatory pathway timelines (FDA guidance on ''live biotherapeutic products''), and capital investment needed for decentralized vs centralized production. Comparison with other probiotic/microbiota-based products (VSL#3, SER-109, fecal capsule manufacturing).',
    'If inherent: extraction window persists (Snare for patients, Tangled Rope for programs). If temporary: scaffold sunset is real (5-10 years to mature production).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_bottleneck_permanence, empirical, 'Whether FMT manufacturing bottlenecks are inherent or temporary').

omega_variable(
    insurance_coverage_precedent,
    'Will major insurance systems cover FMT co-treatment as part of standard immunotherapy protocols, or require cost-sharing / out-of-pocket payment from patients?',
    'Analysis of insurance coverage decisions for similar ''combination therapy'' innovations (e.g., PD-1 + CTLA-4 dual checkpoint inhibition coverage timelines); interviews with payers regarding cost-effectiveness thresholds for microbiota-based interventions; projection of claims data from early-adopter centers',
    'If covered: access barriers drop significantly within 2-3 years (Snare window shortens). If cost-shared: geographic inequality persists (Snare deepens for lower-income patients).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_coverage_precedent, empirical, 'Whether major insurers will cover FMT co-treatment').

omega_variable(
    vendor_lock_in_risk,
    'Will FMT pill manufacturing consolidate around proprietary microbiota formulations (creating long-term vendor lock-in and extraction), or transition to open-source standards (enabling decentralized manufacturing)?',
    'Patent analysis (proprietary vs licensed intellectual property); manufacturing partnerships (exclusive vs non-exclusive agreements); open standards initiatives (progress on microbial strain libraries, fermentation protocols); regulatory guidance on ''defined vs undefined'' microbiota products',
    'If proprietary consolidation: extraction persists indefinitely (Snare, Tangled Rope permanent). If open standards: Scaffold sunset becomes real (10-15 year transition to decentralized production).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_lock_in_risk, preference, 'Whether FMT manufacturing will consolidate proprietary or transition to open standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fmt_oncology_realignment_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_tr_t0, fmt_oncology_realignment_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fmt_tr_t6, fmt_oncology_realignment_2026, theater_ratio, 6, 0.52).
narrative_ontology:measurement(fmt_tr_t12, fmt_oncology_realignment_2026, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(fmt_be_t0, fmt_oncology_realignment_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fmt_be_t6, fmt_oncology_realignment_2026, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(fmt_be_t12, fmt_oncology_realignment_2026, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fmt_oncology_realignment_2026, resource_allocation).
narrative_ontology:affects_constraint(fmt_oncology_realignment_2026, checkpoint_inhibitor_toxicity_gradient).
narrative_ontology:affects_constraint(fmt_oncology_realignment_2026, microbiota_manufacturing_standardization).
narrative_ontology:affects_constraint(fmt_oncology_realignment_2026, oncology_center_protocol_fragmentation).

% DUAL FORMULATION NOTE:
% The FMT realignment decomposes into three linked constraints: (1) checkpoint inhibitor toxicity gradient (ε≈0.15, Mountain-like: inherent to immune activation), (2) FMT manufacturing standardization (ε≈0.58, Tangled Rope: genuine coordination bundled with vendor lock-in), (3) oncology center protocol fragmentation (ε≈0.42, Tangled Rope: coordination problem + access inequality). Each has distinct ε and perspectives; they form a family linked by affects_constraints. The present story focuses on (2).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fmt_oncology_realignment_2026, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
