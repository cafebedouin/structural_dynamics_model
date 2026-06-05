% ============================================================================
% CONSTRAINT STORY: fmeca_procedures_1980
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fmeca_procedures_1980, []).

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
 *   constraint_id: fmeca_procedures_1980
 *   human_readable: MIL-STD-1629A FMECA Procedures Mandate
 *   domain: technological/military/legal
 *
 * SUMMARY:
 *   MIL-STD-1629A establishes mandatory Failure Mode, Effects, and
 *   Criticality Analysis (FMECA) procedures for all Defense Department
 *   systems. Since its adoption in 1980, the standard has served a genuine
 *   coordination function: ensuring defense systems meet standardized
 *   reliability requirements across fragmented supply chains, reducing
 *   catastrophic failure risk, and enabling interoperability verification
 *   without duplicative proprietary testing. However, over 46 years, the
 *   constraint has accumulated procedural theater, become a significant
 *   barrier to new entrant defense contractors, and created acute friction
 *   with modern rapid-development cycles (particularly in AI/ML and
 *   autonomous systems). The constraint exhibits the classic Tangled Rope
 *   signature: a genuine coordination benefit (standardized reliability
 *   verification) coupled with asymmetric extraction (small contractors bear
 *   disproportionate compliance costs; established firms amortize overhead
 *   across multiple programs). The theater ratio (0.65) reflects that modern
 *   FMECA analyses often devolve into template-driven box-checking rather
 *   than genuine failure mode discovery, particularly in domains where
 *   FMECA's classical fault-tree methodology struggles (AI systems, emergent
 *   behaviors, adversarial failure modes). The extractiveness trajectory
 *   (0.28→0.52 over 46 years) shows increasing burden as system complexity
 *   has outpaced verification capacity, forcing greater reliance on
 *   procedural compliance rather than substantive analysis. The constraint
 *   now poses a strategic question for DoD: whether the coordination benefit
 *   justifies the innovation drag and market concentration effects, or
 *   whether alternative assurance frameworks (compositional verification,
 *   rapid certification tracks for low-risk domains) should be substituted
 *   for high-velocity development.
 *
 * KEY AGENTS:
 *   - DoD Acquisition Authority: Primary beneficiary (institutional/arbitrage) — solves critical coordination problem of standardized reliability verification across supply chains
 *   - Established Defense Contractors: Secondary beneficiary (powerful/arbitrage) — benefit from procedural barrier to entry; can amortize FMECA infrastructure across multiple programs
 *   - Small Defense Subcontractors: Primary victim (powerless/trapped) — bear full compliance burden per unit; cannot negotiate reduced requirements; face business failure if unable to comply
 *   - Rapid Innovation Cycles (AI/ML, Autonomous Systems): Victim (moderate/constrained) — FMECA delays time-to-market by 6-12 months; procedures incompatible with agile development
 *   - Systems Engineering Profession: Secondary beneficiary (organized/constrained) — FMECA procedures create professional employment and standardization; benefit from procedural authority
 *   - FMECA Compliance Bureaucracy: Institutional actor (institutional/constrained) — maintains procedural framework through regulatory inertia; acknowledges increasing theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies hybrid coordination-extraction structure; recognizes unintended market concentration and innovation inhibition effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fmeca_procedures_1980, 0.52).
domain_priors:suppression_score(fmeca_procedures_1980, 0.68).
domain_priors:theater_ratio(fmeca_procedures_1980, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fmeca_procedures_1980, extractiveness, 0.52).
narrative_ontology:constraint_metric(fmeca_procedures_1980, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fmeca_procedures_1980, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fmeca_procedures_1980, tangled_rope).
narrative_ontology:human_readable(fmeca_procedures_1980, "MIL-STD-1629A FMECA Procedures Mandate").
narrative_ontology:topic_domain(fmeca_procedures_1980, "technological/military/legal").

domain_priors:requires_active_enforcement(fmeca_procedures_1980).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fmeca_procedures_1980, dod_acquisition_authority).
narrative_ontology:constraint_beneficiary(fmeca_procedures_1980, systems_engineering_profession).
narrative_ontology:constraint_beneficiary(fmeca_procedures_1980, defense_contractors_established).
narrative_ontology:constraint_victim(fmeca_procedures_1980, small_defense_subcontractors).
narrative_ontology:constraint_victim(fmeca_procedures_1980, rapid_innovation_cycle).
narrative_ontology:constraint_victim(fmeca_procedures_1980, cost_constrained_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COST-CONSTRAINED SMALL SUBCONTRACTOR (SNARE) — Mandatory FMECA compliance with no discretion; bears full procedural overhead; lacks negotiating power to reduce documentation burden; cannot exit DoD supply chain without business failure. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(fmeca_procedures_1980, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RAPID INNOVATION CYCLE (SNARE) — Mandatory comprehensive FMECA delays time-to-market by 6-12 months; verification-heavy procedures incompatible with agile development; cannot exit without forfeiting defense contracts. Emerging technology domains (AI/ML, autonomous systems) experience acute constraint friction. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.69.
constraint_indexing:constraint_classification(fmeca_procedures_1980, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-SIZE DEFENSE CONTRACTOR NETWORK (TANGLED ROPE) — FMECA procedures create coordination benefit (shared reliability standards, interoperability verification) but also enforce asymmetric cost distribution. Larger contractors absorb overhead via scale; smaller contractors pay per-unit compliance cost. Constrained exit due to defense contract dependency but also organized response capacity. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(fmeca_procedures_1980, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOD ACQUISITION AUTHORITY (ROPE) — FMECA mandate solves critical coordination problem: ensures defense systems meet reliability requirements across supply chains; reduces catastrophic failure risk in deployed systems; enables standardized verification without proprietary duplication. Benefits from reduced liability and predictable system performance. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(fmeca_procedures_1980, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ESTABLISHED DEFENSE CONTRACTOR ELITE (TANGLED ROPE) — FMECA procedures create coordination benefit (standardized reliability requirements) but also serve as a barrier to entry for new competitors. Established firms can amortize FMECA infrastructure across multiple programs; new entrants must build capability before first contract. Benefits from constraint through network effects; minor cost burden due to scale. d≈0.32, f(d)≈0.25, σ=1.0 → χ≈0.13. Low effective extraction but asymmetric benefit.
constraint_indexing:constraint_classification(fmeca_procedures_1980, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FMECA COMPLIANCE BUREAUCRACY (PITON) — Maintenance and interpretation of FMECA procedures has become substantially performative. Theater_ratio=0.65 reflects: many FMECA analyses are template-driven box-checking rather than evidence-based failure mode discovery; verification personnel often unable to meaningfully assess highly complex failure modes (AI/ML systems, quantum-resilient cryptography); procedures persist through regulatory inertia despite diminishing functional returns. The bureaucracy itself benefits from constraint maintenance (job security, budget justification) but acknowledges the procedures as degraded.
constraint_indexing:constraint_classification(fmeca_procedures_1980, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view identifies genuine coordination benefit (reduced systemic failure risk in critical systems) but also recognizes asymmetric extraction through procedural compliance costs and market concentration effects. The constraint is neither a natural law nor pure extraction — it is a hybrid institutional arrangement that solves a real coordination problem while simultaneously creating unintended barriers to entry and innovation. d≈0.71, f(d)≈1.13, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(fmeca_procedures_1980, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fmeca_procedures_1980_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fmeca_procedures_1980, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fmeca_procedures_1980, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fmeca_procedures_1980, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fmeca_procedures_1980, TR),
    TR >= 0.70.

:- end_tests(fmeca_procedures_1980_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.52): Moderate-high. The constraint imposes substantial per-unit compliance costs (documentation, analysis personnel, verification labor) that disproportionately affect small contractors. However, the extraction is not as severe as pure Snare (0.66+) because the coordination benefit is genuine and measurable — preventing catastrophic failures in deployed systems generates real value. The midpoint value reflects that extraction is coupled with legitimate coordination function. Suppression (0.68): High. Mandatory compliance with no discretion; no legitimate exit path without forfeiting defense contracts; regulatory enforcement is strict. However, suppression is not absolute (0.75+) because established contractors have negotiating capacity and can structure compliance efficiently. Theater Ratio (0.65): Elevated. Modern FMECA analyses often become template-driven rather than genuinely investigative, particularly for complex systems. Many contractors maintain FMECA documentation primarily to satisfy contractual requirements rather than to drive design decisions. The theater ratio has risen from ~0.35 (1980) to 0.65 (present) as system complexity has exceeded FMECA's analytical capacity for emerging domains (AI/ML, adversarial failure modes, quantum systems).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical position transforms classification. The DoD Acquisition Authority experiences pure coordination benefit (Rope: d≈0.08) — FMECA solves their critical problem of standardized reliability verification. The established contractor elite experience low-level mixed benefit (Tangled Rope with d≈0.32) — they get barrier-to-entry protection and coordination benefits at moderate cost. The small subcontractor experiences severe extraction (Snare: d≈0.92) — full compliance burden, no negotiating power, trapped exit. The rapid innovation cycle experiences extraction (Snare: d≈0.88) — procedures delay deployment by months relative to benefit. The FMECA compliance bureaucracy experiences piton-status degradation (theater_ratio=0.65) — procedures persist through inertia despite acknowledged decline in functional returns. The analytical observer sees the true hybrid structure (Tangled Rope: d≈0.71) — genuine coordination coupled with unintended extraction and market concentration effects.
 *
 * DIRECTIONALITY LOGIC:
 *   DoD Acquisition Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; constraint solves their critical coordination problem. Established Defense Contractors: Beneficiary + arbitrage → d≈0.32, f(d)≈0.25. Low effective extraction; benefit from barrier-to-entry; scale advantages reduce per-unit cost. Small Subcontractors: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction; mandatory compliance, no exit, no negotiating power, disproportionate cost burden. Rapid Innovation Cycle: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction; constrained ability to modify procedures; cannot exit without forfeiting contracts. Systems Engineering Profession: Beneficiary + constrained → d≈0.42, f(d)≈0.42. Low effective extraction; benefit from professional standardization and employment; some constraint on methodology innovation. Analytical Observer: analytical → d≈0.71, f(d)≈1.13. Hybrid perspective; sees both genuine coordination and unintended extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY RISK. The constraint exhibits genuine coordination function (standardized reliability verification across supply chains) coupled with asymmetric extraction (compliance cost burden concentrated on small contractors and rapid-development programs). Classification as Tangled Rope is supported by: (1) beneficiaries (DoD, established contractors) derive measurable benefit; (2) victims (small subcontractors, innovation cycles) bear extraction costs; (3) active enforcement via contract requirements; (4) coordination function is not trivial — alternative verification frameworks would be complex and expensive. However, the trajectory suggests degradation toward pure extraction: extractiveness rising from 0.28 to 0.52 over 46 years; theater_ratio rising from 0.35 to 0.65; procedural gaming increasing; barrier-to-entry effects becoming more acute. MANDATROPHY RESOLUTION REQUIRED: Determine whether rising theater and extractiveness indicate (a) the coordination benefit is genuine and justified (Tangled Rope remains appropriate classification, constraint should persist with modification to reduce theater), or (b) the constraint has degraded into extraction camouflaged by procedural complexity (should be classified as Snare for most perspectives, requiring either significant reform or alternative frameworks for emerging domains). The emerging technology domains (AI/ML, autonomous systems, quantum cryptography) are the critical case: if FMECA cannot meaningfully analyze their failure modes, the constraint becomes pure theater for those domains and should be replaced with domain-specific assurance frameworks (Scaffold classification: temporary, with sunset toward alternative mechanisms).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fmeca_effectiveness_empirical,
    'What proportion of catastrophic failures in deployed DoD systems were actually predicted by FMECA procedures versus discovered post-failure?',
    'Retrospective analysis of system failure data post-2000; correlation between FMECA predictions and actual field failures; comparison with non-FMECA baseline systems',
    'If FMECA captures >80% of critical failures: coordination benefit is substantial, justifying extraction costs (Rope or Tangled Rope confirmed). If <50%: procedures are largely performative, extraction is unjustified (Snare confirmed for victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fmeca_effectiveness_empirical, empirical, 'Empirical predictive accuracy of FMECA procedures').

omega_variable(
    innovation_velocity_threshold,
    'At what development cycle speed does FMECA compliance create net negative returns (delays exceed failure risk reduction)?',
    'Longitudinal comparison of FMECA-constrained vs accelerated development programs; measurement of time-to-deployment costs vs prevented-failure benefits; case studies in AI/ML and autonomous systems',
    'If threshold <6 months: rapid innovation domains should be exempt (Scaffold or Snare for innovation cycle). If >18 months: FMECA remains justified across all programs (Rope or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_velocity_threshold, empirical, 'Development cycle speed threshold for FMECA cost-benefit reversal').

omega_variable(
    barrier_to_entry_quantification,
    'What fraction of new defense contractor formation is inhibited by FMECA infrastructure requirements relative to other barriers (capital, security clearance, facility certification)?',
    'Survey of failed new-entrant defense firms; cost-benefit analysis of FMECA capability building vs other startup costs; comparison with non-defense tech sectors',
    'If FMECA accounts for >30% of entry barrier: constraint is a significant anticompetitive mechanism (Snare for new entrants confirmed). If <10%: procedural burden is secondary to other barriers (Rope benefit justifies burden).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_to_entry_quantification, empirical, 'FMECA procedures as proportion of defense contractor entry barriers').

omega_variable(
    procedural_gaming_extent,
    'How extensively do contractors engage in FMECA box-checking (template filling) versus genuine failure mode investigation?',
    'Analysis of FMECA documentation from awarded contracts; qualitative assessment by independent systems engineers; comparison of template-reuse patterns across similar programs',
    'If >60% of analyses are template-driven: theater_ratio should be higher, classification shifts toward Piton (procedural degradation confirmed). If <30%: theater_ratio is lower, coordination benefit is more genuine (Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_gaming_extent, empirical, 'Extent of FMECA procedural gaming vs genuine analysis').

omega_variable(
    emerging_tech_fit,
    'Can FMECA procedures meaningfully analyze failure modes in emerging technology domains (AI/ML, quantum cryptography, biological systems)?',
    'Expert assessment of FMECA applicability to AI/ML training failure modes, adversarial perturbations, quantum error correction; case studies of AI systems subject to FMECA; comparison with alternative assurance frameworks',
    'If not applicable: emerging domains should have alternative assurance mechanisms (Scaffold for new tech, Snare for constrained innovators). If applicable with adaptation: unified procedure maintains Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_tech_fit, conceptual, 'FMECA procedural fit for emerging technology domains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fmeca_procedures_1980, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmeca_tr_t0, fmeca_procedures_1980, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fmeca_tr_t15, fmeca_procedures_1980, theater_ratio, 15, 0.52).
narrative_ontology:measurement(fmeca_tr_t30, fmeca_procedures_1980, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(fmeca_be_t0, fmeca_procedures_1980, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fmeca_be_t15, fmeca_procedures_1980, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(fmeca_be_t30, fmeca_procedures_1980, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fmeca_procedures_1980, enforcement_mechanism).
narrative_ontology:affects_constraint(fmeca_procedures_1980, defense_contractor_consolidation).
narrative_ontology:affects_constraint(fmeca_procedures_1980, acquisition_schedule_compression).
narrative_ontology:affects_constraint(fmeca_procedures_1980, ai_system_certification_bottleneck).

% DUAL FORMULATION NOTE:
% MIL-STD-1629A is the umbrella procedural constraint. Specific structural consequences decompose into separate constraints: (1) defense_contractor_consolidation (ε≈0.55, Snare for new entrants) — FMECA infrastructure requirements driving market concentration; (2) acquisition_schedule_compression (ε≈0.50, Tangled Rope) — FMECA delays coupled with schedule pressure create mixed coordination-extraction for program managers; (3) ai_system_certification_bottleneck (ε≈0.65, Snare for emerging tech) — FMECA procedures inadequate for AI failure modes, creating extraction without coordination benefit. These downstream constraints should each be evaluated independently; network links indicate that if FMECA is modified, all three will experience pressures to reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fmeca_procedures_1980, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
