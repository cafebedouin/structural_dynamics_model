% ============================================================================
% CONSTRAINT STORY: fmeca_procedures_1980
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: MIL-STD-1629A (FMECA Procedures)
 *   domain: technological/military/legal
 *
 * SUMMARY:
 *   MIL-STD-1629A established mandatory Failure Mode, Effects, and
 *   Criticality Analysis (FMECA) procedures for U.S. Department of Defense
 *   systems acquisition in 1980. The standard represents a hybrid constraint:
 *   a genuine coordination mechanism for enforcing risk discipline across
 *   thousands of defense contractors, combined with procedural extraction
 *   that benefits the DoD acquisition bureaucracy and creates overhead rents
 *   for the consulting industry. The constraint exhibits all six
 *   classification types from different structural positions. For large
 *   contractors with diverse portfolios, FMECA is coordination (Rope) — they
 *   have arbitrage power and genuinely integrate failure analysis into
 *   design. For small suppliers, FMECA is extraction (Snare) — trapped by
 *   contractual requirement, bearing overhead without negotiating power. For
 *   program management offices, it is hybrid (Tangled Rope) — they enforce a
 *   genuine discipline while deferring risk accountability to the procedure.
 *   For the consulting industry, it is degraded ritual (Piton) — maintaining
 *   itself through institutional inertia despite alternative methods being
 *   available. For reform-minded industry coalitions, it is a temporary
 *   problem being solved (Scaffold) — modern software testing and agile risk
 *   methods create pathways around traditional FMECA. The analytical observer
 *   risks naturalizing the standard as an immutable law (Mountain) — the
 *   necessity for failure analysis is real, but the 1980-era procedure itself
 *   is contingent and declining in functionality.
 *
 * KEY AGENTS:
 *   - DoD Acquisition Bureaucracy: Primary beneficiary (institutional/arbitrage) — imposes standardized requirements, controls supplier behavior, delegates risk accountability
 *   - Defense Contractors (Large): Secondary beneficiary (powerful/arbitrage) — can negotiate FMECA scope, integrate with own risk processes, use compliance as market differentiation
 *   - Small Defense Suppliers: Primary victim (powerless/trapped) — cannot exit contracts, absorb overhead regardless of system complexity, no negotiating power on scope
 *   - Program Management Offices: Moderate actor (moderate/constrained) — benefit from structured risk discipline but constrained by requirement to impose procedure; use FMECA as risk proxy
 *   - FMECA Consulting Industry: Institutional actor (institutional/arbitrage) — maintains procedure through contractual demand; maintains itself through inertia despite declining functional necessity
 *   - Commercial Defense Industry Reform Coalition: Organized agents (organized/constrained) — developing alternative verification pathways (agile risk, continuous integration); constrained by regulatory requirement but building exit path
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to failure analysis necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fmeca_procedures_1980, 0.38).
domain_priors:suppression_score(fmeca_procedures_1980, 0.52).
domain_priors:theater_ratio(fmeca_procedures_1980, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fmeca_procedures_1980, extractiveness, 0.38).
narrative_ontology:constraint_metric(fmeca_procedures_1980, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fmeca_procedures_1980, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fmeca_procedures_1980, tangled_rope).
narrative_ontology:human_readable(fmeca_procedures_1980, "MIL-STD-1629A (FMECA Procedures)").
narrative_ontology:topic_domain(fmeca_procedures_1980, "technological/military/legal").

domain_priors:requires_active_enforcement(fmeca_procedures_1980).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fmeca_procedures_1980, defense_contractors).
narrative_ontology:constraint_beneficiary(fmeca_procedures_1980, military_procurement_bureaucracy).
narrative_ontology:constraint_beneficiary(fmeca_procedures_1980, risk_management_certification_industry).
narrative_ontology:constraint_victim(fmeca_procedures_1980, program_cost_containment).
narrative_ontology:constraint_victim(fmeca_procedures_1980, system_design_agility).
narrative_ontology:constraint_victim(fmeca_procedures_1980, small_defense_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL SUPPLIER (SNARE) — Trapped in compliance burden. Cannot exit DoD contract work without abandoning that revenue stream. Must absorb FMECA overhead regardless of system complexity or risk profile. No flexibility in methodology or scope. Bears extraction cost without meaningful exit path.
constraint_indexing:constraint_classification(fmeca_procedures_1980, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRAM MANAGEMENT OFFICE (TANGLED ROPE) — Constrained by regulatory requirement to mandate FMECA, but also genuinely benefits from structured failure analysis discipline. The procedure enforces risk discipline and creates institutional accountability. However, extraction occurs through procedural overhead: PMOs can defer to FMECA compliance as risk justification without conducting independent analysis. Coordination benefit (structured risk thinking) plus extractive benefit (procedural liability transfer).
constraint_indexing:constraint_classification(fmeca_procedures_1980, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DoD ACQUISITION BUREAUCRACY (ROPE) — Primary beneficiary. FMECA procedures establish institutional power: procurement offices can impose standardized requirements, contractors must comply, and compliance is delegated to external contractors/consultants. The constraint solves a genuine coordination problem (how to enforce risk discipline across thousands of suppliers) while creating extractive rents (consulting fees, contractor overhead). Can arbitrage between competing contractors; can modify FMECA scope and interpretation to favor preferred suppliers.
constraint_indexing:constraint_classification(fmeca_procedures_1980, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMERCIAL INDUSTRY REFORM (SCAFFOLD) — Organized agents (industry consortia, government efficiency advocates, software-centric defense firms) see FMECA as a temporary procedural constraint with a sunset. Modern software testing practices, agile risk management, and continuous integration/deployment create alternative verification pathways that bypass traditional FMECA's static analysis model. The constraint persists due to institutional inertia but is being superseded by adaptive methods. Theater ratio declining as practices shift.
constraint_indexing:constraint_classification(fmeca_procedures_1980, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FMECA CONSULTING ESTABLISHMENT (PITON) — Institutional actor maintaining the procedural ritual despite degraded functionality. FMECA analysis is often performed as compliance theater: contractors hire certified FMECA consultants, generate required documentation, present findings to PMO, then design and operate systems using alternative risk methods (design reviews, failure prediction, reliability growth testing). The procedure persists through contractual requirement and institutional inertia, not because it's the primary mechanism by which risk discipline is actually enforced.
constraint_indexing:constraint_classification(fmeca_procedures_1980, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, structured failure analysis is a fundamental requirement of complex system design: you cannot responsibly operate safety-critical systems without systematically identifying and analyzing failure modes. FMECA codifies this necessity. However, the mountain classification is vulnerable to false summit detection: the necessity is for failure analysis, not for the specific 1980-era procedure and its theater elements. The procedure itself is contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(fmeca_procedures_1980, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fmeca_procedures_1980_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fmeca_procedures_1980, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fmeca_procedures_1980, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fmeca_procedures_1980, TR),
    TR >= 0.70.

:- end_tests(fmeca_procedures_1980_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The constraint transfers significant overhead cost to contractors, particularly small suppliers. FMECA documentation, analysis, and revision cycles consume engineering resources and calendar time. However, extraction is not maximal (0.66+) because some of the overhead serves genuine risk discipline — failure analysis is real work, not pure waste. The extraction occurs through procedural overkill: requiring the same analysis depth and documentation rigor regardless of system complexity or risk profile. Suppression (0.52): Moderate-high. Contractors cannot negotiate FMECA scope or depth; the procedure is mandated in contract terms. Suppliers cannot avoid the cost without exiting defense contracts. However, suppression is not total (0.60+) because alternatives exist (though they are not currently accepted in DoD contracts). There is institutional flexibility to modify the standard, but doing so requires government-level action. Theater ratio (0.65): Moderate-high. A significant fraction of FMECA activity is compliance documentation rather than generative analysis. Consultants are hired to produce FMECA reports that satisfy contractual requirements; the actual failure analysis discipline often occurs through parallel engineering processes (design reviews, testing, reliability modeling). The theater ratio has increased over the interval (from 0.42 to 0.65) as the standard has aged and alternative methods have become available, making the procedure's necessity less obvious.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the DoD acquisition bureaucracy's experience (Rope: coordination mechanism with net benefit to the imposed standard) and small suppliers' experience (Snare: pure extraction with trapped exit). Large contractors occupy an intermediate position (Rope to Tangled Rope depending on negotiating power). The PMO's Tangled Rope perspective arises because the requirement benefits PMOs (structured risk discipline) while extracting from them (accountability deferral to procedure). The consulting industry's Piton perspective reflects that the procedure persists despite declining functional necessity — the ritual is maintained through contractual requirement and institutional inertia, not because FMECA is the primary mechanism by which risk is actually managed in modern defense programs. The commercial reform coalition's Scaffold perspective reflects genuine structural change: agile methods, continuous integration, and property-based testing provide alternative verification pathways that bypass traditional FMECA's static analysis model. The analytical observer's Mountain perspective risks false summit — the necessity for failure analysis is real, but the 1980-era procedure is contingent and increasingly obsolescent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. The DoD bureaucracy benefits from imposed standard + arbitrage options → low d → negative/low χ (experienced as Rope). Large contractors benefit + have some negotiating power + mobile/arbitrage → low-moderate d → moderate χ (experienced as Rope). Small suppliers bear full cost + trapped exit + no negotiating power → high d → high χ (experienced as Snare). PMOs enforce requirement + benefit from discipline + constrained by requirement → moderate d → moderate χ (experienced as Tangled Rope). The consulting industry benefits from ongoing contract requirement + arbitrage → low-moderate d → moderate χ (experienced as Piton due to high theater ratio rather than high extractiveness). Reform coalition is organized + constrained + sees exit path → moderate d → moderate χ (experienced as Scaffold due to sunset clause perception). The analytical observer sits at universal scope + civilizational time horizon → d=0.72 (canonical analytical) → high f(d) → would suggest Snare, but the false summit detection gate identifies the mountain claim as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR OF MANDATROPHY RESOLUTION: This constraint demonstrates how the mandatrophy is resolved by distinguishing between the necessity of the functional requirement (failure analysis) and the contingency of the institutional procedure (MIL-STD-1629A). The false summit detector identifies the analytical observer's mountain classification as naturalizing a contingent procedure as an immutable law. The true necessity (failure analysis of complex systems) is real and immutable; the institutional procedure is not. The constraint resolves mandatrophy by decomposing: (1) the underlying functional requirement (necessity — mountain-class), and (2) the 1980-era standard and its procedural overhead (contingent institutional arrangement — tangled_rope/scaffold/piton depending on perspective and time horizon). The standard itself exhibits theater ratio drift (0.42 → 0.65 over 20 years) indicating that the procedure's functional necessity is declining relative to its performative content. This suggests the constraint is transitioning from Tangled Rope (genuine coordination + extraction) toward Piton (degraded ritual) and toward its sunset as alternative methods mature (Scaffold perspective). The extraction is real and quantifiable (overhead cost), but it is separable from the underlying necessity for risk discipline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fmeca_necessity_vs_procedure,
    'Is the mandatory extraction cost attributable to FMECA''s intrinsic necessity, or to the specific 1980-era standard''s procedural overhead?',
    'Comparative analysis: cost/schedule impact of FMECA-compliant programs vs. modern risk-engineering practices (continuous integration, property-based testing, formal verification); correlation between FMECA procedural scope and risk reduction achieved',
    'If intrinsic necessity: constraint classification remains tangled_rope across all contexts. If procedural overhead: classification shifts to scaffold/piton, indicating the specific standard is degraded and can be replaced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fmeca_necessity_vs_procedure, empirical, 'Whether extraction cost is inherent to failure analysis or to the 1980 standard''s design').

omega_variable(
    small_supplier_exit_cost,
    'What proportion of small DoD suppliers would exit the contracting pool if FMECA compliance were discretionary rather than mandatory?',
    'Industry survey of compliance cost as percentage of contract value; analysis of margin pressure on different supplier segments; historical analysis of supplier concentration trends before/after MIL-STD-1629A adoption',
    'If >30% would exit: suppression is high (trapped exit is real). If <10% would exit: suppression is moderate (some suppliers have option value in defense work).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_supplier_exit_cost, empirical, 'Proportion of small suppliers with economically-forced participation').

omega_variable(
    pmoa_independent_analysis,
    'What fraction of Program Management Office risk decisions are made independent of FMECA findings, vs. deferring to FMECA as proxy?',
    'PMO decision log analysis: compare programs with and without FMECA requirements; measure correlation between FMECA risk ratings and actual design decisions; assess PMO risk capacity vs. FMECA complexity',
    'If >60% defer: extraction mechanism confirmed (PMO uses FMECA as liability shield). If <30% defer: coordination benefit is primary (PMO uses FMECA as input to independent analysis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pmoa_independent_analysis, empirical, 'Whether PMO conducts independent risk analysis or defers to FMECA').

omega_variable(
    alternative_method_effectiveness,
    'Do agile risk methods (continuous failure tracking, automated testing, design review integration) achieve equivalent or superior risk discipline compared to FMECA''s static upfront analysis?',
    'Comparative failure rate analysis: FMECA-mandated programs vs. modern risk-engineered programs; time-to-detection of critical failure modes; rework rates; correlation between method used and post-delivery reliability growth',
    'If superior/equivalent: scaffold perspective confirmed — alternative methods can replace FMECA, establishing sunset path. If inferior: FMECA remains necessary despite procedural overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_method_effectiveness, empirical, 'Relative effectiveness of agile risk methods vs. FMECA').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fmeca_procedures_1980, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmeca_tr_t0, fmeca_procedures_1980, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fmeca_tr_t10, fmeca_procedures_1980, theater_ratio, 10, 0.55).
narrative_ontology:measurement(fmeca_tr_t20, fmeca_procedures_1980, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(fmeca_be_t0, fmeca_procedures_1980, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fmeca_be_t10, fmeca_procedures_1980, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(fmeca_be_t20, fmeca_procedures_1980, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fmeca_procedures_1980, enforcement_mechanism).
narrative_ontology:affects_constraint(fmeca_procedures_1980, defense_acquisition_cost_escalation).
narrative_ontology:affects_constraint(fmeca_procedures_1980, supplier_consolidation_barrier).
narrative_ontology:affects_constraint(fmeca_procedures_1980, military_technology_agility).

% DUAL FORMULATION NOTE:
% MIL-STD-1629A is a specific institutional instantiation of the more general constraint that complex systems require failure analysis. The underlying functional requirement (failure analysis necessity) is a mountain-class constraint; the 1980-era procedure with its modern overhead is a tangled_rope/scaffold/piton hybrid. These are decomposed into separate logical constraints in the network: the functional necessity is invariant; the procedural form is path-dependent and declining in functionality as alternative methods mature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fmeca_procedures_1980, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
