% ============================================================================
% CONSTRAINT STORY: gig_economy_worker_protections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_worker_protections, []).

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
 *   constraint_id: gig_economy_worker_protections
 *   human_readable: Gig Economy Worker Protections Classification
 *   domain: labor/economic_policy
 *
 * SUMMARY:
 *   The gig economy worker protections constraint encompasses the structural
 *   relationship between digital platforms (Uber, DoorDash, TaskRabbit, Lyft,
 *   Instacart) and workers classified as independent contractors rather than
 *   employees. This classification regime enables platforms to avoid
 *   providing health insurance, retirement benefits, unemployment protection,
 *   workers' compensation, and wage guarantees that traditional employment
 *   law mandates. The constraint exhibits maximal perspectival divergence:
 *   platforms experience the system as pure coordination (matching workers to
 *   tasks efficiently); workers experience it as a snare with no exit; the
 *   state faces a tangled rope (genuine coordination problem of updating
 *   labor law alongside asymmetric extraction); employment law itself
 *   functions as theater (formalist contractor/employee distinction masking
 *   substantive platform control). Extractiveness has increased over the
 *   interval as platform dependence has deepened, suppression has risen
 *   through algorithmic control mechanisms and regulatory capture, and
 *   theater has grown as legal formalism increasingly diverges from
 *   operational reality (platforms exercise management authority while
 *   maintaining contractor classification through legal framing). This
 *   constraint is a canonical example of how regulatory arbitrage,
 *   technological opacity, and ideological naturalization combine to create
 *   durable extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victims (powerless/trapped) — economically dependent on platform income, subject to algorithmic control, no legal protections. Bears maximum extraction.
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — extract value through contractor classification, avoid benefits/regulation costs, control through algorithmic systems. Net beneficiaries.
 *   - Consumer Base: Secondary beneficiaries (powerful/arbitrage) — subsidized through suppressed worker costs, experience pure coordination benefit. Zero exit cost.
 *   - Precarious Worker Coalition: Organized victims (organized/constrained) — emerging labor organizing efforts face algorithmic deactivation threats and fragmentation. Constrained by suppression mechanisms.
 *   - Regulatory State: Institutional mediator (organized/constrained) — must balance tax base, social stability, and lobbying pressure from platforms; faces coordination problem (updating labor law) alongside extraction asymmetry.
 *   - Employment Law System: Institutional theater (institutional/arbitrage) — maintains formalist contractor/employee distinction that no longer maps to operational reality. Theater derives from regulatory capture and jurisdictional fragmentation.
 *   - Analytical Observer (Professional): Epistemic actor (analytical/identity_locked) — trained in efficiency/optimization frames that prevent perceiving worker vulnerability as structural. Identity fusion to platform-economy inevitability.
 *   - False Summit Perspective: Naturalization frame (analytical/analytical) — treats worker precarity as immutable coordination cost or natural law. Engine detects as false summit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_worker_protections, 0.68).
domain_priors:suppression_score(gig_economy_worker_protections, 0.72).
domain_priors:theater_ratio(gig_economy_worker_protections, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_worker_protections, extractiveness, 0.68).
narrative_ontology:constraint_metric(gig_economy_worker_protections, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gig_economy_worker_protections, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_worker_protections, snare).
narrative_ontology:human_readable(gig_economy_worker_protections, "Gig Economy Worker Protections Classification").
narrative_ontology:topic_domain(gig_economy_worker_protections, "labor/economic_policy").

domain_priors:requires_active_enforcement(gig_economy_worker_protections).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_worker_protections, platform_corporations).
narrative_ontology:constraint_beneficiary(gig_economy_worker_protections, consumer_base).
narrative_ontology:constraint_victim(gig_economy_worker_protections, gig_workers).
narrative_ontology:constraint_victim(gig_economy_worker_protections, traditional_employment_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Trapped by economic necessity in a system with minimal legal protections, no benefits, and algorithmic control. Cannot exit without abandoning income. Bears maximum cost of extraction through wage suppression, forced contractor status, and algorithmic discipline without traditional labor protections. Zero degrees of freedom.
constraint_indexing:constraint_classification(gig_economy_worker_protections, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS WORKER COALITION (SNARE) — Moderate power through potential collective action, but constrained by dispersed workforce, algorithmic deactivation threats, and fragmented labor law. Can theoretically exit but at high cost (job loss, blacklisting). Coalition visibility is growing but suppression mechanisms (gig-dependent income, platform control over algorithmic access) remain severe.
constraint_indexing:constraint_classification(gig_economy_worker_protections, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM CORPORATION (ROPE) — Experiences the classification as pure coordination with minimal friction: platforms provide logistics, payment processing, and algorithmic matching. Worker classification as contractor (not employee) is presented as a coordination innovation that enables efficient resource allocation. Net beneficiary — extraction accumulates toward the platform; exit costs for the platform are minimal (can relocate or restructure).
constraint_indexing:constraint_classification(gig_economy_worker_protections, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER BASE (ROPE) — Benefits from low prices and service availability made possible by suppressed worker costs. Experiences the constraint as pure coordination of supply and demand. Minimal awareness of extraction mechanism; exit costs are zero (can switch platforms or use alternatives). Net beneficiary through subsidy.
constraint_indexing:constraint_classification(gig_economy_worker_protections, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY STATE (TANGLED ROPE) — Faces genuine coordination problem (matching labor law to new economic models) alongside extraction beneficiary asymmetry (platforms extract value while retaining contractor classification). Must maintain tax base and social stability while platforms arbitrage legal boundaries. Constrained by lobbying power and jurisdictional fragmentation; organized enough to perceive the problem but lacks enforcement mechanism. Active enforcement gate triggers because platform lobbying actively prevents reclassification.
constraint_indexing:constraint_classification(gig_economy_worker_protections, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EMPLOYMENT LAW SYSTEM (PITON) — Post-industrial labor law is substantially theater: independent contractor vs employee distinction originated in 20th-century tax and union dynamics, but now functions primarily to obscure platform leverage. The classification ritual persists despite low correlation with actual worker dependence or platform control. Theater derives from the gap between formal legal status and substantive control mechanisms — platforms exercise management authority that would trigger employee status under a functional test, but maintain contractor classification through regulatory capture and jurisdictional arbitrage.
constraint_indexing:constraint_classification(gig_economy_worker_protections, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE with IDENTITY_LOCKED EXIT) — The analytical frame itself becomes identity-locked to the platform economy as inevitable and efficient. Analysts trained in network effects, optimization logic, and labor arbitrage struggle to perceive worker protections as anything other than friction costs. This perspective demonstrates the oracle gap: the analytical instruments prevent seeing the constraint structure that decomposing multiple perspectives reveals. The identity lock is professional/epistemic — efficiency frames make worker vulnerability literally unthinkable within the dominant analytical paradigm.
constraint_indexing:constraint_classification(gig_economy_worker_protections, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 8: FALSE SUMMIT VIEW — From a naïve naturalizing perspective, worker precarity in gig economy is treated as immutable: 'matching workers to tasks at scale requires minimal overhead', 'platform coordination has inherent costs', 'flexibility requires contractor status.' These frames present contingent institutional choices (classification regime, tax structure, minimum labor standards) as laws of nature. The engine detects this as a false summit through accessibility collapse and resistance metrics that do not support the mountain claim.
constraint_indexing:constraint_classification(gig_economy_worker_protections, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_worker_protections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_worker_protections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_worker_protections, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_worker_protections, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_economy_worker_protections, TR),
    TR >= 0.70.

:- end_tests(gig_economy_worker_protections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, increased from 0.45 at interval start. The constraint extracts from workers through suppressed wages, eliminated benefits, and shifted cost-bearing (healthcare, equipment, vehicle maintenance). The extraction is partially masked as coordination (efficient matching) and partially obscured through legal formalism (contractor status). The rising trajectory reflects deepening platform dependence and accumulation of control mechanisms (ratings systems, algorithmic task assignment, deactivation threats). Suppression (0.72): High. Multiple mechanisms prevent exit: economic necessity (gig income is primary for many workers), algorithmic dependence (workers cannot access tasks without platform's algorithmic system), regulatory arbitrage (contractor classification is legally sanctioned across jurisdictions despite worker substantive dependence), threat of deactivation (algorithmic exclusion from work), cognitive capture (efficiency narratives naturalize the arrangement as inevitable). Theater ratio (0.58): Moderate-high, increasing from 0.35. The primary theater is employment law formalism: contractor vs employee distinction is presented as a coherent legal category, but operationally platforms exercise management control (task assignment, performance rating, deactivation) that would trigger employee status under a functional dependence test. The theater has increased as platforms have invested in legal sophistication and lobbying while simultaneously tightening algorithmic control. Claimed type (snare): Multiple structural signatures support: high extractiveness (0.68 > 0.46 threshold), high suppression (0.72 > 0.60 threshold), no genuine coordination benefit to trapped workers (they do not choose to participate without economic necessity), no sunset clause (constraint is indefinite), no natural emergence (platform architecture and legal classification are designed and enforced, not emergent).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Platforms experience rope (pure coordination: they see themselves as solving logistics coordination, enabling flexible supply-side matching, reducing transaction costs). Consumers experience rope (they benefit from subsidized prices and convenience; exit cost is zero). Workers experience snare (trapped by necessity, subject to algorithmic control, no exit option that preserves income). The regulatory state experiences tangled rope (genuine coordination problem of updating labor law to digital economy, but also extraction asymmetry where platforms avoid social contribution costs). Employment law experiences piton (the contractor/employee distinction persists through institutional inertia despite functional dependence of modern gig workers on platforms). The analytical observer at the professional level experiences identity_locked snare (efficiency frames make precarity invisible; exit from the efficiency paradigm would require epistemic reorientation). The false summit perspective risks naturalizing contingent institutional choices (tax law history of contractor classification, legal sanctions for arbitrage) as immutable laws of economic coordination. The perspectival gap is maximal here: the same structural arrangement is simultaneously rope, snare, tangled_rope, piton, and false-summit depending on the agent and their exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's structural relationship to the extraction flow. Platform corporations are beneficiaries with arbitrage exit (can relocate, restructure, or exit markets) — d ≈ 0.05-0.15, f(d) ≈ -0.12 to -0.01, experienced χ is negative (they benefit). Workers are victims with trapped exit (economic necessity, algorithmic dependence, no alternative income source) — d ≈ 0.95, f(d) ≈ 1.42, experienced χ is maximal (full extraction). Consumers are beneficiaries with mobile/arbitrage exit (can choose alternative services, zero switching cost) — d ≈ 0.10, experienced χ is low or negative (subsidized). The precarious worker coalition is victims with constrained exit (can organize but face deactivation and fragmentation threats) — d ≈ 0.70-0.85, f(d) ≈ 1.15-1.28, experienced χ is high but not maximal. The regulatory state is both beneficiary (avoids upfront transition costs of reclassification) and victim (loses future tax base and social stability) — d ≈ 0.50-0.60, experienced χ is moderate-high. The employment law system is a beneficiary (maintains institutional role through formalist interpretation) with arbitrage (can shift interpretation if political will exists) — d ≈ 0.20, experienced χ is negative (system benefits from maintaining status quo). The analytical observer with identity_locked exit is structurally a beneficiary (efficiency frames benefit their epistemic community) but experientially perceives themselves as neutral observer — d ≈ 0.30-0.40 (true structural position as beneficiary), but self-perceived as d ≈ 0.50 (analytical neutrality) — the gap reveals the identity lock.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as snare (extractiveness 0.68 > 0.46 threshold, suppression 0.72 > 0.60 threshold, χ > 0.66 for trapped workers) yet demonstrates high dimensionality of perspectives (8 total). The mandatrophy resolution is: this is NOT a mislabeled rope (pure coordination) because workers do not experience coordination benefit — they experience only extraction and control. This is NOT a mislabeled tangled rope (mixed coordination-extraction) because there is no genuine coordination function at the worker level — platforms provide logistics coordination for their own benefit, not for workers. This IS a genuine snare: extraction with suppression, maintained by legal formalism and regulatory capture, with zero escape mechanisms for trapped workers. The perspectival divergence (platforms see rope, state sees tangled rope, workers see snare, law sees piton) is not a failure of classification but a diagnostic signal of how thoroughly the extraction mechanism is obscured. The analytical observer's false-summit perspective (treating worker precarity as natural law) is a critical detection case: the engine's false-summit validator (accessibility_collapse, resistance metrics, emerges_naturally flag) would reject this perspective, revealing that naturalization is part of the extraction mechanism itself — the constraint persists partly because it is perceived as immutable by those with power to change it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contractor_vs_employee_threshold,
    'What structural features should determine contractor vs employee classification under a functional dependence test rather than tax-law formalism?',
    'Cross-jurisdictional comparison of worker dependence: income concentration (percentage from single platform), algorithmic control (management of task assignment, pace, quality), availability of substitutes (portability of skills and customer base to competing platforms), investment requirements (worker''s capital contribution vs platform''s infrastructure)',
    'If functional test applied: most gig workers classify as employees under existing labor law, triggering benefits mandate and reducing effective extraction. If tax-law formalism preserved: contractor classification persists, suppression remains high, snare classification stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_vs_employee_threshold, empirical, 'Functional dependence test for employment classification').

omega_variable(
    algorithmic_control_mechanism,
    'Is algorithmic task assignment and performance rating equivalent to management control, and does equivalence trigger employment status regardless of platform legal characterization?',
    'Comparative analysis: worker autonomy in gig platforms vs traditional employment; measurement of algorithmic constraints on task selection, pricing, scheduling, and deactivation decisions; correlation between algorithmic constraints and income stability',
    'If algorithmic control = management control: establishes structural basis for employee reclassification. If algorithmic control is distinct category: may require new legal framework (intermediate status between contractor and employee).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_mechanism, empirical, 'Whether algorithmic control constitutes employer management').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Can platforms indefinitely maintain contractor classification through jurisdictional arbitrage, or will harmonization of labor standards create exit costs that force reclassification?',
    'Tracking of regulatory changes across jurisdictions (EU driver reclassification, California AB5 litigation, UK Supreme Court Uber decision); measurement of platform operational costs when contractor status is not available; industry migration to higher-barrier markets or business model shifts (consolidation, automation, subsidy dependence)',
    'If arbitrage sustainable: platforms maintain extraction mechanism indefinitely, snare classification persists. If harmonization progresses: platforms face choice between reclassification costs or market exit, shifting classification toward tangled_rope or scaffold (temporary state during transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, empirical, 'Sustainability of jurisdictional arbitrage for contractor classification').

omega_variable(
    worker_coalition_threshold,
    'What organizational threshold (membership, coordination capacity, threat capacity) would upgrade the precarious worker coalition from constrained to arbitrage exit status?',
    'Analysis of successful labor organizing in gig economy; measurement of coalition size, strike capacity, and platform vulnerability to collective action; correlation between coalition strength and platform concessions or reclassification',
    'If threshold reached: worker perspective upgrades from trapped/constrained to organized/mobile, shifting experienced chi downward. If threshold persistently blocked: suppression mechanisms (algorithmic deactivation, work dispersal) prevent coalition formation, snare classification deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_coalition_threshold, empirical, 'Coalition threshold for worker collective power').

omega_variable(
    universal_basic_income_counterfactual,
    'Would universal basic income (reducing income necessity from gig work) change the structural classification, or is the extraction mechanism independent of income floor?',
    'Pilot data from UBI programs in gig-heavy regions; measurement of gig labor supply elasticity under UBI; worker exit rates and renegotiation of terms when gig income is discretionary rather than necessary',
    'If UBI eliminates snare: classification shifts to rope (pure coordination, freely entered). If snare persists: extraction mechanism is not income necessity alone but also platform control, algorithmic opacity, and regulatory capture — structural changes required beyond income support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_basic_income_counterfactual, empirical, 'Whether UBI would change snare classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_worker_protections, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig_wp_tr_t0, gig_economy_worker_protections, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gig_wp_tr_t5, gig_economy_worker_protections, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gig_wp_tr_t10, gig_economy_worker_protections, theater_ratio, 10, 0.58).
narrative_ontology:measurement(gig_wp_tr_t15, gig_economy_worker_protections, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(gig_wp_be_t0, gig_economy_worker_protections, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gig_wp_be_t5, gig_economy_worker_protections, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(gig_wp_be_t10, gig_economy_worker_protections, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gig_wp_be_t15, gig_economy_worker_protections, base_extractiveness, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_worker_protections, resource_allocation).
narrative_ontology:affects_constraint(gig_economy_worker_protections, platform_algorithmic_opacity).
narrative_ontology:affects_constraint(gig_economy_worker_protections, labor_standard_regulatory_capture).
narrative_ontology:affects_constraint(gig_economy_worker_protections, consumer_price_subsidy_mechanism).

% DUAL FORMULATION NOTE:
% Gig economy worker protections decompose into three structurally distinct constraints: (1) platform_algorithmic_opacity (ε=0.52, Snare) — workers cannot understand task assignment, rating, or deactivation mechanisms; (2) labor_standard_regulatory_capture (ε=0.45, Tangled Rope) — regulatory state faces genuine coordination problem alongside extraction asymmetry where platforms avoid social costs; (3) consumer_price_subsidy_mechanism (ε=0.38, Rope) — consumer benefits from suppressed costs without perceiving extraction. Each story has distinct ε and beneficiary/victim patterns but are causally linked (algorithmic opacity enables regulatory capture enables consumer subsidy). This story integrates them as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gig_economy_worker_protections, analytical, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
