% ============================================================================
% CONSTRAINT STORY: procedural_justice_deficit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_justice_deficit, []).

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
 *   constraint_id: procedural_justice_deficit
 *   human_readable: Procedural Justice Deficit in Administrative Systems
 *   domain: governance/administrative_law/institutional_fairness
 *
 * SUMMARY:
 *   Procedural justice deficits arise when administrative systems exclude
 *   affected parties from meaningful participation in decisions that allocate
 *   resources, restrict rights, or impose burdens. These deficits exist at
 *   multiple institutional scales — from immigration adjudication to welfare
 *   determination to regulatory permitting — and operate through both
 *   explicit procedural exclusion (no right to hearing, no access to
 *   evidence) and subtle forms (compressed timeframes, high burden of proof,
 *   opaque decision criteria). The constraint creates a structural tension
 *   between administrative efficiency (achieved through procedural shortcuts)
 *   and institutional legitimacy (which requires meaningful access). From
 *   different positions, this manifests as pure extraction (for the excluded
 *   party), coordination with asymmetric benefits (for moderately organized
 *   groups), enabling flexibility (for administrators), and a solvable
 *   temporary problem (for reformers). The constraint's theater ratio (0.68)
 *   reflects the gap between rhetoric of procedural fairness and actual
 *   procedural exclusion: administrative systems profess commitment to due
 *   process while enforcing structural barriers.
 *
 * KEY AGENTS:
 *   - Excluded Claimants: Primary victims (powerless/trapped) — face procedural barriers with no exit capacity; cannot contest decisions affecting them
 *   - Administrative Bureaucracy: Primary beneficiaries (institutional/arbitrage) — benefits from procedural flexibility; can allocate resources and manage caseloads without binding procedural constraints
 *   - Organized Advocacy Coalition: Secondary actor (moderate/constrained) — mobilizes excluded parties but may develop incentive to maintain deficit as source of advocacy need
 *   - Procedural Reform Movement: Organized reformers (organized/constrained) — civil rights advocates and legal scholars with agency and clear reform pathways
 *   - Institutional Accountability Capacity: Distributed victim — the system's ability to detect and correct errors degrades under procedural exclusion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing administrative efficiency as immutable constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_justice_deficit, 0.58).
domain_priors:suppression_score(procedural_justice_deficit, 0.65).
domain_priors:theater_ratio(procedural_justice_deficit, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_justice_deficit, extractiveness, 0.58).
narrative_ontology:constraint_metric(procedural_justice_deficit, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(procedural_justice_deficit, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_justice_deficit, tangled_rope).
narrative_ontology:human_readable(procedural_justice_deficit, "Procedural Justice Deficit in Administrative Systems").
narrative_ontology:topic_domain(procedural_justice_deficit, "governance/administrative_law/institutional_fairness").

domain_priors:requires_active_enforcement(procedural_justice_deficit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_justice_deficit, administrative_bureaucrats).
narrative_ontology:constraint_beneficiary(procedural_justice_deficit, resource_concentrators).
narrative_ontology:constraint_victim(procedural_justice_deficit, procedurally_excluded_claimants).
narrative_ontology:constraint_victim(procedural_justice_deficit, institutional_accountability_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CLAIMANT (SNARE) — Faces absolute barriers to contesting administrative decisions: no right to hearing, no standing to appeal, no access to discovery mechanisms. Procedural exclusion is structural and enforced. Bears full extraction cost with no exit capacity.
constraint_indexing:constraint_classification(procedural_justice_deficit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED ADVOCACY COALITION (TANGLED ROPE) — Constrained by resource requirements and political capture, but also benefits from procedural deficits that create ongoing demand for legal services and advocacy infrastructure. Mixed: genuine coordination function (protecting rights through collective action) coexists with asymmetric extraction (capturing a portion of remedy-seeking for institutional gatekeepers).
constraint_indexing:constraint_classification(procedural_justice_deficit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADMINISTRATIVE BUREAUCRACY (ROPE) — Benefits from procedural flexibility: can allocate resources, prioritize cases, and manage caseload without binding procedural constraints. Experiences constraint as enabling coordination (efficiency in resource-scarce administrative systems). Net beneficiary with maximum exit capacity.
constraint_indexing:constraint_classification(procedural_justice_deficit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROCEDURAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (civil rights groups, administrative law scholars, judicial activism) see procedural deficits as a temporary institutional gap with clear remedies: right to hearing, discovery, written findings, appeal pathways. High agency and an identifiable exit path through legal reform. Theater declining as alternative accountability mechanisms mature.
constraint_indexing:constraint_classification(procedural_justice_deficit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DUE PROCESS RHETORIC (PITON) — Administrative systems invoke due process language and fairness principles while maintaining procedural exclusions. The rhetoric persists through institutional inertia — courts and administrators profess commitment to fairness while enforcing structural barriers to procedural access. Theater ratio high because performance of justice exceeds actual procedural functionality.
constraint_indexing:constraint_classification(procedural_justice_deficit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL NECESSITY VIEW (MOUNTAIN) — From a civilizational analytical position, some procedural shortcuts are inherent to large-scale administration: complete individual hearings for every decision would paralyze systems. This perspective naturalizes resource constraints as immutable features of governance. However, the structural data contradicts this — procedural exclusion is a policy choice, not a law of nature.
constraint_indexing:constraint_classification(procedural_justice_deficit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_justice_deficit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_justice_deficit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_justice_deficit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_justice_deficit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(procedural_justice_deficit, TR),
    TR >= 0.70.

:- end_tests(procedural_justice_deficit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The benefit flow is asymmetric — excluded claimants bear decision burdens; administrators capture flexibility benefits. However, the extraction is not maximal (0.66+) because some procedural access exists (administrative appeal, judicial review in some jurisdictions), and procedural expansion is politically feasible. The trajectory shows accumulation: as case complexity increases, procedural shortcuts become more extractive, while rhetoric of fairness persists. Suppression (0.65): High. Suppression operates through multiple mechanisms: legal standing requirements, resource barriers to accessing procedures, temporal pressure (compressed decision windows), epistemic barriers (opaque decision criteria), and internalization (claimants believe they have no right to contest). Suppression is both structural and internalized — legal barriers combine with cultural acceptance that 'administration knows best.' Theater ratio (0.68): High and rising. Administrative systems maintain extensive procedural rhetoric (administrative procedure acts, appeals processes, written findings requirements) while systematically excluding claimants from meaningful access. The performance of procedural fairness masks actual exclusion, and this performance gap has widened as caseloads increased and expertise became more concentrated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how procedural justice operates asymmetrically across power levels. Excluded claimants see an absolute barrier to contesting decisions (Snare — pure extraction, no escape). Organized advocacy groups see a problem with solutions and a role for themselves (Tangled Rope — mixed coordination and extraction). Administrators see coordination enabler (Rope — the constraint solves the problem of processing high caseloads). Reformers see a solvable problem with sunset (Scaffold — procedural expansion and transparency create exit from the current system). The due process rhetoric layer sees persistent performance of fairness divorced from function (Piton — the talk persists through institutional inertia). The civilizational view risks naturalizing procedural exclusion as inherent to scale (Mountain — false summit). The gap reveals that procedural justice is not a natural constraint but a policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks who bears extraction relative to each agent's structural position. Powerless, trapped agents derive maximum d (≈0.95) as victims of pure exclusion. Institutional beneficiaries with arbitrage options derive low d (≈0.15) from beneficiary status. Organized moderates with constrained exit derive intermediate d (≈0.60) — they benefit from some coordination function but also face extraction through resource barriers. The administrative bureaucracy's arbitrage options produce negative effective extraction (they experience the constraint as enabling). The scaffold perspective's organized agents with constrained exit and a clear reform pathway produce moderate d (≈0.50) reflecting both agency and current barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED STRUCTURE: The mandatrophy resolves through clarity on what constitutes 'procedural justice coordination.' From the beneficiary (administrator) perspective, the constraint genuinely coordinates a resource allocation problem — without procedural shortcuts, systems could not function at scale. From the victim perspective (excluded claimants), there is no coordination function, only extraction. The Tangled Rope classification holds both: genuine coordination (resource management) genuinely coexists with asymmetric extraction (procedural exclusion). The classification is stable when both coordination and extraction are explicitly present in the base properties. The false mountain (institutional necessity view) naturalizes what is actually a policy choice — comparative institutional analysis shows systems with full procedural access at comparable scales, contradicting the 'immutable limit' framing. The Scaffold perspective confirms that alternatives exist: procedural expansion through administrative procedure acts, judicial review expansion, and transparency mechanisms are real institutional pathways with identifiable costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    administrative_capacity_threshold,
    'At what scale does administrative capacity become genuinely insufficient to provide individualized procedural access, versus being a choice to exclude?',
    'Comparative institutional analysis: systems that provide full procedural access at comparable scales; measurement of actual administrative burden from procedural inclusion in expanded systems',
    'If capacity genuinely insufficient: reclassify from Snare to Scaffold (temporary until systems scale). If choice: confirms Tangled Rope / Snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_capacity_threshold, empirical, 'Administrative capacity threshold for procedural inclusion').

omega_variable(
    suppression_internalization_mechanism,
    'How much of the suppression is structural (legal barriers to access) versus internalized (claimants believe they have no standing or don''t deserve a hearing)?',
    'Post-reform measurement: do claimants exercise access when procedures are guaranteed? Tracking belief systems pre- and post-procedural reform implementation',
    'If highly internalized: suppression persists after legal barriers fall. If structural: suppression declines with procedural access reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Proportion of suppression that is structural vs. internalized').

omega_variable(
    advocacy_capture_feedback_loop,
    'Does organized advocacy for procedural access inadvertently capture the issue, creating incentive to maintain deficit in order to sustain advocacy need?',
    'Historical analysis of advocacy group positions during windows of procedural reform opportunity; tracking whether reforms are pursued or delayed',
    'If captured: advocacy coalition becomes partial beneficiary, sustaining extraction. Tangled Rope classification confirmed with stronger extractive component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_capture_feedback_loop, empirical, 'Whether advocacy infrastructure becomes capture-resistant to procedural deficit').

omega_variable(
    theater_vs_function_threshold,
    'What ratio of procedural theater to actual substantive review constitutes a constraint crossing from Rope (functional coordination) to Tangled Rope (performative extraction)?',
    'Audit trail analysis: proportion of administrative decisions with documented reasoning and binding review vs. pro forma decisions with minimal review; measurement of actual revision rates from appeals',
    'If theater high and revisions low: Tangled Rope or Snare confirmed. If theater lower and revisions meaningful: Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_vs_function_threshold, empirical, 'Theater-to-function ratio threshold for classification boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_justice_deficit, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(procjust_tr_t0, procedural_justice_deficit, theater_ratio, 0, 0.55).
narrative_ontology:measurement(procjust_tr_t10, procedural_justice_deficit, theater_ratio, 10, 0.62).
narrative_ontology:measurement(procjust_tr_t20, procedural_justice_deficit, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(procjust_be_t0, procedural_justice_deficit, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(procjust_be_t10, procedural_justice_deficit, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(procjust_be_t20, procedural_justice_deficit, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_justice_deficit, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_justice_deficit, administrative_legitimacy_deficit).
narrative_ontology:affects_constraint(procedural_justice_deficit, rights_enforcement_gap).

% DUAL FORMULATION NOTE:
% Procedural justice deficit decomposes into structural exclusion mechanisms (legal standing, procedural barriers) with ε≈0.58 and rhetorical fairness performance (due process framing) with ε≈0.72. This story models the hybrid — the constraint as administered system with both functional and performative components. The pure extraction from procedural exclusion (Snare) and pure performance of fairness (Piton) are decomposed into separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(procedural_justice_deficit, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
