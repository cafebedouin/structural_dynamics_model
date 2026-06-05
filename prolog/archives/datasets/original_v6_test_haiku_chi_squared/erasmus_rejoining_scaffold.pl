% ============================================================================
% CONSTRAINT STORY: erasmus_rejoining_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_erasmus_rejoining_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: erasmus_rejoining_scaffold
 *   human_readable: UK's potential re-entry into the EU Erasmus+ student exchange program
 *   domain: political/education/international_relations
 *
 * SUMMARY:
 *   Following its 2020 exit from the EU and the Erasmus+ program, the UK is
 *   considering rejoining the student exchange framework. This constraint
 *   represents a classic Scaffold structure: a temporary coordination
 *   mechanism that solves a genuine problem (student mobility access) with
 *   declining enforcement overhead and an explicit sunset clause. The
 *   coordination benefits (expanded learning opportunities for students,
 *   research collaboration for institutions, network effects for the EU
 *   program) are real and immediate. The suppression (visa bureaucracy,
 *   data-governance friction, compliance costs) is significant but declining
 *   as frameworks normalize. The theater_ratio (0.62) reflects that political
 *   framing around rejoining is partly performative ('pragmatic cooperation'
 *   vs. 'maintaining independence' narratives) while the underlying
 *   coordination mechanism is functional. The constraint exhibits different
 *   classifications from different structural perspectives: students and EU
 *   administration see pure coordination (Rope); UK institutions see
 *   symmetric coordination with declining overhead (Scaffold); UK government
 *   sees mixed coordination and autonomy cost (Tangled Rope); the post-Brexit
 *   institutional consensus performs the rejoining as both cooperation and
 *   sovereignty reclamation (Piton). The analytical observer confirms the
 *   Scaffold classification with explicit sunset logic: either integration
 *   deepens over 5-10 years or political friction causes withdrawal.
 *
 * KEY AGENTS:
 *   - UK students: Primary beneficiary (powerful/mobile) — gain access to EU learning opportunities and institutional networks
 *   - UK higher education institutions: Primary beneficiary (organized/constrained) — benefit from collaboration and funding reciprocity; face compliance overhead
 *   - EU Erasmus+ program administration: Secondary beneficiary (institutional/arbitrage) — benefits from expanded network and student diversity; no extraction mechanism
 *   - UK government: Constrained institutional actor (institutional/constrained) — experiences mixed coordination and autonomy cost; manages domestic political friction
 *   - Post-Brexit institutional consensus: Meta-institutional actor (institutional/arbitrage) — maintains performative political framing around cooperation; sees own commitment as weakly institutionalized
 *   - Isolated students (visa barriers): Potential victim group (powerless/trapped) — if rejoining fails or suppression barriers remain high; currently constrained but improving
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(erasmus_rejoining_scaffold, 0.28).
domain_priors:suppression_score(erasmus_rejoining_scaffold, 0.35).
domain_priors:theater_ratio(erasmus_rejoining_scaffold, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(erasmus_rejoining_scaffold, scaffold).
narrative_ontology:human_readable(erasmus_rejoining_scaffold, "UK's potential re-entry into the EU Erasmus+ student exchange program").
narrative_ontology:topic_domain(erasmus_rejoining_scaffold, "political/education/international_relations").

domain_priors:requires_active_enforcement(erasmus_rejoining_scaffold).
narrative_ontology:has_sunset_clause(erasmus_rejoining_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_students).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_higher_education_institutions).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, eu_student_exchange_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UK STUDENT COHORT (SCAFFOLD) — UK students benefit from access to Erasmus+ exchange pathways, but this access is temporary and contingent on sustained political commitment. The program operates under a sunset clause: continued participation requires annual negotiation and goodwill maintenance with EU partners. Students experience moderate suppression (visa barriers, bureaucratic friction) that is declining as frameworks normalize. d≈0.35, f(d)≈0.32, σ=1.1 → χ≈0.10. Low effective extraction; the coordination function (access to learning abroad) dominates.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 2: UK HE INSTITUTIONS (SCAFFOLD) — Universities benefit from student mobility, research collaboration, and institutional prestige. But participation is contingent on maintaining EU funding reciprocity agreements. Suppression includes compliance overhead (student visa systems, data-sharing governance) and uncertainty about multi-year funding commitments. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.18. Symmetric costs and benefits; institutions experience this as a coordination framework with temporary enforcement overhead.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EU ERASMUS+ ADMINISTRATION (ROPE) — The EU program benefits from UK participation (expanded network, student diversity). Experiences the rejoining as pure coordination: establishing student mobility pipelines, processing exchanges, managing reciprocal funding. No net extraction from the EU's perspective — both sides gain. d≈0.20, f(d)≈0.05, σ=1.1 → χ≈0.01. Effective extraction near zero; this is genuine coordination.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: UK GOVERNMENT (TANGLED ROPE) — The government faces mixed incentives: political benefit from 'EU cooperation' rhetoric, but constrained by sovereign-autonomy commitments and budget pressures. Participation requires accepting EU data standards and governance oversight — a modest extraction of political autonomy in exchange for the coordination benefit of student mobility. Suppression includes domestic political friction from Euroskeptic factions. d≈0.65, f(d)≈0.95, σ=1.1 → χ≈0.29. Moderate-low extraction; the constraint is a hybrid of genuine coordination (shared student mobility goal) and political cost (loss of autonomy, domestic opposition).
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-BREXIT INSTITUTIONAL CONSENSUS (PITON) — The broader political consensus on whether UK-EU cooperation should expand is partly performative. Rejoining Erasmus+ is theatrically positioned as both 'practical cooperation on shared goals' and 'sovereignty reclamation' depending on domestic political audience. The actual governance coordination is functional, but the framing ritual (speeches about 'pragmatic partnership' vs. 'maintaining independence') is theatrical. theater_ratio≈0.62 satisfies the piton gate. This constraint persists through institutional inertia — it fills a real coordination gap, but the political theater around it suggests the underlying functional commitment is weaker than proclaimed. d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.02. The institutional actor (UK government) sees this as a beneficiary position with low extraction, but the theatrical maintenance suggests degradation of actual commitment.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational perspective, UK Erasmus+ rejoining is a temporary coordination mechanism with an explicit sunset logic: either integration deepens over 5-10 years (moving toward permanent Rope status) or political friction causes withdrawal (returning to Snare status for isolated students). The constraint is structurally scaffolded: it solves a genuine coordination problem (student mobility) with explicitly declining suppression (visa barriers normalizing, bureaucratic friction declining) and declining enforcement overhead. This matches the canonical Scaffold signature. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.22. The analysis confirms that temporary coordination with declining theater and suppression is the core structural signature.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erasmus_rejoining_scaffold_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(erasmus_rejoining_scaffold, TR),
    TR >= 0.70.

:- end_tests(erasmus_rejoining_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The constraint operates primarily as coordination (student mobility is the shared benefit), not extraction. Some asymmetry exists (UK government accepts autonomy costs; EU administration accepts reciprocal funding), but neither side is systematically extracting from the other. The value reflects that suppression (visa/data-governance barriers) is real but declining — this is temporary enforcement overhead characteristic of Scaffold, not permanent rent-seeking. Suppression (0.35): Moderate. Student visa processing, data-governance friction, compliance costs, and institutional coordination overhead create genuine friction. But these barriers are intentionally declining: Erasmus+ is designed to reduce these costs over time through reciprocal framework maturation. Theater ratio (0.62): Moderate-high. Political framing around rejoining is partly performative. Both UK and EU actors have incentive to present the rejoining as both 'pragmatic cooperation' (for pragmatists) and 'sovereignty reclamation' (for Euroskeptics). This rhetorical theater doesn't negate the functional coordination, but it signals that underlying political commitment is not fully institutionalized. Has sunset clause (true): Explicit. Rejoining creates a contingent framework subject to annual review, political friction points, and withdrawal conditions. The 5-10 year horizon matches the 'temporary support with declining sunset' signature of Scaffold.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence from the same structural data. UK students and EU administrators see near-pure coordination (Rope-adjacent): both parties gain, suppression is declining, no extraction. UK institutions see symmetric Scaffold: genuine benefits offset by declining compliance costs, sunset logic built in. UK government sees Tangled Rope: coordination benefit offset by autonomy cost and domestic political friction (Euroskeptic suppression of rejoining). The post-Brexit institutional consensus sees Piton: the functional coordination is real, but the political theater around it (speeches about 'pragmatic partnership' varying by audience) suggests the commitment is weaker than proclaimed, hence degraded institutional inertia. The powerless student cohort at the margin (those who cannot afford visa processing, those excluded by data-governance rules) would see Snare: trapped by barriers, no exit. The analytical observer confirms the Scaffold: genuine temporary coordination with declining enforcement overhead and explicit sunset.
 *
 * DIRECTIONALITY LOGIC:
 *   UK students: Beneficiary + mobile → d≈0.35, f(d)≈0.32. Net beneficiary with agency to exit if barriers remain. UK HE institutions: Beneficiary + constrained → d≈0.50, f(d)≈0.65. Symmetric position — both cost and benefit in balance. EU administration: Beneficiary + arbitrage → d≈0.20, f(d)≈0.05. Pure beneficiary with full exit option (they set terms). UK government: Mixed + constrained → d≈0.65, f(d)≈0.95. Extraction via autonomy cost, but also beneficiary via coordination. Post-Brexit consensus: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Beneficiary position but theatrical framing suggests weaker actual commitment. Isolated students (high visa cost): Victim + trapped → d≈0.85, f(d)≈1.15. If rejoining fails to lower barriers, these agents face Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   SUNSET RESOLUTION: The mandatrophy (is this coordination or extraction?) is resolved through the explicit sunset clause and declining suppression signature. This is not a permanent Rope (no stabilized institutional structure yet) nor a permanent Snare (no irreversible extraction mechanism). The Scaffold classification captures the dynamic: genuine coordination that solves the student mobility problem, but with declining support and explicit sunset logic. Either deep integration over 5-10 years transforms this into Rope (permanent institutional embedding) or political friction causes withdrawal (revealing the Snare that was latent in high visa barriers and institutional friction). The theater_ratio indicates that political framing is not yet fully aligned with functional reality — the commitment is more performative than permanent, which is structurally characteristic of Scaffold: temporary support maintained through declining enforcement theater. If theater_ratio remains above 0.70 while extractiveness stays low, the constraint degrades to Piton (institutional inertia without functional commitment). If theater_ratio declines below 0.50 and extractiveness remains low, the constraint transitions to permanent Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_commitment_depth,
    'Is UK Erasmus+ rejoining driven by genuine long-term institutional commitment or performative political cooperation rhetoric?',
    'Multi-year funding commitments, institutional integration depth (data-sharing agreements, reciprocal governance), and tracking whether withdrawal threats emerge during budget cycles',
    'If deep commitment: Scaffold→Rope transition over 5-10 years. If performative: Piton degradation pathway → eventual withdrawal, reclassifying as Snare for isolated students.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_commitment_depth, empirical, 'Whether rejoining reflects institutional commitment or political theater').

omega_variable(
    suppression_barrier_normalization,
    'Will visa bureaucracy, data-governance friction, and coordination overhead decline as expected in the Scaffold model, or will administrative costs plateau?',
    'Tracking visa processing times, student participation costs, institutional compliance burden over 3-5 year interval; comparison to baseline pre-Brexit levels',
    'If declining: Scaffold model confirmed; sunset logic suggests eventual integration. If plateauing: suppression remains high; reclassifies as Tangled Rope or even Snare from student perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_barrier_normalization, empirical, 'Whether administrative suppression declines as Scaffold model predicts').

omega_variable(
    sovereignty_autonomy_cost,
    'What is the actual structural cost to UK political autonomy from accepting EU data standards and governance oversight?',
    'Policy analysis of data-sharing agreements, regulatory alignment, and domestic political friction from sovereignty commitments; tracking whether autonomy costs generate withdrawal pressure',
    'If costs are low: genuine Rope classification for EU administration perspective. If costs are high: Tangled Rope or Snare classification for UK government perspective; political withdrawal becomes likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_autonomy_cost, conceptual, 'Whether UK political autonomy costs justify the coordination benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(erasmus_rejoining_scaffold, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eras_tr_t0, erasmus_rejoining_scaffold, theater_ratio, 0, 0.45).
narrative_ontology:measurement(eras_tr_t3, erasmus_rejoining_scaffold, theater_ratio, 3, 0.58).
narrative_ontology:measurement(eras_tr_t7, erasmus_rejoining_scaffold, theater_ratio, 7, 0.62).

% Extraction over time
narrative_ontology:measurement(eras_be_t0, erasmus_rejoining_scaffold, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eras_be_t3, erasmus_rejoining_scaffold, base_extractiveness, 3, 0.26).
narrative_ontology:measurement(eras_be_t7, erasmus_rejoining_scaffold, base_extractiveness, 7, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(erasmus_rejoining_scaffold, information_standard).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, uk_data_governance_alignment).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, european_higher_education_integration).

% DUAL FORMULATION NOTE:
% UK Erasmus+ rejoining is downstream of broader UK-EU post-Brexit alignment frameworks. The constraint's ε=0.28 reflects that this is primarily a coordination mechanism, not an extraction system. Upstream constraints around data governance and regulatory alignment set the suppression baseline; downstream constraints (research funding reciprocity, visa harmonization) depend on whether this Scaffold matures into permanent Rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
