% ============================================================================
% CONSTRAINT STORY: institutional_mutation_without_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_mutation_without_selection, []).

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
 *   constraint_id: institutional_mutation_without_selection
 *   human_readable: The Zombie Bureaucracy Drift
 *   domain: organizational/political
 *
 * SUMMARY:
 *   A zombie bureaucracy is an institution whose internal rules and
 *   operational procedures have mutated substantially from their original
 *   design, but which is shielded from external accountability mechanisms
 *   that would normally trigger reform or dissolution. The institution
 *   persists in a degraded state because: (1) stakeholders have incomplete
 *   exit options (public services cannot be abandoned, regulated entities
 *   cannot easily relocate), (2) formal accountability mechanisms (audits,
 *   oversight, elections) operate on longer cycles than mutation accumulates,
 *   and (3) the institution itself maintains sufficient symbolic legitimacy
 *   (compliance theater, mission statements, leadership turnover) to deflect
 *   reform pressure. The constraint is structural: as long as exit is
 *   constrained and external selection is weak relative to internal mutation
 *   rate, the institution will accumulate procedural cruft that serves
 *   internal constituency interests rather than public mission. This creates
 *   a Snare dynamic: subordinate staff and public stakeholders bear the
 *   extraction cost (compliance burden, degraded services, wasted time),
 *   while accountability mechanisms prove insufficient to arrest the drift.
 *   The theater ratio (0.81) reflects that compliance apparatus (audits,
 *   reporting, reviews, oversight briefings) becomes increasingly
 *   performative relative to actual institutional function — the institution
 *   can document its procedures in excruciating detail while delivering
 *   outputs that fail the original mission.
 *
 * KEY AGENTS:
 *   - Subordinate Staff: Primary victims (powerless/trapped) — must comply with mutating rules, bear blame for performance failures, cannot exit without career termination
 *   - Public Stakeholders: Primary victims (powerless/trapped) — depend on institutional outputs; cannot exit or reform institution; bear service degradation
 *   - Mid-Level Administrators: Secondary beneficiaries (organized/constrained) — gain expanded domain control and reduced external scrutiny; constrained by hierarchy and civil service rules; bears reputational risk
 *   - Political Leadership: Institutional beneficiary (institutional/arbitrage) — insulated from institutional mutation by civil service protections; can defer reform indefinitely
 *   - Formal Accountability Theater: Institutional actor (institutional/arbitrage) — maintains legitimacy through compliance documentation while being functionally disconnected from actual institutional control
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees the constraint as a structural feature of institutions where exit is constrained and external selection is weak relative to internal mutation rate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_mutation_without_selection, 0.58).
domain_priors:suppression_score(institutional_mutation_without_selection, 0.68).
domain_priors:theater_ratio(institutional_mutation_without_selection, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_mutation_without_selection, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_mutation_without_selection, snare).
narrative_ontology:human_readable(institutional_mutation_without_selection, "The Zombie Bureaucracy Drift").
narrative_ontology:topic_domain(institutional_mutation_without_selection, "organizational/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(institutional_mutation_without_selection, public_stakeholders).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, subordinate_staff).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, policy_effectiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE STAFF (SNARE) — Trapped within the institution's escalating internal bureaucracy. Must comply with mutating rules that no longer serve stated organizational purpose. No exit without career termination; no leverage to reform. Maximum extraction: comply with contradictory mandates, document compliance, absorb blame when outcomes fail.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC STAKEHOLDERS (SNARE) — Citizens, communities, or constituents depend on institutional outputs (education, welfare, infrastructure, justice). As internal mutation accelerates, outputs degrade while institution remains unaccountable. Exit is partial or impossible (cannot switch police department, state licensing board, or primary school without relocation). Bear full cost; cannot reform mechanism.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-LEVEL ADMINISTRATORS (TANGLED ROPE) — Can influence internal mutation through policy memo circulation, budget reallocation, hiring decisions. Benefit from expanded domain control and reduced external scrutiny. Constrained by civil service rules and organizational hierarchy. Mixed extraction: gain agency in a degraded institution; lose alignment with institutional mission; bear reputational risk when outputs fail.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: POLITICAL LEADERSHIP (ROPE) — Insulated from direct institutional mutation by civil service protections. Can shift blame to bureaucracy; claim credit for stated intentions while ignoring actual outputs. High arbitrage: can exit political role without consequence; can defer structural reform indefinitely. Sees constraint as coordination mechanism: managing information about institutional performance.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL ACCOUNTABILITY THEATER (PITON) — Audit trails, compliance reporting, performance metrics, and oversight hearings persist despite disconnection from actual institutional function. These rituals maintain legitimacy while shielding mutation from correction. Piton classification: high theater_ratio (0.81), low effective function. The apparatus is maintained through institutional inertia; its degradation is obvious but replacement is politically costly.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational distance, zombie bureaucracy represents a coordination mechanism (internal mutation requires collective maintenance) that extracts from public trust. The constraint is neither a natural law nor pure extraction — it is an institution-specific pathology enabled by missing accountability selection. The analytical view sees this as a Tangled Rope because genuine coordination occurs (staff coordination around new internal rules) alongside asymmetric extraction from those without exit.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_without_selection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_mutation_without_selection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_mutation_without_selection, TR),
    TR >= 0.70.

:- end_tests(institutional_mutation_without_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The institution extracts from subordinate staff (compliance burden, career risk, blame for failures) and from public stakeholders (degraded service quality, wasted time, opportunity cost). The extraction is not maximal (0.90+) because some institutional function persists and some stakeholders can negotiate partial exits. However, the extraction is substantial and increasing over time because mutation accumulates faster than accountability mechanisms can arrest it. Suppression (0.68): High. Multiple mechanisms: civil service rules prevent firing mid-level administrators despite institutional degradation; exit costs for stakeholders are high (cannot switch to alternative school, police department, or regulator); career consequences silence internal critics; political insulation allows leadership to defer reform indefinitely. Theater ratio (0.81): Very high. Compliance apparatus (audit trails, performance metrics, oversight hearings, annual reviews) persists and grows even as institutional function degrades. The theater masks the mutation — formal procedures create legitimacy while actual operations drift. The increase over the measurement interval (0.55 → 0.81) reflects that institutions under low selection pressure systematically increase their documentary/ceremonial burden to justify continued existence. Beneficiaries: None declared because the constraint is purely extractive — no group benefits while others pay. The mid-level administrators gain expanded domain control, but this is a secondary consequence, not a coordination function. Political leadership gains insulation, but they are the mechanism that prevents accountability, not a beneficiary of the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional structure appears as pure extraction (Snare) to trapped agents, as coordination-with-constraints (Tangled Rope) to intermediate agents with partial agency, as a coordination mechanism (Rope) to beneficiaries with exit, and as degraded ritual (Piton) to observers of the formal apparatus. The gap is not about disagreement on facts — all perspectives agree that mutation is accumulating and accountability is weak — but about experienced directionality. The trapped see their costs mounting. The intermediate see expanded control offset by constraint. The beneficiaries see a problem solved (how to defer difficult choices while maintaining legitimacy). The formal apparatus sees itself as degraded. The resolution of this perspectival gap depends on which accountability mechanisms can be activated: if external selection can be restored (elections, market pressure, performance-based funding), the constraint collapses and Snare classification was correct. If selection cannot overcome path dependency, the constraint is true and the Snare classification holds.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the mutation-accountability feedback loop. Subordinate staff are trapped victims (d ≈ 0.95): high power of constraint over them, no exit, full bearing of extraction. Public stakeholders are similarly trapped victims (d ≈ 0.90): constrained exits (cannot simply relocate), dependent on institutional output, cannot reform mechanism. Mid-level administrators occupy intermediate position (d ≈ 0.55): they benefit from expanded control (low d) but are also constrained by hierarchy and reputational risk (high d); the net effect is near-symmetric. Political leadership are beneficiaries with high exit (d ≈ 0.10): can exit political role without consequence; insulated from institutional mutation; experience constraint as solution to their coordination problem. The analytical observer (d ≈ 0.73) sees the full structure without stake in any position; derives moderate-high effective extraction from analyzing the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids the false naturalization trap by refusing to classify the mutation as an inherent property of bureaucracy itself. The Snare classification is conditional: it requires that exit be constrained AND that external selection be weak relative to internal mutation rate. If either condition changes, the classification changes. The constraint is not 'bureaucracies are extractive' (false naturalization) but 'institutions without accountability relative to mutation rate become extractive' (conditional and resolvable). The analytical observer's Tangled Rope classification captures the true structure: the institution does perform coordination functions (internal rules coordination, blame management, symbolic legitimacy maintenance), but alongside extraction from those without exit. The Piton classification for the accountability theater is diagnostically valuable: it flags that compliance apparatus has become performative — the theater maintains institutional legitimacy while being functionally disconnected from control. This creates a key diagnostic insight: institutions under low external selection pressure do NOT become less rule-bound; they become MORE rule-bound while their rules become increasingly disconnected from function. The theater ratio increase (0.55 → 0.81) is not a sign of improving accountability; it is a sign of deteriorating institutional health. The constraint resolves mandatrophy by showing that all six types can be legitimate perspectival readings without claiming that any single type is 'the' answer — the presheaf over institutional contexts is the answer, and the constraint's structural reality is the mapping between accountability strength and institutional purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mutation_detection_threshold,
    'At what rate of internal rule mutation does organizational output begin to systematically degrade relative to the institution''s stated mission?',
    'Longitudinal comparison of organizational output metrics (graduation rates for schools, case resolution for courts, infrastructure maintenance for public works) against rate of internal procedural changes; cross-sectional comparison of high-mutation vs low-mutation institutions in same domain',
    'If threshold is low (mutation rate > 5% per year causes measurable output decline): mutation is the primary causal driver, not a symptom. If threshold is high (mutation rate > 15% per year required): other factors (resource constraints, external policy) dominate, and mutation is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutation_detection_threshold, empirical, 'Rate of internal mutation required to cause measurable output degradation').

omega_variable(
    accountability_mechanism_sufficiency,
    'Can existing accountability mechanisms (elections, audits, legislative oversight, voter initiative) actually trigger institutional reform when mutation-driven output degradation is chronic but slow enough to normalize?',
    'Case study analysis of institutional reforms triggered by performance metrics vs those triggered by political crisis; identification of latency periods (how long degradation persists before formal remediation); comparison of institutions with different accountability architectures (elected vs appointed leadership, sunset clauses vs open-ended mandates)',
    'If accountability sufficient: Snare classification is incorrect — mechanism exists to check mutation. If insufficient: Snare is correct; institutional mutation is inherently shielded from correction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_mechanism_sufficiency, empirical, 'Whether formal accountability mechanisms can trigger reform for chronically degraded institutions').

omega_variable(
    mutation_origin_internalization,
    'To what extent do staff internalize mutated rules as legitimate organizational purpose, vs. recognizing them as exogenous drift?',
    'Survey and interview analysis comparing staff perception of institutional mission vs actual internal rules; longitudinal tracking of mission statements vs procedural manuals; analysis of staff advocacy for specific rule changes (do they advocate for alignment with original mission, or for continuation of current practice?)',
    'If staff internalize mutation: compliance is volitional, and the constraint operates with lower suppression. If staff recognize drift: suppression must be actively maintained through sanctions; mutation is visibly contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutation_origin_internalization, empirical, 'Degree to which staff internalize mutated rules as legitimate vs recognize them as drift').

omega_variable(
    external_selection_simulation,
    'If external selection pressure were restored (market competition, electoral threat, performance-based funding), would institutional mutation reverse or would path dependency lock in the degraded configuration?',
    'Natural experiments: comparison of institutional reform trajectories following exogenous accountability changes (new legislation, budget crisis, leadership transition with explicit mandate); agent-based modeling of institutional mutation under different selection regimes',
    'If selection easily reverses mutation: the constraint is contingent and conditional on missing accountability. If selection cannot reverse mutation: path dependency is the true constraint, not the absence of selection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_selection_simulation, empirical, 'Whether restoring external selection pressure would reverse institutional mutation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mutation_without_selection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zombbur_tr_t0, institutional_mutation_without_selection, theater_ratio, 0, 0.55).
narrative_ontology:measurement(zombbur_tr_t5, institutional_mutation_without_selection, theater_ratio, 5, 0.68).
narrative_ontology:measurement(zombbur_tr_t10, institutional_mutation_without_selection, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(zombbur_be_t0, institutional_mutation_without_selection, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(zombbur_be_t5, institutional_mutation_without_selection, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(zombbur_be_t10, institutional_mutation_without_selection, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_mutation_without_selection, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, civil_service_path_dependency).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, political_accountability_lag).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, bureaucratic_compliance_creep).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_mutation_without_selection, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
