% ============================================================================
% CONSTRAINT STORY: institutional_mutation_domestication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_mutation_domestication, []).

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
 *   constraint_id: institutional_mutation_domestication
 *   human_readable: The Jedi Bureaucratic Capture
 *   domain: political/social
 *
 * SUMMARY:
 *   The Jedi Order represents a high-agency mutation — a concentrattion of
 *   power, knowledge, and ethical autonomy that exists outside normal
 *   bureaucratic structures. When integrated into the Galactic Republic, this
 *   mutation becomes subject to domestication: gradual institutional capture
 *   that preserves nominal independence while subordinating actual
 *   decision-making to political priorities. The constraint exhibits the core
 *   structure of tangled rope: genuine coordination function (Jedi provide
 *   stability and legitimacy) coupled with asymmetric extraction (political
 *   control over mission selection, deployment, and strategic alignment). The
 *   theater ratio (0.68) reflects the performative aspects of Jedi Council
 *   'approval' of missions — the Council maintains formal independence while
 *   operating under de facto political constraint. The suppression (0.65)
 *   captures the barriers to resistance: withdrawal risks being labeled a
 *   faction; internal dissent risks Jedi conflict; individual refusal risks
 *   expulsion and mission reassignment to a compliant peer.
 *
 * KEY AGENTS:
 *   - Galactic Republic Executive: Primary beneficiary (institutional/arbitrage) — gains force projection, legitimacy, and conflict resolution without direct military command structure
 *   - Jedi Council: Primary organized actor (organized/constrained) — derives legitimacy and resources from Republic affiliation while facing increasing political constraint on operational independence
 *   - Field Jedi: Primary victim (powerless/trapped) — deployed on politically-driven missions, bearing ethical and operational risk while benefiting from Order hierarchy and institutional support
 *   - Jedi Operational Autonomy: Structural victim (abstract) — the constraint degrades the independence that made Jedi valuable as coordination mechanism
 *   - Republic Constitutional Order: Secondary actor (institutional/arbitrage) — nominally protected (Jedi independence) while functionally eroded by capture
 *   - Outer Rim Separatists: Secondary victim (powerful/mobile) — experience Jedi as enforcement mechanism of Republic hegemony, though some coordination benefits exist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_mutation_domestication, 0.52).
domain_priors:suppression_score(institutional_mutation_domestication, 0.65).
domain_priors:theater_ratio(institutional_mutation_domestication, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_mutation_domestication, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_mutation_domestication, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_mutation_domestication, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_mutation_domestication, tangled_rope).
narrative_ontology:human_readable(institutional_mutation_domestication, "The Jedi Bureaucratic Capture").
narrative_ontology:topic_domain(institutional_mutation_domestication, "political/social").

domain_priors:requires_active_enforcement(institutional_mutation_domestication).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_mutation_domestication, galactic_republic_executive).
narrative_ontology:constraint_beneficiary(institutional_mutation_domestication, jedi_order_hierarchy).
narrative_ontology:constraint_victim(institutional_mutation_domestication, jedi_operational_autonomy).
narrative_ontology:constraint_victim(institutional_mutation_domestication, republic_checks_and_balances).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD JEDI (SNARE) — Individual Jedi deployed across the galaxy operate under mandate from the Republic, bearing operational risk and ethical burden while institutional benefits accrue to Order leadership and executive hierarchy. Cannot exit deployment without violating Jedi Code or career. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(institutional_mutation_domestication, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JEDI COUNCIL (TANGLED ROPE) — Organized leadership derives coordination benefit (legitimacy, funding, access to Republic resources) while maintaining nominal independence. However, constrained exit and increasing enforcement obligations create asymmetric extraction: the Republic shapes mission parameters and political alignment. d≈0.58, f(d)≈0.72, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(institutional_mutation_domestication, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REPUBLIC EXECUTIVE (ROPE) — Solves coordination problem: Jedi provide force projection, conflict resolution, and legitimacy for Republic governance without direct institutional control costs. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; experiences constraint as pure coordination.
constraint_indexing:constraint_classification(institutional_mutation_domestication, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL ORDER (PITON) — Separation of civilian and military power — embodied in Jedi independence from direct political command — is a core structural principle. However, the constraint operates as degraded inertia: Jedi are functionally subordinate to political priorities while nominally independent. theater_ratio=0.68 reflects performative constitutional integrity (Jedi Council approves missions) masking political subordination. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(institutional_mutation_domestication, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OUTER RIM SEPARATISTS (TANGLED ROPE) — Experience Jedi deployment as extraction mechanism enforcing Republic hegemony. However, Separatists also benefit from some coordination function (Jedi mediation reduces random violence). Mobile exit (secession) remains theoretically available but expensive. d≈0.70, f(d)≈1.05, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(institutional_mutation_domestication, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW ATTEMPT) — Risk of false summit: interpreting bureaucratic domestication as a law of institutional physics ('all power eventually corrupts,' 'hierarchies inevitably constrain mutations'). Structural data (ε=0.52, suppression=0.65) contradicts mountain claim. The engine flags this as false natural law — the constraint is contingent institutional design, not inherent to governance.
constraint_indexing:constraint_classification(institutional_mutation_domestication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_domestication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_mutation_domestication, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_mutation_domestication, TR),
    TR >= 0.70.

:- end_tests(institutional_mutation_domestication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Republic captures significant political value (force projection, legitimacy, mediation capacity) without granting Jedi veto power. However, extraction is not maximal because Jedi retain real agenda-setting capacity within boundaries and genuine coordination benefits flow to all parties. Suppression (0.65): Moderate-high. Barriers include: (1) institutional inertia — the Jedi Code binds individual choice; (2) career risk — expulsion is professional death; (3) ideological capture — Jedi believe in service; (4) resource dependence — Jedi need Republic logistics. Theater ratio (0.68): High. The Jedi Council's formal 'approval' process creates performative deliberation. Actual mission parameters are set by political actors, with Council assent presented as independent judgment. The theater has increased over the interval as political pressure has grown while formal independence has been maintained for legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The Republic Executive sees Rope (pure coordination solution to governance). The Jedi Council sees Tangled Rope (coordination with constraints). Field Jedi see Snare (deployed with no exit). The constitutional order sees Piton (its own degradation). Separatists see Tangled Rope (enforcement with some coordination). The analytical observer risks seeing Mountain (bureaucratic capture as law of power) but this is a false summit — the constraint is contingent institutional design. The perspectival gap reveals that the same structural phenomenon — Jedi integration into Republic governance — produces radically different classifications based on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Republic Executive: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; low effective extraction. Jedi Council: Beneficiary + constrained (ambivalent: resources + autonomy loss) + organized → d≈0.58, f(d)≈0.72. Mixed; constrained exit dominates despite beneficiary status. Field Jedi: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Constitutional Order: Beneficiary status nominal + arbitrage, but constrained by political override → d≈0.50, f(d)≈0.65. Piton gate triggered by theater_ratio. Separatists: Victim + mobile → d≈0.70, f(d)≈1.05. Significant extraction but with exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly declaring both coordination and extraction. Beneficiaries (Republic executive, Jedi order hierarchy) are explicitly distinguished from victims (field Jedi, Jedi operational autonomy, separatists). The tangled rope classification preserves both the genuine coordination function (Jedi legitimacy is valuable to Republic) and the asymmetric extraction (political control). The piton perspective (constitutional order degradation) and snare perspective (field Jedi) show that while the order-level classification is tangled rope, individual Jedi and structural principles experience capture as snare. The false summit (mountain perspective) is deliberately included to demonstrate how analytical observers risk naturalizing contingent institutional arrangements. Mandatrophy is resolved by showing that the constraint is neither pure coordination nor pure extraction but a hybrid whose classification depends on the structural position of the evaluator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jedi_autonomy_threshold,
    'At what level of political integration does Jedi operational autonomy cease to be meaningful?',
    'Historical analysis of Jedi Council decision independence; correlation between Council votes and executive preferences; tracking of mission refusal rates',
    'If threshold already crossed: Jedi are fully captured, classify as Snare from Council perspective. If threshold not yet crossed: tangled_rope analysis holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jedi_autonomy_threshold, empirical, 'Threshold where Jedi autonomy becomes nominal').

omega_variable(
    republic_structural_dependence,
    'Does the Galactic Republic''s political legitimacy actually depend on Jedi independence, or has capture already delegitimized both institutions?',
    'Public perception analysis; correlation between Jedi scandal and Republic institutional trust; examination of separatist recruitment rhetoric',
    'If republic depends on Jedi independence: capture is actively destabilizing (structural decay). If republic has already abandoned legitimacy-via-independence: constraint is pure extraction (Snare from all perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(republic_structural_dependence, empirical, 'Whether Republic legitimacy depends on Jedi independence').

omega_variable(
    mutation_domestication_reversibility,
    'Is institutional domestication of high-agency mutations reversible, or does integration necessarily entail absorption?',
    'Comparative institutional analysis; case studies of power factions integrated into state hierarchies; examination of exit mechanisms available to captured actors',
    'If reversible: scaffold sunset perspective is plausible. If irreversible: mutation is permanently degraded to piton/snare status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mutation_domestication_reversibility, conceptual, 'Reversibility of bureaucratic capture of high-agency actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mutation_domestication, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_mutation_domestication, theater_ratio, 0, 0.35).
narrative_ontology:measurement(inst_tr_t500, institutional_mutation_domestication, theater_ratio, 500, 0.55).
narrative_ontology:measurement(inst_tr_t1000, institutional_mutation_domestication, theater_ratio, 1000, 0.68).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_mutation_domestication, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(inst_be_t500, institutional_mutation_domestication, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(inst_be_t1000, institutional_mutation_domestication, base_extractiveness, 1000, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_mutation_domestication, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_mutation_domestication, republic_democratic_accountability).
narrative_ontology:affects_constraint(institutional_mutation_domestication, separatist_military_asymmetry).
narrative_ontology:affects_constraint(institutional_mutation_domestication, jedi_code_institutional_conflict).

% DUAL FORMULATION NOTE:
% Institutional mutation domestication is a constraint family member alongside constraints on democratic accountability (how Republic executive avoids checks) and military asymmetry (how Jedi deployment shapes sectional conflicts). The upstream constraint on democratic accountability creates structural pressure for bureaucratic solutions; this constraint is a downstream response. All three are linked by institutional dynamics: the Republic escapes democratic constraint via Jedi autonomy myth, which then creates pressure on Jedi autonomy itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_mutation_domestication, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
