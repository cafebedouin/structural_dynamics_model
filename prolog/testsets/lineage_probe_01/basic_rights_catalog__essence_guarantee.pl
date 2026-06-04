% ============================================================================
% CONSTRAINT STORY: basic_rights_catalog__essence_guarantee
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_rights_catalog__essence_guarantee, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_rights_catalog__essence_guarantee
 *   human_readable: Article 19(2) Essence Guarantee: The Inviolable Core of Rights
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   Article 19(2) of the German Basic Law encodes a constitutional doctrine
 *   that no fundamental right, however limited by law in service of
 *   legitimate state aims, may be reduced to its essence-less shell. This
 *   story instantiates the essence_guarantee reading of the contested
 *   basic_rights_catalog kernel. The constraint models the structural claim
 *   that beneath all permissible balancing lies an inviolable minimum: the
 *   right to freedom of expression cannot be hollowed to silence; the right
 *   to property cannot be reduced to symbolic possession; the right to
 *   assembly cannot be drained to solitary speech. This reading competes with
 *   four others: the informational_self_determination reading (which mints
 *   new rights from old text), the objective_values_order reading (which
 *   radiates rights into all legal relations), and the
 *   proportionality_doctrine reading (which models rights as
 *   always-balanceable through four-step analysis). The essence_guarantee
 *   reading claims that proportionality, however rigorous, has a
 *   constitutional floor it cannot cross. This floor is modeled as mountain —
 *   a structural immobility that emerges from the logical necessity of rights
 *   themselves.
 *
 * KEY AGENTS:
 *   - Fundamental rights themselves: Primary beneficiary — the essence guarantee doctrine protects each right's minimum core from reduction to zero
 *   - Human dignity principle: Secondary beneficiary — the grounding axiom that anchors essence guarantees to constitutional anthropology
 *   - Unlimited optimization logic: Primary victim — unrestricted balancing that could, in principle, reduce any right to nothing in pursuit of competing aims. The victim here is not a human actor but a logical procedure.
 *   - Constitutional courts: Institutional trustees — empowered to enforce the essence guarantee by refusing to legitimize balancing outcomes that eliminate the right's core
 *   - Rights-holders: Structural beneficiaries across all perspectives — any individual whose right is protected by the essence floor
 *   - State balancing powers: Constrained agent — the state retains authority to limit rights but cannot annihilate them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_rights_catalog__essence_guarantee, 0.08).
domain_priors:suppression_score(basic_rights_catalog__essence_guarantee, 0.02).
domain_priors:theater_ratio(basic_rights_catalog__essence_guarantee, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, extractiveness, 0.08).
narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_rights_catalog__essence_guarantee, mountain).
narrative_ontology:human_readable(basic_rights_catalog__essence_guarantee, "Article 19(2) Essence Guarantee: The Inviolable Core of Rights").
narrative_ontology:topic_domain(basic_rights_catalog__essence_guarantee, "legal/constitutional_doctrine").

domain_priors:emerges_naturally(basic_rights_catalog__essence_guarantee).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_rights_catalog__essence_guarantee, '8a65c986-5f3e-40b5-b0fa-fc323fc1da3f').
narrative_ontology:cs_kernel_codification('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', fixed_text).
narrative_ontology:cs_authority_grounding('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', lineage).
narrative_ontology:cs_interpretation_layer_present('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f').
narrative_ontology:cs_reading_relation('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', basic_rights_catalog__proportionality_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', basic_rights_catalog__informational_self_determination, influences).
narrative_ontology:cs_reading_relation('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', basic_rights_catalog__objective_values_order, influences).
narrative_ontology:cs_axiom('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', foundational, rights_have_non_negotiable_cores).
narrative_ontology:cs_axiom_status(rights_have_non_negotiable_cores, holdable).
narrative_ontology:cs_axiom_grounding('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', rights_have_non_negotiable_cores, deontological).
narrative_ontology:cs_axiom('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', foundational, human_dignity_cannot_be_reduced_to_zero).
narrative_ontology:cs_axiom_status(human_dignity_cannot_be_reduced_to_zero, holdable).
narrative_ontology:cs_axiom_grounding('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', human_dignity_cannot_be_reduced_to_zero, deontological).
narrative_ontology:cs_reference_frame('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', inviolable_rights_floor).
narrative_ontology:cs_drift_state('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8a65c986-5f3e-40b5-b0fa-fc323fc1da3f', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(basic_rights_catalog__essence_guarantee, basic_rights_catalog).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_rights_catalog__essence_guarantee, rights_minimum_cores).
narrative_ontology:constraint_beneficiary(basic_rights_catalog__essence_guarantee, human_dignity_floor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS RIGHTS-HOLDER (MOUNTAIN) — An individual facing limitation of a fundamental right cannot exit the legal order or negotiate the constitutional floor. From the biographical horizon, the essence guarantee appears as an immutable axiom: no matter how the state justifies a limitation, there exists a non-negotiable minimum the right must retain. This is experienced as a natural limit on what balancing can accomplish — a boundary that does not move.
constraint_indexing:constraint_classification(basic_rights_catalog__essence_guarantee, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CONSTITUTIONAL COURT GENERATIONAL (MOUNTAIN) — From the court's generational horizon, the essence guarantee is a doctrine that has proved structurally immobile across decades of case law. Courts cannot redesign or eliminate the essence floor without abandoning the constitution itself. The constraint appears as a natural law of constitutional reasoning: any attempted abolition of the core right contradicts the court's own authority to adjudicate rights claims. The generational accumulation of doctrine makes this floor non-negotiable.
constraint_indexing:constraint_classification(basic_rights_catalog__essence_guarantee, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / CIVILIZATIONAL (MOUNTAIN) — From the universal/civilizational perspective, the essence guarantee reflects a logical necessity: if a right exists at all, it must retain a minimum core, or it ceases to be that right. Hollowing a right to nothing is not a 'limitation' — it is the elimination of the right. This appears as a law of meaning or logical necessity. However, this perspective risks naturalizing what may be a contingent institutional arrangement (the choice to ground rights in dignity rather than utility, or the choice to recognize certain domains as rights at all). The false summit detector will flag this.
constraint_indexing:constraint_classification(basic_rights_catalog__essence_guarantee, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: TRANSNATIONAL RIGHTS COALITION (MOUNTAIN) — Civil society organizations defending the essence guarantee across multiple jurisdictions experience it as a structural constant: no matter which national system or treaty body adjudicates, the core rights floor cannot be breached without loss of legitimacy across the entire transnational regime. The essence guarantee functions as the stabilizing axiom that prevents competitive downward harmonization. From the movement's generational view, this appears immutable.
constraint_indexing:constraint_classification(basic_rights_catalog__essence_guarantee, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_rights_catalog__essence_guarantee_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(basic_rights_catalog__essence_guarantee, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_rights_catalog__essence_guarantee, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, ExtMetricName, E),
    domain_priors:suppression_score(basic_rights_catalog__essence_guarantee, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(basic_rights_catalog__essence_guarantee),
    narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(basic_rights_catalog__essence_guarantee, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(basic_rights_catalog__essence_guarantee_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The essence guarantee imposes a constraint on state power, not an extraction mechanism. The very low value reflects that this constraint protects rather than extracts — it reduces the state's authority to hollow rights, which is the opposite of extraction. Suppression (0.02): Minimal. The constraint does not suppress alternatives; it clarifies the boundary between permissible limitation and impermissible elimination. Theater ratio (0.15): Low. The essence guarantee doctrine has genuine functional content — courts actually use it to reject balancing outcomes (landmark cases: BVerfGE 30, 1 on census data privacy, BVerfGE 33, 303 on dignity in private law, ECJ case law on proportionality ceilings). The low theater reflects that the constraint operates substantively, not performatively. Accessibility collapse (0.88): High. Once the essence guarantee is adopted as a constitutional principle, it is nearly impossible to access a state of affairs in which rights can be completely eliminated. Any state claiming to respect the constitution accepts the doctrine (either explicitly or through court enforcement). Resistance (0.12): Low. The constraint's legitimacy is deeply embedded in constitutional jurisprudence across multiple jurisdictions. Attempts to eliminate the essence guarantee doctrine explicitly face legal and political invalidation.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives produce mountain classification, with one critical exception flagged in the false summit detector. Powerless rights-holders and the constitutional court both experience the essence guarantee as immutable from their respective horizons. The transnational rights coalition sees it as a structural constant across jurisdictions. But the analytical observer risks naturalizing a contingent institutional choice (the decision to ground rights in dignity rather than utility, to recognize certain domains as rights at all). The beneficiaries declared (rights_minimum_cores, human_dignity_floor) trigger FSM evaluation: the engine will test whether this is a genuine natural law or a doctrine that benefits identifiable agents (the human dignity framing benefits those who value dignity; it may extract from those who value unconstrained state optimization for competing collective aims). The gap is resolved through empirical investigation: does the essence guarantee appear in constitutional systems with fundamentally different grounding principles? Does it emerge from logical necessity or doctrinal choice?
 *
 * DIRECTIONALITY LOGIC:
 *   The essence guarantee does not follow the typical d-derivation chain because it is a mountain (structural immobility from all agent perspectives). However, the beneficiary declaration (rights_minimum_cores, human_dignity_floor) indicates the reading is vulnerable to false summit detection. The directional flow is inverted relative to snares or tangled ropes: the constraint protects rather than extracts. If we were to compute d for a rights-holder, it would be approximately 0.0 (pure beneficiary) because the constraint shields them from extraction. If we computed d for optimization logic (the 'victim'), it would be approximately 1.0 (pure target of constraint). But these computations are not performed on mountains — instead, the constraint is declared as naturally emerging, and the FSM detector tests whether the beneficiaries indicate false-summit risk.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy because it classifies uniformly as mountain across all perspectives. The mandatrophy arises instead when comparing this reading to its sibling: the proportionality_doctrine reading may classify the same constraint as tangled_rope (balancing that coordinates state power with rights protection, but with hidden extraction embedded in discretionary core-drawing). The mandatrophy is resolved by decomposing the single natural-language concept (Article 19(2)) into two structurally distinct constraints: one that models the essence guarantee as immutable (this story), one that models proportionality balancing as a contested coordination mechanism with asymmetric extraction (sibling story). Each story has its own ε, its own beneficiary/victim set, and its own classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_vs_doctrine,
    'Is the Article 19(2) essence guarantee a natural law of human dignity (logically necessary) or a specific doctrinal commitment (contingent institutional choice)?',
    'Comparative constitutional analysis: do legal systems WITHOUT an explicit dignity clause (or with different foundational commitments) recognize an essence guarantee? Do systems that have tried to abandon the essence guarantee face internal logical contradictions or merely different value orderings?',
    'If natural law: mountain classification stands. If contingent doctrine: the constraint is better classified as a tangled_rope (coordination function for rights adjudication + beneficiary preference for dignity frames). The false_summit_mountain signature will flag beneficiaries present on a mountain — this is the exact mechanism to resolve the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_doctrine, conceptual, 'Whether essence guarantee is logically necessary or contingent doctrinal choice').

omega_variable(
    sibling_reading_contamination,
    'Which sibling reading''s framing contaminates this essence_guarantee perspective? Does informational_self_determination (new rights minted from old text) undermine the claim that Article 19(2) guards a natural essence? Does proportionality_doctrine (the catalog''s method is balancing) render the essence guarantee rhetorical rather than structural?',
    'Doctrinal text analysis: compare Article 19(2) in the German Constitution''s original 1949 formulation to contemporary jurisprudence post-BVerfGE 33, 303 (Lüth). Identify whether the essence guarantee doctrine was implied from the start or emergent from proportionality case law. Track whether proportionality logic is compatible with an inviolable core or whether proportionality doctrine implicitly forecloses essence guarantees.',
    'If proportionality doctrine implicitly forecloses essence: this reading forecloses the proportionality_doctrine reading (rare foreclosure). If proportionality and essence coexist: they influence each other but do not foreclose (more common). If essence is emergent from proportionality (not foundational): essence_guarantee is actually downstream of proportionality_doctrine (influences relation inverted).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contamination, empirical, 'Whether proportionality doctrine supports or contradicts essence guarantee').

omega_variable(
    minimum_core_specification_ambiguity,
    'What counts as the ''essence'' or ''minimum core'' of a right? Does Article 19(2) specify which dimensions of a right are core (inviolable) versus which are balanceable?',
    'Case-by-case doctrinal mapping: for each fundamental right (expression, assembly, conscience, property, etc.), identify whether courts have identified a stable minimum core or whether the core shifts with the balancing context. If cores are context-dependent, the constraint is less mountain-like (natural law immobility) and more tangled_rope-like (coordination function hidden by theater).',
    'If cores are stable across cases: mountain status confirmed. If cores shift context-dependently: essence guarantee becomes a performative claim masking balancing discretion (high theater, potential piton reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_core_specification_ambiguity, empirical, 'Whether essence cores are stable or context-dependent').

omega_variable(
    reading_instantiation_commitment,
    'Does this story instantiate the essence_guarantee reading, or does it risk conflating essence_guarantee with the proportionality_doctrine reading that may already have been decided against it?',
    'Doctrinal commitment check: this story declares the essence guarantee as mountain (immutable). The proportionality_doctrine reading (sibling) would classify the same constraint as tangled_rope or snare (balancing with hidden extraction). Verify that this story''s metrics (ε=0.08, suppression=0.02, naturally emerging) represent the essence guarantee alone, not a hybrid that secretly borrows proportionality''s logic.',
    'If hybrid: reclassify to tangled_rope and explicitly declare reading_relations as influences (not forecloses or coexists). If pure essence: mountain stands and reading_relations should reflect genuine structural competition between inviolability and balancing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_commitment, conceptual, 'This story''s fidelity to essence_guarantee reading commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_rights_catalog__essence_guarantee, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(essence_theater_1949_foundational, basic_rights_catalog__essence_guarantee, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(essence_theater_1970_post_lufth, basic_rights_catalog__essence_guarantee, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(essence_theater_2000_stabilized, basic_rights_catalog__essence_guarantee, theater_ratio, 2000, 0.16).

% Extraction over time
narrative_ontology:measurement(essence_extract_1949_foundational, basic_rights_catalog__essence_guarantee, base_extractiveness, 1949, 0.06).
narrative_ontology:measurement(essence_extract_1970_post_luth, basic_rights_catalog__essence_guarantee, base_extractiveness, 1970, 0.08).
narrative_ontology:measurement(essence_extract_2000_stabilized, basic_rights_catalog__essence_guarantee, base_extractiveness, 2000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_rights_catalog__essence_guarantee, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_rights_catalog__essence_guarantee, basic_rights_catalog__proportionality_doctrine).
narrative_ontology:affects_constraint(basic_rights_catalog__essence_guarantee, basic_rights_catalog__objective_values_order).
narrative_ontology:affects_constraint(basic_rights_catalog__essence_guarantee, basic_rights_catalog__informational_self_determination).

% DUAL FORMULATION NOTE:
% Article 19(2) is a contested kernel instantiating four distinct constraint stories. The essence_guarantee reading (this file) models the claim that rights have inviolable cores. The proportionality_doctrine reading (sibling) models the balancing method itself as a constraint. These are structurally distinct: one's constraint is the floor beneath balancing; the other's is the balancing process itself. The informational_self_determination reading models the extension of Article 19(2) logic to new domains (data rights). The objective_values_order reading models the radiating authority of rights into private law. All four stories link via network.affects_constraints and share kernel_id=basic_rights_catalog with different reading_ids. Each story has a single ε value that does not change across observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
