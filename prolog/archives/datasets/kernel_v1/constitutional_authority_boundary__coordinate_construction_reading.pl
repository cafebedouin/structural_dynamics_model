% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Constitutional Authority Boundary: Coordinate Construction Reading
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   The coordinate construction reading of constitutional authority
 *   establishes that the three branches — legislative, executive, and
 *   judicial — possess co-equal interpretive authority within their
 *   respective spheres, with no single final arbiter of constitutional
 *   meaning. This reading claims that the Constitution's separation of powers
 *   is not a hierarchy but a distribution: each branch interprets the
 *   Constitution as it carries out its assigned functions, and no branch
 *   possesses monopoly authority to impose its reading on the others. The
 *   legislative branch interprets the Constitution through legislation and
 *   appropriations; the executive interprets it through implementation and
 *   oath-taking; the judicial branch interprets it through case law. This
 *   reading coexists with two siblings: the judicial supremacy reading (which
 *   assigns final interpretive authority to courts) and the parliamentary
 *   primacy reading (which assigns final authority to the legislature). The
 *   coordinate construction reading has been historically prominent in
 *   American constitutional theory and practice, though empirical evidence
 *   suggests that judicial supremacy has increasingly dominated actual
 *   institutional outcomes over the past 150 years. The constraint exhibits
 *   classic tangled rope structure: genuine coordination function
 *   (distributed authority prevents any single branch from monopolizing
 *   constitutional meaning) coexists with extraction mechanisms (each branch
 *   uses its interpretive power to expand its own institutional reach). The
 *   theater ratio has increased over time as the gap has widened between what
 *   constitutional law textbooks teach (coordinate construction) and what the
 *   Supreme Court does (impose its interpretation as binding on the other
 *   branches).
 *
 * KEY AGENTS:
 *   - Legislative Branch: Organized institutional actor (constrained exit) — benefits from distributed authority that enables legislative voice in constitutional interpretation; constrained by judicial supremacy and executive veto
 *   - Executive Branch: Organized institutional actor (constrained exit) — benefits from distributed authority that prevents judicial usurpation of executive power; constrained by legislative override and judicial review
 *   - Judicial Branch: Organized institutional actor (constrained exit) — benefits from distributed authority that establishes judicial authority; constrained by legislative override (amendment) and executive non-compliance
 *   - Citizens: Powerless trapped agents — subject to contradictory mandates from three authorities; no exit option; maximum experienced extraction
 *   - Constitutional Framework (meta-institutional): Beneficiary of coordination function — distributed authority is the mechanism that prevents monopoly control
 *   - Constitutional Stability (victim group): Suffers from inter-branch deadlock and contradictory authority claims; victim of suppression costs
 *   - Analytical Observer: Civilizational view — risks naturalizing the contingent institutional arrangements as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.48).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Constitutional Authority Boundary: Coordinate Construction Reading").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '7c2103c2-27d1-49f0-96b5-0aa53892f2ee').
narrative_ontology:cs_kernel_codification('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', formalized).
narrative_ontology:cs_authority_grounding('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', lineage).
narrative_ontology:cs_interpretation_layer_present('7c2103c2-27d1-49f0-96b5-0aa53892f2ee').
narrative_ontology:cs_reading_relation('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', foundational, co_equal_authority).
narrative_ontology:cs_axiom_status(co_equal_authority, holdable).
narrative_ontology:cs_axiom_grounding('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', co_equal_authority, conventional).
narrative_ontology:cs_axiom('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', foundational, structural_check_against_monopoly).
narrative_ontology:cs_axiom_status(structural_check_against_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', structural_check_against_monopoly, instrumental).
narrative_ontology:cs_reference_frame('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', three_coequal_branches_framework).
narrative_ontology:cs_drift_state('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', contemporary_judicial_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c2103c2-27d1-49f0-96b5-0aa53892f2ee', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, branch_independence_maintenance).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, distributed_oversight_capacity).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, constitutional_stability).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, unified_policy_implementation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN UNDER COORDINATE AUTHORITY (SNARE) — The distributed authority system creates strategic incoherence from the citizen's position. Cannot exit or arbitrage between branches; faces contradictory mandates and institutional deadlock. No single authority can issue binding clarification. Maximum experienced extraction as competing authorities impose conflicting obligations.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE BRANCH (TANGLED ROPE) — Genuine coordination function: distributed authority prevents executive/judicial monopoly and enables legislative voice in constitutional interpretation. Extraction component: legislative override powers and funding control create asymmetric leverage. Constrained exits reflect both institutional legitimacy and political consequence of abdication.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (TANGLED ROPE) — Genuine coordination function: distributed authority prevents judicial usurpation of executive power; enables executive constitutional voice through implementation and oath-taking. Extraction component: executive non-acquiescence and Commander-in-Chief powers create unilateral leverage. Constrained by constitutional legitimacy and legislative/judicial response.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL BRANCH (TANGLED ROPE) — Genuine coordination function: distributed authority prevents legislative/executive monopoly; enables judicial constitutional voice through case law. Extraction component: case-or-controversy doctrine and appellate review create leverage to impose judicial readings. Constrained by legislative override (constitutional amendment) and executive non-compliance.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL FRAMEWORK (ROPE) — Pure coordination function from this institutional perspective. Distributed authority is the mechanism that enables all three branches to participate in constitutional meaning-making without any one monopolizing interpretation. No extraction from this perspective — the coordinate construction is the coordination solution itself.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DEMOCRATIC RENEWAL MOVEMENT (SCAFFOLD) — Sees coordinate construction as a temporary coordination mechanism that can be reformed through democratic amendment. The 'sunset' is the constitutional amendment process itself — if distributed authority becomes dysfunctional, the people via amendment can establish a different allocation. Low effective extraction because the democratic process provides exit.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ACADEMIC CONSTITUTIONAL DISCOURSE (PITON) — The coordinate construction reading persists in scholarly work and judicial opinions largely through institutional inertia and theoretical elegance, despite increasing evidence that actual practice diverges toward judicial supremacy (US context). The discourse maintains the reading as 'what the Constitution says' while acknowledging 'what the Court does.' Theater-high because the normative work (arguments that distributed authority exists) far exceeds functional work (actual parallel authority structures producing competing constitutional readings with equal weight).
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL LOGIC VIEW (MOUNTAIN) — From a universal/logical perspective, any legitimacy framework with three co-equal centers of authority cannot have a single final arbiter by definition. This is a mathematical/logical constraint on what 'co-equal' means. However, the structural data shows how this 'logical necessity' masks the contingent institutional facts that coordinate construction requires active maintenance and generates constant inter-branch conflict. The engine flags this as a false summit.
constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_authority_boundary__coordinate_construction_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The coordinate construction generates moderate extraction through several mechanisms: (1) inter-branch competition imposes suppression costs on citizens and policy implementation; (2) the rhetorical commitment to coordinate authority enables each branch to claim constitutional legitimacy while pursuing institutional expansion; (3) the uncertainty about which branch's reading prevails creates opportunities for strategic interpretation and non-compliance. However, extractiveness is not high because the coordination function is genuine — distributed authority does prevent monopoly extraction and enables all three branches to participate in meaning-making. The value reflects the mixed structure: real coordination + moderate extraction asymmetry. Suppression (0.48): Moderate. The constraint requires active maintenance because the natural tendency is toward either judicial supremacy (courts have case-controversy jurisdiction and compel compliance through contempt) or legislative supremacy (legislatures control funding). The suppression includes: rhetorical work (constitutional law doctrine teaching coordinate construction), institutional design (separation of powers mechanisms), and political conventions (acceptance of coordinate authority norms). But suppression is not high because the three branches have structural incentives to maintain their own authority — they don't need to be forced to resist dominance by the others. Theater ratio (0.55): Moderate-high. The gap between normative teaching and actual practice has grown over time. Constitutional law scholarship emphasizes coordinate construction, but empirical analysis shows that Supreme Court decisions have increasingly been treated as binding constitutional interpretation that the other branches must follow. The theater has increased from 0.35 to 0.55 over the measurement interval, indicating that the coordinate construction reading has become increasingly performative relative to the actual distribution of authority.
 *
 * PERSPECTIVAL GAP:
 *   The coordinate construction reading produces radically different classifications across power positions. Trapped citizens with no exit experience pure extraction (Snare) — they cannot arbitrage between authorities. Organized branches with constrained exits experience mixed coordination-extraction (Tangled Rope) — they benefit from distributed authority but also extract through institutional leverage. The constitutional framework itself experiences pure coordination (Rope) — distributed authority is the solution to the coordination problem of preventing monopoly. The academic discourse experiences degraded performance (Piton) — it maintains the coordinate construction reading through doctrinal tradition despite evidence that judicial supremacy has dominated actual practice. The analytical observer at civilizational scope risks naturalizing the institutional distribution as logical necessity (Mountain), when in fact it is a contingent institutional arrangement requiring active maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is determined by structural position within the coordinate construction system. Citizens have high d (trapped + no exit → 0.95) because they bear the suppression costs of inter-branch conflict without power to influence outcomes. Legislative/executive/judicial branches have moderate d (organized + constrained exits → ~0.50-0.55) because they benefit from distributed authority but face inter-branch competition. The constitutional framework institution has very low d (beneficiary + arbitrage → 0.15) because the coordinate structure is its core function. The academic discourse has high d (institutional position increasingly decoupled from actual power → 0.65) because it performs coordinate construction rhetoric while institutional reality has shifted.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the coordinate construction reading from its siblings: (1) the judicial supremacy reading, which assigns final authority to courts, forecloses the coordinate construction claim that all three branches are co-equal arbiters; (2) the parliamentary primacy reading, which assigns final authority to legislatures, similarly forecloses coordinate construction. However, within the coordinate construction reading alone, the mandatrophy is resolved by understanding that extraction and coordination coexist: the constraint enables all three branches to participate in constitutional meaning-making (coordination function) while also enabling each branch to expand its institutional reach through constitutional interpretation (extraction mechanism). The increasing theater ratio over time signals that the coordinate construction reading is losing descriptive accuracy — the actual distribution of authority has shifted toward judicial supremacy — but the coordinate construction claim persists in legal doctrine and political rhetoric due to identity lock and institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_vs_judicial_supremacy_empirical,
    'Does actual constitutional interpretation in practice exhibit genuinely parallel coordinate authority (all three branches producing competing readings with comparable weight), or does judicial supremacy dominate despite coordinate construction rhetoric?',
    'Empirical tracking of constitutional disputes: count outcomes where legislative or executive constitutional interpretation prevails against explicit judicial precedent; measure citation patterns in subsequent jurisprudence; analyze amendment/override frequency',
    'If coordinate: constraint is Tangled Rope across all institutional perspectives (coordination + extraction coexist). If judicial supremacy: constraint is actually a Snare (judicial extraction masked by coordinate rhetoric) — the reading itself instantiates false summit phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinate_vs_judicial_supremacy_empirical, empirical, 'Whether coordinate construction is descriptive or normative').

omega_variable(
    crisis_threshold_for_authority_collapse,
    'What level of inter-branch constitutional disagreement triggers institutional failure? At what point does distributed authority create deadlock rather than coordination?',
    'Historical case analysis of constitutional crises (nullification, Reconstruction, executive overreach, etc.); identification of tipping points where one branch capitulates or authority consolidates',
    'If threshold is high: coordinate construction is robust Tangled Rope. If threshold is low: distributed authority is fragile, and the suppression cost (maintaining branch independence against pressure to consolidate) becomes the dominant structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_threshold_for_authority_collapse, empirical, 'Institutional stability threshold for coordinate authority').

omega_variable(
    amendment_mechanism_sufficiency,
    'Does the amendment process function as a genuine outlet for reforming the coordinate construction (scaffold sunset), or does the difficulty of amendment make it effectively inaccessible?',
    'Amendment success rates and timeline analysis; comparison of proposed vs ratified amendments addressing inter-branch authority; public polling on perceived legitimacy of amendment process',
    'If accessible: scaffold perspective is structurally sound. If inaccessible: coordinate construction lacks a democratic exit, and the constraint reclassifies toward snare (locked distribution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_mechanism_sufficiency, empirical, 'Whether constitutional amendment provides functional democratic exit').

omega_variable(
    reading_committer_ambiguity,
    'Is the coordinate construction reading a descriptive claim about how constitutional authority actually functions, or a normative commitment to how it ought to function? Do the framers'' intent support coordinate construction or judicial primacy?',
    'Originalist textual analysis of Federalist Papers, state ratification debates, and constitutional structure; comparison of framing-era norms vs contemporary institutional practice; identification of whether original understanding supported coordinate authority or delegated final authority to courts',
    'If descriptive: the reading grounds itself in fact. If normative: the reading is aspirational and may face axiom_overriding drift as practice diverges. If frames intended judicial primacy: this reading is in substantive foreclosure relation to the original understanding (though not to contemporary coordinate construction advocates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Descriptive vs normative status of coordinate construction claim').

omega_variable(
    institutional_identity_lock_coordinate_reading,
    'To what extent is the academic, political, and judicial commitment to coordinate construction reading locked by institutional and professional identity (law school curriculum, judicial education, separation-of-powers doctrine as professional identity anchor) rather than by structural evidence?',
    'Meta-analysis of constitutional law scholarship: track whether coordinate construction is defended on structural grounds or on doctrinal-tradition grounds; survey legal academics on counterfactual: ''if the evidence showed judicial supremacy was inevitable, would you revise the coordinate construction teaching?''; examine whether legal education in different jurisdictions (parliamentary systems, hybrid systems) teach coordinate construction',
    'If highly identity-locked: the reading persists despite empirical falsification because it constitutes professional legitimacy. This instantiates the oracle gap (Theorem 4) — the analytical observer inside the legal system cannot see the reading''s identity lock. If weakly locked: the reading survives because it is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_coordinate_reading, conceptual, 'Identity-lock status of coordinate construction reading in legal institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constauth_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(constauth_tr_t50, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(constauth_tr_t100, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(constauth_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(constauth_be_t50, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(constauth_be_t100, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(constauth_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(constauth_su_t50, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(constauth_su_t100, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, inter_branch_deadlock_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers_degradation).

% DUAL FORMULATION NOTE:
% The constitutional authority boundary is a single kernel with three structurally distinct readings: coordinate_construction_reading (this file), judicial_supremacy_reading, and parliamentary_primacy_reading. Each reading produces a different ε and different classification profile. The network links show that all three are interpretations of the same underlying constitutional text, but they instantiate different constraints due to their different axioms and authority groundings. The network also links to downstream constraints that depend on which reading is adopted: inter-branch deadlock results from coordinate construction; judicial overreach results from judicial supremacy; legislative entrenchment results from parliamentary primacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
