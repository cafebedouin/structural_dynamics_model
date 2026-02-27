% ============================================================================
% CONSTRAINT STORY: procedural_legitimacy_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_legitimacy_decay, []).

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
 *   constraint_id: procedural_legitimacy_decay
 *   human_readable: The Hollow Formality Trap
 *   domain: social/political
 *
 * SUMMARY:
 *   The hollow formality trap describes a bureaucratic constraint where
 *   procedural form persists despite erosion of the social consensus that
 *   originally justified it. Examples include licensing requirements for
 *   occupations where guild gatekeeping has replaced quality assurance,
 *   residency visa procedures maintained after their original purpose
 *   dissolved, professional credentialing that screens status rather than
 *   competence, or legislative oversight committees that lack actual
 *   enforcement power. The constraint exhibits the defining feature of
 *   tangled rope: it simultaneously provides genuine coordination (uniform
 *   treatment, predictable outcomes, documented access) AND extraction (time
 *   cost, compliance overhead, gatekeeping control). The procedural
 *   administrators experience it as coordination; procedure subjects
 *   experience it primarily as extraction. The defining pathology is that the
 *   theater ratio grows as legitimacy erodes — once subjective belief in the
 *   procedure's justification decays, maintaining compliance requires
 *   increasingly elaborate performance and symbolic emphasis on procedural
 *   legitimacy itself. The constraint has moved from 35% theater (genuine
 *   coordination with some ritual) at t=0 to 78% theater (primarily ritual
 *   maintenance with residual coordination function) at t=20. This diagnostic
 *   signature distinguishes hollow formality from both genuine rope (low
 *   theater throughout) and pure piton (theater remains constant at high
 *   level as institutional inertia holds form).
 *
 * KEY AGENTS:
 *   - Procedure Subjects: Primary victims (powerless/trapped) — compelled to navigate bureaucratic process that no longer has internal justification; cannot exit without forfeiting resource/status access
 *   - Procedural Administrators: Primary beneficiaries (institutional/arbitrage) — maintain gatekeeping control and coordinate resource allocation through formality; see procedure as coordination mechanism
 *   - Compliant Participants: Secondary victims (moderate/constrained) — internalized procedure as legitimate; now trapped between eroded belief and continued compliance
 *   - Historical Procedure Tradition: Institutional memory (institutional/constrained) — procedure was once genuinely coordinative; form persists through inertia (piton perspective)
 *   - Reform Coalition: Organized agents (organized/mobile) — civil society, reformist bureaucrats, legal scholars building alternative pathways with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structural hybridity: procedure has both genuine coordination value AND extraction value
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_legitimacy_decay, 0.52).
domain_priors:suppression_score(procedural_legitimacy_decay, 0.65).
domain_priors:theater_ratio(procedural_legitimacy_decay, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_legitimacy_decay, extractiveness, 0.52).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_legitimacy_decay, tangled_rope).
narrative_ontology:human_readable(procedural_legitimacy_decay, "The Hollow Formality Trap").
narrative_ontology:topic_domain(procedural_legitimacy_decay, "social/political").

domain_priors:requires_active_enforcement(procedural_legitimacy_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_legitimacy_decay, procedural_administrators).
narrative_ontology:constraint_beneficiary(procedural_legitimacy_decay, institutional_gatekeepers).
narrative_ontology:constraint_victim(procedural_legitimacy_decay, procedure_subjects).
narrative_ontology:constraint_victim(procedural_legitimacy_decay, legitimacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROCEDURE SUBJECT (SNARE) — Individual compelled to navigate bureaucratic process that lacks internal legitimacy. Cannot exit without forfeiting access to resource/status. No alternative pathway. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROCEDURAL ADMINISTRATOR (ROPE) — Institutional actor who coordinates compliance and resource allocation through the procedure. Experiences constraint as legitimate coordination mechanism: the formality creates predictability and uniform treatment. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANT PARTICIPANT (TANGLED ROPE) — Moderate agent who has internalized procedure as legitimate despite knowing the underlying consensus has eroded. Benefits from perceived fairness and predictability; bears cost of time investment and psychological acceptance of hollow ritual. d≈0.60, f(d)≈0.72, σ=0.9 → χ≈0.34.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HISTORICAL PROCEDURE TRADITION (PITON) — Institutional memory view: the procedure was once genuinely legitimate (coordinated around shared values). The form persists through inertia even though the consensus foundation has dissolved. Theater ratio 0.78 reflects that compliance is now primarily performative maintenance of symbolic legitimacy rather than genuine collective action problem solving. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized actors (civil society, reformist bureaucrats, legal scholars) see the procedure as a temporary failure of legitimacy with a sunset clause. Distributed pressure to redesign procedures is creating alternative pathways that bypass hollow formality. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22. Low effective extraction because coalition has exit capacity and visibility.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, hollow procedures serve dual functions: they extract compliance costs from subjects while maintaining a coordination apparatus. The procedure has both genuine coordination value (uniform treatment, procedural predictability) AND extraction value (gatekeeping access, maintaining administrative control). This hybridity is the constraint's defining feature. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_legitimacy_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_legitimacy_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(procedural_legitimacy_decay, TR),
    TR >= 0.70.

:- end_tests(procedural_legitimacy_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The procedure imposes compliance costs (time investment, attention allocation, procedural navigation) on subjects. These costs are no longer justified by genuine coordination gains — they have become primarily extraction. However, some residual coordination value persists (documented access, uniform treatment principle), preventing classification as pure snare. The 0.52 value reflects that extraction is substantial but not maximal; subjects have not completely lost access to the underlying resource. Suppression (0.65): Moderate-high. Significant barriers to exit include: lack of alternative pathways, institutional requirement for procedure completion before resource access, social stigma for non-compliance, and absence of formal complaint mechanisms. However, some subjects can and do exit through informal channels or by forfeiting the resource. Theater ratio (0.78): High and rising. The procedure's justifying rationale has largely dissolved (its original purpose achieved, context changed, alternatives available), but the form persists. Current compliance is motivated primarily by fear of rejection, social conformity, and internalized legitimacy rather than agreement with underlying purpose. Administrative discourse increasingly emphasizes 'fair process' and 'consistency' (procedural values) rather than the original substantive goals, a diagnostic indicator of theater growth.
 *
 * PERSPECTIVAL GAP:
 *   The procedure subject experiences snare (high extraction, trapped exit) because they see the compliance costs without the justifying coordination benefit. The procedural administrator experiences rope (genuine coordination mechanism) because the procedure legitimately solves their coordination problem: how to allocate resources uniformly and predictably. The compliant participant experiences tangled rope because they have internalized the procedure's legitimacy narrative even as external evidence of its justification has eroded. The historical tradition perspective reveals piton (inertial degradation): the procedure was once genuinely coordinative but the foundation has dissolved. The reform coalition sees scaffold (temporary failure with sunset): new procedures without hollow formality can capture the coordination value while reducing extraction. The analytical observer sees the full hybridity: the procedure simultaneously coordinates AND extracts, and the theater ratio's growth reveals the tension between these functions as legitimacy erodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Procedure subjects: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Procedural administrators: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiaries, negative effective extraction. Compliant participants: Victims + constrained → d≈0.60, f(d)≈0.72. Moderate extraction; have constrained exit options (can exit but at significant cost). Reform coalition: Organized agents + mobile → d≈0.42, f(d)≈0.42. Low effective extraction; have agency and visible alternatives. The directionality derivation reveals the structural asymmetry: procedure administrators exit the constraint's overhead (arbitrage exit from administrative burden) while subjects remain trapped. This asymmetry is the mechanism of extraction: the procedure imposes costs it insulates administrators from bearing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially resolved by recognizing that tangled rope is the primary classification: the procedure has BOTH genuine coordination function (uniform treatment, documented access, predictable outcomes) AND asymmetric extraction (time cost, gatekeeping, administrative control). The mandatrophy would only fully resolve if evidence revealed that the coordination function is separable from extraction (scaffold/reform pathway is sufficient) — at that point the constraint would be reclassified as a temporary failure rather than a structural feature. Current evidence suggests the coordination and extraction are structurally coupled through the procedure's legitimacy apparatus: removing the formality's symbolic weight (theater reduction) risks undermining the coordination value itself. This coupling is why hollow procedures persist despite widespread recognition of their hollowness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold_collapse,
    'What percentage of procedure subjects must lose subjective belief in legitimacy before the procedure becomes objectively extractive rather than coordinative?',
    'Longitudinal survey data on perceived legitimacy; correlation with compliance rates, resistance behavior, and institutional pressure to maintain procedure',
    'If threshold < 30%: most procedures are hollow by this standard (broadens snare classification). If threshold > 70%: requires near-complete consensus loss before extraction becomes structural (narrows snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold_collapse, empirical, 'Legitimacy belief threshold for classification shift').

omega_variable(
    reform_pathway_sufficiency,
    'Can alternative procedural designs capture the genuine coordination value of hollow procedures without the extraction overhead?',
    'Pilot programs testing streamlined procedures; measurement of coordination failure rates, fairness perception, and compliance in alternative designs',
    'If yes: scaffold sunset is real and the constraint is temporary. If no: procedure is inescapable tangled rope (coordination and extraction are structurally coupled).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_pathway_sufficiency, empirical, 'Whether reform pathways can maintain coordination without hollowness').

omega_variable(
    symbolic_legitimacy_necessity,
    'Is subjective belief in procedural legitimacy a causal prerequisite for the procedure''s coordination function, or merely correlated with it?',
    'Behavioral experiments on compliance with procedures that are known to be hollow; analysis of compliance under transparency vs opacity',
    'If causal: legitimacy is the coordination mechanism itself (piton classification is correct — the form maintains function). If merely correlated: hollowness is parasitic extraction (snare is primary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symbolic_legitimacy_necessity, conceptual, 'Whether subjective legitimacy belief is necessary for procedure function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_legitimacy_decay, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(procd_tr_t0, procedural_legitimacy_decay, theater_ratio, 0, 0.35).
narrative_ontology:measurement(procd_tr_t10, procedural_legitimacy_decay, theater_ratio, 10, 0.55).
narrative_ontology:measurement(procd_tr_t20, procedural_legitimacy_decay, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(procd_be_t0, procedural_legitimacy_decay, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(procd_be_t10, procedural_legitimacy_decay, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(procd_be_t20, procedural_legitimacy_decay, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_legitimacy_decay, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_legitimacy_decay, occupational_licensing_gatekeeping).
narrative_ontology:affects_constraint(procedural_legitimacy_decay, credentialing_status_screening).
narrative_ontology:affects_constraint(procedural_legitimacy_decay, visa_procedure_inertia).

% DUAL FORMULATION NOTE:
% The hollow formality trap is a family of related constraints that share the same structural pattern: procedural form maintained despite legitimacy erosion. Specific instances (licensing, credentialing, visa procedures) have their own empirical ε values but all exhibit the theater-ratio diagnostic signature of legitimacy decay. The family members are linked because reform in one domain (e.g., licensing modernization) propagates pressure to other domains (visa/credentialing modernization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(procedural_legitimacy_decay, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
