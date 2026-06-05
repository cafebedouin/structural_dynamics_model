% ============================================================================
% CONSTRAINT STORY: tribunician_sacrosanctity__popular_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribunician_sacrosanctity__popular_shield_reading, []).

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
 *   constraint_id: tribunician_sacrosanctity__popular_shield_reading
 *   human_readable: Tribunician Sacrosanctity as Popular Shield
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The tribunician veto power in the Roman Republic presents one of
 *   history's clearest cases of a constraint that functions as both
 *   coordination mechanism and power transfer. This constraint story
 *   instantiates the POPULAR SHIELD READING: the inviolable tribune exists to
 *   stand between the powerless citizen and the magistrate's executive
 *   coercion. In this reading, sacrosanctity is a structural mechanism that
 *   reduces suppression at the point of contact — a tribune's status is that
 *   their body cannot be touched, and this untouchability becomes the
 *   citizen's shield. The constraint coordinates the republic by creating a
 *   predictable veto point against summary magisterial force. However, this
 *   reading coexists with two structurally coherent alternatives: the
 *   DEMAGOGIC LEVER reading (sacrosanctity became the instrument for
 *   ambitious tribunes to seize factional advantage, wielding the people's
 *   veto as cover for personal power) and the IMPERIAL ABSORPTION reading
 *   (sacrosanctity ended as Augustus incorporated tribunician power into the
 *   throne, converting the shield into the crown's legitimacy foundation).
 *   All three readings are empirically consistent with the historical record
 *   — they represent different framings of the same institutional facts. The
 *   shield reading emphasizes the protective function and sees extraction as
 *   secondary. The demagogic reading emphasizes leverage and sees protection
 *   as incidental cover. The imperial reading emphasizes the eventual
 *   consolidation of all three forces (shield, lever, crown) into a single
 *   authority. This constraint instantiates the shield reading; the other
 *   readings are separate constraint stories with their own ε values and
 *   network positions.
 *
 * KEY AGENTS:
 *   - Plebeian Citizen: Primary beneficiary (powerless/constrained) — sacrosanctity interrupts magisterial coercion at the bodily level; reduces immediate suppression
 *   - Plebeian Assembly: Secondary beneficiary (organized/constrained) — tribunal veto provides focal point for collective grievance and constraint on magisterial override
 *   - Patrician Magistrate: Structural victim of extracted coercive power (institutional/constrained) — the magistrate's unilateral coercive capacity is curtailed by the tribune's veto
 *   - Republic Constitution: System-level beneficiary (institutional/constrained) — sacrosanctity enables negotiated hierarchy rather than constant civil war
 *   - Individual Tribune: Institutional actor (institutional/constrained) — occupies a role with genuine power to protect but also opportunity to leverage; in the shield reading, the protective function is primary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tribunician_sacrosanctity__popular_shield_reading, 0.18).
domain_priors:suppression_score(tribunician_sacrosanctity__popular_shield_reading, 0.35).
domain_priors:theater_ratio(tribunician_sacrosanctity__popular_shield_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tribunician_sacrosanctity__popular_shield_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(tribunician_sacrosanctity__popular_shield_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(tribunician_sacrosanctity__popular_shield_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tribunician_sacrosanctity__popular_shield_reading, rope).
narrative_ontology:human_readable(tribunician_sacrosanctity__popular_shield_reading, "Tribunician Sacrosanctity as Popular Shield").
narrative_ontology:topic_domain(tribunician_sacrosanctity__popular_shield_reading, "legal/doctrinal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tribunician_sacrosanctity__popular_shield_reading, '74284b39-f062-43d7-8b3c-34f9955c5e85').
narrative_ontology:cs_kernel_codification('74284b39-f062-43d7-8b3c-34f9955c5e85', formalized).
narrative_ontology:cs_authority_grounding('74284b39-f062-43d7-8b3c-34f9955c5e85', lineage).
narrative_ontology:cs_interpretation_layer_present('74284b39-f062-43d7-8b3c-34f9955c5e85').
narrative_ontology:cs_reading_relation('74284b39-f062-43d7-8b3c-34f9955c5e85', tribunician_sacrosanctity__demagogic_lever_reading, coexists_with).
narrative_ontology:cs_reading_relation('74284b39-f062-43d7-8b3c-34f9955c5e85', tribunician_sacrosanctity__imperial_absorption_reading, influences).
narrative_ontology:cs_axiom('74284b39-f062-43d7-8b3c-34f9955c5e85', foundational, inviolable_tribune_protects_powerless).
narrative_ontology:cs_axiom_status(inviolable_tribune_protects_powerless, holdable).
narrative_ontology:cs_axiom_grounding('74284b39-f062-43d7-8b3c-34f9955c5e85', inviolable_tribune_protects_powerless, deontological).
narrative_ontology:cs_axiom('74284b39-f062-43d7-8b3c-34f9955c5e85', foundational, summary_magisterial_coercion_is_arrestable).
narrative_ontology:cs_axiom_status(summary_magisterial_coercion_is_arrestable, holdable).
narrative_ontology:cs_axiom_grounding('74284b39-f062-43d7-8b3c-34f9955c5e85', summary_magisterial_coercion_is_arrestable, empirically_contingent).
narrative_ontology:cs_reference_frame('74284b39-f062-43d7-8b3c-34f9955c5e85', plebeian_protective_function).
narrative_ontology:cs_drift_state('74284b39-f062-43d7-8b3c-34f9955c5e85', late_republic_factional_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74284b39-f062-43d7-8b3c-34f9955c5e85', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(tribunician_sacrosanctity__popular_shield_reading, tribunician_sacrosanctity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tribunician_sacrosanctity__popular_shield_reading, plebeian_citizen).
narrative_ontology:constraint_beneficiary(tribunician_sacrosanctity__popular_shield_reading, powerless_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN CITIZEN (ROPE) — The inviolable tribune provides genuine coordination benefit: a reliable mechanism to halt magisterial coercion at the point of bodily contact. The citizen's exit is constrained by social hierarchy and resource barriers, but the tribune's presence materially reduces suppression they would otherwise face. Low extractiveness because the office's function is aligned with the citizen's survival interest.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: PLEBEIAN ASSEMBLY (ROPE) — Sacrosanctity coordinates the assembly's collective voice: the inviolable tribune serves as a focal point for grievances and a mechanism to block magisterial override of assembly will. The extraction is minimal because the office serves the assembly's direct interests. Organized power allows some exit (exit the city, form alternative structures) but social and legal barriers make this costly.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PATRICIAN MAGISTRACY (TANGLED ROPE) — The magistrate faces a genuine coordination problem: the tribune's veto power interrupts unilateral coercion, but this creates a predictable mechanism for negotiation rather than chaos. The magistrate must treat the tribune as an institutional counterweight. Extractiveness emerges because the magistrate's unilateral power is curtailed — the constraint transfers coercive capacity from magistrate to tribune. But this is mixed with coordination function: the veto mechanism enables structured conflict resolution rather than violence.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: REPUBLIC CONSTITUTION (ROPE) — Sacrosanctity coordinates the entire constitutional order: it creates a structural balance between patrician executive power and plebeian veto, enabling the republic to function without constant civil conflict. The extractiveness is low across the generational horizon because the mechanism serves system stability. Exit options are constrained by the embedded nature of constitutional structures — magistrates are born into hierarchy; plebeians are born into subordination — but sacrosanctity reduces the *cost* of non-exit by making subordination negotiable rather than absolute.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN?) — From the civilizational analytical view, sacrosanctity appears as a natural law of power: any stable system of hierarchy requires some mechanism by which the powerless can veto catastrophic abuse. The inviolable tribune is the logical form this mechanism takes in a hierarchical society — a person whose touch cannot be violated is the only guarantee that power cannot run unlimited. This perspective risks naturalizing a contingent Roman institutional solution as a civilizational universal. The constraint is NOT a mountain — it is a highly specific contingent invention. The mountain classification here is a false summit revealing how the shield reading can itself be instrumentalized as an argument for inevitability.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tribunician_sacrosanctity__popular_shield_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tribunician_sacrosanctity__popular_shield_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(tribunician_sacrosanctity__popular_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint coordinates the republic by transferring coercive capacity from magistrate to tribune, but the primary beneficiary is the powerless citizen — the tribune's veto protects against summary coercion. The extractiveness is not zero (the magistrate experiences a loss of unilateral power) but it is low because the transfer serves system coordination. If the tribune used the veto primarily for factional leverage, extractiveness would rise sharply toward 0.45-0.65; that is the demagogic reading with higher ε. The shield reading holds low extractiveness by maintaining that protection is the primary function. Suppression (0.35): Moderate. Sacrosanctity reduces suppression at the point of contact — a magistrate cannot legally coerce a tribune, and this legal immunity extends protection to plebeians who can invoke the tribune's status. However, suppression is not eliminated: the plebeian still faces overwhelming structural inequality, lack of economic resources, and limited exit options. Sacrosanctity interrupts summary violence but does not eliminate systemic subordination. Theater ratio (0.28): Low. The mechanism is concrete: the tribune's body is inviolable, and this physical fact enables the veto. Invocation of sacrosanctity is more material than performative — it works because actors treat the inviolability as real. The theater increases slightly over the 50-year interval (0.25 to 0.30) as the institution ages and deference to the office begins to substitute for genuine fear of sacrosanct violation.
 *
 * PERSPECTIVAL GAP:
 *   The plebeian citizen and the patrician magistrate perceive the same constraint through opposite directionality vectors. For the citizen (powerless), sacrosanctity is pure benefit — a rare mechanism that interrupts otherwise unchecked coercion. The citizen classifies it as rope: coordination that reduces their suppression. For the magistrate (institutional), sacrosanctity is a structural constraint on their own executive power — they cannot unilaterally coerce, and must negotiate with the tribune instead. The magistrate experiences this as extraction of their unilateral authority. Yet the magistrate also perceives coordination value: the veto mechanism is predictable, and predictability enables strategic action. This creates the tangled rope classification: genuine coordination (the veto is a focal point for negotiation) alongside genuine extraction (coercive power is transferred from magistrate to tribune). The plebeian assembly bridges these: they see the protection and the opportunity to veto together, which is rope. The republic as a whole sees the constitutional balance that makes hierarchy stable — also rope. The analytical observer risks seeing sacrosanctity as a universal natural law (mountain) unless they remain aware that this is one specific institutional invention among many possible alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   In the shield reading, the plebeian citizen's structural position is as beneficiary: they gain protection from magisterial coercion. The magistrate's position is as victim of extracted coercive power: their unilateral authority is constrained. The tribune's position is as institutional custodian: they wield the power but in the shield reading the power is exercised to protect the powerless, not to extract from them. Directionality at the citizen level: beneficiary status + constrained exit + powerless power = low d, minimal experienced extraction. Directionality at the magistrate level: victim-of-constraint status + constrained exit + institutional power = elevated d, but not maximal because the constraint is negotiable rather than absolute. The constraint is coordination-weighted rather than extraction-weighted because both beneficiary and victim experience the mechanism as stabilizing: the citizen stops fearing arbitrary coercion; the magistrate stops facing unpredictable resistance. This is what makes it rope rather than tangled rope at the citizen and assembly levels — the extraction flow is minimal.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacrosanctity_materiality_vs_performativity,
    'Does sacrosanctity function through actual enforcement of bodily inviolability (magistrates cannot physically touch the tribune) or through performative invocation (the tribune''s status is powerful because actors treat it as such)?',
    'Historical record of attempted violations: did magistrates ever attempt to arrest or strike a tribune? What happened when they did? If zero violations occurred, mechanism is performative (credible threat sufficient). If violations occurred and were prosecuted, mechanism is material enforcement.',
    'If performative: the constraint is less robust than the shield reading claims — it depends on magistrate deference and can collapse if that deference fails. If material enforcement: the shield reading is accurate, but sacrosanctity is expensive to maintain (requires consensus that violations are unacceptable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacrosanctity_materiality_vs_performativity, empirical, 'Whether sacrosanctity is enforced through bodily inviolability or performative deference').

omega_variable(
    shield_reading_vs_demagogic_reading_empirical_gate,
    'Did tribunes elected on anti-magistrate platforms actually reduce summary coercion against plebeians, or did they primarily use veto power for factional advantage?',
    'Quantitative analysis of tribunal veto records: proportion of vetoes protecting plebeian citizen bodies vs vetoes used for factional leverage; temporal correlation between specific tribunes'' actions and rates of magisterial coercion against citizen population.',
    'If shield function dominant (>60% of vetoes protect citizen bodies): popular shield reading is empirically supported. If demagogic leverage dominant (>60% of vetoes serve factional interests): demagogic lever reading is empirically supported. If mixed: both readings are empirically coherent; the contest is about framing priorities, not falsifiable facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shield_reading_vs_demagogic_reading_empirical_gate, empirical, 'Empirical proportion of protective vs factional tribune vetoes').

omega_variable(
    reading_contest_not_resolvable_by_data,
    'Is the contest between the three readings fundamentally about how to weight different types of evidence (protection vs leverage, shield vs tool), or is one reading factually wrong?',
    'Meta-analysis: even if empirical data shows both protective and demagogic tribune actions coexisted, the readings still represent three different framings of the same facts. The shield reading emphasizes the protective function and sees leverage as secondary. The demagogic reading emphasizes leverage and sees protection as secondary. The imperial reading emphasizes decay of either function into pure power consolidation. Different weight, not different facts.',
    'If fundamentally about framing: all three readings coexist (coexists_with relations are correct). If one reading''s factual premise is false: at least one reading forecloses another. Example: if the shield reading requires that sacrosanctity actually protected plebeians *in practice* and empirical data shows zero protective effect, then the shield reading''s core premise collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_not_resolvable_by_data, conceptual, 'Whether reading contest is empirical or fundamentally about framing priorities').

omega_variable(
    authority_grounding_shift_during_interval,
    'Did sacrosanctity''s authority grounding shift from lineage (transmission of the office as inviolable) to extraction (magistrates treating sacrosanctity as a constraint they must work within for their own benefit) across the interval?',
    'Textual and institutional analysis: do early tribunes invoke sacrosanctity as an inherited right and obligation? Do later tribunes invoke it as a tool for leverage? Does the rhetorical frame change from ''this office is inviolable because it was founded inviolable'' to ''this office is inviolable because I will use it''?',
    'If authority_grounding shifts from lineage to extraction: the constraint evolves from a shield (lineage-grounded obligation to protect) to a lever (extraction-grounded power grab). This would support the demagogic reading as a historical evolution rather than an alternative interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_shift_during_interval, empirical, 'Whether authority grounding of sacrosanctity shifts from lineage to extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tribunician_sacrosanctity__popular_shield_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tribunician_theater_t0, tribunician_sacrosanctity__popular_shield_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tribunician_theater_t25, tribunician_sacrosanctity__popular_shield_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(tribunician_theater_t50, tribunician_sacrosanctity__popular_shield_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(tribunician_extractiveness_t0, tribunician_sacrosanctity__popular_shield_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tribunician_extractiveness_t25, tribunician_sacrosanctity__popular_shield_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(tribunician_extractiveness_t50, tribunician_sacrosanctity__popular_shield_reading, base_extractiveness, 50, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(tribunician_suppression_t0, tribunician_sacrosanctity__popular_shield_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tribunician_suppression_t25, tribunician_sacrosanctity__popular_shield_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(tribunician_suppression_t50, tribunician_sacrosanctity__popular_shield_reading, suppression_requirement, 50, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tribunician_sacrosanctity__popular_shield_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__popular_shield_reading, tribunician_sacrosanctity__demagogic_lever_reading).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__popular_shield_reading, tribunician_sacrosanctity__imperial_absorption_reading).

% DUAL FORMULATION NOTE:
% The tribunician sacrosanctity kernel generates three structurally distinct constraints with different ε values. The shield reading (this constraint, ε=0.18) emphasizes the protective coordination function and low extraction. The demagogic lever reading (ε=0.55) emphasizes factional leverage and asymmetric extraction. The imperial absorption reading (ε=0.72) emphasizes the eventual consolidation of all institutional power into the throne. Each reading is a complete constraint story with its own perspectives, beneficiary/victim structure, and empirical status. They are linked via network.affects_constraints to show that alternative readings of the same kernel would change the structural classification significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
