% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_categorical_prohibition, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: IHL Categorical Prohibition on Autonomous Weapons (Martens Clause Reading)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the CATEGORICAL PROHIBITION READING of the
 *   contested IHL kernel governing autonomous lethal weapons systems (LAWS).
 *   The kernel is Martens Clause principles of humanity and public conscience
 *   embedded in humanitarian law. This reading interprets Martens as
 *   mandating an absolute ban: machine-decided killing violates human dignity
 *   per se, regardless of technical performance or outcomes-based assessment.
 *   The reading forecloses outcomes-based justifications (e.g., 'AI targeting
 *   is more precise than human bombing'). It also forecloses the human-agency
 *   reading (which permits LAWS if human deliberation is preserved in the
 *   loop). This categorical prohibition reading extracts from advanced
 *   military technology states and the defense industrial complex
 *   (foreclosing an entire technology class) while benefiting anti-militarist
 *   civil society and technologically disadvantaged states (who benefit from
 *   the leveling of military advantage). The constraint exhibits high
 *   suppression (treaty enforcement, export controls, reputational coercion)
 *   and moderate-high extractiveness (the prohibition removes a significant
 *   military capability market). The theater ratio is moderate — the
 *   verification system relies partly on norm cascades and reputational cost,
 *   not purely on technical inspection. The analytical observer risks
 *   naturalizing this reading as an immutable humanitarian law principle,
 *   when it is actually one defensible interpretation of an indeterminate
 *   legal kernel.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary victim (powerless/trapped) — irreversibly subjected to autonomous targeting. No exit from this domain once systems are deployed.
 *   - Anti-Militarist Civil Society and Humanitarian Law Advocates: Primary beneficiary (moderate/constrained) — this reading aligns with their normative commitment. They face suppression via marginalization in technical debates but retain organizing power.
 *   - Technologically Disadvantaged States: Secondary beneficiary (institutional/arbitrage) — categorical ban protects their relative military position. Low extraction because the coordination benefit is clear.
 *   - Advanced Military Technology States: Secondary victim (institutional/constrained) — extraction derives from capping autonomous capability advantage. Also experience some coordination benefit (mutual vulnerability eliminated). Mixed Tangled Rope experience.
 *   - Defense Industrial Complex and Autonomous Systems Researchers: Primary victim (powerful/mobile) — despite mobility, experience snare because the categorical prohibition forecloses their primary product category entirely. Zero arbitrage space.
 *   - IHL Verification System: Institutional observer (institutional/constrained) — maintains the prohibition framework but faces theater because autonomous software is inherently unverifiable. Theater ratio reflects norm-maintenance vs. technical verification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.75).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, snare).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "IHL Categorical Prohibition on Autonomous Weapons (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, 'kernel-reading-ihl-categorical-20260226').
narrative_ontology:cs_kernel_codification('kernel-reading-ihl-categorical-20260226', formalized).
narrative_ontology:cs_authority_grounding('kernel-reading-ihl-categorical-20260226', lineage).
narrative_ontology:cs_interpretation_layer_present('kernel-reading-ihl-categorical-20260226').
narrative_ontology:cs_reading_relation('kernel-reading-ihl-categorical-20260226', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('kernel-reading-ihl-categorical-20260226', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('kernel-reading-ihl-categorical-20260226', foundational, human_dignity_inalienable_in_lethal_decision).
narrative_ontology:cs_axiom_status(human_dignity_inalienable_in_lethal_decision, holdable).
narrative_ontology:cs_axiom_grounding('kernel-reading-ihl-categorical-20260226', human_dignity_inalienable_in_lethal_decision, deontological).
narrative_ontology:cs_axiom('kernel-reading-ihl-categorical-20260226', foundational, martens_clause_forecloses_performance_justification).
narrative_ontology:cs_axiom_status(martens_clause_forecloses_performance_justification, holdable).
narrative_ontology:cs_axiom_grounding('kernel-reading-ihl-categorical-20260226', martens_clause_forecloses_performance_justification, deontological).
narrative_ontology:cs_reference_frame('kernel-reading-ihl-categorical-20260226', martens_absolute_principles).
narrative_ontology:cs_drift_state('kernel-reading-ihl-categorical-20260226', contemporary_autonomous_systems_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('kernel-reading-ihl-categorical-20260226', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, technologically_disadvantaged_states).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, humanitarian_law_tradition).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_technology_states).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, autonomous_systems_researchers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_industrial_complex).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped by the irreversibility of autonomous lethal action. No exit from machine-decided killing once deployed. The categorical prohibition reading claims this represents absolute, non-negotiable extraction of human dignity. Civilians cannot exit the domain of autonomous weapons; they bear the full cost of any machine error or mission drift. Maximum experienced suppression.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITARIAN LAW ADVOCATES (SNARE) — Constrained by military doctrine inertia, state sovereignty claims, and lack of enforcement mechanism. The categorical prohibition reading is their position: LAWS are categorically unlawful regardless of performance metrics. They face suppression via classification as anti-defense, undermined authority, and marginalization in technical debates. But they retain some organizing power through treaty ratification campaigns and public advocacy.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TECHNOLOGICALLY DISADVANTAGED STATES (ROPE) — Experience the categorical prohibition as pure coordination benefit. A global ban on LAWS protects their military position by preventing advanced states from gaining autonomous capability advantage. Low extraction — the beneficiary is clear and structural. Exit option is arbitrage: they can defect to human-controlled systems (which they already use) or comply with prohibition (their preferred outcome).
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVANCED MILITARY TECHNOLOGY STATES (TANGLED ROPE) — Experience mixed extraction and coordination. The categorical prohibition extracts by capping their technological advantage (snare-like), but it also provides coordination benefit: mutual vulnerability to LAWS deployment is eliminated, reducing destabilization risk from accidents or rogue deployments. Constrained exit: they face pressure to comply via treaty, but retain ambiguity in compliance definitions (human-in-loop semantics are contested). Some genuine coordination function exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEFENSE INDUSTRIAL COMPLEX (SNARE) — Despite having mobile exit options (pivot to civilian applications, other military domains), experience the categorical prohibition as snare because the constraint is designed to foreclose their primary product category entirely. The categorical reading leaves zero arbitrage space: LAWS are prohibited per se, not contingently. High suppression via treaty verification mechanisms, technology export controls, and reputational cost. Extraction derives from the removal of an entire technology market, not from coercive enforcement overhead.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: IHL VERIFICATION SYSTEM (PITON) — The categorical prohibition produces significant institutional theater. Defining 'autonomous' (vs human-in-loop decision-making) is inherently ambiguous. Verification of compliance relies on self-reporting, impossible-to-inspect software, and contestable definitions of machine autonomy. The constraint's performative content (treaties, compliance declarations, oversight bodies) is high, while actual verification of covert LAWS development is uncertain. Theater ratio reflects that enforcement relies on norm maintenance rather than technical verification.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MARTENS NATURAL LAW VIEW (MOUNTAIN) — The categorical prohibition reading claims that Martens Clause principles constitute an irreducible legal minimum: human dignity, distinction, proportionality, and public conscience are civilizational commitments that transcend technical possibility. From this view, LAWS are categorically impermissible because machine-decided killing violates human dignity *per se*, not contingently. This perspective risks naturalizing what may be a constructed interpretive reading of humanitarian law tradition.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ihl_distinction_proportionality__categorical_prohibition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, TR),
    TR >= 0.70.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The categorical prohibition extracts by removing an entire technology market and capping advanced states' military advantage. The extraction is not maximal (0.95+) because some beneficiaries exist (technologically disadvantaged states, humanitarian norms), creating coordination function alongside extraction. The measurement trajectory shows extractiveness rising over time (0.35 → 0.52 → 0.68) as the prohibition becomes more costly to states with autonomous systems in development. Suppression (0.75): High. The constraint operates via treaty enforcement, technology export controls, reputational coercion, and marginalization of research communities. Suppression is not total (0.95+) because the prohibition remains contested — advanced states retain defection options and ambiguous compliance definitions (human-in-loop semantics). The measurement trajectory shows suppression rising as verification mechanisms are strengthened and norm cascades intensify. Theater ratio (0.35): Moderate. The categorical prohibition is less performative than outcomes-based readings would be, because the rule is simple ('LAWS prohibited, period') rather than contingent ('LAWS prohibited if they violate distinction/proportionality'). But theater exists because verification of autonomous decision-making is technically impossible — enforcement relies on self-reporting, norm adherence, and reputational cost rather than inspection. The reading's power derives from clarity (no ambiguity about what is prohibited) rather than from technical verification capacity.
 *
 * PERSPECTIVAL GAP:
 *   The categorical prohibition reading produces stark perspectival divergence. The defense industrial complex sees pure snare (extraction with zero coordination benefit, because the prohibition forecloses their product entirely). Advanced states see Tangled Rope (mixed extraction and coordination). Disadvantaged states see Rope (pure coordination benefit). Humanitarian advocates see Rope (norm alignment with low extraction). Civilians see Snare (irreversible harm with no exit). The verification system sees Piton (performative treaty maintenance with uncertain enforcement). The analytical observer risks Mountain (naturalizing as immutable humanitarian law principle). The perspectival gap reveals that the reading's validity depends on one's structural position relative to autonomous weapons development and deployment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to the categorical prohibition. Defense industrial actors who benefit from LAWS development face d ≈ 0.90 (full target of extraction). Technologically disadvantaged states who benefit from the ban face d ≈ 0.05 (full beneficiary). Advanced military states experience d ≈ 0.65 (mixed: extraction via capability cap, coordination via mutual vulnerability reduction). Humanitarian law advocates experience d ≈ 0.25 (beneficiary of norm alignment, but constrained by enforcement gaps). The analytical observer faces d ≈ 0.72 (ambiguous position: claims to observe objectively while risking naturalization of a contested reading). These values feed into the sigmoid f(d) function to produce chi (effective extraction). The constraint's chi varies significantly across perspectives: beneficiaries experience low or negative chi, victims experience high chi, and constrained or mixed actors experience moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε > 0.70): This constraint avoids mandatrophy because its classification varies legitimately with agent perspective. There is no contradiction between snare (defense industry), tangled rope (advanced states), rope (disadvantaged states), and piton (verification system) — each is the correct classification from that perspective's position. The mandatrophy would arise if the constraint were classified as a single type from all positions. Instead, the multi-perspective structure is the feature, not a bug. The categorical prohibition is a snare for those who benefit from LAWS development, a rope for those who benefit from the ban, and a coordination mechanism with theatrical elements for the verification system. The analytical observer's risk of naturalizing the reading as Mountain is resolved by examining the beneficiary/victim structure — the constraint clearly benefits some agents (humanitarian advocates, disadvantaged states) and harms others (military technologists), indicating it is constructed, not natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    martens_clause_interpretation_contest,
    'Does Martens Clause ''principles of humanity and public conscience'' mandate categorical prohibition of LAWS, or do they permit LAWS that demonstrably respect distinction and proportionality?',
    'Historical-textual analysis of Martens Clause adoption (1899 Hague Convention II, Preamble) and subsequent state practice; legal interpretation across ICJ precedents, ICRC guidance, and state military doctrine documents. Comparison with how other technology prohibitions (chemical weapons, biological weapons, blinding lasers) were grounded in Martens language vs. technical specifications.',
    'If Martens mandates categorical prohibition: the reading is structurally sound — no autonomy level can satisfy the constraint. If Martens permits outcomes-based assessment: the categorical prohibition collapses to a constrained outcomes-based reading (sibling constraint), and ε drops substantially (from 0.68 to ~0.35).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_interpretation_contest, conceptual, 'Whether Martens Clause mandates categorical LAWS prohibition or permits outcomes-based assessment').

omega_variable(
    human_agency_irreplaceability,
    'Is there a meaningful structural distinction between human-in-loop targeting (human reviews AI recommendation) and outcome-equivalence (human and machine produce identical targeting decisions)? Does the categorical prohibition rest on irreplaceability of human agency or on empirical outcomes?',
    'Comparative analysis of proportionality assessments: human military lawyers vs. AI systems trained on past targeting decisions. Field study of actual human-in-loop systems to determine whether human review is genuine deliberation or post-hoc ritual. Examination of whether the categorical prohibition''s axiom is ''humans must decide'' (irreplaceability) or ''outcomes must respect distinction/proportionality'' (outcomes-based).',
    'If human agency is irreplaceable per se: categorical prohibition is well-grounded regardless of empirical outcomes. If outcomes are the true criterion: the constraint is actually outcomes-based (sibling reading), and the categorical prohibition is aspirational framing of an empirical standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_agency_irreplaceability, conceptual, 'Whether categorical prohibition rests on human agency irreplaceability or empirical outcomes').

omega_variable(
    enforcement_mechanism_realism,
    'Given the technical impossibility of verifying autonomous decision-making in hidden military systems, does the categorical prohibition (requiring universal enforcement) represent a realistic legal constraint, or does it rely on norm cascades and defection risk rather than actual compliance verification?',
    'Comparative analysis of verification mechanisms: chemical weapons (inspectable chemical facilities), biological weapons (fermentation equipment dual-use), blinding lasers (technically identifiable optical specifications) vs. LAWS (software verification is fundamentally opaque). Historical analysis of whether categorical bans (landmines, cluster munitions) achieved compliance through verification or through norm adherence + reputational cost of defection.',
    'If verification is impossible: suppression score may be overstated (actual enforcement is theater, not structural coercion). Constraint may degrade toward scaffold or piton (temporary norm, not permanent legal structure). If norm cascades are sufficient: suppression is correct; categorical prohibition works through reputational mechanism, not inspection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_realism, empirical, 'Whether technical verification of LAWS compliance is possible or if enforcement relies entirely on norm adherence').

omega_variable(
    kernel_reading_ambiguity,
    'Is this reading (categorical prohibition grounded in Martens absolute principles) the authentic interpretation of IHL, or is it one of multiple defensible readings of a contested kernel?',
    'This is a committer-structure omega. Documented explicitly in cs_structure.reading_relations and cs_structure.axioms. The sibling readings (human_agency_reading, outcomes_based_reading) represent alternative framings of the same legal foundation. The engine''s reading-relations classifier determines whether this reading forecloses the siblings or coexists with them.',
    'If categorical prohibition forecloses other readings: LAWS are unlawful under all frames. If coexists: the constraint''s type is reading-dependent, and there is genuine legal indeterminacy at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether categorical prohibition is the authoritative IHL reading or one of multiple coexisting readings').

omega_variable(
    civilian_harm_empirics_vs_principles,
    'Does the categorical prohibition derive from empirical evidence that autonomous systems reliably cause more civilian harm than human-controlled systems, or from a principled claim that human dignity cannot be delegated to machines regardless of empirical performance?',
    'Historical data on civilian casualties in conflicts with and without autonomous targeting assistance (e.g., drone strikes with human final decision vs. theoretical fully-autonomous systems). Controlled studies of targeting accuracy and proportionality assessment (human lawyers vs. AI systems trained on past decisions). Philosophical analysis of whether the prohibition is empirical or deontological.',
    'If empirical: the constraint could be contingent on evidence — improved AI might satisfy the prohibition. If deontological: the prohibition is absolute regardless of evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_harm_empirics_vs_principles, empirical, 'Whether categorical prohibition grounds in empirical harm data or principled human dignity claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_cat_theater_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ihl_cat_theater_t5, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ihl_cat_theater_t10, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ihl_cat_extract_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ihl_cat_extract_t5, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ihl_cat_extract_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl_cat_suppress_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ihl_cat_suppress_t5, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ihl_cat_suppress_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, laws_development_incentive_trap).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, autonomous_weapons_verification_bottleneck).

% DUAL FORMULATION NOTE:
% This constraint is part of the ihl_distinction_proportionality kernel family. The categorical prohibition reading (this file) represents the interpretive position that Martens Clause principles mandate absolute ban on machine-decided killing. The human_agency_reading permits human-in-loop systems. The outcomes_based_reading permits LAWS if they demonstrably improve distinction/proportionality. All three are readings of the same legal kernel but represent structurally distinct constraints with different ε values and different victim/beneficiary structures. The categorical reading has the highest ε (0.68) because it forecloses entire technology class; outcomes-based has lower ε (~0.35) because it creates compliance pathway; human-agency is intermediate. Network relationships reflect causal dependency: categorical prohibition forecloses the outcomes-based reading within any single legal framework, creating downstream pressure on states considering LAWS development (affects_constraints: laws_development_incentive_trap). The categorical reading also affects verification bottleneck because the prohibition's verification impossibility generates institutional theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, powerful, 0.9).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
