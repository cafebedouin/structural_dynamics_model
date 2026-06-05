% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_parliamentary_sovereignty, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Constraint Authority: Parliamentary Sovereignty Reading
 *   domain: constitutional_history/political_theory/legal_philosophy
 *
 * SUMMARY:
 *   The parliamentary sovereignty reading of Magna Carta's constraint
 *   authority interprets the 1215 Charter as binding only insofar as it has
 *   been absorbed into parliamentary statute law. Under this reading,
 *   Parliament inherits the authority to interpret, modify, and repeal any
 *   charter provision. This stands in structural opposition to the living
 *   constitutionalism reading, which treats the charter as a transmissible
 *   juridical constraint binding all subsequent sovereigns, and to the feudal
 *   obsolescence reading, which denies the charter any binding authority over
 *   modern governance structures. The parliamentary sovereignty reading
 *   occupies the middle ground: the charter's restraints exist and function,
 *   but they are contingent on parliamentary choice rather than juridical
 *   binding. This creates a tangled rope structure — there is genuine
 *   coordination (parliamentary processes work better with formal charter
 *   principles) alongside genuine extraction (the majority can revise
 *   restraints without consent from those protected by them). The
 *   constraint's extractiveness (0.42) reflects this hybridity: moderate
 *   because the charter does constrain crown prerogative in normal
 *   parliamentary operation, but increasing over time as parliamentary
 *   revision of charter provisions becomes more routine and minorities lose
 *   confidence in parliamentary protection. The theater ratio (0.58) reflects
 *   that much invocation of Magna Carta operates as ceremonial legitimation
 *   rather than substantive restraint — judges cite the charter for authority
 *   while enforcing parliamentary will, and legislation explicitly repealing
 *   charter provisions is rare (invoking parliamentary courtesy rather than
 *   constitutional prohibition).
 *
 * KEY AGENTS:
 *   - Parliamentary Majority Coalition: Primary beneficiary (institutional/arbitrage) — controls both the constraint's operation and its modification; experiences legislative sovereignty as enabling their will
 *   - Constitutional Minorities: Primary victim (powerless/trapped) — cannot exit parliamentary process; no constitutional authority protects them from majoritarian revision of their protections
 *   - Parliamentary Opposition Factions: Secondary agent (moderate/constrained) — benefit from parliament's existence but face suppression of their revisionary power; constrained exit at biographical time
 *   - Constitutional Reform Movements: Organized agent (organized/constrained) — seek to entrench rights against parliamentary revision; face genuine suppression but organized agency
 *   - Judiciary and Legal Profession: Institutional actor (institutional/arbitrage) — maintain ceremonial invocation of charter while enforcing parliamentary statute; extract legitimacy from charter while subordinating it to legislature
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing parliamentary sovereignty as a logical necessity rather than recognizing it as a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.42).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Constraint Authority: Parliamentary Sovereignty Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/political_theory/legal_philosophy").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'd64e5e2f-5ee3-4816-acd4-06b11d7d76ab').
narrative_ontology:cs_kernel_codification('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', fixed_text).
narrative_ontology:cs_authority_grounding('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', extraction).
narrative_ontology:cs_interpretation_layer_present('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab').
narrative_ontology:cs_reading_relation('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', magna_carta_constraint_authority__living_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', foundational, parliament_supreme_lawmaker).
narrative_ontology:cs_axiom_status(parliament_supreme_lawmaker, holdable).
narrative_ontology:cs_axiom_grounding('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', parliament_supreme_lawmaker, conventional).
narrative_ontology:cs_axiom('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', foundational, charter_authority_contingent_on_parliamentary_choice).
narrative_ontology:cs_axiom_status(charter_authority_contingent_on_parliamentary_choice, holdable).
narrative_ontology:cs_axiom_grounding('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', charter_authority_contingent_on_parliamentary_choice, conventional).
narrative_ontology:cs_axiom('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', secondary, majoritarian_protection_adequate).
narrative_ontology:cs_axiom_status(majoritarian_protection_adequate, overridden).
narrative_ontology:cs_axiom_grounding('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', majoritarian_protection_adequate, empirically_contingent).
narrative_ontology:cs_reference_frame('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', parliamentary_legislative_supremacy).
narrative_ontology:cs_drift_state('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', contemporary_constitutional_politics, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d64e5e2f-5ee3-4816-acd4-06b11d7d76ab', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_faction_in_parliament).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL MINORITIES (SNARE) — Minorities trapped within the majoritarian legislative process have no exit and cannot constrain Parliament itself. The charter's restraints are absorbed and revisable by the very majority that may target them. Full extraction with no voice.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY OPPOSITION (TANGLED ROPE) — Opposition parties within Parliament benefit from the institution's existence and its formal procedures (coordination function) but face suppression of their revisionary power while majorities hold agenda control. Mixed coordination and extraction at biographical horizon.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENTARY MAJORITY COALITION (ROPE) — The current majority faction experiences Parliament as a coordination mechanism that enables their will. They benefit from the constraint's absorption into statute (charter provisions can be revised when inconvenient) and experience its operation as pure coordination rather than extraction.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENTS (TANGLED ROPE) — Organized movements seeking to entrench rights against parliamentary revision face genuine suppression (institutional inertia, difficulty of constitutional amendment, parliamentary obstructionism) but also benefit from the parliamentary system as the venue for their reform claims. Constrained but not trapped.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIARY AND LEGAL FORMALISM (PITON) — Courts invoke Magna Carta as ceremonial authority while enforcing parliamentary statute as supreme. The charter functions as theater — cited for legitimacy but subordinate to legislative will. Judicial role is largely performative in enforcing constitutional restraint.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational perspective, parliamentary sovereignty (the principle that no authority can bind the legislature) appears as a logical necessity: any entrenching of a charter would either be revocable (making the entrenchment illusory) or would create a higher authority than the sovereign legislature (a logical contradiction). The constraint may appear as a mathematical necessity. However, this perspective is a false summit candidate — the structural data shows this is a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_constraint_authority__parliamentary_sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate and rising. At the charter's origin (t=0, ε=0.28), it functioned as a genuine restraint on feudal monarchy — real constraints on arbitrary taxation, arbitrary imprisonment, and arbitrary judicial action. Over 400 years (t=400, ε=0.42), extractiveness has increased as parliamentary majorities have systematically revised charter provisions to serve their interests: the 1601 monopolies settlement repeated charter principles while Parliament reserved revision rights; the 1689 Bill of Rights reformulated charter principles as parliamentary statute subject to repeal; modern parliamentary practice explicitly revises charter-derived protections (habeas corpus suspension during emergencies, parliamentary override of judicial decisions). The trajectory reflects not new extraction but increasing visibility of extraction — the charter's restraints are revealed as contingent on majoritarian acquiescence. Suppression (0.48): Moderate. The charter's restraints suppress some parliamentary powers (cannot arbitrarily tax, cannot imprison without process) but not others (can legislate away any restraint through statute). Minorities who depend on charter protection face suppression because they have no exit and cannot constrain the legislature through charter authority — their only recourse is political organization within parliament. Theater ratio (0.58): Moderate-high and rising. The charter is increasingly invoked for ceremonial legitimation rather than substantive restraint. Judicial citations to Magna Carta occur in cases where statute or precedent, not the charter itself, supplies the decision rule. Parliamentary invocation of the charter appeals to tradition and legitimacy while exercising the sovereign right to revise it. The rise in theater reflects decreasing functional restraint as parliamentary mechanics (amendment procedures, legislation) become the primary constraint mechanism, not charter principles.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary sovereignty reading produces a perspectival gap between the beneficiary (parliamentary majority) and victim (constitutional minorities) that increases over time. At t=0, the gap is small because the charter genuinely constrains all actors; by t=400, the gap is maximum because parliamentary majorities can revise protections while minorities cannot exit. The majority experiences parliament as rope (coordination mechanism), while minorities experience it as snare (extraction without voice). The analytical observer risks seeing the entire structure as a mountain (logical necessity of sovereignty) when it is actually a contingent institutional arrangement that could theoretically be replaced by entrenched constitutionalism (as in other democracies). The piton perspective reveals the increasing theater — the charter is maintained ceremonially while its functional constraint decays.
 *
 * DIRECTIONALITY LOGIC:
 *   The parliamentary sovereignty reading's directionality structure flows from the core axiom: Parliament is the supreme locus of authority and cannot be bound by instruments it can revise. This produces high directionality variance across perspectives. Parliamentary majorities (beneficiaries with arbitrage exit) derive d ≈ 0.15 (low extraction because they can revise any constraint). Constitutional minorities (victims with trapped exit) derive d ≈ 0.92 (maximum extraction because they cannot exit and cannot constrain parliament). Opposition factions (moderate power, constrained exit) derive d ≈ 0.65 (moderate extraction because they can organize within parliament but face majority suppression). The range across perspectives is wider than in the other readings because this reading's core claim — parliamentary supremacy — admits no super-parliamentary constraint. The living constitutionalism reading would compress this range by positing a constitutional principle that binds parliament itself. The feudal obsolescence reading would collapse it entirely (no binding constraint for any actor).
 *
 * MANDATROPHY ANALYSIS:
 *   The parliamentary sovereignty reading resolves the mandatrophy by showing that the constraint is genuinely tangled (coordination + extraction hybrid) rather than purely extractive (snare) or purely coordinative (rope). The coordination function is real — parliament enables collective decision-making better than arbitrary monarchy. The extraction is also real — the majority can revise protections without consent from the protected. The resolution hinges on recognizing that the charter's restraint is contingent on institutional choice, not binding authority. This distinguishes the reading from the living constitutionalism reading (which treats the constraint as binding and therefore rope-like) and from the feudal obsolescence reading (which denies the constraint exists and therefore classifies as snare from all perspectives). The mandatrophy is real — the charter appears as both restraint (rope) and extraction mechanism (snare) — but is resolvable through the institutional frame: it is restraint when parliament chooses to respect it, extraction when parliament chooses to revise it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_supremacy_vs_constitutional_entrenchment,
    'Is parliamentary legislative supremacy a natural law of sovereignty, or a contingent institutional choice that could theoretically be replaced by entrenched constitutional authority?',
    'Comparative constitutional analysis: examination of jurisdictions with entrenched constitutions that bind legislatures (Canada, Australia, South Africa post-1994, Germany); identification of whether these systems exhibit logical contradictions or merely different authority structures.',
    'If natural law: mountain classification is correct; constitutional entrenchment is impossible. If contingent: mountain is a false summit; parliamentary sovereignty is a choice, not a necessity, and the constraint is tangled_rope with contingent extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_supremacy_vs_constitutional_entrenchment, conceptual, 'Whether parliamentary sovereignty is a logical necessity or institutional choice').

omega_variable(
    charter_absorption_vs_incorporation,
    'When Magna Carta''s provisions are absorbed into statute law, are they incorporated as binding precedent (living constitutionalism reading) or merely adopted as mutable statute (parliamentary sovereignty reading)?',
    'Doctrinal analysis of charter precedent in English case law; examination of judicial reasoning in cases citing Magna Carta to determine whether the charter is treated as binding constitutional principle or as statute subject to express repeal.',
    'If binding precedent: living constitutionalism reading gains structural support; constraint is rope or mountain. If mutable statute: parliamentary sovereignty reading is confirmed; constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_absorption_vs_incorporation, empirical, 'Whether absorbed charter provisions retain binding precedent status or become mutable statute').

omega_variable(
    extinction_vs_dormancy,
    'Has Magna Carta''s original constraint authority been historically extinguished (feudal_obsolescence reading) or merely dormant, revivable through legal or political reinterpretation?',
    'Historical analysis of charter invocation across centuries (1215–present); examination of political movements (English Civil War invokers, 18th-century Whig interpretations, modern due-process advocates) to determine whether charter serves as active constraint or historical symbol.',
    'If extinguished: feudal reading dominates; constraint is snare (extraction without restraint). If dormant/revivable: charter retains potential constraint force; constraint becomes rope or tangled_rope depending on entrenchment mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinction_vs_dormancy, empirical, 'Whether charter authority has been historically extinguished or remains revivable').

omega_variable(
    parliamentary_representation_sufficiency,
    'Does parliamentary representation (in principle) adequately protect all interests against the majoritarian extraction mechanism, or does the constraint require extra-parliamentary safeguards?',
    'Empirical analysis of minority protection outcomes in parliamentary systems; correlation between legislative majority size and minority-targeted policy outcomes; examination of whether parliamentary procedure alone prevents oppressive legislation.',
    'If representation sufficient: victim identification is incorrect; no constitutional minorities trapped by majoritarian extraction. If insufficient: tangled_rope and snare classifications confirmed; constraint requires supplementary constitutional protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_representation_sufficiency, empirical, 'Whether parliamentary representation adequately protects minorities against majoritarian extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mgcp_theater_t0_feudal_context, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mgcp_theater_t200_victorian_legalism, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 200, 0.52).
narrative_ontology:measurement(mgcp_theater_t400_ceremonial_invocation, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 400, 0.58).

% Extraction over time
narrative_ontology:measurement(mgcp_extractiveness_t0_1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mgcp_extractiveness_t200_reform_acts, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(mgcp_extractiveness_t400_contemporary, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 400, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_democracy_majoritarianism_constraint).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_entrenchment_possibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel magna_carta_constraint_authority. The sibling readings (feudal_obsolescence_reading, living_constitutionalism_reading) are separate constraint stories with their own ε values and structural properties. All three readings share the same kernel (Magna Carta's authority) but instantiate different constraint types based on different institutional framings. The parliamentary sovereignty reading's moderate extractiveness (0.42) reflects its position between the feudal reading's high extractiveness (0.58, snare) and the living constitutionalism reading's low extractiveness (0.28, rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
