% ============================================================================
% CONSTRAINT STORY: indian_constitution_1950__amendment_and_basic_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_constitution_1950__amendment_and_basic_structure, []).

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
 *   constraint_id: indian_constitution_1950__amendment_and_basic_structure
 *   human_readable: Indian Constitution: Amendment Power vs. Basic Structure Doctrine
 *   domain: constitutional_law/political
 *
 * SUMMARY:
 *   The Indian Constitution of 1950 embodies a foundational ambiguity: it
 *   grants Parliament an unusually flexible amendment power (Article 368)
 *   while the Constituent Assembly's intent was to enable reform and
 *   adaptation to changing times. Yet within 23 years, the Supreme Court
 *   (Kesavananda Bharati v. State of Kerala, 1973) imposed a judge-made
 *   doctrine of basic structure — certain principles of the Constitution
 *   (federalism, democracy, fundamental rights, secularism, the rule of law)
 *   cannot be amended, even by the supermajority procedures of Article 368.
 *   This reading instantiates the deepest constitutional fight in independent
 *   India: the collision between flexible amendment power and immutable
 *   constitutional core. The constraint exhibits Tangled Rope structure:
 *   Parliament enjoys genuine coordination benefits from constitutional
 *   stability (the basic structure prevents chaotic amendment wars and
 *   protects minority rights across majority changes) while experiencing
 *   extraction (its sovereign text-revision power is constrained by
 *   judge-made doctrine). The judiciary experiences pure coordination (their
 *   authority is legitimated by protecting constitutional identity).
 *   Parliamentary majorities experience near-snare conditions: formal power
 *   exists but is functionally suppressed. From the civilizational analytical
 *   perspective, the doctrine risks appearing as natural law (immutable
 *   constitutional principles) but the structural data reveals it as a
 *   contingent institutional choice that beneficiaries the constitutional
 *   identity across majorities while victimizing parliamentary sovereignty.
 *
 * KEY AGENTS:
 *   - Parliamentary Majorities: Primary victim (powerless/trapped) — formal amendment power constrained by judge-made doctrine; cannot revise basic structure regardless of electoral mandate
 *   - Constitutional Identity (federalism, democracy, fundamental rights, secularism): Primary beneficiary (powerful/mobile) — protected across successive majorities; cannot be eroded by amendment even if new coalition prefers revision
 *   - The Judiciary (Supreme Court): Secondary beneficiary (institutional/arbitrage) — gains authority to veto amendments; legitimacy derived from protecting constitutional structure
 *   - Parliament as Institution: Mixed actor (institutional/constrained) — benefits from amendment flexibility for non-basic-structure revisions; constrained by doctrine on core principles
 *   - Reform Movements: Secondary victim (powerful/mobile) — movements seeking structural change (federal restructuring, presidential abolition, radical redistribution) experience doctrine as suppression of parliamentary pathways
 *   - Democratic Majorities Across Time: Beneficiary-victim (powerful/arbitrage) — protected from future majorities' ability to dismantle fundamental principles; constrained from their own preferred radical revisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_constitution_1950__amendment_and_basic_structure, 0.38).
domain_priors:suppression_score(indian_constitution_1950__amendment_and_basic_structure, 0.68).
domain_priors:theater_ratio(indian_constitution_1950__amendment_and_basic_structure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_constitution_1950__amendment_and_basic_structure, extractiveness, 0.38).
narrative_ontology:constraint_metric(indian_constitution_1950__amendment_and_basic_structure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indian_constitution_1950__amendment_and_basic_structure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_constitution_1950__amendment_and_basic_structure, tangled_rope).
narrative_ontology:human_readable(indian_constitution_1950__amendment_and_basic_structure, "Indian Constitution: Amendment Power vs. Basic Structure Doctrine").
narrative_ontology:topic_domain(indian_constitution_1950__amendment_and_basic_structure, "constitutional_law/political").

domain_priors:requires_active_enforcement(indian_constitution_1950__amendment_and_basic_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(indian_constitution_1950__amendment_and_basic_structure, '89432629-7d27-4de2-8e71-6e04076fe9a6').
narrative_ontology:cs_kernel_codification('89432629-7d27-4de2-8e71-6e04076fe9a6', fixed_text).
narrative_ontology:cs_authority_grounding('89432629-7d27-4de2-8e71-6e04076fe9a6', extraction).
narrative_ontology:cs_interpretation_layer_present('89432629-7d27-4de2-8e71-6e04076fe9a6').
narrative_ontology:cs_reading_relation('89432629-7d27-4de2-8e71-6e04076fe9a6', indian_constitution_1950__directive_principles_part_iv, influences).
narrative_ontology:cs_reading_relation('89432629-7d27-4de2-8e71-6e04076fe9a6', indian_constitution_1950__federal_asymmetry, influences).
narrative_ontology:cs_reading_relation('89432629-7d27-4de2-8e71-6e04076fe9a6', indian_constitution_1950__fundamental_rights_part_iii, coexists_with).
narrative_ontology:cs_reading_relation('89432629-7d27-4de2-8e71-6e04076fe9a6', indian_constitution_1950__social_revolution_provisions, influences).
narrative_ontology:cs_axiom('89432629-7d27-4de2-8e71-6e04076fe9a6', foundational, some_constitutional_features_logically_prior_to_amendment).
narrative_ontology:cs_axiom_status(some_constitutional_features_logically_prior_to_amendment, holdable).
narrative_ontology:cs_axiom_grounding('89432629-7d27-4de2-8e71-6e04076fe9a6', some_constitutional_features_logically_prior_to_amendment, deontological).
narrative_ontology:cs_axiom('89432629-7d27-4de2-8e71-6e04076fe9a6', foundational, judicial_authority_to_enforce_constitutional_limits_on_amendment).
narrative_ontology:cs_axiom_status(judicial_authority_to_enforce_constitutional_limits_on_amendment, holdable).
narrative_ontology:cs_axiom_grounding('89432629-7d27-4de2-8e71-6e04076fe9a6', judicial_authority_to_enforce_constitutional_limits_on_amendment, deontological).
narrative_ontology:cs_reference_frame('89432629-7d27-4de2-8e71-6e04076fe9a6', flexible_amendment_with_core_protection).
narrative_ontology:cs_drift_state('89432629-7d27-4de2-8e71-6e04076fe9a6', post_kesavananda_1973_to_contemporary, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('89432629-7d27-4de2-8e71-6e04076fe9a6', '').
narrative_ontology:cs_kernel_id(indian_constitution_1950__amendment_and_basic_structure, indian_constitution_1950).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_constitution_1950__amendment_and_basic_structure, constitutional_identity).
narrative_ontology:constraint_beneficiary(indian_constitution_1950__amendment_and_basic_structure, judicial_authority).
narrative_ontology:constraint_victim(indian_constitution_1950__amendment_and_basic_structure, parliamentary_sovereignty).
narrative_ontology:constraint_victim(indian_constitution_1950__amendment_and_basic_structure, amendment_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENTARY MAJORITY (SNARE) — An elected supermajority cannot amend the basic structure, no matter how large the mandate or how democratically compelling the revision. Suppression is maximal: the formal power to amend exists but is functionally constrained by a judge-made doctrine with no textual basis. The majority is trapped — exit requires revolution, not legislation.
constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENT AS INSTITUTION (TANGLED ROPE) — Parliament gains coordination benefits from constitutional stability (the basic structure prevents chaotic amendment wars) while experiencing extraction (its sovereign text-revision power is constrained). Parliament can amend most of the Constitution freely — genuine coordination function. But the basic structure doctrine foreclosed unlimited revision, creating asymmetric extraction that benefits the constitutional text's identity across majorities.
constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (ROPE) — The Supreme Court benefits from arbitrage: it gains authority to veto constitutional amendments without themselves being amended. The basic structure doctrine is pure institutional coordination from the judiciary's perspective: it protects constitutional structure while enabling the Court to adjudicate disputes. The Court experiences this as genuine coordination with no extraction cost — their power is legitimated by protecting the Constitution itself.
constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL IDENTITY (ROPE) — The basic structure doctrine is pure coordination: it protects the Constitution's core identity across successive majorities. Without basic structure protection, constitutional identity dissolves into the preferences of the ruling coalition. This perspective sees the doctrine as coordination (preventing constitutional meltdown) with no extraction — the beneficiary is the document itself and the civic commitment it represents.
constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM MOVEMENTS (TANGLED ROPE) — Movements seeking fundamental constitutional revision (e.g., replacement of the federal structure, abolition of the presidency, radical redistribution) experience the basic structure as both coordination and extraction. Coordination: the constraint prevents demagogic amendment wars that would destabilize rights-bearing structures. Extraction: the constraint forecloses their preferred transformations. Mobile exit options (extrajudicial constitutional conventions, constituent assembly rhetoric) exist but are costly — the doctrine suppresses parliamentary pathways to total revision.
constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the basic structure doctrine instantiates an immutable constitutional core: certain principles (federalism, democracy, fundamental rights, secularism) are inherent to the constitutional identity and logically prior to amendment procedures. Amendment power itself presupposes a constitution to be amended — therefore some constitutional features are procedurally prior and cannot be touched. This perspective sees the doctrine as natural law of constitutional structures. However, the structural data contradicts the mountain classification — the engine will evaluate this as a false summit, revealing that 'immutable constitutional core' naturalizes what is a contingent institutional choice (the judges' decision in Kesavananda).
constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_constitution_1950__amendment_and_basic_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_constitution_1950__amendment_and_basic_structure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(indian_constitution_1950__amendment_and_basic_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint exhibits moderate extractiveness that increases from pre-doctrine (0.22) to post-Kesavananda (0.38-0.42). Pre-1973, Parliament had genuine flexible amendment power with minimal functional constraint. Kesavananda imposed an invisible extraction: formal power remains but functional suppression of unlimited revision. The extractiveness is not as high as snare-level (0.46+) because (a) most of the Constitution remains amendable, (b) the basic structure itself has been interpreted evolutively rather than rigidly, and (c) amendment within the structure is genuinely easier than total constitution-writing. Suppression (0.68): High suppression of unlimited amendment power. Parliament cannot revise basic structure through any procedural route — not by 2/3 supermajority, not by constituent assembly, not by special amendment. The suppression is structural (embedded in doctrine enforced by the judiciary) and durable (reinforced by subsequent Supreme Court decisions). Suppression increased sharply at Kesavananda (from 0.25 to 0.68) as the doctrine was announced and consolidated. Theater ratio (0.55): Moderate theater. The doctrine operates through legal reasoning (textual interpretation of Article 368) that carries legitimacy but also contains performative elements. The Supreme Court's assertion of basic structure authority required creative constitutional interpretation not explicitly warranted by the text — the theater is the construction of this legitimacy. Post-Kesavananda, theater stabilized (0.58) as the doctrine became institutionalized and the Court's authority was accepted (even if contested). The theater is lower than snare-level (0.70+) because the doctrine rests on genuine constitutional reasoning, not pure performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Parliamentary majorities see near-snare conditions: formal power constrained by doctrine without textual warrant, unable to exit except through constitutional breakdown. The judiciary sees pure rope: legitimate coordination (protecting constitutional identity) with no extraction. Parliament as an institution sees tangled rope: coordination benefits from stability offsetting the extraction of revision-power constraints. Constitutional identity sees pure rope: protected across majorities through judicial enforcement. Reform movements see snare: parliamentary pathways to structural change foreclosed by doctrine. The analytical observer risks seeing mountain (immutable constitutional principles) but the structural data reveals false summit: the 'immutability' depends on judicial enforcement of a doctrine that could be reversed (by amendment of the amendment power itself, if not for the basic structure doctrine — circular enforcement). The perspectival gap reveals the ambiguity at the Constitution's core: is basic structure a necessary protection of constitutional identity, or is it an extraction mechanism that entrenches judicial power and parliament's subordination to judge-made limits?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position and exit options. Parliamentary majorities as powerless/trapped agents experience maximum directionality (d ≈ 0.95), producing high experienced extractiveness through the sigmoid f(d) ≈ 1.42. The judiciary as institutional/arbitrage agents experiences low directionality (d ≈ 0.05), producing negative/neutral f(d) ≈ -0.12, reflecting that the doctrine benefits them without extraction cost. Constitutional identity as powerful/mobile beneficiary experiences d ≈ 0.15 (beneficiary + mobile), f(d) ≈ -0.01, reflecting protection without extraction. Reform movements as powerful/mobile victims experience d ≈ 0.70, f(d) ≈ 1.10, reflecting that they bear costs (suppression of their preferred amendments) but have exit options (revolutionary constituent assembly, extrajudicial reform movements). These directionality values are structural — they derive from the agent's actual position relative to extraction flow, not from conventional power hierarchies.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination function from asymmetric extraction. The basic structure doctrine serves genuine coordination: it prevents constitutional meltdown by protecting core principles across majority changes. Minorities are protected from having fundamental rights amended away. The Constitution maintains identity despite radical shifts in electoral coalitions. These are real coordination benefits. Simultaneously, the doctrine extracts from parliamentary sovereignty: unlimited amendment power is constrained by judge-made limits. Reform movements' pathways to constitutional restructuring are suppressed. The supermajority procedures of Article 368 are circumvented by judicial veto. The mandatrophy is resolved by acknowledging both: this is tangled rope, not pure rope (which would ignore extraction) and not snare (which would ignore coordination). The extraction is real and asymmetric, but it is paired with genuine coordination function that justifies some suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_source_of_basic_structure,
    'Is the basic structure doctrine a legitimate constitutional interpretation of Article 368 (amendment power), or is it a judge-made creation without textual warrant?',
    'Historical textual analysis of Article 368''s language and drafting intent; comparison with other constitutions'' explicit unamendable provisions; examination of whether the Constituent Assembly intended implicit limits',
    'If textual interpretation: doctrine is coordination (protecting constitutional design). If judge-made creation: doctrine is extraction (judicial power-grab constraining parliamentary sovereignty). Classification shifts from Tangled Rope to Snare from Parliament''s perspective if judge-made.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_source_of_basic_structure, conceptual, 'Whether basic structure doctrine derives from constitutional text or judicial creation').

omega_variable(
    counterfactual_amendment_danger,
    'Without the basic structure doctrine, would unlimited amendment power actually lead to constitutional meltdown (validating coordination function), or is this catastrophe-prevention narrative a cover story for judicial power consolidation?',
    'Comparative constitutional analysis: how many democracies have constitutional collapse via amendment? Correlation between explicit unamendable clauses and constitutional stability. Post-hoc analysis: did Kesavananda prevent destructive amendments, or did it prevent amendments that would have been survived by constitutional structures?',
    'If amendment danger real: doctrine is genuine coordination (snare classification reversed, becomes rope). If catastrophe narrative exaggerated: doctrine is pure extraction masked as protection (snare from Parliament confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_amendment_danger, empirical, 'Whether constitutional collapse risk justifies basic structure constraints').

omega_variable(
    alternative_structural_protection,
    'Could constitutional identity be protected without a judge-made unamendable core — e.g., through super-super-majoritarian amendment procedures, or through political norms that delegitimize certain revisions?',
    'Comparative study of democracies using qualified majority (2/3, 3/5) amendment procedures vs explicit unamendable clauses; analysis of whether political norms alone (constitutional convention) can protect core structures without judicial veto',
    'If alternatives sufficient: basic structure doctrine is unnecessary extraction (Snare confirmed). If alternatives insufficient: doctrine is necessary coordination (Tangled Rope with genuine coordination function confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_structural_protection, empirical, 'Whether alternatives to judge-made basic structure could protect constitutional identity').

omega_variable(
    reading_contest_framework_ambiguity,
    'Does this reading (amendment_and_basic_structure) stand as the primary constitutional trap, or do the sibling readings (directive_principles, federal_asymmetry, fundamental_rights, social_revolution) represent equally deep fights that the basic structure doctrine was engineered to settle?',
    'Historical analysis of post-Kesavananda constitutional litigation; measurement of which doctrinal fights have produced the most substantive constitutional change and political consequence; examination of whether basic structure doctrine was protection for the sibling readings or an independent fight',
    'If basic structure is primary: this reading captures the Constitution''s deepest logic. If sibling readings are equally deep or prior: the basic structure doctrine is secondary institutional machinery protecting other commitments, and this reading''s centrality is inflated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_framework_ambiguity, conceptual, 'Whether amendment/basic-structure fight is primary or secondary to other constitutional struggles').

omega_variable(
    judicial_identity_lock,
    'Is the judiciary identity-locked into the basic structure doctrine (internalized commitment that the Constitution''s survival depends on judicial veto), or is the doctrine a rational strategic choice that could be abandoned if political pressure mounted?',
    'Analysis of Supreme Court reversals or narrowings of basic structure doctrine; examination of whether courts have historically backed down under constitutional amendment pressure; study of whether judicial reasoning about basic structure shows defensive institutional logic (protecting the judiciary''s own role) vs. neutral constitutional principle',
    'If identity-locked: judicial support for doctrine is robust but potentially incoherent (the Court cannot see alternatives). If strategic: doctrine is negotiable and extractiveness may be politically constrained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_identity_lock, conceptual, 'Whether judiciary is strategically committed or identity-locked to basic structure doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_constitution_1950__amendment_and_basic_structure, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ic_abs_theater_t0_pre_kesavananda, indian_constitution_1950__amendment_and_basic_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ic_abs_theater_t1_kesavananda_1973, indian_constitution_1950__amendment_and_basic_structure, theater_ratio, 1, 0.55).
narrative_ontology:measurement(ic_abs_theater_t2_post_1973_doctrine_stabilization, indian_constitution_1950__amendment_and_basic_structure, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(ic_abs_extractiveness_t0_pre_kesavananda, indian_constitution_1950__amendment_and_basic_structure, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ic_abs_extractiveness_t1_kesavananda_1973, indian_constitution_1950__amendment_and_basic_structure, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(ic_abs_extractiveness_t2_post_1973_consolidation, indian_constitution_1950__amendment_and_basic_structure, base_extractiveness, 2, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ic_abs_suppression_t0_pre_kesavananda, indian_constitution_1950__amendment_and_basic_structure, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ic_abs_suppression_t1_kesavananda_1973, indian_constitution_1950__amendment_and_basic_structure, suppression_requirement, 1, 0.68).
narrative_ontology:measurement(ic_abs_suppression_t2_post_1973_entrenchment, indian_constitution_1950__amendment_and_basic_structure, suppression_requirement, 2, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_constitution_1950__amendment_and_basic_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(indian_constitution_1950__amendment_and_basic_structure, indian_constitution_1950__fundamental_rights_part_iii).
narrative_ontology:affects_constraint(indian_constitution_1950__amendment_and_basic_structure, indian_constitution_1950__federal_asymmetry).
narrative_ontology:affects_constraint(indian_constitution_1950__amendment_and_basic_structure, indian_constitution_1950__directive_principles_part_iv).
narrative_ontology:affects_constraint(indian_constitution_1950__amendment_and_basic_structure, indian_constitution_1950__social_revolution_provisions).

% DUAL FORMULATION NOTE:
% The amendment/basic-structure reading constrains all sibling readings: the basic structure doctrine protects fundamental rights (Part III), federal structure, social revolution provisions, and the directive principles framework itself by foreclosing amendments that would dismantle them. Each sibling reading is downstream of this reading — they become entrenched constitutional commitments precisely because the basic structure doctrine protects them from unlimited amendment. The network represents this upstream-downstream relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_constitution_1950__amendment_and_basic_structure, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
