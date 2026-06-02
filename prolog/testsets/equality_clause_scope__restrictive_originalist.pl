% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause Scope: Restrictive Originalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * KEY AGENTS:
 *   - Propertied White Males (Franchise Beneficiaries): institutional/arbitrage — directly benefit from narrow scope; experience constraint as coordination of stable governance and property protection
 *   - Excluded Populations (Women, Enslaved Persons, Non-Landowners): powerless/trapped — bear full extraction cost; structurally barred from invoking the equality clause as interpreted
 *   - State Legislatures (Franchise Gatekeepers): institutional/arbitrage — retain control over electoral qualifications; benefit from constraint on federal expansion of franchise
 *   - Expansion Claimants (Abolitionists, Suffragists, Civil Rights Activists): moderate/identity_locked — caught between benefiting from constitutional language (provides lever for claims) and being suppressed by originalist scope (claims defined as illegitimate departures)
 *   - Constitutional Amendment Coalition: organized/constrained — pathway exists but at high cost; must mobilize supermajority consensus to override the reading
 *   - Originalist Jurists (Interpreters of the Constraint): institutional/arbitrage — derive legitimacy and authority from maintaining the reading; beneficiaries of the interpretive methodology they champion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.58).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.72).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause Scope: Restrictive Originalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '50fa8b93-0d81-42b3-9734-020463d9f873').
narrative_ontology:cs_kernel_codification('50fa8b93-0d81-42b3-9734-020463d9f873', fixed_text).
narrative_ontology:cs_authority_grounding('50fa8b93-0d81-42b3-9734-020463d9f873', lineage).
narrative_ontology:cs_interpretation_layer_present('50fa8b93-0d81-42b3-9734-020463d9f873').
narrative_ontology:cs_reading_relation('50fa8b93-0d81-42b3-9734-020463d9f873', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_reading_relation('50fa8b93-0d81-42b3-9734-020463d9f873', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('50fa8b93-0d81-42b3-9734-020463d9f873', foundational, original_scope_permanently_fixed).
narrative_ontology:cs_axiom_status(original_scope_permanently_fixed, holdable).
narrative_ontology:cs_axiom_grounding('50fa8b93-0d81-42b3-9734-020463d9f873', original_scope_permanently_fixed, empirically_contingent).
narrative_ontology:cs_axiom('50fa8b93-0d81-42b3-9734-020463d9f873', foundational, textual_meaning_immutable_at_enactment).
narrative_ontology:cs_axiom_status(textual_meaning_immutable_at_enactment, holdable).
narrative_ontology:cs_axiom_grounding('50fa8b93-0d81-42b3-9734-020463d9f873', textual_meaning_immutable_at_enactment, empirically_contingent).
narrative_ontology:cs_reference_frame('50fa8b93-0d81-42b3-9734-020463d9f873', original_scope_1787_propertied_franchise).
narrative_ontology:cs_drift_state('50fa8b93-0d81-42b3-9734-020463d9f873', contemporary_post_amendment_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('50fa8b93-0d81-42b3-9734-020463d9f873', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, states_with_narrow_franchise).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, excluded_populations).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, expansion_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POPULATIONS (SNARE) — Completely trapped by the originalist reading. No structural path to franchise expansion without overthrowing the constraint itself. The constraint actively suppresses alternative legal readings and denies legitimacy to expansion claims. Maximum extractive load with zero exit options. The reading itself forecloses their claims by defining equality narrowly within the original scope.
constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXPANSION CLAIMANTS (TANGLED ROPE) — Identity-locked within a legal tradition that simultaneously constrains and enables them. They benefit from the constitutional framework itself (it provides the language of rights and equality to appeal to) while bearing the extraction cost of having their claims defined as illegitimate departures from the original scope. They are trapped by identity fusion with the very tradition they contest — professional legitimacy depends on operating within constitutional discourse that the restrictive originalist reading controls.
constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPERTIED FRANCHISE (ROPE) — Primary beneficiary with full arbitrage capacity. Experiences the constraint as pure coordination: the constitutional guarantee of equal political rights among the qualified electorate solves the collective action problem of securing property rights and stable governance without needing to extend franchise. Net beneficiary with maximum exit flexibility. The constraint protects their interests without requiring active suppression — they are the intended scope.
constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LEGISLATURES (ROPE) — Retain control over franchise requirements and property qualifications. The constraint coordinates their interests by guaranteeing equal treatment within their chosen electorate while preserving their sovereign right to define the electorate. No suppression required — they benefit from preserving franchise control. Straightforward coordination function with no extractive asymmetry.
constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AMENDMENT MECHANISM (SCAFFOLD) — Organized agents (abolitionists, suffragists, civil rights movements) see the restrictive originalist reading as a temporary constraint with a clear sunset: the amendment process (however difficult) provides a formal pathway to overturn or supersede the constraint. The 13th, 14th, 15th, 19th, and 26th Amendments collectively represent the sunset of successive exclusions. Theater ratio is low here — the amendment mechanism is genuinely functional, not performative. High enforcement cost but genuine exit pathway.
constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL STRUCTURE (MOUNTAIN) — From a civilizational/universal analytical perspective, the restriction to 'political actors' at a defined historical moment appears as a logical or textual constraint: the clause means what it meant in 1787, and no alternative interpretive framework can change its original scope without violating the basic principle that texts have determinate meaning at their point of enactment. However, this perspective carries a false summit risk: the 'immutable meaning' is actually a choice of interpretive methodology, not an inherent property of the text. The originalist framework naturalizes a contingent interpretive commitment.
constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equality_clause_scope__restrictive_originalist, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The restrictive originalist reading produces genuine asymmetric extraction from excluded populations — they are barred from invoking the constitutional guarantee while propertied males gain its protection. However, the extractiveness is not maximum (0.72+) because the constraint is not entirely coercive: the amendment mechanism provides a formal, if difficult, exit pathway. The measurement trajectory shows extractiveness declining over 200 years as amendments erode the scope: from 0.72 in 1787 (near-total exclusion) to 0.28 by 1987 (core exclusions eliminated, though ongoing struggle). This trajectory reflects that the constraint's extraction mechanism has been partially disabled by legal amendment. Suppression (0.72): High. The constraint actively suppresses alternative readings by defining them as illegitimate constitutional interpretation. The suppression has two mechanisms: (1) textual authority — the claim that 1787 meaning is fixed prevents alternative framings from being heard as constitutional interpretation; (2) institutional gatekeeping — originalist judges and scholars control what counts as valid legal argument, making expansion claims face an uphill legitimacy battle. The suppression requirement declines over time as non-originalist methodologies gain institutional presence (legal realism, living constitutionalism, progressive textualism). Theater ratio (0.48): Low-moderate. The restrictive originalist reading is substantive, not merely performative — it actually forecloses certain legal arguments and upholds certain outcomes. However, some theater creeps in as the reading's defenders face mounting pressure to explain persistent exclusions: claims that equality 'really does' protect excluded groups but requires amendment (rather than interpretation) add a thin performative layer. The theater ratio rises slightly over time (0.35 → 0.48) as originalist defenders must explain away their reading's increasing divergence from democratic outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The same constraint appears as pure coordination (Rope) to its beneficiaries, pure extraction (Snare) to the excluded, mixed tangled rope to expansion claimants trying to work within the tradition, and potentially immutable law (Mountain from the false summit perspective) to observers who naturalize 'original meaning' as a textual property rather than an interpretive choice. The amendment mechanism opens a scaffold perspective — organized coalitions see a real sunset pathway, even if historically high-cost. The originalist jurists themselves occupy an institutional perspective with high arbitrage value — they benefit from controlling what counts as legitimate constitutional interpretation. The analytical observer risks the false summit error by treating 'what the text originally meant' as an objective fact rather than an output of choosing originalism as an interpretive methodology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined for each perspective by the agent's structural power, exit options, and relationship to the constraint's benefits/costs. Propertied males (institutional/arbitrage/beneficiary) have d ≈ 0.05 (full beneficiary, can exit costlessly) → f(d) ≈ -0.12 → very low effective extraction experienced. Excluded populations (powerless/trapped/victim) have d ≈ 0.95 (full target, cannot exit) → f(d) ≈ 1.42 → maximum experienced extraction. Expansion claimants (moderate/identity_locked/victim) have d ≈ 0.89 (structural mobility masked by identity fusion) → f(d) ≈ 1.28 → very high experienced extraction. State legislatures (institutional/arbitrage/beneficiary) have d ≈ 0.08 (beneficiary, retain franchise control) → f(d) ≈ -0.09 → negative effective extraction (they benefit). The perspectival gap is wide: beneficiaries experience rope (coordination without extraction) while victims experience snare (pure extraction). Expansion claimants occupy the tangled_rope middle: they benefit from constitutional language (coordination function) but are suppressed by originalist scope (extraction function). The identity_locked exit option for expansion claimants is critical — they are structurally mobile (could adopt alternative legal frameworks) but cannot exercise that mobility because their identity as constitutional scholars/activists is fused with the tradition they contest.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: The mandatrophy is resolved by recognizing that the three readings (restrictive originalist, expansive universalist, progressive textualist) represent genuine alternative commitments, each producing a different constraint with different ε and beneficiary/victim structure. The restrictive originalist reading (this constraint) produces high extraction from excluded populations because its core axiom (original scope is permanently fixed) forecloses claims by those outside the 1787 franchise. The expansive universalist reading produces low extraction because its core axiom (equality is universal principle) mandates inclusion. The progressive textualist reading produces tangled rope because it coordinates on evolving meaning while permitting interpretive battles. The question is not 'which type is correct?' but 'which reading does the constitutional authority accept?' That is a political/institutional question, not a classification question. The mandatrophy resolves by decomposing the single contested concept (equality) into three structurally distinct constraints corresponding to three readings. Each has its own extractiveness, its own beneficiary/victim structure, and its own classification. All three are live positions, and the gap between them IS the constitutional conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_framers_disagreement,
    'Did the framers intend the equality clause to have a narrow scope permanently fixed at 1787 (original public meaning), or did they intend it as a principle capable of application to new classes of persons?',
    'Historical evidence: framers'' private correspondence, convention debates, and statements about whether equality was a bounded principle or an open principle. Textual analysis of competing historical records.',
    'If narrow/fixed: restrictive originalist reading is structurally grounded. If open/principle: the constraint should be reclassified as rope (coordination on the principle, not extraction from exclusion). This is the central reading-foreclusion question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_intent_vs_framers_disagreement, empirical, 'Whether framers intended equality scope as fixed or open principle').

omega_variable(
    interpretive_methodology_vs_textual_fact,
    'Is the narrow scope of the equality clause an inherent property of the constitutional text, or a choice of interpretive methodology (originalism)?',
    'Philosophy of language analysis: compare originalist results with living constitutionalist, progressive textualist, and natural law readings of the same clause. Track which methodological assumptions produce which scope conclusions.',
    'If inherent textual property: mountain classification correct. If methodological choice: the apparent mountain is a false summit — the constraint naturalizes a contingent interpretive commitment. Reclassifies to tangled rope at the institutional/analytical level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_methodology_vs_textual_fact, conceptual, 'Whether scope is textual property or interpretive choice').

omega_variable(
    amendment_mechanism_accessibility,
    'Does the constitutional amendment process function as a genuine exit pathway for excluded populations, or is it so high-cost that it functions as a normalization of permanent exclusion?',
    'Historical data on amendment success rates for civil rights expansions. Comparison of amendment cost (political mobilization, consensus requirements) vs. constraint severity (exclusion magnitude). Timeline analysis: how long did extensions take after organized demand began?',
    'If genuine exit pathway: scaffold perspective is correct — the constraint has real sunset logic. If prohibitively high cost: the amendment mechanism is performative (theater), and the constraint functions as permanent snare for excluded populations. This shifts the Boltzmann classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_mechanism_accessibility, empirical, 'Whether amendment process provides genuine exit or performative pseudopath').

omega_variable(
    identity_fusion_of_expansion_claimants,
    'Are expansion claimants (rights activists, progressive jurists) trapped by material barriers to legal success, or by identity fusion with the constitutional tradition itself (identity_locked)?',
    'Narrative analysis of rights movement discourse. Do advocates see themselves as working within or against the Constitution? Can they articulate an alternative legal framework that is not constitutional, and if so, why do they not adopt it? Comparison of exit costs: legal defeat vs. identity dissolution.',
    'If material barriers: classify as constrained. If identity-fused: classify as identity_locked. The distinction determines whether the constraint is a structural barrier or a cognitive capture mechanism. Changes the therapeutic logic for exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_of_expansion_claimants, conceptual, 'Whether expansion claimants are materially constrained or identity-locked to constitutional framework').

omega_variable(
    sibling_reading_empirical_underdetermination,
    'Can historical evidence determine which reading (restrictive originalist vs. expansive universalist vs. progressive textualist) correctly interprets the framers'' intent?',
    'Meta-historical analysis: comprehensive review of evidence weighted by scholars across all three readings. Identification of interpretive underdetermination: evidence consistent with multiple readings. Mapping of where evidence gaps exist.',
    'If determinate evidence exists favoring restrictive originalist: the reading is empirically grounded, not merely axiomatic. If underdetermined: the three readings coexist as live scholarly positions, confirming coexists_with relation to siblings. This affects the authority grounding of the CS structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_underdetermination, empirical, 'Whether historical evidence underdetermines reading choice or supports restrictive originalist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqcs_theater_1787, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eqcs_theater_1837, equality_clause_scope__restrictive_originalist, theater_ratio, 50, 0.38).
narrative_ontology:measurement(eqcs_theater_1887, equality_clause_scope__restrictive_originalist, theater_ratio, 100, 0.42).
narrative_ontology:measurement(eqcs_theater_1937, equality_clause_scope__restrictive_originalist, theater_ratio, 150, 0.46).
narrative_ontology:measurement(eqcs_theater_1987, equality_clause_scope__restrictive_originalist, theater_ratio, 200, 0.48).

% Extraction over time
narrative_ontology:measurement(eqcs_extractiveness_1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(eqcs_extractiveness_1837, equality_clause_scope__restrictive_originalist, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(eqcs_extractiveness_1887, equality_clause_scope__restrictive_originalist, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(eqcs_extractiveness_1937, equality_clause_scope__restrictive_originalist, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(eqcs_extractiveness_1987, equality_clause_scope__restrictive_originalist, base_extractiveness, 200, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(eqcs_suppression_1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(eqcs_suppression_1837, equality_clause_scope__restrictive_originalist, suppression_requirement, 50, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, fourteenth_amendment_incorporation).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, voting_rights_act_scope).

% DUAL FORMULATION NOTE:
% The equality_clause_scope kernel generates three constraint stories corresponding to three readings. The restrictive originalist reading (this file) produces high extraction from excluded populations and tangled rope for expansion claimants. The expansive universalist reading produces low extraction and rope for all perspectives because it mandates inclusion. The progressive textualist reading produces intermediate extraction and tangled rope for institutional actors battling over meaning. All three are live constitutional positions held by different judicial/scholarly coalitions. The network links reflect institutional dependency: all three readings reference the same constitutional text and operate within the same amendment mechanism. However, they produce radically different classification outcomes from the same base constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
