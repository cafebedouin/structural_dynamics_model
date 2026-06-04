% ============================================================================
% CONSTRAINT STORY: indian_constitution_1950__directive_principles_part_iv
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_constitution_1950__directive_principles_part_iv, []).

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
 *   constraint_id: indian_constitution_1950__directive_principles_part_iv
 *   human_readable: Indian Constitution Part IV: Directive Principles of Social Policy (Non-Justiciable Conscience)
 *   domain: constitutional_law/political_economy
 *
 * SUMMARY:
 *   The Indian Constitution's Part IV (Directive Principles of State Policy)
 *   is a constitutional conscience without a lawsuit: it mandates that the
 *   state 'ensure that the operation of the economic system does not result
 *   in the concentration of wealth and means of production to the common
 *   detriment' (Article 39(c)), that the state 'take steps to ensure
 *   participation of workers in management of undertakings in the private
 *   sector engaged in production or distribution' (Article 43a), that 'there
 *   is an adequate means of livelihood for all' (Article 39(a)), and that
 *   'the ownership and control of the material resources of the community are
 *   so distributed as best to subserve the common good' (Article 39(b)) — but
 *   declares these principles 'non-justiciable' (Article 37). A citizen
 *   cannot sue the state for violation of Part IV. This constraint
 *   instantiates one reading of the contested kernel 'the Indian
 *   Constitution': the reading that treats Part IV as the constitution's
 *   deepest commitment to social transformation, even as it renders that
 *   commitment structurally unenforceable. The extractiveness trajectory
 *   shows accumulation: the gap between constitutional promise and material
 *   reality has widened from 1950 (when implementation seemed imminent)
 *   through 2010 (when the gap was undeniable despite judicial adaptation) to
 *   2026 (when Part IV exists as aspirational text performing legitimacy for
 *   governments that defer its implementation indefinitely). The sibling
 *   readings — fundamental rights (Part III), amendments, federal structure,
 *   social revolution provisions — each contest what the constitution's
 *   deepest commitment actually is and what the non-justiciability of Part IV
 *   really means for the constitutional order.
 *
 * KEY AGENTS:
 *   - The Dispossessed (landless workers, marginalized communities): Primary victims (powerless/trapped) — Part IV names their grievance but forecloses legal remedy. Cannot sue to enforce land redistribution, wage floors, or economic participation.
 *   - Welfare State Blueprint (the institutional aspiration): Primary beneficiary (institutional/arbitrage) — Part IV legitimizes social legislation while preserving executive discretion to implement minimally or defer indefinitely.
 *   - Reform-minded Legislators: Secondary agents (moderate/constrained) — can use Part IV to justify social legislation but face electoral and pressure-group constraints on implementation.
 *   - Ruling Coalition: Institutional beneficiary (institutional/arbitrage) — benefits from Part IV's non-justiciability: can signal commitment to social transformation while avoiding judicial enforcement of specific obligations.
 *   - Social Rights Litigation Movement: Organized agents (organized/constrained) — building pathways to transform Part IV into justiciable rights through Part III interpretation; acting as a sunset mechanism.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the non-justiciability as an immutable feature of constitutional design rather than a contingent choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_constitution_1950__directive_principles_part_iv, 0.58).
domain_priors:suppression_score(indian_constitution_1950__directive_principles_part_iv, 0.68).
domain_priors:theater_ratio(indian_constitution_1950__directive_principles_part_iv, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_constitution_1950__directive_principles_part_iv, extractiveness, 0.58).
narrative_ontology:constraint_metric(indian_constitution_1950__directive_principles_part_iv, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indian_constitution_1950__directive_principles_part_iv, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_constitution_1950__directive_principles_part_iv, tangled_rope).
narrative_ontology:human_readable(indian_constitution_1950__directive_principles_part_iv, "Indian Constitution Part IV: Directive Principles of Social Policy (Non-Justiciable Conscience)").
narrative_ontology:topic_domain(indian_constitution_1950__directive_principles_part_iv, "constitutional_law/political_economy").

domain_priors:requires_active_enforcement(indian_constitution_1950__directive_principles_part_iv).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(indian_constitution_1950__directive_principles_part_iv, 'f6b40fca-0aec-4bff-8eea-e6ad8ad70899').
narrative_ontology:cs_kernel_codification('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', formalized).
narrative_ontology:cs_authority_grounding('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', extraction).
narrative_ontology:cs_interpretation_layer_present('f6b40fca-0aec-4bff-8eea-e6ad8ad70899').
narrative_ontology:cs_reading_relation('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', indian_constitution_1950__amendment_and_basic_structure, coexists_with).
narrative_ontology:cs_reading_relation('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', indian_constitution_1950__fundamental_rights_part_iii, influences).
narrative_ontology:cs_reading_relation('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', indian_constitution_1950__federal_asymmetry, coexists_with).
narrative_ontology:cs_reading_relation('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', indian_constitution_1950__social_revolution_provisions, coexists_with).
narrative_ontology:cs_axiom('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', foundational, non_justiciability_as_constitutional_feature).
narrative_ontology:cs_axiom_status(non_justiciability_as_constitutional_feature, holdable).
narrative_ontology:cs_axiom_grounding('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', non_justiciability_as_constitutional_feature, conventional).
narrative_ontology:cs_axiom('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', foundational, deferral_permission_as_implementation_gap_source).
narrative_ontology:cs_axiom_status(deferral_permission_as_implementation_gap_source, holdable).
narrative_ontology:cs_axiom_grounding('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', deferral_permission_as_implementation_gap_source, empirically_contingent).
narrative_ontology:cs_reference_frame('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', legislative_discretionary_welfare_implementation).
narrative_ontology:cs_drift_state('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6b40fca-0aec-4bff-8eea-e6ad8ad70899', '').
narrative_ontology:cs_kernel_id(indian_constitution_1950__directive_principles_part_iv, indian_constitution_1950).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_constitution_1950__directive_principles_part_iv, welfare_state_blueprint).
narrative_ontology:constraint_beneficiary(indian_constitution_1950__directive_principles_part_iv, legislative_majorities).
narrative_ontology:constraint_victim(indian_constitution_1950__directive_principles_part_iv, dispossessed_claimants).
narrative_ontology:constraint_victim(indian_constitution_1950__directive_principles_part_iv, inequality_targets_without_remedy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LANDLESS AGRICULTURAL WORKER (SNARE) — Part IV mandates the state 'ensure that the operation of the economic system does not result in the concentration of wealth and means of production to the common detriment' (Article 39). But this mandate is non-justiciable: the worker cannot sue to enforce land redistribution, minimum wages indexed to living costs, or cooperative farming rights. The directive principles exist; the remedy does not. Trapped in a promise-without-enforcement structure. Maximum extraction: the constitutional text names their grievance but forecloses the legal pathway to redress.
constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-MINDED LEGISLATOR (TANGLED ROPE) — Part IV functions as both a coordination mechanism and an extraction mechanism. It coordinates: legislators can cite Part IV principles as the constitutional justification for social legislation (land reform, wage councils, food security programs), legitimizing redistribution within the law. But it also extracts: the non-justiciability leaves implementation entirely to legislative will and administrative capacity. Legislators can claim to implement Part IV while delivering minimal substance. Constrained by electoral cycles and pressure from landed interests, but possessing real agency to frame policy within the Part IV mandate.
constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RULING COALITION'S WELFARE STATE BLUEPRINT (ROPE) — Part IV is pure coordination from this perspective. It provides the constitutional framework for legislation without constraining *which* legislation gets priority or *how much* gets spent. The blueprint benefits from Part IV's non-justiciability: ambitious welfare language signals commitment to social reform while preserving executive and legislative discretion in implementation and resource allocation. No victim can sue to force implementation; the ruling coalition coordinates around Part IV principles while choosing which ones to fund and which to defer indefinitely. Arbitrage position: can cite Part IV to justify any reform (high legitimacy) while avoiding judicial enforcement of any specific obligation (low cost).
constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTION AS PERFORMED TEXT (PITON) — Part IV has become largely performative in institutional practice. Constitution Day speeches invoke the directive principles; annual reports cite them; courts acknowledge them as interpretive guidance without enforcement power. The performance persists despite 75 years of implementation gaps: landlessness persists, wealth concentration has accelerated, many Article 39-44 mandates remain programmatic rather than real. The ritual maintains Part IV in the constitutional pantheon while material non-implementation accumulates uncontested. Theater ratio reflects that ceremonial invocation of Part IV principles far exceeds substantive implementation.
constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL RIGHTS LITIGATION MOVEMENT (SCAFFOLD) — Organized civil society and left-wing parties have built institutional pathways to transform Part IV non-justiciable principles into effective rights through Part III (justiciable fundamental rights) interpretation. Courts have read Part IV into Article 21 (right to life) to mandate food security, healthcare, education, and environmental protection. This is a sunset clause in operation: as judicial interpretation deepens Part IV's enforcement via Part III, the original non-justiciability becomes structurally obsolete. Constrained by judicial conservatism and the need to anchor claims in explicit Part III language, but possessing genuine agency to reshape the constraint.
constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a jurisprudential perspective emphasizing the structural constraints on constitutional design, non-justiciable directive principles represent an immutable feature of social constitutionalism: the law can codify aspirational norms without creating justiciable obligations, preserving legislative flexibility while signaling constitutional commitment. This is presented as inherent to how post-colonial constitutions must balance competing demands. However, the structural data contradicts this — the non-justiciability is a design choice, not a natural law, and the benefit flows primarily to ruling coalitions and away from the dispossessed. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_constitution_1950__directive_principles_part_iv_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_constitution_1950__directive_principles_part_iv, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_constitution_1950__directive_principles_part_iv, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_constitution_1950__directive_principles_part_iv, TR),
    TR >= 0.70.

:- end_tests(indian_constitution_1950__directive_principles_part_iv_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The non-justiciability permits indefinite deferral of Part IV implementation. Landless workers and marginalized communities cannot sue to enforce constitutional promises. The 75-year gap between Part IV's 1950 enactment and persistent landlessness, wealth concentration, and untouchability suggests that non-justiciability functions as permission to defer. The extractiveness has accumulated: early in the constitution's life, Part IV was read as a serious commitment with imminent implementation; by 1970-90, the gap was obvious; by 2026, Part IV is ceremonially invoked while material conditions contradict it. Suppression (0.68): High. Multiple barriers suppress attempts to enforce Part IV principles: (1) formal non-justiciability — no lawsuit can directly compel implementation; (2) judicial conservatism — courts acknowledge Part IV but treat it as hortatory rather than binding; (3) legislative sovereignty — parliament can cite Part IV while choosing minimal implementation and resource allocation; (4) the absence of any organized enforcer — victims cannot litigate, and political parties that champion Part IV face resource constraints and electoral barriers. Theater ratio (0.65): Moderate-high. Constitutional Day speeches cite Part IV's vision; government reports claim implementation progress; courts invoke Part IV as interpretive guidance. But the ceremonial invocation far exceeds substantive impact. Land reform remained incomplete; wage councils were established then weakened; worker participation in management is symbolic; food security programs exist but cover only fractions of the eligible population. The theater has increased over time as the gap between promise and reality has widened — the performance of Part IV commitment intensifies as material implementation stalls.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival gap in the corpus. The same constitutional text — Part IV, Article 39 et al. — produces six structurally incommensurable classifications. The dispossessed see a snare: they are named as beneficiaries of rights they cannot enforce. The welfare state blueprint sees a rope: the perfect coordination mechanism that permits implementation without mandating it. The reform legislator sees tangled_rope: genuine coordination with embedded extraction. The ruling coalition sees rope: pure coordination with discretionary implementation. The social rights movement sees a scaffold with a sunset clause: Part IV is being absorbed into Part III (justiciable fundamental rights) through court interpretation, progressively making non-justiciability obsolete. The civilizational observer risks seeing a mountain: non-justiciability as an inherent feature of how constitutions must balance aspirational norms with legislative flexibility. The structural data contradicts the mountain classification — the non-justiciability is a design choice that benefits identifiable agents (ruling coalitions, legislative majorities) and harms identifiable victims (the dispossessed). This reading contest IS the perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for Part IV produces distinct d values across perspectives: (1) The dispossessed (powerless/trapped) have d ≈ 0.95: they are pure victims with no exit option and no remedy. The sigmoid f(d) produces maximum f(d) ≈ 1.42, amplifying experienced extraction. (2) The welfare state blueprint (institutional/arbitrage) has d ≈ 0.05: it is a beneficiary with full exit (can be cited to justify any reform while avoiding specific implementation). The sigmoid produces f(d) ≈ -0.12, creating negative/institutional chi — the constraint subsidizes this agent. (3) Reform-minded legislators (moderate/constrained) have d ≈ 0.60: partial victims (constrained by electoral and pressure-group limits) and partial beneficiaries (can use Part IV to frame legislation). f(d) ≈ 0.95 produces moderate chi. (4) The ruling coalition (institutional/arbitrage) has d ≈ 0.10: beneficiary with full discretion. f(d) ≈ -0.08. The perspectival gaps are large: the same constitutional text appears as pure extraction (snare) from the powerless perspective, as pure coordination (rope) from the beneficiary perspective, as mixed (tangled_rope) from the moderate perspective, and as degraded ritual (piton) from the civilizational perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   Part IV resolves mandatrophy by showing that the constraint is simultaneously a coordination mechanism (legitimate social legislation) and an extraction mechanism (indefinite deferral of constitutional promises). The mandatrophy is not 'which type is correct?' but 'what does non-justiciability do?' From the victim's perspective, non-justiciability is pure extraction: the promise is constitutional but the remedy is not. From the ruling coalition's perspective, non-justiciability is pure coordination: it allows legislation without judicial enforcement of specific implementation. From the moderate legislator's perspective, non-justiciability is tangled: it both enables and constrains reform. The constraint is genuinely tangled_rope because it possesses BOTH a coordination function (legitimizing social legislation) AND asymmetric extraction (permitting indefinite deferral). The presence of both functions satisfies the tangled_rope gate: beneficiaries (welfare state blueprint, ruling coalition), victims (dispossessed claimants), and active enforcement (constitutional text plus legislative/administrative apparatus minus judicial remedy) are all structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_reading_trajectory,
    'Will Indian courts continue to absorb Part IV principles into Part III (justiciable fundamental rights) interpretation, effectively sunsetting the non-justiciability?',
    'Historical trajectory of cases citing Part IV as interpretive source for Part III rights (food security via Article 21, environmental protection, education); documentation of whether courts explicitly declare Part IV justiciable or maintain the formal non-justiciability while achieving enforcement through Part III anchors',
    'If courts successfully integrate Part IV into Part III: the scaffold perspective is confirmed and the constraint undergoes terminal transformation toward justiciable rights. If courts maintain formal non-justiciability: the snare and tangled_rope perspectives persist and Part IV remains a constitutional promise without remedy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_reading_trajectory, empirical, 'Whether judicial interpretation is sunsetting Part IV non-justiciability').

omega_variable(
    implementation_gap_causation,
    'Is the 75-year implementation gap in Part IV (landlessness, wealth concentration, untouchability persistence) caused primarily by resource scarcity, legislative indifference, or the structural permission that non-justiciability provides to defer indefinitely?',
    'Comparative analysis: states/nations with justiciable social rights vs. those with non-justiciable directive principles; correlation between justiciability and implementation rates; counterfactual: what would implementation rates be if Part IV had been made justiciable in 1950?',
    'If resource scarcity is primary: the constraint''s extractiveness is lower than assessed (η ≈ 0.40) because the bottleneck is material, not institutional. If legislative indifference is primary: the extractiveness is higher (η ≈ 0.72) because non-justiciability permits choosing not to implement despite capacity. If non-justiciability''s permission is primary: the constraint''s beneficiary is explicitly the deferral mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_causation, empirical, 'Whether non-justiciability is the primary cause of implementation gap').

omega_variable(
    reading_contest_ambiguity,
    'Is Part IV a coordination mechanism for legitimate social legislation (ruling coalition reading) or an extraction mechanism that permits indefinite deferral of constitutional promises (victim reading)?',
    'Framers'' intent analysis (Constituent Assembly debates); comparison with contemporary social constitutions (South Africa, Brazil, Kenya); measurement of whether non-justiciability correlates with larger or smaller welfare states; analysis of whether justiciable alternatives in other nations produce superior outcomes',
    'If coordination is primary: Part IV is a rope-type mechanism and non-justiciability is a feature (preserving legislative flexibility). If extraction is primary: Part IV is a snare-type mechanism and non-justiciability is a bug (permitting indefinite deferral). This ambiguity is the kernel dispute itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_ambiguity, conceptual, 'Whether Part IV is coordination or extraction mechanism').

omega_variable(
    committer_frame_ambiguity,
    'Is this constraint properly classified as ''one reading of the kernel Indian Constitution'' or as a distinct structural mechanism with its own stable ε?',
    'Decompose the Part IV constraint from the broader constitution constraint family; test whether ε remains stable when Part IV is evaluated independently vs. as a component of constitutional interpretation; determine whether the sibling readings (Part III rights, amendments, federal structure) have sufficient structural independence to be separate constraints',
    'If Part IV is genuinely a reading (different interpretation of the same commitment system): omega-variable treatment is appropriate. If Part IV is structurally distinct: it should be a separate constraint linked via network.affects_constraints to its siblings. Current authoring assumes the reading frame; empirical testing may require decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Ambiguity in whether Part IV is a kernel reading or independent constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_constitution_1950__directive_principles_part_iv, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, indian_constitution_1950__directive_principles_part_iv, theater_ratio, 0, 0.45).
narrative_ontology:measurement(indi_tr_t15, indian_constitution_1950__directive_principles_part_iv, theater_ratio, 15, 0.62).
narrative_ontology:measurement(indi_tr_t30, indian_constitution_1950__directive_principles_part_iv, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, indian_constitution_1950__directive_principles_part_iv, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(indi_be_t15, indian_constitution_1950__directive_principles_part_iv, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(indi_be_t30, indian_constitution_1950__directive_principles_part_iv, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(indi_su_t0, indian_constitution_1950__directive_principles_part_iv, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(indi_su_t15, indian_constitution_1950__directive_principles_part_iv, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(indi_su_t30, indian_constitution_1950__directive_principles_part_iv, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_constitution_1950__directive_principles_part_iv, enforcement_mechanism).
narrative_ontology:affects_constraint(indian_constitution_1950__directive_principles_part_iv, indian_constitution_1950__fundamental_rights_part_iii).
narrative_ontology:affects_constraint(indian_constitution_1950__directive_principles_part_iv, indian_constitution_1950__amendment_and_basic_structure).
narrative_ontology:affects_constraint(indian_constitution_1950__directive_principles_part_iv, indian_constitution_1950__social_revolution_provisions).

% DUAL FORMULATION NOTE:
% Part IV is one reading of the Indian Constitution kernel. It is structurally entangled with Part III (Fundamental Rights), the amendment/basic structure doctrine, and the social revolution provisions. Each reading has different ε and different classification structure. They are linked via network.affects_constraints because judicial interpretation, amendments, and legislative action under Part III all reshape Part IV's effective enforceability. However, each reading should be authored as a separate constraint story with its own stable ε. The constraint family is: indian_constitution_1950__directive_principles_part_iv (this story, ε ≈ 0.58), indian_constitution_1950__fundamental_rights_part_iii (ε ≈ 0.25, Mountain from judicial perspective), indian_constitution_1950__amendment_and_basic_structure (ε ≈ 0.40, Tangled Rope), indian_constitution_1950__social_revolution_provisions (ε ≈ 0.15, Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_constitution_1950__directive_principles_part_iv, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
