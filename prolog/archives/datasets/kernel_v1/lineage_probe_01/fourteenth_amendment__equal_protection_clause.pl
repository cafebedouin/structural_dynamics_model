% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment__equal_protection_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment__equal_protection_clause, []).

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
 *   constraint_id: fourteenth_amendment__equal_protection_clause
 *   human_readable: Fourteenth Amendment Equal Protection Clause: State-Imposed Hierarchy Suppression
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Equal Protection Clause forbids states to deny any person the equal
 *   protection of the laws. This constraint instantiates ONE reading of the
 *   Fourteenth Amendment kernel — a contested constitutional commitment that
 *   can be read through the equal protection lens (this story), the
 *   citizenship lens (citizenship clause reading), the due process lens (due
 *   process clause reading), or the privileges/immunities lens (privileges or
 *   immunities clause reading). Each reading instantiates a structurally
 *   distinct constraint with different beneficiary/victim relationships,
 *   different enforcement mechanisms, and different doctrinal architecture.
 *   This story focuses on the equal protection reading: the doctrine that
 *   states cannot use explicit classifications to create or maintain caste
 *   hierarchies, and that equal treatment (rather than citizenship, due
 *   process, or privileges) is the constitutional guarantee. The constraint
 *   operates as a tangled rope: genuine coordination function (organizing
 *   judicial review of state classifications) alongside asymmetric extraction
 *   (federal review limiting state sovereignty). The suppression measurement
 *   trajectory from 1865 to 1975 reflects the doctrine's declining
 *   enforcement intensity as states adapted by adopting facially neutral
 *   mechanisms (residential zoning, wealth-based education funding, disparate
 *   impact doctrine). The theater_ratio trajectory reflects increasing
 *   performative compliance: states invoke equal protection language while
 *   maintaining hierarchies through facially neutral means. The doctrine
 *   functions as a scaffold during the Civil Rights era (temporary structural
 *   support for dismantling explicit caste), then degrades into a piton
 *   (persistent ritual language with attenuated function).
 *
 * KEY AGENTS:
 *   - Disfavored Classes: Primary beneficiary and victim (powerless/trapped) — benefit from equal protection guarantee but trapped within the categories the state uses for classification; cannot exit the classification itself
 *   - Civil Rights Coalition: Organized victim group (organized/constrained) — benefits from the doctrine as a legal framework for mobilization but constrained by state power and variable judicial enforcement
 *   - Federal Judiciary: Institutional beneficiary (institutional/arbitrage) — experiences doctrine as pure coordination mechanism for organizing review authority; arbitrages between scrutiny levels
 *   - State Legislatures: Institutional victim (institutional/constrained) — constrained by equal protection review but benefit from legitimate state purposes; experience mixed extraction and coordination
 *   - Progressive Reform Movements: Temporary coalition (organized/constrained, generational timescale) — use equal protection as scaffolding during Civil Rights era; doctrine's coercive force declines as states internalize norms
 *   - Doctrinal System: Institutional ritual (institutional/arbitrage, civilizational timescale) — maintains performative compliance language while substantive suppression attenuates; piton perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment__equal_protection_clause, 0.35).
domain_priors:suppression_score(fourteenth_amendment__equal_protection_clause, 0.68).
domain_priors:theater_ratio(fourteenth_amendment__equal_protection_clause, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment__equal_protection_clause, extractiveness, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment__equal_protection_clause, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment__equal_protection_clause, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment__equal_protection_clause, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment__equal_protection_clause, "Fourteenth Amendment Equal Protection Clause: State-Imposed Hierarchy Suppression").
narrative_ontology:topic_domain(fourteenth_amendment__equal_protection_clause, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment__equal_protection_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment__equal_protection_clause, 'ec855081-8604-4ff1-bb67-20271c97413d').
narrative_ontology:cs_kernel_codification('ec855081-8604-4ff1-bb67-20271c97413d', fixed_text).
narrative_ontology:cs_authority_grounding('ec855081-8604-4ff1-bb67-20271c97413d', lineage).
narrative_ontology:cs_interpretation_layer_present('ec855081-8604-4ff1-bb67-20271c97413d').
narrative_ontology:cs_reading_relation('ec855081-8604-4ff1-bb67-20271c97413d', fourteenth_amendment__citizenship_clause, coexists_with).
narrative_ontology:cs_reading_relation('ec855081-8604-4ff1-bb67-20271c97413d', fourteenth_amendment__due_process_clause, coexists_with).
narrative_ontology:cs_reading_relation('ec855081-8604-4ff1-bb67-20271c97413d', fourteenth_amendment__privileges_or_immunities_clause, influences).
narrative_ontology:cs_axiom('ec855081-8604-4ff1-bb67-20271c97413d', foundational, state_classification_suppression_requirement).
narrative_ontology:cs_axiom_status(state_classification_suppression_requirement, holdable).
narrative_ontology:cs_axiom_grounding('ec855081-8604-4ff1-bb67-20271c97413d', state_classification_suppression_requirement, deontological).
narrative_ontology:cs_axiom('ec855081-8604-4ff1-bb67-20271c97413d', secondary, hierarchy_elimination_through_formal_equality).
narrative_ontology:cs_axiom_status(hierarchy_elimination_through_formal_equality, overridden).
narrative_ontology:cs_axiom_grounding('ec855081-8604-4ff1-bb67-20271c97413d', hierarchy_elimination_through_formal_equality, empirically_contingent).
narrative_ontology:cs_reference_frame('ec855081-8604-4ff1-bb67-20271c97413d', equal_protection_guarantee_against_explicit_state_caste).
narrative_ontology:cs_drift_state('ec855081-8604-4ff1-bb67-20271c97413d', contemporary_post_1975_doctrinal_present, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec855081-8604-4ff1-bb67-20271c97413d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(fourteenth_amendment__equal_protection_clause, fourteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment__equal_protection_clause, disfavored_classes).
narrative_ontology:constraint_beneficiary(fourteenth_amendment__equal_protection_clause, equal_protection_doctrine).
narrative_ontology:constraint_victim(fourteenth_amendment__equal_protection_clause, state_classification_regimes).
narrative_ontology:constraint_victim(fourteenth_amendment__equal_protection_clause, subordinating_hierarchies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISFAVORED CLASS MEMBER (SNARE) — Trapped within state classification regimes (race, gender, national origin) with no exit from the category itself. Experiences maximum suppression: the state denies equal protection through law and custom simultaneously. The doctrine provides a nominal guarantee but enforcement is incomplete and contested. For the trapped agent, this is pure extraction — the state uses classification to subordinate and the remedy is structurally weak.
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized victim groups (NAACP, civil rights movements, allied organizations) benefit from the equal protection clause as a coordination mechanism for challenging state hierarchy. The doctrine provides a legal framework for organization and mobilization. But enforcement remains constrained by state power and judicial review intensity. The coalition bears the cost of ongoing litigation and political struggle while also benefiting from the doctrinal tool. Mixed coordination and extraction.
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY (ROPE) — Experiences equal protection doctrine as a pure coordination mechanism for adjudicating state classification regimes. The judiciary has institutional power to interpret the clause and arbitrage between different levels of scrutiny (rational basis, intermediate, strict). The doctrine enables the judiciary to organize its review function. No extraction flows to the judiciary from the equal protection doctrine itself — it is a framework for organizing judicial authority over state action.
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LEGISLATURES (TANGLED ROPE) — States benefit from classification authority (regulating education, criminal justice, social benefits, occupational licensing) but are constrained by equal protection review. States experience the doctrine as both coordinating legitimate state purposes (public health, safety, welfare) and extracting power from states by forbidding certain classifications. The constraint is mixed: genuine coordination (defining valid state purposes) with asymmetric extraction (federal review of state sovereignty).
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE REFORM MOVEMENTS (SCAFFOLD) — During Reconstruction and the Civil Rights era, the equal protection clause functioned as temporary structural support for dismantling explicit racial hierarchies. The doctrine had a sunset logic: once states abandoned de jure classifications and adopted race-neutral language, the clause's enforcement pressure declined. The Modern Strict Scrutiny framework (post-1960s) applies high review to explicit classifications but lower review to facially neutral policies with disparate impact, creating the scaffold's sunset: the doctrine's coercive force was meant to be temporary, dissolving as states internalized equal protection norms. This perspective sees the clause as high-theater coordination during the reform period.
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DOCTRINE AS PITON (DEGRADED ENFORCEMENT) — At civilizational scale, formal equal protection doctrine has degraded into performative compliance. States maintain race-neutral language while sorting populations through facially neutral mechanisms (residential segregation, disparate impact in criminal justice, wealth-based educational allocation). The doctrine persists as an institutional ritual — courts invoke strict scrutiny language while applying rational basis review in practice (Arlington Heights disparate impact doctrine). The theater ratio is high because the doctrine's core function (preventing state-imposed hierarchy) has attenuated while the symbolic commitment persists. Theater_ratio reflects that enforcement has become increasingly theatrical relative to function.
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL/UNIVERSAL (MOUNTAIN CANDIDATE) — From a universal civilizational perspective, equal protection against state-imposed caste could be viewed as a fundamental logical principle: any legitimate state authority requires a basis for differential treatment, and absent such basis, equal treatment is the default. The analytical observer might frame equal protection as an irreducible feature of legitimate authority — states cannot justify hierarchy without reason, and classification without reason is logically impossible. However, the structural data shows identifiable beneficiaries (disfavored classes) and contested enforcement regimes, which contradicts the mountain classification. The engine will flag this as a false summit: naturalizing a contingent doctrinal reading as a universal logical requirement.
constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment__equal_protection_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment__equal_protection_clause, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourteenth_amendment__equal_protection_clause, TR),
    TR >= 0.70.

:- end_tests(fourteenth_amendment__equal_protection_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The equal protection clause, as a constraint on state action, does extract compliance costs from states (limiting classification options, requiring justification for differential treatment). But the extraction is not severe because states can satisfy the constraint through facially neutral mechanisms that maintain hierarchical outcomes. The modern doctrine (Washington v Davis intent standard) permits substantial hierarchy maintenance as long as the state articulates a non-discriminatory purpose. The low base extractiveness reflects that the constraint's suppressive force has attenuated over time — states adapted by adopting the language of equal protection while maintaining hierarchical effects. Suppression (0.68): High. The suppression measurement is high because the constraint specifically forbids state use of explicit classifications (the mechanism of caste). But suppression is not total (0.85+) because states can achieve similar outcomes through facially neutral mechanisms. Suppression measures the state's inability to use explicit caste classifications, not the state's inability to maintain hierarchies. Theater_ratio (0.55): Moderate-high. The doctrine has increasingly become performative: states adopt equal protection language and structures (formal equal review, rational basis tests) while maintaining substantive hierarchies through facially neutral mechanisms (housing segregation via zoning, educational stratification via wealth-based funding, criminal justice disparities via ostensibly race-neutral policies). The theater_ratio trajectory (0.30 → 0.55 over 110 years) reflects the increasing gap between doctrinal language and substantive equality. At the Reconstruction moment (1865), the doctrine's function was more direct — it actively suppressed explicit caste classifications and enforcement was immediate. By 1975, the doctrine had become increasingly theatrical — the same language persisted but the enforcement mechanism (intent standard) permitted hierarchies to persist through facially neutral means.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is large and consequential. The disfavored class member (powerless/trapped) experiences pure snare: the state's legal denial of equal protection. The organized civil rights coalition (organized/constrained) experiences tangled rope: coordination mechanism for mobilization alongside constrained enforcement. The federal judiciary (institutional/arbitrage) experiences rope: a coordination framework for review authority. State legislatures (institutional/constrained) experience tangled rope: constrained state authority alongside legitimate state purposes. The reform coalition (organized/constrained, generational timescale) experiences scaffold: temporary structural support with a sunset as norms internalize. The doctrinal system itself (institutional/arbitrage, civilizational timescale) experiences piton: performative ritual language with attenuated function. The analytical observer (analytical/analytical, civilizational/universal scope) risks seeing mountain: fundamental logical requirement of legitimate authority. This perspectival spread — from snare to mountain — reflects that equal protection is not a neutral fact but a contested institutional arrangement whose classification depends on power, time horizon, and scope. The false summit risk is particularly acute here: the analytical observer (or post-Reconstruction legal theorist) might naturalize equal protection as a universal requirement of legitimate governance, but the structural data reveals it as a contingent doctrinal choice with identifiable beneficiaries (disfavored classes, federal judiciary) and contested enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position: power level, exit options, and relationship to the constraint's flow. Disfavored class members (powerless/trapped) have d approaching 1.0 — they are full targets of state classification and have no exit. Civil rights coalitions (organized/constrained) have d in the 0.5-0.7 range — they bear costs of ongoing struggle but have some agency and mobilization capacity. Federal judiciary (institutional/arbitrage) has d near 0.0 — they benefit from the constraint (authority to review state action) and have exit options (alternative constitutional doctrines). State legislatures (institutional/constrained) have d in the 0.4-0.6 range — they are targets of federal review but also benefit from legitimate state purposes. The analytical observer (analytical/analytical) has d around 0.7 — they experience the constraint as a general principle but are not bound by its outcomes in the way trapped agents are. The directionality derivation reveals that the constraint's experienced extractiveness (χ) varies dramatically across perspectives: low or negative for beneficiaries (judiciary, state authority), high for victims (disfavored classes, constrained agents). This perspectival differentiation is the core insight of the constraint story — equal protection is not uniformly extractive or coordinating, but rather creates radically different structural experiences depending on one's position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies how mandatrophy resolves through perspectival multiplication rather than type selection. The question 'is equal protection a coordination mechanism (rope) or an extractive constraint (snare)?' has no single answer because it is not a single question. Equal protection is a rope from the judiciary's perspective (pure coordination for organizing review), a snare from the disfavored class's perspective (maximum experienced extraction), a tangled rope from the civil rights coalition's perspective (mixed), a scaffold from the reform movement's perspective (temporary support with sunset), and a piton from the civilizational perspective (degraded performative ritual). The resolution is not 'it is actually a tangled rope' but rather 'it is a presheaf of constraints indexed to observer position.' The analytical observer's mountain classification is a false summit — naturalizing a contingent institutional choice as a universal law. The mandatrophy resolves when the analyst recognizes that equal protection is not a single constraint type but a family of constraint types instantiated across different observational contexts. This moves the analysis from 'which type is correct?' to 'how does the constraint's type depend on observer position, and what does that dependence reveal about the constraint's legitimacy?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classifier_intent_vs_effect,
    'Does equal protection doctrine track the intent of state classification or its actual effect on caste hierarchies?',
    'Historical analysis of Washington v. Davis (intent standard) vs alternatives (disparate impact standard); comparison of doctrinal outcomes under intent vs effect frameworks; empirical measurement of hierarchy reduction under each standard',
    'Intent standard (current doctrine): states can maintain hierarchies through facially neutral mechanisms, reducing extractiveness of suppression and raising theater_ratio. Effect standard: would lower theater_ratio and raise suppression measurement, changing classification trajectory. This is not a false choice between two facts about the world — it is a choice about which social reality the doctrine recognizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classifier_intent_vs_effect, conceptual, 'Whether equal protection tracks intent or effect of classification').

omega_variable(
    remedial_scope_boundary,
    'When the equal protection clause forbids a classification, what remedies and scope of correction does the doctrine require? Full equalization or merely ceasing active suppression?',
    'Doctrinal analysis of disparate impact remedies, affirmative action jurisprudence, institutional reform cases (desegregation, reapportionment); empirical measurement of whether equal protection enforcement reduces measured hierarchy or merely prevents new classifications',
    'Minimal scope (cease suppression only): extractiveness remains moderate, theater_ratio remains high. Maximal scope (mandate equalization): extractiveness increases (more institutional coercion on states), theater_ratio decreases. The boundary choice determines whether equal protection is a snare-suppressor or a generative equalizer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_scope_boundary, conceptual, 'Scope of remedies required by equal protection doctrine').

omega_variable(
    facially_neutral_hierarchy_permissibility,
    'Under modern doctrine, are facially neutral state actions that maintain historical caste hierarchies (residential segregation via facially neutral zoning, educational stratification via wealth-based funding) prohibited by equal protection?',
    'Analysis of Arlington Heights v Metropolitan Housing Corp and disparate impact doctrine; empirical measurement of whether facially neutral classifications correlate with caste maintenance; comparison with jurisdictions that apply disparate impact standards vs intent-only standards',
    'If facially neutral hierarchies are prohibited: extractiveness rises (broader suppression of state action), theater_ratio falls (doctrine enforces actual equality). If permitted: extractiveness remains moderate (suppression applies only to explicit classifications), theater_ratio remains high (formal compliance without substantive change). This is the critical boundary determining whether the doctrine prevents caste or merely prevents explicit caste language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(facially_neutral_hierarchy_permissibility, empirical, 'Whether facially neutral maintenance of hierarchies violates equal protection').

omega_variable(
    reading_kernel_contest,
    'How does the equal protection reading compete with the citizenship clause, due process clause, and privileges or immunities readings of the Fourteenth Amendment as a whole?',
    'Doctrinal analysis of how each clause has been interpreted across periods; examination of cases that invoke multiple clauses; assessment of which clause is operative in different domains (citizenship status, substantive rights, procedural fairness, equal treatment); historical analysis of whether one reading forecloses another or whether all coexist within contemporary jurisprudence',
    'If equal protection forecloses citizenship-based rights (all protection through equal classification, not citizenship status): the citizenship clause reading becomes subordinate. If due process incorporates equal protection (substantive due process includes equality), the readings influence each other. If all four coexist in separate domains: no foreclosure, just structural differentiation. The reading contest is not empirically resolvable — it is a matter of how judges choose to integrate the amendment''s text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Structural relationship between equal protection and other Fourteenth Amendment readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment__equal_protection_clause, 1865, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_theater_1865_reconstruction, fourteenth_amendment__equal_protection_clause, theater_ratio, 1865, 0.3).
narrative_ontology:measurement(epc_theater_1900_jim_crow, fourteenth_amendment__equal_protection_clause, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(epc_theater_1954_brown, fourteenth_amendment__equal_protection_clause, theater_ratio, 1954, 0.48).
narrative_ontology:measurement(epc_theater_1975_modern_doctrine, fourteenth_amendment__equal_protection_clause, theater_ratio, 1975, 0.55).

% Extraction over time
narrative_ontology:measurement(epc_extractiveness_1865, fourteenth_amendment__equal_protection_clause, base_extractiveness, 1865, 0.12).
narrative_ontology:measurement(epc_extractiveness_1900, fourteenth_amendment__equal_protection_clause, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(epc_extractiveness_1954, fourteenth_amendment__equal_protection_clause, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(epc_extractiveness_1975, fourteenth_amendment__equal_protection_clause, base_extractiveness, 1975, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(epc_suppression_1865_reconstruction, fourteenth_amendment__equal_protection_clause, suppression_requirement, 1865, 0.95).
narrative_ontology:measurement(epc_suppression_1900_jim_crow, fourteenth_amendment__equal_protection_clause, suppression_requirement, 1900, 0.88).
narrative_ontology:measurement(epc_suppression_1954_brown_decision, fourteenth_amendment__equal_protection_clause, suppression_requirement, 1954, 0.75).
narrative_ontology:measurement(epc_suppression_1975_post_civil_rights, fourteenth_amendment__equal_protection_clause, suppression_requirement, 1975, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment__equal_protection_clause, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment__equal_protection_clause, fourteenth_amendment__citizenship_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__equal_protection_clause, fourteenth_amendment__due_process_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__equal_protection_clause, fourteenth_amendment__privileges_or_immunities_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__equal_protection_clause, strict_scrutiny_doctrine).
narrative_ontology:affects_constraint(fourteenth_amendment__equal_protection_clause, disparate_impact_doctrine).
narrative_ontology:affects_constraint(fourteenth_amendment__equal_protection_clause, washington_v_davis_intent_standard).

% DUAL FORMULATION NOTE:
% The equal protection clause is one reading of a contested constitutional kernel (the Fourteenth Amendment). Other readings of the same kernel include citizenship, due process, and privileges/immunities. These are not different observational perspectives on a single constraint — they are structurally distinct constraints instantiated from the same text. Network effects: the equal protection reading influences the strict scrutiny doctrine (doctrinal operationalization), the disparate impact doctrine (competing interpretation), and the Washington v Davis intent standard (limiting interpretation). The reading is itself influenced by the citizenship and privileges readings, which could provide alternative doctrinal grounds for protecting disfavored classes. The interaction between readings is not a dependency graph but a field of competing claims about what the Fourteenth Amendment's core function is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment__equal_protection_clause, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
