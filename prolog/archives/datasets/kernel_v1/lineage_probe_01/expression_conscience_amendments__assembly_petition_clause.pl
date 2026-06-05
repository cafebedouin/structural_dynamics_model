% ============================================================================
% CONSTRAINT STORY: expression_conscience_amendments__assembly_petition_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expression_conscience_amendments__assembly_petition_clause, []).

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
 *   constraint_id: expression_conscience_amendments__assembly_petition_clause
 *   human_readable: Assembly and Petition Clauses: Collective Political Aggregation
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Assembly and Petition Clauses (First Amendment, read alongside the
 *   Fourteenth Amendment's incorporation and protection) establish the
 *   structural mechanism by which politically powerless individuals aggregate
 *   into a force the state must acknowledge. This reading of the First
 *   Amendment freedom cluster treats collective political action as the
 *   foundational right — not individual expression, not institutional press
 *   freedom, not religious exercise — but the capability to organize
 *   collectively against state power. The constraint operates as a tangled
 *   rope: it provides genuine coordination function (enabling political
 *   movements, enabling minority voice, creating legal pathways for dissent)
 *   while simultaneously extracting from state order-maintenance capacity and
 *   channeling dissent into manageable institutional forms. The measurement
 *   trajectory (extractiveness rising from 0.18 to 0.38 over the interval,
 *   suppression rising from 0.45 to 0.65) reflects that enforcement of
 *   assembly and petition rights requires increasingly active state
 *   constraint on its own suppression machinery — as state capacity for
 *   surveillance and control grows (modern policing, digital tracking, permit
 *   systems), the enforcement burden rises. Theater rises modestly (0.42 to
 *   0.55) because modern assembly rights enforcement increasingly involves
 *   performative compliance: permits issued that are rarely denied, marches
 *   authorized with police presence, dissent channeled through legal pathways
 *   while substantive state power remains concentrated. The constraint is a
 *   reading of the contested First Amendment kernel, one among five distinct
 *   constitutional protections (free speech, free press, free exercise,
 *   establishment, assembly/petition) that ground their legitimacy in
 *   different axioms and authority structures. This reading emphasizes the
 *   aggregation function — the otherwise powerless becoming powerful through
 *   collective action — as the foundational democratic right.
 *
 * KEY AGENTS:
 *   - Movements and Organized Marginal: Primary beneficiary (organized/mobile) — civil rights organizations, labor unions, protest movements gain aggregated political power and legal protection for collective action
 *   - Atomized Citizens: Primary victim (powerless/trapped) — without assembly/petition protection, face maximum state extraction through enforced isolation and lack of collective voice
 *   - Order-Maintenance State: Secondary victim from the reading's perspective (powerful/arbitrage) — constrained from using most efficient suppression mechanisms (assembly bans, petition criminalization, enforced atomization)
 *   - Liberal Constitutional Framework: Institutional actor (institutional/constrained) — courts and legislatures that enforce and interpret the clause; experience both genuine coordination (stable democracy) and extracted legitimacy (appearance of openness masking concentrated power)
 *   - Protest Movement: Moderate actor (moderate/constrained) — gains assembly rights but faces suppression through permits, dispersal, counter-demonstrations; experiences mixed coordination and extraction
 *   - Civil Rights Organization: Established organization (organized/mobile) — experiences pure coordination through assembly/petition rights; minimal extraction due to institutional capacity and legal sophistication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expression_conscience_amendments__assembly_petition_clause, 0.38).
domain_priors:suppression_score(expression_conscience_amendments__assembly_petition_clause, 0.65).
domain_priors:theater_ratio(expression_conscience_amendments__assembly_petition_clause, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expression_conscience_amendments__assembly_petition_clause, extractiveness, 0.38).
narrative_ontology:constraint_metric(expression_conscience_amendments__assembly_petition_clause, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(expression_conscience_amendments__assembly_petition_clause, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expression_conscience_amendments__assembly_petition_clause, tangled_rope).
narrative_ontology:human_readable(expression_conscience_amendments__assembly_petition_clause, "Assembly and Petition Clauses: Collective Political Aggregation").
narrative_ontology:topic_domain(expression_conscience_amendments__assembly_petition_clause, "political/constitutional").

domain_priors:requires_active_enforcement(expression_conscience_amendments__assembly_petition_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(expression_conscience_amendments__assembly_petition_clause, '3d520b20-a066-4d2b-8c2a-743c06f14ede').
narrative_ontology:cs_kernel_codification('3d520b20-a066-4d2b-8c2a-743c06f14ede', formalized).
narrative_ontology:cs_authority_grounding('3d520b20-a066-4d2b-8c2a-743c06f14ede', lineage).
narrative_ontology:cs_interpretation_layer_present('3d520b20-a066-4d2b-8c2a-743c06f14ede').
narrative_ontology:cs_reading_relation('3d520b20-a066-4d2b-8c2a-743c06f14ede', expression_conscience_amendments__free_speech_clause, coexists_with).
narrative_ontology:cs_reading_relation('3d520b20-a066-4d2b-8c2a-743c06f14ede', expression_conscience_amendments__free_press_clause, coexists_with).
narrative_ontology:cs_reading_relation('3d520b20-a066-4d2b-8c2a-743c06f14ede', expression_conscience_amendments__free_exercise_clause, coexists_with).
narrative_ontology:cs_reading_relation('3d520b20-a066-4d2b-8c2a-743c06f14ede', expression_conscience_amendments__establishment_clause, coexists_with).
narrative_ontology:cs_axiom('3d520b20-a066-4d2b-8c2a-743c06f14ede', foundational, aggregated_political_power_necessary_right).
narrative_ontology:cs_axiom_status(aggregated_political_power_necessary_right, holdable).
narrative_ontology:cs_axiom_grounding('3d520b20-a066-4d2b-8c2a-743c06f14ede', aggregated_political_power_necessary_right, deontological).
narrative_ontology:cs_axiom('3d520b20-a066-4d2b-8c2a-743c06f14ede', foundational, atomization_extraction_mechanism).
narrative_ontology:cs_axiom_status(atomization_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3d520b20-a066-4d2b-8c2a-743c06f14ede', atomization_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('3d520b20-a066-4d2b-8c2a-743c06f14ede', political_aggregation_enabling_framework).
narrative_ontology:cs_drift_state('3d520b20-a066-4d2b-8c2a-743c06f14ede', contemporary_digital_surveillance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d520b20-a066-4d2b-8c2a-743c06f14ede', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(expression_conscience_amendments__assembly_petition_clause, expression_conscience_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__assembly_petition_clause, movements_and_organized_marginal).
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__assembly_petition_clause, political_associations).
narrative_ontology:constraint_victim(expression_conscience_amendments__assembly_petition_clause, order_maintenance_interests).
narrative_ontology:constraint_victim(expression_conscience_amendments__assembly_petition_clause, state_monopoly_on_force).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATOMIZED INDIVIDUAL (SNARE) — Without assembly and petition rights, the individual citizen faces maximum extraction by the state: no collective power to resist, no organized voice, no mechanism to aggregate dispersed interests into audible political force. The constraint (if absent or suppressed) operates as pure extraction: enforced political atomization. This perspective shows why the reading matters — absence of assembly/petition protection is a snare on the powerless.
constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROTEST MOVEMENT (TANGLED ROPE) — Experiences the clause as both coordination and extraction. The clause enables assembly and petition (genuine coordination function) but is constrained by permit requirements, dispersal laws, counter-demonstration rights, and police discretion. The movement benefits from the right to organize but also bears suppression costs. Extraction flows from order-maintenance interests seeking to minimize disruption, but the movement has agency through numbers and media attention.
constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL RIGHTS ORGANIZATION (ROPE) — Established organizations (NAACP, Sierra Club, labor unions) experience the clause as pure coordination: it creates the legal basis for their core function (political mobilization, petition, advocacy). Extraction is minimal because these actors have capacity to navigate legal constraints and media platforms. The clause solves the collective action problem without coercive overhead.
constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ORDER-MAINTENANCE STATE (SNARE) — Government interests in order maintenance, public safety, and efficient administration experience the clause as pure extraction: it forbids the most efficient suppression mechanisms (banning assembly, criminalizing petition, enforcing atomization). The state cannot freely extract obedience if citizens can organize collectively. The clause's existence constrains the state's maximum coercive power. This is snare from the powerful because the constraint operates as a unilateral restriction on their preferred tools.
constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LIBERAL CONSTITUTIONAL FRAMEWORK (TANGLED ROPE) — The doctrine as institutional actor (courts, legislatures interpreting and enforcing the clause) experiences both genuine coordination (enabling stable democratic participation) and embedded extraction (protecting incumbent power against revolutionary overthrow, channeling dissent through manageable legal pathways). The framework coordinates political voice while extracting legitimacy from the appearance of openness — a genuine tangled rope.
constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LOGICAL NECESSITY (MOUNTAIN) — From a civilizational analytical perspective, the assembly and petition rights are viewed as logically necessary to any stable democratic system: without the ability to aggregate dispersed political power, formal democratic procedures (voting, representation) cannot function. This perspective treats the clause as a natural law of political systems. However, the structural data (identified beneficiaries and victims, measurable suppression and extractiveness) indicates this is a false summit — the constraint is contingent and produces asymmetric extraction.
constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expression_conscience_amendments__assembly_petition_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expression_conscience_amendments__assembly_petition_clause, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(expression_conscience_amendments__assembly_petition_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, rising from 0.18): Moderate and increasing. The base extractiveness reflects that assembly and petition rights protect against a specific form of state extraction — enforced political atomization — but do not eliminate state power or order-maintenance capacity. The rising trajectory indicates that as state surveillance and control technologies advance, the enforcement burden on the Assembly/Petition Clause increases: more active constraint is required to prevent state suppression of collective action. Modern permit systems, surveillance infrastructure, and police tactics represent increasing state capacity for suppression that the clause must actively counter. Suppression (0.65, rising from 0.45): Moderate-high and increasing. Suppression measures include permit denial, dispersal authority, counter-demonstration rights, protest injunctions, surveillance of organizing, and police presence at assemblies. These are not absolute bans (which would suggest a snare from all perspectives) but graduated restrictions that suppress the right without eliminating it. Rising suppression reflects institutional ratchet effect: as state apparatus develops new suppression tools, the doctrine must evolve to maintain the constraint. Theater (0.55, rising from 0.42): Moderate and increasing. Modern assembly rights enforcement increasingly involves performative compliance: permits issued routinely but with conditions, marches authorized with heavy police presence, dissent legally protected while institutional power remains concentrated. Theater rises as courts and legislatures handle assembly rights through procedural accommodation rather than substantive power-sharing. This is the tangled-rope signature: genuine coordination function co-present with embedded extraction and theatrical legitimation.
 *
 * PERSPECTIVAL GAP:
 *   The Assembly/Petition reading generates stark perspectival gaps across power levels. The atomized citizen (powerless/trapped) sees the clause as protecting against pure extraction (snare absent the clause; would be snare if suppressed). The order-maintenance state (powerful/arbitrage) sees the clause as pure extraction of its suppression capacity (snare from the state's perspective). The civil rights organization (organized/mobile) sees pure coordination (rope) — the clause creates the legal foundation for organizing. The protest movement (moderate/constrained) sees tangled rope — genuine aggregation plus suppression costs. The constitutional framework (institutional/constrained) sees tangled rope — both coordination and extracted legitimacy. The analytical observer (analytical/analytical) risks seeing natural law (mountain) — assembly rights as logically necessary to democracy — but the structural data (rising suppression, rising extractiveness, identified beneficiaries and victims, rising theater) indicate this is a false summit naturalizing a contingent institutional arrangement. The gap between mountain and tangled rope reveals the reading's core stakes: is collective political action a natural right or a contingent constitutional grant that requires constant enforcement?
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their structural position relative to the constraint's extraction flow. Atomized citizens are victims without exit options (trapped) — maximum d (near 1.0) produces high experienced extraction (snare). The order-maintenance state benefits from absence of the constraint but is target of the constraint when it exists — they are effectively victims of the constraint's enforcement, producing high d and snare classification. Civil rights organizations are beneficiaries with exit alternatives (organizational capacity, media platforms, legal resources) — low d (near 0.15) produces minimal experienced extraction (rope). Protest movements are victims with some exit capacity (media attention, numbers, geographic mobility) — moderate d (around 0.60) produces moderate extraction (tangled rope). The constitutional framework is institutionally beneficiary (creates legitimate governance appearance) but constrained by the need to enforce the clause against state capacity for suppression — moderate-low d producing tangled rope. These directionality values differ from simple beneficiary/victim designation because they account for exit capacity and power asymmetries within roles. A victim with arbitrage options (civil rights organization with media access and funding) has lower d than a victim with no exit (atomized citizen). A beneficiary facing enforcement constraints (state order-maintenance) has higher d than an unconstrained beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This is one reading of the contested First Amendment kernel. The mandatrophy is resolved by explicitly declaring the reading identity and axiom set, then showing that the classification follows from the axioms plus the structural data. The tangled-rope classification derives from: (1) genuine coordination function (enabling political aggregation, protecting minority voice, creating legal pathways for dissent) and (2) asymmetric extraction (constraining state suppression capacity, channeling dissent through institutional forms that preserve state power concentration). The rising extractiveness and suppression measurements show accumulating cost of enforcement — the constraint must work harder as state suppression capacity grows. Theater rising indicates increasing performative dimension: dissent is legally protected but substantive power remains concentrated. The mountain classification from the analytical observer is a false summit — the claim that assembly rights are natural laws of democracy. The FSM would trigger because: (1) beneficiaries are identified (movements, organized marginal), (2) the constraint produces measurable extraction effects, and (3) the natural law framing obscures contingent institutional choices. The committer frame (Rule 2) routes this through omega variables: the reading contest omega, the naturalness-vs-contingency omega, and the measuring-suppression omega all document the committer content — what distinguishes this reading from siblings, where the disagreement is located, and what alternatives the reading forecloses or coexists with.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    assembly_suppression_measurement,
    'What constitutes suppression of assembly and petition rights: de jure bans only, or de facto barriers (permit denial, policing tactics, counter-demonstration abuse)?',
    'Comparative analysis of suppression trajectories: jurisdictions with formal rights but high permit denial rates vs. jurisdictions with explicit bans. Measurement of actual assembly frequency and participation rates vs. formal legal status.',
    'If suppression is measured de jure only: many modern democracies appear to protect the right while suppressing the exercise. If measured de facto: effective suppression is higher than doctrinal analysis suggests, and the tangled-rope classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(assembly_suppression_measurement, empirical, 'Definition and measurement of suppression of assembly and petition rights').

omega_variable(
    atomization_extractiveness,
    'How much extractive capacity does the state gain from enforced political atomization compared to other suppression mechanisms?',
    'Historical comparison of state capacity in regimes with vs. without assembly/petition protections. Analysis of extraction rates (taxation, forced labor, conscription, compliance with arbitrary orders) before and after prohibition of collective action.',
    'If atomization dramatically increases state extraction capacity: the snare classification (from the powerless perspective) is understated, and the order-maintenance state''s perspective becomes more intensely snare. If extraction gains are modest: the constraint is more coordination-focused and less purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atomization_extractiveness, empirical, 'Extractiveness gains from enforced political atomization').

omega_variable(
    reading_contest_committer,
    'Which reading of the First Amendment freedom cluster is primary: Free Speech (individual marketplace logic), Free Press (institutional gate), Free Exercise (conscience protection), Establishment (secular state), or Assembly/Petition (collective aggregation)?',
    'Jurisprudential analysis across major Supreme Court decisions: which reading''s axioms recur across cases, which are invoked to override others, which are treated as foundational vs. derivative.',
    'If Free Speech or Free Press is primary: Assembly/Petition becomes derivative (serving individual expression or press function). If Assembly/Petition is primary: other rights are seen as protecting preconditions for the fundamental act of aggregation. This is the kernel contest — sibling readings coexist but compete for primacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_committer, conceptual, 'Hierarchy and conceptual priority among First Amendment readings').

omega_variable(
    constitution_as_natural_law_false_summit,
    'Is the Assembly and Petition Clause a natural requirement of democratic systems, or a contingent institutional choice that benefits particular actors (movements, political organizations, incumbent democracies)?',
    'Cross-regime comparison: do non-democratic or authoritarian states systematically suppress assembly/petition? Are there functionally democratic systems without formal protections? Analysis of why these protections emerged historically (constitutional design choices vs. logical necessity).',
    'If natural law: mountain classification sustained; no false summit. If contingent: FSM triggers; constraint reclassifies as tangled rope or snare depending on regime context. The beneficiary and victim declarations already signal contingency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitution_as_natural_law_false_summit, conceptual, 'Whether assembly/petition rights are natural laws or contingent constitutional choices').

omega_variable(
    reading_relation_free_speech_interaction,
    'Does the Assembly/Petition reading foreclose, coexist with, or influence the Free Speech reading? Can both be held simultaneously without contradiction?',
    'Jurisprudential case analysis: instances where assembly protections conflict with free speech doctrine (e.g., permit denial on speech grounds, restrictions on expressive assembly). Identification of whether conflicts are logical (foreclosure) or pragmatic (coexistence with tension).',
    'If foreclosure: the readings are mutually exclusive foundational claims about First Amendment architecture. If coexistence: both remain live positions within liberal constitutionalism. If influence: one reading shapes the other''s scope without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_free_speech_interaction, conceptual, 'Logical and structural relationship between Assembly/Petition and Free Speech readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expression_conscience_amendments__assembly_petition_clause, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(assembly_petition_theater_t0, expression_conscience_amendments__assembly_petition_clause, theater_ratio, 0, 0.42).
narrative_ontology:measurement(assembly_petition_theater_t40, expression_conscience_amendments__assembly_petition_clause, theater_ratio, 40, 0.48).
narrative_ontology:measurement(assembly_petition_theater_t80, expression_conscience_amendments__assembly_petition_clause, theater_ratio, 80, 0.55).

% Extraction over time
narrative_ontology:measurement(assembly_petition_extractiveness_t0, expression_conscience_amendments__assembly_petition_clause, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(assembly_petition_extractiveness_t40, expression_conscience_amendments__assembly_petition_clause, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(assembly_petition_extractiveness_t80, expression_conscience_amendments__assembly_petition_clause, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(assembly_petition_suppression_t0, expression_conscience_amendments__assembly_petition_clause, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(assembly_petition_suppression_t40, expression_conscience_amendments__assembly_petition_clause, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(assembly_petition_suppression_t80, expression_conscience_amendments__assembly_petition_clause, suppression_requirement, 80, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expression_conscience_amendments__assembly_petition_clause, identity_coordination).
narrative_ontology:boltzmann_floor_override(expression_conscience_amendments__assembly_petition_clause, 0.12).
narrative_ontology:affects_constraint(expression_conscience_amendments__assembly_petition_clause, expression_conscience_amendments__free_speech_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__assembly_petition_clause, expression_conscience_amendments__free_press_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__assembly_petition_clause, expression_conscience_amendments__free_exercise_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__assembly_petition_clause, expression_conscience_amendments__establishment_clause).

% DUAL FORMULATION NOTE:
% The five First Amendment readings form a kernel cluster. Each reading is a distinct constraint with its own ε, its own beneficiary/victim structure, and its own axiomatic foundation. They are linked via network.affects_constraints because they share the contested kernel and compete for interpretive authority. The assembly_petition_clause reading instantiates aggregation logic; free_speech instantiates individual expression; free_press instantiates institutional gatekeeping; free_exercise instantiates conscience protection; establishment instantiates secular authority. Each reading produces different directionality values and different classifications for the same constitutional apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
