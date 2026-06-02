% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty: Absolute Non-Intervention Doctrine
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The absolute non-intervention doctrine claims that state territorial
 *   sovereignty is inviolable and that external interference in domestic
 *   affairs is per se illegitimate regardless of internal conduct. This is
 *   one reading of the Westphalian kernel — a reading that prioritizes state
 *   monopoly over territorial authority over any external enforcement of
 *   human rights or democratic standards. The constraint exhibits a core
 *   contradiction: it is presented as a natural law of international order
 *   (an immutable principle necessary to prevent great-power conflict) while
 *   simultaneously serving identifiable interests (state elites,
 *   authoritarian regimes, great powers seeking to protect client states).
 *   The constraint's extractiveness has risen from 0.35 (1648, when the
 *   doctrine emerged as mutual coordination among European powers) to 0.68
 *   (Cold War peak, when both superpowers used sovereignty immunity to shield
 *   client regimes from intervention pressure) to 0.58 (contemporary era,
 *   where humanitarian intervention norms have eroded but not eliminated the
 *   doctrine). Theater ratio has risen to 0.65 as the gap between doctrine
 *   (non-intervention is absolute) and practice (humanitarian intervention
 *   occurs) has widened. The doctrine is a false summit candidate: it
 *   naturalizes power asymmetry (strong states can tolerate humanitarian
 *   costs; weak states cannot resist intervention if powerful states choose
 *   it) as an unavoidable necessity. This story instantiates the absolute
 *   non-intervention reading of the sovereignty kernel, contrasting with
 *   sibling readings that make intervention conditional on internal conduct
 *   (conditional_responsibility) or that grade sovereignty by regime type
 *   (graded_sovereignty).
 *
 * KEY AGENTS:
 *   - State Elites / Authoritarian Regimes: Primary beneficiary (institutional/arbitrage) — claim absolute immunity from external pressure on domestic conduct; monopolize territorial authority without external accountability
 *   - Civilian Populations Under State Control: Primary victim (powerless/trapped) — confined to territory; cannot exit or seek external protection under non-intervention doctrine; bear full cost of atrocities
 *   - International Human Rights Bodies: Secondary victim (moderate/constrained) — cannot legitimately intervene; can only document and advocate within constrained framework; bear reputational cost of inaction
 *   - International Community / Mixed State Interests: Complex agent (organized/constrained) — collectively benefit from sovereignty norm protection (coordinates mutual non-interference) but asymmetrically benefit/suffer based on regime type
 *   - International Law Establishment: Institutional actor (institutional/arbitrage) — maintains Westphalian doctrine ceremonially despite widening gap between doctrine and practice; sees own framework as degraded piton
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law of international order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.58).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, snare).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty: Absolute Non-Intervention Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '2c5800ba-40c8-4583-b881-0e617aa13bd6').
narrative_ontology:cs_kernel_codification('2c5800ba-40c8-4583-b881-0e617aa13bd6', fixed_text).
narrative_ontology:cs_authority_grounding('2c5800ba-40c8-4583-b881-0e617aa13bd6', lineage).
narrative_ontology:cs_interpretation_layer_present('2c5800ba-40c8-4583-b881-0e617aa13bd6').
narrative_ontology:cs_reading_relation('2c5800ba-40c8-4583-b881-0e617aa13bd6', conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('2c5800ba-40c8-4583-b881-0e617aa13bd6', graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('2c5800ba-40c8-4583-b881-0e617aa13bd6', foundational, territorial_sovereignty_categorically_inviolable).
narrative_ontology:cs_axiom_status(territorial_sovereignty_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('2c5800ba-40c8-4583-b881-0e617aa13bd6', territorial_sovereignty_categorically_inviolable, conventional).
narrative_ontology:cs_axiom('2c5800ba-40c8-4583-b881-0e617aa13bd6', foundational, external_interference_per_se_illegitimate).
narrative_ontology:cs_axiom_status(external_interference_per_se_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2c5800ba-40c8-4583-b881-0e617aa13bd6', external_interference_per_se_illegitimate, deontological).
narrative_ontology:cs_reference_frame('2c5800ba-40c8-4583-b881-0e617aa13bd6', westphalian_mutual_non_interference).
narrative_ontology:cs_drift_state('2c5800ba-40c8-4583-b881-0e617aa13bd6', contemporary_humanitarian_intervention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c5800ba-40c8-4583-b881-0e617aa13bd6', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, civilian_populations_under_state_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, international_human_rights_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped within borders enforced by non-intervention doctrine. State sovereignty prohibits external intervention regardless of internal atrocity. No exit mechanism; no external advocacy claims legitimacy. Maximum experienced extraction — the doctrine itself is the suppression mechanism.
constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL HUMAN RIGHTS BODIES (SNARE) — Constrained by sovereignty doctrine from intervening directly. Can document, advocate, impose reputational cost, but cannot legitimately cross the non-intervention threshold. Extraction mechanism is the structural prohibition on enforcement power. Generational horizon reflects that human rights norms accumulate but implementation remains blocked by sovereignty immunity.
constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STATE ELITES / BENEFICIARY STATES (ROPE) — Experience the doctrine as pure coordination mechanism: mutual non-interference guarantees territorial monopoly and prevents external challenge to regime legitimacy. No extraction is experienced because the constraint coordinates mutual interest. Exit option is arbitrage — states can invoke sovereignty selectively (claim it when facing pressure, ignore it when pursuing geopolitical advantage).
constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY / MIXED STATE INTERESTS (TANGLED ROPE) — States collectively benefit from non-intervention norm (protects their own sovereignty) but also bear costs when this norm shields atrocities they morally oppose or geopolitically destabilize regions. Genuine coordination function (mutual territorial security) coupled with asymmetric extraction (authoritarian states extract immunity while democracies bear reputational cost of inaction). Constrained exit reflects that withdrawal from the sovereignty system is impossible without fundamental state system redesign.
constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LAW ESTABLISHMENT (PITON) — The Westphalian doctrine is substantially theater in contemporary practice. Humanitarian intervention occurs despite non-intervention principle (NATO/Kosovo 1999, Libya 2011, Syria R2P discourse). The doctrine persists through institutional inertia — law schools teach it, treaties invoke it, states cite it even when violating it. Theater ratio is high because the gap between doctrine and practice is structural: the principle is maintained ceremonially even as exceptions proliferate. The establishment sees its own framework as degraded but continues performing it.
constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, state territorial sovereignty appears as an immutable structural necessity of international order: any system of states requires non-interference to prevent perpetual great-power conflict. The doctrine appears as a natural law of international relations — the price of avoiding hegemonic war. However, this naturalizes what is actually a contingent institutional arrangement designed to benefit state elites. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalia_sovereignty__absolute_non_intervention, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, TR),
    TR >= 0.70.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The doctrine extracts substantial benefit for authoritarian regimes and great powers seeking to protect client states. The beneficiary group clearly extracts immunity from external accountability. However, extractiveness is not at snare maximum (0.70+) because: (1) the constraint does coordinate a genuine benefit for all states (mutual non-interference protects all sovereignties equally in theory), (2) humanitarian intervention exceptions have eroded the doctrine's totality, (3) soft-power costs to authoritarian regimes are rising. Suppression (0.72): High. The mechanism is structural: territorial borders prevent exit; the doctrine itself prohibits external rescue; regime monopoly on force prevents internal exit. Suppression is not total only because some exit pathways exist (emigration, international asylum) but these are costly and limited. Theater ratio (0.65): Moderate-high. The gap between the absolute doctrine and conditional practice is substantial. NATO intervenes in Kosovo (1999) and Libya (2011) despite non-intervention principle. Responsibility to Protect doctrine explicitly permits intervention under R2P; humanitarian exemptions populate international law. Yet states still cite Westphalian principle, and academic debate still treats non-intervention as foundational. The theater is the maintenance of the absolute principle despite systematic violation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. State elites see pure coordination (Rope) — the doctrine mutually guarantees their territorial monopoly. Trapped populations see pure extraction (Snare) — the doctrine itself is the suppression mechanism preventing external rescue. The international community sees mixed experience (Tangled Rope) — genuine coordination benefits coupled with asymmetric extraction favoring authoritarian regimes. The law establishment sees degraded theater (Piton) — the doctrine persists through inertia despite systematic exceptions. The analytical observer risks seeing natural law (Mountain) — non-intervention appears as an immutable requirement of state system stability. The gap reveals that the constraint is not a neutral principle but a power-asymmetric arrangement naturalized as necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim structure. State elites are beneficiaries with arbitrage exit (they can invoke sovereignty selectively, respecting it when convenient, ignoring it when geopolitically advantageous). This produces low d and negative/minimal chi — they experience the constraint as coordination benefit, not extraction. Civilian populations are trapped victims with no exit — high d, maximum f(d), high chi. International human rights bodies are moderately constrained victims — they can advocate and document but cannot intervene directly; moderate d, moderate chi. The mixed state interests agent faces both beneficiary and victim positions depending on regime type — balanced but asymmetric experience produces tangled_rope classification. The international law establishment has arbitrage exit (they maintain the doctrine through institutional inertia despite eroding legitimacy) and institutional power — low d, piton classification from theater gate rather than chi gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that absolute non-intervention is a reading of a contested kernel, not a universal principle. The three readings of the Westphalian kernel are: (1) absolute non-intervention (this constraint) — sovereignty is categorically inviolable, (2) conditional responsibility (sibling) — intervention is permitted when internal conduct exceeds atrocity threshold, (3) graded sovereignty (sibling) — intervention rights depend on regime type (democracies have greater immunity). These readings coexist in contemporary international law; none forecloses the others. The mandatrophy emerges from asking 'which reading is correct?' as if sovereignty has a single fixed meaning. The framework's answer is: all three are structurally real, each valid from its own perspective. Absolute non-intervention is the reading that naturalizes power asymmetry most effectively (false summit). Conditional responsibility erodes beneficiary immunity. Graded sovereignty explicitly ties intervention legitimacy to regime type. The constraint story's role is to make explicit what absolute non-intervention naturalizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_definition,
    'What magnitude or type of internal state conduct justifies external intervention, and who decides this threshold?',
    'Comparative case analysis of interventions (Kosovo, Iraq, Libya, Syria); identification of whether threshold criteria are applied consistently or selectively based on intervener interests',
    'If threshold is absolute/clear: doctrine is coherent mountain (immutable principle). If threshold is contested/applied selectively: doctrine is contingent snare (immunity for powerful states, vulnerability for weak ones).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_threshold_definition, conceptual, 'Definition and application of atrocity threshold for intervention legitimacy').

omega_variable(
    reading_vs_doctrine_gap,
    'Is absolute non-intervention an accurate description of contemporary international law, or is it a normative reading of the Westphalian principle that contemporary practice contradicts?',
    'Survey of humanitarian intervention doctrine (Responsibility to Protect, UNSC precedents, humanitarian exemptions); analysis of gap between Charter text and state practice',
    'If doctrine describes practice: this constraint story is accurate. If doctrine diverges from practice: this story describes a reading that has been substantially overridden by emergence of conditional intervention norms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_doctrine_gap, empirical, 'Whether absolute non-intervention reflects current international law or a contested reading').

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the Westphalian kernel (absolute non-intervention) or a distinct constraint altogether (sovereignty doctrine per se)?',
    'Comparison with sibling readings (conditional_responsibility: intervention is permitted under R2P; graded_sovereignty: intervention rights depend on regime type). If this reading forecloses the siblings, it is the same kernel read absolutely. If siblings coexist, kernel is contested.',
    'If same kernel: this story documents one legitimate reading among contested alternatives. If distinct constraint: this story documents a historical doctrine that has been superseded by newer normative frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is a reading of a contested kernel or a superseded doctrine').

omega_variable(
    beneficiary_identification_accuracy,
    'Who actually benefits from the absolute non-intervention doctrine in contemporary practice?',
    'Network analysis of which states invoke sovereignty most frequently; correlation between invocation frequency and regime type; analysis of which states benefit from non-intervention immunity vs which bear costs of deference to it',
    'If authoritarian regimes + great powers benefit: beneficiary list is accurate, snare classification confirmed. If distribution is more symmetric: constraint may be rope rather than snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_accuracy, empirical, 'Beneficiary distribution in contemporary sovereignty doctrine practice').

omega_variable(
    false_summit_candidacy,
    'Is the absolute non-intervention doctrine presented as natural law when it is actually a contingent institutional arrangement that benefits identifiable elites?',
    'Historical analysis of doctrine emergence (Treaty of Westphalia 1648, Peace of Westphalia intent); comparison with alternative possible doctrines (conditional intervention, graded sovereignty, universal human rights). If doctrine serves identifiable interests and was consciously constructed, it is false summit candidate.',
    'If false summit confirmed: non-intervention doctrine naturalizes power asymmetry. If genuine natural law: doctrine represents optimal solution to coordination problem among states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidacy, conceptual, 'Whether absolute non-intervention is natural law or naturalized institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 375).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_theater_1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.4).
narrative_ontology:measurement(westphalia_theater_1798, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 150, 0.45).
narrative_ontology:measurement(westphalia_theater_1948, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 300, 0.5).
narrative_ontology:measurement(westphalia_theater_1998, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 350, 0.65).

% Extraction over time
narrative_ontology:measurement(westphalia_extractiveness_1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(westphalia_extractiveness_1798, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(westphalia_be_t300, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 300, 0.68).
narrative_ontology:measurement(westphalia_extractiveness_1998, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 350, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, responsibility_to_protect_framework).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, state_system_great_power_stability).

% DUAL FORMULATION NOTE:
% The absolute non-intervention reading is upstream of contemporary humanitarian intervention doctrine and R2P framework. The genealogy: Westphalian mutual non-interference (1648) → non-intervention principle as international law (1800s-1900s) → Cold War sovereignty weaponization → humanitarian intervention exceptions (1990s) → R2P framework (2000s). This story documents the reading that downstream revisions have eroded. The sibling readings (conditional_responsibility, graded_sovereignty) represent the downstream normative framework that competes with absolute non-intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
