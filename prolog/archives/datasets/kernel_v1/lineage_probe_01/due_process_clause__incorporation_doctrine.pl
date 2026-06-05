% ============================================================================
% CONSTRAINT STORY: due_process_clause__incorporation_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_due_process_clause__incorporation_doctrine, []).

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
 *   constraint_id: due_process_clause__incorporation_doctrine
 *   human_readable: Incorporation Doctrine: Due Process as Conduit for Bill of Rights Binding on States
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The incorporation doctrine is the constitutional mechanism by which the
 *   Due Process Clause of the Fourteenth Amendment makes the enumerated
 *   protections of the Bill of Rights binding on state governments. Before
 *   incorporation (1868–1925), states were exempt from federal constitutional
 *   constraints on their criminal procedures; the Bill of Rights applied only
 *   to the federal government. Incorporation gradually extended these
 *   protections to the states, creating a federalized constitutional floor
 *   for criminal procedure. This constraint exemplifies tangled_rope
 *   classification: it serves a genuine coordination function (establishing
 *   uniform constitutional baselines across the federation), but this
 *   function is entangled with asymmetric extraction (suppression of state
 *   procedural autonomy). The doctrine is one reading of the Fourteenth
 *   Amendment's due process clause, competing with procedural_due_process
 *   (which emphasizes the process-centered rather than incorporation-centered
 *   interpretation) and substantive_due_process (which emphasizes that some
 *   liberties are fundamentally protected, not merely procedurally
 *   safeguarded). This story instantiates the incorporation_doctrine reading:
 *   due process is the vehicle through which the Bill of Rights binds states,
 *   not merely a guarantee of fair procedure or fundamental liberty
 *   protection.
 *
 * KEY AGENTS:
 *   - Individual Rights-Holders Against States: Primary beneficiary (powerless/trapped pre-incorporation, now moderate/constrained) — incorporation suppresses state exemption and provides federal constitutional floor
 *   - State Criminal Justice Systems: Primary victim of suppression (institutional/constrained) — lose autonomy to design criminal procedures; extraction occurs through mandatory federal constitutional compliance
 *   - Federal Judicial System: Secondary beneficiary (institutional/arbitrage) — gains interpretive authority and appellate jurisdiction through incorporation doctrine
 *   - Civil Rights Organizations: Organized beneficiary (organized/constrained) — benefit from uniform national floor but constrained by federal judicial monopoly on constitutional interpretation
 *   - Federalism Advocates / States' Rights: Secondary victim (institutional/arbitrage) — structural state sovereignty is suppressed by federal incorporation; states lose the exit option of exemption
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing incorporation as inevitable federalism solution when alternatives (state constitutions, interstate compacts) exist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(due_process_clause__incorporation_doctrine, 0.38).
domain_priors:suppression_score(due_process_clause__incorporation_doctrine, 0.62).
domain_priors:theater_ratio(due_process_clause__incorporation_doctrine, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(due_process_clause__incorporation_doctrine, extractiveness, 0.38).
narrative_ontology:constraint_metric(due_process_clause__incorporation_doctrine, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(due_process_clause__incorporation_doctrine, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(due_process_clause__incorporation_doctrine, tangled_rope).
narrative_ontology:human_readable(due_process_clause__incorporation_doctrine, "Incorporation Doctrine: Due Process as Conduit for Bill of Rights Binding on States").
narrative_ontology:topic_domain(due_process_clause__incorporation_doctrine, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(due_process_clause__incorporation_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(due_process_clause__incorporation_doctrine, 'ffe4225c-0b09-465f-8fd4-74fdf99d878f').
narrative_ontology:cs_kernel_codification('ffe4225c-0b09-465f-8fd4-74fdf99d878f', fixed_text).
narrative_ontology:cs_authority_grounding('ffe4225c-0b09-465f-8fd4-74fdf99d878f', extraction).
narrative_ontology:cs_interpretation_layer_present('ffe4225c-0b09-465f-8fd4-74fdf99d878f').
narrative_ontology:cs_reading_relation('ffe4225c-0b09-465f-8fd4-74fdf99d878f', due_process_clause__procedural_due_process, coexists_with).
narrative_ontology:cs_reading_relation('ffe4225c-0b09-465f-8fd4-74fdf99d878f', due_process_clause__substantive_due_process, influences).
narrative_ontology:cs_axiom('ffe4225c-0b09-465f-8fd4-74fdf99d878f', foundational, due_process_incorporates_enumerated_rights).
narrative_ontology:cs_axiom_status(due_process_incorporates_enumerated_rights, holdable).
narrative_ontology:cs_axiom_grounding('ffe4225c-0b09-465f-8fd4-74fdf99d878f', due_process_incorporates_enumerated_rights, empirically_contingent).
narrative_ontology:cs_axiom('ffe4225c-0b09-465f-8fd4-74fdf99d878f', foundational, federal_constitutional_floor_coordinates_federalism).
narrative_ontology:cs_axiom_status(federal_constitutional_floor_coordinates_federalism, holdable).
narrative_ontology:cs_axiom_grounding('ffe4225c-0b09-465f-8fd4-74fdf99d878f', federal_constitutional_floor_coordinates_federalism, instrumental).
narrative_ontology:cs_reference_frame('ffe4225c-0b09-465f-8fd4-74fdf99d878f', state_constitutional_autonomy).
narrative_ontology:cs_drift_state('ffe4225c-0b09-465f-8fd4-74fdf99d878f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffe4225c-0b09-465f-8fd4-74fdf99d878f', '').
narrative_ontology:cs_kernel_id(due_process_clause__incorporation_doctrine, due_process_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(due_process_clause__incorporation_doctrine, individual_rights_holders_against_states).
narrative_ontology:constraint_victim(due_process_clause__incorporation_doctrine, state_criminal_procedure_autonomy).
narrative_ontology:constraint_victim(due_process_clause__incorporation_doctrine, federalist_state_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ACCUSED IN STATE COURT (SNARE) — Before incorporation, state defendants had no federal constitutional floor. The state criminal procedure system extracted maximal discretion with minimal oversight. Individual defendants are structurally trapped — no exit from state jurisdiction, no alternative forum, no federal constitutional recourse. The incorporation doctrine suppresses the alternative of state exemption, forcing states to provide minimum protections. From this perspective, incorporation is liberation, but the prior regime was pure snare.
constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE CRIMINAL JUSTICE SYSTEM (TANGLED ROPE) — States benefit from the incorporation doctrine's coordination function: it provides a clear, unified constitutional floor across the federation, enabling predictable interstate legal commerce and reducing forum-shopping incentives. But incorporation also extracts autonomy: states cannot design criminal procedures optimized for their unique legal cultures, demographics, or constitutional traditions. They bear the cost of federal oversight without choosing the federal rules. This is tangled — genuine coordination (unified standards) layered with asymmetric extraction (suppression of state design autonomy).
constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIAL SYSTEM (ROPE) — Incorporation expands federal jurisdiction and prestige; federal courts become the final arbiter of state criminal procedure constitutionality. The federal judiciary experiences incorporation as coordination: defining the floor of rights enables vertical integration of the constitutional order. The federal courts have exit options (they could decline habeas review, could narrow incorporation) and exercise them strategically. Net beneficiary in institutional standing and interpretive authority.
constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized rights advocates see incorporation as coordination: establishing uniform federal baselines enables nationwide advocacy and prevents jurisdictional arbitrage by governments. But the doctrine also imposes costs: it routes all disputes through federal courts, creating a monopoly on constitutional interpretation and making reform dependent on judicial goodwill. Exit options are constrained — advocates cannot simply appeal to state constitutions (though some do) without losing the national coordination benefit. Mixed experience: genuine benefit (enforcement floor) with real constraint (doctrinal monopoly).
constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FOURTEENTH AMENDMENT APPARATUS (PITON) — The incorporation doctrine is historically and doctrinally degraded. The 14th Amendment's text does not explicitly incorporate the Bill of Rights; incorporation is a judge-made doctrine that rests on selective history (the incorporation debates were contested; historical evidence is ambiguous). The doctrine persists through institutional inertia and precedent stare decisis, not because the textual or historical case is strong. Theater ratio is moderate-high: the Supreme Court performs constitutional interpretation while the actual driver is institutional path-dependence. Incorporation is the doctrine everyone uses but few scholars defend as originalist or textualist on merits.
constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FEDERALISM-RIGHTS TENSION (MOUNTAIN) — From the civilizational view, federalism and individual rights are in structural tension: you cannot have unlimited state sovereignty and absolute individual rights simultaneously. Some mechanism must mediate between them. Incorporation is one such mechanism, but it is not inevitable — one could imagine alternatives (state bills of rights, interstate compacts, federal conditional spending). The mountain classification reflects the view that SOME mechanism of rights-federalism mediation is inescapable, not that incorporation specifically is. However, the structural data (active enforcement, extracted state autonomy, alternative mechanisms available) contradicts the mountain gate — this is a false summit naturalizing a contingent choice.
constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(due_process_clause__incorporation_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(due_process_clause__incorporation_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(due_process_clause__incorporation_doctrine, TR),
    TR >= 0.70.

:- end_tests(due_process_clause__incorporation_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The incorporation doctrine extracts state autonomy in criminal procedure design — states cannot opt out of federal constitutional requirements, and the cost of federal compliance is non-trivial. However, the extraction is not maximal because: (1) incorporation provides coordination benefits (unified standards reduce federalism tensions), (2) states retain autonomy in procedural details above the federal floor, and (3) the doctrine has been applied selectively (some guarantees incorporated, others not). The metric rises over time (0.15 → 0.38) reflecting that as incorporation has been filled in with more specific doctrinal content, the extraction has become more granular and demanding. Suppression (0.62): High. States cannot exit incorporation; they cannot choose to be exempt from federal constitutional floor; the only exit option is constitutional amendment (effectively impossible). The suppression is structural — not merely costly but legally impossible to avoid. States have constrained exit, not trapped exit, because they can attempt doctrinal resistance, can appeal for federal clarification, or can implement the floor at higher stringency than required. Theater ratio (0.48): Moderate. The Supreme Court's incorporation decisions are partially performative (the historical arguments are weak; the doctrine is judge-made despite textualist claims), but the actual enforcement mechanism is not purely theatrical. Federal habeas review and appellate reversal of state convictions have real bite. The doctrine is not a piton (theater_ratio < 0.7) because the enforcement is substantial, even if the justification is somewhat theatricalized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects the classic federalism divide. The individual accused in state court experiences incorporation as liberation (snare → constrained); the state criminal justice system experiences it as extraction (autonomy loss). The federal judiciary experiences it as coordination (unified standards, expanded authority). Civil rights advocates experience mixed effects: genuine benefit (floor) but also constraint (monopolized interpretation). The analytical observer risks a false summit by naturalizing what is a contingent constitutional choice. The piton perspective reveals that incorporation persists through precedent and inertia despite weak original historical support — the doctrine works but is doctrinally degraded. The perspectival gap is widest between the powerless (who gain protection) and the states (who lose autonomy), and between the federal judiciary (who gain authority) and state courts (who lose interpretive discretion).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in each perspective is derived from the agent's structural relationship to incorporation. Individual defendants are beneficiaries with trapped exit (pre-incorporation) or constrained exit (post-incorporation) — high d. States are victims with constrained exit (they must comply but can design details) — moderate-high d. Federal courts are beneficiaries with arbitrage exit (they can choose how strictly to enforce) — low d. Civil rights advocates are beneficiary-beneficiary hybrids (they benefit from the floor but are constrained by federal monopoly) — moderate d. The piton perspective has arbitrage exit (the institution persists because it has institutional inertia, not because enforcement is impossible) — d is moderate but theater_ratio is high. The analytical observer's d is canonical for analytical contexts (~0.73) but the false summit signals that mountain classification is perspectival rather than structural. The incorporation doctrine suppresses state exit and benefits individual rights-holders, creating the asymmetric d profile that drives tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The incorporation doctrine resolves mandatrophy by being a genuine tangled_rope: it coordinates a federalism problem (how to apply constitutional rights across a federal system) while extracting state procedural autonomy. The doctrine is not pure coordination (Rope) because extraction is real and substantial; it is not pure extraction (Snare) because coordination benefits are genuine and measurable. The piton perspective (institutional/arbitrage/civilizational) reveals that the doctrine is historically degraded — the textual and historical case is weak, the doctrine persists through precedent inertia rather than strong justification. The mountain perspective is a false summit: incorporation naturalizes a contingent constitutional choice as an inevitable feature of federalism structure. The mandatrophy resolves by showing that integration of rights and federalism requires some mechanism, but incorporation is one choice among several (state bills of rights, interstate compacts, federal conditional spending). The doctrine's strength lies in coordination and institutional lock-in, not in inevitable necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incorporation_historical_support,
    'Did the framers of the Fourteenth Amendment intend to incorporate the Bill of Rights against the states, or is incorporation a modern judicial creation?',
    'Original public meaning analysis of Section 1 of the Fourteenth Amendment; historical legislative records; contemporaneous state constitutional debates; comparison with explicit incorporation mechanisms in other constitutional democracies',
    'If incorporation was intended: doctrine is a faithful reading of constitutional text (mountain candidate). If modern creation: doctrine is judge-made law with contingent legitimacy (tangled rope). If ambiguous: kernels shift — procedural_due_process reading becomes stronger relative to incorporation_doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incorporation_historical_support, empirical, 'Historical support for incorporation as intended vs. modern judicial creation').

omega_variable(
    state_constitutional_sufficiency,
    'Can state constitutions and state bills of rights provide adequate protection without federal incorporation, or does federalism create systematic rights deficits?',
    'Comparative study of state constitutional doctrine before and after incorporation; analysis of which states had equivalent or superior protections pre-incorporation; measurement of post-incorporation doctrinal convergence; empirical analysis of rights outcomes in states that have independent state constitutional protections',
    'If state protections were sufficient: incorporation was coordination (Rope) rather than extraction (Tangled Rope), and the victim set is misidentified. If state protections were systematically inadequate: incorporation suppresses a real federalism gap, supporting Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_constitutional_sufficiency, empirical, 'Whether state constitutional mechanisms provide adequate protection absent federal incorporation').

omega_variable(
    incorporation_scope_and_selectivity,
    'Why does incorporation apply some guarantees (5th Amendment self-incrimination, 6th Amendment trial rights) but not others (7th Amendment jury trial in civil cases, 3rd Amendment quartering)? Is this doctrinally coherent or contingent?',
    'Doctrine-internal analysis of the ''fundamental to our scheme of ordered liberty'' standard; historical tracking of which guarantees were incorporated when; analysis of whether the exclusions reflect principled federalism doctrine or historical accident',
    'If selective incorporation is principled: the doctrine has internal integrity (supported by Tangled Rope + piton reading of implementation). If selective and arbitrary: incorporation is performative and degraded (piton). If the exclusions are indefensible: the doctrine is itself captured by path-dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incorporation_scope_and_selectivity, conceptual, 'Whether selective incorporation reflects principled doctrine or historical contingency').

omega_variable(
    competing_doctrinal_readings,
    'Are incorporation_doctrine, procedural_due_process, and substantive_due_process logically compatible or mutually exclusive readings of the Fourteenth Amendment?',
    'Doctrinal mapping of how each reading treats the same textual and case law material; identification of core premises that would be violated if alternative readings were adopted; analysis of whether Supreme Court doctrine uses all three simultaneously (indicating coexistence) or whether adoption of one forecloses the others',
    'If compatible: all three readings remain live (coexists_with relations valid). If one forecloses another: reading_relations should reflect foreclosure. If the Court uses all three, it may be doctrinally incoherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_doctrinal_readings, conceptual, 'Logical compatibility of incorporation vs procedural vs substantive due process readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of state criminal procedure autonomy a necessary structural feature (states cannot opt out of federal constitutional baseline), a contingent policy choice (incorporation could be decoupled with alternative federalism mechanisms), or a strategic institutional lock-in (the federal judiciary suppresses state alternatives because it benefits from monopolized constitutional authority)?',
    'Comparative analysis of other federal systems and their mechanisms for coordinating rights and federalism (Canada''s Charter, Australia''s Commonwealth-State interaction, EU fundamental rights); historical analysis of whether pre-incorporation state alternatives were systematically suppressed or merely underused; institutional analysis of federal judicial incentives',
    'If necessary: suppression is part of the structure (Tangled Rope + Mountain false summit). If contingent and better alternatives exist: suppression is institutional extraction (Snare or higher-chi Tangled Rope). If lock-in: incorporation is a piton maintained by federal judicial inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Whether suppression of state autonomy is structural necessity vs contingent choice vs institutional lock-in').

omega_variable(
    incorporation_reading_vs_siblings,
    'This constraint is ONE reading of the Fourteenth Amendment''s due process clause. How does the incorporation_doctrine reading relate to the procedural_due_process and substantive_due_process readings — does it foreclose them, coexist with them, or create structural pressure on them?',
    'Doctrinal analysis showing how Supreme Court uses all three concepts simultaneously; identification of whether any reading is logically incompatible with the others; mapping of whether one reading (e.g., incorporation) assumes or depends on assumptions from the other readings (e.g., that some processes are fundamentally inadequate, suggesting substantive content limits)',
    'Determines reading_relations in cs_structure: if incorporation forecloses substantive due process, the relation is forecloses (rare). If all three coexist in contemporary doctrine despite underlying tensions, the relation is coexists_with. If incorporation creates institutional pressure that changes how the siblings operate, the relation is influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incorporation_reading_vs_siblings, conceptual, 'Doctrinal relationship between incorporation_doctrine and its sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(due_process_clause__incorporation_doctrine, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duep_incorp_theater_t0, due_process_clause__incorporation_doctrine, theater_ratio, 0, 0.35).
narrative_ontology:measurement(duep_incorp_theater_t50, due_process_clause__incorporation_doctrine, theater_ratio, 50, 0.42).
narrative_ontology:measurement(duep_incorp_theater_t100, due_process_clause__incorporation_doctrine, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(duep_incorp_extract_t0, due_process_clause__incorporation_doctrine, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(duep_incorp_extract_t50, due_process_clause__incorporation_doctrine, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(duep_incorp_extract_t100, due_process_clause__incorporation_doctrine, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(duep_incorp_suppress_t0, due_process_clause__incorporation_doctrine, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(duep_incorp_suppress_t50, due_process_clause__incorporation_doctrine, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(duep_incorp_suppress_t100, due_process_clause__incorporation_doctrine, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(due_process_clause__incorporation_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(due_process_clause__incorporation_doctrine, due_process_clause__procedural_due_process).
narrative_ontology:affects_constraint(due_process_clause__incorporation_doctrine, due_process_clause__substantive_due_process).
narrative_ontology:affects_constraint(due_process_clause__incorporation_doctrine, federalism_individual_rights_tension).
narrative_ontology:affects_constraint(due_process_clause__incorporation_doctrine, state_criminal_procedure_autonomy).

% DUAL FORMULATION NOTE:
% The incorporation_doctrine constraint is one reading of the due_process_clause kernel. The sibling readings (procedural_due_process, substantive_due_process) are separate constraints because they focus on different structural aspects of due process — the vehicle/mechanism (incorporation) vs the substance (procedure or liberty). All three readings share the same Fourteenth Amendment text but decompose it differently. The incorporation_doctrine reading's ε (0.38) reflects the mixed coordination-extraction profile; sibling readings would have different ε values reflecting their different structural content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(due_process_clause__incorporation_doctrine, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
