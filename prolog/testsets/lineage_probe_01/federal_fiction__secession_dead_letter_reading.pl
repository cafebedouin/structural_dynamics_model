% ============================================================================
% CONSTRAINT STORY: federal_fiction__secession_dead_letter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_fiction__secession_dead_letter_reading, []).

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
 *   constraint_id: federal_fiction__secession_dead_letter_reading
 *   human_readable: Article 17 Secession Right as Dead Letter in Soviet Federation
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   The Soviet federal constitution granted republics the explicit right to
 *   secede (Article 17), yet invoking this right was criminalized as
 *   counter-revolutionary agitation. This dead letter transforms the written
 *   constitution into a trap: the text grants rights whose assertion is the
 *   crime. The constraint exhibits the structure of a snare disguised as
 *   coordination. For unitary state control, the federal form (with its dead
 *   letter secession clause) functions as rope—a coordination mechanism that
 *   preserves the appearance of multinational union while guaranteeing that
 *   actual invocation of exit rights triggers enforcement. For national
 *   movements, the same constraint is pure snare: the written right becomes
 *   entrapment. The historical arc from 1922 (early Soviet, relatively
 *   functional federal procedures) through 1937 (Stalinist consolidation,
 *   suppression intensifies) to 1960-1991 (normalized terror, federal form
 *   increasingly theatrical) shows measurable degradation from tangled rope
 *   toward snare. By the late Soviet period, the federal apparatus persists
 *   as piton—institutional theater performing the coordination it no longer
 *   functionally achieves.
 *
 * KEY AGENTS:
 *   - Unitary State Control Apparatus (institutional/arbitrage): Primary beneficiary—extracts legitimacy (federal appearance) while central ministries execute unified policy. Federal form with dead letter secession clause enables this coordination-as-cover.
 *   - National Movements (powerless/trapped): Primary victims—invoke Article 17 as perceived constitutional right, triggering criminalization. The constraint converts political dissent into actionable conspiracy.
 *   - Titular Republic Elites (moderate/constrained): Secondary victims—possess nominal authority and federal form privileges, yet exercise of constitutional rights activates suppression. Caught between cosmetic power and criminalized assertion.
 *   - Soviet Ideological Authority (powerful/arbitrage): Institutional actor enforcing suppression as protection of proletarian internationalism—frames counter-revolutionary agitation charges as legitimate enforcement.
 *   - Federal Form Apparatus (institutional/arbitrage): Institutional structure itself—initially functional (coordinating genuine multinational union), increasingly theatrical by late Soviet period.
 *   - Analytical Observer (analytical/analytical): Reveals the snare structure at civilizational scale—constitutionally guaranteed rights whose invocation is criminal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_fiction__secession_dead_letter_reading, 0.68).
domain_priors:suppression_score(federal_fiction__secession_dead_letter_reading, 0.82).
domain_priors:theater_ratio(federal_fiction__secession_dead_letter_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_fiction__secession_dead_letter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federal_fiction__secession_dead_letter_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(federal_fiction__secession_dead_letter_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_fiction__secession_dead_letter_reading, snare).
narrative_ontology:human_readable(federal_fiction__secession_dead_letter_reading, "Article 17 Secession Right as Dead Letter in Soviet Federation").
narrative_ontology:topic_domain(federal_fiction__secession_dead_letter_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(federal_fiction__secession_dead_letter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_fiction__secession_dead_letter_reading, '8bbf92ea-0eba-4e02-af11-a41508209eee').
narrative_ontology:cs_kernel_codification('8bbf92ea-0eba-4e02-af11-a41508209eee', formalized).
narrative_ontology:cs_authority_grounding('8bbf92ea-0eba-4e02-af11-a41508209eee', extraction).
narrative_ontology:cs_interpretation_layer_present('8bbf92ea-0eba-4e02-af11-a41508209eee').
narrative_ontology:cs_reading_relation('8bbf92ea-0eba-4e02-af11-a41508209eee', federal_fiction__centralized_reality_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bbf92ea-0eba-4e02-af11-a41508209eee', federal_fiction__nationality_form_reading, influences).
narrative_ontology:cs_axiom('8bbf92ea-0eba-4e02-af11-a41508209eee', foundational, constitutional_text_as_snare).
narrative_ontology:cs_axiom_status(constitutional_text_as_snare, holdable).
narrative_ontology:cs_axiom_grounding('8bbf92ea-0eba-4e02-af11-a41508209eee', constitutional_text_as_snare, empirically_contingent).
narrative_ontology:cs_axiom('8bbf92ea-0eba-4e02-af11-a41508209eee', foundational, exit_prohibition_through_legal_assertion).
narrative_ontology:cs_axiom_status(exit_prohibition_through_legal_assertion, holdable).
narrative_ontology:cs_axiom_grounding('8bbf92ea-0eba-4e02-af11-a41508209eee', exit_prohibition_through_legal_assertion, deontological).
narrative_ontology:cs_reference_frame('8bbf92ea-0eba-4e02-af11-a41508209eee', federal_socialism_with_actual_republic_exit_rights).
narrative_ontology:cs_drift_state('8bbf92ea-0eba-4e02-af11-a41508209eee', stalinist_consolidation_through_late_soviet, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('8bbf92ea-0eba-4e02-af11-a41508209eee', '').
narrative_ontology:cs_kernel_id(federal_fiction__secession_dead_letter_reading, federal_fiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_fiction__secession_dead_letter_reading, unitary_state_control).
narrative_ontology:constraint_victim(federal_fiction__secession_dead_letter_reading, national_movements).
narrative_ontology:constraint_victim(federal_fiction__secession_dead_letter_reading, subnational_republics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATIONAL MOVEMENT (SNARE) — Trapped by the written constitutional right to secede that carries the structural consequence of criminalization. Invoking Article 17 is itself the crime — the clause exists to permit exit but its assertion is suppressed as counter-revolutionary agitation. Maximum experienced extraction: the written right is a trap that transforms legitimate political dissent into criminal conspiracy.
constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TITULAR REPUBLIC ELITE (SNARE) — Constrained by the paradox of federal form: republics have nominal sovereignty, titular nationality rights, and constitutional secession clauses, yet exercise of these rights triggers criminal charges. Leadership faces choice between cosmetic authority (safe but powerless) or constitutional assertion (criminalized). High suppression, no exit.
constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITARY STATE CONTROL (ROPE) — Primary beneficiary. Experiences the constraint as a coordination mechanism: the federal form creates decorative borders that organize cultural and administrative space while central ministries execute unified policy. The dead letter secession clause is a functional element of this coordination—it permits nominal federal structure while guaranteeing that actual invocation triggers suppression. Net beneficiary: unitary control extracts legitimacy (federal appearance) and administrative capacity (centralized command) without risk of fragmentation.
constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET IDEOLOGICAL AUTHORITY (TANGLED ROPE) — Exercises real coordination function (binding multinational unity under proletarian internationalism) while extracting through suppression of nationalist exit claims. Sees the constraint as both genuine federation (coordination of national republics toward historical purpose) and necessary enforcement (counter-revolutionary agitation cannot be tolerated). Active enforcement required; suppression framed as protection of collective good.
constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERAL FORM AS PITON (DEGRADED COORDINATION) — The federal apparatus itself—titular republics, national soviets, constitutional clauses—increasingly functions as theater by the 1970s-80s. The form persists through institutional inertia: federal structure was once functional (organizing genuinely multinational union), but the central command economy has rendered federal decision-making performative. Theater ratio rises as real policy flows through unitary channels while federal organs conduct rituals of consultation.
constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational view: the constitutional text guarantees a right whose invocation is criminalized. This is pure extraction disguised as law. The federation is a structural snare: it grants formal rights to exit that function as a trap, converting political legitimate dissent into actionable conspiracy. The suppression mechanism is built into the legal text itself—the article exists precisely to transform national movements into criminals.
constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_fiction__secession_dead_letter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_fiction__secession_dead_letter_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_fiction__secession_dead_letter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_fiction__secession_dead_letter_reading, TR),
    TR >= 0.70.

:- end_tests(federal_fiction__secession_dead_letter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting the systematic conversion of constitutional rights into criminal liability. The unitary state extracts political control (national movements cannot exit) and legitimacy (federal appearance without federal reality). Measurement trajectory shows extraction rising from 0.28 (early Soviet, some functional federal procedures) to 0.68 (Stalinist through late Soviet, normalized suppression). Suppression (0.82): Very high. The dead letter mechanism suppresses exit through multiple channels: criminalization of invocation, removal of nationalist cadres, propaganda against separatism, and coordination of central security forces. Suppression is not merely external barriers but written into constitutional text—the right exists precisely to criminalize its assertion. Theater ratio (0.65): Moderate-high and rising. Early Soviet federal organs had some genuine policy-setting function; by 1960-1991, federal structures conduct ritual consultation while unitary command channels execute actual policy. The federal form persists through institutional inertia, not functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence. Unitary control sees rope (coordination mechanism preserving federation while centralizing power). National movements see snare (written right that transforms dissent into crime). Titular elites see tangled rope (hybrid coordination and suppression). Analytical observer sees snare (structural trap). The gap reveals that the same constitutional mechanism serves incompatible functions: it legitimates the state (federal appearance) while criminalizing exit (snare enforcement). The dead letter structure is the entire point—Article 17 exists to be violated, not invoked.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position. Unitary control benefits from the constraint (low d, negative effective extraction chi) because the federal form legitimates central authority while dead letter suppression prevents fragmentation. National movements are full targets (high d, maximum chi) because the constraint grants rights whose invocation triggers criminal liability. Titular elites are partial targets (moderate-high d) because they possess nominal authority but face suppression upon constitutional assertion. The snare classification is stable across all non-beneficiary perspectives because the extraction mechanism is intrinsic to the constitutional text itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy through kernel specificity. The constraint is not 'is the Soviet federation snare or rope?' but 'what does Article 17's dead letter status tell us about the constitutional order?' The snare classification is correct FOR THIS READING because the measurement focuses on the secession right and its criminalization. Alternative readings would measure different structural elements (central ministries for centralized_reality; titular institution functions for nationality_form) and produce different classifications. The mandatrophy is resolved by acknowledging that all three readings are legitimate interpretations of the contested kernel, each extracting different structural facts from the same constitutional text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_sham_vs_functional_hybrid,
    'Was Article 17 a deliberate constitutional sham (dead letter by design) or a hybrid mechanism that functioned differently at different phases of Soviet development?',
    'Historical analysis of 1922-1991 secession invocations: Were early republic formations facilitated by Article 17 procedures? When did the clause transition from procedural to criminalized? Comparison of Lenin-era constitutional enforcement vs Stalin-era enforcement vs late-Soviet period.',
    'If sham: extractiveness remains at 0.68 (pure snare design). If hybrid: extractiveness was lower pre-1930s (when some exit procedures functioned) and increased monotonically with Stalinization, suggesting a degradation trajectory rather than structural snare from inception.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_sham_vs_functional_hybrid, empirical, 'Whether Article 17 was dead letter by design or degraded over time').

omega_variable(
    nationalist_invocation_as_genuine_exit_attempt,
    'When Baltic, Caucasian, or Central Asian movements cited Article 17, were they invoking a perceived legal right or performing a rhetorical gesture they knew was criminalized?',
    'Archival analysis of movement statements, legal briefs, and internal communications: Do they cite Article 17 as a claimed legal basis expecting compliance, or invoke it knowing criminalization follows? Distinction between good-faith constitutional argument and strategic rhetorical invocation.',
    'If good-faith: victims genuinely believed the constitutional text granted rights; the trap was cognitive. If strategic: movements knowingly invoked the clause as a symbolic assertion against suppression; the constraint''s extractiveness is lower (movements had agency, accepted the risk). If mixed: different movements had different epistemic positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_invocation_as_genuine_exit_attempt, empirical, 'Whether Article 17 citations were good-faith legal claims or strategic rhetoric').

omega_variable(
    federal_form_legitimacy_claim_substance,
    'Did the federal form''s legitimacy claim—that the union truly coordinated distinct national republics—have any real institutional substance, or was it purely theatrical cover for unitary command?',
    'Institutional analysis: Did federal organs (Supreme Soviet of RSFSR, Ukraine, etc.) exercise any independent policy-setting authority? Did titular republic leadership influence central ministries? Did linguistic and cultural policies reflect republic preferences or central directives? Comparison with working federations (Yugoslavia, Czechoslovakia pre-1968).',
    'If purely theatrical: beneficiary (unitary control) experiences rope only because extraction mechanism is disguised. Constraint is snare with performative coordination layer. If some substance: constraint is genuine tangled rope—coordination is real (though asymmetric), not merely disguised extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_form_legitimacy_claim_substance, empirical, 'Whether federal form had institutional substance or was purely theatrical').

omega_variable(
    reading_specificity_false_summit,
    'Is this reading (Article 17 as dead letter snare) a genuine constitutional reading, or does it risk naturalizing one particular Stalinist-era enforcement pattern as the inherent meaning of the federal form?',
    'Committer-frame recognition: This reading instantiates ONE interpretation of the contested kernel (federal_fiction). The centralized_reality_reading and nationality_form_reading offer structurally different framings of the same constitutional text. The false summit risk is declaring Article 17''s snare structure as inevitable/natural rather than as one contingent reading among three. The snare classification is correct FOR THIS READING, but the reading itself is contested.',
    'Acknowledging this as one reading among three prevents naturalizing a particular era''s enforcement (Stalinist suppression) as inherent to the constitutional text. The dead letter character was real under Stalin; less so under Lenin; different again under Gorbachev. The reading captures the Stalinist-to-late-Soviet period reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specificity_false_summit, conceptual, 'Recognition that dead letter reading is one contested interpretation, not inevitable structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_fiction__secession_dead_letter_reading, 1922, 1991).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1922_functional_federal, federal_fiction__secession_dead_letter_reading, theater_ratio, 1922, 0.35).
narrative_ontology:measurement(theater_1937_stalinist_consolidation, federal_fiction__secession_dead_letter_reading, theater_ratio, 1937, 0.58).
narrative_ontology:measurement(theater_1960_degraded_ritual, federal_fiction__secession_dead_letter_reading, theater_ratio, 1960, 0.65).

% Extraction over time
narrative_ontology:measurement(extract_1922_early_soviet, federal_fiction__secession_dead_letter_reading, base_extractiveness, 1922, 0.28).
narrative_ontology:measurement(extract_1937_stalinist_consolidation, federal_fiction__secession_dead_letter_reading, base_extractiveness, 1937, 0.62).
narrative_ontology:measurement(extract_1960_normalized_terror, federal_fiction__secession_dead_letter_reading, base_extractiveness, 1960, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1922_early_soviet, federal_fiction__secession_dead_letter_reading, suppression_requirement, 1922, 0.25).
narrative_ontology:measurement(suppression_1937_stalinist_consolidation, federal_fiction__secession_dead_letter_reading, suppression_requirement, 1937, 0.75).
narrative_ontology:measurement(suppression_1960_normalized_terror, federal_fiction__secession_dead_letter_reading, suppression_requirement, 1960, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_fiction__secession_dead_letter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_fiction__secession_dead_letter_reading, federal_fiction__centralized_reality_reading).
narrative_ontology:affects_constraint(federal_fiction__secession_dead_letter_reading, federal_fiction__nationality_form_reading).

% DUAL FORMULATION NOTE:
% The dead letter reading is part of the federal_fiction constraint family. All three readings (secession_dead_letter, centralized_reality, nationality_form) interpret the same constitutional kernel differently. The secession_dead_letter reading (this file) measures the snare structure of Article 17 specifically. The centralized_reality reading measures unitary command beneath federal form (likely snare or tangled rope with different beneficiary/victim structure). The nationality_form reading measures federal institutions as genuine (if contained) coordination mechanisms (likely tangled rope). Each reading has its own epsilon and beneficiary/victim structure. They are linked by network.affects_constraints because the constitutional fate of the kernel determines what each reading can sustain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
