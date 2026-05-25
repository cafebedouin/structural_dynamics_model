% ============================================================================
% CONSTRAINT STORY: ulysses_chp15
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp15, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp15
 *   human_readable: The Nighttown Phantasmagoria (Circe) - Hallucinatory Extraction in the Red-Light District
 *   domain: social/psychological/religious
 *
 * SUMMARY:
 *   Chapter 15 of Ulysses renders Nighttown (Dublin's red-light district) as
 *   a site of extreme hallucinatory extraction where the normal rules of
 *   social hierarchy, psychological coherence, and physical law dissolve.
 *   Stephen Dedalus and Leopold Bloom enter the district seeking temporary
 *   escape, sexual fulfillment, or spiritual insight, but encounter instead a
 *   constraint that operates through sensory overload, designed seduction,
 *   psychological vulnerability, and the total suppression of exit
 *   alternatives. The Nighttown Phantasmagoria is a Snare from the
 *   perspectives of its trapped victims (Stephen, Bloom, sex workers) and a
 *   Rope from the perspectives of its beneficiaries (proprietors,
 *   exploiters). The constraint's theater ratio (0.88) reflects that the
 *   district's hallucinatory mechanisms are substantially performative:
 *   elaborate rituals of temptation, degradation, and psychological
 *   manipulation obscure the underlying economic extraction. The modernist
 *   literary form itself contributes to the theater: Joyce's representation
 *   renders the constraint textually vivid while potentially obscuring its
 *   material structural dynamics.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary victim (powerless/trapped) — young intellectual seeking self-assertion; psychologically vulnerable to the district's extraction; bears cost of humiliation and spiritual/psychological damage
 *   - Leopold Bloom: Primary victim (powerless/trapped) — wanderer seeking sensory experience; trapped by desire and alcohol; experiences voyeurism and sexual inadequacy within the constraint
 *   - Sex Workers: Collective victim (powerless/trapped) — women trapped within patriarchal economic system, legal vulnerability, and the district's designed extraction machinery
 *   - Bella Cohen / Madams: Primary beneficiary (institutional/arbitrage) — extract economically from clients and labor; maintain control through psychological and social mechanisms
 *   - Nighttown Proprietors: Institutional beneficiary (institutional/arbitrage) — landlords, pimps, business operators who profit from the constraint's infrastructure
 *   - Patriarchal Religious Establishment: Institutional beneficiary (organized/constrained) — benefits from Nighttown as moral boundary; uses the district to reinforce gender and sexual hierarchies
 *   - Modernist Literary Observer: Analytical witness (analytical/analytical) — Joyce's artistic representation creates a textual rendering that may obscure or reveal structural extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp15, 0.78).
domain_priors:suppression_score(ulysses_chp15, 0.82).
domain_priors:theater_ratio(ulysses_chp15, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp15, extractiveness, 0.78).
narrative_ontology:constraint_metric(ulysses_chp15, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(ulysses_chp15, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp15, snare).
narrative_ontology:human_readable(ulysses_chp15, "The Nighttown Phantasmagoria (Circe) - Hallucinatory Extraction in the Red-Light District").
narrative_ontology:topic_domain(ulysses_chp15, "social/psychological/religious").

domain_priors:requires_active_enforcement(ulysses_chp15).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp15, nighttown_proprietors).
narrative_ontology:constraint_beneficiary(ulysses_chp15, exploitation_machinery).
narrative_ontology:constraint_victim(ulysses_chp15, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp15, leopold_bloom).
narrative_ontology:constraint_victim(ulysses_chp15, sex_workers).
narrative_ontology:constraint_victim(ulysses_chp15, male_psyche_collective).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED WANDERER (SNARE) — Stephen and Bloom experience Nighttown as a hallucinatory trap with no exit. Sensory overload, alcohol, psychological vulnerability, and the district's designed seduction mechanics create maximum suppression of alternatives. Both agents are powerless to resist the constraint's extractive pull. They bear full psychological and financial cost with no organized defense or mobility.
constraint_indexing:constraint_classification(ulysses_chp15, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SEX WORKERS (SNARE) — The workers in Nighttown are trapped within multiple layers of extraction: economic desperation, legal vulnerability, patriarchal control, and the architectural design of the district itself. Their labor is extracted not just economically but psychologically and spiritually. The constraint operates through normalized violence and denial of exit options.
constraint_indexing:constraint_classification(ulysses_chp15, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: NIGHTTOWN PROPRIETORS (ROPE) — The madams, pimps, and business operators experience Nighttown as a pure coordination mechanism: organizing labor, managing clientele, maintaining the district's infrastructure and reputation. Their perspective naturalizes the constraint as legitimate commerce. Extraction runs toward these agents; they experience the district as enabling cooperation among exploiters.
constraint_indexing:constraint_classification(ulysses_chp15, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: PATRIARCHAL RELIGIOUS ESTABLISHMENT (TANGLED ROPE) — The Catholic Church and patriarchal social order benefit from Nighttown's existence: it serves as a moral boundary marker, a location where sexual transgression is contained and condemned, and an outlet for male desire that reinforces gender hierarchies. The establishment has both coordinating and extractive functions relative to sexual morality — it enforces the constraint while benefiting from it.
constraint_indexing:constraint_classification(ulysses_chp15, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST OBSERVER (SCAFFOLD) — Social reform movements (late 19th century antiprostitution and temperance advocates) see Nighttown as a temporary moral failure with a sunset clause. The constraint is viewed as remediable through education, legal reform, and moral persuasion. This perspective treats the district as a degraded coordination problem that can be solved through structural change.
constraint_indexing:constraint_classification(ulysses_chp15, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: MODERNIST LITERARY WITNESS (PITON) — Joyce's artistic representation of Nighttown creates a performative space where the constraint is rendered textually but its actual structural dynamics are eclipsed by hallucinatory theater. The literary form itself becomes a form of inertial representation: the text dramatizes extraction without enabling material exit. The constraint persists in its literary form long after historical Nighttown fades.
constraint_indexing:constraint_classification(ulysses_chp15, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp15_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp15, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp15, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp15, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp15, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp15_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The district extracts through multiple channels: economic (fees, wages seized), psychological (humiliation, violation of dignity), sexual (coerced or exploited sexuality), and spiritual (desecration of the self). Victims bear costs across all dimensions. The value reflects that Nighttown is designed to extract maximum value while minimizing victims' alternatives. Suppression (0.82): Very high. The constraint operates through: (1) sensory overload and hallucinatory disorientation that prevent rational decision-making; (2) alcohol and drugs that impair agency; (3) psychological vulnerability (Stephen's shame, Bloom's sexual inadequacy); (4) the district's physical design as a total environment with no easy exit; (5) legal and social vulnerability of sex workers; (6) patriarchal conditioning that naturalizes male access to female bodies. Exit options are systematically closed. Theater ratio (0.88): Very high and increasing over the chapter's duration. The hallucinatory mechanisms, elaborate seduction rituals, and psychological manipulations are substantially performative. The constraint's power rests on creating an altered reality where normal rules do not apply. The modernist literary form amplifies this theater — the text's hallucinatory style mirrors and potentially naturalizes the constraint's mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here. Stephen and Bloom see a Snare with no exit. The proprietors see a Rope enabling profitable coordination. The religious establishment sees both a Snare (morally problematic) and a Rope (socially functional for maintaining hierarchies). The reformist observer sees a Scaffold with a sunset clause (the district will be eliminated through moral reform). The modernist literary witness sees a Piton: the district persists through theatrical representation even as its material reality fades. The gap between perspectives is not merely observational but existential — agents literally inhabit different realities depending on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim asymmetry is stark. Proprietors and madams (institutional/arbitrage exit) experience low directionality (~0.05-0.15), meaning they perceive the constraint as enabling coordination among themselves. Their effective extraction chi is dampened by their arbitrage options — they can exit the district economically if needed. Trapped agents (powerless/trapped exit) experience maximum directionality (~0.95), meaning they perceive the constraint as extractive with no alternative. Their effective extraction chi is amplified by their trapped position. The patriarchal establishment (organized/constrained exit) occupies a middle position: they benefit from the constraint's existence but are constrained by reformist pressure and their own ideological investment in containing sexuality. Their d-value (~0.40-0.50) reflects mixed experience: coordination among controllers with some risk of exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_ci_rope. MaxEnt consensus tangled_rope (classical 0.641, indexed 0.969); engine override accepted given ε=0.78 + named victims.
 *   Mandatrophy is resolved by recognizing that Nighttown operates as a pure Snare from the victims' perspectives and as a pure Rope from the beneficiaries' perspectives. There is no mandatrophy because there is no ambiguity about which type is in effect — the classification depends entirely on whose structural position you adopt. The potential false summit is the modernist artistic representation, which risks naturalizing the constraint as an immutable feature of human psychology or urban life. Joyce's modernist form creates a theatrical rendering that may obscure rather than reveal the contingent institutional structures (legal vulnerability of sex workers, male sexual entitlement, economic desperation) that maintain the constraint. The mandatrophy analysis suggests that the critical reading of Nighttown requires stepping outside the text's hallucinatory logic to see the material extraction beneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hallucination_vs_reality_boundary,
    'Is the hallucinatory chaos of Nighttown a representation of the agents'' psychological states or a structural property of the district itself?',
    'Comparison of Stephen and Bloom''s subjective experiences with corroborating historical accounts of Dublin''s red-light district; analysis of where hallucinatory content originates (drug-induced, psychological trauma, environmental design)',
    'If hallucination is primarily subjective: extractiveness value drops to ~0.55 (psychological snare). If hallucination is environmentally induced: extractiveness remains ~0.78 (structural snare with designed sensory suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hallucination_vs_reality_boundary, conceptual, 'Whether hallucinatory content represents psychological or structural constraint').

omega_variable(
    circe_mythic_agency,
    'Does the Circe figure (Bella Cohen) exercise intentional magical/psychological control over her victims, or is she herself trapped within the extraction machinery?',
    'Textual analysis of Bella''s characterization; historical research into madam agency and labor conditions; comparison with Homer''s Circe figure',
    'If Bella has genuine agency: she is a beneficiary with moderate power (organized/arbitrage). If she is also trapped: she becomes a secondary victim, reducing the beneficiary count and shifting the snare classification toward shared victimhood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circe_mythic_agency, conceptual, 'Whether Bella Cohen exercises true agency or is trapped within extraction').

omega_variable(
    modernist_complicity_threshold,
    'Does Joyce''s artistic rendering of Nighttown constitute critical representation of extraction or aesthetic complicity in the voyeurism it depicts?',
    'Literary criticism evaluating authorial stance; comparison of textual treatment with contemporary social reform discourse; analysis of reader positioning relative to victim experience',
    'If complicity: the piton classification is strengthened (theatrical representation replacing material change). If critical: the text becomes an analytical observer perspective revealing the constraint rather than naturalizing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_complicity_threshold, preference, 'Whether literary representation enables or obscures critical analysis of extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp15, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulysses_circe_tr_t0, ulysses_chp15, theater_ratio, 0, 0.65).
narrative_ontology:measurement(ulysses_circe_tr_t5, ulysses_chp15, theater_ratio, 5, 0.78).
narrative_ontology:measurement(ulysses_circe_tr_t10, ulysses_chp15, theater_ratio, 10, 0.88).

% Extraction over time
narrative_ontology:measurement(ulysses_circe_be_t0, ulysses_chp15, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(ulysses_circe_be_t5, ulysses_chp15, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(ulysses_circe_be_t10, ulysses_chp15, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp15, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp15, male_sexual_entitlement_apparatus).
narrative_ontology:affects_constraint(ulysses_chp15, patriarchal_psychological_conditioning).
narrative_ontology:affects_constraint(ulysses_chp15, dublin_economic_desperation).

% DUAL FORMULATION NOTE:
% The Nighttown Phantasmagoria decomposes into three structurally distinct constraints: (1) the economic extraction of sex workers (high ε, victim-focused); (2) the psychological extraction from male clients (high ε, voyeurism and shame); (3) the institutional benefit to the patriarchal order (moderate ε, coordination function). This story focuses on the unified constraint as experienced by Stephen and Bloom. Upstream constraints (male sexual entitlement, patriarchal conditioning) feed into Nighttown; downstream constraints (literary representation, modernist naturalization) feed out of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp15, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
