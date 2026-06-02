% ============================================================================
% CONSTRAINT STORY: apartheid_nuclear_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_apartheid_nuclear_program, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: apartheid_nuclear_program
 *   human_readable: Apartheid South Africa's Clandestine Nuclear Program
 *   domain: political/military/technological
 *
 * SUMMARY:
 *   Apartheid South Africa's clandestine nuclear weapons program (1974-1990)
 *   represents a pure extraction mechanism disguised as a
 *   coordination/deterrence strategy. The regime pursued nuclear weapons to
 *   secure minority rule against a growing liberation movement and
 *   international isolation. The program extracted resources from uranium
 *   mining workers, foreclosed political autonomy for the black majority, and
 *   externalized the proliferation risk to the international system and
 *   future generations. The constraint demonstrates how a security narrative
 *   can legitimate systematic extraction, and how suppression (state secrecy)
 *   makes extraction invisible until post-apartheid disclosure. The program
 *   was terminated and weaponized devices dismantled after apartheid's
 *   collapse in 1994, but the environmental contamination and proliferation
 *   precedent persist. This constraint exhibits classical snare properties
 *   from most perspectives — suppression of knowledge was near-total during
 *   the regime period (theater ratio 0.75), extractiveness intensified as the
 *   program scaled (0.42 to 0.68 over 16 years), and victims had no exit
 *   options. The regime leadership alone experienced the constraint as a rope
 *   or coordination mechanism, perceiving it as rational national security
 *   policy. Post-apartheid analysis reveals the extraction: the program did
 *   not prevent regime collapse; it accelerated it by signaling desperation.
 *   The analytical observer's civilizational perspective reveals the deepest
 *   extraction — embedding nuclear weapons in a collapsing political order
 *   created acute proliferation risk that the international community then
 *   absorbed.
 *
 * KEY AGENTS:
 *   - Apartheid Regime Leadership (institutional/arbitrage): Primary beneficiary — pursues nuclear deterrent as strategy for regime survival and minority rule preservation
 *   - Black Majority Population (powerless/trapped): Primary victim — subject to regime's exclusive deterrent logic; nuclear escalation raises stakes of resistance
 *   - Uranium Mining Workers (moderate/constrained): Secondary victim — extract radioactive material under hazardous conditions with minimal compensation; predominantly black labor force
 *   - Western Allied Powers (powerful/arbitrage): Complicit beneficiary — provide technical intelligence and diplomatic cover; benefit from South African anti-communist alignment; constrained by nuclear proliferation risk
 *   - International Anti-Apartheid Coalition (organized/mobile): Secondary actor — apply sanctions and non-proliferation pressure; see sunset in regime collapse
 *   - Future Generations (powerless/analytical): Civilizational victim — inherit proliferation precedent and environmental contamination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(apartheid_nuclear_program, 0.68).
domain_priors:suppression_score(apartheid_nuclear_program, 0.92).
domain_priors:theater_ratio(apartheid_nuclear_program, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(apartheid_nuclear_program, extractiveness, 0.68).
narrative_ontology:constraint_metric(apartheid_nuclear_program, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(apartheid_nuclear_program, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(apartheid_nuclear_program, snare).
narrative_ontology:human_readable(apartheid_nuclear_program, "Apartheid South Africa's Clandestine Nuclear Program").
narrative_ontology:topic_domain(apartheid_nuclear_program, "political/military/technological").

domain_priors:requires_active_enforcement(apartheid_nuclear_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(apartheid_nuclear_program, apartheid_regime_leadership).
narrative_ontology:constraint_victim(apartheid_nuclear_program, black_majority_population).
narrative_ontology:constraint_victim(apartheid_nuclear_program, international_community).
narrative_ontology:constraint_victim(apartheid_nuclear_program, uranium_mining_workers).
narrative_ontology:constraint_victim(apartheid_nuclear_program, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BLACK MAJORITY POPULATION (SNARE) — Subject to the apartheid regime's exclusive nuclear deterrent logic, which treats them as demographic liability to be managed through institutional violence. The nuclear program deepens their entrapment by raising the stakes of resistance: armed uprising now risks nuclear escalation. No exit option; bear full extraction cost.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URANIUM MINING WORKERS (SNARE) — Predominantly black workers in Witwatersrand mines extract material for weapons program. Exposed to radiation hazards, unsafe conditions, minimal compensation. Constrained exit due to economic desperation and apartheid labor control. Extraction embedded in the supply chain itself.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: WESTERN ALLIED POWERS (TANGLED ROPE) — US, UK, France, Israel provide technical intelligence, enrichment technology, and diplomatic cover to South Africa's program during Cold War. Coordination function: mutual deterrence against Soviet expansion in Southern Africa. But asymmetric extraction: South Africa becomes dependent on Western support, constraining its geopolitical autonomy. Benefits from South African stability as anti-communist buffer; pays price of being complicit in apartheid escalation.
constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL ANTI-APARTHEID COALITION (SCAFFOLD) — UN sanctions, arms embargoes, nuclear non-proliferation regime, divestment campaigns create external pressure for regime collapse. Low effective extraction because the coalition has agency and sees a sunset: as the regime destabilizes, the nuclear program becomes a liability rather than an asset. The constraint dissolves when apartheid ends. Theater ratio declines as open disclosure becomes inevitable.
constraint_indexing:constraint_classification(apartheid_nuclear_program, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: APARTHEID REGIME LEADERSHIP (ROPE) — Primary beneficiary. Nuclear weapons solve a coordination problem: how to maintain white minority rule against growing resistance without conventional military exhaustion. The program is experienced as coordination mechanism, not as extraction. Leadership has maximum exit optionality and maximum benefit. Perception: rational security necessity.
constraint_indexing:constraint_classification(apartheid_nuclear_program, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view of nuclear proliferation, the program represents a pure extraction from the future: it embeds nuclear weapons into a collapsing political order, creating acute proliferation risk and contamination of the nonproliferation regime. The constraint extracts from all future generations through increased nuclear instability. High suppression (state secrecy) masks the true scope until post-apartheid disclosure reveals the extraction magnitude.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(apartheid_nuclear_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(apartheid_nuclear_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The program transfers resources (uranium, enriched material, scientific talent, labor under duress) from the broader South African population to regime security apparatus. Extractiveness increases over the interval (0.42 to 0.68) as the program scales from research to weapons assembly. The extraction is not total (0.92 would require complete economic paralysis), but it is severe — the program consumes significant state resources that could have been allocated to health, education, or economic development for the majority population. Suppression (0.92): Extremely high. The program is state-secret for its entire 16-year operational period. Knowledge is restricted to regime elite and Western intelligence partners. Not even the apartheid parliament votes on the program openly. Workers are not informed they are supplying nuclear weapons. International inspections are evaded through shell companies and false declarations. The suppression is near-total and only dissolves after 1994. Theater ratio (0.58): Moderate. The program operates largely in secrecy (high theater), but internally, regime leadership experiences it as rational security policy, not as theater. Post-apartheid disclosure reveals the incoherence: the nuclear deterrent did not prevent regime collapse; it signaled desperation. The theater reflects the gap between the narrative (rational deterrence) and the outcome (regime collapse triggered in part by apartheid's isolation, to which the nuclear program contributed).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between regime leadership and the black majority is maximal. The regime experiences nuclear weapons as a rational coordination mechanism solving the problem of how to maintain minority rule against resistance — a rope from their perspective. The black majority experiences the same program as pure extraction: resources diverted from their welfare, and the nuclear escalation deepens their entrapment by raising the existential stakes of resistance. Workers experience constrained snare: they are compelled to extract and process radioactive material under unsafe conditions with minimal understanding of the ultimate use. Western allies experience tangled rope: they coordinate with South Africa on Cold War anti-communism but extract a price through dependence and proliferation complicity. The anti-apartheid coalition experiences a temporary constraint with a sunset: sanctions and non-proliferation pressure that dissolve when apartheid ends. The analytical observer experiences a civilizational snare: the program embeds nuclear weapons in a collapsing political order, externalizing proliferation risk to the future. All six perspectives are structurally legitimate readings of the same base properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is straightforward for most perspectives: beneficiaries (regime) have arbitrage exit (d ≈ 0.05, low extraction experienced), while victims (black majority, workers) have trapped/constrained exit (d ≈ 0.95, high extraction experienced). The Western allies occupy an intermediate position — they benefit from South African anti-communism (beneficiary logic) but are constrained by the proliferation risk they help create (victim logic). The analytical observer is purely analytical (d ≈ 0.72), observing the civilizational extraction without structural position. No directionality overrides are needed — the derivation chain produces accurate perspectival readings. Beneficiary status derives from explicit regime strategy documentation and resource allocation records. Victim status derives from structural position: black majority has no exit from apartheid; workers have constrained exit from employment under hazardous conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH PERSPECTIVAL DECOMPOSITION: The mandatrophy question is 'Is this a rational security coordination (Rope) or a destabilizing extraction mechanism (Snare)?' The analytical answer: both are true from different observation sites. From the regime's immediate perspective, the program is experienced as rational security coordination. From the black majority's perspective, it is pure extraction. From the analytical/civilizational perspective, the program is extraction from the future — it creates proliferation risk and environmental contamination that persist after the regime collapses. The mandatrophy resolves by recognizing that the program's primary function (deterrence) and its primary effect (acceleration of regime isolation and collapse) are structurally distinct. The function is coordination (from regime perspective); the effect is extraction (from majority and civilizational perspectives). The presheaf of indexed classifications captures this: the constraint is not ambiguous, it is perspectival. Apartheid regime leadership's mistake was treating the nuclear program as rational security policy (rope) while ignoring that it functioned as extraction from the majority and as a proliferation liability to the international system (snare from those perspectives). The regime's collapse validates the snare classification: the program did not provide the security it promised because it was premised on extracting compliance from a population that had no reason to provide it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_rationality_premise,
    'Was nuclear deterrence a rational security strategy for apartheid regime survival, or a psychological manifestation of siege mentality that actually accelerated regime collapse?',
    'Counterfactual analysis: regime stability trajectories with vs without nuclear program; comparative study of other collapsing regimes that did/did not pursue nuclear options; interviews with regime leadership and defectors',
    'If rational: snare classification shifts toward tangled_rope from regime perspective (coordination function genuine). If psychological: snare classification hardens (extraction from future through unnecessary proliferation risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_rationality_premise, conceptual, 'Whether nuclear deterrence was rational regime survival strategy or siege mentality manifestation').

omega_variable(
    western_complicity_boundary,
    'Where does Western support for apartheid nuclear program cross from passive intelligence-sharing to active conspiracy?',
    'Declassified documents analysis (UK, US, France FOI releases); trace technology flow and enrichment supply chain; identify decision points where Western actors could have blocked escalation',
    'If passive: Western powers see tangled_rope (coordination + extraction). If active conspiracy: Western powers shift to snare (knowing extraction agents). Changes charter of responsibility and cascades to international law implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(western_complicity_boundary, empirical, 'Boundary between passive intelligence-sharing and active conspiracy in Western support').

omega_variable(
    worker_knowledge_extent,
    'To what extent did uranium mining workers understand they were supplying a nuclear weapons program, and did this understanding change extraction dynamics?',
    'Oral history collection from mining workers; archival records of union organizing efforts; analysis of safety discourse (whether framed as economic vs existential risk)',
    'If workers aware: extraction was transparent (snare classification hardened for that perspective). If unaware: extraction was hidden until post-apartheid disclosure (snare classification correct but theater ratio was artificially suppressed during regime period).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_knowledge_extent, empirical, 'Extent of worker knowledge about nuclear weapons program supply chain').

omega_variable(
    environmental_contamination_scope,
    'What is the full scope of radioactive contamination from the uranium enrichment and nuclear testing program, and how many communities were affected?',
    'Radiological surveys of enrichment facilities and test sites; health outcome studies of exposed populations; archive searches for secret testing and dumping records',
    'If contamination localized and small: snare classification remains. If widespread and unacknowledged: extraction extends to present-day communities (future generations victim status expands), intensifying the civilizational snare perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_contamination_scope, empirical, 'Full scope of radioactive contamination from enrichment and testing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(apartheid_nuclear_program, 1974, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anp_tr_t1974, apartheid_nuclear_program, theater_ratio, 1974, 0.75).
narrative_ontology:measurement(anp_tr_t1982, apartheid_nuclear_program, theater_ratio, 1982, 0.62).
narrative_ontology:measurement(anp_tr_t1990, apartheid_nuclear_program, theater_ratio, 1990, 0.58).

% Extraction over time
narrative_ontology:measurement(anp_be_t1974, apartheid_nuclear_program, base_extractiveness, 1974, 0.42).
narrative_ontology:measurement(anp_be_t1982, apartheid_nuclear_program, base_extractiveness, 1982, 0.65).
narrative_ontology:measurement(anp_be_t1990, apartheid_nuclear_program, base_extractiveness, 1990, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(apartheid_nuclear_program, enforcement_mechanism).
narrative_ontology:affects_constraint(apartheid_nuclear_program, nuclear_proliferation_regimes).
narrative_ontology:affects_constraint(apartheid_nuclear_program, extractive_uranium_mining).
narrative_ontology:affects_constraint(apartheid_nuclear_program, cold_war_intelligence_partnerships).

% DUAL FORMULATION NOTE:
% The apartheid nuclear program decomposes into three structurally distinct constraints: (1) nuclear_proliferation_regimes — the international coordination failure that enabled the program; (2) extractive_uranium_mining — the supply chain extraction from mining workers; (3) cold_war_intelligence_partnerships — the Western complicity structure. This story focuses on the integrated program as a snare; the decomposition captures how the constraint cascades through multiple institutional structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
