% ============================================================================
% CONSTRAINT STORY: active_passive_citizenship__property_franchise_logic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_active_passive_citizenship__property_franchise_logic_reading, []).

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
 *   constraint_id: active_passive_citizenship__property_franchise_logic_reading
 *   human_readable: Active-Citizen Franchise: Property Threshold as Stake Doctrine
 *   domain: legal/political_philosophy/constitutional
 *
 * SUMMARY:
 *   The French Revolution's active-citizen franchise embodied a doctrine:
 *   only the economically independent can vote independently. The property
 *   threshold — measured as a tax equivalent to three days' labor — was
 *   presented not as suppression but as the objective boundary between
 *   citizens capable of free deliberation and those whose economic dependence
 *   corrupted their judgment. This constraint is one reading of a contested
 *   kernel: the same revolutionary moment that declared 'all men are created
 *   equal' also split citizens into two categories, one with full rights and
 *   one with passive rights only. The property franchise logic reading
 *   instantiates the doctrine as stated — economic independence as the
 *   prerequisite for deliberative independence — and exhibits it as a snare:
 *   a constraint that suppresses alternative voices through doctrine, not
 *   only through force, making the suppression appear rational and inevitable
 *   rather than extractive.
 *
 * KEY AGENTS:
 *   - Property-Owning Deliberative Class: Primary beneficiary (institutional/arbitrage) — benefits from exclusive voting power; the doctrine legitimizes their monopoly on deliberation
 *   - Wage-Dependent Citizens: Primary victim (powerless/trapped) — cannot meet the three-day labor tax threshold; completely excluded from the franchise
 *   - Revolutionary Political Movement: Secondary organized victim (organized/constrained) — remembers the Declaration's universalist claim; mobilizes against the property restriction through political organizing
 *   - Doctrinal Authority Structure: Institutional actor enforcing the stake doctrine — legislative bodies, jurists, constitutional interpreters maintaining the reading
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the stake doctrine as an immutable requirement of democratic theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(active_passive_citizenship__property_franchise_logic_reading, 0.58).
domain_priors:suppression_score(active_passive_citizenship__property_franchise_logic_reading, 0.72).
domain_priors:theater_ratio(active_passive_citizenship__property_franchise_logic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(active_passive_citizenship__property_franchise_logic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(active_passive_citizenship__property_franchise_logic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(active_passive_citizenship__property_franchise_logic_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(active_passive_citizenship__property_franchise_logic_reading, snare).
narrative_ontology:human_readable(active_passive_citizenship__property_franchise_logic_reading, "Active-Citizen Franchise: Property Threshold as Stake Doctrine").
narrative_ontology:topic_domain(active_passive_citizenship__property_franchise_logic_reading, "legal/political_philosophy/constitutional").

domain_priors:requires_active_enforcement(active_passive_citizenship__property_franchise_logic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(active_passive_citizenship__property_franchise_logic_reading, '811a6c1f-571a-4f00-a445-6a57a91743e3').
narrative_ontology:cs_kernel_codification('811a6c1f-571a-4f00-a445-6a57a91743e3', formalized).
narrative_ontology:cs_authority_grounding('811a6c1f-571a-4f00-a445-6a57a91743e3', lineage).
narrative_ontology:cs_interpretation_layer_present('811a6c1f-571a-4f00-a445-6a57a91743e3').
narrative_ontology:cs_reading_relation('811a6c1f-571a-4f00-a445-6a57a91743e3', exclusion_of_the_poor__active_passive_citizenship, coexists_with).
narrative_ontology:cs_reading_relation('811a6c1f-571a-4f00-a445-6a57a91743e3', women_excluded__active_passive_citizenship, coexists_with).
narrative_ontology:cs_axiom('811a6c1f-571a-4f00-a445-6a57a91743e3', foundational, economic_independence_enables_deliberative_independence).
narrative_ontology:cs_axiom_status(economic_independence_enables_deliberative_independence, holdable).
narrative_ontology:cs_axiom_grounding('811a6c1f-571a-4f00-a445-6a57a91743e3', economic_independence_enables_deliberative_independence, empirically_contingent).
narrative_ontology:cs_axiom('811a6c1f-571a-4f00-a445-6a57a91743e3', secondary, property_threshold_measures_stake_requirement).
narrative_ontology:cs_axiom_status(property_threshold_measures_stake_requirement, holdable).
narrative_ontology:cs_axiom_grounding('811a6c1f-571a-4f00-a445-6a57a91743e3', property_threshold_measures_stake_requirement, instrumental).
narrative_ontology:cs_reference_frame('811a6c1f-571a-4f00-a445-6a57a91743e3', propertied_deliberation_framework).
narrative_ontology:cs_drift_state('811a6c1f-571a-4f00-a445-6a57a91743e3', universal_suffrage_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('811a6c1f-571a-4f00-a445-6a57a91743e3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(active_passive_citizenship__property_franchise_logic_reading, active_passive_citizenship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(active_passive_citizenship__property_franchise_logic_reading, propertied_deliberation_theory).
narrative_ontology:constraint_beneficiary(active_passive_citizenship__property_franchise_logic_reading, property_owning_men).
narrative_ontology:constraint_victim(active_passive_citizenship__property_franchise_logic_reading, wage_dependent_citizens).
narrative_ontology:constraint_victim(active_passive_citizenship__property_franchise_logic_reading, disenfranchised_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT CITIZEN (SNARE) — Trapped by the three-day labor tax threshold. Cannot exit the constraint; the stake doctrine priced participation at an economic barrier designed to exclude. No alternatives to the franchise; no exit from the suppression. Maximum extraction.
constraint_indexing:constraint_classification(active_passive_citizenship__property_franchise_logic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTY-OWNING DELIBERATIVE CLASS (ROPE) — Benefits from the stake doctrine as coordination of 'independent judgment.' The reading itself legitimizes this perspective: propertied deliberation theory presents the constraint as solving the problem of ensuring that voters have enough at stake to deliberate seriously. This perspective experiences the constraint as pure coordination — the mechanism that secures deliberative quality.
constraint_indexing:constraint_classification(active_passive_citizenship__property_franchise_logic_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REVOLUTIONARY POLITICAL MOVEMENT (SNARE) — Organized agents (Jacobins, sans-culottes, radical democrats) face suppression through legal exclusion, but also carry the memory of 1789's Declaration — 'all men are created equal.' This organized movement sees the property franchise as betrayal and extraction. The constraint suppresses alternative political organization and exits. High suppression, high extraction.
constraint_indexing:constraint_classification(active_passive_citizenship__property_franchise_logic_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as a natural law of political science: only those with sufficient economic independence can deliberate freely; dependence on wages corrupts judgment. This perspective risks naturalizing the property threshold as an immutable requirement of democratic deliberation. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'independence = property' equation is a theoretical construct, not a law of nature.
constraint_indexing:constraint_classification(active_passive_citizenship__property_franchise_logic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(active_passive_citizenship__property_franchise_logic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(active_passive_citizenship__property_franchise_logic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(active_passive_citizenship__property_franchise_logic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(active_passive_citizenship__property_franchise_logic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(active_passive_citizenship__property_franchise_logic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts through legal doctrine: it prices participation at three days' labor, a barrier designed to exclude wage-dependent citizens. The extraction is real (participation is denied) and structural (the barrier compounds over time as suppression mechanisms harden). The measurement trajectory shows increasing extractiveness from 0.45 to 0.62 as the doctrine solidifies and enforcement mechanisms mature. Suppression (0.72): High. The constraint suppresses alternatives through multiple mechanisms: (1) legal doctrine that naturalizes the property threshold as rational necessity, (2) lack of viable exit (wage-dependent citizens cannot become property owners rapidly enough to vote before the next election cycle), (3) legal prohibition of the franchise itself (not just expensive, but forbidden), and (4) no alternative participation mechanisms. Theater ratio (0.45): Moderate. The constraint's mechanism is primarily functional suppression (legal exclusion) rather than performative ritual. The doctrine provides a rational cover story ('deliberative independence requires property independence'), but the mechanism is direct exclusion, not theater. This distinguishes it from a constraint that maintains an extractive ritual for show.
 *
 * PERSPECTIVAL GAP:
 *   The property-owning deliberative class experiences the stake doctrine as pure coordination (Rope) — the mechanism that ensures deliberative quality by restricting the franchise to those with sufficient stake in outcomes. The wage-dependent citizen experiences it as pure extraction (Snare) — a legal mechanism that prices participation at an impossible threshold. The revolutionary political movement sees it as betrayal and extraction (Snare) — they held the memory of 1789's universalist Declaration and perceive the property threshold as suppression. The analytical observer at the civilizational level risks seeing it as a natural law (Mountain) — 'only the independent can deliberate freely' appears as an immutable principle of political science — but the structural data reveals it as a false summit: the 'independence = property' equation is a contingent doctrinal choice, not a discovered law. The perspectival gap exposes the reading's core mechanism: using doctrinal language ('deliberative independence') to legitimize extraction ('exclude the poor').
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation runs from beneficiary/victim status through exit options. The property-owning class benefits (low d) and faces high exit options (arbitrage — they can always withdraw to their estates). The wage-dependent class is victimized (high d) and faces trapped exit (legal prohibition). The doctrine itself is the beneficiary ('propertied deliberation theory') — the reading instantiates the doctrine as a structural agent. The suppression increases over time as enforcement mechanisms harden and the doctrine becomes increasingly reified as 'natural law.' The analytical observer at the civilizational level computes d as observer-position (d ≈ 0.72, moderate victim position because the analysis sees the arbitrary exclusion).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_property_equivalence,
    'Does economic independence actually require property ownership? Or does wage stability enable independent judgment equally well?',
    'Historical analysis of voting behavior by property status and wage stability; comparative analysis of deliberative quality in democratic systems with different franchise rules; contemporary evidence from universal suffrage democracies',
    'If property is necessary: snare classification confirmed — stake doctrine unavoidably excludes dependent voters. If wage stability suffices: snare classification confirmed but the underlying theory is false — the constraint extracts by enforcing a false doctrine, not by enforcing a necessary condition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independence_property_equivalence, empirical, 'Whether property ownership is necessary for independent judgment').

omega_variable(
    deliberative_quality_correlation,
    'Is deliberative quality actually higher in property-restricted franchises than in universal-suffrage systems?',
    'Comparative historical analysis of legislative deliberation quality (debate transcripts, amendment specificity, evidence-responsiveness) across restricted vs. universal franchise periods; qualitative assessment of policy outcomes',
    'If higher: stake doctrine has empirical warrant; the theory is not false, though still extractive. If equal or lower: stake doctrine is revealed as extractive theory masquerading as necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_quality_correlation, empirical, 'Whether property-restricted franchises produce higher-quality deliberation').

omega_variable(
    tax_threshold_arbitrariness,
    'Is the three-day labor threshold a discovered necessity or an arbitrary enforcement boundary that could have been set differently?',
    'Historical investigation of the legislative debates that set the threshold; analysis of alternative thresholds proposed and rejected; comparison to property franchises in other constitutional contexts',
    'If arbitrary: snare classification confirmed; the threshold is pure suppression mechanism, not derived from any inherent requirement. If necessary: still extractive, but the extraction is structural to deliberative democracy rather than purely political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_threshold_arbitrariness, empirical, 'Whether the tax threshold is necessary or arbitrary').

omega_variable(
    kernel_contest_reading_disambiguation,
    'Is this reading the stake doctrine as a genuine theory of deliberative independence, or the stake doctrine as a naked suppression mechanism masquerading as theory?',
    'The frame itself contains the ambiguity. Rule 1 requires generating this reading cleanly (the stake doctrine as theory). Rule 2 routes the ambiguity to omega variables. The question is whether the actors holding this reading believed the theory or deployed it as cover for extraction.',
    'If believed (doctrinal commitment): the reading is a genuine commitment-system axiom, overrideable only by empirical refutation of the independence hypothesis. If deployed as cover (extractive use): the reading is a snare that uses doctrinal language to suppress alternatives. The classification (snare) holds either way; the omega disambiguates the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_disambiguation, conceptual, 'Whether stake doctrine is sincerely held theory or extractive cover story').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(active_passive_citizenship__property_franchise_logic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(apfc_extractiveness_t0, active_passive_citizenship__property_franchise_logic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(apfc_extractiveness_t5, active_passive_citizenship__property_franchise_logic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(apfc_extractiveness_t10, active_passive_citizenship__property_franchise_logic_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(apfc_suppression_t0, active_passive_citizenship__property_franchise_logic_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(apfc_suppression_t5, active_passive_citizenship__property_franchise_logic_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(apfc_suppression_t10, active_passive_citizenship__property_franchise_logic_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(active_passive_citizenship__property_franchise_logic_reading, identity_coordination).
narrative_ontology:affects_constraint(active_passive_citizenship__property_franchise_logic_reading, exclusion_of_the_poor__active_passive_citizenship).
narrative_ontology:affects_constraint(active_passive_citizenship__property_franchise_logic_reading, women_excluded__active_passive_citizenship).

% DUAL FORMULATION NOTE:
% The active_passive_citizenship kernel decomposes into three constraint stories, each with different ε values reflecting their structural-analytical distinctness. The property_franchise_logic_reading (this file) has ε=0.58 (snare). The exclusion_of_the_poor_reading has ε=0.72 (higher extraction, emphases the betrayal of universalism). The women_excluded_reading has ε=0.65 (the combined class+gender suppression). All three are linked via network.affects_constraints because they contest the same constitutional kernel and produce different victim sets and beneficiary configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(active_passive_citizenship__property_franchise_logic_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
