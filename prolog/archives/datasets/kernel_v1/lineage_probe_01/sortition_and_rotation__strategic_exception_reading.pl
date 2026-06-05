% ============================================================================
% CONSTRAINT STORY: sortition_and_rotation__strategic_exception_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sortition_and_rotation__strategic_exception_reading, []).

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
 *   constraint_id: sortition_and_rotation__strategic_exception_reading
 *   human_readable: Sortition and Rotation: Strategic Exception Reading (Military/Treasury Posts)
 *   domain: legal/doctrinal/political_structure
 *
 * SUMMARY:
 *   This constraint captures a critical moment in Athenian democratic
 *   doctrine: the explicit carving out of military command and treasury
 *   control from the sortition system. Sortition — selection by lot — was the
 *   signature democratic mechanism in classical Athens, preventing the
 *   formation of a permanent political class and ensuring that most offices
 *   rotated through the citizen body. Yet Athens recognized that some stakes
 *   were too high for random selection. Generals and treasurers were elected
 *   by vote, not determined by lot. This exception instantiates a reading of
 *   sortition that sees the system as pragmatically bounded by competence
 *   requirements: where incompetence kills (military command) or where fraud
 *   drains the commons (treasury), aristocratic selection — selection by the
 *   demonstrated ability of candidates to persuade the assembly — replaced
 *   democratic lottery. This reading of sortition is one of three competing
 *   kernel interpretations. The anti_professional_reading sees sortition as
 *   fundamentally anti-elitist, designed to prevent any political class from
 *   forming, making the military exception a corruption of sortition's core
 *   logic. The equal_chance_reading sees sortition as operationalizing
 *   equality: where votes measure wealth and persuasion, only the lot ensures
 *   truly equal access to rule, making the exception a failure of commitment
 *   to equality. The strategic_exception_reading — the one this constraint
 *   instantiates — sees sortition as a toolkit pragmatically applied: using
 *   the lot where stakes are survivable, using election where competence is
 *   critical. This reading treats the exception not as a defeat of sortition
 *   but as sortition's intelligent boundary. The structural delta for this
 *   reading is clear: suppression of sortition where stakes are lethal;
 *   beneficiary is military competence (and those best positioned to
 *   demonstrate it); victim set is pure-sortition doctrine; extractiveness
 *   flows from the concentration of power in elected posts that bypass
 *   democratic lottery.
 *
 * KEY AGENTS:
 *   - Non-aristocratic Citizens: Powerless/trapped agents — face structural exclusion from military and treasury posts; cannot exit the rule; sortition governs minor offices but not the positions where real power concentrates.
 *   - Aristocratic Military Families: Powerful/constrained — benefit from elected military posts and the training/equipment advantages that come with aristocratic status; constrained by actual performance requirements and the risk of public failure in military command.
 *   - Pericles' Faction: Institutional/arbitrage — primary beneficiary of the strategic exception; uses control of military posts to project power domestically and abroad; capable of exiting the constraint by losing elections or military leadership role.
 *   - Athenian State: Institutional/arbitrage — benefits from the constraint as a coordination mechanism; elects competent generals rather than trusting the lot with military command; sees the exception as functional necessity.
 *   - Pure Sortition Doctrine: Institutional/constrained (treating doctrine as an agent) — victim of the exception; suppressed in scope; experiences the constraint as extraction from its universal principle while remaining the governing framework for non-lethal offices.
 *   - Analytical Observer: Analytical/analytical — risks naturalizing the exception as inevitable ('competence kills, therefore sortition must have limits') rather than as a strategic political choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sortition_and_rotation__strategic_exception_reading, 0.52).
domain_priors:suppression_score(sortition_and_rotation__strategic_exception_reading, 0.62).
domain_priors:theater_ratio(sortition_and_rotation__strategic_exception_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sortition_and_rotation__strategic_exception_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(sortition_and_rotation__strategic_exception_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sortition_and_rotation__strategic_exception_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sortition_and_rotation__strategic_exception_reading, tangled_rope).
narrative_ontology:human_readable(sortition_and_rotation__strategic_exception_reading, "Sortition and Rotation: Strategic Exception Reading (Military/Treasury Posts)").
narrative_ontology:topic_domain(sortition_and_rotation__strategic_exception_reading, "legal/doctrinal/political_structure").

domain_priors:requires_active_enforcement(sortition_and_rotation__strategic_exception_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sortition_and_rotation__strategic_exception_reading, '7e87cff8-a6a2-4051-8a91-a81aab260bd7').
narrative_ontology:cs_kernel_codification('7e87cff8-a6a2-4051-8a91-a81aab260bd7', formalized).
narrative_ontology:cs_authority_grounding('7e87cff8-a6a2-4051-8a91-a81aab260bd7', lineage).
narrative_ontology:cs_interpretation_layer_present('7e87cff8-a6a2-4051-8a91-a81aab260bd7').
narrative_ontology:cs_reading_relation('7e87cff8-a6a2-4051-8a91-a81aab260bd7', sortition_and_rotation__anti_professional_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e87cff8-a6a2-4051-8a91-a81aab260bd7', sortition_and_rotation__equal_chance_reading, coexists_with).
narrative_ontology:cs_axiom('7e87cff8-a6a2-4051-8a91-a81aab260bd7', foundational, pragmatic_boundary_setting).
narrative_ontology:cs_axiom_status(pragmatic_boundary_setting, holdable).
narrative_ontology:cs_axiom_grounding('7e87cff8-a6a2-4051-8a91-a81aab260bd7', pragmatic_boundary_setting, instrumental).
narrative_ontology:cs_axiom('7e87cff8-a6a2-4051-8a91-a81aab260bd7', foundational, competence_criticality).
narrative_ontology:cs_axiom_status(competence_criticality, holdable).
narrative_ontology:cs_axiom_grounding('7e87cff8-a6a2-4051-8a91-a81aab260bd7', competence_criticality, empirically_contingent).
narrative_ontology:cs_reference_frame('7e87cff8-a6a2-4051-8a91-a81aab260bd7', pragmatic_sortition_boundary).
narrative_ontology:cs_drift_state('7e87cff8-a6a2-4051-8a91-a81aab260bd7', late_classical_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e87cff8-a6a2-4051-8a91-a81aab260bd7', '').
narrative_ontology:cs_kernel_id(sortition_and_rotation__strategic_exception_reading, sortition_and_rotation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sortition_and_rotation__strategic_exception_reading, military_competence).
narrative_ontology:constraint_beneficiary(sortition_and_rotation__strategic_exception_reading, pericles_faction).
narrative_ontology:constraint_beneficiary(sortition_and_rotation__strategic_exception_reading, elected_aristocracy).
narrative_ontology:constraint_victim(sortition_and_rotation__strategic_exception_reading, pure_sortition_doctrine).
narrative_ontology:constraint_victim(sortition_and_rotation__strategic_exception_reading, non_aristocratic_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ARISTOCRATIC CITIZEN (SNARE) — Trapped in the asymmetry that sortition governs most offices but excludes generals and treasurers. The suppression is absolute: the citizen cannot exit or change the rule. They bear the cost of strategic exclusion (military command flows to families with resources for military training and equipment) while the enforcement is structural — the rule is written into the constitution. Maximum extraction from this perspective: power that should flow to them (by sortition) flows instead to the elected few.
constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ARISTOCRATIC MILITARY FAMILY (TANGLED ROPE) — Benefits from the elected military posts (extractiveness running toward them), but constrained by the requirement to succeed — incompetence in command is fatal, literally, and public failure is career-ending. They coordinate genuine military function: training, equipment sourcing, strategic planning. The constraint serves real coordination (does not select randomly for lethal stakes) alongside asymmetric extraction (power concentrates where it does). Suppression moderate: families *can* be displaced by better performers, but the structural barrier (equipment costs, social networks, military education) is high.
constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ATHENIAN STATE / MILITARY APPARATUS (ROPE) — Solves the critical coordination problem: electing generals from a pool of capable commanders. The constraint enables effective military action without sacrificing democratic principle in non-lethal offices. The state benefits from competent military leadership and sees the exception as purely coordinating — not as extraction but as functional necessity. From this institutional perspective, the exception is a rational allocation mechanism, not an asymmetric imposition.
constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: PURE SORTITION DOCTRINE (TANGLED ROPE) — Treated as a victim here because the strategic exception suppresses the doctrine's full application. The exception extracts from the purity of sortition (reducing its scope) while the doctrine still coordinates democratic principle (the exception preserves rather than eliminates sortition for non-lethal posts). The doctrine experiences mixed cost and benefit: its authority is compromised (not universal, therefore not purely sortition-based) but its core function is preserved (preventing political class formation). The constraint is hybrid from the doctrine's perspective: partly coordinating democratic equality, partly extracting from the universality of the sortition principle.
constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: SORTITION RITUAL AND PROCEDURAL FORM (PITON) — The constraint maintains performative sortition for non-military/non-treasury posts while quietly electing the strategic roles. Over time, the exception becomes normalized and the theatrical purpose (demonstrating democratic principle via lots) persists for posts where stakes are low, even as the meaningful power (military command) concentrates in elected hands. Theater ratio is low (0.28) because this is a genuine exception with real structural function, not pure performance — but the piton classification reflects the possibility that over time, as the exception becomes implicit rather than debated, the sortition ritual for minor posts becomes increasingly performative (justifying the system without distributing real power).
constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — This perspective naturalizes the constraint as a structural law: states cannot randomly select military commanders when incompetence is fatal. The observation generalizes beyond Athens — any system claiming to govern by sortition must carve out exceptions where stakes are lethal. This reading risks a false summit: naturalizing what is actually a contingent political choice (to preserve sortition's symbolic function in low-stakes offices while excluding it from high-stakes command). The mountain classification depends on treating the exception as inevitable rather than as a strategic decision by the Athenian elite to preserve sortition symbolically while concentrating power functionally.
constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sortition_and_rotation__strategic_exception_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sortition_and_rotation__strategic_exception_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sortition_and_rotation__strategic_exception_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sortition_and_rotation__strategic_exception_reading, TR),
    TR >= 0.70.

:- end_tests(sortition_and_rotation__strategic_exception_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts power from the general citizen body and concentrates it in the elected military/treasury posts. The extractiveness is not extreme because the constraint does preserve sortition for other offices (roughly 400 posts were determined by lot) and because the election of military leaders is genuinely open to persuasion — any citizen can theoretically run and persuade the assembly. The value reflects that real power asymmetry exists (military command concentrates in families with resources for training) but is not absolute (the elected posts are genuinely competitive). Suppression (0.62): Moderate-high. Non-aristocratic citizens face structural barriers to military command: equipment costs, prior military training requirements, social networks for demonstrated competence. The suppression is enforceable through the rule itself (generals must be elected, and the assembly is persuaded by displays of competence). Over time, the measurement shows rising suppression as the exception becomes more normalized and the structural barriers more entrenched. Theater ratio (0.28): Low. This reading treats the exception as functionally motivated, not primarily performative. The election of generals genuinely serves a coordination purpose — identifying competent military leaders. The constraint is not masked theater (the exception is explicit) and is not maintained primarily for symbolic effect. The low theater distinguishes this reading from the piton perspective, which sees the sortition ritual in minor offices as increasingly performative while power concentrates in elected posts. The trajectory shows rising theater over time as the exception becomes implicit ('of course generals are elected') rather than continually justified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a deep perspectival gap rooted in the kernel contest. The non-aristocratic citizen sees snare (pure exclusion from power). The aristocratic military family sees tangled_rope (real coordination function plus asymmetric benefit). The Athenian state sees rope (coordination mechanism). The pure sortition doctrine sees tangled_rope (partially suppressed, partially preserved). The ritual itself is piton (performative sortition for minor posts). The civilizational analytical observer risks mountain (naturalizing the exception as inevitable). These are not contradictions — they are the readings that the three sibling interpretations of sortition produce. The anti_professional reading would emphasize the snare and mountain perspectives (the exception corrupts the anti-professional purpose, and competence requirements are socially constructed power grabs). The equal_chance reading would emphasize the piton and snare perspectives (the exception betrays the equal-access principle, and the sortition ritual is theater for the non-military posts). The strategic_exception reading — this one — acknowledges all perspectives but sees the tangled_rope and rope framings as primary: genuine coordination function exists alongside asymmetric power concentration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for this reading derive from the structural relationships created by the strategic exception. Non-aristocratic citizens are full targets of extraction (d ≈ 0.88, trapped exit) — they cannot access military/treasury posts and cannot change the rule. Aristocratic military families are partial beneficiaries (d ≈ 0.28, constrained exit) — they benefit from the exception but face performance pressure and can be displaced. The Athenian state is a beneficiary (d ≈ 0.12, arbitrage exit) — it solves a coordination problem and can adjust the exception if needed. The pure sortition doctrine is a victim (d ≈ 0.72, constrained exit) — the doctrine is partially suppressed but remains the governing framework for most offices. These directionality values feed the chi formula via f(d) to produce the effective extraction experienced by each agent: low chi for the state (institution with arbitrage), high chi for non-aristocratic citizens (trapped), moderate chi for aristocrats (constrained with benefit). The piton perspective derives high d (0.68) but applies low chi because theater_ratio is low — the constraint's performative component is minimal, so the experienced extraction is dampened by the low-theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that all three readings coexist as legitimate interpretations of the sortition kernel. The mandatrophy here is not 'which type is correct?' but 'which reading of sortition is Athens instantiating?' The strategic_exception_reading claims that Athens is pragmatically applying sortition (using the lot where stakes are survivable, using election where competence is critical) and sees the exception as functional rather than as a corruption of sortition's principle. This reading produces a tangled_rope structure: genuine coordination (military competence) mixed with asymmetric extraction (power concentration in elected posts). The anti_professional_reading would produce a snare structure: the exception betrays sortition's core purpose (preventing professionalization). The equal_chance_reading would produce a snare structure: the exception violates equality (different citizens have different access to power). The three readings are not resolved by empirical evidence alone (the mandatrophy is not empirically decidable) but by committing to a reading of what sortition's purpose fundamentally is. This constraint instantiates one commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lethal_stakes_definition,
    'What qualifies as ''stakes so lethal that incompetence kills''? Is this natural or socially constructed?',
    'Historical analysis of actual military failure consequences vs. perceived competence requirements; comparison with cases where sortition governed military posts (Rome''s tribunes); examination of whether the stakes were truly incomparable or socially elevated.',
    'If lethal stakes are natural/objective: the exception is structurally justified (mountain-adjacent). If constructed/socially elevated: the exception is a choice to preserve aristocratic power (snare-adjacent). The classification shifts from tangled_rope to either rope (if natural) or snare (if constructed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lethal_stakes_definition, conceptual, 'Whether military stakes are objectively lethal or socially constructed as requiring aristocratic selection').

omega_variable(
    aristocratic_competence_correlation,
    'Did aristocratic selection actually produce better military outcomes, or did the exception simply concentrate power while nominally preserving democratic form?',
    'Comparative analysis of Athenian military outcomes under elected vs hypothetical sortition-selected generals; examination of whether aristocratic advantage derived from training/equipment access (structural) or from actual talent (natural law). Cross-temporal comparison with periods of strategic failure by elected generals.',
    'If aristocratic selection was empirically superior: the exception reflects genuine competence advantage (validates the exception''s functional claim). If outcomes were similar to sortition: the exception extracts power while claiming functional necessity (validates the snare/tangled_rope reading). If outcomes were worse: the exception was purely extractive (pure snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aristocratic_competence_correlation, empirical, 'Whether elected military posts produced materially better outcomes than sortition would have').

omega_variable(
    pericles_faction_extraction,
    'Did the strategic exception primarily benefit Pericles and his faction through disproportionate military command access, or did it broadly benefit Athenian military capability?',
    'Prosopographic analysis of general elections during Periclean era; examination of whether generalships clustered in specific families and whether command concentration benefited Pericles'' political faction more than the broader Athenian state. Analysis of command decisions that served Pericles'' strategic interests vs state interests.',
    'If extraction concentrated in Pericles'' faction: the constraint primarily benefits specific individuals (clarifies the snare component). If benefits diffused across Athenian military elite: the constraint is genuinely coordinating military competence (clarifies the rope component). Shifts classification emphasis within tangled_rope typology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pericles_faction_extraction, empirical, 'Whether the strategic exception benefited Pericles'' faction or broadly served Athenian military effectiveness').

omega_variable(
    sibling_reading_contest,
    'Which sibling reading — anti_professional_reading, equal_chance_reading, or strategic_exception_reading — correctly describes the actual Athenian commitment to sortition?',
    'Doctrinal and textual analysis: What did Athenian theorists (Aristotle, Xenophon) claim about sortition''s purpose? Did they emphasize preventing professionalization, ensuring equal access, or strategic optimization? How did they justify the military exception? What evidence exists of self-conscious debate about the constraint''s true rationale?',
    'This omega documents the kernel itself — the three readings coexist in the historical record, and no single reading can claim definitive support. Each reading instantiates a different constraint structure with different beneficiary/victim configurations and different extractiveness profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_contest, conceptual, 'Which of the three contested readings (anti_professional, equal_chance, strategic_exception) represents the Athenian commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sortition_and_rotation__strategic_exception_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sort_strat_theater_t0, sortition_and_rotation__strategic_exception_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sort_strat_theater_t25, sortition_and_rotation__strategic_exception_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(sort_strat_theater_t50, sortition_and_rotation__strategic_exception_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(sort_strat_extract_t0, sortition_and_rotation__strategic_exception_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sort_strat_extract_t25, sortition_and_rotation__strategic_exception_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(sort_strat_extract_t50, sortition_and_rotation__strategic_exception_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sort_strat_supp_t0, sortition_and_rotation__strategic_exception_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sort_strat_supp_t25, sortition_and_rotation__strategic_exception_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(sort_strat_supp_t50, sortition_and_rotation__strategic_exception_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sortition_and_rotation__strategic_exception_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sortition_and_rotation__strategic_exception_reading, sortition_and_rotation__anti_professional_reading).
narrative_ontology:affects_constraint(sortition_and_rotation__strategic_exception_reading, sortition_and_rotation__equal_chance_reading).

% DUAL FORMULATION NOTE:
% The sortition_and_rotation kernel has three structurally distinct readings, each instantiating a different constraint with different ε values, beneficiary/victim configurations, and classifications. The strategic_exception_reading (this constraint) treats the military/treasury exception as pragmatically justified (ε=0.52, tangled_rope). The anti_professional_reading treats the exception as a corruption of sortition's core anti-elitist purpose (higher ε, snare-dominant). The equal_chance_reading treats the exception as a violation of equality (snare-dominant). These are not the same constraint viewed from different angles — they are different claims about what sortition's fundamental purpose is and whether the exception is justified. All three must be instantiated as separate stories linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sortition_and_rotation__strategic_exception_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
