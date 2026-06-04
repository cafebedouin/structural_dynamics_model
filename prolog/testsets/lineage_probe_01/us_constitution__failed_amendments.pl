% ============================================================================
% CONSTRAINT STORY: us_constitution__failed_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution__failed_amendments, []).

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
 *   constraint_id: us_constitution__failed_amendments
 *   human_readable: Constitutional Definition by Refused Amendments
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Constitution is defined as much by the amendments the polity
 *   considered and refused as by those it ratified. This reading claims that
 *   constitutional meaning is not exhausted by the text and ratified
 *   amendments (the conventional view) but is shaped structurally by the
 *   failed amendments — the proposals that achieved political support but
 *   could not reach the supermajority threshold required by Article V. The
 *   constraint emerges from the tension between two facts: (1) proposed
 *   amendments that reflect genuine political movements (women's suffrage,
 *   labor guarantees, child welfare, representation reform) failed
 *   ratification; (2) the formal constitutional record treats these failures
 *   as though they represent the considered judgment of the polity, when in
 *   fact they may represent the structural suppression created by the
 *   ratification threshold itself. This reading instantiates ONE
 *   interpretation of the contested kernel 'us_constitution' — other readings
 *   include the original_constitution_1787 (the unamended text is complete),
 *   bill_of_rights_1791 (the Constitution required the Bill of Rights to
 *   achieve legitimacy), later_amendment_eras (the Constitution's meaning is
 *   set by successive waves of formal revision), and
 *   pre_constitutional_frameworks (the Constitution replaced confederal
 *   compacts). The failed-amendments reading differs from these by attending
 *   to what was NOT ratified as constitutive of constitutional meaning.
 *
 * KEY AGENTS:
 *   - Constituencies denied failed amendments (powerless/trapped) — women denied suffrage until 1920, Black citizens denied equal protection guarantees in 19th century, labor advocates denied workplace guarantees, indigenous peoples denied representation. Structurally suppressed by the very fact of non-ratification.
 *   - Status quo beneficiaries and state governments (institutional/arbitrage) — slaveholding states, industrial capital, patriarchal property regimes, segregationist states. Benefit from suppression of unwanted constitutional changes. Experience constraint as low-extraction coordination of veto power.
 *   - Reform coalitions across eras (moderate/constrained) — suffragists, abolitionists, labor organizers, civil rights advocates. Face asymmetric mobilization costs and supermajority threshold. Experience constraint as tangled coordination/extraction.
 *   - Constitutional governance institutions (organized/constrained) — Congress, state legislatures, courts, ratification conventions. Benefit from stable amendment process but suffer under supermajority requirement when reform majorities form.
 *   - Constitutional scholarship and civic education (institutional/arbitrage) — legal scholars, judges, schools maintaining the mythology that the Constitution is 'what it says' (implicitly: what was ratified), not what was refused.
 *   - Analytical observer (analytical/analytical) — cross-temporal perspective seeing the structure of refusal as either natural law or institutional design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution__failed_amendments, 0.52).
domain_priors:suppression_score(us_constitution__failed_amendments, 0.68).
domain_priors:theater_ratio(us_constitution__failed_amendments, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution__failed_amendments, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution__failed_amendments, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_constitution__failed_amendments, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution__failed_amendments, tangled_rope).
narrative_ontology:human_readable(us_constitution__failed_amendments, "Constitutional Definition by Refused Amendments").
narrative_ontology:topic_domain(us_constitution__failed_amendments, "political/legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution__failed_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution__failed_amendments, '319f81bc-e6db-47fc-b292-936464213687').
narrative_ontology:cs_kernel_codification('319f81bc-e6db-47fc-b292-936464213687', formalized).
narrative_ontology:cs_authority_grounding('319f81bc-e6db-47fc-b292-936464213687', lineage).
narrative_ontology:cs_interpretation_layer_present('319f81bc-e6db-47fc-b292-936464213687').
narrative_ontology:cs_reading_relation('319f81bc-e6db-47fc-b292-936464213687', us_constitution__original_constitution_1787, coexists_with).
narrative_ontology:cs_reading_relation('319f81bc-e6db-47fc-b292-936464213687', us_constitution__bill_of_rights_1791, coexists_with).
narrative_ontology:cs_reading_relation('319f81bc-e6db-47fc-b292-936464213687', us_constitution__later_amendment_eras, influences).
narrative_ontology:cs_reading_relation('319f81bc-e6db-47fc-b292-936464213687', us_constitution__pre_constitutional_frameworks, coexists_with).
narrative_ontology:cs_axiom('319f81bc-e6db-47fc-b292-936464213687', foundational, refusal_constitutes_meaning).
narrative_ontology:cs_axiom_status(refusal_constitutes_meaning, holdable).
narrative_ontology:cs_axiom_grounding('319f81bc-e6db-47fc-b292-936464213687', refusal_constitutes_meaning, conventional).
narrative_ontology:cs_axiom('319f81bc-e6db-47fc-b292-936464213687', foundational, ratification_threshold_is_contingent_design).
narrative_ontology:cs_axiom_status(ratification_threshold_is_contingent_design, holdable).
narrative_ontology:cs_axiom_grounding('319f81bc-e6db-47fc-b292-936464213687', ratification_threshold_is_contingent_design, empirically_contingent).
narrative_ontology:cs_reference_frame('319f81bc-e6db-47fc-b292-936464213687', amendment_contestation_framework).
narrative_ontology:cs_drift_state('319f81bc-e6db-47fc-b292-936464213687', contemporary_constitutional_scholarship, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('319f81bc-e6db-47fc-b292-936464213687', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution__failed_amendments, us_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution__failed_amendments, ratified_status_quo_beneficiaries).
narrative_ontology:constraint_victim(us_constitution__failed_amendments, constituencies_with_failed_guarantees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUENCIES DENIED FAILED AMENDMENTS (SNARE) — Groups whose proposed constitutional guarantees failed ratification (women seeking suffrage 1878–1920, Black citizens seeking equal protection in 19th century, indigenous peoples, working-class advocates for labor guarantees) remain trapped in the structural absence of those protections. The suppression is enforced by the very fact of non-ratification — the refused amendment defines what the Constitution does NOT protect. No exit from this constraint; it structures their legal status generationally.
constraint_indexing:constraint_classification(us_constitution__failed_amendments, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUCCESSIVE REFORM COALITIONS (TANGLED ROPE) — Groups organizing to ratify new amendments (suffragists, abolitionists, civil rights advocates) experience genuine coordination — they must mobilize sufficient support to achieve ratification. But the constraint also extracts from them: the requirement for supermajority ratification (3/5 of states) tilts power toward the status quo and toward states with concentrated beneficiaries from the refusal. Reform coalitions benefit from the amendment framework (it is their only constitutional pathway) but face asymmetric costs in persuasion and mobilization.
constraint_indexing:constraint_classification(us_constitution__failed_amendments, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATUS QUO BENEFICIARIES AND STATE GOVERNMENTS (ROPE) — Entities that benefit from the current constitutional allocation (slaveholding states, industrial-era capital, patriarchal property structures, segregationist states) experience the failed-amendment constraint as pure coordination: they coordinate their veto power through the ratification threshold, preventing unwanted change. For these agents, the constraint is low-extraction coordination of common interests. They have arbitrage options — if federal constitutional protection were granted, they could lobby for carve-outs or exemptions — but they can exercise veto power here without paying those costs.
constraint_indexing:constraint_classification(us_constitution__failed_amendments, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL AMENDMENT GOVERNANCE INSTITUTIONS (TANGLED ROPE) — Congress, state legislatures, and ratification conventions experience this constraint as both coordination mechanism (Articles V provides a formal pathway for revision) and extraction mechanism (the supermajority threshold concentrates power, making changes expensive and slow). These institutions benefit from having a stable constitutional process (they are the guardians of it), but they also suffer under it — reform-minded majorities in Congress are structurally impotent if supermajority coalitions cannot be built. Institutional agents face genuine coordination tasks and genuine asymmetric costs.
constraint_indexing:constraint_classification(us_constitution__failed_amendments, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL MYTHOLOGY AND CIVIC RITUAL (PITON) — The narrative that failed amendments represent the polity's considered refusal (rather than the structural suppression created by the ratification threshold) has become largely performative. Civics education teaches that the Constitution's meaning is set by its text and ratified amendments, not by what was refused. The mythology persists through institutional repetition — courts invoke the 'original understanding' as though only ratified amendments matter, schools teach the Constitution as a completed document rather than an ongoing contested field. The piton classification reflects theater_ratio (0.35): the narrative is relatively low-theater (constitutional scholars and jurists genuinely engage with the question), but the operative function — preventing consideration of what the refusal reveals about structural power — is largely ceremonial.
constraint_indexing:constraint_classification(us_constitution__failed_amendments, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the refusal of amendments may appear structurally immutable: once a polity refuses an amendment, that refusal enters the historical record as a fact about what the polity was willing to accept. The constraint appears natural — the constitutional record simply is what it is. However, the structural data contradicts the mountain classification. The 'fact' of refusal is not natural law; it is the outcome of a specific institutional structure (the supermajority requirement, the state-based ratification process, the geographic concentration of veto power). The engine will detect this as a false summit: what appears immutable from a civilizational timescale is actually the product of contingent institutional design that benefits identifiable agents.
constraint_indexing:constraint_classification(us_constitution__failed_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution__failed_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution__failed_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution__failed_amendments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution__failed_amendments, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution__failed_amendments, TR),
    TR >= 0.70.

:- end_tests(us_constitution__failed_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint preserves asymmetric power through the structural fact of refusal. Status quo beneficiaries captured value by preventing unwanted constitutional expansion from 1787 forward — the 19th Amendment (women's suffrage) was proposed in 1878 but not ratified until 1920 (42-year lag). The extractiveness is not as severe as a pure snare (which would be ~0.70+) because the reform coalitions did eventually succeed with some amendments, and the constraint is functionally a coordination mechanism (Article V is available). But it is high enough to mark tangled rope because the supermajority requirement creates genuine asymmetry: status quo forces need only 13 states to block any change, while reformers need 36 to mandate it (under historical ratios). Suppression (0.68): High. The constraint actively suppresses alternatives — refused amendments are not merely 'not adopted,' they are structurally absent from the constitution's operative protection. The suppression is enforced through institutional design (the ratification threshold). This is not suppression in the sense of violent coercion, but suppression as structural impossibility: certain changes, even when commanding majority support in Congress and a significant portion of states, cannot be realized. Theater ratio (0.35): Low-to-moderate. The failed-amendments reading is not primarily a theater about what the Constitution is; it is a methodological claim about what constitutional interpretation must attend to. The constraint has real structural work: the refusal shapes what protections exist and who bears the costs. Theater has increased over time as constitutional mythology (the 'complete document' framing) has become more institutionalized in legal education and judicial practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a severe perspectival gap between the powerless/trapped perspective (snare: pure suppression through structural impossibility) and the institutional/arbitrage perspective (rope: low-extraction coordination of veto). The same constraint — the requirement for supermajority ratification — appears as entrapment to reform constituencies unable to achieve constitutional change and as elegant coordination mechanism to status quo forces exercising veto. The analytical observer risks naturalizing this asymmetry as an immutable constitutional feature (mountain), when in fact it is the product of specific institutional design (the 3/5 threshold, the state-based ratification mechanism, the electoral college's bias toward lower-population states). The piton perspective reveals that the mythology of the 'completed Constitution' is largely performative — courts and schools teach that constitutional meaning is fixed in ratified text, not by attending to refused amendments, which would foreground the constraint's extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Status quo beneficiaries (institutional/arbitrage) have d ≈ 0.05–0.15: they benefit from non-change and can exercise veto power without friction. Constituencies denied failed amendments (powerless/trapped) have d ≈ 0.95: they bear the full cost of the suppression. Reform coalitions (moderate/constrained) have d ≈ 0.65–0.75: they face mobilization asymmetry but retain some agency through the amendment process. The engine derives these d values from the beneficiary/victim declarations and exit options; no override is needed because the structural data is clear: refusal benefits status quo, extracts from reform constituencies.
 *
 * MANDATROPHY ANALYSIS:
 *   The failed-amendments constraint resolves the mandatrophy by showing that the perspectival gap IS the mandatrophy. The same institutional design (Article V's supermajority requirement) is experienced as pure coordination by beneficiaries and as pure extraction by those denied. The constraint is tangled rope precisely because it performs both functions simultaneously from different perspectives. The false summit (the mountain view) is the most dangerous classification here — if failed amendments are treated as expressions of the polity's 'considered judgment' rather than as artifacts of institutional design, the constraint's extractive structure becomes invisible. The failed-amendments reading resists this false summit by foregrounding the refused alternatives: the moment you ask 'what would the Constitution look like under a lower ratification threshold?' the machinery of suppression becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refusal_vs_non_consideration,
    'Does a failed amendment represent the polity''s considered refusal, or the structural impossibility of bringing certain proposals to successful ratification votes?',
    'Historical analysis of failed amendment proposals: did the polity actively debate and reject them, or were they never mobilized for ratification because their failure was structurally predetermined? Track amendment campaigns that achieved majority support in Congress but failed in state ratification.',
    'If true refusal: the failed amendments are legitimate data about constitutional limits on the polity''s willingness to change. If structural impossibility: the failed amendments reveal veto power concentration, not democratic judgment. Classification shifts from rope (coordinated rejection) to snare (structural suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refusal_vs_non_consideration, empirical, 'Whether failed amendments represent considered refusal or structural impossibility of ratification').

omega_variable(
    counterfactual_amendment_ratification,
    'If the ratification threshold had been lower (simple majority, or proportional ratification), which failed amendments would have succeeded?',
    'Simulation of historical amendment campaigns under alternative ratification rules; analysis of state-by-state voting patterns and estimated probability of success under different thresholds. Historical case: the 19th Amendment (women''s suffrage) passed with 36 ratifications in 1920 when 36 were required (3/5 of 48 states). Counterfactual: would it have passed earlier under a lower threshold?',
    'If many failed amendments had succeeded under lower thresholds: the constraint is institutional design (extractive for reform coalitions, beneficial for status quo). If few would have succeeded regardless: the constraint reflects genuine constitutional limits. Extractiveness interpretation shifts from 0.52 (institutional design) to 0.25 (institutional expression of deeper consensus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_amendment_ratification, empirical, 'Counterfactual ratification likelihood under alternative thresholds').

omega_variable(
    kernel_reading_ambiguity,
    'Is the failed-amendments reading a claim about the Constitution''s actual meaning (what it is), or a methodological claim about how to interpret it (how to read it)?',
    'Distinguish between: (A) the Constitution''s operative meaning IS set by refused amendments as much as ratified ones (descriptive claim about what the document means), versus (B) interpreting the Constitution requires attending to failed amendments as methodological context (interpretive practice, not substance). The two have different epistemological status and different implications for classification.',
    'If (A): the constraint is structural — refused amendments are part of the constitutional order. If (B): the constraint is epistemic — failed amendments reveal structural biases in interpretation. The two readings have different relationships to beneficiary/victim structure and may warrant different extractiveness values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether failed-amendments reading is ontological or methodological').

omega_variable(
    sibling_reading_foreclosure,
    'Does the failed-amendments reading foreclose any of its siblings (original_constitution_1787, bill_of_rights_1791, later_amendment_eras)?',
    'Test logical consistency: can a single framework hold both failed-amendments reading AND (for each sibling) the sibling''s core premise? If any sibling''s premise is logically incompatible with the failed-amendments claim, the relation is forecloses. Otherwise, coexists_with or influences.',
    'Affects cs_structure.reading_relations declarations. If any sibling is foreclosed, the reading exerts hard logical constraint on the kernel interpretation space. If all coexist, the kernel admits multiple incompatible readings held by different interpretive communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Foreclosure relations between this reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution__failed_amendments, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1787_low, us_constitution__failed_amendments, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_19th_century_rise, us_constitution__failed_amendments, theater_ratio, 50, 0.32).
narrative_ontology:measurement(theater_20th_century_plateau, us_constitution__failed_amendments, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(extractiveness_1787_baseline, us_constitution__failed_amendments, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extractiveness_19th_century_rise, us_constitution__failed_amendments, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(extractiveness_20th_century_plateau, us_constitution__failed_amendments, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1787_baseline, us_constitution__failed_amendments, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(suppression_19th_century_intensification, us_constitution__failed_amendments, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(suppression_20th_century_stable, us_constitution__failed_amendments, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution__failed_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution__failed_amendments, bill_of_rights_1791).
narrative_ontology:affects_constraint(us_constitution__failed_amendments, later_amendment_eras).
narrative_ontology:affects_constraint(us_constitution__failed_amendments, original_constitution_1787).
narrative_ontology:affects_constraint(us_constitution__failed_amendments, pre_constitutional_frameworks).

% DUAL FORMULATION NOTE:
% The failed-amendments reading is one of five constraints jointly interpreting the contested kernel 'us_constitution'. Each sibling constraint has its own ε value: original_constitution_1787 is likely mountain (ε ≤ 0.25); bill_of_rights_1791 is likely rope or tangled_rope; later_amendment_eras is likely rope (ε ≤ 0.35); pre_constitutional_frameworks is likely rope. The failed-amendments reading (ε = 0.52) foregrounds the extraction mechanism hidden when other readings dominate. No single reading is 'correct' — they are simultaneous interpretations held by different legal and political communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
