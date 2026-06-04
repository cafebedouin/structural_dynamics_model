% ============================================================================
% CONSTRAINT STORY: ostracism_institution__safety_valve_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ostracism_institution__safety_valve_reading, []).

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
 *   constraint_id: ostracism_institution__safety_valve_reading
 *   human_readable: Ostracism as Safety Valve (Reading: Conflict De-Escalation Mechanism)
 *   domain: legal/constitutional/athenian_democracy
 *
 * SUMMARY:
 *   Ostracism in classical Athens (ca. 487–417 BCE) was a legal procedure
 *   enabling the assembly to exile a single individual for ten years without
 *   trial, without confiscation, without legal disgrace, and with scheduled
 *   return. This constraint story instantiates the safety_valve_reading:
 *   ostracism as a mechanism for breaking elite factional deadlock before
 *   factional conflict escalated to violence, oligarchy, or the overthrow of
 *   democracy itself. The reading interprets ostracism as a coordination
 *   mechanism that solves the structural problem of how a popular assembly
 *   can manage elite factions without becoming either tyrannical (permanent
 *   exile, confiscation, death sentences) or passive (allowing factions to
 *   escalate to civil conflict). The mechanism is minimal-extraction by
 *   design: it removes one factional leader for a bounded period, preserves
 *   their property and eventual repatriation, and explicitly avoids legal
 *   condemnation. The beneficiary is the athenian civic polity itself; the
 *   victim set is one principal per crisis, and the extraction is gentle —
 *   exile without disgrace or confiscation. The safety_valve_reading competes
 *   with two sibling readings: the arbitrary_exile_reading (ostracism as
 *   democracy's capacity to mobilize popular sentiment for arbitrary removal
 *   of any prominent figure) and the elite_competition_regulator_reading
 *   (ostracism as the demos serving as referee among elite rivalries not of
 *   the demos' own choosing). These three readings cannot all be true for the
 *   same voting decision; they represent incommensurable interpretations of
 *   ostracism's institutional function.
 *
 * KEY AGENTS:
 *   - Athenian Civic Polity (institutional/mobile): Beneficiary of factional de-escalation. Experiences ostracism as coordination mechanism.
 *   - Factional Leadership Under Threat (moderate/constrained): Victim per crisis, but gentle victim. Exiled for ten years with property preserved and scheduled return.
 *   - Democratic Assembly (moderate/trapped): Voter and mechanism operator. Trapped by the structural need to manage factional conflict; experiences ostracism as functional coordination.
 *   - Oligarchic Factions (organized/constrained): Secondary beneficiaries when their rivals are ostracized; constrained by the threat of being ostracized themselves.
 *   - Analytical Observer (analytical/analytical): Comparative constitutional theorist assessing whether ostracism solves the problem it exists to solve (elite factional de-escalation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ostracism_institution__safety_valve_reading, 0.18).
domain_priors:suppression_score(ostracism_institution__safety_valve_reading, 0.35).
domain_priors:theater_ratio(ostracism_institution__safety_valve_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ostracism_institution__safety_valve_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(ostracism_institution__safety_valve_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ostracism_institution__safety_valve_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ostracism_institution__safety_valve_reading, rope).
narrative_ontology:human_readable(ostracism_institution__safety_valve_reading, "Ostracism as Safety Valve (Reading: Conflict De-Escalation Mechanism)").
narrative_ontology:topic_domain(ostracism_institution__safety_valve_reading, "legal/constitutional/athenian_democracy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ostracism_institution__safety_valve_reading, '008cec52-cf2f-45e0-b057-6f7a6d164c15').
narrative_ontology:cs_kernel_codification('008cec52-cf2f-45e0-b057-6f7a6d164c15', formalized).
narrative_ontology:cs_authority_grounding('008cec52-cf2f-45e0-b057-6f7a6d164c15', lineage).
narrative_ontology:cs_interpretation_layer_present('008cec52-cf2f-45e0-b057-6f7a6d164c15').
narrative_ontology:cs_reading_relation('008cec52-cf2f-45e0-b057-6f7a6d164c15', ostracism_institution__arbitrary_exile_reading, coexists_with).
narrative_ontology:cs_reading_relation('008cec52-cf2f-45e0-b057-6f7a6d164c15', ostracism_institution__elite_competition_regulator_reading, coexists_with).
narrative_ontology:cs_axiom('008cec52-cf2f-45e0-b057-6f7a6d164c15', foundational, ostracism_minimal_extraction_by_design).
narrative_ontology:cs_axiom_status(ostracism_minimal_extraction_by_design, holdable).
narrative_ontology:cs_axiom_grounding('008cec52-cf2f-45e0-b057-6f7a6d164c15', ostracism_minimal_extraction_by_design, deontological).
narrative_ontology:cs_axiom('008cec52-cf2f-45e0-b057-6f7a6d164c15', foundational, factional_escalation_prevention_mechanism).
narrative_ontology:cs_axiom_status(factional_escalation_prevention_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('008cec52-cf2f-45e0-b057-6f7a6d164c15', factional_escalation_prevention_mechanism, instrumental).
narrative_ontology:cs_reference_frame('008cec52-cf2f-45e0-b057-6f7a6d164c15', athenian_democratic_stability).
narrative_ontology:cs_drift_state('008cec52-cf2f-45e0-b057-6f7a6d164c15', fourth_century_abandonment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('008cec52-cf2f-45e0-b057-6f7a6d164c15', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(ostracism_institution__safety_valve_reading, ostracism_institution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ostracism_institution__safety_valve_reading, athenian_civic_polity).
narrative_ontology:constraint_beneficiary(ostracism_institution__safety_valve_reading, democratic_assembly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OSTRACIZED INDIVIDUAL (ROPE) — Exiled for ten years without confiscation or legal disgrace. The constraint coordinates the demos' fear of factional collapse with the exile subject's property rights and eventual repatriation. The exile is painful (constrained by forced geographic separation) but not maximal extraction — the property remains intact, return is scheduled, and disgrace is explicitly withheld. The individual experiences this as a coordination mechanism that extracts their presence but preserves their status and wealth. At biographical time horizon, the individual classifies this as rope, not snare, because the mechanism is transparent and has a known endpoint.
constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: ATHENIAN DEMOS / COLLECTIVE ASSEMBLY (ROPE) — Coordinating the demos' collective fear of elite factional war with the need to remove one factional leader without creating a martyr or driving rivals toward violent escalation. The constraint is pure coordination: the assembly votes to remove a threat to civic peace without condemning the threat, returning the exiled party after a fixed term. The demos is trapped in the position (cannot exit the need for periodic de-escalation), but experiences the mechanism itself as functional coordination rather than extraction. Rope classification reflects the genuine coordination function: the problem being solved (factional deadlock) is solved by the mechanism (temporary removal with property preservation).
constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: DEMOCRATIC INSTITUTIONAL STRUCTURE (ROPE) — From the institutional perspective operating at generational time scale, ostracism coordinates three structural demands: (1) the assembly's need to manage elite factional conflict, (2) the rule of law requirement that exile not be permanent or disgraceful, and (3) the franchise requirement that the demos retain agency to reverse the exile decision (implicitly, by failing to renew the practice after ten years). The constraint is lightweight coordination machinery. Extractiveness is minimal by design — the institution explicitly preserves property rights and schedules return. At generational scale, the institution is mobile: Athens could abandon ostracism if the factional pressure diminished, and the constraint enables rather than prevents that transition.
constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: OLIGARCHIC FACTION UNDER THREAT (SCAFFOLD) — A factional leadership confronting the assembly's vote to ostracize them experiences the constraint as temporary containment with a known endpoint (ten years). The organizing faction has agency (constrained but not trapped) — they can prepare for return during exile, maintain correspondence, build anticipation of their return during the ten-year interval. The constraint functions as a safety valve that prevents violent escalation to assassination or coup while keeping the factional option alive. From this perspective, ostracism is not permanent exile but temporary removal from the competitive arena with scheduled re-entry. Scaffold classification reflects the bounded duration and the active agency available during exile (organization, planning, return preparation).
constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COMPARATIVE CONSTITUTIONAL ANALYSIS (ROPE) — From the analytical perspective, ostracism coordinates the permanent structural tension in democracy between elite factional conflict and popular sovereignty. The constraint enables the demos to manage elite threats without creating permanent legal categories (banned persons, classes of exile), without permanent confiscation (which would create revenge incentives), and without violence (which would create cycles of retaliation). The mechanism is pure coordination: it solves the problem it exists to solve (breaking factional deadlock before collapse into violence or oligarchy). Extractiveness is minimal by design. The constraint appears as rope from this perspective: a coordination mechanism that extracts no one and benefits the civic polity as a whole.
constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ostracism_institution__safety_valve_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ostracism_institution__safety_valve_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ostracism_institution__safety_valve_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The mechanism explicitly preserves property rights and schedules return after ten years, minimizing structural extraction from the exile subject. The extractiveness reflects the cost of geographic exile and political exclusion during the ten-year period, but not confiscation or permanent loss of status. The measurement shows stable extractiveness over the interval (0.10 → 0.18), reflecting stability in the mechanism's design. Suppression (0.35): Moderate. The assembly must suppress factional escalation by removing one leader, creating an implicit threat to any faction that becomes too dominant. But suppression is not maximal because the mechanism does not create permanent legal categories, does not confiscate property, and does not prevent return or re-entry into politics. Suppression reflects the constraint that factional leaders face — the threat of ostracism — rather than severe coercion. Theater ratio (0.42): Moderate. The ostracism vote involves performative elements (assembly speechmaking, persuasion rituals, ballot procedures), but the core mechanism is functional: it removes a factional threat and breaks deadlock. The mechanism is not primarily performative; the theater is secondary to the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The safety_valve_reading produces high convergence across perspectives toward rope classification. The powerless exile subject, the trapped assembly, the mobile institutional structure, and the analytical observer all classify ostracism as rope (pure coordination). The organized faction under threat classifies it as scaffold (temporary removal with return preparation). This perspectival convergence on rope distinguishes the safety_valve_reading from its sibling readings: the arbitrary_exile_reading would produce snare from the exile subject's perspective (arbitrary power undressed); the elite_regulation_reading would produce tangled_rope (genuine assembly agency mixed with serving elite factional interests). The safety_valve_reading's perspectival gap is minimal — all perspectives agree on coordination function — whereas the sibling readings would produce larger gaps between beneficiary and victim perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The safety_valve_reading interprets ostracism as a coordination mechanism where directionality is symmetric across perspectives. The athenian demos (moderate/trapped) and the ostracized individual (powerless/constrained) both experience the mechanism as coordination, not extraction. The beneficiary (civic peace) is abstract but real — the polity's structural stability. The victim is the exile subject, but the victimhood is bounded: ten years, property preserved, return scheduled, no legal disgrace. Directionality derives from the mechanism's explicit design to minimize extraction while achieving coordination. The reading's coherence depends on beneficiary/victim declarations and exit option differentiation working together: the assembly is trapped in the need for factional de-escalation; the exile subject is constrained by ten-year exile but not trapped by permanent loss; the civic polity has mobile options (abandoning ostracism if factional pressure diminishes). Each agent's d value reflects their structural position relative to this specific coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   Extractiveness (0.18) places ostracism below the mandatrophy threshold (> 0.46) where high-extraction constraints require explicit resolution. The safety_valve_reading resolves the mandatrophy implicitly: if ostracism is a coordination mechanism (rope), not an extraction mechanism (snare/tangled_rope), then there is no mandatrophy to resolve. The mechanism's low extractiveness follows from its design: property preservation, scheduled return, no legal disgrace, bounded duration. The reading's coherence depends on this low extractiveness being intentional design rather than accident or cover story. The empirical resolution mechanism addresses this: if ostracism votes correlate with factional escalation crises (supporting safety_valve hypothesis), the low extractiveness is explained by design intent. If votes are arbitrary (supporting arbitrary_exile_reading), the low extractiveness might be accidental or vestigial rather than intentional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_instantiation_contest,
    'Is ostracism structurally a safety valve (de-escalation mechanism) or an arbitrary exile procedure (demo power undressed) or elite competition regulation (demos as referee of factional rivalries)?',
    'Comparative historical analysis: (1) empirical frequency of ostracism use as factional de-escalation vs arbitrary political removal; (2) correlation between ostracism votes and factional escalation crises; (3) analysis of justifications offered in assembly speeches (safety-valve framing vs arbitrary power framing vs factional regulation framing); (4) examination of non-exiled alternatives available to the assembly and why ostracism was chosen instead.',
    'If the safety-valve reading dominates: ostracism is rope (pure coordination). If the arbitrary-exile reading dominates: ostracism is snare (demos power undressed). If elite-regulation reading dominates: ostracism is tangled_rope (demos has genuine agency as referee but serves elite factional interests). The three readings cannot all be simultaneously true for the same voting decision — they represent incommensurable interpretations of the same procedural mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_contest, empirical, 'Structural function of ostracism: safety valve vs arbitrary exile vs elite regulation').

omega_variable(
    kernel_reading_specification,
    'Which reading of the ostracism kernel is this story instantiating, and how does it relate to the sibling readings?',
    'This story instantiates the safety_valve_reading: ostracism as a conflict de-escalation mechanism designed to break elite factional deadlock before it breaks the city. The sibling readings (arbitrary_exile_reading and elite_competition_regulator_reading) represent alternative structural interpretations of the same procedural mechanism. The three readings coexist as competing claims about the same institution, held by different historians and constitutional theorists.',
    'The safety_valve reading emphasizes: (1) minimal extractiveness by design (property preserved, return scheduled), (2) civic-peace beneficiary, (3) single-principal victim per crisis, (4) suppression of factional escalation rather than arbitrary domination. If this reading is correct, ostracism is rope. If the sibling readings are correct, ostracism is snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_specification, conceptual, 'Selection among incommensurable readings of the ostracism kernel').

omega_variable(
    property_preservation_scope,
    'Does the explicit preservation of property rights and scheduled return fundamentally change the classification, or does extractiveness derive primarily from the exile itself regardless of property status?',
    'Comparative analysis: (1) subjective reports from exiled individuals (via Plutarch, Aristotle, inscription evidence) about whether property preservation affected their experience of extraction; (2) political economy analysis of whether the ten-year exile period was economically manageable given preserved property and estate management by family; (3) examination of whether returnees re-entered politics with the same resource base as pre-exile.',
    'If property preservation and scheduled return are material to the classification: extractiveness is low (rope classification supported). If exile itself is the primary extraction regardless of property preservation: extractiveness is higher (snare or tangled_rope classification). The safety_valve_reading depends structurally on property preservation being material.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_preservation_scope, empirical, 'Impact of property preservation on experienced extractiveness of ostracism').

omega_variable(
    factional_escalation_hypothesis,
    'Is there empirical evidence that ostracism votes occurred at moments of factional escalation (supporting safety-valve hypothesis) or at arbitrary times independent of factional conflict intensity (supporting arbitrary-exile hypothesis)?',
    'Timeline analysis: (1) construction of a chronology of factional tensions (from Thucydides, Aristotle, inscription evidence, architectural/military records); (2) mapping of ostracism votes onto this timeline; (3) statistical correlation between vote frequency and conflict intensity; (4) examination of whether factional leaderships explicitly mobilized ostracism as de-escalation mechanism (as opposed to opportunistic rival-removal).',
    'If ostracism votes correlate strongly with escalation crises: safety_valve_reading is empirically supported. If votes are independent of factional intensity: arbitrary_exile_reading or elite_regulation_reading more plausible. The safety_valve reading''s core claim depends on this correlation existing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factional_escalation_hypothesis, empirical, 'Correlation between ostracism votes and factional escalation crises').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ostracism_institution__safety_valve_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ostracism_sv_theater_t0, ostracism_institution__safety_valve_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ostracism_sv_theater_t5, ostracism_institution__safety_valve_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(ostracism_sv_theater_t10, ostracism_institution__safety_valve_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(ostracism_sv_extractiveness_t0, ostracism_institution__safety_valve_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ostracism_sv_extractiveness_t5, ostracism_institution__safety_valve_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(ostracism_sv_extractiveness_t10, ostracism_institution__safety_valve_reading, base_extractiveness, 10, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(ostracism_sv_suppression_t0, ostracism_institution__safety_valve_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ostracism_sv_suppression_t5, ostracism_institution__safety_valve_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ostracism_sv_suppression_t10, ostracism_institution__safety_valve_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ostracism_institution__safety_valve_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ostracism_institution__safety_valve_reading, ostracism_institution__arbitrary_exile_reading).
narrative_ontology:affects_constraint(ostracism_institution__safety_valve_reading, ostracism_institution__elite_competition_regulator_reading).

% DUAL FORMULATION NOTE:
% The ostracism kernel admits three structurally distinct readings with different ε values and classifications. This story (safety_valve_reading) presents ostracism as low-extraction rope (ε=0.18). The arbitrary_exile_reading would present ostracism as higher-extraction snare (ε≈0.45–0.55); the elite_competition_regulator_reading would present ostracism as tangled_rope (ε≈0.35–0.45). All three stories address the same historical procedure but instantiate different structural interpretations of its function. The three readings coexist as competing claims about the ostracism kernel; they are linked by network.affects_constraints to enable comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
