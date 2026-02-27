% ============================================================================
% CONSTRAINT STORY: suanne_face_restoration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suanne_face_restoration, []).

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
 *   constraint_id: suanne_face_restoration
 *   human_readable: The Coup of Positive Politeness: SuAnne's Face Restoration Strategy
 *   domain: social/interpersonal_dynamics
 *
 * SUMMARY:
 *   In 1988, during a high school basketball game, SuAnne Marie Big Crow, a
 *   Lakota athlete, faced a hostile crowd engaging in racial taunts and
 *   mockery. Rather than responding with confrontation or silence, SuAnne
 *   performed a strategic act of positive politeness: she adopted the
 *   cheerleader role, performing for the crowd, treating their hostility as
 *   an invitation to dance and entertain. This move — restoring the hostile
 *   crowd's face by performing competence and belonging — has been celebrated
 *   as a triumph of dignity and grace. The Deferential Realism framework
 *   reveals this constraint's deeper structure: it is simultaneously a
 *   coordination mechanism (the hostile environment becomes manageable
 *   through performance), an extraction mechanism (SuAnne's agency and
 *   dignity are subordinated to white comfort), a ritual performance (the
 *   entire dynamic validates the system as 'working'), and a paradox of
 *   positive politeness (affirming the hostile crowd's humanity while their
 *   humanity is being expressed through racial animus). The constraint
 *   exhibits all six classification types from different structural
 *   positions, making it a diagnostic exemplar for how interpersonal dynamics
 *   can embed and perpetuate systemic inequality.
 *
 * KEY AGENTS:
 *   - SuAnne Marie Big Crow: Primary target (powerless/trapped) — faces hostility with no exit option; performance is coerced appeasement
 *   - Lakota Community: Primary victim (powerless/trapped) — constrained by racial hierarchy embedded in school institutions
 *   - Hostile Crowd Members: Dual role (moderate/constrained) — both enforcers of racial dominance and victims of rigid racial scripts
 *   - School Athletic System: Institutional beneficiary (institutional/arbitrage) — benefits from constraint that reproduces hierarchy while appearing to be managed
 *   - Racial Civility Performance: Institutional ritual (institutional/arbitrage) — perpetuates itself through theater; validates system as functional
 *   - Analytical Observer: Civilizational view (analytical/analytical) — can see the constraint's dual function (coordination + extraction) as simultaneously true
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suanne_face_restoration, 0.38).
domain_priors:suppression_score(suanne_face_restoration, 0.68).
domain_priors:theater_ratio(suanne_face_restoration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suanne_face_restoration, extractiveness, 0.38).
narrative_ontology:constraint_metric(suanne_face_restoration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(suanne_face_restoration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suanne_face_restoration, tangled_rope).
narrative_ontology:human_readable(suanne_face_restoration, "The Coup of Positive Politeness: SuAnne's Face Restoration Strategy").
narrative_ontology:topic_domain(suanne_face_restoration, "social/interpersonal_dynamics").

domain_priors:requires_active_enforcement(suanne_face_restoration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suanne_face_restoration, suanne_big_crow).
narrative_ontology:constraint_beneficiary(suanne_face_restoration, lakota_team_dignity).
narrative_ontology:constraint_beneficiary(suanne_face_restoration, hostile_crowd_face_restoration).
narrative_ontology:constraint_victim(suanne_face_restoration, lakota_community_autonomy).
narrative_ontology:constraint_victim(suanne_face_restoration, suanne_individual_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUANNE (SNARE) — Trapped in a hostile environment with no exit. The taunting crowd has set a constraint: remain silent (accept humiliation) or respond (escalate). SuAnne's extraction is maximal because she bears the full social cost of the crowd's racial animus while having no structural recourse. d≈0.98, f(d)≈1.40, σ=0.8 (local scope) → χ≈0.43.
constraint_indexing:constraint_classification(suanne_face_restoration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LAKOTA COMMUNITY (SNARE) — Trapped in a system of racial hierarchy embedded in school sports. The community cannot exit the constraint (their children attend school, sports are a path to scholarships and dignity). The constraint suppresses alternative narratives about Lakota competence and worth. d≈0.92, f(d)≈1.35, σ=0.9 (regional) → χ≈0.47.
constraint_indexing:constraint_classification(suanne_face_restoration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: HOSTILE CROWD (TANGLED ROPE) — The crowd is both victim and enforcer of a racial hierarchy constraint. They benefit from the social coordination of whiteness (unquestioned dominance in their own social space) but are also trapped by rigid racial scripts that prevent genuine interaction. SuAnne's face-restoration move constrains their options: they can escalate (lose moral standing) or accept the gesture (acknowledge Lakota humanity). d≈0.55, f(d)≈0.75, σ=0.8 → χ≈0.22. The coordination function is paradoxical: SuAnne is enforcing white solidarity by restoring their face.
constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ATHLETIC SYSTEM (ROPE) — School sports administration sees the constraint as coordination: the taunting crowd (reinforcing white dominance) enables the athletic hierarchy that reproduces institutional power. The system benefits from both the taunting and SuAnne's dignified response — it validates the system as 'working' (even hostile environments are managed through civility). d≈0.10, f(d)≈0.05, σ=0.8 → χ≈0.00. Net beneficiary; no extraction cost.
constraint_indexing:constraint_classification(suanne_face_restoration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: RACIAL CIVILITY RITUAL (PITON) — The entire dynamic — hostile taunting followed by dignified response — is a theatrical performance of 'good race relations.' The theater_ratio (0.55) reflects that the primary function (racial boundary maintenance via hierarchy) is hidden behind the performance of civility and dignity. The institutional framework perpetuates this ritual through inertia: story becomes legend becomes validation that 'civility works' rather than interrogation of why civility was required in the first place.
constraint_indexing:constraint_classification(suanne_face_restoration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, this constraint exhibits the paradox of positive politeness as a coordinating mechanism that perpetuates asymmetry. SuAnne's face-restoration strategy (adopting the cheerleader role, performing competence as entertainment) coordinates the hostile environment into civility while extracting from her own dignity and from the Lakota community's structural position. The observer sees both functions: genuine coordination (hostile situation becomes manageable) and genuine extraction (Lakota agency is subordinated to white comfort). d≈0.60, f(d)≈0.80, σ=1.1 (continental scope) → χ≈0.33.
constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suanne_face_restoration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suanne_face_restoration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_face_restoration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(suanne_face_restoration, TR),
    TR >= 0.70.

:- end_tests(suanne_face_restoration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts dignity and agency from SuAnne and the Lakota community, but this extraction is partially coordinated with the immediate problem (managing hostility). The extraction is not maximal (as in pure Snare) because SuAnne achieves genuine social coordination — the crowd becomes manageable, the game continues. However, the long-term extraction (structural subordination reinforced through the narrative of her dignity) is significant. The lower value reflects that the immediate coordination function is real, not purely theatrical. Suppression (0.68): High. The constraint operates by suppressing alternatives: direct confrontation is not available (escalates danger), silence is not available (accepts humiliation), alternative pathways for Lakota recognition are blocked. The suppression includes narrative suppression — the celebration of SuAnne's grace naturalizes the constraint rather than interrogating it. Theater ratio (0.55): Moderate-high. The primary function (maintaining racial hierarchy through white dominance) is performed and hidden behind the performance of civility, dignity, and grace. The narrative of SuAnne's triumph is substantially theatrical — it validates the system as functioning rather than revealing its structural inequality. The theater has increased over time as the event becomes legend, displaced from its actual structural context.
 *
 * PERSPECTIVAL GAP:
 *   SuAnne (trapped powerless) sees a Snare: she has no good options, and whatever she chooses extracts from her dignity. The hostile crowd (moderate constrained) sees a Tangled Rope: their hostility is coordinated into civility, but they are also constrained by SuAnne's refusal to accept humiliation. The school system (institutional arbitrage) sees a Rope: the constraint coordinates the hostile environment into civility without requiring institutional intervention. The racial civility ritual (institutional piton) sees its own degradation: the primary function (hierarchy maintenance) is being replaced by performance (civility), and the ritual persists through inertia. The analytical observer sees a Tangled Rope: genuine coordination (hostility becomes manageable) mixed with genuine extraction (Lakota subordination is perpetuated). The crucial gap: SuAnne's perspective (Snare) is suppressed, replaced by the triumphalist narrative of her grace, which validates the institutional perspective (Rope) that the system is working.
 *
 * DIRECTIONALITY LOGIC:
 *   SuAnne: Victim + trapped → d≈0.98, f(d)≈1.40. Maximum extraction. She has no alternatives; whatever she chooses extracts from her. Lakota community: Victim + trapped → d≈0.92, f(d)≈1.35. High extraction. Structural positioning makes exit impossible. Hostile crowd: Victim + constrained (of rigid racial scripts) + beneficiary (of white dominance) → d≈0.55, f(d)≈0.75. Mixed. They enforce hierarchy but are also trapped in it. School system: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary; system reproduces itself without cost. Racial civility ritual: Institutional + arbitrage → d≈0.10, f(d)≈0.05. Piton classification from theater gate, not from high d. Analytical observer: Analytical → d≈0.60, f(d)≈0.80. Middle position; can see both coordination and extraction as simultaneously true.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification from the analytical position, but this classification simultaneously validates and masks the Snare classification from SuAnne's position. The constraint exhibits genuine coordination (hostile environment becomes manageable) and genuine extraction (SuAnne's dignity is subordinated to white comfort). The mandatrophy is not 'is it coordination or extraction?' but 'which position gets to define the constraint's meaning?' The institutional perspective (school system, racial civility ritual) defines it as coordination (Rope) and celebrates it as such. The victim's perspective (SuAnne, Lakota community) experiences it as extraction (Snare) but is suppressed by the triumphalist narrative. The tangled_rope classification at the analytical level reveals this as the constraint's true structure: both functions are real, but they are asymmetrically distributed. SuAnne bears the extraction cost to coordinate the crowd's comfort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suanne_agency_vs_coercion,
    'Was SuAnne''s face-restoration move a genuine strategic choice or a coerced performance of compliance?',
    'First-person account from SuAnne; analysis of alternatives available to her at the moment; examination of whether the move was consistent with her stated values or responsive to immediate pressure',
    'If agency: the constraint is Tangled Rope (coordination with asymmetric structure). If coercion: the constraint is pure Snare (extraction with no genuine coordination function). The classification hinges on whether SuAnne had meaningful alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suanne_agency_vs_coercion, conceptual, 'Whether SuAnne''s response was authentic agency or coerced performance').

omega_variable(
    positive_politeness_function,
    'Does positive politeness (face-restoration through affiliation) actually reduce racial hostility or merely suppress its expression while deepening structural subordination?',
    'Longitudinal follow-up: Did the crowd''s attitudes change? Did subsequent interactions show reduced hostility or merely better behavioral management? Comparison with communities where direct confrontation (rather than appeasement) occurred.',
    'If reduces hostility: the constraint has genuine coordination function, classification remains Tangled Rope. If suppresses expression only: the constraint is closer to pure Snare with performative management, classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_politeness_function, empirical, 'Whether positive politeness changes attitudes or merely manages behavior').

omega_variable(
    lakota_collective_cost,
    'What is the community-level cost of individual face-restoration strategies? Does SuAnne''s dignified response enable the system to persist without change?',
    'Historical analysis: Did the Lakota community gain structural power/representation in subsequent years? Or did the narrative of SuAnne''s triumph substitute for material change? Examination of whether individual dignity performances delay collective structural transformation.',
    'If dignified response accelerated change: constraint extraction is genuine (mixed with coordination). If it enabled system persistence: constraint is more extractive than the tangled_rope classification suggests — the extraction is the long-term community cost masked by short-term interpersonal success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lakota_collective_cost, preference, 'Community-level cost of individual face-restoration strategies').

omega_variable(
    white_crowd_capacity_for_transformation,
    'Can direct acknowledgment of racial hierarchy and Lakota dignity (without face-restoration) move a hostile crowd, or is appeasement the only viable mechanism in that moment?',
    'Thought experiments and comparative historical analysis: What happens when marginalized groups refuse face-restoration and instead name the constraint directly? Does hostility increase, decrease, or remain unchanged? Examination of alternative scenarios.',
    'If direct confrontation is viable: SuAnne''s choice becomes strategic rather than forced, and the constraint shifts toward Tangled Rope or Rope (her choice, not coercion). If direct confrontation is impossible: the constraint is structural Snare (no alternatives exist), and SuAnne''s agency is retrospectively mythologized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(white_crowd_capacity_for_transformation, conceptual, 'Whether direct confrontation is structurally viable or constrained away').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suanne_face_restoration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(suanne_tr_t0, suanne_face_restoration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(suanne_tr_t5, suanne_face_restoration, theater_ratio, 5, 0.4).
narrative_ontology:measurement(suanne_tr_t10, suanne_face_restoration, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(suanne_be_t0, suanne_face_restoration, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(suanne_be_t5, suanne_face_restoration, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(suanne_be_t10, suanne_face_restoration, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suanne_face_restoration, enforcement_mechanism).
narrative_ontology:affects_constraint(suanne_face_restoration, racial_hierarchy_school_systems).
narrative_ontology:affects_constraint(suanne_face_restoration, positive_politeness_suppression).

% DUAL FORMULATION NOTE:
% SuAnne's face-restoration strategy is downstream of the racial hierarchy constraint built into school athletic systems but represents a distinct constraint on how marginalized agents can respond to hostility. The upstream constraint (racial hierarchy) has its own ε; the face-restoration constraint has ε=0.38 reflecting the mixed coordination-extraction structure of positive politeness mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(suanne_face_restoration, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
