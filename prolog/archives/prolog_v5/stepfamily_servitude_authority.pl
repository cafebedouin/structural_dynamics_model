% ============================================================================
% CONSTRAINT STORY: stepfamily_servitude_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stepfamily_servitude_authority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: stepfamily_servitude_authority
 *   human_readable: Stepfamily Servitude Authority and Blended Household Labor Asymmetry
 *   domain: interpersonal/family_dynamics
 *
 * SUMMARY:
 *   Stepfamily servitude authority represents a structural constraint
 *   operating at the intersection of kinship, household organization, and
 *   power asymmetries inherent to blended families. The constraint exhibits a
 *   complex perspectival landscape where the same structural phenomenon — the
 *   stepchild's position in a household with limited exit options and
 *   asymmetric authority — appears as pure extraction (snare) from the
 *   stepchild's perspective, as mixed coordination and extraction (tangled
 *   rope) from the biological parent's perspective, as straightforward
 *   coordination (rope) from the stepparent's perspective, and risks being
 *   naturalized as an immutable feature of family structure (mountain) from
 *   the analytical observer's perspective. The constraint is distinct from
 *   biological parent-child authority because the legitimacy of stepparent
 *   authority is structurally fragile: it depends entirely on the biological
 *   parent's active delegation and could be revoked through that parent's
 *   exit or redirection. This conditional legitimacy differentiates it from
 *   inherited parental authority but also creates the primary extraction
 *   mechanism — enforcement of authority without corresponding duty of care
 *   or institutional backing. The measurements show increasing extractiveness
 *   and theater ratio over the interval (0-9 years, roughly the early-to-mid
 *   adolescence period of a stepchild), reflecting accumulation of labor
 *   demands and increasing divergence between performed parental care and
 *   actual authority enforcement as the stepchild develops cognitive capacity
 *   to recognize the asymmetry.
 *
 * KEY AGENTS:
 *   - Stepchild (early-to-mid adolescence): Primary victim (powerless/trapped) — bears extraction without reciprocal care; lacks exit options; subject to stepparent authority that lacks independent institutional legitimacy
 *   - Biological Parent (adult): Secondary victim and coordinator (moderate/constrained) — genuinely coordinates household while enforcing stepparent authority on stepchild; trapped by marriage commitment, custody concerns, and guilt dynamics
 *   - Stepparent (adult, authority position): Primary beneficiary (institutional/arbitrage) — derives authority from marriage to biological parent; benefits from household labor and compliance; can exit at moderate cost relative to stepchild
 *   - Older Stepchild/Adolescent (age 14+): Emerging organized agent (organized/constrained) — develops capacity to negotiate, threaten exit, seek external support; constraint morphs from snare toward tangled rope as agency emerges
 *   - Cultural Authority Narrative (civilizational): Piton perspective (institutional/arbitrage) — inherited parental authority frameworks applied to stepfamily context; maintains legitimacy through theater and inertia despite institutional degradation of actual authority support
 *   - Analytical Observer (universal scope): Risks false summit (analytical/analytical) — temptation to naturalize contingent institutional arrangements as immutable kinship logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stepfamily_servitude_authority, 0.58).
domain_priors:suppression_score(stepfamily_servitude_authority, 0.68).
domain_priors:theater_ratio(stepfamily_servitude_authority, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stepfamily_servitude_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(stepfamily_servitude_authority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(stepfamily_servitude_authority, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stepfamily_servitude_authority, tangled_rope).
narrative_ontology:human_readable(stepfamily_servitude_authority, "Stepfamily Servitude Authority and Blended Household Labor Asymmetry").
narrative_ontology:topic_domain(stepfamily_servitude_authority, "interpersonal/family_dynamics").

domain_priors:requires_active_enforcement(stepfamily_servitude_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stepfamily_servitude_authority, biological_parent_in_blended_unit).
narrative_ontology:constraint_beneficiary(stepfamily_servitude_authority, stepparent_authority_position).
narrative_ontology:constraint_victim(stepfamily_servitude_authority, stepchild_labor_extraction).
narrative_ontology:constraint_victim(stepfamily_servitude_authority, household_emotional_labor_asymmetry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEPCHILD IN EARLY ADOLESCENCE (SNARE) — Faces full extraction with minimal coordination benefit. Trapped by dependency on household, legal guardianship, and emotional bonds to biological parent. Suppression is severe: exit means abandoning primary attachment figure, loss of housing, school disruption, social isolation. Experiences systematic labor extraction (chores, childcare, emotional regulation) with no reciprocal care or voice in household decisions. The stepparent authority is legitimized through marriage to biological parent, creating structural coercion. Maximum experienced extraction.
constraint_indexing:constraint_classification(stepfamily_servitude_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BIOLOGICAL PARENT MEDIATING (TANGLED ROPE) — Genuinely coordinates household function (finances, caregiving, conflict management between stepparent and stepchild) while also extracting from stepchild through enforcing stepparent authority. High constraint costs (relationship tension, loyalty conflicts, guilt) alongside genuine benefit from household stability and partner support. Exit is constrained by marriage commitment, fear of losing custody, financial interdependence, and desire to create functional family unit. Experiences mixed coordination and extraction.
constraint_indexing:constraint_classification(stepfamily_servitude_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: STEPPARENT IN AUTHORITY POSITION (ROPE) — Experiences the constraint as coordination: organizing household labor, establishing boundaries, creating family cohesion. Benefits from legitimacy through marriage to biological parent and cultural norms treating stepparent authority as natural. Can exit (remarriage dissolution, relocation) at moderate cost compared to biological child. Extracted value (labor from stepchild, authority recognition) appears to them as coordinated household function, not extraction. Net beneficiary.
constraint_indexing:constraint_classification(stepfamily_servitude_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: OLDER STEPCHILD/ADOLESCENT WITH AGENCY (TANGLED ROPE) — As stepchild approaches adulthood, develops capacity to organize (negotiate boundaries, threaten exit, seek external support). Constraint becomes mixed: still experiences extraction but develops partial escape pathways (after-school employment, selective compliance, peer refuge, institutional mediation through schools). Constrained exit (leaving creates relational rupture, college funding loss, loss of identity investment in family), but exit is possible at higher cost. Organized agency enters the picture, lowering effective extraction from snare toward tangled rope range.
constraint_indexing:constraint_classification(stepfamily_servitude_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CULTURAL AUTHORITY NARRATIVE (PITON) — The stepfamily structure inherits Puritan/Victorian parental authority (stepparent as full authority figure, obedience obligation, labor contribution as moral duty) while actual institutional support for that authority has degraded. Family courts now recognize stepchild autonomy, schools track welfare, child abuse norms have shifted. The stepparent authority is maintained through institutional inertia and theater (calling labor assignments 'chores' and 'responsibilities' rather than 'unpaid work'; framing authority as 'love' and 'structure'). The theater ratio (0.65) reflects the gap between the performed role (concerned stepparent maintaining household) and actual mechanism (labor extraction justified by inherited authority narrative). Functional verification has declined — the parental authority once enforced through total institutional support (church, law, social isolation) now relies on family-internal enforcement.
constraint_indexing:constraint_classification(stepfamily_servitude_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW RISK (MOUNTAIN) — From a civilizational/universal perspective, stepfamily authority appears to be an immutable feature of human kinship and household organization: blended families always require some hierarchy, authority differentials are inherent to adult-child relationships, labor distribution is a natural problem of multi-generational cohabitation. This perspective risks naturalizing what is actually a contingent institutional arrangement inherited from parental authority systems designed for biological families. However, structural data reveals this as a false summit: the extraction flows from power asymmetries and legitimacy deficits specific to stepfamily structures, not from immutable kinship logic. The analytical observer must recognize its own risk of naturalizing institutional arrangements.
constraint_indexing:constraint_classification(stepfamily_servitude_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stepfamily_servitude_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stepfamily_servitude_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stepfamily_servitude_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stepfamily_servitude_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stepfamily_servitude_authority, TR),
    TR >= 0.70.

:- end_tests(stepfamily_servitude_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The stepchild faces systematic labor extraction (household chores, secondary childcare, emotional regulation) alongside suppression of voice in household decisions. However, extractiveness is not maximal (0.70+) because some genuine coordination occurs: household stability does require labor distribution, some stepparent-stepchild relationships develop affection alongside authority, and the stepchild's labor does contribute to household functioning (not pure parasitic extraction). The measurement trajectory shows increasing extractiveness from 0.35 to 0.60 as the stepchild enters adolescence and becomes capable of more complex labor (emotional regulation, adolescent management, household planning) and more clearly perceives the asymmetry. Suppression (0.68): Moderate-high. Multiple barriers to exit exist: housing insecurity (stepchild cannot independently afford housing), school disruption (exit means changing schools, social networks), emotional bonds to biological parent and possibly stepsiblings, legal guardianship, and cultural/family shame around rejecting family unit. However, suppression is not total (0.90+) — stepchildren do have partial exit pathways: negotiation with biological parent, school counselor intervention, extended family support, employment (older adolescents), and eventual aging-out of household. Theater ratio (0.65): Moderate-high. The stepparent performs the role of concerned authority figure organizing household, setting rules, maintaining structure — framing labor demands as 'chores' and 'responsibilities' (child development language) and authority as 'love' and 'boundaries' (parental care language). The performed narrative is genuine parenting, but actual mechanism is authority enforcement without corresponding institutional duty of care (stepparent has no legal obligation of care, inheritance rights, or decision-making authority if biological parent exits the relationship). The gap between performed role and actual institutional position creates theater. The theater increases over time (0.55 to 0.65) as the stepchild's cognitive capacity increases and the gap between performance and reality becomes more apparent.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the stepchild's snare classification and the stepparent's rope classification is the diagnostic signal. Both agents interact with the same constraint, but their structural positions (beneficiary vs victim, arbitrage exit vs trapped exit) produce incompatible classifications. The gap reveals that the constraint's classification is observer-dependent — the same authority system that solves a genuine coordination problem (household organization) for the beneficiary functions as pure extraction for the victim. This is the mandatrophy mechanism: the beneficiary genuinely solves coordination, so rope classification is structurally honest; the victim genuinely experiences extraction, so snare classification is structurally honest; but both cannot be 'the' classification of the constraint. The resolution is that the constraint IS tangled rope at the base level (has both coordination and asymmetric extraction) — the snare and rope perspectives are partial views of a hybrid system. The biological parent, positioned between beneficiary and victim, perceives the tangled rope directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is computed from their beneficiary/victim status and exit options. The stepchild-as-victim-with-trapped-exit derives d ≈ 0.95 (maximum target), producing f(d) ≈ 1.42 (maximum experienced extraction multiplier). The stepparent-as-beneficiary-with-arbitrage-exit derives d ≈ 0.05 (near-full beneficiary), producing f(d) ≈ -0.12 (extraction runs toward them). The biological parent-as-mixed-coordinator-and-enforcer-with-constrained-exit derives d ≈ 0.55 (symmetric), producing f(d) ≈ 0.75 (moderate experienced extraction). The older adolescent-as-victim-with-increasing-exit-capacity derives d ≈ 0.70 (moderating from the stepchild's 0.95 as agency develops), producing f(d) ≈ 1.00 (moderate extraction). These flows explain the perspectival classifications: high d agents (victims) perceive snare; low d agents (beneficiaries) perceive rope; medium d agents (mixed roles) perceive tangled rope. The biological parent's constraint derives from enforcer role (benefits from stepparent partnership) and enforced role (bears cost of stepchild resentment, relationship tension); this dual position produces mixed classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy resolution mechanism through the biological parent's perspective. The biological parent genuinely experiences coordination (household stability, financial interdependence, partnership support) alongside genuine extraction (enforcing asymmetric authority on stepchild, bearing conflict costs, managing loyalty triangulation). Neither rope nor snare alone captures this experience — both are present. The classification cannot collapse to snare (which would deny the real coordination benefit) or rope (which would deny the real extraction cost on stepchild). The base classification is tangled_rope: it has both a genuine coordination function (household organization, resource allocation, collective childcare) and asymmetric extraction (stepchild bears labor and authority without reciprocal voice or care obligation). The stepchild perceives snare because they bear extraction without perceiving coordination benefit (household stability feels imposed, not jointly created). The stepparent perceives rope because they perceive coordination benefit without bearing the extraction cost (authority feels like functional role, not enforced asymmetry). The biological parent perceives the actual structure: tangled rope. The mandatrophy is resolved by recognizing that indexical classification is structurally honest — different observers genuinely experience different classification types based on their structural position, and the base constraint is the hybrid that all their positions depend on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (dependency on household, housing insecurity, school disruption from exit) or primarily internalized (stepchild''s identity fusion with family unit, internalized obligation narratives, identity lock preventing perception of exit as legitimate)?',
    'Post-exit suppression trajectory: interview stepchildren 2-5 years after leaving household regarding ease of boundary-setting, shame/guilt persistence, autonomy development. If suppression persists after structural barriers are removed, classification as partially internalized. If suppression drops immediately upon exit, classification as primarily structural.',
    'If primarily structural: suppression (0.68) is accurate to actual barrier height; snare classification stands. If primarily internalized: effective suppression is higher than measured (target carries internalization with them); constraint boundary extends beyond household (identity_locked exit captures the mechanism better than trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in stepfamily authority').

omega_variable(
    stepparent_authority_legitimacy_source,
    'Does the stepparent''s authority derive primarily from the biological parent''s delegation (conditional legitimacy, revocable, relationally dependent) or from culturally inherited parental authority narratives (treated as intrinsic to the stepparent role, independent of biological parent''s active delegation)?',
    'Ethnographic interviews with stepparent-stepchild dyads regarding source of obedience (fear of consequences vs respect/love vs internalized duty vs deference to biological parent''s wishes). Analysis of household negotiation patterns when biological parent is absent vs present. Cultural discourse analysis on stepparent authority narratives in parenting media.',
    'If delegated (conditional): authority is contingent on biological parent''s active endorsement; removing that endorsement rapidly degrades stepparent power. If inherited (intrinsic): authority feels naturalized and self-justifying; stepchild obedience persists even without biological parent enforcement. Classification shifts from snare (extractive authority) toward tangled_rope if legitimacy is conditional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stepparent_authority_legitimacy_source, conceptual, 'Source of stepparent authority legitimacy (delegated vs inherited)').

omega_variable(
    coordination_function_household_stability,
    'How much of the extracted labor (chores, emotional regulation, secondary childcare) actually contributes to genuine household coordination vs serving as proxy control mechanism (obedience extraction disconnected from functional need)?',
    'Comparative household function analysis: stepfamilies with high labor extraction vs stepfamilies with low extraction on dimensions of financial stability, meal preparation, child supervision, household maintenance quality, emotional atmosphere. Identify whether extraction level correlates with actual need or with stepparent authority enforcement.',
    'If extraction matches functional need (high-extraction households are more complex, require more labor): constraint is tangled_rope at base level. If extraction is decoupled from need (low-complexity households still demand high extraction): constraint is snare at base level; tangled_rope is false classification masking pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_household_stability, empirical, 'Whether extracted labor serves coordination or control').

omega_variable(
    biological_parent_exit_capacity_myth,
    'Is the biological parent''s perceived constraint (inability to leave stepparent relationship without losing stepchild cooperation/stability) real or performative? Can biological parents actually exit stepparent relationships without proportional loss of household function or stepchild wellbeing?',
    'Longitudinal outcome tracking: biological parents who leave stepparent relationships on dimensions of household function, stepchild grades/behavior, co-parenting stability, child reports of wellbeing. Separate genuine functional dependence on stepparent from fear-based or identity-based perceived dependence.',
    'If exit truly degrades outcomes: biological parent''s constraint is real; tangled_rope classification for biological parent stands. If outcomes remain stable: biological parent''s constraint is identity-locked or performative; classification shifts to constrained or mobile (revealing that they enforced extraction partly to maintain perceived household stability that didn''t actually require that enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_parent_exit_capacity_myth, empirical, 'Whether biological parent actually needs stepparent extraction for household function').

omega_variable(
    stepchild_identity_fusion_threshold,
    'At what age/stage does stepchild identity fusion with blended family unit peak, and does it prevent perception of exit as legitimate beyond structural barriers?',
    'Longitudinal identity interviews with stepchildren age 8-25 on self-concept development, family identity centrality, perception of exit legitimacy. Identify age at which identity_locked exit becomes observable (identity fusion makes exit psychologically unavailable even when structurally mobile).',
    'If identity fusion is strong (early adolescence through late teens): stepchild experiences identity_locked exit in addition to trapped/constrained structural barriers; suppression is compounded. If identity fusion is weak (childhood stages, or post-18 stages): exit is primarily constrained/trapped by material barriers, not psychological; classification adjusts downward on internalization dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stepchild_identity_fusion_threshold, empirical, 'Stepchild identity fusion with blended family and exit perception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stepfamily_servitude_authority, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stepfam_tr_t0, stepfamily_servitude_authority, theater_ratio, 0, 0.55).
narrative_ontology:measurement(stepfam_tr_t3, stepfamily_servitude_authority, theater_ratio, 3, 0.62).
narrative_ontology:measurement(stepfam_tr_t6, stepfamily_servitude_authority, theater_ratio, 6, 0.67).
narrative_ontology:measurement(stepfam_tr_t9, stepfamily_servitude_authority, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(stepfam_be_t0, stepfamily_servitude_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stepfam_be_t3, stepfamily_servitude_authority, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(stepfam_be_t6, stepfamily_servitude_authority, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(stepfam_be_t9, stepfamily_servitude_authority, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stepfamily_servitude_authority, attachment_coordination).
narrative_ontology:boltzmann_floor_override(stepfamily_servitude_authority, 0.12).
narrative_ontology:affects_constraint(stepfamily_servitude_authority, blended_family_loyalty_triangulation).
narrative_ontology:affects_constraint(stepfamily_servitude_authority, stepchild_identity_development_institutional_capture).

% DUAL FORMULATION NOTE:
% Stepfamily servitude authority decomposes into two structurally distinct constraints: (1) household labor extraction (this story, ε≈0.58), focusing on material labor asymmetry and authority mechanisms; (2) emotional loyalty triangulation (linked story, ε≈0.42), focusing on identity formation and affective entanglement. Each has distinct measurement trajectories and resolution pathways. This story addresses the extraction mechanism; the linked story addresses the psychological lock mechanism. Both operate simultaneously but have different dominant mechanisms (power asymmetry vs identity fusion) and different exit pathways (renegotiating authority delegation vs identity frame disruption).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stepfamily_servitude_authority, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
