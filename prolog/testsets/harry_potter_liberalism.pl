% ============================================================================
% CONSTRAINT STORY: harry_potter_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hp_liberalism, []).

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
 *   constraint_id: harry_potter_liberalism
 *   human_readable: The Potterverse Liberalism Constraint
 *   domain: socio_political/cultural_generation
 *
 * SUMMARY:
 *   The Potterverse Liberalism Constraint captures a structural tension
 *   between the cultural narrative that shaped a generation (1990s-2010s
 *   liberal values: tolerance, institutional trust, non-violence, diversity
 *   as solution) and the material conditions facing the subsequent generation
 *   (climate crisis, economic precarity, institutional capture, concentration
 *   of wealth). The constraint is neither a natural law nor a pure
 *   coordination mechanism, but a hybrid: it genuinely solved real problems
 *   for its beneficiaries (multicultural coexistence, ending explicit
 *   discrimination, enabling institutional diversity) while systematically
 *   extracting future resources and closing policy options for those facing
 *   different, more severe material constraints. The 'Mirror of Erised'
 *   metaphor captures that the constraint shows each observer what they most
 *   desire: the liberal sees their own virtue reflected back; the precariat
 *   sees the closing door; the institutional gatekeeper sees their moral
 *   authority; the change advocate sees the cage. The increasing theater
 *   ratio (0.42 → 0.65) reflects growing performativity: by 2020, liberal
 *   institutional responses to structural problems (diversity initiatives
 *   without redistribution, inclusion statements without power-sharing,
 *   climate pledges without consumption reduction) became increasingly
 *   ritualistic. The extractiveness increase (0.28 → 0.52) reflects that the
 *   framework's suppression of radical alternatives has intensified as the
 *   gap between liberal promises and material outcomes has widened.
 *
 * KEY AGENTS:
 *   - Millennial Liberal Coalition: Primary beneficiary (institutional/arbitrage) — captured institutional power via liberal framing; benefits from constraint's validation of their virtue and career path
 *   - Gen Z Precariat: Primary victim (powerless/trapped) — faces material constraints (climate, inequality, debt) but is told patience and institutional faith are virtues; cannot exit or voice alternatives
 *   - Systemic Change Advocates: Secondary victim (moderate/constrained) — recognize institutional inadequacy but face professional and social suppression for naming it; moderate agency but high career risk
 *   - Progressive Institutions: Intermediate actor (organized/constrained) — universities, nonprofits, media led by millennials; provide coordination function but enforce conformity to liberal values
 *   - Liberal International Order: Institutional layer (institutional/arbitrage) — UN, human rights apparatus, NGO networks; maintenance is performative, function is atrophied
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as timeless truth about coexistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harry_potter_liberalism, 0.52).
domain_priors:suppression_score(harry_potter_liberalism, 0.48).
domain_priors:theater_ratio(harry_potter_liberalism, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harry_potter_liberalism, extractiveness, 0.52).
narrative_ontology:constraint_metric(harry_potter_liberalism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(harry_potter_liberalism, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harry_potter_liberalism, tangled_rope).
narrative_ontology:human_readable(harry_potter_liberalism, "The Potterverse Liberalism Constraint").
narrative_ontology:topic_domain(harry_potter_liberalism, "socio_political/cultural_generation").

domain_priors:requires_active_enforcement(harry_potter_liberalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harry_potter_liberalism, millennial_liberal_coalition).
narrative_ontology:constraint_beneficiary(harry_potter_liberalism, institutional_gatekeepers).
narrative_ontology:constraint_victim(harry_potter_liberalism, gen_z_precariat).
narrative_ontology:constraint_victim(harry_potter_liberalism, systemic_change_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEN Z PRECARIAT (SNARE) — Faces climate crisis, economic precarity, and institutional capture but is told institutional patience and tolerance are virtues. Cannot exit the constraint (material conditions trap them); must accept the extraction of their future and resources while their parents' generation preaches non-violence and faith in institutions. Maximum experienced extraction without exit option. The liberal framework forbids both exit and voice.
constraint_indexing:constraint_classification(harry_potter_liberalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYSTEMIC CHANGE ADVOCATES (SNARE) — Recognize structural impossibility of solving climate/inequality through institutional compromise but face coordinated suppression (professional consequences, social sanction, algorithmic deamplification) for naming this reality. Exit from the constraint is costly — career risk, social ostracism, reputational damage. High extraction via suppression of alternatives.
constraint_indexing:constraint_classification(harry_potter_liberalism, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILLENNIAL LIBERAL COALITION (ROPE) — Primary beneficiary. The constraint provides coordination function: shared values (tolerance, institutional trust, non-violence) enable coalition cohesion and access to institutional power. Experiences the constraint as genuine coordination — enables their ascent into media, academia, nonprofits. Arbitrage exit: can move freely between institutions, platforms, career paths. Net beneficiary. Low experienced extraction because exit options are available.
constraint_indexing:constraint_classification(harry_potter_liberalism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE INSTITUTIONS (TANGLED ROPE) — Universities, nonprofits, media organizations led by millennial liberals have both coordination and extraction functions. The constraint enables their self-conception as moral actors (the heroes fighting injustice) while it extracts conformity from staff and protégés who must perform liberal values or face professional consequences. Mixed: genuine coordination for some, asymmetric extraction for others within the same institutional boundary.
constraint_indexing:constraint_classification(harry_potter_liberalism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LIBERAL INTERNATIONAL ORDER (PITON) — Global institutional substrate (UN, human rights law, NGO networks) derived its legitimacy from post-Cold War liberalism. The constraint persists through institutional inertia: liberal human-rights framing is the official moral vocabulary of international institutions, but the institutions' actual capacity to solve problems (climate, inequality, war) has atrophied. Theater ratio high: much human-rights rhetoric, little structural change. Maintenance is performative. Beneficiaries of the international order (wealthy nations, financial institutions) have arbitrage options and keep the ritual alive; victims have no voice in the maintenance.
constraint_indexing:constraint_classification(harry_potter_liberalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN AT RISK) — The constraint could be read as a natural law of large-scale social coordination: diversity and tolerance are structurally required for multiethnic/multivalue societies to cohere without violence. This is the naturalization risk. However, the base properties (extractiveness 0.52, suppression 0.48, active enforcement required) indicate this is contingent institutional arrangement, not natural law. The false summit detector will flag this perspective as naturalized liberalism.
constraint_indexing:constraint_classification(harry_potter_liberalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harry_potter_liberalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harry_potter_liberalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harry_potter_liberalism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harry_potter_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harry_potter_liberalism, TR),
    TR >= 0.70.

:- end_tests(harry_potter_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts future options and resources from those it governs without consent. The extraction is not maximal (0.7+) because some beneficiaries (progressive institutions, some millennials) genuinely believe in the framework's coordination function and are not purely extractive. The extraction increases over time as the gap between liberal promises and material outcomes widens — institutions must increasingly suppress alternative frameworks to maintain coherence, raising the suppression cost. Suppression (0.48): Moderate. The constraint suppresses alternatives through professional gatekeeping, social sanction, algorithmic deamplification, and rhetorical dismissal (dismissing systemic critique as 'cynical' or 'divisive'). But suppression is not total — systemic critique exists and finds audiences, just at reduced scale. Theater ratio (0.65): High and rising. By 2020, much liberal institutional response to structural problems is performative: diversity initiatives that don't redistribute power, inclusion statements from homogeneous leadership, carbon-neutral pledges that outsource emissions. The theater has grown because the framework must increasingly maintain appearances as actual outcomes diverge from promises.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival gaps across the observation site. For millennials with institutional access (institutional/arbitrage), it classifies as Rope — genuine coordination that enabled their career and self-conception. For Gen Z without institutional access (powerless/trapped), it classifies as Snare — suppression of alternatives with no exit. For organized progressive institutions (organized/constrained), it is Tangled Rope — both coordination function (coalition identity) and extraction function (conformity enforcement). For the global liberal order (institutional/arbitrage), it is Piton — the rituals persist but the function has atrophied, maintained by inertia. The analytical observer risks Mountain classification — reading the constraint as a natural law of multicultural coexistence — but the base properties reveal this as naturalization of a contingent institutional choice. The gap is not observational; it is structural. Beneficiaries and victims genuinely experience different constraints because the extraction flows asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural relationship to the extraction flow. Millennial liberals are low-d beneficiaries (arbitrage exit, benefit asymmetrically) — the pipeline derives d ≈ 0.15, producing low/negative χ from their perspective. Gen Z precariat are high-d victims (trapped, no exit) — d ≈ 0.95, producing maximum f(d) ≈ 1.42. Systemic change advocates are moderate-to-high-d victims (constrained exit due to career risk) — d ≈ 0.65-0.75, producing moderate-high χ. Progressive institutions are intermediate — beneficiaries (low d) internally but enforcers of suppression (pushes d upward for staff), creating internal perspectival gap. The institutional liberal order is low-d beneficiary (arbitrage) but increasingly dependent on suppression (rising d), creating temporal gap. No directionality overrides are needed; the structural derivation captures the gaps accurately. The constraint is not symmetric — extraction flows primarily from powerless to institutional, mediated by organized gatekeepers.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY VECTOR: The constraint's classification depends on whether one reads 'liberalism' as a coordination mechanism (solving the problem of diverse coexistence) or as extraction (closing policy options for institutional reform). The 1990s reading was genuine coordination — the liberal framework solved real problems after the Cold War and apartheid era. The 2020s reading reveals extraction — the same framework now suppresses the systemic changes required to address climate and inequality. This is not a contradiction but a temporal shift: the constraint solved its coordination problem so effectively that institutional actors (beneficiaries) now use it to suppress the next phase of adaptation. The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification: the constraint is both genuinely coordinative (it still enables diverse institutional coexistence) and extractive (it systematically prevents the systemic changes younger generations require). The beneficiary/victim asymmetry is structural, not perspectival. Millennials benefit; Gen Z pays. The framework is not false; it is insufficient for new material conditions and is being weaponized to prevent adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_material_condition_shift,
    'Do subsequent generations'' material conditions (climate, inequality, institutional capture) represent genuinely different constraints or are they testing the same liberal framework under stress?',
    'Comparative analysis of institutional response to Gen X challenges (Cold War end, early neoliberalism) vs Gen Z challenges (climate, precarity). If institutions adopt same tolerant/incremental approach, confirms framework stress. If they adapt mechanisms, suggests different constraint.',
    'If different: the Potterverse liberalism constraint is specifically a 1990s-2000s phenomenon, not a transhistorical liberalism constraint. If same: liberalism itself is the constraint, blocking adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_material_condition_shift, empirical, 'Whether material shifts represent new constraints or stress on the same one').

omega_variable(
    institutional_gatekeeping_intentionality,
    'Is the suppression of systemic change advocacy (professional sanctions, deamplification, social shaming) an intentional enforcement mechanism or emergent coordination artifact?',
    'Analysis of institutional policy documentation, communications, and gatekeeping patterns. Explicit statements of values-based filtering vs algorithmic/market-driven deamplification.',
    'If intentional enforcement: clearly Tangled Rope (active enforcement, explicit extraction). If emergent: more ambiguous — could be Scaffold (coordination norm that will self-correct) or Snare (suppression without conscious strategy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_intentionality, empirical, 'Whether suppression is intentional institutional policy or emergent coordination').

omega_variable(
    liberal_framework_adaptability,
    'Can the liberal framework itself incorporate systemic critique and institutional reform without collapsing its core coordination function?',
    'Historical case studies of liberal frameworks adapting to critique (civil rights, women''s suffrage, environmental regulation). Pattern analysis: do frameworks expand or splinter when challenged?',
    'If adaptable: the constraint is Scaffold with sunset — liberal institutions can incorporate Gen Z demands without wholesale replacement. If not: the constraint is Snare — the framework structurally forbids the changes it must undergo, causing the trap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liberal_framework_adaptability, conceptual, 'Whether liberalism can adapt to systemic critique without losing coherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harry_potter_liberalism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harr_tr_t0, harry_potter_liberalism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(harr_tr_t15, harry_potter_liberalism, theater_ratio, 15, 0.55).
narrative_ontology:measurement(harr_tr_t30, harry_potter_liberalism, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(harr_be_t0, harry_potter_liberalism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(harr_be_t15, harry_potter_liberalism, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(harr_be_t30, harry_potter_liberalism, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harry_potter_liberalism, information_standard).
narrative_ontology:affects_constraint(harry_potter_liberalism, climate_policy_incrementalism).
narrative_ontology:affects_constraint(harry_potter_liberalism, institutional_gatekeeping_professional_norms).
narrative_ontology:affects_constraint(harry_potter_liberalism, generational_wealth_accumulation_asymmetry).

% DUAL FORMULATION NOTE:
% The Potterverse Liberalism Constraint decomposes into two related but distinct constraints: (1) 1990s-2010s Liberal Coordination (ε ≈ 0.15, Mountain-to-Rope, solved genuine diversity problem), and (2) 2010s-2025 Liberal Extraction (ε ≈ 0.52, Tangled Rope, suppresses systemic change). These are not the same constraint viewed from different angles — they have different ε values because the material conditions changed and the framework's function shifted from solving coordination to enforcing stasis. The upstream constraint was Rope; the downstream is Tangled Rope. The network link documents this evolution: the successful coordination constraint created the institutional gatekeeping layer that now enables extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harry_potter_liberalism, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
