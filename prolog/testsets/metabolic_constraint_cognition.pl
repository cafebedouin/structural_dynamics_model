% ============================================================================
% CONSTRAINT STORY: metabolic_constraint_cognition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metabolic_constraint_cognition, []).

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
 *   constraint_id: metabolic_constraint_cognition
 *   human_readable: The ATP Ceiling as an Exploitable Limit
 *   domain: biological/technological/economic
 *
 * SUMMARY:
 *   The ATP ceiling represents the metabolic limit of human cognitive
 *   capacity: the brain consumes ~20W at baseline and ~40W during peak
 *   cognitive states, constrained by oxygen delivery, mitochondrial ATP
 *   production, and lactate buffering. This biological boundary has become
 *   exploitable through institutional arrangements in three ways: (1)
 *   pharmaceutical manufacturers capture the market for stimulants and
 *   cognitive enhancers that temporarily bypass the ceiling; (2) surveillance
 *   capitalism extracts value from the attention scarcity the ceiling
 *   creates; (3) labor markets demand sustained cognitive output that exceeds
 *   sustainable ATP availability, creating burnout, cognitive decline, and
 *   dependence on enhancement. The constraint exhibits all six DR types from
 *   different perspectives, demonstrating how a biological limit becomes an
 *   economic extraction mechanism when institutional actors organize demand
 *   around it rather than adjusting expectations to match metabolic reality.
 *   The theater_ratio (0.38) reflects that productivity ideology frames
 *   overwork as commitment or excellence rather than acknowledging metabolic
 *   exhaustion. Over the 10-year interval, extractiveness has risen from 0.28
 *   to 0.52 as cognitive demand in knowledge work has intensified and
 *   pharmaceutical interventions have proliferated, enabling institutions to
 *   extend extraction beyond what biology alone would enforce.
 *
 * KEY AGENTS:
 *   - Cognitive Workers: Primary victims (powerless/trapped) — face unsustainable demand; economic lock-in and debt trap them in overextension
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — profit from stimulant and nootropic demand created by the ceiling
 *   - Surveillance Capitalism: Secondary beneficiary (institutional/arbitrage) — extract attention value from the scarcity the ceiling creates
 *   - Labor Organizing & Regenerative Work Movements: Organized agents (organized/constrained) — building alternative norms (sabbaticals, 4-day weeks, right-to-disconnect) with generational sunset
 *   - Productivity Ideology Managers: Institutional actors (institutional/arbitrage) — maintain performative overwork culture despite declining returns
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing institutional arrangements as metabolic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metabolic_constraint_cognition, 0.52).
domain_priors:suppression_score(metabolic_constraint_cognition, 0.68).
domain_priors:theater_ratio(metabolic_constraint_cognition, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metabolic_constraint_cognition, extractiveness, 0.52).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metabolic_constraint_cognition, tangled_rope).
narrative_ontology:human_readable(metabolic_constraint_cognition, "The ATP Ceiling as an Exploitable Limit").
narrative_ontology:topic_domain(metabolic_constraint_cognition, "biological/technological/economic").

domain_priors:requires_active_enforcement(metabolic_constraint_cognition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, cognitive_enhancement_industry).
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, surveillance_capitalism).
narrative_ontology:constraint_victim(metabolic_constraint_cognition, cognitive_workers).
narrative_ontology:constraint_victim(metabolic_constraint_cognition, attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVEREXTENDED KNOWLEDGE WORKER (SNARE) — Faces demand for sustained cognitive output exceeding metabolic capacity. Unable to exit: economic pressure, debt, career lock-in, and social expectations trap them in a cycle of cognitive overextension. No alternative exists in high-skill labor markets. Bears full cost of ATP depletion: burnout, cognitive decline, attention fragmentation, health deterioration. Maximum experienced extraction.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADAPTIVE COGNITIVE PROFESSIONAL (TANGLED ROPE) — Moderately skilled workers develop some agency through specialization, remote work options, and tactical rest periods. Exit options are constrained but not eliminated. Experience both extraction (unsustainable demand, time pressure) and coordination benefits (specialized networks, knowledge-sharing ecosystems). The constraint functions as coordination for some tasks while extracting from sustainable attention.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURERS & COGNITIVE ENHANCEMENT INDUSTRY (ROPE) — Primary beneficiary. Experiences the ATP ceiling as a coordination mechanism: the metabolic constraint creates demand for stimulants, nootropics, caffeine, beta-blockers, and other pharmacological interventions. The constraint enables market creation. Extraction runs toward this actor; they benefit from the perpetual demand structure. Exit options abundant—they can pivot product lines, exit markets, arbitrage regulatory differences across jurisdictions.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SURVEILLANCE CAPITALISM & ATTENTION EXTRACTION (ROPE) — Secondary beneficiary. The ATP ceiling creates attention scarcity. Scarce attention commodifies: platform companies, advertisers, and content distributors benefit from the perpetual attention shortage. The constraint coordinates value extraction from cognitive scarcity—each person has only ~4 hours of high-quality attention daily. Institutional actors with arbitrage options (regulatory exceptions, data access, algorithmic priority) extract disproportionate share. Experience rope: genuine coordination benefit from the scarcity structure.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR ORGANIZING & REGENERATIVE WORK MOVEMENTS (SCAFFOLD) — Organized agents (unions, worker cooperatives, time-banking networks, sabbatical-advocacy groups) see the ATP ceiling as a temporary institutional dysfunction being addressed through: 4-day work weeks, sabbatical policies, cognitive load caps, right-to-disconnect laws, and metabolic-aware scheduling. These norms are maturing in some jurisdictions (e.g., EU right-to-disconnect, Iceland 4-day pilot). Exit path visible: structural norms can change within a generation. Suppression is declining as alternatives mature.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INDUSTRIAL PRODUCTIVITY IDEOLOGY (PITON) — The narrative that 'more cognitive output = more value' is a degraded institutional practice. The ideology persists through corporate performance metrics, academic prestige systems, and productivity theater (Slack status messages, calendar visibility, meeting attendance signals) despite evidence that it reduces actual output. The ritual continues not because it works but because it's institutionalized. Theater ratio high: performative overwork signals commitment rather than generating cognitive gains. Exits from this ideology are available in some subcommunities but remain costly.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / METABOLIC CONSTRAINT VIEW (MOUNTAIN) — From a civilizational/universal perspective, the ATP ceiling is a metabolic law: human brains consume ~20W continuously, peak performance demands ~40W during high-cognition states. Oxygen delivery, mitochondrial ATP production, and lactate buffering impose physical limits on sustained cognitive output. These are constraints of biochemistry, not policy. However, the structural data reveals this perspective as risk of false summit: the constraint's extractiveness (0.52) and suppression (0.68) exceed typical mountains, indicating that institutional arrangements and market incentives are amplifying what might otherwise be a manageable coordination problem.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_constraint_cognition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metabolic_constraint_cognition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(metabolic_constraint_cognition, TR),
    TR >= 0.70.

:- end_tests(metabolic_constraint_cognition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ATP ceiling is exploitable because institutional actors (pharma, platforms, employers) profit from demand that exceeds sustainable capacity. The extraction is not maximal—it could theoretically reach 0.75+ if total cognitive collapse were acceptable—but current institutional arrangements extract substantially while maintaining enough cognitive function for continued labor. The measurement trajectory (0.28→0.52 over 10 years) reflects accelerating institutional demand and pharmaceutical market expansion. Suppression (0.68): High. Significant barriers prevent exit: (a) economic—debt, mortgage, healthcare access lock workers into high-demand roles; (b) social—cognitive overwork is valorized as commitment, dedication, excellence; (c) technological—always-on communication systems eliminate recovery windows; (d) structural—high-skill labor markets offer no low-demand alternatives with comparable compensation. Theater ratio (0.38): Moderate. Productivity signals (visible calendars, response speed, meeting presence) are increasingly performative—they signal commitment but decline actual cognitive output. Theater has grown as tools for workplace visibility have proliferated. The ratio remains below 0.5 because some productive work genuinely occurs; the constraint is not purely theatrical like piton, but performance inflation is detectable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. The knowledge worker sees a snare—they are trapped in unsustainable demand with no exit. The pharmaceutical company sees a rope—the ATP ceiling creates a reliable market for stimulants and cognitive enhancers. The surveillance platform sees rope—attention scarcity is a coordination feature they exploit. The labor organizer sees a scaffold—working-time norms can reset the constraint within a generation through policy change. The productivity ideology manager sees a piton—the overwork ritual persists through institutional inertia, not function. The civilizational observer risks seeing a mountain—ATP limits are laws of biochemistry. But the structural data contradicts the mountain: if the constraint were purely metabolic, extractiveness would not be rising (0.28→0.52 over the interval), and suppression would be symmetrically distributed. Rising extractiveness indicates institutional amplification; asymmetric suppression indicates that some actors benefit from the constraint while others do not.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the actor's structural position relative to the ATP ceiling. Cognitive workers occupy d≈0.95 (full targets): they bear the costs of ATP depletion while institutional actors capture the benefits. Pharmaceutical manufacturers occupy d≈0.05 (full beneficiaries): they profit from cognitive overextension without bearing metabolic costs. Surveillance platforms occupy d≈0.08: attention scarcity is their revenue source; they benefit from the constraint's existence. Labor organizers occupy d≈0.60: they are partially victims (constrained by existing demand structures) but also have some agency and exit paths through collective action. The productivity ideology managers occupy d≈0.12: they benefit from the perceived legitimacy of overwork, but they also suffer from the institutional inertia of outdated performance metrics. The wide spread in d values (0.05 to 0.95) explains the perspectival gap: beneficiaries experience negative effective extraction (χ<0), while victims experience χ well above base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by revealing how a biological limit becomes extracted through institutional choice. The mountain perspective (metabolic laws are immutable) is a false summit—the constraint's rising extractiveness proves that institutional arrangements, not biology, are the binding factor. The rope perspective (pharmaceutical manufacturers see pure coordination) is legitimate but incomplete: yes, stimulants do coordinate around the ceiling, but they also prevent the institutional adjustment that would make stimulants unnecessary. The snare perspective (workers are trapped) is the structural reality: trapped agents have no arbitrage options and bear full costs. The scaffold perspective (organizing can change norms) is the actionable exit: working-time protections and sabbatical policies can reset expectations within a generation. The piton perspective (productivity theater persists) is accurate diagnosis: performance metrics persist despite evidence they reduce actual cognitive output. The tangled rope perspective (moderate agents experience mixed extraction/coordination) captures the realistic intermediate position. The mandatrophy is resolved by recognizing that the constraint is NOT a biological law being exploited (false summit) but an institutional choice to demand cognitive output exceeding sustainable ATP availability while capturing the surplus through pharmaceutical markets and attention extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_reserve_vs_ceiling,
    'Is the ATP limit a hard biological ceiling or a cognitively-negotiable boundary that varies by individual metabolic efficiency, neural optimization, and motivational state?',
    'Longitudinal studies of sustained cognitive output across populations with varying training, fitness, and metabolic profiles. Comparison of measured ATP dynamics vs reported cognitive fatigue thresholds.',
    'If hard ceiling: Mountain classification gains credence; suppression is unavoidable. If negotiable: the constraint is more Tangled Rope than mountain; institutional incentive structures are amplifying what biology alone would not enforce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_reserve_vs_ceiling, empirical, 'Whether ATP ceiling is a hard biological limit or cognitively negotiable').

omega_variable(
    pharmacological_substitution_sufficiency,
    'Do pharmaceutical interventions (stimulants, nootropics, enhanced mitochondrial function) genuinely extend the ATP ceiling or merely delay metabolic exhaustion while increasing long-term cognitive decline?',
    'Long-term cohort studies of chronic stimulant users vs controls; measurement of sustained cognitive performance over 5-20 year horizons; autopsies for neuroinflammation and mitochondrial damage.',
    'If sufficient: the constraint is purely exploitable—pharmacological bypass makes rope/snare dynamics permanent. If insufficient: deferred exhaustion reveals extraction mechanism—the industry profits from the ceiling without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmacological_substitution_sufficiency, empirical, 'Whether pharmacological interventions durably extend ATP capacity').

omega_variable(
    institutional_demand_elevation,
    'How much of the experienced ATP ceiling is biological limit vs institutional demand escalation—i.e., how many hours of daily cognitive work did knowledge workers sustain in pre-digital labor markets vs post-always-on expectations?',
    'Historical analysis of documented work hours, cognitive task complexity, and recovery time across decades. Controlled experiments comparing productivity under varying demand structures. Cross-cultural comparison of cognitive output expectations.',
    'If biological limit dominant: suppression is unavoidable (mountain tendency). If institutional escalation dominant: the constraint is Tangled Rope—institutions could reduce demand, but extraction incentives prevent it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_demand_elevation, empirical, 'Proportion of ATP ceiling due to biology vs institutional demand').

omega_variable(
    sabbatical_effectiveness,
    'Do generational sabbatical policies and right-to-disconnect laws actually reset cognitive capacity and prevent degradation, or do they merely delay and redistribute exhaustion across demographic cohorts?',
    'Comparative analysis of burnout rates, cognitive performance, and long-term career trajectories in jurisdictions with vs without sabbatical/disconnect protections. Measurement of ATP dynamics and cognitive recovery during enforced rest periods.',
    'If effective: Scaffold perspective confirmed—norms can reset the constraint. If ineffective: the constraint persists despite policy intervention, suggesting extraction mechanisms override institutional protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sabbatical_effectiveness, empirical, 'Whether sabbatical and disconnect policies durably reset cognitive capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metabolic_constraint_cognition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metcog_tr_t0, metabolic_constraint_cognition, theater_ratio, 0, 0.15).
narrative_ontology:measurement(metcog_tr_t5, metabolic_constraint_cognition, theater_ratio, 5, 0.28).
narrative_ontology:measurement(metcog_tr_t10, metabolic_constraint_cognition, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(metcog_be_t0, metabolic_constraint_cognition, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(metcog_be_t5, metabolic_constraint_cognition, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(metcog_be_t10, metabolic_constraint_cognition, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metabolic_constraint_cognition, resource_allocation).
narrative_ontology:affects_constraint(metabolic_constraint_cognition, attention_scarcity_commodification).
narrative_ontology:affects_constraint(metabolic_constraint_cognition, stimulant_dependency_escalation).
narrative_ontology:affects_constraint(metabolic_constraint_cognition, cognitive_labor_precarity).

% DUAL FORMULATION NOTE:
% The ATP ceiling decomposes into three structurally distinct constraints: (1) metabolic_constraint_cognition (this story, ε=0.52) — institutional demand exceeding sustainable capacity; (2) pharmacological_bypass_circularity (ε=0.58) — stimulant solutions that prevent institutional adjustment; (3) attentional_surplus_extraction (ε=0.48) — platforms extracting value from attention scarcity. Each has distinct beneficiaries, victims, and potential exits. They are linked: pharmaceutical intervention depends on the ceiling; attention extraction depends on scarcity the ceiling creates; institutional demand is amplified by pharmacological options that make overextension temporarily possible. This story focuses on the institutional demand amplification and the snare/rope/scaffold dynamics it creates. Upstream biological constraint (mitochondrial ATP limits) has ε≈0.15 (mountain). Downstream constraint (attention commodification) has ε≈0.48 (tangled rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metabolic_constraint_cognition, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
