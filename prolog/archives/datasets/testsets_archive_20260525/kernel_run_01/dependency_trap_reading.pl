% ============================================================================
% CONSTRAINT STORY: dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dependency_trap_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dependency_trap_reading
 *   human_readable: Dependency Trap: Income Support as Skill Atrophy and Labor Market Exit
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   The dependency trap reading of unconditional income support frames the
 *   constraint as extractive transfer from working taxpayers to non-workers,
 *   with the mechanism operating through skill atrophy and labor market exit.
 *   This reading is one of three contending interpretations of the
 *   income_support_commitment kernel. The constraint models a genuine
 *   coordination problem (poverty reduction, income security) layered with
 *   extraction asymmetry (work-subsidizing non-work, skill degradation over
 *   biographical time, intergenerational welfare dependence). The reading
 *   explicitly adopts the position that unconditional income generates
 *   perverse incentives—that the 'unconditional' framing masks a dependency
 *   mechanism that simultaneously appears beneficial (ensures subsistence)
 *   and harmful (undermines human capital development and productive
 *   participation). The extractiveness value (0.52) reflects moderate
 *   asymmetry: the constraint does coordinate basic security, but the
 *   extraction from productive to non-productive class and the skill atrophy
 *   mechanism are structurally real under this reading.
 *
 * KEY AGENTS:
 *   - Unconditional Income Recipients (exiting labor): Primary beneficiaries (institutional/arbitrage) — receive income without work requirement; experience immediate welfare gain but face long-term skill and labor market positioning costs
 *   - Working Taxpayers: Primary victims (moderate/constrained) — fund redistribution; experience extraction as tax burden and framing suppression (dependency trap narrative suppressed or dismissed as inhumane); can organize politically but face collective action barriers
 *   - Skill-Atrophying Recipients: Secondary victims (powerless/trapped) — experience dependency trap most directly; initially benefit from income security but gradually lose labor market viability as skills degrade; face increasing isolation from productive participation
 *   - Welfare State Administration: Institutional actor (institutional/arbitrage) — benefits from expanded administrative apparatus and poverty reduction legitimacy; coordinates the transfer system; has arbitrage ability to adjust parameters
 *   - Labor Movement / Organized Working Class: Organized secondary actor (organized/constrained) — faces wage suppression from expanded unconditional income reducing labor scarcity; also benefits from reduced precarity and improved bargaining position from guaranteed subsistence
 *   - Tech-Enabled Retraining Coalition: Powerful secondary actor (powerful/mobile) — sees dependency trap as solvable through skills investment; building alternative pathways with sunset logic (online credentials, remote work, skill accessibility)
 *   - Post-Scarcity Ideological Frame: Institutional cover story (institutional/arbitrage) — the framing that work-disincentive is inherent and unacceptable is itself vestigial; maintained by status anxiety about post-labor society
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dependency_trap_reading, 0.52).
domain_priors:suppression_score(dependency_trap_reading, 0.48).
domain_priors:theater_ratio(dependency_trap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dependency_trap_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dependency_trap_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dependency_trap_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(dependency_trap_reading, "Dependency Trap: Income Support as Skill Atrophy and Labor Market Exit").
narrative_ontology:topic_domain(dependency_trap_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(dependency_trap_reading, fixed_text).
narrative_ontology:cs_authority_grounding(dependency_trap_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(dependency_trap_reading).
narrative_ontology:cs_kernel_id(dependency_trap_reading, income_support_commitment).
narrative_ontology:cs_reading_relation(dependency_trap_reading, freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation(dependency_trap_reading, targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom(dependency_trap_reading, foundational, labor_participation_economically_necessary).
narrative_ontology:cs_axiom_status(labor_participation_economically_necessary, holdable).
narrative_ontology:cs_axiom(dependency_trap_reading, foundational, unconditional_income_reduces_labor_supply).
narrative_ontology:cs_axiom_status(unconditional_income_reduces_labor_supply, holdable).
narrative_ontology:cs_reference_frame(dependency_trap_reading, welfare_state_with_work_as_primary_distributive_mechanism).
narrative_ontology:cs_drift_state(dependency_trap_reading, contemporary_automation_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dependency_trap_reading, unconditional_income_recipients_exiting_labor).
narrative_ontology:constraint_victim(dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(dependency_trap_reading, skill_atrophying_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SKILL-ATROPHYING RECIPIENT (SNARE) — Trapped by income support that simultaneously enables subsistence and undermines labor market re-entry. As labor atrophies over biographical time, structural exit from dependence becomes impossible despite the superficial 'unconditional' framing. The constraint extracts human capital and social positioning. No alternative pathway visible once skills degrade.
constraint_indexing:constraint_classification(dependency_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING TAXPAYER (SNARE) — Constrained by tax obligation and suppressed by framing of income support as uncontroversial good. Bears extraction as funding transfers to non-productive class expand. Limited exit: cannot unilaterally withdraw tax contribution (trapped-adjacent); can only exit through political organization or relocation. Experiences the constraint as asymmetric burden.
constraint_indexing:constraint_classification(dependency_trap_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE STATE ADMINISTRATION (ROPE) — Benefits from the coordination function: income support solves the immediate problem of mass poverty and reduces visible destitution. Administrative apparatus expands, bureaucratic legitimacy increases, and political stability is maintained. Experiences the constraint as coordination mechanism with net institutional benefit. Can arbitrage the system (adjust parameters, expand coverage) to maintain political support.
constraint_indexing:constraint_classification(dependency_trap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT / ORGANIZED WORKING CLASS (TANGLED ROPE) — Faces coordinated extraction (wage compression from expanded unconditional income reducing labor scarcity), but also benefits from reduced precarity and increased bargaining power from guaranteed subsistence. Constrains working conditions: can organize because basic survival is decoupled from immediate employment. Experiences genuine coordination function (collective security) alongside asymmetric extraction (wage suppression). Generational horizon reveals accumulating skill gaps in cohort replacement.
constraint_indexing:constraint_classification(dependency_trap_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TECH-ENABLED RETRAINING COALITION (SCAFFOLD) — Mobile, powerful actors (education sector, technology firms, workforce development agencies) see the dependency trap as a temporary coordination failure solvable through skills investment and online credential programs. The constraint has a potential sunset: as digital skills access expands and remote work enables labor market re-entry without geographic relocation, the skill atrophy mechanism loses force. Low experienced extraction because this perspective has agency and can build alternative pathways. However, sunset requires sustained investment and cultural shift — not automatic.
constraint_indexing:constraint_classification(dependency_trap_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-SCARCITY IDEOLOGICAL FRAME (PITON) — The dependency trap narrative assumes labor markets must remain the primary distributive mechanism and that unconditional income is a supplement to work rather than a replacement. This framing is increasingly performative: as automation expands and traditional employment shrinks, the frame that UBI must not 'disincentivize work' becomes a vestigial constraint maintained by institutional inertia and status anxiety among knowledge workers. The frame persists because alternatives (post-labor society, care economy valuation) threaten established credentialing hierarchies. Theater_ratio high because the 'dependency trap' story continues despite empirical evidence from pilot programs showing modest labor supply effects.
constraint_indexing:constraint_classification(dependency_trap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BEHAVIORAL ECONOMICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, the dependency trap is presented as a law of behavioral incentives: unconditional income necessarily reduces labor supply because income effects dominate substitution effects under standard behavioral assumptions. This perspective naturalizes what is actually a contingent institutional arrangement depending on relative wage levels, alternative status signaling mechanisms, and cultural valuation of labor. The engine will flag this as a false summit — the claim that work-disincentive effects are inherent to income support rather than socially constructed.
constraint_indexing:constraint_classification(dependency_trap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dependency_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dependency_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dependency_trap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dependency_trap_reading, TR),
    TR >= 0.70.

:- end_tests(dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate, reflecting genuine asymmetry without maximum exploitation. The constraint extracts from working taxpayers (via taxation) and from skill-atrophying recipients (via labor market closure). However, extraction is not total—unconditional income does provide genuine security benefit, and the magnitude of labor supply reduction appears empirically modest (5-15% in most pilot studies). The value reflects that this is Tangled Rope territory: real coordination function (poverty reduction, income security) exists alongside real extraction (skill atrophy, work-to-non-work transfer, intergenerational dependence). Suppression (0.48): Moderate. The dependency trap narrative itself constitutes suppression—the framing that unconditional income necessarily reduces work disincentivizes policy experimentation and locks in behavioral assumptions. However, suppression is not total because counter-narratives exist (freedom floor reading, targeting efficiency reading) and empirical evidence is accumulating. Theater ratio (0.35): Low-moderate. The dependency trap claim is not primarily performative; it rests on behavioral economic theory with some empirical support. However, the measurement's increase over the interval (0.28→0.35) reflects the growing performative use of dependency trap rhetoric as policy cover story—as pilot program data accumulated showing modest effects, the rhetorical deployment of the trap narrative intensified despite weakening empirical justification.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates maximum perspectival divergence across the observation site. The skill-atrophying recipient sees pure extraction (Snare)—trapped by a system that appears to help but structurally undermines re-entry. Working taxpayers see pure extraction (Snare)—forced contribution with no reciprocal benefit and narrative suppression preventing dissent. Welfare state administration sees coordination (Rope)—solving poverty through efficient redistribution. Labor movement sees mixed dynamics (Tangled Rope)—extraction via wage suppression, benefit via precarity reduction. Tech-enabled coalition sees temporary constraint (Scaffold)—solvable through skills investment and online credentials. Post-scarcity frame views work-disincentive as inherent law of behavior (Mountain, false summit)—naturalized when it is actually contingent on institutional design and cultural valuation of labor. The perspectival landscape reveals the dependency trap reading as fundamentally dependent on the observer's power position and exit options: those with power to exit or arbitrage see coordination; those trapped by the extraction see pure Snare; those with organizational capacity see temporary problems with solutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the structural relationship between each agent and the extraction flow. Working taxpayers are victims bearing costs (high d → high f(d) → high experienced χ). Unconditional income recipients are beneficiaries receiving transfers, but the reading emphasizes the long-term extraction they experience through skill atrophy (moderate-high d reflecting ambiguity between immediate benefit and biographical-horizon harm). Welfare state administration is beneficiary with arbitrage options (low d, can adjust parameters to maintain legitimacy). Labor movement experiences both extraction (wage suppression) and benefit (precarity reduction)—moderate d reflecting the Tangled Rope classification. Tech-enabled coalition has mobile exit (can build alternative systems), hence lower d despite the policy constraint itself having high suppression. The piton perspective on post-scarcity framing reflects that the ideological frame constraining unconditional income is itself inertial—maintained not by extraction but by institutional commitment to labor as primary value signaling mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The dependency trap reading does NOT resolve the mandatrophy. Instead, it instantiates the mandatrophy by demonstrating that the classification collapses when the kernel is contested. From the dependency trap perspective, unconditional income is Tangled Rope (mixed coordination and extraction). From the freedom floor perspective (sibling reading), the same institutional structure is Rope or even Mountain (foundational coordination mechanism, unavoidable feature of just society). From the targeting efficiency perspective (sibling reading), the constraint is Snare (extractive waste from lack of means-testing). The three readings are logically incompatible at the claim level (one reading's mechanism explanation contradicts another's) but structurally coexist as live policy positions held by different state actors and constituencies. The mandatrophy is not resolved by finding the 'true' type; it is resolved by recognizing that all three readings are legitimate extractions from the contested kernel, and the type diversity reflects real disagreement about what the income support commitment means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_disincentive_magnitude_threshold,
    'What magnitude of labor supply reduction constitutes ''dependency trap'' extraction versus acceptable policy trade-off for poverty reduction?',
    'Longitudinal empirical analysis of UBI pilot programs (Kenya, Finland, Stockton CA, Brazil) measuring labor supply changes, skill investment patterns, and entrepreneurship rates; comparison across different baseline wage levels and unconditional income amounts',
    'If labor supply reduction < 10%: dependency trap framing is overstated; constraint reclassifies as Rope (coordination-dominant). If > 20%: dependency trap is confirmed; constraint remains Tangled Rope or Snare depending on redistributive effects. If effects vary by demographic (age, gender, prior employment): constraint disaggregates into separate stories by cohort.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(work_disincentive_magnitude_threshold, empirical, 'Threshold labor supply elasticity defining dependency trap magnitude').

omega_variable(
    skill_atrophy_mechanism_causality,
    'Does unconditional income CAUSE skill atrophy (reduced labor market engagement degrades human capital), or does it REVEAL existing skill deficits by removing necessity for precarious employment?',
    'Controlled panel analysis of recipients'' skill acquisition rates pre- and post-income transfer; comparison with control groups in labor-coercive systems; assessment of whether recipients use freed time for education versus leisure',
    'If causality confirmed (income→atrophy): dependency trap is real structural mechanism; Snare classification appropriate for biographical perspective. If revelation (income reveals prior deficits): the ''trap'' is a misdiagnosis of inequality structure; constraint reclassifies as Rope or Tangled Rope with different victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_mechanism_causality, empirical, 'Whether unconditional income causes or reveals skill gaps').

omega_variable(
    reading_vs_freedom_floor_foreclosure,
    'Does the dependency trap reading (extraction from productive to non-productive class) logically foreclose the freedom floor reading (unconditional income as foundational liberty enabling real autonomy)?',
    'Conceptual analysis of whether both readings can hold simultaneously within a single normative framework; examination of whether beneficiary-class expansion necessarily reduces freedom floor for remaining workers, or whether expanded freedom floor for recipients increases net societal autonomy despite labor supply changes',
    'If foreclosure confirmed: readings cannot coexist in single framework; this reading''s axioms directly contradict freedom floor axioms; constraint manifests as epistemic competition rather than institutional coexistence. If no foreclosure: readings coexist as different value orderings; both retain ''holdable'' status; ambiguity routes through omega variables rather than being resolved by structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_freedom_floor_foreclosure, conceptual, 'Whether dependency trap and freedom floor readings logically exclude each other').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the ''dependency trap'' reading an honest structural analysis of incentive effects, or a rhetorical cover for resistance to redistribution by beneficiaries of existing inequality?',
    'Historical and discourse analysis: track the dependency trap narrative''s evolution; identify which actors promote it; correlate with material interests in tax minimization vs. redistribution; examine whether the same actors accept skill atrophy arguments in other contexts (e.g., accepting structural unemployment as price of price stability)',
    'If rhetorical cover: the reading''s authority derives from committer material interest rather than empirical foundation; constraint''s legitimacy is subject to false-summit detection; the ''natural'' barriers to unconditional income are actually ideological commitments. If structural analysis: the reading captures real incentive dynamics deserving analysis; committer interests do not negate structural reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, preference, 'Whether dependency trap framing reflects structural analysis or rhetorical rationalization').

omega_variable(
    sibling_reading_targeting_efficiency_influence,
    'Does the dependency trap reading''s effectiveness in suppressing unconditional income create structural pressure on the targeting efficiency reading by making means-tested alternatives appear comparatively legitimate?',
    'Policy genealogy: examine whether countries that adopted targeting-efficiency rhetoric did so independently or as rhetorical counter to dependency trap arguments; measure whether availability of dependency trap framing reduces policy experimentation with unconditional approaches',
    'If influence confirmed: this reading affects resource allocation between policy approaches; influences which constraints are perceived as legitimate objects of state action; creates structural coupling with targeting efficiency reading. If no influence: readings operate independently; both can persist simultaneously without interaction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_targeting_efficiency_influence, empirical, 'Whether dependency trap framing influences adoption of targeting efficiency approaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dependency_trap_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deptrap_tr_t0, dependency_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(deptrap_tr_t5, dependency_trap_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(deptrap_tr_t10, dependency_trap_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(deptrap_be_t0, dependency_trap_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(deptrap_be_t5, dependency_trap_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(deptrap_be_t10, dependency_trap_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(dependency_trap_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel has three structurally distinct readings with different ε values and beneficiary/victim structures. Each reading is its own constraint story: dependency_trap_reading (ε=0.52, Tangled Rope), freedom_floor_reading (ε≤0.30, Rope or Mountain), targeting_efficiency_reading (ε≥0.60, Snare). These are not alternative measurements of one constraint; they are different constraints derived from the same kernel through different readings of what that kernel means. Each story links via network.affects_constraints to document the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
