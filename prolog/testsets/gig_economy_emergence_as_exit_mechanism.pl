% ============================================================================
% CONSTRAINT STORY: gig_economy_emergence_as_exit_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_emergence_as_exit_mechanism, []).

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
 *   constraint_id: gig_economy_emergence_as_exit_mechanism
 *   human_readable: Gig Economy Emergence as Exit Mechanism from Traditional Employment Constraint
 *   domain: economic_labor_relations
 *
 * SUMMARY:
 *   The gig economy emerged over the past 15 years as a structural response
 *   to labor market rigidities in traditional employment: credential
 *   requirements, geographic lock-in, scheduling inflexibility, and
 *   benefits-tied-to-employer dependency. It simultaneously functions as a
 *   genuine exit mechanism (workers escape trapped circumstances) and an
 *   extraction mechanism (platforms capture surplus through classification
 *   arbitrage and algorithmic management). This constraint exhibits the full
 *   range of DR types depending on observer position: desperate workers in
 *   genuine traps see a snare; strategic participants see tangled rope with
 *   real benefits; platform operators see pure coordination;
 *   capital-intensive employers see profitable extraction; unions see
 *   organizing opportunity; reformers see a temporary problem with a sunset;
 *   the traditional employment norm persists as degraded ritual; and
 *   civilizational analysis risks naturalizing contingent policy as economic
 *   law. The theater ratio has remained moderate (0.48) because the gig
 *   framing doesn't require elaborate legitimacy performance — the appeal to
 *   'freedom' and 'flexibility' is genuinely experienced by some while
 *   remaining invisible to others. Extractiveness has accelerated from 0.28
 *   to 0.58 over the interval as platforms have consolidated market power,
 *   algorithmic management has tightened, and the population of dependent gig
 *   workers has grown. Suppression shows a dual nature: structural barriers
 *   (credential lock-in, geographic relocation costs) that gig economy
 *   genuinely reduces, and new algorithmic barriers (deactivation thresholds,
 *   rating-based blocking) that replace them.
 *
 * KEY AGENTS:
 *   - Desperate Gig Workers: Primary victim (powerless/trapped) — lack credential recognition or geographic mobility; use gig work as exit from domestic violence, underemployment, or geographic isolation; trapped when alternatives are absent.
 *   - Strategic Gig Participants: Secondary victim/moderate beneficiary (moderate/constrained) — use gig flexibility for education, caregiving, skill-building; exit is costly but possible; experience genuine mix of coordination and extraction.
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture network effects and classification arbitrage; experience constraint as pure coordination mechanism; have maximum exit flexibility.
 *   - Capital-Intensive Employers: Beneficiary (powerful/mobile) — use gig platforms to manage demand volatility without fixed labor costs; choose to leverage gig despite mobile alternatives; benefits from labor cost arbitrage.
 *   - Traditional Labor Unions: Organized actor (organized/constrained) — face declining membership from gig shift but see organizing opportunity; can mobilize to shift classification; possess real agency despite constraints.
 *   - Regulatory Reformers: Powerful reformer (powerful/mobile) — implementing portable benefits, algorithmic transparency, classification floors; see this as temporary coordination failure with sunset.
 *   - Traditional Employment Stability Norm: Institutional residue (institutional/arbitrage) — mid-20th-century bargain persists as vocabulary while structure degrades; maintained through inertia rather than function.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing precarity as economic necessity rather than policy choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_emergence_as_exit_mechanism, 0.58).
domain_priors:suppression_score(gig_economy_emergence_as_exit_mechanism, 0.52).
domain_priors:theater_ratio(gig_economy_emergence_as_exit_mechanism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_emergence_as_exit_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(gig_economy_emergence_as_exit_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gig_economy_emergence_as_exit_mechanism, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_emergence_as_exit_mechanism, tangled_rope).
narrative_ontology:human_readable(gig_economy_emergence_as_exit_mechanism, "Gig Economy Emergence as Exit Mechanism from Traditional Employment Constraint").
narrative_ontology:topic_domain(gig_economy_emergence_as_exit_mechanism, "economic_labor_relations").

domain_priors:requires_active_enforcement(gig_economy_emergence_as_exit_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_emergence_as_exit_mechanism, platform_operators).
narrative_ontology:constraint_beneficiary(gig_economy_emergence_as_exit_mechanism, capital_intensive_employers).
narrative_ontology:constraint_beneficiary(gig_economy_emergence_as_exit_mechanism, consumers_of_gig_services).
narrative_ontology:constraint_victim(gig_economy_emergence_as_exit_mechanism, gig_workers_precariat).
narrative_ontology:constraint_victim(gig_economy_emergence_as_exit_mechanism, traditional_employment_stability).
narrative_ontology:constraint_victim(gig_economy_emergence_as_exit_mechanism, labor_standards_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESPERATE GIG WORKER (SNARE) — Trapped in gig model due to absent alternatives, lack of credential recognition across jurisdictions, and immediate income needs. Presented as 'freedom' and 'flexibility,' gig work extractively removes wage protections, benefits continuity, and scheduling stability. Worker bears suppression through algorithmic management and precarious income while platform captures surplus. No genuine exit available — traditional employment is locked behind credentials or geographic relocation barriers.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STRATEGIC GIG PARTICIPANT (TANGLED ROPE) — Moderate power agent who genuinely benefits from gig flexibility for education, caregiving, or skill-building while bearing costs of benefit discontinuity and income volatility. Exit is possible but costly (credential investment, relocation, time gap). Experiences genuine coordination function (matching supply/demand at scale) alongside extraction (wage compression, arbitrage of benefits package). Real perspectival gap: this agent's experience diverges sharply from the desperate worker's — same platform, radically different structural relationship.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Institutional actor with maximum arbitrage optionality. Experiences the constraint as pure coordination: matching workers to tasks at unprecedented scale and speed. Captures network effects and data advantage. From this perspective, the gig model solves the genuine coordination problem of dispersed labor supply and granular demand. The extraction is invisible to this agent — framed as value creation rather than surplus capture.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL-INTENSIVE EMPLOYER (TANGLED ROPE) — Powerful agent with mobile exit options but structurally benefits from gig platform ecosystem. Uses gig workers to manage demand volatility without fixed labor costs or benefits obligations. Experiences genuine coordination benefit (demand-responsive scaling) alongside extraction benefit (labor cost arbitrage). Not trapped; chooses to leverage gig model because it's more extractive than traditional employment while framing the choice as operational flexibility.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL LABOR UNIONS (TANGLED ROPE) — Organized agent facing structural constraint but with real agency. Unions benefit from the crisis this creates (organizing opportunity) and bear cost (membership decline, jurisdictional ambiguity). Organized power creates constrained rather than trapped exit. The constraint requires active enforcement (classification arbitrage: are gig workers employees?) to persist. Unions have demonstrated capacity to shift classification through organizing and litigation, making this a tangled_rope from the organized perspective rather than a snare.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY REFORMERS (SCAFFOLD) — Powerful agents (states, labor departments, legislative bodies) with mobile options implementing sunset mechanisms through classification reform and benefits portability. Sees gig emergence as a temporary coordination failure to be resolved through new institutional forms: portable benefits, algorithmic transparency requirements, minimum standards floors. Not an exit FROM the gig model but an exit FROM the current extractive form of it. Theater low because reformers openly acknowledge the underlying problem rather than perform legitimacy. Sunset: estimated 10-15 years for benefits portability standards to mature.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: TRADITIONAL EMPLOYMENT STABILITY (PITON) — The mid-20th century postwar bargain (employer loyalty = benefits security = identity stability) has functionally atrophied. The constraint is the residual performance of this norm's legitimacy: governments and employers still claim to value stable employment while structurally incentivizing gig precarity. Theater-ratio high because the ritual observance of 'good jobs' persists while the structural form degrades into contingency. Institutional inertia maintains the norm's vocabulary while the constraint itself operates as a snare for those trapped in it.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some labor precarity is inherent to capital: perfect employment stability is structurally impossible when firms must remain competitive and responsive to demand shocks. This perspective naturalizes precarity as economic law. The engine's false summit detection will flag this: the structural data reveals contingent institutional choices (benefits arbitrage, classification manipulation, algorithmic management) that are not natural laws but coordinated policy regimes.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_emergence_as_exit_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_emergence_as_exit_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_economy_emergence_as_exit_mechanism, TR),
    TR >= 0.70.

:- end_tests(gig_economy_emergence_as_exit_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over the interval. The gig model extracts through multiple mechanisms: (1) classification arbitrage — workers classified as independent contractors avoid triggering employer benefit obligations; (2) algorithmic wage compression — wage-setting algorithms exploit lack of unionization and transparency; (3) demand-side benefits capture — consumers benefit from labor cost arbitrage without paying higher prices. Initial extractiveness (0.28) reflected partial legitimacy of the exit mechanism narrative — for some workers, gig work genuinely solved serious problems. Current value (0.58) reflects consolidation: as alternatives to gig work remain constrained and gig has become primary income source for millions, the extraction mechanism has tightened. Suppression (0.52): Moderate. Gig work genuinely reduces some suppression barriers (credential requirements, geographic lock-in) that traditional employment maintains. Simultaneously, it introduces new suppression (algorithmic control, precarious income, no scheduling predictability). Net suppression remains substantial because the barriers it removes are replaced rather than eliminated — workers aren't gaining autonomy, they're exchanging one set of constraints for another. Theater ratio (0.48): Moderate, indicating genuine coordination benefits alongside extractive mechanisms. The gig framing doesn't require elaborate legitimacy theater because 'freedom' and 'flexibility' are credible values for some workers while remaining invisible to others who experience only precarity. The moderate theater reflects this perspectival split rather than performative elements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap driven by structural position rather than power level alone. A desperate worker in geographic or credential isolation sees a genuine exit from an impossible situation — the snare of no alternatives. The same gig platform appears to a strategic participant as mixed coordination and extraction, generating real value while imposing costs they can absorb. The platform operator experiences pure coordination: the network matching problem they solve. Capital-intensive employers experience profitable extraction while framing it as operational flexibility. Unions recognize the constraint as a threat and opportunity simultaneously. Reformers see a temporary coordination failure being resolved through institutional evolution. The piton perspective recognizes that traditional employment stability has atrophied into ritual. The civilizational analytical view risks naturalizing the whole arrangement as economic law. All these readings are legitimate from within their structural positions — the engine's job is to render them all simultaneously visible, revealing that gig economy emergence is not a single constraint but a presheaf of structurally distinct constraints layered onto the same institutional form.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from beneficiary/victim status combined with exit options. Desperate workers trapped with no alternatives: d ≈ 0.95 (maximum target). Strategic participants with constrained but real exit options and mixed beneficiary/victim status: d ≈ 0.50 (symmetric). Platform operators with arbitrage options and beneficiary status: d ≈ 0.10 (strong beneficiary). Capital-intensive employers with mobile options and beneficiary status: d ≈ 0.20 (significant beneficiary). Unions with organized power and constrained exit: d ≈ 0.55 (mixed with some agency). Reformers with mobile options and beneficiary framing (fixing the system benefits society): d ≈ 0.35 (moderate beneficiary). Traditional employment norm (not an agent, a residual structure): no d value needed. Analytical observer: d ≈ 0.72 (analytical canonical fallback). These d values, combined with f(d) sigmoid conversion and σ(S) scope scaling, produce the experienced extractiveness chi for each perspective. The beneficiary/victim declarations (platform_operators, capital_intensive_employers, consumers_of_gig_services as beneficiaries; gig_workers_precariat, traditional_employment_stability, labor_standards_enforcement as victims) are structurally accurate: the gig model transfers surplus from labor precariat and collective labor standards to platform operators and demand-side consumers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint nominally resolves the mandatrophy by declaring tangled_rope as the claimed type with explicit beneficiaries (three distinct groups), victims (three distinct groups), and enforcement requirements. However, the eight perspectives reveal that mandatrophy resolution is more nuanced. The desperate worker perspective classifies as snare — pure extraction with no coordination function visible from their position. The strategic participant perspective classifies as tangled_rope — the coordination function (matching at scale) is genuinely visible. The platform operator perspective classifies as rope — pure coordination, no extraction visible. The powerful employer perspective classifies as tangled_rope with different beneficiary/victim alignment than the worker perspective. Unions see tangled_rope with institutional agency. Reformers see scaffold (temporary failure being remedied). The piton perspective reveals that traditional employment hasn't disappeared but degraded into theater. The mountain perspective risks naturalizing the whole arrangement. The mandatrophy resolves by acknowledging that all eight readings are structurally legitimate, but the distribution of types reveals the constraint's true structure: it appears as pure coordination to those capturing surplus (rope), as pure extraction to those bearing costs (snare), and as mixed coordination-extraction to those with moderate power to resist or benefit (tangled_rope). The constraint persists because it benefits institutional actors with arbitrage options and extraction gain, while imposing costs on precariat workers and collective labor standards enforcement. Calling it tangled_rope at the analytical level is accurate — it has genuine coordination benefits alongside extraction — but fails to capture that the distribution of types across perspectives is itself the diagnosis: when a constraint looks like rope to the powerful and snare to the powerless, mandatrophy resolution requires acknowledging this is not a coordination problem with extraction as side effect, but an extraction mechanism masquerading as coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_mechanism_or_extraction_substitution,
    'Does gig economy emergence function as a genuine exit mechanism FROM the traditional employment constraint, or does it substitute one extraction form (fixed-wage exploitation) with another (algorithmic precarity extraction)?',
    'Longitudinal comparison of net worker welfare outcomes: wage trajectories, benefit access, income stability, and work-life autonomy for cohorts choosing gig vs traditional employment with equivalent credentials and geographic mobility. Distinguish between those escaping genuine traps (domestic violence, geographic isolation, credential barriers) and those choosing gig for marginal convenience gains.',
    'If genuine exit: gig economy classifies as rope or scaffold from many perspectives — removes barriers, enables agency. If substitution: remains snare/tangled_rope — new trap door replaces old. Classification implications: perceived extractiveness may remain ~0.58 in both cases, but the mandatrophy resolution differs. If exit, the constraint was legitimate bottleneck. If substitution, the constraint is mislabeled freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mechanism_or_extraction_substitution, empirical, 'Whether gig economy is genuine exit or extraction substitution').

omega_variable(
    classification_manipulation_as_coordination,
    'To what degree does the gig economy''s institutional persistence depend on jurisdictional classification ambiguity (independent contractor vs employee) vs genuine coordination benefits?',
    'Counterfactual: measure platform coordination efficiency under hypothetical scenarios (mandatory employee classification, mandatory benefits portability, algorithmic wage floors). Compare platform utilization rates and network effects before/after classification changes in jurisdictions that have implemented reforms (UK, EU).',
    'If coordination-dependent: high efficiency losses from classification changes; gig model solves real coordination problem. Reduces effective extractiveness. If classification-manipulation-dependent: platforms remain efficient under reform; extractiveness increases under higher labor standards. Reclassifies from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_manipulation_as_coordination, empirical, 'Extent of gig coordination dependence on classification arbitrage').

omega_variable(
    identity_locked_precariat_formation,
    'For long-term gig workers, does precarity become internalized as identity rather than remaining a structural barrier they could overcome through credential investment or relocation?',
    'Longitudinal ethnographic and survey data: track cohort''s self-description, aspirations, and exit behavior over 10+ years. Monitor whether workers retain sense of temporal contingency (we''ll transition to stable work when conditions allow) or adopt identity positioning (gig is my permanent condition). Analyze decision patterns: do workers invest in credentials that enable exit, or accept permanent precarity?',
    'If identity-locked emerges: classify long-term gig worker exit_options as identity_locked rather than trapped. Implies the exit mechanism is cognitive-framing rather than structural. Increases effective suppression in measurements over biographical/generational horizon as internalization deepens. Suggests mandatrophy risk: the exit mechanism becomes self-enforcing (workers don''t try to leave because they can''t imagine themselves outside precarity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_precariat_formation, empirical, 'Precarity internalization into identity among long-term gig workers').

omega_variable(
    algorithmic_suppression_opacity,
    'Does the measured suppression (0.52) accurately capture algorithmic management coercion, or does opacity in pricing algorithms and task allocation conceal higher effective suppression than the structural metric suggests?',
    'Algorithmic auditing: systematic measurement of task availability, earnings variance, and deactivation thresholds across worker cohorts. Comparison of worker perception of ''freedom'' vs measurable constraints imposed through algorithmic systems (acceptance rate thresholds, rating-based blocking, surge-pricing opacity). Survey-based suppression self-reporting vs researcher-measured constraints.',
    'If opacity conceals higher suppression: reclassify as snare-adjacent rather than tangled_rope. Increases effective suppression in the constraint. If suppression perception matches measurable coercion: current (0.52) value is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_suppression_opacity, empirical, 'Algorithmic suppression opacity and actual constraint magnitude').

omega_variable(
    network_effects_as_false_coordination,
    'Do network effects in platform matching (more workers → better matching → more users → more work) constitute genuine coordination benefits, or do they mask extraction through switching costs and lock-in?',
    'Measure platform switching costs: barriers to multi-homing, data portability, reputation transfer, and task discovery outside the dominant platform. Compare worker earnings and autonomy in markets with competing platforms vs monopoly platforms. Analyze whether network growth benefits workers proportionally or concentrates benefits with platform operators.',
    'If genuine coordination: tangled_rope classification is correct — real coordination function alongside extraction. If lock-in masquerading as network benefit: platform operator perspective should downgrade from rope toward snare-with-institutional-power. Reclassifies institutional actor''s experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_as_false_coordination, empirical, 'Network effects as coordination vs switching-cost lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_emergence_as_exit_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig__tr_t0, gig_economy_emergence_as_exit_mechanism, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gig__tr_t5, gig_economy_emergence_as_exit_mechanism, theater_ratio, 5, 0.4).
narrative_ontology:measurement(gig__tr_t10, gig_economy_emergence_as_exit_mechanism, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(gig__be_t0, gig_economy_emergence_as_exit_mechanism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gig__be_t5, gig_economy_emergence_as_exit_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gig__be_t10, gig_economy_emergence_as_exit_mechanism, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_emergence_as_exit_mechanism, resource_allocation).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, traditional_employment_benefit_dependency).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, labor_credential_lock_in).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, geographic_mobility_barriers).

% DUAL FORMULATION NOTE:
% The gig economy emergence represents a decomposition of constraints. The upstream constraints are traditional employment's barriers (credential lock-in, geographic lock-in, benefits dependency). Gig economy acts as exit mechanism for those trapped in the upstream constraints while simultaneously creating new downstream constraints (algorithmic precarity, classification arbitrage, benefits discontinuity). The family of related constraints includes the residual traditional employment norm (piton: degraded but still performing legitimacy ritual) and emerging regulatory reform frameworks (scaffold: temporary institutional responses). Each story in the family has distinct epsilon values: traditional employment barriers ε ≈ 0.65 (snare for locked-in workers); gig emergence as exit ε ≈ 0.58 (tangled rope — real benefits alongside extraction); regulatory reform ε ≈ 0.30 (scaffold — temporary coordination problem being solved).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gig_economy_emergence_as_exit_mechanism, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
