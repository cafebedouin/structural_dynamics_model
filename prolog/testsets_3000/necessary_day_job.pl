% ============================================================================
% CONSTRAINT STORY: necessary_day_job
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_necessary_day_job, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: necessary_day_job
 *   human_readable: The Necessary Day Job for Creatives
 *   domain: economic/social
 *
 * SUMMARY:
 *   The necessary day job represents a fundamental structural constraint in
 *   creative economies where artistic production cannot generate subsistence
 *   income without access to capital, pre-existing audience, or institutional
 *   patronage. This constraint decouples creative vocation from livelihood,
 *   creating a systematic extraction of cognitive and temporal resources from
 *   creative work. The constraint exhibits hybrid coordination-extraction
 *   characteristics: employers benefit from access to educated labor at
 *   below-market rates (coordination benefit); creative practitioners are
 *   trapped between biological necessity and vocational desire (extraction
 *   cost). The day job has intensified over recent decades as creative
 *   industries have concentrated wealth upward (winner-take-most dynamics)
 *   while creative income thresholds have risen (larger audience required for
 *   sustainability). Simultaneously, emerging alternatives — remote work
 *   flexibility, digital platforms enabling direct monetization, arts funding
 *   ecosystem expansion, and UBI pilots — are creating potential exit routes
 *   that may reduce the constraint's severity over a generational timescale.
 *
 * KEY AGENTS:
 *   - Creative Practitioners: Primary victims (powerless/trapped) — depend on day job for subsistence while bearing full opportunity cost of constrained creative time
 *   - Employers: Primary beneficiaries (institutional/arbitrage) — access educated, motivated labor force at suppressed wage rates; benefit from employment-based discipline
 *   - Labor Market Gatekeepers: Secondary beneficiaries (institutional/arbitrage) — maintain wage compression through employment classification and benefits systems
 *   - Artistic Output Quality: Primary victim (abstract) — constrained creative output degrades artistic field; reduced experimental work, higher safe/commercial bias
 *   - Arts Funding Ecosystem: Organized agents (organized/constrained) — grants, residencies, patronage systems attempting to reduce day job necessity; success varies by discipline
 *   - Digital Platforms: Organized agents (organized/arbitrage) — reduce publishing friction and audience access costs; enable direct monetization (Patreon, Substack, digital music) but extract platform rent
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as hybrid coordination-extraction mechanism; reveals how subsistence dependency disciplines labor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(necessary_day_job, 0.52).
domain_priors:suppression_score(necessary_day_job, 0.68).
domain_priors:theater_ratio(necessary_day_job, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(necessary_day_job, extractiveness, 0.52).
narrative_ontology:constraint_metric(necessary_day_job, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(necessary_day_job, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(necessary_day_job, tangled_rope).
narrative_ontology:human_readable(necessary_day_job, "The Necessary Day Job for Creatives").
narrative_ontology:topic_domain(necessary_day_job, "economic/social").

domain_priors:requires_active_enforcement(necessary_day_job).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(necessary_day_job, employers).
narrative_ontology:constraint_beneficiary(necessary_day_job, labor_market_gatekeepers).
narrative_ontology:constraint_victim(necessary_day_job, creative_practitioners).
narrative_ontology:constraint_victim(necessary_day_job, artistic_output_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE STRUGGLING ARTIST (SNARE) — Creative practitioner cannot exit wage labor without sacrificing basic subsistence. Trapped between biological necessity (food, shelter, healthcare) and creative vocation. Day job claims majority cognitive and temporal resources, leaving only residual energy for creative work. No alternatives: artistic income is insufficient in early/mid-career stages; savings are absent; geographic mobility is constrained by employment. Maximum extraction experienced.
constraint_indexing:constraint_classification(necessary_day_job, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER CREATIVE PROFESSIONAL (TANGLED ROPE) — Some creative income achieved through partial success (grants, commissions, small audience), but still requires day job to hedge income volatility. Experiences constraint as mixed: day job enables stability (coordination benefit — reduces risk of complete creative abandonment) while simultaneously constraining creative output (asymmetric extraction). Career trajectory requires maintaining day job employment while competing for limited creative opportunities. Moderate agency but constrained by need to maintain employment.
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EMPLOYER (ROPE) — Benefits from access to educated, motivated labor force at below-market rates. Creative workers are systematically underpaid relative to their education and capability because their labor supply is inelastic (they cannot exit due to subsistence requirement). Day job serves as labor market coordination mechanism: predictable wage, benefits structure, employment relationship. Employer experiences the constraint as pure coordination — extracting labor value is incidental to the employment relationship, not its primary function. Net beneficiary.
constraint_indexing:constraint_classification(necessary_day_job, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING ARTS SUPPORT ECOSYSTEM (SCAFFOLD) — Organized agents (arts grants, residencies, artist collectives, collaborative funding models, remote work normalization) are building alternative pathways that reduce day job dependency. Universal Basic Income pilots, patronage systems, and distributed digital funding create exit routes from the traditional constraint. Theater low because alternative mechanisms (grants, Patreon, crowdfunding) are functionally transparent rather than performative. Sunset logic: As digital platforms reduce publishing friction and audience access costs decline, creative income thresholds lower and day job necessity diminishes over 15-25 year horizon.
constraint_indexing:constraint_classification(necessary_day_job, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ARTS PATRONAGE INSTITUTION (PITON) — Traditional patronage, foundational support structures, and institutional arts funding remain substantially performative. Grant selection appears meritocratic but reflects existing status (artists already with institutional backing have higher success rates). Foundation review committees and institutional gatekeepers maintain appearance of neutral evaluation while perpetuating existing hierarchies. Theater high: grant writing, institutional affiliation, peer review rituals are largely performative rather than functionally selecting for creative output quality. Primary function (allocating resources to support art) has atrophied; constraint persists through institutional inertia and identity maintenance.
constraint_indexing:constraint_classification(necessary_day_job, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational viewpoint, the day job constraint serves genuine coordination function (labor market stabilization, risk pooling through employment benefits) while simultaneously extracting creative surplus (monetizing creative labor below market value, using subsistence dependency as labor discipline). The constraint is neither pure extraction (snare) nor pure coordination (rope) — it is a hybrid where coordination benefit to the employer is asymmetrically distributed. Creative workers bear the cost of the constraint (time, energy, opportunity), while employers capture the coordination benefit (stable workforce, access to educated labor).
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(necessary_day_job_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(necessary_day_job, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(necessary_day_job, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(necessary_day_job, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(necessary_day_job, TR),
    TR >= 0.70.

:- end_tests(necessary_day_job_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The day job systematically extracts cognitive and temporal resources from creative work, reducing creative output quality and quantity. But extraction is not total — some creative work persists even under day job constraints (evenings, weekends, residual energy), and the constraint operates differently across disciplines. The measurement shows trajectory from 0.35 (1990s, when internet had not yet reduced publishing friction) to 0.52 (2020s, when platform concentration has raised audience thresholds despite digital ease). Suppression (0.68): High. Significant barriers prevent escape: subsistence income requirements (cannot survive on creative income alone), risk aversion (day job provides healthcare, stability, retirement), skill lock-in (day job employment requires maintaining labor market skills), and geographic immobility (job location often constrains where creative work is possible). But suppression is not absolute — some creative practitioners do escape through sustained effort, patronage access, or discipline-specific advantage (software engineering). Theater ratio (0.58): Moderate-high. Arts funding and patronage institutions maintain considerable performativity: grant selection appears meritocratic but perpetuates existing status hierarchies; institutional backing increases success rates disproportionately. Day job itself is less theatrical — employment is functionally real labor. But the constraint narrative ('starving artist' is necessary and romantic) contains theatrical elements obscuring the structural extraction.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap lies between the creative practitioner's experience (snare: trapped, extraction, no exit) and the employer's experience (rope: coordination, labor market smoothing, mutual benefit). The struggling artist sees the day job as pure constraint — it steals creative time and energy with no benefit beyond subsistence. The employer sees coordination: the employment relationship solves both parties' problems (the employer needs reliable labor; the employee needs income). The analytical observer resolves this gap by recognizing that the coordination benefit exists but is asymmetrically distributed. The employer captures the coordination gain (access to educated labor at below-market rates); the employee bears the coordination cost (time/energy allocation). The mid-career professional occupies an intermediate position — the day job provides some coordination benefit (stability that enables creative work) but also extraction (resource constraint). The scaffold perspective reveals an emerging exit through alternative mechanisms (grants, digital platforms, UBI), suggesting the constraint is not immutable. The piton perspective notes that existing patronage systems perpetuate the constraint through performative gatekeeping rather than functional support.
 *
 * DIRECTIONALITY LOGIC:
 *   The struggling artist (powerless/trapped) experiences high directionality value (d ≈ 0.92) — full target of extraction. Benefits zero (no subsistence provided by the constraint); costs maximum (time, energy, opportunity). Mid-career professional (moderate/constrained) experiences moderate directionality (d ≈ 0.65) — partial victim, partial beneficiary (day job provides stability that enables some creative work, but constrains ambition). Employer (institutional/arbitrage) experiences low directionality (d ≈ 0.12) — net beneficiary (accesses labor at suppressed rates; subsistence dependency creates compliant workforce). Analytical observer (analytical/analytical) experiences middle directionality (d ≈ 0.72) — sees the structural asymmetry but understands the coordination function that justifies some of the employer's position. The perspectival gap arises from this directionality spread: the struggling artist experiences maximal extraction; the employer experiences pure coordination benefit; the analytical observer sees it as hybrid because the coordination benefit is asymmetrically distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves mandatrophy by distinguishing genuine coordination function from extractive asymmetry. The day job IS a coordination mechanism — it solves the labor supply/demand problem through wage employment. But the coordination benefit is skewed toward employers through the subsistence dependency mechanism. Creative workers cannot reject the employment offer because they lack alternative subsistence sources. This creates asymmetric bargaining power that enables wage suppression below true labor market value. The mandatrophy resolution strategy: (1) Recognize the coordination function (employment is legitimate labor market mechanism). (2) Identify the extraction mechanism (subsistence dependency creates inelastic labor supply enabling wage suppression). (3) Declare perspectives that reflect both dimensions: the employer experiences rope (pure coordination); the struggling artist experiences snare (pure extraction); the analytical observer and mid-career professional experience tangled_rope (hybrid). (4) Note that extraction severity varies by discipline — software engineers escape the constraint at higher rates because their labor commands market premium that overcomes subsistence dependency. Literary fiction writers remain trapped because creative income thresholds are structurally high. (5) Track the constraint's potential sunset through scaffold mechanisms (UBI, digital platforms, arts funding growth). If the constraint evolves toward pure coordination (creative income rises, day job becomes optional rather than necessary), it may transition to rope classification. Current tangled_rope classification reflects the actual hybrid state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creative_income_sufficiency_threshold,
    'At what level of creative income does the day job become optional rather than necessary?',
    'Longitudinal analysis of artist income distribution; identification of minimum sustainable creative income across different disciplines and geographies; historical comparison with pre-internet era when creative income thresholds were higher',
    'If threshold is universally low (< 30k annually): day job is unnecessary for most creatives with modest market traction. If threshold is high (> 70k annually): only successful artists escape the constraint. Current evidence suggests discipline-dependent threshold (literary fiction 80k+, visual art 40k, music 35k, software 60k).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creative_income_sufficiency_threshold, empirical, 'Minimum creative income level at which day job becomes optional').

omega_variable(
    cognitive_bandwidth_substitution,
    'Is the day job''s extraction of cognitive bandwidth from creative work recoverable through alternative scheduling (remote work, part-time, episodic employment)?',
    'Comparative analysis of creative output quality/quantity for full-time employed artists vs. gig-employed artists vs. part-time employed artists; longitudinal tracking of creative productivity gains from shift to remote/flexible employment; qualitative interviews on attention residue and creative recovery time requirements',
    'If recoverable through scheduling changes: the snare classification drops to tangled_rope for many practitioners. Day job persists but suppression decreases. If not recoverable: day job extraction is fundamental, and only income sufficiency provides exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_bandwidth_substitution, empirical, 'Whether scheduling flexibility reduces day job extraction of creative bandwidth').

omega_variable(
    universal_basic_income_sufficiency,
    'Does unconditional cash transfer at subsistence level (UBI or equivalent) actually enable creative work at quality levels competitive with full-time artists, or does it merely reduce day job hours without enabling genuine creative vocations?',
    'Pilot program outcomes (Finland UBI experiment, Stockton SEED program); longitudinal tracking of creative output pre/post UBI in pilot cohorts; quality and audience metrics for creative work produced under UBI vs. traditional employment',
    'If sufficient: scaffold perspective is confirmed — UBI is a real sunset for the day job constraint. If insufficient: UBI reduces suppression but does not eliminate the constraint; day job replacement by UBI creates same structural problem with different labor mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_basic_income_sufficiency, empirical, 'Whether UBI sufficiently enables creative work without day job').

omega_variable(
    discipline_specific_extraction_variance,
    'Why do certain creative disciplines (software engineering, design) have lower day job necessity while others (literary fiction, visual art, experimental music) have high necessity?',
    'Comparative analysis of income distribution, audience size, market friction, and capital requirements across creative disciplines; identification of structural factors that enable direct monetization (software) vs. those that require patronage (visual art)',
    'If variance is market-structural (audience size, capital requirements, platform economics): the constraint operates differently across disciplines and requires discipline-specific perspectives. If variance is social/cultural (prestige systems, credentialing, gatekeeping): the constraint is partially artificial and reducible through institutional change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discipline_specific_extraction_variance, empirical, 'Variation in day job necessity across creative disciplines').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(necessary_day_job, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dayj_tr_t0, necessary_day_job, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dayj_tr_t15, necessary_day_job, theater_ratio, 15, 0.52).
narrative_ontology:measurement(dayj_tr_t30, necessary_day_job, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dayj_be_t0, necessary_day_job, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dayj_be_t15, necessary_day_job, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dayj_be_t30, necessary_day_job, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(necessary_day_job, resource_allocation).
narrative_ontology:affects_constraint(necessary_day_job, artistic_risk_aversion).
narrative_ontology:affects_constraint(necessary_day_job, creative_field_concentration).
narrative_ontology:affects_constraint(necessary_day_job, winner_take_most_distribution).

% DUAL FORMULATION NOTE:
% The necessary day job decomposes into discipline-specific constraints with different ε values. Software engineering creative work (e.g., open-source projects) has lower day job necessity (ε ≈ 0.25) due to high creative income thresholds and remote work normalization. Literary fiction has higher necessity (ε ≈ 0.68) due to low creative income thresholds despite large audience pools. These should be tracked as separate constraint stories linked through this family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
