% ============================================================================
% CONSTRAINT STORY: crew_skill_training_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crew_skill_training_bottleneck, []).

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
 *   constraint_id: crew_skill_training_bottleneck
 *   human_readable: Crew Skill Training Bottleneck
 *   domain: labor_economics/organizational_systems
 *
 * SUMMARY:
 *   The crew skill training bottleneck creates a structural extraction
 *   mechanism where access to professional competency is controlled through
 *   credential gatekeeping. This constraint exhibits the full range of DR
 *   types across different structural positions: incumbent employers and
 *   credentialing institutions benefit from restricted labor supply and
 *   gatekeeping power; entry-level workers are trapped by access barriers;
 *   moderate workers experience mixed coordination and extraction; organized
 *   reform coalitions perceive a temporary problem with viable alternatives;
 *   traditional apprenticeship persists as theater; and the civilizational
 *   analyst risks naturalizing a contingent institutional arrangement as an
 *   immutable feature of labor markets. Extractiveness has increased from
 *   0.35 to 0.52 over the interval as credentialing scope has expanded and
 *   credential requirements have bundled, raising barriers to entry. Theater
 *   ratio has increased from 0.48 to 0.64 as formal training programs have
 *   grown more focused on compliance documentation and less on actual skill
 *   transfer, with on-the-job learning carrying ever more of the functional
 *   weight.
 *
 * KEY AGENTS:
 *   - Entry-Level Workers: Primary victims (powerless/trapped) — excluded by cost, time requirements, and gatekeeping; cannot acquire entry-level skills through formal channels
 *   - Incumbent Workers: Secondary victims and beneficiaries (moderate/constrained) — benefit from wage protection but constrained by retraining barriers if displaced
 *   - Employers: Primary beneficiaries (institutional/arbitrage) — benefit from restricted labor supply and liability protection; can arbitrage through internal training or lobbying
 *   - Credentialing Institutions: Primary beneficiaries (institutional/constrained) — extract through gatekeeping and credential scope expansion; genuinely coordinate competency verification
 *   - Reform Coalition: Organized actors (organized/mobile) — labor unions, workforce developers, community colleges building alternative pathways with sunset logic
 *   - Traditional Apprenticeship System: Institutional actor (institutional/arbitrage) — maintains performative function through regulatory inertia despite atrophied skill-transfer role
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent labor market limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crew_skill_training_bottleneck, 0.52).
domain_priors:suppression_score(crew_skill_training_bottleneck, 0.68).
domain_priors:theater_ratio(crew_skill_training_bottleneck, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crew_skill_training_bottleneck, extractiveness, 0.52).
narrative_ontology:constraint_metric(crew_skill_training_bottleneck, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(crew_skill_training_bottleneck, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crew_skill_training_bottleneck, tangled_rope).
narrative_ontology:human_readable(crew_skill_training_bottleneck, "Crew Skill Training Bottleneck").
narrative_ontology:topic_domain(crew_skill_training_bottleneck, "labor_economics/organizational_systems").

domain_priors:requires_active_enforcement(crew_skill_training_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crew_skill_training_bottleneck, incumbent_employers).
narrative_ontology:constraint_beneficiary(crew_skill_training_bottleneck, credentialing_gatekeepers).
narrative_ontology:constraint_victim(crew_skill_training_bottleneck, entry_level_workers).
narrative_ontology:constraint_victim(crew_skill_training_bottleneck, displaced_workforce).
narrative_ontology:constraint_victim(crew_skill_training_bottleneck, skill_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL WORKER (SNARE) — Trapped by training barriers, licensing requirements, and industry gatekeeping. Cannot acquire skills without employer sponsorship or expensive private training. No exit path: formal routes are closed by cost and time requirements; informal routes are suppressed by legal restrictions on unlicensed practice. Bears full cost of the bottleneck through exclusion and wage suppression.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT WORKER (TANGLED ROPE) — Experiences mixed extraction and coordination. Training bottleneck limits new entrants (protecting wage premium), but also constrains their own skill advancement and retraining options if displaced. Genuine coordination function exists: formal training ensures baseline safety and competency. But asymmetric extraction protects incumbent wages at expense of new entrants. Constrained exit: can switch jobs but retraining barriers limit mobility.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Experiences the constraint as coordination: formal training programs ensure consistent quality and reduce liability. Benefits from restricted labor supply (lower wage pressure). Can arbitrage: substitute internal training, use apprenticeships, or lobby for training tax credits. Net beneficiary — gains wage suppression and workforce quality assurance simultaneously.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CREDENTIALING INSTITUTION (TANGLED ROPE) — Genuinely coordinates safety and baseline competency (coordination function). But also extracts through gatekeeping: maintains monopoly on credential production, sets barriers to entry, captures rents from licensing fees. Constrained by regulatory environment and accreditation requirements, but actively enforces training bottleneck through credential scope expansion and credential bundling.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Labor unions, workforce development agencies, and community colleges see the bottleneck as a temporary problem with a sunset: competency-based credentials, stackable micro-credentials, apprenticeships, and employer-led training are building alternative pathways. Low effective extraction because organized agents have leverage and perceive clear exit path. Genuine coordination function (skill verification) is being decoupled from gatekeeping extraction.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL APPRENTICESHIP MODEL (PITON) — Once the primary training mechanism in craft trades; now largely theatrical. Formal apprenticeships persist through regulatory requirement and institutional inertia despite being a small fraction of actual skill transfer (mostly on-the-job learning). Theater ratio high: programs spend time on compliance documentation, classroom instruction, and credential rituals while actual skill transfer happens informally. Primary function (skill transmission) has atrophied; constraint persists because institutions have invested in maintenance.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some training lag is inherent to skill acquisition: complex competencies require time and practice, and the gap between entry and mastery is unavoidable. This perspective risks naturalizing the bottleneck as an immutable feature of labor markets. However, the structural data contradicts the mountain classification — comparable countries with lower training barriers achieve faster skill acquisition, indicating the bottleneck is contingent, not natural.
constraint_indexing:constraint_classification(crew_skill_training_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crew_skill_training_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crew_skill_training_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crew_skill_training_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crew_skill_training_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(crew_skill_training_bottleneck, TR),
    TR >= 0.70.

:- end_tests(crew_skill_training_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The training bottleneck extracts through restricted labor supply (wage suppression for new entrants), gatekeeping rents (credentialing fees), and enforced credential bundling that increases cost beyond functional requirement. However, extraction is not total because genuine coordination function exists: formal training does reduce liability and ensures baseline competency. The trajectory from 0.35 to 0.52 reflects scope creep in credentialing — once-specialized credentials have been bundled with general work permits, raising barriers. Suppression (0.68): High. Entry barriers are substantial: cost of formal training (often $5k-$25k+), time requirements (6 months to 4 years), and legal restrictions on unlicensed practice prevent informal skill acquisition and on-the-job learning. Suppression is maintained through credential scope expansion, legal title protection, and employer collusion on hiring standards. Theater ratio (0.64): High and increasing. Formal training programs spend significant time on compliance documentation, classroom instruction disconnected from practice, and credential rituals, while actual skill transfer happens primarily through informal on-the-job learning. The theater has increased as training institutions have grown larger and more focused on credential output metrics rather than competency development.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap separates beneficiaries from victims along clear structural lines. Employers and credentialing institutions experience the bottleneck as coordination with secondary benefits. Entry-level workers experience it as pure extraction. The reform coalition perceives a solvable problem. Traditional apprenticeship experiences itself as necessary ritual (piton). The crucial gap is between the beneficiary view (rope: rational coordination) and the victim view (snare: impossible barrier). This gap reveals that the bottleneck functions as a wealth transfer from entry-level workers to incumbents and gatekeepers, dressed in the language of safety and competency. The theater ratio increase (0.48 → 0.64) indicates that as the barrier persists, less actual skill transfer happens through formal routes and more happens informally, reducing the functional justification for the formal constraint while the extraction mechanism persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position. Entry-level workers are pure victims with no exit path (d ≈ 0.95 → high f(d) → high experienced extraction). Incumbent workers are mixed (d ≈ 0.55 → moderate f(d)): they benefit from wage protection but bear costs of retraining barriers. Employers are beneficiaries with exit options (d ≈ 0.10 → low f(d) → negative effective extraction, meaning they gain). Credentialing institutions are beneficiaries with constrained exit (d ≈ 0.20 → slightly negative f(d)): they genuinely extract through gatekeeping but are also constrained by regulatory requirements and accreditation standards. The reform coalition has high agency and perceives exit paths (d ≈ 0.40 → moderate f(d)): organized actors can leverage alternative approaches. The traditional apprenticeship system has arbitrage-level exit (d ≈ 0.05 → negative f(d)): maintains extraction power through inertia despite low functional necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint exhibits genuine coordination function (safety verification, baseline competency) alongside asymmetric extraction (gatekeeping, wage suppression, credential bundling). The tangled rope classification is correct because both functions are structural and active, not because one is illusory. However, the trajectory matters: as theater increases (compliance documentation replacing skill transfer), the coordination function weakens while extraction persists, creating a mandatrophy risk — the constraint could degrade toward snare if the bottleneck persists but credentials become purely ceremonial. The reform coalition's scaffold perspective identifies this: the sunset mechanism exists (alternative credentials) but requires active transition, not passive naturalizing of the current system as inevitable. The analytical observer's mountain perspective naturalizes what should be recognized as institutional choice — comparable jurisdictions achieve better outcomes with lower barriers, falsifying the 'inherent to labor markets' framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credentialing_function_genuine,
    'Is credential-based training genuinely necessary for safety/competency, or is much of the credential requirement extractive gatekeeping?',
    'Cross-country analysis of credential requirements vs competency outcomes; incident rates in jurisdictions with lower credentialing barriers vs high-barrier regions',
    'If genuine: tangled rope classification correct; suppression primarily functional. If extractive: snare classification more appropriate; suppression primarily non-functional gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credentialing_function_genuine, empirical, 'Whether credential requirements serve genuine safety/competency function or primarily gatekeep labor supply').

omega_variable(
    employer_incentive_alignment,
    'Do employers benefit more from training bottleneck (wage suppression, restricted labor supply) or from accessible training (larger labor pool, lower turnover)?',
    'Employer hiring speed data in high-barrier vs low-barrier regions; correlation between credential restrictions and productivity/profitability',
    'If restricted supply benefit outweighs: rope classification dominates. If accessible training benefit outweighs: constraint reclassifies as scaffold/temporary, not stable rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_incentive_alignment, empirical, 'Employer net benefit from training bottleneck vs accessible training').

omega_variable(
    alternative_credentialing_viability,
    'Can competency-based credentials, micro-credentials, or employer-led training deliver equivalent skill verification without current bottleneck?',
    'Pilot data on alternative credential acceptance; employer hiring patterns for alternative-credential holders; incident/competency outcomes',
    'If viable: scaffold perspective validated, sunset is real, constraint classifies as temporary. If not viable: traditional credentialing is irreplaceable, snare/tangled rope classification confirmed as stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Viability of alternative credentialing models to replace traditional bottleneck').

omega_variable(
    suppression_mechanism_structural_vs_intentional,
    'Is suppression (0.68) primarily structural (training genuinely requires time/cost) or primarily intentional (institutions actively restrict access to maintain scarcity)?',
    'Institutional analysis of credential scope evolution; interviews with credentialing bodies on access policy; comparison of barrier heights across comparable jurisdictions',
    'If structural: tangled rope correct; suppression reflects genuine complexity. If intentional: snare classification more appropriate; high suppression indicates active gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_intentional, conceptual, 'Whether suppression is structural necessity or active gatekeeping mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crew_skill_training_bottleneck, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crew_train_tr_t0, crew_skill_training_bottleneck, theater_ratio, 0, 0.48).
narrative_ontology:measurement(crew_train_tr_t10, crew_skill_training_bottleneck, theater_ratio, 10, 0.58).
narrative_ontology:measurement(crew_train_tr_t20, crew_skill_training_bottleneck, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(crew_train_be_t0, crew_skill_training_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crew_train_be_t10, crew_skill_training_bottleneck, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(crew_train_be_t20, crew_skill_training_bottleneck, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crew_skill_training_bottleneck, enforcement_mechanism).
narrative_ontology:affects_constraint(crew_skill_training_bottleneck, labor_supply_restriction_mechanisms).
narrative_ontology:affects_constraint(crew_skill_training_bottleneck, intergenerational_mobility_barriers).

% DUAL FORMULATION NOTE:
% The crew skill training bottleneck is structurally linked to broader labor market constraints. It is downstream of institutional decisions about credentialing scope and credential bundling, but represents a distinct extraction mechanism through access restriction. Related constraints share the same root cause (institutional gatekeeping) but have different target populations (skilled workers vs entry-level vs displaced workers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crew_skill_training_bottleneck, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
