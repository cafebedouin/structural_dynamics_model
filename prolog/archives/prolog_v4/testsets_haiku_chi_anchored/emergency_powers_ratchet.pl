% ============================================================================
% CONSTRAINT STORY: emergency_powers_ratchet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_powers_ratchet, []).

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
 *   constraint_id: emergency_powers_ratchet
 *   human_readable: The Permanent Crisis Scaffold
 *   domain: political/legal/social
 *
 * SUMMARY:
 *   The emergency powers ratchet is a structural tendency in democracies and
 *   autocracies alike for temporary legal authorities, granted during acute
 *   crisis (war, pandemic, financial collapse), to persist and become
 *   normalized as permanent administrative fixtures. The constraint exhibits
 *   multiple perspectives from the same base properties: the executive
 *   experiences it as genuine coordination during crisis; the citizens
 *   experience it as extraction via suppressed civil liberties;
 *   constitutional reformers see it as a scaffold with solvable design
 *   (sunset clauses); the historical constitutional framework views itself as
 *   degraded ritual (piton); and the powerless legislative branch experiences
 *   it as structural disability. Theater ratio rises from 0.20 (acute crisis,
 *   genuine emergency framing) to 0.65 (normalized administrative tool,
 *   crisis language vestigial). Extractiveness rises from 0.35 (legitimate
 *   temporary coordination) to 0.58 (permanent rent-seeking apparatus). The
 *   constraint is Tangled Rope, not pure Snare, because it genuinely solves
 *   coordination problems during acute crisis while asymmetrically extracting
 *   power from legislative and civil liberties constituencies after the
 *   crisis resolves. The mandate is resolved: the constraint combines real
 *   coordination function (emergency response) with asymmetric extraction
 *   (permanent power consolidation), meeting the Tangled Rope gates of
 *   requiring active enforcement, multiple beneficiaries, and multiple
 *   victims.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — captures and retains concentrated power; experiences constraint as coordination mechanism during crisis, extraction mechanism after normalization
 *   - Security Apparatus: Primary beneficiary (institutional/constrained) — expanded surveillance, detention, and enforcement authorities; interests aligned with executive branch in power persistence
 *   - Administrative Bureaucracy: Primary beneficiary (institutional/arbitrage) — emergency authorities bypass normal budgetary, oversight, and procedural constraints; gains operational flexibility
 *   - Civil Liberties Constituency: Primary victim (powerless/trapped) — subject to surveillance, detention, travel restrictions, and censorship with no effective exit or remedy mechanism
 *   - Legislative Oversight Capacity: Primary victim (moderate/trapped) — structurally disabled by delegation of powers to executive; eroded by precedent and normalization; cannot reclaim authority without destabilizing acknowledged-competent arrangements
 *   - Rule of Law Institutional Framework: Primary victim (institutional/trapped) — separation of powers, constitutional constraints on executive authority, due process protections all degraded by emergency powers normalization
 *   - Constitutional Reform Movements: Secondary actor (organized/constrained) — advocates for sunset clauses, supermajority renewal thresholds, emergency impact assessments; sees constraint as solvable through institutional design
 *   - Historical Constitutional Doctrine: Institutional observer (institutional/arbitrage) — doctrinal justifications for emergency powers persist through inertia despite changed crisis circumstances; maintains performative role (piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_powers_ratchet, 0.58).
domain_priors:suppression_score(emergency_powers_ratchet, 0.68).
domain_priors:theater_ratio(emergency_powers_ratchet, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_powers_ratchet, extractiveness, 0.58).
narrative_ontology:constraint_metric(emergency_powers_ratchet, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(emergency_powers_ratchet, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_powers_ratchet, tangled_rope).
narrative_ontology:human_readable(emergency_powers_ratchet, "The Permanent Crisis Scaffold").
narrative_ontology:topic_domain(emergency_powers_ratchet, "political/legal/social").

domain_priors:requires_active_enforcement(emergency_powers_ratchet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, executive_branch).
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, security_apparatus).
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, administrative_bureaucracy).
narrative_ontology:constraint_victim(emergency_powers_ratchet, civil_liberties_constituency).
narrative_ontology:constraint_victim(emergency_powers_ratchet, legislative_oversight_capacity).
narrative_ontology:constraint_victim(emergency_powers_ratchet, rule_of_law_institutional_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONALLY-PROTECTED CITIZEN (SNARE) — Subject to indefinite emergency powers with no mechanism to revoke them; trapped within jurisdiction; cannot arbitrage to alternative legal systems. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Maximum extraction: civil liberties suppressed without exit.
constraint_indexing:constraint_classification(emergency_powers_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE BRANCH OVERSIGHT CAPACITY (SNARE) — Structurally disabled by emergency powers transferred to executive; normalization of delegation erodes legislature's ability to reclaim authority; trapped by precedent and institutional inertia. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.78. Extraction of institutional capacity from legislative to executive.
constraint_indexing:constraint_classification(emergency_powers_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL LIBERTIES ORGANIZATIONS (TANGLED ROPE) — Constrained by resource limits and legal standing requirements; also benefit from emergency powers framework as it generates litigation, fundraising, and public attention to constitutional issues. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.63. Mixed: extraction of liberties + coordination benefit of visibility.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE BRANCH AS FUNCTIONAL COORDINATOR (ROPE) — During acute crisis, emergency powers solve genuine coordination problems (rapid resource mobilization, unified command). Executive experiences the constraint as coordination mechanism. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.006. Negative effective extraction: net beneficiary through coordination function.
constraint_indexing:constraint_classification(emergency_powers_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents (constitutional reform advocates, emergency sunset clause proponents) see this as a temporary coordination failure with a clear sunset mechanism: constitutional amendment requiring automatic expiration of emergency powers, supermajority renewal thresholds, or emergency sunset clauses in legislation. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.20. Low effective extraction because organized agents see agency and path forward.
constraint_indexing:constraint_classification(emergency_powers_ratchet, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL CONSTITUTIONAL THEATER (PITON) — From civilizational distance, emergency powers frameworks persist through ritual performance of crisis response rather than functional necessity; constitutional interpretations that enabled the original powers persist through doctrinal inertia despite changed circumstances. theater_ratio=0.65. Degraded mechanism: the constitutional justifications (emergency/temporary) have atrophied while the powers remain.
constraint_indexing:constraint_classification(emergency_powers_ratchet, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal/civilizational perspective, emergency powers ratchet appears as an immutable property of state capacity: any state facing existential crisis must concentrate power in executive; once concentrated, power cannot be disaggregated without loss of institutional memory and capacity; therefore emergency powers persist as natural law of political organization. However, ε=0.58, suppression=0.68, theater=0.65 contradicts mountain classification — reveals false summit naturalizing what is contingent institutional design (constitutional frameworks can enforce sunsetting).
constraint_indexing:constraint_classification(emergency_powers_ratchet, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_powers_ratchet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_powers_ratchet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_powers_ratchet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_powers_ratchet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_powers_ratchet, TR),
    TR >= 0.70.

:- end_tests(emergency_powers_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts power from legislatures and civil liberties from citizens, but the extraction is not total because (a) it provides genuine coordination benefit during acute crisis, and (b) partial oversight mechanisms exist (courts, institutional resistance, public protest) that create friction. The value represents the asymmetric extraction after normalization, not during peak crisis. Suppression (0.68): High. Civil liberties are suppressed through justified emergency measures, with limited appeal mechanisms. Alternative governance structures are suppressed through the claim that emergency powers are necessary. Exit options are severely limited — citizens cannot arbitrage to alternative legal systems or opt out of emergency jurisdiction. Theater ratio (0.65): Moderate-high. The framework maintains ritual invocation of 'emergency' and 'temporary' language, but these are increasingly performative — the administrative structures persist and are treated as permanent fixtures. Crisis language becomes theater as the genuine emergency recedes and the powers remain. The theater has increased from 0.20 (genuine crisis, authentic emergency) to 0.65 (normalized fixture, crisis language vestigial) over the measured interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the beneficiary (executive) sees coordination and necessity; the victim (citizen) sees extraction and suppression; the reformer (constitutional movement) sees a solvable design problem; the institution (legislative branch) sees its own degradation; the historian (civilizational view) sees normalized ritual. The critical gap is between immediate crisis response (genuine coordination necessity) and post-crisis normalization (pure extraction). The executive's Rope perspective is valid during acute crisis when emergency powers solve genuine problems — but the constraint persists and extracts value after the crisis resolves, shifting toward Snare. The scaffold perspective is rational (sunset mechanisms can work) but is resisted by the beneficiary due to power loss aversion. The piton perspective reveals the normalization dynamic: crisis language persists performatively while actual function atrophies — emergency powers become administrative routine justified by outdated crisis narratives. The mountain perspective is a false summit: the analytical observer risks naturalizing a contingent institutional arrangement (emergency power delegations that could be constitutionally constrained) as an immutable property of state capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive branch: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary during and after crisis. Security apparatus: Beneficiary + constrained → d≈0.12, f(d)≈-0.02. Slight beneficiary; powers constrain but primarily expand their authority. Citizens: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — no exit, no remedy. Legislative branch: Victim + trapped → d≈0.88, f(d)≈1.35. Severe extraction — disabled by precedent and normalization. Constitutional reformers: Organized + constrained → d≈0.35, f(d)≈0.35. Low effective extraction; have agency and clear reform pathway. Historical constitutional framework: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from high theater, not from directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival false summit (naturalizes contingent design).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through Tangled Rope classification: The constraint combines genuine coordination function (emergency crisis response) with asymmetric extraction (permanent power consolidation after crisis ends). The base properties satisfy all Tangled Rope gates: (1) requires_active_enforcement=true (emergency powers must be continually invoked and defended), (2) beneficiaries=[executive_branch, security_apparatus, administrative_bureaucracy] (multiple institutional beneficiaries), (3) victims=[civil_liberties_constituency, legislative_oversight_capacity, rule_of_law_framework] (multiple victims). The mandatrophy arises from the false summit risk: the constraint appears to be immutable property of state capacity (Mountain) from the civilizational/analytical perspective, which would collapse it to pure law and obscure the extractive dimensions. The Tangled Rope classification prevents this collapse by showing that the 'necessity' of emergency powers is contingent on (a) whether the crisis has actually ended, and (b) whether constitutional mechanisms enforce sunsetting. Democracies with effective sunset clauses (Germany, Austria, post-1949 constitutional frameworks) experience lower extractiveness and shorter power duration; this proves the extraction is not natural law but institutional design choice. The constraint is resolved: it is Tangled Rope, not false Mountain, because the contingent institutional arrangements can be reformed (sunset clauses, supermajority renewal, emergency impact assessments) without destroying legitimate crisis response capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crisis_necessity_threshold,
    'What objective criteria distinguish genuine emergency requiring power concentration from politically-manufactured crisis used to justify normalization?',
    'Comparative historical analysis across democracies: measure correlation between crisis severity metrics (mortality, economic disruption, external threat) and duration of emergency powers; identify cases where powers persisted despite demonstrable crisis resolution',
    'If genuine emergencies require longer powers than currently exercised: ratchet is functional (Rope/Scaffold). If crisis severity shows no correlation with power duration: ratchet is pure extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_necessity_threshold, empirical, 'Distinction between genuine emergency necessity and political crisis manufacture').

omega_variable(
    sunsetting_mechanism_feasibility,
    'Can constitutional automatic expiration, supermajority renewal, or emergency impact assessment requirements actually function to limit power ratcheting, or do these mechanisms themselves get circumvented?',
    'Case study of democracies with constitutional sunset clauses (Germany post-1949, Austria, various post-colonial states); measure actual compliance rates and mechanisms used to circumvent/extend sunset clauses',
    'If sunset mechanisms work: scaffold perspective confirmed and mandatrophy resolved through institutional design. If consistently circumvented: constraint is truly snare, not scaffold, and mandatrophy unresolvable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunsetting_mechanism_feasibility, empirical, 'Effectiveness of constitutional sunset mechanisms in limiting emergency power duration').

omega_variable(
    institutional_memory_loss_claim,
    'Does devolution of emergency powers to legislatures actually result in measurable loss of crisis response capacity, or is this narrative justification for executive retention?',
    'Comparative analysis: measure response times, resource mobilization efficiency, and coordination effectiveness in crisis scenarios under different power distributions; control for prior preparation and institutional investment',
    'If true capacity loss occurs: emergency powers ratchet has genuine functional basis (Rope component). If no measurable loss: ratchet is pure rent-seeking (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_loss_claim, empirical, 'Whether devolution of emergency powers causes measurable loss of crisis response capacity').

omega_variable(
    normalization_psychological_mechanism,
    'What psychological and institutional mechanisms cause actors to normalize emergency powers as ''business as usual,'' treating temporary measures as permanent fixtures?',
    'Analysis of legislative debate transcripts, executive orders, and legal briefs over time; measure frequency of ''emergency'' framing vs permanent administrative language; interview government officials on perceived temporality of specific powers',
    'If strong normalization mechanisms exist: theater component of constraint increases over time (piton perspective strengthens). If normalization resists, crisis language persists: constraint remains Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normalization_psychological_mechanism, conceptual, 'Psychological mechanisms underlying normalization of temporary emergency measures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_powers_ratchet, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, emergency_powers_ratchet, theater_ratio, 0, 0.2).
narrative_ontology:measurement(emerg_tr_t5, emergency_powers_ratchet, theater_ratio, 5, 0.45).
narrative_ontology:measurement(emerg_tr_t10, emergency_powers_ratchet, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, emergency_powers_ratchet, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emerg_be_t5, emergency_powers_ratchet, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(emerg_be_t10, emergency_powers_ratchet, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_powers_ratchet, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_powers_ratchet, separation_of_powers_degradation).
narrative_ontology:affects_constraint(emergency_powers_ratchet, civil_liberties_suppression_normalization).
narrative_ontology:affects_constraint(emergency_powers_ratchet, executive_accountability_erosion).

% DUAL FORMULATION NOTE:
% Emergency powers ratchet is downstream of legitimate crisis response needs and upstream of systemic separation-of-powers erosion. The constraint family includes: (1) acute crisis coordination (high ε, genuine Rope/Scaffold), (2) power normalization after crisis (moderate ε, Tangled Rope), (3) constitutional framework degradation (high ε, Snare from legislator/citizen perspectives). This story captures the Tangled Rope (moderate-high ε) after normalization has occurred but before total institutional collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emergency_powers_ratchet, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
