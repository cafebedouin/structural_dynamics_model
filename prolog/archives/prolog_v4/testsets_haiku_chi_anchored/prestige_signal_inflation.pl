% ============================================================================
% CONSTRAINT STORY: prestige_signal_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prestige_signal_inflation, []).

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
 *   constraint_id: prestige_signal_inflation
 *   human_readable: The Credential Red Queen
 *   domain: social/economic/educational
 *
 * SUMMARY:
 *   The Credential Red Queen constraint models the systematic devaluation of
 *   status markers (degrees, certifications, titles, luxury consumption
 *   signals) as they become more accessible, widespread, or mandatory for
 *   baseline labor market participation. The constraint operates through a
 *   self-reinforcing cycle: as credentials proliferate, their signaling value
 *   decreases, forcing credential-seekers to pursue additional or higher
 *   credentials to maintain relative position. Meanwhile, credential issuers
 *   (universities, professional bodies, bootcamp providers) profit from
 *   increased enrollment and certification demand. Early adopters and elites
 *   capture the scarcity premium before devaluation; late entrants and
 *   non-credentialed workers are forced into ever-escalating credential
 *   pursuit. The constraint exhibits high theater (65%) because much
 *   credential checking in hiring is performative — employers screen for
 *   degrees without verifying competence, perpetuating the requirement even
 *   as degrees lose predictive value. Alternative credential pathways
 *   (bootcamps, portfolios, apprenticeships) are emerging but have not yet
 *   achieved parity with traditional degrees. The constraint is thus a
 *   tangled_rope: it has genuine coordination function (helping match workers
 *   to complex roles), but that function is systematically undermined by
 *   extraction (credential inflation premium captured by issuers and elites).
 *   Suppression is high (68%) because late entrants face structural barriers:
 *   credential requirements are mandated by both employers and competitors,
 *   creating trapped exit options.
 *
 * KEY AGENTS:
 *   - Late-Entrant Workers: Primary victims (powerless/trapped) — must pursue escalating credentials to reach baseline competitive position; forced to finance higher education or certification at increasing cost
 *   - Credential Issuers: Primary beneficiaries (institutional/arbitrage) — universities, professional bodies, bootcamp providers; profit from increased enrollment and credential demand
 *   - Early Adopter Elite: Secondary beneficiaries (powerful/arbitrage) — captured scarcity premium before credential devaluation; maintain status through exclusivity or informal gatekeeping
 *   - Mid-Career Professionals: Secondary victims (moderate/constrained) — invested in existing credentials now devalued; constrained by organizational advancement filters that depend on credentials
 *   - Alternative Credentials Coalition: Organized challengers (organized/mobile) — bootcamps, tech companies, alternative credentialing bodies; building parallel signals; mobile exit from traditional credential system
 *   - Credentialing Bureaucracy: Institutional performer (institutional/constrained) — government agencies, regulatory bodies, corporate HR departments; maintains degree requirements through inertia and legal/contractual mandates
 *   - Non-Credentialed Workers: Victims (powerless/trapped) — excluded from credentialed roles despite potential competence; bearing cost of credential requirements without recourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prestige_signal_inflation, 0.52).
domain_priors:suppression_score(prestige_signal_inflation, 0.68).
domain_priors:theater_ratio(prestige_signal_inflation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prestige_signal_inflation, extractiveness, 0.52).
narrative_ontology:constraint_metric(prestige_signal_inflation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(prestige_signal_inflation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prestige_signal_inflation, tangled_rope).
narrative_ontology:human_readable(prestige_signal_inflation, "The Credential Red Queen").
narrative_ontology:topic_domain(prestige_signal_inflation, "social/economic/educational").

domain_priors:requires_active_enforcement(prestige_signal_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prestige_signal_inflation, credential_issuers).
narrative_ontology:constraint_beneficiary(prestige_signal_inflation, early_adopters).
narrative_ontology:constraint_victim(prestige_signal_inflation, late_entrants).
narrative_ontology:constraint_victim(prestige_signal_inflation, non_credentialed_workers).
narrative_ontology:constraint_victim(prestige_signal_inflation, credential_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-ENTRANT WORKER (SNARE) — Cannot exit credential-seeking; must pursue ever-higher qualifications to reach baseline competitive position. Each credential devaluation forces additional investment. Career timeline erodes. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(prestige_signal_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Constrained by existing credential investments and organizational advancement filters. Benefits from credential systems that legitimize their current status while suffering extraction as credentials devalue. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(prestige_signal_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIAL ISSUER (ROPE) — Universities and professional bodies benefit from credential inflation; increased enrollment and certification demand. Arbitrage exit through voluntary credential redesign or market segmentation. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(prestige_signal_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY ADOPTER ELITE (ROPE) — Captured credential scarcity premium early; can now signal via exclusivity or informal gatekeeping. Leverage first-mover advantage and social capital. d≈0.12, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(prestige_signal_inflation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING BUREAUCRACY (PITON) — Institutional persistence of degree requirements despite declining predictive validity. Theater_ratio=0.65 reflects performative credential checking (hiring screens for degrees without verification of competence). Extraction function degraded; constraint maintained by institutional inertia and regulatory mandates. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(prestige_signal_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALS COALITION (TANGLED ROPE) — Organized stakeholders (bootcamps, credentialing bodies, tech companies) building parallel credential systems. Mobile exit: can migrate to alternative signals. Benefits from traditional credential failure while extracting from those locked into devaluing cycles. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(prestige_signal_inflation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Models the constraint as a self-reinforcing cycle: credential devaluation drives demand for new/higher credentials, which become devalued in turn. Coordination function (signal reliability) exists but is undermined by extraction (credential inflation premium). d≈0.68, f(d)≈1.08, σ=1.2 → χ≈0.58.
constraint_indexing:constraint_classification(prestige_signal_inflation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prestige_signal_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prestige_signal_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prestige_signal_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prestige_signal_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prestige_signal_inflation, TR),
    TR >= 0.70.

:- end_tests(prestige_signal_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value through credential premium capture by issuers and elites, plus opportunity costs and time/money barriers for late entrants. The value increased from 0.28 to 0.52 over the interval as degree oversupply became visible and new credentials (masters, bootcamps, professional certificates) proliferated. Extraction is not total (0.52 < 0.66 snare threshold) because credentials still provide genuine labor market access and coordination value. Suppression (0.68): High. Structural barriers include employer mandates for credentials, regulatory/professional requirements, competitive pressure (if peers have degrees, you must match), and limited alternative pathways. Workers in late-entry positions have minimal options for avoiding credential pursuit. Theater_ratio (0.65): High, indicating that credential checking in hiring is substantially performative. Many employers screen for degree presence without verifying competence or field relevance; the degree functions as a filtering heuristic rather than a competence signal. Theater increased from 0.42 to 0.65 as degree proliferation made filtering increasingly performative (more false positives from degree holders, more false negatives from non-credentialed candidates). Claimed_type (tangled_rope): The constraint exhibits both genuine coordination (organizing complex labor market matching) and asymmetric extraction (credential premium capture, barrier creation). Requires_active_enforcement (true): Coordination in the credential system requires active maintenance through hiring mandates, regulatory requirements, and social reproduction of credential legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival fragmentation. The late-entrant worker sees a pure extraction trap (snare) — forced credential pursuit with no exit. The credential issuer sees pure coordination benefit (rope) — solving matching problems with no extraction experienced. The early adopter elite sees rental scarcity (rope) — benefiting from restricted access. The mid-career professional sees mixed extraction and coordination (tangled_rope) — their existing credentials legitimize their position but are simultaneously devaluing. The alternative credentials coalition sees an opportunity structure (tangled_rope) — exploiting credential system failure while building alternatives. The credentialing bureaucracy sees degraded ritual (piton) — perpetuating degree requirements through institutional inertia despite declining predictive value. The analytical observer sees a self-reinforcing arms race (tangled_rope) — coordination function (signaling reliability) undermined by extraction logic (credential inflation). No two perspectives agree on classification; the perspectival gap reveals that the same structural constraint is experienced as snare (powerless), rope (institutional beneficiary), and tangled_rope (organized challengers) simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Late-entrant workers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — forced credential pursuit with no alternative. Credential issuers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary — can redesign credentials or market segmentation. Early adopter elite: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.08. Net beneficiary — captured scarcity premium, can maintain status via exclusivity. Mid-career professionals: Mixed (both victim via devaluation and beneficiary via existing credentials) + constrained → d≈0.62, f(d)≈0.82. Moderate extraction — constrained by credential investments, benefiting from legitimacy they provide. Alternative credentials coalition: Organized (both victim of credential system capture and beneficiary of building alternatives) + mobile → d≈0.45, f(d)≈0.50. Low-moderate extraction — mobile exit available through building alternative pathways. Analytical observer: d≈0.68, f(d)≈1.08. Moderate extraction — observes self-reinforcing cycle but is not trapped in it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness = 0.52, below 0.70 threshold but above 0.46): The constraint is genuinely tangled_rope, not falsely elevated snare or rope. Evidence: (1) Coordination function is real: credentials DO solve labor market matching problems and provide reliable signals in complex industries (software, healthcare, finance). Eliminating credential systems entirely would harm matching quality. (2) Extraction is real: credential inflation premium is captured by issuers and elites; late entrants face escalating cost-of-entry; theater (0.65) indicates performative enforcement. (3) Active enforcement is structural: hiring mandates, regulatory requirements, and social reproduction of credential legitimacy all actively maintain the system. No perspective perceives this as pure coordination or pure extraction — tangled_rope is the only type that fits all observed data. The mandatrophy is resolved by recognizing that the constraint PREVENTS both pure rope (coordination without extraction) AND pure snare (extraction without coordination) — it maintains extraction precisely by providing enough coordination value to justify mandatory participation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_learning_decay,
    'Does credential inflation reflect genuine skill requirement increases or just screening proliferation?',
    'Longitudinal job task analysis; comparison of actual required competencies vs credential requirements; measurement of skill-credential gap over time',
    'If genuine skill increase: constraint is primarily coordination (organizing larger, more complex labor markets). If screening proliferation: constraint is primarily extraction (rent-seeking through artificial barrier creation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_learning_decay, empirical, 'Whether credential inflation reflects real skill requirement increases or screening proliferation').

omega_variable(
    alternative_signal_viability,
    'Can alternative credentials (portfolios, apprenticeships, bootcamps, work history) effectively replace traditional degrees as labor market signals?',
    'Longitudinal hiring outcomes analysis; comparison of employment rates and wage trajectories for workers with alternative vs traditional credentials; measurement of employer acceptance rates',
    'If viable: scaffold perspective confirmed — sunset clause is real as alternative pathways mature. If not viable: constraint persists as snare/tangled_rope without sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_signal_viability, empirical, 'Whether alternative credentials can effectively replace traditional degrees').

omega_variable(
    enforcement_mechanism_dependence,
    'Do hiring organizations truly require credentials for genuine selection purposes, or do they require credentials because other organizations require them (cascade enforcement)?',
    'Controlled hiring experiments; interviews with hiring managers and talent acquisition officers; measurement of degree-requirement justification vs actual job description requirements',
    'If genuine selection: enforcement is structural coordination. If cascade: enforcement is primarily theater (piton perspective validated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_dependence, empirical, 'Whether credential requirements reflect genuine selection needs or cascade enforcement').

omega_variable(
    extraction_margin_attribution,
    'What portion of credential inflation is extractive rent-seeking vs legitimate coordination overhead?',
    'Economic modeling of signaling equilibrium; comparison of credential costs vs employer willingness-to-pay for credentials; measurement of deadweight loss from credential arms race',
    'If primarily extraction (>70%): snare perspective dominates. If mixed (30-70%): tangled_rope confirmed. If primarily coordination (<30%): rope perspectives dominate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_margin_attribution, empirical, 'Proportion of credential inflation attributable to extraction vs coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prestige_signal_inflation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psi_tr_t0, prestige_signal_inflation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(psi_tr_t5, prestige_signal_inflation, theater_ratio, 5, 0.54).
narrative_ontology:measurement(psi_tr_t10, prestige_signal_inflation, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(psi_be_t0, prestige_signal_inflation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(psi_be_t5, prestige_signal_inflation, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(psi_be_t10, prestige_signal_inflation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prestige_signal_inflation, resource_allocation).
narrative_ontology:affects_constraint(prestige_signal_inflation, student_debt_accumulation).
narrative_ontology:affects_constraint(prestige_signal_inflation, wage_inequality_signaling).
narrative_ontology:affects_constraint(prestige_signal_inflation, educational_access_barrier).

% DUAL FORMULATION NOTE:
% The credential red queen decomposes into three related constraints: (1) Credential system as pure coordination problem (rope) — matching complex labor markets; (2) Credential inflation as extractive rent-seeking (snare) — capturing premium through barrier creation; (3) Theater-ratio degradation as institutional inertia (piton) — performative degree checking despite declining predictive value. This story treats all three as aspects of a single tangled_rope constraint. Alternative decomposition: three separate stories at different ε values would isolate coordination (ε≈0.15, rope) from extraction (ε≈0.55, snare) from theater (ε≈0.40, piton).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prestige_signal_inflation, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
