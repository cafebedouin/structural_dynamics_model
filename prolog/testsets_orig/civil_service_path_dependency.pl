% ============================================================================
% CONSTRAINT STORY: civil_service_path_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_service_path_dependency, []).

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
 *   constraint_id: civil_service_path_dependency
 *   human_readable: Civil Service Path Dependency and Institutional Capture
 *   domain: political_economy/institutional_governance
 *
 * SUMMARY:
 *   Civil service path dependency represents a structural constraint on
 *   institutional adaptability maintained through formal rules (seniority
 *   systems, pension structures, hierarchical advancement) and informal
 *   institutional norms (identity fusion with organizational continuity,
 *   normative resistance to external disruption). The constraint originates
 *   in a genuine coordination function: the Northcote-Trevelyon model was
 *   designed to prevent political patronage and create professional impartial
 *   public service. Over decades, the coordination function has partially
 *   atrophied while the extraction mechanisms have intensified. Reform-minded
 *   civil servants face suppressed alternatives: lateral entry is rare,
 *   internal dissent damages careers, and exit carries severe pension costs.
 *   Incumbent leadership benefits from stable institutional reproduction that
 *   locks out external competition. Political executives inherit a system
 *   they cannot rapidly reform without appearing to invite patronage and
 *   destabilizing implementation capacity. The constraint exhibits all
 *   tangled rope characteristics: genuine coordination function
 *   (institutional continuity), asymmetric extraction (benefits to
 *   incumbents, costs to reformers), and active enforcement through rules and
 *   norms.
 *
 * KEY AGENTS:
 *   - Reform-minded Civil Servants: Primary victims (powerless/trapped) — locked by seniority systems, pension constraints, and hierarchical advancement rules; exit costs are severe
 *   - Incumbent Civil Service Leadership: Primary beneficiaries (institutional/arbitrage) — benefits from stability and career certainty; can exit at any time to private sector
 *   - Reform Coalition (Unions, Progressive Cadres): Secondary organized actors (organized/constrained) — perceive coordination function but also face extraction; have some organizational capacity but face structural constraints
 *   - Political Executive (Incoming Government): Institutional actor (institutional/constrained) — must enforce the system while being constrained by it; cannot rapidly replace personnel without appearing to invite patronage
 *   - Historical Civil Service Ideal: Institutional narrative (institutional/arbitrage) — formal rationale for the constraint; increasingly performative as coordination function atrophies
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the full tangled structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_service_path_dependency, 0.52).
domain_priors:suppression_score(civil_service_path_dependency, 0.68).
domain_priors:theater_ratio(civil_service_path_dependency, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_service_path_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(civil_service_path_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(civil_service_path_dependency, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_service_path_dependency, tangled_rope).
narrative_ontology:human_readable(civil_service_path_dependency, "Civil Service Path Dependency and Institutional Capture").
narrative_ontology:topic_domain(civil_service_path_dependency, "political_economy/institutional_governance").

domain_priors:requires_active_enforcement(civil_service_path_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_service_path_dependency, incumbent_civil_service_executives).
narrative_ontology:constraint_beneficiary(civil_service_path_dependency, institutional_continuity_guardians).
narrative_ontology:constraint_victim(civil_service_path_dependency, reform_minded_personnel).
narrative_ontology:constraint_victim(civil_service_path_dependency, policy_innovation_capacity).
narrative_ontology:constraint_victim(civil_service_path_dependency, public_service_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM-MINDED CIVIL SERVANT (SNARE) — Trapped within hierarchical seniority systems where career advancement requires conformity to institutional norms. Exit through resignation means loss of pension, tenure protection, and professional identity. The constraint suppresses structural alternatives: lateral entry is restricted, early exit carries severe costs, and internal dissent damages career trajectories. Maximum experienced extraction with no viable exit.
constraint_indexing:constraint_classification(civil_service_path_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITION (TANGLED ROPE) — Organized agents perceive genuine coordination function: civil service stability and predictable personnel management are real public goods. But the constraint also extracts through seniority-based promotion that locks out merit-based advancement, pension rules that bind reformers to the status quo, and internal politics that suppress innovation. Organized power and generational time horizon reduce experienced extraction — but the constraint remains hybrid with asymmetric payoffs favoring institutional conservatism.
constraint_indexing:constraint_classification(civil_service_path_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT CIVIL SERVICE LEADERSHIP (ROPE) — Experiences the constraint as pure coordination: civil service path dependency ensures institutional memory, continuity of operations, and predictable succession. Senior executives benefit from the stability that locks out external competition and defines career ladders favoring those already embedded in the system. Exit options are arbitrage — they can move to private sector, NGOs, or international bodies at any time, but institutional rewards make staying valuable. Low or negative experienced extraction.
constraint_indexing:constraint_classification(civil_service_path_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL EXECUTIVE / INCOMING GOVERNMENT (TANGLED ROPE) — Elected leaders see coordination function (stable implementation of policy) but also face extractive constraints: unable to rapidly replace personnel aligned with previous administrations, bound by civil service regulations designed to prevent political capture, constrained by pension liabilities and seniority protections. Must enforce the system while being constrained by it. Active enforcement is required to maintain the path dependency; removal is costly and politically contentious.
constraint_indexing:constraint_classification(civil_service_path_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CIVIL SERVICE IDEAL (PITON) — The formal rationale for path dependency is the Northcote-Trevelyon ideal: professional, impartial public service protected from political patronage. This historical coordination function has substantially atrophied — the protection now primarily serves institutional self-preservation rather than preventing politicization. The civil service maintains the theatrical apparatus of impartiality and merit while actual advancement increasingly reflects faction, seniority, and institutional allegiance. Theater ratio (0.64) reflects this performative maintenance of merit criteria that no longer drive allocation.
constraint_indexing:constraint_classification(civil_service_path_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint exhibits all features of a genuine tangled rope: (1) Coordination function: institutional continuity, implementation capacity, and insulation from political whims are real public goods. (2) Asymmetric extraction: benefits concentrate on incumbent executives and institutional conservators while costs fall on reform-minded personnel and policy innovation capacity. (3) Active enforcement: political executives, pension rules, and seniority systems actively maintain the constraint. The constraint is not a natural law and not pure extraction — it is a hybrid coordination mechanism with embedded asymmetry that serves institutional continuity at the cost of adaptability.
constraint_indexing:constraint_classification(civil_service_path_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_service_path_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_service_path_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_service_path_dependency, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civil_service_path_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civil_service_path_dependency, TR),
    TR >= 0.70.

:- end_tests(civil_service_path_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant resources and career opportunities from reform-minded personnel who cannot exit without losing pension protection and professional standing. The extraction is not maximal (snare-level) because some institutional coordination genuinely occurs and some reformers do advance through conformity or patience. The measurement trajectory (0.38 → 0.52 over 30 years) shows accumulation as seniority systems have deepened and external entry has become more restricted. Suppression (0.68): High. Multiple formal barriers suppress alternatives: closed advancement, pension rules that make exit costly, civil service regulations that prevent lateral entry, and promotion systems that reward conformity over innovation. Career risk for internal dissent is substantial. But suppression is not absolute — some lateral entry occurs, some reformers persist, and union organization provides limited collective voice. Theater ratio (0.64): Moderate-high and increasing. The formal merit criteria and impartiality tests are increasingly disconnected from actual advancement, which increasingly reflects seniority, factional alignment, and institutional preservation. The constraint maintains theatrical apparatus (merit exams, impartiality standards) while the actual allocation mechanism is seniority-based. The theater has increased from 0.48 to 0.64 as the gap between stated and actual criteria has widened.
 *
 * PERSPECTIVAL GAP:
 *   The most critical gap is between incumbent leadership's Rope experience and reform-minded personnel's Snare experience. The constraint solves the same coordination problem (implementing policy continuity) for both, but distributes costs and benefits asymmetrically. Incumbent leadership perceives the constraint as beneficial (career certainty, protected status, predictable succession). Reform-minded personnel perceive it as constraining (blocked advancement, suppressed innovation, trapped by pension costs). The political executive perceives mixed experience (Tangled Rope) — inheriting a system they cannot rapidly change without appearing to invite patronage. The analytical observer, from a civilizational perspective, sees the constraint as a hybrid: genuine coordination function married to asymmetric extraction. The gap reveals that the constraint is not a natural law (not Mountain) but a contingent institutional arrangement that could be restructured.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and power/exit combinations. Reform-minded civil servants (powerless/trapped) experience maximum directionality (d ≈ 0.95) — they are structurally mobile (seniority is not physical confinement) but materially trapped (exit costs are severe). Incumbent leadership (institutional/arbitrage) experiences low directionality (d ≈ 0.10) — they are beneficiaries with maximal exit options. Political executives (institutional/constrained) experience moderate directionality (d ≈ 0.55) — they inherit constraint enforcement requirements and cannot rapidly exit. The reform coalition (organized/constrained) experiences moderate directionality (d ≈ 0.45) — they have organizational capacity but face structural barriers to fundamental reform. The identity-locked component (if operative) would appear in experienced suppression that persists after material barriers are removed — reformers who internalize institutional logic and perceive reform as impossible even when material costs are lower than they believe.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: (1) Coordination function is real — institutional continuity and implementation capacity are public goods that path dependency partially provides. (2) Extraction is asymmetric — benefits concentrate on incumbents and institutional conservators while costs fall on reformers and policy innovation capacity. (3) Active enforcement is required — pension rules, seniority systems, and hierarchy explicitly maintain the constraint. This is not coordination falsely labeled as extraction, nor extraction falsely labeled as coordination. It is a hybrid where both functions are operational but costs and benefits are asymmetric. The constraint cannot be classified as pure Rope (which would ignore the asymmetric extraction) or pure Snare (which would ignore the genuine coordination function). The tangled classification is the accurate structural diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_inertia_vs_structural_extraction,
    'Is path dependency primarily institutional inertia (requiring continuous enforcement) or structural extraction (self-sustaining)?',
    'Counterfactual analysis: removal of enforcement mechanisms (seniority rules, pension constraints, hierarchical advancement) and observation of whether system reverts to merit-based allocation or sustains hierarchical reproduction through informal channels.',
    'If inertia: the constraint is more Scaffold than Tangled Rope — sunset mechanisms targeting specific enforcement points could substantially reduce extraction. If structural: extraction mechanisms run deeper than formal rules, and formal reform produces only surface-level change (Piton degradation rather than Rope recovery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_structural_extraction, empirical, 'Whether path dependency is maintained by formal enforcement or structural self-interest').

omega_variable(
    lateral_entry_feasibility,
    'Could lateral entry from private sector and NGOs substantially replace the closed-advancement model without degrading institutional continuity?',
    'Comparative institutional analysis: examination of public services that have opened lateral entry (UK Fast Stream, Singapore Administrative Service lateral hiring) and measurement of institutional continuity, implementation capacity, and staff retention across reform phases.',
    'If feasible: path dependency is a conservative choice rather than a functional necessity, and the constraint is primarily extractive (higher Snare component). If infeasible: institutional continuity genuinely requires embedded knowledge and continuous relationships, supporting the coordination function claim (higher Rope component).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lateral_entry_feasibility, empirical, 'Whether lateral entry models can sustain institutional function').

omega_variable(
    reform_suppression_mechanism,
    'Are reform attempts suppressed primarily by identity-locked institutional resistance or by material costs (pension loss, career termination)?',
    'Analysis of reform proposals: which failed through active suppression vs which failed because reformers lacked material resources or faced unacceptable personal costs. Documentation of internal reform coalitions and their stated constraints.',
    'If identity-locked: reformers perceive the constraint as unchangeable even when material barriers are lower than they believe. The constraint operates through internalized frames. If material: costs are genuine; reform requires credible off-ramps (early retirement packages, transition support). The identity lock may reinforce but does not create the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_suppression_mechanism, empirical, 'Whether suppression is primarily identity-locked or material cost-based').

omega_variable(
    pension_liability_counterfactual,
    'How much of the path dependency is maintained by pension rules vs by other institutional factors (hierarchy, seniority, internal politics)?',
    'Historical decomposition: analysis of civil service expansion and entrenchment periods relative to pension rule establishment; simulation of personnel flows under different pension structures; interviews with reform-minded personnel on actual vs perceived constraints.',
    'If pension rules are dominant (>50% of lock-in): focused reform of pension structures could substantially reduce extraction. If secondary (<30%): pension reform would be necessary but insufficient; deeper institutional restructuring required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pension_liability_counterfactual, empirical, 'Pension rules'' proportion of institutional lock-in mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_service_path_dependency, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t0, civil_service_path_dependency, theater_ratio, 0, 0.48).
narrative_ontology:measurement(civi_tr_t15, civil_service_path_dependency, theater_ratio, 15, 0.58).
narrative_ontology:measurement(civi_tr_t30, civil_service_path_dependency, theater_ratio, 30, 0.64).
narrative_ontology:measurement(civi_tr_t45, civil_service_path_dependency, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(civi_be_t0, civil_service_path_dependency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(civi_be_t15, civil_service_path_dependency, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(civi_be_t30, civil_service_path_dependency, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(civi_be_t45, civil_service_path_dependency, base_extractiveness, 45, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_service_path_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(civil_service_path_dependency, civil_service_political_capture).
narrative_ontology:affects_constraint(civil_service_path_dependency, pension_liability_intergenerational).
narrative_ontology:affects_constraint(civil_service_path_dependency, bureaucratic_innovation_suppression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civil_service_path_dependency, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
