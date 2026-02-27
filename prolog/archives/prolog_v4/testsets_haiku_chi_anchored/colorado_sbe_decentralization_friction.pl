% ============================================================================
% CONSTRAINT STORY: colorado_sbe_decentralization_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colorado_sbe_decentralization_friction, []).

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
 *   constraint_id: colorado_sbe_decentralization_friction
 *   human_readable: Colorado SBE Institutional Preservation (Educational Decentralization Friction)
 *   domain: political/regulatory/education
 *
 * SUMMARY:
 *   The Colorado State Board of Education (SBE) functions as an institutional
 *   gatekeeper for educational legitimacy in the state. Teachers must hold
 *   SBE-approved credentials; schools must align curricula with SBE-adopted
 *   standards to receive state funding and legal legitimacy; alternative
 *   education models (homeschools, microschools, competency-based programs,
 *   online academies) must navigate SBE approval processes to operate
 *   legally. This constraint exhibits a tangled_rope structure: it performs
 *   genuine coordination functions (standardized teacher certification
 *   reduces hiring friction, shared curricular frameworks enable resource
 *   pooling, common standards allow student transfers), but it also extracts
 *   significant costs from entities that prefer alternative models. The
 *   extraction manifests as regulatory friction: alternatives must either
 *   conform to legacy frameworks (losing their competitive advantage) or
 *   operate in legal grey zones that suppress their growth. The theater_ratio
 *   has risen from 0.38 to 0.64 over the interval, indicating that SBE's
 *   functional gatekeeping power has atrophied while its performative
 *   legitimacy maintenance has intensified — accreditation processes,
 *   curriculum review meetings, and regulatory compliance continue, but their
 *   actual suppression of alternatives has weakened as federal policy, market
 *   forces, and parental choice have eroded the board's monopoly. The
 *   extractiveness has declined (0.68 → 0.52) as the constraint has shifted
 *   toward piton characteristics: the SBE persists through institutional
 *   inertia and theatrical maintenance, but its ability to suppress
 *   alternatives has declined.
 *
 * KEY AGENTS:
 *   - State Board of Education: Institutional beneficiary (institutional/arbitrage) — maintains regulatory authority, budget control, legitimacy through gatekeeping; can shift to lighter oversight if decentralization pressure becomes too severe
 *   - Alternative Education Providers: Powerless victims (powerless/trapped) — cannot operate without SBE approval or legal exemption; must conform to legacy frameworks or operate in grey zones
 *   - Local School Districts: Moderate victims (moderate/constrained) — benefit from SBE coordination (standardization, funding eligibility) but constrained by compliance requirements and unable to fully autonomize
 *   - Parent and Decentralization Movements: Organized agents (organized/constrained) — benefit from SBE coordination infrastructure but suppressed by enforcement of uniformity requirements; have growing market and legal alternatives
 *   - Legacy Governance Structure: Institutional observer (institutional/arbitrage) — the deeper pattern of how educational authority is distributed; SBE's role is atrophying through obsolescence
 *   - Analytical Policy Observer: Civilizational view (analytical/analytical) — sees the constraint as transitional; decentralization is building alternative legitimacy pathways that will eventually make SBE gatekeeping obsolete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.52).
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.58).
domain_priors:theater_ratio(colorado_sbe_decentralization_friction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colorado_sbe_decentralization_friction, tangled_rope).
narrative_ontology:human_readable(colorado_sbe_decentralization_friction, "Colorado SBE Institutional Preservation (Educational Decentralization Friction)").
narrative_ontology:topic_domain(colorado_sbe_decentralization_friction, "political/regulatory/education").

domain_priors:requires_active_enforcement(colorado_sbe_decentralization_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colorado_sbe_decentralization_friction, state_board_of_education).
narrative_ontology:constraint_beneficiary(colorado_sbe_decentralization_friction, credential_administrators).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, alternative_education_models).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, local_districts_with_autonomy_pressure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE EDUCATION PROVIDERS (SNARE) — Cannot operate without SBE legitimacy gates. Homeschools, microschools, online academies, and competency-based programs face mandatory approval/accreditation bottlenecks. High suppression: alternatives must mirror SBE curricular frameworks or face legal exclusion. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.69.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL SCHOOL DISTRICTS (TANGLED ROPE) — Districts benefit from SBE coordination (standardized teacher certification, shared curriculum frameworks, funding eligibility), but are constrained by SBE enforcement of compliance requirements. Cannot fully autonomize without losing state funding and legitimacy. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE BOARD OF EDUCATION (ROPE) — Institutional beneficiary. SBE maintains legitimacy, budget authority, and regulatory influence through accreditation gatekeeping. Experiences constraint as coordination: standardized frameworks enable district participation, reduce compliance friction. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary through arbitrage: can exit to lighter oversight if threatened.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PARENT AND DECENTRALIZATION MOVEMENTS (TANGLED ROPE) — Organized agents (school choice advocates, education freedom coalitions, homeschool associations) benefit from the existence of the SBE coordination infrastructure (legal framework, teacher standards enable diverse schools to operate within a common system), but are suppressed by SBE enforcement of uniformity requirements. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.35.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGACY GOVERNANCE STRUCTURE (PITON) — The SBE's authority has atrophied relative to its historical gatekeeping function. Federal Title I requirements, market-driven charter schools, online education, and parental choice have degraded the board's functional monopoly on legitimacy. Yet the institutional structure persists through inertia: legislatures maintain SBE budgets, compliance offices still file reports, accreditation processes continue. theater_ratio=0.64 reflects that much SBE activity is now performative — maintaining legitimacy rituals (public comment periods, curriculum review meetings) without constraining actual educational alternatives as severely as in earlier decades. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Institutional arbitrage preserved through theatrical maintenance.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / POLICY REFORM (SCAFFOLD) — From a generational policy perspective, the SBE constraint is transitional. Decentralization pressures (school choice, homeschool growth, charter autonomy, competency-based alternatives) are building alternative legitimacy pathways that will eventually make the SBE's gatekeeper role obsolete. The constraint functions as a temporary coordination mechanism that is losing its extraction component as alternatives mature. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.30. Sunset mechanism: as alternatives accumulate sufficient market legitimacy and legal standing (estimated 15-25 years), the SBE's enforcement capacity will decline through obsolescence, not formal sunset.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colorado_sbe_decentralization_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colorado_sbe_decentralization_friction, TR),
    TR >= 0.70.

:- end_tests(colorado_sbe_decentralization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and declining. The SBE extracts significant costs from alternative education models that must either conform to legacy standards or navigate approval processes. However, the extraction is not as severe as in earlier decades (baseline 0.68) because federal policy flexibility (Title I waivers, charter school expansion, homeschool exemptions), market-driven alternatives (online education), and parental choice movements have partially bypassed the SBE's gatekeeper authority. The declining trajectory reflects degradation toward piton. Suppression (0.58): Moderate-high. The SBE maintains multiple suppression mechanisms: mandatory teacher credential requirements that limit hiring autonomy for alternative models, curriculum alignment requirements for state funding, accreditation processes that create regulatory friction, and legal threats against unlicensed instruction. However, suppression is not total — alternatives exist and are growing, indicating that suppression mechanisms are leaky. Theater ratio (0.64, rising): The SBE's functional gatekeeping power has atrophied relative to its performative legitimacy maintenance. Public comment periods, curriculum review meetings, and accreditation processes continue at high volume, but their actual constraining effect on alternatives has declined. The rise from 0.38 to 0.64 indicates increasing performative content: the board is maintaining legitimacy rituals while losing actual gatekeeping capacity.
 *
 * PERSPECTIVAL GAP:
 *   Alternative education providers see a snare — trapped, high extraction, no exit. Local districts see tangled_rope — coordination benefits offset by compliance constraints. The SBE sees rope — they coordinate standards and teacher certification; experience their role as solving collective action problems. Parent movements see tangled_rope — benefit from the coordination infrastructure but suppressed by uniformity enforcement. The legacy governance structure sees itself as piton — its functional role has atrophied, but the institution persists through inertia and theatrical maintenance. The analytical observer sees scaffold — a transitional constraint with a sunset mechanism as decentralization alternatives mature. The perspectival range demonstrates that the SBE constraint is genuinely hybrid: it performs real coordination functions (teachers can move between districts, schools can share resources, legitimate alternatives can access state systems) while also extracting real costs (regulatory friction, conformity pressure, suppressed alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   SBE: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through institutional arbitrage; can shift regulatory stance if threatened. Alternative providers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; cannot exit without legal jeopardy. Local districts: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; have some autonomy within constraints. Parent movements: Mixed + constrained → d≈0.55, f(d)≈0.75. Both benefit from coordination infrastructure and bear costs from suppression; organized status gives them some negotiating power. Legacy governance structure: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification from theater, not from high chi. Analytical observer: Analytical → d≈0.50, f(d)≈0.65. Scaffold classification reflects generational sunset logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_gatekeeping_boundary,
    'Is the SBE''s role primarily coordination (providing common standards that enable diverse schools) or gatekeeping (suppressing alternatives that don''t conform to legacy frameworks)?',
    'Longitudinal analysis of SBE approvals vs denials; measurement of actual regulatory friction experienced by alternative models; comparison with states lacking strong SBE authority',
    'If primarily coordination: more Rope perspectives, lower ε (≈0.30). If primarily gatekeeping: more Snare perspectives, higher ε (≈0.65). Current analysis assumes both; classification as tangled_rope reflects the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_gatekeeping_boundary, empirical, 'Whether SBE functions as coordination or suppression mechanism').

omega_variable(
    alternative_legitimacy_maturation,
    'Are decentralized education alternatives (charters, homeschools, competency-based programs, microschools) accumulating sufficient market and legal legitimacy to bypass SBE gatekeeping within the next 20 years?',
    'Tracking of alternative education enrollment growth, parental satisfaction metrics, college/workforce outcomes for non-SBE-credentialed students, legislative removal of SBE barriers, multi-state reciprocity agreements',
    'If alternatives mature quickly: scaffold classification is confirmed, sunset is real within 15 years, constraint will degrade to piton. If alternatives stall: decentralization pressure is rhetorical, constraint persists as tangled_rope/snare indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_maturation, empirical, 'Whether alternative education models achieve sufficient legitimacy to bypass SBE').

omega_variable(
    federal_overrule_probability,
    'To what degree will federal education policy (Title I flexibility, competency-based reciprocity frameworks, school choice expansion) override state SBE authority?',
    'Analysis of federal policy trajectories; identification of which educational models already operate outside SBE control (federal programs, interstate compacts, private alternatives); assessment of Congressional appetite for educational devolution vs standardization',
    'If federal policy accelerates devolution: SBE constraint transforms to piton within 5-10 years. If federal policy strengthens standardization: SBE constraint persists as snare/tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_overrule_probability, preference, 'Federal policy trajectory for educational decentralization vs standardization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colorado_sbe_decentralization_friction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cosbe_tr_t0, colorado_sbe_decentralization_friction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cosbe_tr_t10, colorado_sbe_decentralization_friction, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cosbe_tr_t20, colorado_sbe_decentralization_friction, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(cosbe_be_t0, colorado_sbe_decentralization_friction, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(cosbe_be_t10, colorado_sbe_decentralization_friction, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cosbe_be_t20, colorado_sbe_decentralization_friction, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colorado_sbe_decentralization_friction, enforcement_mechanism).
narrative_ontology:affects_constraint(colorado_sbe_decentralization_friction, educational_credentialism_barrier).
narrative_ontology:affects_constraint(colorado_sbe_decentralization_friction, school_choice_implementation_friction).
narrative_ontology:affects_constraint(colorado_sbe_decentralization_friction, homeschool_legal_status_ambiguity).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific educational choice mechanisms (charter school autonomy, competency-based pathways, homeschool flexibility) but represents a distinct structural constraint on the legitimacy gatekeeper function. The SBE institutional preservation operates at the governance level, while downstream constraints deal with specific policy implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
