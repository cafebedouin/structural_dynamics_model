% ============================================================================
% CONSTRAINT STORY: elite_overproduction_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_overproduction_instability, []).

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
 *   constraint_id: elite_overproduction_instability
 *   human_readable: The Aspirant's Bottleneck: Elite Overproduction Instability
 *   domain: social/educational/economic
 *
 * SUMMARY:
 *   Elite overproduction creates a structural bottleneck where educational
 *   and credentialing institutions produce far more individuals prepared for
 *   high-status positions than the economy or social hierarchy can
 *   accommodate. This constraint exhibits the full range of indexical
 *   perspectives because it combines a genuine coordination function
 *   (matching talent to selective roles) with systematic extraction (surplus
 *   aspirants bear the costs of positional competition while incumbent elites
 *   capture benefits). The constraint operates through a tangled rope
 *   mechanism: credentialing institutions enforce credential standards
 *   (requiring enforcement), aspirants genuinely benefit from education and
 *   status signaling, but the system extracts from the surplus through
 *   psychological costs (status anxiety, credential burden), financial costs
 *   (debt, underemployment), and social costs (cohesion loss, political
 *   instability). Theater has increased as the meritocratic legitimation
 *   narrative—that positions are allocated fairly to the most talented—has
 *   degraded relative to observable allocation patterns that track social
 *   origin.
 *
 * KEY AGENTS:
 *   - Surplus Aspirants: Primary victims (powerless/trapped) — educated, credentialed, but structurally unable to access promised elite positions; bear debt burden, status anxiety, precarious employment
 *   - Incumbent Elites: Primary beneficiaries (institutional/arbitrage) — benefit from credential surplus enabling selective hiring, wage depression, and cultural homogeneity; can arbitrage into alternative hierarchies if threatened
 *   - Credentialing Institutions: Primary beneficiaries (institutional/arbitrage) — capture enrollment revenue, tuition streams, and credential scarcity value; maintain institutional autonomy through arbitrage (credential redefinition, tier creation)
 *   - Aspirant Coalition: Organized actors (organized/constrained) — professional associations, credential holders' unions, activist networks organizing response to bottleneck; building pressure on salaries, working conditions, credential standards
 *   - Social Cohesion: Victim (powerless/trapped) — abstract collective good; overproduction correlates with political instability, polarization, and trust decline; no organized advocate
 *   - Meritocratic Legitimation System: Institutional (institutional/arbitrage) — performative claim that elite selection is merit-based; degraded through observable allocation bias but maintained through ritual (ranking systems, certification ceremonies, selection narratives)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_overproduction_instability, 0.58).
domain_priors:suppression_score(elite_overproduction_instability, 0.68).
domain_priors:theater_ratio(elite_overproduction_instability, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_overproduction_instability, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_overproduction_instability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(elite_overproduction_instability, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_overproduction_instability, tangled_rope).
narrative_ontology:human_readable(elite_overproduction_instability, "The Aspirant's Bottleneck: Elite Overproduction Instability").
narrative_ontology:topic_domain(elite_overproduction_instability, "social/educational/economic").

domain_priors:requires_active_enforcement(elite_overproduction_instability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_overproduction_instability, incumbent_elites).
narrative_ontology:constraint_beneficiary(elite_overproduction_instability, credentialing_institutions).
narrative_ontology:constraint_victim(elite_overproduction_instability, surplus_aspirants).
narrative_ontology:constraint_victim(elite_overproduction_instability, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURPLUS ASPIRANT (SNARE) — Educated, credentialed, but structurally trapped in a system that promised status in exchange for conformity to the credential pathway. No exit: cannot uncredential, cannot unlearn, cannot opt out of the comparison game. Bears full extraction: delayed gratification, debt burden, status anxiety, precarious employment despite credentials. Maximum suppression through social norming and credential-gatekeeping.
constraint_indexing:constraint_classification(elite_overproduction_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOWNWARDLY MOBILE CREDENTIAL HOLDER (SNARE) — Possesses credentials signaling elite position but cannot access corresponding roles. Constrained exit: some mobility into adjacent fields but penalized for 'waste' of education. High suppression through credential ranking and field-specific gatekeeping. Experiences extraction as credential demotion and constant justification for status mismatch.
constraint_indexing:constraint_classification(elite_overproduction_instability, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT ELITE (ROPE) — Benefits from credential surplus: excess aspirants competing for limited positions drives down wage expectations and increases compliance with organizational norms. High selectivity enables elite groups to extract cultural homogeneity and ideological conformity. Experiences the constraint as coordination mechanism: selecting from a surplus pool is more efficient than rationing scarce positions. Arbitrage exit available — elites can shift to parallel credential systems or build alternative hierarchies.
constraint_indexing:constraint_classification(elite_overproduction_instability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CREDENTIALING INSTITUTION (ROPE) — Structurally benefits from overproduction: excess aspirants increase enrollment, tuition revenue, and credential value (through scarcity signaling). Experiences the constraint as pure coordination: matching aspirants to positions (even failed matches) validates the institution's function. Arbitrage exit available — can shift credentialing definitions, create new tiers (honors, specializations), or claim meritocratic sorting even as sorting fails.
constraint_indexing:constraint_classification(elite_overproduction_instability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ASPIRANT COALITION (TANGLED ROPE) — Organized surplus aspirants (professional associations, credential holders' unions, activist networks) observe both coordination function and extraction. The system does provide genuine upward mobility for some and genuine status signaling; but it also enforces conformity and extracts psychological and financial resources from the many to benefit the few. Active enforcement of credential standards keeps the bottleneck in place; but coalitional organizing is building pressure on salaries, working conditions, and credential signaling practices.
constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MERITOCRATIC LEGITIMATION SYSTEM (PITON) — The overproduction constraint is maintained by the performative claim that positions are allocated meritocratically and that all credential holders are competing fairly. This narrative is largely degraded — elite positions correlate more with social origin than credential differentiation — yet the meritocratic theater persists through institutional inertia. High theater ratio: credentialing rituals, ranking systems, and selection ceremonies maintain the appearance of fairness while actual allocation follows social network and resource availability. The system itself acknowledges its own degradation (well-known selection bias, credential inflation, meritocratic myths) but continues ritual performance.
constraint_indexing:constraint_classification(elite_overproduction_instability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_overproduction_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_overproduction_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_overproduction_instability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_overproduction_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elite_overproduction_instability, TR),
    TR >= 0.70.

:- end_tests(elite_overproduction_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from surplus aspirants through credential burden and status anxiety, but also provides genuine educational benefit and some upward mobility pathway. The trajectory from 0.35 to 0.58 reflects credential inflation accelerating over the interval: more positions require credentials, more credentials are produced, credential value declines, extraction pressure increases. Suppression (0.68): High. Multiple mechanisms suppress alternatives to credential competition: social norming (all respectable careers require credentials), institutional gatekeeping (role access tied to certified credentials), cultural narratives (meritocratic justification discourages questioning the system). Exit options are severely limited — surplus aspirants cannot uncredential, cannot exit the comparison game without status loss, and cannot access alternative hierarchies that bypass credential requirements. Theater ratio (0.62): Moderate-high. The system maintains significant performative content: ranking systems create illusion of differentiation, selection ceremonies (interviews, applications, assessments) create impression of merit-based sorting despite observed allocation bias, meritocratic narratives persist despite degradation. Theater increased over the interval as credential inflation accelerated — more symbolic ranking created to preserve illusion of scarcity and differentiation as actual differentiation declined.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal. Incumbent elites and credentialing institutions experience the constraint as coordination (Rope) — surplus aspirants provide efficient matching and enable selective hiring. But surplus aspirants experience the constraint as extraction (Snare) — the system promises status in exchange for conformity but systematically fails to deliver for the majority. The aspirant coalition occupies a middle position (Tangled Rope) — they recognize both the coordination function (education has real value) and the extraction (credential burden exceeds educational benefit for many). The piton perspective reveals that the entire system is degraded: meritocratic legitimation is now largely performative theater, yet the ritual persists through institutional inertia because no actor has sufficient power or incentive to fundamentally restructure the hierarchy. The meritocratic narrative is maintained not because it is believed, but because it is convenient for incumbents and because alternatives (explicit stratification, lottery-based allocation, skill-based sorting) threaten the credentialing institutions themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Surplus aspirants are trapped in the credential competition (d ≈ 0.95) — they cannot exit without status loss, have no arbitrage options, and bear maximum suppression through social norming. Their d value produces high f(d) ≈ 1.42, making their experienced extractiveness very high. Incumbent elites are beneficiaries with arbitrage options (d ≈ 0.05) — they benefit from the surplus, can exit into alternative hierarchies, and experience the constraint as coordination rather than extraction. Their d value produces negative f(d), lowering their experienced χ. Credentialing institutions occupy a similar position (d ≈ 0.10) — beneficiaries with arbitrage options (they can redefine credentials, create new tiers, or shift credentialing mechanisms). The aspirant coalition has constrained but organized options (d ≈ 0.55) — they can neither fully exit nor fully accept; they are building pressure that produces moderate experienced extraction. The meritocratic legitimation system is institutional/arbitrage (d ≈ 0.10) — it primarily serves incumbent interests through narrative justification and experiences low χ because it can arbitrage into new narratives if current ones fail.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves the mandatrophy by showing that both coordination and extraction are genuine structural features of the same system. The bottleneck IS a coordination mechanism (matching talent to selective roles through credentials) AND an extraction mechanism (surplus aspirants bear the costs of competition while benefits concentrate). The suppression of the coordination function by extraction is the key: as overproduction increases, the coordination value decreases (most credentials don't lead to elite positions, so matching efficiency declines) while extraction increases (more aspirants competing for fewer positions). The theater ratio increase (0.45 to 0.62) indicates Goodhart degradation: the meritocratic legitimation narrative has become increasingly decoupled from actual allocation, suggesting the system is shifting from 'genuine coordination with extraction overhead' toward 'pure extraction with coordination performance.' The classified type (tangled_rope) is correct at current interval end: enforced credential standards + multiple beneficiaries + multiple victims + genuine but asymmetric benefit flow. However, the trajectory toward higher theater suggests the system may degrade toward piton (pure performance) if meritocratic legitimation continues to fail. The aspirant coalition's organizing effort represents potential pressure toward scaffold (sunset) if alternative credential systems gain viability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_position_count,
    'What is the true structural demand for elite positions (controlling for credential inflation and role proliferation)?',
    'Historical analysis of elite role creation, credential requirement changes, and actual task complexity changes; comparison of role definitions 50 years prior vs. current; survey of elite-position incumbents on necessity of credential levels',
    'If true demand is higher than assumed: overproduction diagnosis may be overstated, and constraint is more coordination (Rope) than extraction (Snare). If true demand is flat or declining: overproduction is systemic and extractive classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_position_count, empirical, 'True structural demand for elite positions vs. credential supply').

omega_variable(
    status_competition_endogeneity,
    'Does credential overproduction cause aspirants to acquire credentials, or does credential demand cause overproduction? (Which is the driver?)',
    'Causal analysis of credential growth vs. position availability over time; identification of inflection points when supply began exceeding demand; analysis of aspirant motivation (intrinsic interest vs. positional competition)',
    'If aspirants are driven by positional competition: extraction is endogenous and the system is a snare. If credential demand is genuinely independent: overproduction is exogenous shock, and the constraint is temporary (Scaffold not Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(status_competition_endogeneity, conceptual, 'Whether overproduction is endogenous or exogenous to aspirant behavior').

omega_variable(
    social_cohesion_causation,
    'Does elite overproduction directly destabilize social cohesion, or does it merely correlate with other destabilizing factors (economic stagnation, globalization, polarization)?',
    'Comparative study of societies with similar overproduction rates but different cohesion outcomes; causal pathway analysis between credential mismatch and political instability; identification of confounders (income inequality, geographic mobility, demographic change)',
    'If direct causation: constraint victimizes social_cohesion and should remain in victims list. If correlation only: social_cohesion is affected but not a victim of the constraint itself — classification changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cohesion_causation, empirical, 'Direct causal link between overproduction and cohesion loss').

omega_variable(
    alternative_hierarchy_viability,
    'Can alternative credential or status systems (guild certification, skill-based sorting, network-based advancement) successfully compete with the traditional elite hierarchy?',
    'Case studies of alternative credential systems (vocational certification, open-source credentials, direct hire models); measurement of adoption rates and stability; comparison of satisfaction and stability outcomes',
    'If viable alternatives exist: bottleneck is Scaffold (sunset clause implicit) not Snare. If alternatives are suppressed or systematically outcompeted: Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hierarchy_viability, empirical, 'Viability of alternative status systems as exits from elite hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_overproduction_instability, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eop_tr_t0, elite_overproduction_instability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(eop_tr_t20, elite_overproduction_instability, theater_ratio, 20, 0.55).
narrative_ontology:measurement(eop_tr_t40, elite_overproduction_instability, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(eop_be_t0, elite_overproduction_instability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eop_be_t20, elite_overproduction_instability, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(eop_be_t40, elite_overproduction_instability, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_overproduction_instability, resource_allocation).
narrative_ontology:affects_constraint(elite_overproduction_instability, credentialism_signaling_cascade).
narrative_ontology:affects_constraint(elite_overproduction_instability, downward_mobility_precarity).
narrative_ontology:affects_constraint(elite_overproduction_instability, populist_backlash_instability).

% DUAL FORMULATION NOTE:
% Elite overproduction is upstream of credentialism-as-signaling cascade (the constraint creates excess demand for credentials, which accelerates signaling inflation). It is also upstream of downward mobility precarity (the bottleneck creates credential holders unable to access promised positions). The constraint network includes: credential requirement inflation → elite overproduction → social cohesion loss → political instability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elite_overproduction_instability, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
