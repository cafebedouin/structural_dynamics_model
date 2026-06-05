% ============================================================================
% CONSTRAINT STORY: china_contraceptive_tax
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_contraceptive_tax, []).

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
 *   constraint_id: china_contraceptive_tax
 *   human_readable: China's Differential Tax on Contraceptives
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's differential taxation of contraceptive methods represents a
 *   structural hybrid of state population control policy and extraction
 *   mechanism. The 17% VAT on condoms, combined with exemptions or subsidies
 *   for IUDs and sterilization, creates a coordinated system that serves the
 *   state's demographic objectives while extracting behavioral compliance
 *   from consumers and healthcare providers. The constraint is tangled_rope
 *   at the structural level: it possesses a genuine coordination function
 *   (implementing population policy goals across healthcare system and
 *   consumer markets) while simultaneously extracting through financial
 *   burden, suppression of alternatives, and coercion of provider behavior.
 *   The theater ratio (0.48) is moderate — the tax is nominally framed as
 *   standard VAT policy and health standards cost-recovery, partially
 *   disguising its demographic intent. Extractiveness has increased over the
 *   interval (0.42 → 0.58) as enforcement mechanisms have matured and
 *   IUD/sterilization subsidies have expanded, while theater has actually
 *   decreased slightly as the demographic policy rationale has become more
 *   transparent in policy documents.
 *
 * KEY AGENTS:
 *   - Individual Condom Consumer: Primary victim (powerless/trapped) — bears tax burden with minimal exit options; faces informational suppression about alternatives
 *   - State Family Planning Authority: Primary beneficiary (institutional/arbitrage) — captures behavioral compliance, demographic outcomes, and tax revenue; directs coordination of health system toward population control objectives
 *   - Public Health Clinician: Secondary victim/participant (moderate/constrained) — experiences mixed coordination benefit (clearer clinical guidelines) and extraction (coerced method steering, performance metrics tied to state-preferred methods)
 *   - Reproductive Rights Organizations: Organized opposition (organized/constrained) — attempt to create alternatives and increase autonomy; constrained by legal/regulatory restrictions
 *   - Condom Industry: Secondary institutional actor (institutional/arbitrage) — maintains formal market presence despite adverse tax conditions; experiences piton-like degradation through institutional inertia
 *   - Analytical Observer: Civilizational analytical view (analytical/analytical) — risks naturalizing demographic extraction as immutable economic/health policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_contraceptive_tax, 0.58).
domain_priors:suppression_score(china_contraceptive_tax, 0.65).
domain_priors:theater_ratio(china_contraceptive_tax, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_contraceptive_tax, extractiveness, 0.58).
narrative_ontology:constraint_metric(china_contraceptive_tax, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_contraceptive_tax, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_contraceptive_tax, tangled_rope).
narrative_ontology:human_readable(china_contraceptive_tax, "China's Differential Tax on Contraceptives").
narrative_ontology:topic_domain(china_contraceptive_tax, "economic/political").

domain_priors:requires_active_enforcement(china_contraceptive_tax).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, state_family_planning_authority).
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, iud_manufacturers).
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, state_population_control_agenda).
narrative_ontology:constraint_victim(china_contraceptive_tax, condom_consumers).
narrative_ontology:constraint_victim(china_contraceptive_tax, sexual_autonomy).
narrative_ontology:constraint_victim(china_contraceptive_tax, reproductive_choice_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CONDOM CONSUMER (SNARE) — Price-sensitive populations face a 17% tax burden on the only contraceptive method that provides dual protection (pregnancy + STI prevention) and does not require invasive medical procedures. Exit options are severely constrained: switching to subsidized IUDs requires medical access and acceptance of invasive methods; purchasing contraband imports risks legal sanction; abstinence is not a realistic exit. The constraint extracts financial burden while suppressing knowledge of alternatives and enforcement is active through tax authority mechanisms.
constraint_indexing:constraint_classification(china_contraceptive_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH CLINICIAN (TANGLED ROPE) — Healthcare providers in state clinics experience the constraint as a hybrid: they coordinate the population health agenda (coordination benefit: IUD/sterilization methods align with long-acting reversible contraceptive guidelines) while simultaneously being coerced to steer patients away from condoms through subsidy structure. Exit options are constrained by employment dependence on state institutions. Suppression operates through performance metrics tied to use of state-preferred methods. Some benefit from clearer clinical guidelines; significant extraction through moral hazard and professional autonomy constraint.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE FAMILY PLANNING AUTHORITY (ROPE) — The primary beneficiary. Experiences the constraint as pure coordination: the differential tax implements the state's long-term population control objectives (Han majority growth constraints, ethnic minority policies). IUD/sterilization methods are state-preferred because they are more difficult to reverse and require medical compliance. The constraint coordinates the incentive structure with population policy. Extraction operates toward this institutional actor — tax revenue, behavioral compliance, and demographic outcomes all flow inward. Active enforcement through tax authorities and health ministry coordination.
constraint_indexing:constraint_classification(china_contraceptive_tax, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REPRODUCTIVE RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized actors (NGOs, international reproductive health bodies) see the constraint as both a coordination mechanism (implementing state goals) and an extraction mechanism targeting reproductive autonomy. These organizations have partial exit options: international funding sources, alliance with international health bodies, potential reputational leverage. Constrained by legal restrictions on grassroots organizing in China. Suppression is active through censorship of messaging around condom access equity. Moderate power derived from organizational capacity, but extraction flow is toward state family planning objectives.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONDOM INDUSTRY / FORMAL MARKET (PITON) — Manufacturers operating in the formal market experience a degraded constraint. The 17% VAT was nominally justified by health/protection standards compliance, but the exemption of state-preferred methods reveals the underlying population control agenda. Industry compliance is maintained through institutional inertia — formal manufacturers continue operations despite adverse conditions because (a) alternative markets are unavailable, (b) exiting triggers regulatory retaliation, (c) black-market alternatives exist but lack legitimacy. Theater ratio is moderate-high: the tax is framed as fiscal policy, disguising its demographic intent. Extraction persists because the mechanism is embedded in broader tax/health systems, not because it is functionally necessary.
constraint_indexing:constraint_classification(china_contraceptive_tax, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist analytical stance, some differential pricing of contraceptive methods might appear immutable: different methods have different costs, efficacy profiles, and side-effect profiles; price differentiation based on these technical factors could be framed as natural. However, the structural data contradicts the mountain classification: the 17% VAT is politically determined, not technically necessitated. The exemption of IUDs/sterilization is demographic policy, not health-based ranking. This perspective reveals the false summit: what appears as natural market economics is actually institutional extraction.
constraint_indexing:constraint_classification(china_contraceptive_tax, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_contraceptive_tax_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_contraceptive_tax, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_contraceptive_tax, TR),
    TR >= 0.70.

:- end_tests(china_contraceptive_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint operates through multiple extraction mechanisms: (1) direct 17% price premium on condoms, bearing disproportionately on low-income consumers; (2) behavioral steering through subsidy asymmetry; (3) provider coercion through performance metrics. The value reflects that extraction is substantial but not absolute — consumers have some purchasing power to absorb the tax, some black-market alternatives exist, and state enforcement is not total surveillance. The increase over the interval (0.42 → 0.58) reflects maturation of enforcement mechanisms and expansion of IUD/sterilization subsidies. Suppression (0.65): High. Multiple suppression mechanisms operate: informational (media narratives emphasizing IUD/sterilization safety and condom limitations), institutional (provider training biased toward state-preferred methods), and financial (price barrier). These are active and coordinated, not accidental. Theater ratio (0.48): Moderate. The tax is framed as standard VAT/health policy, partially disguising demographic intent. Unlike pure performative constraints (theater > 0.70), the tax actually accomplishes behavioral change — it is functional extraction, not ritualistic maintenance. Theater has declined slightly as policy documents have become more explicit about demographic objectives.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (trapped consumer) and rope perspective (institutional beneficiary) represent opposite structural positions: one experiences maximum extraction with no exit, the other experiences coordination benefit flowing inward. The tangled_rope perspectives (clinician, reproductive rights organizations) represent agents with partial agency but significant constraints — they see both coordination and extraction. The piton perspective (condom industry) reveals institutional inertia — formal market presence maintained despite adverse conditions because alternatives are worse. The false summit (analytical natural law view) reveals the critical insight: what appears as immutable economic reality is actually contingent institutional arrangement. The mandatrophy here is resolved by showing that all six perspectives are legitimate readings of the same constraint structure, each revealing different aspects of how extraction and coordination are bundled.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural relationship to the tax mechanism. Beneficiaries (state family planning authority) derive low d values because they experience subsidy flows and behavioral compliance flowing toward them. Victims (condom consumers) derive high d values because they bear the tax burden with limited exit options. Organized opposition (reproductive rights organizations) derive intermediate d because they have partial agency (international support, reputational leverage) but constrained exit (legal restrictions). The state family planning authority's institutional power and arbitrage options (enforcement through tax authority, health ministry integration) drive f(d) toward enabling extraction. The consumer's powerlessness and trapped exit options drive f(d) toward maximizing experienced extractiveness. The clinician's moderate power but constrained exit produces intermediate d reflecting mixed benefit/burden experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit classical mandatrophy (false natural law masquerading as coordination). Instead, it exhibits a subtle form: INSTITUTIONAL NATURALIZATION. The constraint is genuinely both coordination (implementing population policy) and extraction (taxing consumer choice). The mandatrophy emerges when observers frame the extraction as natural or inevitable — 'different contraceptive methods have different costs, so differential pricing is natural.' The structural data reveals this as false: the 17% VAT and IUD/sterilization exemptions are politically determined, not technically necessary. The mandatrophy is resolved by declaring the constraint as tangled_rope (genuinely hybrid) while documenting that the coordination function (population policy) is itself contested — reproductive autonomy advocates see the coordination goal as illegitimate extraction. Thus the constraint resolves to tangled_rope without false natural law at the institutional level, but with the caveat that the 'coordination' being achieved is itself a values question. No false summit is present once the institutional naturalization is stripped away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_intent_vs_health_framing,
    'Is the differential tax primarily a population control mechanism or a genuine health/protection-standard cost-recovery measure?',
    'Historical document analysis of tax rationale statements; correlation between tax structure changes and stated population policy objectives; international health body assessments of condom safety vs IUD safety standards',
    'If primarily demographic: snare classification correct, extraction flow is toward state population control. If primarily health-based: rope classification correct, coordination benefit is legitimate public health governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_intent_vs_health_framing, empirical, 'Whether tax is demographic policy or health cost-recovery').

omega_variable(
    actual_behavioral_elasticity,
    'Does the 17% tax actually shift contraceptive choice behavior, or do consumers absorb the cost and maintain condom use despite the tax burden?',
    'Market data: condom sales volume pre/post tax; survey data on method choice by income quintile; black-market penetration rates; STI incidence correlation with tax timing',
    'High elasticity: constraint is effective extraction. Low elasticity: constraint is theatrical (piton-like) — extraction exists on paper but not in behavioral reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_behavioral_elasticity, empirical, 'Whether tax meaningfully shifts contraceptive choice behavior').

omega_variable(
    suppression_mechanism_reversibility,
    'Is the suppression of condom knowledge/accessibility reversible through policy change, or has it created irreversible institutional dependencies on IUD/sterilization pathways?',
    'Institutional analysis of health system training, supply chain dependencies, provider performance metrics; feasibility analysis of shifting subsidies toward condoms; international experience with similar reversals',
    'If reversible: constraint is malleable policy (scaffold-like with sunset potential). If irreversible: suppression has created locked-in institutional architecture (snare with high persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_reversibility, conceptual, 'Whether suppression of condom accessibility is reversible').

omega_variable(
    enforcement_cost_sustainability,
    'Can the state sustain active enforcement of the tax/subsidy differential as generational cohorts age and fertility preferences shift toward smaller families independent of policy?',
    'Fiscal analysis of enforcement costs relative to revenue; demographic modeling of natural fertility decline; international precedent for demographic constraint persistence under economic pressure',
    'Low sustainability: constraint transitions to piton (inertial). High sustainability: constraint remains tangled_rope with stable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_sustainability, empirical, 'Long-term fiscal and institutional sustainability of enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_contraceptive_tax, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cctax_tr_t0, china_contraceptive_tax, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cctax_tr_t5, china_contraceptive_tax, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cctax_tr_t10, china_contraceptive_tax, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cctax_be_t0, china_contraceptive_tax, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cctax_be_t5, china_contraceptive_tax, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cctax_be_t10, china_contraceptive_tax, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_contraceptive_tax, resource_allocation).
narrative_ontology:affects_constraint(china_contraceptive_tax, china_reproductive_coercion_history).
narrative_ontology:affects_constraint(china_contraceptive_tax, iud_market_capture_mechanism).

% DUAL FORMULATION NOTE:
% The contraceptive tax constraint is a contemporary instantiation of China's long-standing population control architecture. It is downstream of historical reproductive coercion mechanisms (one-child policy, forced sterilization practices) and upstream of market structure mechanisms (IUD manufacturing capacity, provider dependencies). The constraint family spans both policy/demographic analysis and health economics analysis. Separate stories exist for historical coercion mechanisms (higher ε, snare-dominant) and market capture mechanisms (lower ε, tangled_rope dominant).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_contraceptive_tax, powerless, 0.92).
constraint_indexing:directionality_override(china_contraceptive_tax, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
