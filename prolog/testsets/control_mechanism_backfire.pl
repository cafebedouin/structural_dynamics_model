% ============================================================================
% CONSTRAINT STORY: control_mechanism_backfire
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_control_mechanism_backfire, []).

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
 *   constraint_id: control_mechanism_backfire
 *   human_readable: Control Mechanism Backfire: Internet Shutdowns and Communication Restrictions in Russia 2022-2026
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   Between 2022 and 2026, the Russian state implemented escalating internet
 *   shutdowns, Telegram restrictions, and VPN bans justified as national
 *   security measures to control information flow and prevent opposition
 *   organizing during the Ukraine conflict. The constraint exhibits a
 *   structural backfire pattern: the control mechanisms destroyed critical
 *   regime support infrastructure faster than they suppressed opposition.
 *   Small businesses dependent on Telegram advertising lost their only
 *   affordable marketing channel. Military volunteer networks coordinating
 *   soldier supply and family communication were disrupted during active
 *   conflict. Previously loyal demographics — provincial voters and
 *   pensioners — experienced economic hardship (business closures, banking
 *   app disruptions, pension payment delays) severe enough to override
 *   identity commitments, producing a 6.4-point collapse in United Russia
 *   Party support (34.1% December 2025 → 27.7% April 2026) and unprecedented
 *   protest application rates from these groups. The security apparatus
 *   experiences the constraint as coordination (information control for
 *   regime stability), but the analytical perspective reveals pure
 *   extraction: the regime is consuming its own support base to maintain
 *   short-term control, with no functional coordination substitute for the
 *   destroyed communication channels.
 *
 * KEY AGENTS:
 *   - Small Business Sector: Primary victim (powerless/trapped) — lost Telegram advertising (only affordable marketing), e-commerce logistics coordination via VPN restrictions; cannot relocate
 *   - Military Volunteer Networks: Primary victim (powerless/trapped) — Telegram was primary tool for crowdfunding, logistics, family communication; disruption during active conflict
 *   - Provincial Loyal Demographics: Primary victim (powerless/identity_locked) — identity constructed through regime loyalty; material extraction (business closures, economic disruption) eroding identity lock; United Russia support collapsing
 *   - Pensioner Demographics: Primary victim (moderate/constrained) — pension payment disruptions, family contact loss (emigrated children), state service access blocked; filing protest applications at unprecedented rates
 *   - Security Apparatus: Primary beneficiary (institutional/arbitrage) — experiences constraint as coordination (information control); has unrestricted communication channels; immediate time horizon
 *   - Regional Governors: Secondary victim (institutional/constrained) — benefit from regime stability but bear local costs (tax revenue loss, protest surge, military volunteer disruption); mixed experience
 *   - Analytical Observer: Generational perspective (analytical/analytical) — sees pure extraction masquerading as security coordination; backfire is structural and observable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(control_mechanism_backfire, 0.68).
domain_priors:suppression_score(control_mechanism_backfire, 0.82).
domain_priors:theater_ratio(control_mechanism_backfire, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(control_mechanism_backfire, extractiveness, 0.68).
narrative_ontology:constraint_metric(control_mechanism_backfire, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(control_mechanism_backfire, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(control_mechanism_backfire, snare).
narrative_ontology:human_readable(control_mechanism_backfire, "Control Mechanism Backfire: Internet Shutdowns and Communication Restrictions in Russia 2022-2026").
narrative_ontology:topic_domain(control_mechanism_backfire, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(control_mechanism_backfire).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(control_mechanism_backfire, security_apparatus).
narrative_ontology:constraint_victim(control_mechanism_backfire, small_business_sector).
narrative_ontology:constraint_victim(control_mechanism_backfire, military_volunteer_networks).
narrative_ontology:constraint_victim(control_mechanism_backfire, provincial_loyal_demographics).
narrative_ontology:constraint_victim(control_mechanism_backfire, pensioner_demographics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL BUSINESS OWNERS (SNARE) — Trapped by geographic and economic constraints. Cannot relocate businesses or customer bases. Telegram advertising was the only affordable marketing channel; VPN restrictions destroyed e-commerce logistics coordination. No coordination function visible — pure extraction destroying livelihoods to maintain regime information control. Maximum experienced extraction.
constraint_indexing:constraint_classification(control_mechanism_backfire, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY VOLUNTEER NETWORKS (SNARE) — Trapped by patriotic identity and geographic dispersion. Telegram was the primary coordination tool for crowdfunding, logistics, and family communication with deployed soldiers. Restrictions destroyed the supply networks the regime claimed to support. Immediate time horizon because disruption is acute and ongoing. Pure extraction with no coordination substitute.
constraint_indexing:constraint_classification(control_mechanism_backfire, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROVINCIAL LOYAL DEMOGRAPHICS (SNARE) — Identity-locked rather than materially trapped. These voters constructed their identity through regime loyalty and patriotic narratives. The communication restrictions destroyed their economic stability (small business closures, pension payment disruptions via blocked banking apps) while the regime's justification (national security) remained legible within their identity frame. But the material extraction is severe enough that support is eroding (United Russia 34.1% → 27.7%) even without identity frame collapse. Identity lock is weakening under material pressure.
constraint_indexing:constraint_classification(control_mechanism_backfire, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 4: PENSIONER DEMOGRAPHICS (SNARE) — Constrained rather than trapped: some have family abroad or savings, but exit costs are prohibitive for most. Communication restrictions disrupted pension payments (mobile banking apps), family contact (Telegram with emigrated children), and access to state services (online portals). Previously loyal demographic now filing protest applications at unprecedented rates. Moderate power because pensioners have some organizational capacity (Soviet-era networks) but limited resources.
constraint_indexing:constraint_classification(control_mechanism_backfire, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SECURITY APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as coordination: controlling information flow to prevent opposition organizing. Immediate time horizon because threat assessment is short-term. Arbitrage exit because security services can use unrestricted communication channels. The coordination story (national security) is the lived reality from this position, even as the mechanism destroys regime support base.
constraint_indexing:constraint_classification(control_mechanism_backfire, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REGIONAL GOVERNORS (TANGLED ROPE) — Constrained by regime loyalty requirements but also bearing costs. Governors need functional local economies and stable populations to meet performance metrics. Communication restrictions destroyed tax revenue (small business closures), increased protest activity (pensioners, provincial voters), and disrupted military volunteer coordination that governors were tasked with supporting. Mixed experience: benefit from regime stability (coordination) but bear local economic and political costs (extraction).
constraint_indexing:constraint_classification(control_mechanism_backfire, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a generational perspective, the constraint is pure extraction masquerading as security coordination. The coordination story (preventing opposition organizing) is cover for regime survival at the expense of economic functionality and regime legitimacy. The backfire is structural: the mechanism destroyed the regime's own support base (small business, military volunteers, loyal demographics) faster than it suppressed opposition. Observable evidence: United Russia support collapse, protest application surge from previously loyal groups, military supply network disruption during active conflict.
constraint_indexing:constraint_classification(control_mechanism_backfire, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(control_mechanism_backfire_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(control_mechanism_backfire, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(control_mechanism_backfire, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(control_mechanism_backfire, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(control_mechanism_backfire_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint destroys economic functionality (small business closures, supply network disruption, banking service loss) and regime legitimacy (loyal demographic defection) with minimal coordination substitute. The security apparatus benefits from short-term information control, but the regime as a whole bears net costs through support base erosion. The value increased from 0.35 (2022 Q1) to 0.68 (2026 Q1) as the backfire pattern became observable: initial restrictions had plausible security justification, but escalation destroyed more regime infrastructure than opposition capacity. Suppression (0.82): Very high. The constraint operates through technical infrastructure control (internet shutdowns, app bans, VPN restrictions) with no legal exit and severe penalties for circumvention. Suppression increased from 0.55 to 0.82 as enforcement intensified and alternative channels were systematically closed. The high suppression is what makes the backfire so damaging: victims cannot exit or adapt, so extraction accumulates. Theater ratio (0.45): Moderate. The national security justification has some functional content (the restrictions do impede opposition organizing), but the justification increasingly diverges from observed outcomes (regime support collapse, military network disruption). Theater increased from 0.30 to 0.45 as the gap between security narrative and structural damage widened. Not as high as pure performative constraints because the security apparatus genuinely uses the control mechanisms, even as they backfire.
 *
 * PERSPECTIVAL GAP:
 *   The security apparatus sees rope (coordination for national security). Regional governors see tangled_rope (mixed coordination and extraction — regime stability vs local costs). All victim groups see snare (pure extraction destroying livelihoods, networks, and economic stability with no coordination substitute). The analytical observer sees snare at the generational level (the coordination story is cover; the mechanism is consuming regime support base). The gap is structural: the beneficiary's immediate time horizon and arbitrage exit make the coordination function visible and the extraction invisible. The victims' biographical time horizon and trapped/constrained exit make the extraction maximal and the coordination function absent. The identity_locked provincial voters are the diagnostic case: their identity frame makes the security justification legible, but the material extraction is severe enough that support is collapsing anyway (United Russia 34.1% → 27.7%). This is the identity lock weakening under extraction pressure — the frame has not broken, but behavior is changing.
 *
 * DIRECTIONALITY LOGIC:
 *   The security apparatus is the primary beneficiary: they experience the constraint as solving a coordination problem (controlling information to prevent opposition organizing) and have arbitrage exit (unrestricted communication channels for state actors). Their directionality is low (near 0.0), producing low or negative effective extraction — they are net beneficiaries. Small business owners, military volunteer networks, provincial voters, and pensioners are victims: they bear the costs (economic destruction, communication loss, service disruption) with no benefits and no exit options. Their directionality is high (near 1.0 for trapped, slightly lower for identity_locked and constrained), producing high effective extraction. Regional governors have mixed directionality: they benefit from regime stability (coordination function) but bear local costs (economic and political damage). Their directionality is moderate (around 0.4-0.5), producing moderate effective extraction — the tangled_rope classification. The analytical observer sees the full structure: the coordination story is real from the security apparatus position but is cover for extraction from all other positions. The backfire is the diagnostic signal: a genuine coordination mechanism would not destroy its own support base.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the coordination vs extraction classification is observer-dependent and that backfire is a structural property, not a policy error. From the security apparatus position, the constraint is coordination (information control for regime stability) — this is their genuine experience, not a lie. From the victim positions, the constraint is extraction (economic destruction, network disruption, legitimacy erosion) — this is their genuine experience, not a misunderstanding. The analytical observer sees that both are true simultaneously: the mechanism coordinates for the beneficiary while extracting from everyone else, and the extraction is severe enough that it is destroying the regime's own support base. The backfire is not a bug; it is the structural consequence of high suppression + high extraction + no exit. The regime cannot reverse course without admitting the security justification was pretextual, but continuing the mechanism accelerates support base erosion. This is the snare closing on the regime itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_threshold,
    'At what point does a control mechanism''s damage to regime support base exceed its coordination value for information control?',
    'Longitudinal analysis of regime stability metrics (party support, protest rates, economic indicators) vs opposition organizing capacity. Threshold crossed when support erosion rate exceeds opposition suppression rate.',
    'If threshold not yet crossed: constraint is tangled_rope (mixed coordination and extraction). If threshold crossed: constraint is snare (extraction dominates, coordination story is cover). Observable evidence suggests threshold crossed by Q2 2026 (United Russia support -6.4 points, protest applications from loyal demographics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_threshold, empirical, 'Threshold where control mechanism damage exceeds coordination value').

omega_variable(
    identity_lock_durability,
    'How much material extraction can identity-locked loyal demographics sustain before identity frame collapses?',
    'Survey data on regime support vs economic hardship among provincial voters and pensioners. Tracking point where material costs override identity commitments. Historical comparison to other authoritarian regimes'' support base erosion patterns.',
    'If identity lock is durable: provincial and pensioner demographics remain constrained but loyal (tangled_rope from their perspective). If identity lock is breaking: these demographics shift to trapped victims (snare) or organize (moderate power upgrade). Observable evidence: protest applications from pensioners suggest identity lock weakening but not yet broken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Durability of identity lock under material extraction pressure').

omega_variable(
    alternative_coordination_channels,
    'Do security apparatus and regime-loyal actors have functional alternative coordination channels, or are they also damaged by the restrictions?',
    'Analysis of regime internal communication effectiveness, military logistics coordination, and security service operational capacity post-restrictions. If alternatives exist and function, beneficiary classification holds. If alternatives are also degraded, even beneficiaries experience extraction.',
    'If alternatives function: security apparatus remains net beneficiary (rope). If alternatives degraded: security apparatus also bears costs (tangled_rope), and the constraint is even more purely extractive than measured. Evidence: military volunteer network disruption suggests alternatives are not fully functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_channels, empirical, 'Whether regime actors have functional alternative coordination channels').

omega_variable(
    backfire_reversibility,
    'Is the damage to small business sector, military networks, and loyal demographics reversible if restrictions are lifted, or has permanent structural damage occurred?',
    'Economic recovery analysis if restrictions are lifted. Tracking business restart rates, network reconstitution, and voter realignment. Historical comparison to other states that lifted similar restrictions.',
    'If reversible: constraint is temporary extraction (scaffold logic applies if lifted). If irreversible: constraint has permanently destroyed regime support infrastructure, and the snare classification understates long-term damage. Observable evidence: small business closure rate and emigration patterns suggest substantial irreversible damage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(backfire_reversibility, empirical, 'Reversibility of structural damage to regime support base').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(control_mechanism_backfire, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmb_theater_2022_q1, control_mechanism_backfire, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cmb_theater_2023_q3, control_mechanism_backfire, theater_ratio, 6, 0.38).
narrative_ontology:measurement(cmb_theater_2025_q1, control_mechanism_backfire, theater_ratio, 12, 0.42).
narrative_ontology:measurement(cmb_theater_2026_q1, control_mechanism_backfire, theater_ratio, 16, 0.45).

% Extraction over time
narrative_ontology:measurement(cmb_extract_2022_q1, control_mechanism_backfire, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmb_extract_2023_q3, control_mechanism_backfire, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(cmb_extract_2025_q1, control_mechanism_backfire, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(cmb_extract_2026_q1, control_mechanism_backfire, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cmb_suppress_2022_q1, control_mechanism_backfire, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cmb_suppress_2023_q3, control_mechanism_backfire, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(cmb_suppress_2025_q1, control_mechanism_backfire, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(cmb_suppress_2026_q1, control_mechanism_backfire, suppression_requirement, 16, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(control_mechanism_backfire, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is a single structural phenomenon (communication restrictions) with one stable extractiveness value. The backfire is not a separate constraint but an observable consequence of the extraction mechanism operating on the regime's own support base. No decomposition needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(control_mechanism_backfire, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
