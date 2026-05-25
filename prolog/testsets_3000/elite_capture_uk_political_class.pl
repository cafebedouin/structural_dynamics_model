% ============================================================================
% CONSTRAINT STORY: elite_capture_uk_political_class
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_capture_uk_political_class, []).

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
 *   constraint_id: elite_capture_uk_political_class
 *   human_readable: Elite Capture of the UK Political Class
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Elite capture of the UK political class describes a structural constraint
 *   where financial, corporate, and landed elites exercise disproportionate
 *   influence over policy outcomes through interconnected mechanisms:
 *   regulatory capture by financial regulators, campaign finance dependency,
 *   revolving-door employment relationships between government and private
 *   sector, educational gatekeeping (Oxbridge concentration in Parliament),
 *   and informal social networks (clubs, marriages, friendship circles). The
 *   constraint exhibits genuine coordination functions — common-law legal
 *   frameworks, professional civil service, party system stability —
 *   alongside systematic extraction favoring elite interests. Working-class
 *   constituencies experience this as a snare: their representatives are
 *   structurally captured, electoral choice is narrow, and policy responsibly
 *   is suppressed. Regional economies experience mixed coordination and
 *   extraction. Career politicians experience identity lock: their
 *   professional identities are constituted through Westminster culture and
 *   elite relationships, making exit psychologically unavailable regardless
 *   of material cost. The theater ratio (0.65) reflects that Parliament
 *   maintains performative legitimacy while substantive decisions are made in
 *   closed elite networks — regulatory committees, advisory boards, private
 *   sector coordination — where public scrutiny is minimal. Over the 45-year
 *   measurement interval, both theater and extractiveness have risen,
 *   indicating degradation of democratic function alongside increasing elite
 *   preference concentration.
 *
 * KEY AGENTS:
 *   - Financial Sector Elite: Primary beneficiary (institutional/arbitrage) — captures regulatory framework, tax policy, post-parliamentary employment opportunities; experiences constraint as pure coordination of capital flows
 *   - Corporate Monopoly Holders: Primary beneficiary (institutional/arbitrage) — benefits from intellectual property capture, regulatory barriers to competition, government procurement preferences
 *   - Working Class Constituencies: Primary victim (powerless/trapped) — trapped by geography and economic dependency; experiences constraint as snare with systematically suppressed electoral responsiveness
 *   - Regional Economies: Secondary victim (moderate/constrained) — experiences mixed coordination and extraction; London-centric investment policy and deindustrialization subsidies extract value while some public goods coordination remains
 *   - Career Politicians: Tertiary victim (powerful/identity_locked) — constrained by party machinery and donor relationships; identity-fused with Westminster culture and elite social networks; faces high cost to exit the constraint
 *   - Parliamentary Institution: Institutional actor (institutional/constrained) — maintains ceremonial function while substantive decisions are made in elite closed-door settings; theater ratio reflects increasing gap between deliberative legitimacy and actual influence
 *   - Democratic Accountability: Abstract victim (powerless/trapped) — collective good with no self-advocate; systematically suppressed through campaign finance capture, postal voting patterns, two-party squeeze logic
 *   - Analytical Observer: Civilian/long-term perspective (analytical/analytical) — observes genuine coordination (legal frameworks, civil service norms) intertwined with extraction (regulatory capture, tax avoidance facilitation); constraint is tangled rather than pure extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_capture_uk_political_class, 0.58).
domain_priors:suppression_score(elite_capture_uk_political_class, 0.62).
domain_priors:theater_ratio(elite_capture_uk_political_class, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_capture_uk_political_class, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_capture_uk_political_class, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(elite_capture_uk_political_class, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_capture_uk_political_class, tangled_rope).
narrative_ontology:human_readable(elite_capture_uk_political_class, "Elite Capture of the UK Political Class").
narrative_ontology:topic_domain(elite_capture_uk_political_class, "political_economy/governance").

domain_priors:requires_active_enforcement(elite_capture_uk_political_class).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_capture_uk_political_class, financial_sector_elite).
narrative_ontology:constraint_beneficiary(elite_capture_uk_political_class, corporate_monopoly_holders).
narrative_ontology:constraint_beneficiary(elite_capture_uk_political_class, landed_gentry_interests).
narrative_ontology:constraint_beneficiary(elite_capture_uk_political_class, professional_gatekeepers).
narrative_ontology:constraint_victim(elite_capture_uk_political_class, working_class_constituencies).
narrative_ontology:constraint_victim(elite_capture_uk_political_class, regional_economies).
narrative_ontology:constraint_victim(elite_capture_uk_political_class, public_service_capacity).
narrative_ontology:constraint_victim(elite_capture_uk_political_class, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING CLASS CONSTITUENCY (SNARE) — Structurally trapped by geography, economic dependency, and limited political representation. Elites capture regulatory and fiscal policy to extract wealth while maintaining performative engagement through periodic electoral cycles. No meaningful exit from the constraint: voting produces no substantive policy change aligned with constituency interests. Maximum experienced extraction.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ECONOMY (TANGLED ROPE) — Experiences both coordination and extraction. National fiscal and regulatory frameworks coordinate certain public goods (infrastructure, education funding). Simultaneously, elite-captured policies extract through London-centric investment, deindustrialization subsidies, and tax arbitrage favoring financial centers. Significant barriers to exit (relocation costs, skill lockdown) but not absolute. Mixed experience of benefit and cost.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SECTOR ELITE (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: regulatory frameworks, tax policy, and preferential access to decision-making coordinate capital flows and minimize friction for financial operations. Full arbitrage exit option — can relocate capital globally if domestic policy becomes unfavorable. Net beneficiary experiencing minimal extraction.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAREER POLITICIAN (TANGLED ROPE) — Constrained by party machinery, donor relationships, and career path dependencies. Coordinates legitimate legislative function (law-making, representation) while simultaneously extracted from by elite networks that demand loyalty in exchange for campaign funding and post-parliamentary employment. Exit options exist (retire, switch parties) but carry significant career cost. Identity-locked into Westminster culture.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENTARY INSTITUTION (PITON) — Theater ratio (0.65) reflects that Parliament maintains performative legitimacy (debates, committee hearings, legislative rituals) while substantive policy decisions are increasingly made in elite closed-door settings (private dinners, advisory board meetings, regulatory capture in financial regulators and think tanks). Parliament appears to function as a decision-making body but is systematically bypassed or constrained by elite networks. Theater persists through institutional inertia.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEMOCRATIC ACCOUNTABILITY (SNARE) — Abstract collective good with no advocate and no exit. The constraint systematically suppresses real electoral choice through gerrymandering equivalents (postal voting patterns, constituency boundary manipulation favoring incumbents), campaign finance capture, and two-party squeeze logic that forces voters into 'lesser evil' choices. Accountability mechanisms (freedom of information, parliamentary oversight) are ceremonial, activated only after elite consensus has formed. No path to escape the constraint from within the system.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a long-term structural perspective, UK elite capture exhibits both coordination and extraction. Genuine coordination mechanisms include: common-law legal frameworks, civil service professionalism norms, and party system stability that prevent complete state collapse. Simultaneously, extraction is substantial: regulatory capture by financial interests, tax avoidance facilitation, and systematic skew of public investment toward elite-preferred sectors. The constraint is tangled — the coordination and extraction are functionally intertwined; removing the coordination would disrupt extraction, but maintaining coordination requires accepting asymmetric extraction.
constraint_indexing:constraint_classification(elite_capture_uk_political_class, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_capture_uk_political_class_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_capture_uk_political_class, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_capture_uk_political_class, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_capture_uk_political_class, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elite_capture_uk_political_class, TR),
    TR >= 0.70.

:- end_tests(elite_capture_uk_political_class_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Elite capture is substantial but not total — working-class constituencies retain some welfare state benefits, employment protections (though degraded), and electoral access. The extractiveness exceeds rope (≤0.45) because asymmetric benefit concentration is structural: financial sector receives £14bn annual subsidy through implicit guarantees; corporate tax avoidance reduces public revenue by ~£10bn annually; land-value capture through planning decisions favors existing owner interests. But extractiveness falls short of pure snare (≥0.66) because some coordination functions persist (common-law stability, anti-corruption civil service norms) and because coalition pressure from labor movements, public sector unions, and regional organizing occasionally shifts policy (2017-2019 Corbyn period demonstrated that captured constraints can be contested when unified working-class organizing occurs). Suppression (0.62): Moderate-high. Barriers to exit the constraint are substantial: working-class geographic lockdown, skill-credential gatekeeping preventing non-elite entry to Parliament, media control concentrating political narrative, campaign finance requiring elite relationships. But suppression is not total (0.70+) because: alternative media and organizing channels exist, some non-elite politicians succeed (though rare), and historical instances of constraint-breaking occurred (1945 NHS vote, 1997 majoritarian shift, though incomplete). Theater ratio (0.65): Parliament functions performatively. Committee hearings occur and are televised; legislation passes through formal procedures; MPs engage in ritualized debate. Simultaneously, substantive policy decisions (quantitative easing, regulatory forbearance for financial firms, procurement contracts favoring corporate friends) are made in regulatory agencies and private-sector coordination forums with minimal parliamentary input. The rising theater ratio over 45 years (0.50 → 0.65) indicates that Parliament's actual influence has diminished while its ceremonial legitimacy has been maintained through increased spectacle.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence because the same structural mechanism — elite regulatory capture and political finance dependency — is experienced as coordination by beneficiaries and as extraction by victims. This gap is diagnostic of a tangled rope: genuine coordination functions (legal stability, civil service competence) are structurally intertwined with extraction mechanisms (regulatory capture, post-parliamentary employment as compensation for favorable policy). Untangling would require dismantling the mechanisms that enable extraction — campaign finance reform, revolving-door bans, meritocratic access to Parliament — but these mechanisms are also the glue that stabilizes elite coordination. The perspectival gap also reflects identity-lock dynamics in the political class: career politicians perceive the constraint as natural (Westminster culture, donor relationships, party loyalty) because their professional identity is fused with these networks. An external observer sees institutional capture; the captured politician experiences inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status plus exit options. Financial sector elite are beneficiaries with global arbitrage exit (d ≈ 0.05, f(d) ≈ -0.12): they benefit from the constraint and can exit if policy becomes unfavorable. Working-class constituencies are victims with trapped exit (d ≈ 0.95, f(d) ≈ 1.42): they bear costs and have no meaningful exit path. Career politicians are victims with identity_locked exit (d ≈ 0.85, f(d) ≈ 1.28): they are trapped by professional identity and party machinery, though materially they could exit by changing careers or parties — the identity lock prevents exercising this material option. Regional economies are victims with constrained exit (d ≈ 0.60, f(d) ≈ 0.90): they bear costs from London-centric policy but can partially exit through regional organizing and capital flight, though at significant cost. No directionality overrides are required; the structural relationships are unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy (extractiveness 0.58 > 0.46) through perspectival multiplicity. A naive reading would classify as pure Snare (victims maximum extraction) but misses the genuine coordination functions. A naive reading would classify as pure Rope (elites coordinate policy) but misses the asymmetric extraction. The tangled rope classification acknowledges both: the constraint coordinates legal stability, civil service competence, and party system stability (genuine coordination values) while simultaneously extracting through regulatory capture, tax avoidance facilitation, and political finance capture (genuine extraction). The extraction is not incidental to coordination — it is the cost that weaker agents pay to maintain the stability that stronger agents require. Breaking the constraint would require either: (1) introducing alternative coordination mechanisms that do not require asymmetric extraction (campaign finance limits, proportional representation, meritocratic access to Parliament), or (2) accepting degraded coordination in exchange for reduced extraction (post-parliamentary employment bans, one-term politician norms, citizen assembly processes). The analytical observer perceives the tangled-rope structure clearly; the political class perceives it through identity-lock framing ('this is how things work'); working-class constituencies perceive pure snare ('no matter what we vote for, elite interests are protected'). These are not equally valid readings — they are indexed to position. The engine's role is to make this indexicality explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_capture_mechanisms,
    'Is elite capture primarily exercised through formal institutional channels (regulatory agencies, party funding rules) or through informal social networks (school ties, club membership, marriage)?',
    'Institutional mapping of decision-making flows; network analysis of government advisor appointments; campaign finance tracing; comparison of policy outcomes with formal vs informal pressure sources.',
    'If formal: constraint is fragile — legislative or regulatory reform could change outcomes significantly. If informal: constraint is robust — formal reform would be bypassed through social networks, requiring deeper cultural change. If mixed: both channels must be addressed for policy efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_mechanisms, empirical, 'Whether elite capture operates through formal institutions or informal networks').

omega_variable(
    intergenerational_reproduction,
    'Does elite capture persist because elites actively maintain exclusionary mechanisms (gatekeeping, credential inflation) or because non-elites lack structural capacity to compete (access barriers are real, not enforced)?',
    'Longitudinal analysis of social mobility rates; comparison of credential requirements over time; study of cases where non-elite-origin politicians succeed vs fail; measurement of actual vs claimed ''meritocratic'' barriers.',
    'If active gatekeeping: capture is maintained through enforcement — dissolution requires political will. If structural capacity gap: capture persists through path dependency — dissolution requires long-term human capital investment. Solutions differ substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_reproduction, empirical, 'Whether elite capture is maintained through active gatekeeping or structural capacity gaps').

omega_variable(
    coalition_breakpoint_threshold,
    'At what level of economic inequality, public service degradation, or democratic unresponsiveness do working-class constituencies achieve sufficient organization to break the capture constraint?',
    'Historical analysis of prior elite-capture breakdown events (1945-1974 NHS/welfare expansion, 1997-2010 redistributive policies); identification of common thresholds across democracies; modeling of tipping points in working-class coalition formation.',
    'If threshold is far: constraint appears permanent from biographical timescale. If threshold is near: coalitional pressure is imminent. If threshold is variable/contextual: outcome depends on contingent organizing factors rather than structural inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_breakpoint_threshold, empirical, 'Tipping point threshold for working-class coalition breaking elite capture').

omega_variable(
    extraction_vs_stabilization,
    'Do elite extraction mechanisms (tax avoidance, regulatory capture) provide net economic efficiency to the broader system, or are they pure rent extraction that reduces overall system capacity?',
    'Comparative analysis of UK economic growth, productivity, and inequality trajectories vs peer democracies; measurement of public service outcomes controlling for elite extraction; cost-benefit analysis of specific capture mechanisms (financial deregulation, patent monopolies).',
    'If extraction provides stabilization: working-class constituencies face a real trade-off between equality and stability — constraint is harder to break. If extraction reduces capacity: breaking the constraint would increase public investment — constraint is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_stabilization, empirical, 'Whether elite extraction stabilizes or destabilizes the broader system').

omega_variable(
    identity_lock_political_class,
    'Are career politicians bound to elite networks through material dependency (post-parliamentary employment, pension schemes tied to City connections) or through identity fusion (professional identity constituted through Westminster culture and elite friendship networks)?',
    'Interview analysis of politician exit pathways; measurement of post-parliamentary employment dependency on elite networks; psychological analysis of Westminster culture''s role in identity formation; comparison of politicians with vs without elite social capital.',
    'If material dependency dominates: external incentive changes (ban on revolving doors, pension reform) could shift politician behavior. If identity fusion dominates: constraint operates through internalized framing that persists regardless of material incentive change. Identity lock requires frame disruption to break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_political_class, conceptual, 'Whether political class capture is material or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_capture_uk_political_class, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elcap_tr_t0, elite_capture_uk_political_class, theater_ratio, 0, 0.5).
narrative_ontology:measurement(elcap_tr_t15, elite_capture_uk_political_class, theater_ratio, 15, 0.58).
narrative_ontology:measurement(elcap_tr_t30, elite_capture_uk_political_class, theater_ratio, 30, 0.65).
narrative_ontology:measurement(elcap_tr_t45, elite_capture_uk_political_class, theater_ratio, 45, 0.64).

% Extraction over time
narrative_ontology:measurement(elcap_be_t0, elite_capture_uk_political_class, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(elcap_be_t15, elite_capture_uk_political_class, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(elcap_be_t30, elite_capture_uk_political_class, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(elcap_be_t45, elite_capture_uk_political_class, base_extractiveness, 45, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_capture_uk_political_class, enforcement_mechanism).
narrative_ontology:affects_constraint(elite_capture_uk_political_class, financial_deregulation_cascade).
narrative_ontology:affects_constraint(elite_capture_uk_political_class, austerity_policy_lock).
narrative_ontology:affects_constraint(elite_capture_uk_political_class, housing_market_gatekeeping).
narrative_ontology:affects_constraint(elite_capture_uk_political_class, private_school_credential_gatekeeping).

% DUAL FORMULATION NOTE:
% Elite capture of the UK political class is upstream of multiple policy constraints that implement extracted value. Financial deregulation, austerity politics, housing market gatekeeping, and educational credential inflation all flow from regulatory capture by elite interests. These are separate constraint stories with their own extractiveness values (typically higher than the parent constraint) but are only viable because the parent constraint suppresses countervailing political pressure. Decompose when analyzing individual policy domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
