% ============================================================================
% CONSTRAINT STORY: executive_war_powers_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_executive_war_powers_expansion, []).

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
 *   constraint_id: executive_war_powers_expansion
 *   human_readable: Executive War Powers Expansion
 *   domain: political/constitutional_law
 *
 * SUMMARY:
 *   Executive war powers expansion represents a systematic extraction of
 *   constitutional authority from the legislative branch, justified through
 *   security rhetoric and normalized through repeated precedent-setting. The
 *   constraint operates across multiple institutional levels: individual
 *   presidents accumulate authorities; bureaucracies inherit expanded
 *   mandates; subsequent administrations inherit a broader interpretation of
 *   executive prerogative; each crisis generates new claims to emergency
 *   authority; the Overton window shifts incrementally. The mechanism
 *   exhibits high suppression through classification authorities (military
 *   necessity, national security, intelligence protection) that prevent
 *   transparent deliberation. Theater ratio (0.58) reflects that formal
 *   constraints (War Powers Resolution, congressional authorization rhetoric)
 *   persist performatively while functional constraint has degraded. The
 *   constraint is extractive rather than coordinative because the security
 *   rationale serves simultaneously as justification and as mechanism to
 *   obscure the power transfer itself. Civilian populations in conflict zones
 *   authorized by executive order are the primary victims — bearing military
 *   consequences without deliberative input. The legislative system is a
 *   secondary victim — losing constraint capacity through precedent
 *   accumulation and information asymmetries. The executive security
 *   apparatus is the primary beneficiary — gaining speed of response at the
 *   cost of democratic oversight.
 *
 * KEY AGENTS:
 *   - Executive Branch Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains rapid unilateral decision-making authority; experiences constraint as coordination problem solution
 *   - Civilian Populations in Conflict Zones: Primary victim (powerless/trapped) — subject to military action authorized without their deliberative participation; no exit capacity
 *   - Legislative Branch and Congressional System: Secondary victim (moderate/constrained) — erosion of war powers check through precedent, information asymmetries, and political costs of reassertion
 *   - Constitutional Reform Coalitions: Organized agents (organized/constrained) — civil liberties groups, reform legislators attempting to reassert legislative authority; face political weakness but maintain institutional challenge capacity
 *   - War Powers Resolution Framework: Institutional actor (institutional/arbitrage) — formal constraint that persists performatively but lacks functional power; maintained through inertia
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees the extraction mechanism clearly: emergency pretext → precedent → bureaucratic expansion → inherited authority → normalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(executive_war_powers_expansion, 0.68).
domain_priors:suppression_score(executive_war_powers_expansion, 0.72).
domain_priors:theater_ratio(executive_war_powers_expansion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(executive_war_powers_expansion, extractiveness, 0.68).
narrative_ontology:constraint_metric(executive_war_powers_expansion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(executive_war_powers_expansion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(executive_war_powers_expansion, snare).
narrative_ontology:human_readable(executive_war_powers_expansion, "Executive War Powers Expansion").
narrative_ontology:topic_domain(executive_war_powers_expansion, "political/constitutional_law").

domain_priors:requires_active_enforcement(executive_war_powers_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(executive_war_powers_expansion, executive_branch_actors).
narrative_ontology:constraint_victim(executive_war_powers_expansion, legislative_constraint_systems).
narrative_ontology:constraint_victim(executive_war_powers_expansion, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(executive_war_powers_expansion, democratic_deliberation_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Civilians in conflict zones authorized by executive order bear full extraction with no exit capacity. No deliberative input into war decisions; no legal recourse under war powers authorization. Maximum suppression through fog of war and classification authorities. Trapped in zones where executive determinations override constitutional process.
constraint_indexing:constraint_classification(executive_war_powers_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE SYSTEM (SNARE) — Congress faces structural barriers to reasserting war powers: political costs of challenging sitting executive, information asymmetries (classified operations), and sunk institutional costs of prior delegations. Constrained rather than trapped, but extraction is severe — the legislative check has atrophied through disuse and executive precedent-setting. Each executive action shifts the Overton window, making prior congressional authority threshold appear abnormal.
constraint_indexing:constraint_classification(executive_war_powers_expansion, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE SECURITY APPARATUS (ROPE) — Experiences the constraint as coordination: rapid decision-making in security crises requires unified command authority. Interprets war powers expansion as solving a collective action problem (coordinating military response) rather than as extraction. Net beneficiary with arbitrage options — can reinterpret/narrow authorities in subsequent administrations if political winds shift.
constraint_indexing:constraint_classification(executive_war_powers_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM COALITIONS (TANGLED ROPE) — Organized agents (civil liberties groups, reform legislators, international law advocates) see genuine coordination problem (security response requires speed) but experience extraction (war powers authority applied to non-emergency contexts, classified operations prevent oversight). Constrained by political weakness and institutional inertia, but organized enough to create legal challenges and norm contestation. Mixed extraction-coordination experience.
constraint_indexing:constraint_classification(executive_war_powers_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WAR POWERS RESOLUTION (PITON) — The 1973 WPR is largely performative: it requires presidential notification of military actions and authorizes 60 days of operations before congressional approval. In practice, administrations route operations through emergency authorities, ally-coordinated actions, or counter-terrorism designations, bypassing the notification requirement. The formal framework persists (theater ratio = 0.58) but its functional constraint has degraded. Maintained through institutional inertia despite widespread acknowledgment that it fails to constrain executive action.
constraint_indexing:constraint_classification(executive_war_powers_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a universal/civilizational view, the constraint represents the asymmetric extraction from the democratic constitutional order itself. The separation of powers doctrine is being selectively interpreted to expand executive authority. This is not an immutable law but a constitutional interpretation that has progressively favored executive action over legislative deliberation. The analytical perspective sees the mechanism clearly: each crisis creates a precedent for executive action; bureaucratic expansion follows; subsequent administrations inherit expanded authority; crises become pretexts for normalization.
constraint_indexing:constraint_classification(executive_war_powers_expansion, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(executive_war_powers_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(executive_war_powers_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(executive_war_powers_expansion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(executive_war_powers_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(executive_war_powers_expansion, TR),
    TR >= 0.70.

:- end_tests(executive_war_powers_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The executive captures unilateral authority to initiate military operations with minimal legislative input. The extraction has grown over 50 years from baseline (0.35) to current (0.68) through precedent accumulation and bureaucratic expansion. Early constraints (authorization requirements, notification obligations) have been progressively reinterpreted as advisory rather than mandatory. Each administrations' exercise of authority becomes precedent for successor's broader claims. The mechanism is extraction disguised as coordination: security coordination requires speed, therefore legislative consultation is framed as impediment rather than constraint. Suppression (0.72): High. Multiple suppression mechanisms operate: classification authorities prevent public deliberation of threat assessments; fog of war prevents real-time accountability; intelligence protection prevents disclosure of operational details; emergency declarations fast-track decisions; allied intermediaries obscure operational scope. The suppression is structural — even well-intentioned officials face legal barriers to transparency. Theater ratio (0.58): Moderate-high. Formal war powers constraints persist (WPR, authorization language, notification procedures) but function performatively. Congress holds hearings but rarely exercises defunding power; executive provides briefings but classifies operational details; military commanders prepare contingency litigation but rarely challenge orders; public debates war powers in abstract while specific operations proceed under emergency authorities.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The executive security apparatus sees Rope (coordination solution to security crisis requiring speed). Congress sees Snare (loss of constitutional check with no capacity to recover it). Civilians see Snare (subject to military action without participation). Reform coalitions see Tangled Rope (genuine security coordination need exists but overlaid with extraction asymmetry). The WPR sees Piton (formal framework persists but lacks functional power). The analytical observer sees Snare (the democratic constitutional order is the victim; executive authority extraction is the mechanism). The perspectives do not converge because the agents occupy genuinely different structural positions: one gains speed and authority, others lose constraint capacity and deliberative participation. This is not a measurement ambiguity — it is real structural extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from who benefits and bears costs in this constraint structure. The executive security apparatus derives d ≈ 0.10 (beneficiary with arbitrage exit: can reinterpret authorities in successor administrations). Congress derives d ≈ 0.85 (victim with constrained exit: faces political costs and sunk institutional losses from reasserting authority, but retains theoretical capacity to defund or repeal). Civilians in conflict zones derive d ≈ 0.98 (victims with trapped exit: no legal recourse, no deliberative participation, no capacity to refuse military exposure). Constitutional reform coalitions derive d ≈ 0.65 (organized victims with constrained exit: can mount legal challenges and political pressure but lack institutional power to reverse precedent accumulation). The WPR framework derives d ≈ 0.05 (institutional actor maintaining coordination function for security response, though degraded). The analytical observer derives d ≈ 0.75 (observer position captures the extraction asymmetry: executive benefits, legislative system atrophies, civilians bear costs).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves mandatrophy by demonstrating genuine extraction asymmetry rather than coordination disagreement. The security coordination framing (executive perspective) is real — security response does require unified command — but this does not adjudicate the extraction question. The mandatrophy would arise if the constraint genuinely coordinated security response while minimizing extraction. But the empirical trajectory shows increasing extractiveness (0.35 → 0.68) and rising theater ratio (0.42 → 0.58) while security outcomes have not demonstrably improved. The rising theater suggests the coordination function is degrading even as extraction increases — suggesting that post-hoc security rhetoric justifies what is primarily a power transfer. The constraint is Snare rather than Tangled Rope because: (1) suppression is high (0.72) through classification and information asymmetries, (2) victims (legislature, civilians) have no exit capacity, (3) extraction benefits concentrate on executive actors with arbitrage exit, (4) theater ratio shows performative constraint persistence rather than functional coordination. A true Tangled Rope would show either declining theater (genuine coordination replacing ritual) or declining extractiveness (beneficiaries accepting constraint). This constraint shows neither.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_threshold_ambiguity,
    'What constitutes a genuine security emergency versus a manufactured or exaggerated threat justifying war powers expansion?',
    'Post-hoc analysis of threat severity claims versus actual security outcomes; comparison of predicted risks against materialized harms; historical analysis of declared emergencies that did not escalate',
    'If threshold is low: many questionable executive actions receive democratic cover. If threshold is high: genuine security responses face institutional delays. The ambiguity itself is the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_threshold_ambiguity, preference, 'Threshold for distinguishing genuine emergency from pretext').

omega_variable(
    congressional_delegation_intentionality,
    'To what degree have expansions of executive war powers resulted from explicit congressional delegation versus implicit acceptance through inaction and appropriations?',
    'Legislative history analysis; voting record correlation between war funding and dissent; tracking of explicit war authorization versus implicit funding authorization',
    'If explicit: congress retains modification capacity through defunding or repeal. If implicit: the constraint is more entrenched — congressional silence becomes interpreted as consent, and breaking silence becomes politically costly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_delegation_intentionality, empirical, 'Whether war powers expansion is explicit delegation or implicit acceptance').

omega_variable(
    classify_retaliation_authority_scope,
    'Does the executive authority to retaliate against armed attack extend to preemptive action, support of allied military operations, or only direct response to direct attack?',
    'Comparison of executive interpretation across administrations; court rulings on scope of retaliation authority; international law consensus on legitimate scope',
    'If scope is broad: extractiveness increases (executive can classify diverse actions as retaliation). If scope is narrow: most executive military actions fall outside authorized retaliation, requiring explicit congressional approval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classify_retaliation_authority_scope, conceptual, 'Scope of authorized retaliation authority').

omega_variable(
    ally_proxy_operation_accountability,
    'When the executive funds or directs military operations through allied states or private contractors, does the War Powers Resolution apply, or can the executive classify these as indirect support not requiring congressional authorization?',
    'Legal analysis of proxy operation accountability; tracking of operations classified as ally support versus direct executive action; congressional investigation scope',
    'If proxy operations fall outside WPR: extractiveness increases (executive can bypass authorization via intermediaries). If proxy operations are in scope: executive authority is more constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ally_proxy_operation_accountability, conceptual, 'Whether proxy military operations require war powers authorization').

omega_variable(
    democratic_norm_recovery_possibility,
    'Is the legislative constraint erosion reversible through explicit congressional reassertion, or has it become structurally entrenched through precedent and executive bureaucratic expansion?',
    'Analysis of prior constraint reassertions in other domains; feasibility study of legislative recapture mechanisms; modeling of political economy of constraint reversal',
    'If reversible: the snare may have a sunset path and could reclassify as tangled rope. If entrenched: the extraction is permanent within current institutional structure, and reversal requires constitutional amendment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_norm_recovery_possibility, preference, 'Whether legislative constraint erosion is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(executive_war_powers_expansion, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ewpe_tr_t0, executive_war_powers_expansion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ewpe_tr_t20, executive_war_powers_expansion, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ewpe_tr_t50, executive_war_powers_expansion, theater_ratio, 50, 0.58).
narrative_ontology:measurement(ewpe_tr_t10, executive_war_powers_expansion, theater_ratio, 10, 0.46).

% Extraction over time
narrative_ontology:measurement(ewpe_be_t0, executive_war_powers_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ewpe_be_t20, executive_war_powers_expansion, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ewpe_be_t50, executive_war_powers_expansion, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(ewpe_be_t10, executive_war_powers_expansion, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(executive_war_powers_expansion, enforcement_mechanism).
narrative_ontology:affects_constraint(executive_war_powers_expansion, separation_of_powers_erosion).
narrative_ontology:affects_constraint(executive_war_powers_expansion, presidential_emergency_declaration_scope).
narrative_ontology:affects_constraint(executive_war_powers_expansion, classified_information_constraint_on_oversight).

% DUAL FORMULATION NOTE:
% Executive war powers expansion is downstream of both separation-of-powers institutional design and emergency declaration authority. Each upstream constraint interacts with this one: erosion of separation of powers creates permissive environment for war powers expansion; emergency declaration scope determines which actions executive can classify as requiring speed; classification authority determines what information congress can access for oversight. The three constraint family members should be analyzed together for full contamination picture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(executive_war_powers_expansion, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
