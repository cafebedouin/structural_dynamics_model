% ============================================================================
% CONSTRAINT STORY: humanitarian_aid_delivery_gaza
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humanitarian_aid_delivery_gaza, []).

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
 *   constraint_id: humanitarian_aid_delivery_gaza
 *   human_readable: Humanitarian Aid Delivery Constraint in Gaza
 *   domain: humanitarian_affairs/geopolitical_conflict
 *
 * SUMMARY:
 *   Humanitarian aid delivery in Gaza operates under a constraint structure
 *   that combines genuine coordination challenges (logistics, resource
 *   scarcity, population dispersal) with extractive mechanisms (political
 *   conditioning of aid, supply ceilings below minimum requirements, use of
 *   aid access to enforce compliance). The constraint exhibits
 *   characteristics of both Tangled Rope and Snare depending on the
 *   observer's structural position. International humanitarian actors
 *   experience coordination problems to solve; Gaza civilians experience a
 *   control mechanism. The constraint's theater ratio (0.65) reflects the
 *   performative apparatus of humanitarian access negotiations, which
 *   maintain the institutional fiction of neutral humanitarian assistance
 *   while operating within a framework where aid is deliberately weaponized.
 *   Over the measurement interval, both theater_ratio and extractiveness have
 *   increased, indicating drift from coordination toward pure extraction
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Gaza Civilian Population: Primary victim (powerless/trapped) — bears maximum extraction; no exit options or alternative supply sources
 *   - Local Health and Humanitarian Workers: Secondary victims (moderate/constrained) — constrained by employment and security dependence; benefit from aid operations but forced to facilitate control mechanisms
 *   - International NGOs and UN Agencies: Primary beneficiaries (institutional/arbitrage) — benefit from funding and operational legitimacy; have exit options and international protections
 *   - Regional Host States: Secondary beneficiaries (organized/constrained) — bear coordination burden while extracting political leverage; constrained by geopolitical dependence
 *   - Checkpoint Gatekeepers: Tertiary beneficiaries (organized/mobile) — extract through conditionality and diversion; have discretion to modify access
 *   - International Humanitarian System: Institutional actor (institutional/arbitrage) — maintains performative apparatus; benefits from legitimacy while functionality atrophies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humanitarian_aid_delivery_gaza, 0.58).
domain_priors:suppression_score(humanitarian_aid_delivery_gaza, 0.72).
domain_priors:theater_ratio(humanitarian_aid_delivery_gaza, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humanitarian_aid_delivery_gaza, extractiveness, 0.58).
narrative_ontology:constraint_metric(humanitarian_aid_delivery_gaza, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(humanitarian_aid_delivery_gaza, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humanitarian_aid_delivery_gaza, tangled_rope).
narrative_ontology:human_readable(humanitarian_aid_delivery_gaza, "Humanitarian Aid Delivery Constraint in Gaza").
narrative_ontology:topic_domain(humanitarian_aid_delivery_gaza, "humanitarian_affairs/geopolitical_conflict").

domain_priors:requires_active_enforcement(humanitarian_aid_delivery_gaza).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humanitarian_aid_delivery_gaza, aid_logistics_intermediaries).
narrative_ontology:constraint_beneficiary(humanitarian_aid_delivery_gaza, international_ngos).
narrative_ontology:constraint_beneficiary(humanitarian_aid_delivery_gaza, checkpoint_gatekeepers).
narrative_ontology:constraint_victim(humanitarian_aid_delivery_gaza, gaza_civilian_population).
narrative_ontology:constraint_victim(humanitarian_aid_delivery_gaza, humanitarian_sector_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAZA CIVILIANS (SNARE) — Trapped by geographic confinement and total dependency on aid flows. No exit options; cannot leave territory or secure independent supply chains. Bears maximum extraction: aid is withheld as political leverage, humanitarian access is conditioned on military objectives, supply ceilings are set below survival requirements. Experiences aid as coercive control mechanism rather than assistance.
constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL HUMANITARIAN WORKERS (TANGLED ROPE) — Constrained by employment dependency, visa restrictions, and security threat. Benefit from aid operations through employment and organizational resources, but also experience extraction: forced to facilitate population control through aid distribution mechanisms, bear security risk disproportionately, and face coercion to prioritize politically favored distribution. Mixed coordination (sustaining healthcare systems) and extraction (using aid as control infrastructure).
constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL NGOS/UN AGENCIES (ROPE) — Experience aid delivery as coordination function: mobilizing logistics, allocating scarce resources, establishing equity protocols. Benefit from funding legitimacy tied to emergency response and humanitarian mandate. Have exit options (withdraw operations, relocate programs) and international legal protections. Extraction is minimal from their perspective — they see themselves as solving coordination problems despite constraints.
constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL HOST STATES (TANGLED ROPE) — Bear coordination burden (refugee camps, overflow hospitals, border logistics) while experiencing extraction (donor conditionality, political leverage, security externalities from conflict escalation). Organized power but constrained by geopolitical dependence on conflict parties and international donors. Genuine coordination function mixed with asymmetric extraction of resources and sovereignty.
constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN SYSTEM (PITON) — The formal apparatus of neutral humanitarian access, impartiality principles, and coordination mechanisms has substantially atrophied. Performance metrics (tonnage delivered, beneficiaries reached) dominate actual impact measures; theater of humanitarian neutrality persists despite deep politicization. The system maintains legitimacy through procedural compliance rather than functional effectiveness. High theater ratio reflects ritualized access negotiations that prioritize demonstrating neutrality over securing actual need-based distribution.
constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From structural analysis, the aid constraint functions as a coercive control mechanism layered atop humanitarian necessity. The conditioning of survival resources on political compliance, the deliberate creation of supply ceilings below minimum caloric requirements, and the use of aid access to enforce population movement and registration constitute extraction disguised as assistance. Unlike the mountain false summit, the snare classification is justified by structural data: the constraint exists to extract compliance, not to solve a genuine coordination problem.
constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humanitarian_aid_delivery_gaza_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humanitarian_aid_delivery_gaza, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(humanitarian_aid_delivery_gaza, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(humanitarian_aid_delivery_gaza, TR),
    TR >= 0.70.

:- end_tests(humanitarian_aid_delivery_gaza_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant extraction through (1) deliberate supply ceilings below minimum survival requirements, (2) conditioning of aid on political compliance and population movement, (3) diversion and gatekeeping, (4) use of aid access to enforce registration systems. However, the extraction is not maximal (0.72+) because some aid does reach populations through multiple pathways and humanitarian organizations retain some functional independence. The interval trajectory (0.42→0.58) reflects increasing sophistication of extraction mechanisms and erosion of humanitarian space. Suppression (0.72): Very high. Barriers to independent supply include geographic confinement, blockade, prohibition of imports, requirement for aid to flow through authorized checkpoints, documentation requirements that delay delivery, and military objectives that take precedence over humanitarian access. The suppression is not total (0.85+) because some informal supply channels persist and occasional breaches occur, but formal suppression is near-total. Theater ratio (0.65): Moderate-high. Humanitarian access operates through highly formalized negotiation cycles (coordination meetings, approval processes, documentation) whose primary function is increasingly to legitimize predetermined access decisions rather than to facilitate genuine humanitarian need assessment. The formal apparatus persists because withdrawal would delegitimize the system entirely, but its actual impact on access decisions is minimal.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival gap: international humanitarian organizations and conflict parties themselves perceive the system as solving coordination problems (resource allocation, logistics, equity); Gaza civilians experience the same system as a coercive control mechanism that withholds survival resources contingent on compliance. The international NGO perspective as Rope is their genuine experience — they are solving real logistical problems — but this is coherent only from an extractive framing: the logistics exist specifically because access is restricted, and NGOs function as administrators of the restriction. The piton perspective (international humanitarian system) captures the institutional dimension: the formal apparatus persists through inertia and legitimacy needs even as its functional coordination role atrophies. The analytical observer's snare classification correctly identifies the primary structural function (extraction of compliance through aid conditionality) while acknowledging the mixed reality: some genuine coordination occurs within an extractive frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position relative to the aid flow: (1) Trapped civilians with no exit: high d, experience maximum f(d), full snare; (2) Constrained local workers: moderate d (they benefit from employment but face coercion), tangled rope; (3) International NGOs with arbitrage options: low d (they benefit and can withdraw), rope; (4) Regional states: moderate-high d (constrained by geopolitical dependence despite organized power), tangled rope; (5) Analytical observer: high d (sees extraction as primary mechanism), snare. The directionality overrides are not needed — the beneficiary/victim declarations and exit options produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL RESOLUTION: The mandatrophy is resolved by recognizing that this constraint is simultaneously coordination and extraction at different structural levels. At the operational level (NGO logistics), the constraint solves genuine coordination problems — allocating scarce resources, coordinating cross-border logistics, managing supply chains. At the strategic level (geopolitical power), the constraint implements extraction — using aid access to enforce compliance, maintain population control, and extract political concessions. The Tangled Rope classification correctly captures this duality: genuine coordination function (beneficiaries: aid logistics intermediaries, international NGOs) coexists with asymmetric extraction (victims: civilians, humanitarian sector credibility). The Snare classifications from powerless and analytical perspectives reveal the extractive function that the Rope perspective obscures. The mandatrophy is resolved not by declaring one type correct but by recognizing that the constraint is genuinely tangled: it coordinates AND extracts, and the perspectival gap reveals which structural function dominates from each position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_necessity_vs_political_conditioning,
    'Are observed aid delivery constraints caused by logistical bottlenecks inherent to emergency response, or by deliberate political conditioning of survival resources?',
    'Comparative analysis: (1) documented access demands vs. granted access across aid categories; (2) supply ceiling justifications vs. minimum survival requirements by commodity; (3) correlation between aid denial and geopolitical demands; (4) internal communications from checkpoint authorities regarding conditionality.',
    'If logistical: constraint reclassifies as Rope/Scaffold — primarily coordination function with extraction as byproduct. If political: classification stands as Snare/Tangled Rope — extraction is primary mechanism, humanitarian function is theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_necessity_vs_political_conditioning, empirical, 'Whether aid constraints reflect logistics or deliberate political conditioning').

omega_variable(
    checkpoint_gatekeeping_extraction,
    'What fraction of aid delivery delay/diversion occurs due to humanitarian logistics coordination vs. institutional gatekeeping designed to extract political compliance or commercial profit?',
    'Tracking data: (1) aid-in-motion vs. aid-at-destination timelines for different checkpoint configurations; (2) cross-border comparative analysis (different checkpoints, same logistics challenges); (3) pattern analysis of conditional demands vs. operational necessity; (4) documentation of unofficial fees, bribes, or prerequisite conditions.',
    'If primarily logistics: suppression metric should be lower (0.40-0.50 range) and snare classification becomes tangled_rope. If primarily extraction: suppression metric justified and snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(checkpoint_gatekeeping_extraction, empirical, 'Fraction of aid delays attributable to gatekeeping extraction vs. logistics').

omega_variable(
    ngo_complicity_in_control_infrastructure,
    'To what degree do international NGOs function as independent aid providers vs. as administrative extensions of conflict parties'' population control infrastructure?',
    'Organizational independence analysis: (1) funding source tracking and conditional funding restrictions; (2) distribution priorities set independently vs. imposed; (3) beneficiary targeting based on humanitarian need vs. geopolitical/military criteria; (4) ability to refuse operations without organizational collapse.',
    'If independent: NGO perspective as Rope is accurate. If substantially captured: NGO perspective should reclassify toward Tangled Rope, and complicity omega should be created.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ngo_complicity_in_control_infrastructure, empirical, 'NGO organizational independence vs. incorporation into control infrastructure').

omega_variable(
    humanitarian_theater_functionality,
    'Does the performative apparatus of humanitarian access (negotiation cycles, documentation, coordination meetings, neutral messaging) serve genuine coordination function or primarily legitimize extraction?',
    'Process effectiveness analysis: (1) outcome changes attributable to formal negotiation vs. enforcement changes; (2) correlation between theater intensity (frequency of meetings, documentation layers) and actual access improvement; (3) counterfactual: what happens if theater is reduced (coordination fails vs. extraction continues unimpeded)?',
    'If functional: theater_ratio should be lower and Piton classification overstated. If primarily legitimizing: theater_ratio validated and Piton perspective confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_theater_functionality, empirical, 'Whether humanitarian access theater serves coordination or legitimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humanitarian_aid_delivery_gaza, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aid_gaza_tr_t0, humanitarian_aid_delivery_gaza, theater_ratio, 0, 0.45).
narrative_ontology:measurement(aid_gaza_tr_t3, humanitarian_aid_delivery_gaza, theater_ratio, 3, 0.58).
narrative_ontology:measurement(aid_gaza_tr_t6, humanitarian_aid_delivery_gaza, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(aid_gaza_be_t0, humanitarian_aid_delivery_gaza, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aid_gaza_be_t3, humanitarian_aid_delivery_gaza, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(aid_gaza_be_t6, humanitarian_aid_delivery_gaza, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humanitarian_aid_delivery_gaza, resource_allocation).
narrative_ontology:affects_constraint(humanitarian_aid_delivery_gaza, gaza_blockade_enforcement).
narrative_ontology:affects_constraint(humanitarian_aid_delivery_gaza, checkpoint_control_systems).
narrative_ontology:affects_constraint(humanitarian_aid_delivery_gaza, international_aid_funding_conditionality).

% DUAL FORMULATION NOTE:
% The aid delivery constraint is downstream of multiple structural constraints: the blockade that creates scarcity (upstream), checkpoint control systems that enforce gatekeeping (peer), and international funding conditionality that incentivizes NGO compliance (upstream). The aid constraint represents the operational manifestation of strategic extraction mechanisms. Separate stories model the blockade (supply restriction) and checkpoint enforcement (access control); this story models the aid delivery mechanism that instantiates both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humanitarian_aid_delivery_gaza, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
