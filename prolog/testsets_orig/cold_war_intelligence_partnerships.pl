% ============================================================================
% CONSTRAINT STORY: cold_war_intelligence_partnerships
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cold_war_intelligence_partnerships, []).

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
 *   constraint_id: cold_war_intelligence_partnerships
 *   human_readable: Cold War Intelligence Partnerships: Coordination and Asymmetric Extraction
 *   domain: geopolitical/intelligence
 *
 * SUMMARY:
 *   Cold War intelligence partnerships between hegemonic (primarily US/UK)
 *   and junior allied powers created structural asymmetries in information
 *   flow, operational control, and strategic benefit. While genuine
 *   coordination functions existed — pooling resources against shared Soviet
 *   threat, distributing early warning systems, coordinating espionage
 *   operations — the partnerships also embedded systematic extraction:
 *   hegemonic agencies gained intelligence on junior partners' internal
 *   affairs, conducted operations that primarily served hegemonic interests,
 *   and maintained subordinate intelligence services through dependency on
 *   advanced technical capability and threat assessment. The constraint
 *   exhibits hybrid Tangled Rope characteristics from institutional
 *   perspectives and Snare characteristics from operational agents'
 *   perspectives. Post-Cold War, the extractive mechanisms persisted through
 *   institutional inertia despite the functional purpose (containing Soviet
 *   expansion) becoming obsolete.
 *
 * KEY AGENTS:
 *   - Hegemonic Intelligence Apparatus (US/UK/GCHQ): Primary beneficiary (institutional/arbitrage) — gains access to foreign territories, operational recruitment, signals intelligence, strategic intelligence; can shift alliances or operate unilaterally
 *   - Junior Partner Intelligence Services (Canada, Australia, Five Eyes partners, NATO liaison): Secondary beneficiary and victim (moderate/constrained) — gains threat intelligence and security capability but loses intelligence sovereignty and becomes vulnerable to hegemonic espionage
 *   - Allied State Governments: Organized actors (organized/constrained) — gain security guarantees but constrained by alliance structures and unable to independently verify hegemonic intentions
 *   - Operational Field Agents: Primary victims (powerless/trapped) — execute high-risk operations designed primarily to benefit hegemonic apparatus; face maximal extraction with no exit path
 *   - Cold War Containment Framework: Institutional structure (institutional/arbitrage) — persists through inertia post-Cold War despite functional obsolescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cold_war_intelligence_partnerships, 0.58).
domain_priors:suppression_score(cold_war_intelligence_partnerships, 0.68).
domain_priors:theater_ratio(cold_war_intelligence_partnerships, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_war_intelligence_partnerships, extractiveness, 0.58).
narrative_ontology:constraint_metric(cold_war_intelligence_partnerships, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cold_war_intelligence_partnerships, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cold_war_intelligence_partnerships, tangled_rope).
narrative_ontology:human_readable(cold_war_intelligence_partnerships, "Cold War Intelligence Partnerships: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(cold_war_intelligence_partnerships, "geopolitical/intelligence").

domain_priors:requires_active_enforcement(cold_war_intelligence_partnerships).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cold_war_intelligence_partnerships, hegemonic_intelligence_agencies).
narrative_ontology:constraint_beneficiary(cold_war_intelligence_partnerships, western_allied_power_bloc).
narrative_ontology:constraint_victim(cold_war_intelligence_partnerships, junior_partner_sovereignty).
narrative_ontology:constraint_victim(cold_war_intelligence_partnerships, operational_agents_in_field).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL FIELD AGENT (SNARE) — Intelligence officers deployed under allied partnerships face maximal extraction with minimal exit. Their careers, security, and survival depend on partnership structures they cannot modify. They perform high-risk operations that primarily benefit hegemonic intelligence apparatus. Exit means career destruction or defection. No coordination benefit flows to them — they are pure instrumentalities.
constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUNIOR PARTNER INTELLIGENCE SERVICE (TANGLED ROPE) — Genuine coordination function: intelligence sharing improves security capacity of smaller states against shared adversary (USSR). Real benefits in operational capability and threat intelligence. But asymmetric extraction: hegemonic partner extracts strategic intelligence, operational methods, and intelligence on junior partner's own internal affairs under guise of alliance. Constrained exit — breaking partnership invites military/economic retaliation and leaves state exposed to adversary.
constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEGEMONIC INTELLIGENCE APPARATUS (ROPE) — Experiences partnership as pure coordination mechanism: pooling resources, gaining access to junior partner's territory, recruiting agents in allied nations, obtaining signals intelligence from geographic positions inaccessible otherwise. Net beneficiary with arbitrage options — can shift alliance to different partner or operate unilaterally. Extraction flows toward this agent; they perceive no asymmetry.
constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED STATE GOVERNMENT (TANGLED ROPE) — Genuine coordination: security guarantees, military aid, intelligence on adversary intentions. But organized at state level and can perceive extraction: loss of intelligence sovereignty, vulnerability to domestic espionage by nominal ally, constraints on independent foreign policy. Constrained exit — NATO/alliance structures create institutional path-dependence; breaking partnership invokes economic sanctions, military abandonment, or institutional exclusion.
constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLD WAR CONTAINMENT FRAMEWORK (PITON) — Post-Cold War, the institutional mechanisms of intelligence partnerships persist largely through inertia. Five Eyes, NATO intelligence sharing, liaison offices continue structured relationships designed for bipolar competition. But the organizing principle (containing Soviet expansion) has atrophied. Relationships persist through: intelligence-industrial institutional investment, career paths built on partnership mechanisms, classified information exchange cultures, liaison office budgets. Theater is high (formal liaison procedures, classification protocols, regular council meetings) relative to functional verification that partnership still serves original containment purpose. Theater ratio reflects performative maintenance of Cold War structures into 21st century.
constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY VIEW (MOUNTAIN) — From a civilizational perspective, intelligence partnerships between unequal powers appear inevitable: asymmetric states facing common threats must pool intelligence to survive. The extraction is treated as inherent to asymmetric coordination — no alternative exists that preserves both security and equality. This perspective risks naturalizing what is actually contingent institutional arrangement driven by Cold War power dynamics.
constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_war_intelligence_partnerships_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cold_war_intelligence_partnerships, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cold_war_intelligence_partnerships, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cold_war_intelligence_partnerships, TR),
    TR >= 0.70.

:- end_tests(cold_war_intelligence_partnerships_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Partnership demonstrably extracts strategic intelligence, operational access, and intelligence on junior partners themselves. But the extraction is not maximal (0.80+) because genuine security benefits flow to junior partners from shared threat intelligence and technical capability access. The constraint coordinates real security interests alongside asymmetric extraction. Measurement trajectory shows extractiveness rising from 0.42 (early Cold War, genuine threat coordination justified asymmetry) to 0.68 (post-Cold War, same extraction mechanisms continue without functional justification). Suppression (0.68): High. Junior partners face multiple suppression mechanisms: military dependency (cannot defend without hegemonic support), economic sanctions risk (breaking partnership invokes NATO/alliance retaliation), intelligence dependency (lack technical capability for independent operations), and geopolitical vulnerability (abandoning alliance invites adversary expansion or regional competitor advantage). Theater ratio (0.45): Moderate-low. Partnership mechanisms are largely functional rather than performative during Cold War era. Intelligence sharing has genuine security value; liaison offices conduct real operations. Theater increases post-Cold War as functional justification weakens but structures persist.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence driven by power asymmetry and exit options. The hegemonic apparatus (institutional/arbitrage) perceives Rope — pure coordination solving collective security problem. The junior partner state (organized/constrained) perceives Tangled Rope — genuine coordination mixed with vulnerability to extraction and constrained exit. The operational field agent (powerless/trapped) perceives Snare — pure extraction with no exit and no coordination benefit. The civilizational analytical observer risks perceiving Mountain — treating structural power asymmetry as inevitable natural law rather than contingent institutional arrangement. The perspectival gap reveals how the same constraint structure produces radically different experienced extractiveness (chi) depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from agent position relative to extraction flow and exit options. Hegemonic apparatus: beneficiary + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.01 → negative/minimal χ (beneficiary experiences low effective extraction). Junior partner state: victim + constrained exit → d ≈ 0.70 → f(d) ≈ 1.08 → high χ (victim experiences significant extraction). Operational agent: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum χ (trapped agent experiences maximal extraction). The organized coalition (junior partner government) sits between: d ≈ 0.55 → f(d) ≈ 0.75 → moderate χ. The piton classification emerges from theater_ratio persistence (0.45 maintained post-Cold War) despite functional purpose atrophy — institutional inertia sustaining coordination mechanisms designed for obsolete threat environment.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY STRUCTURE: This story represents the interpersonal/institutional hybrid case where the same partnership constraint decomposes into multiple structurally distinct mechanisms: (1) Operational security coordination (ε ≈ 0.25, genuine Rope), (2) Intelligence extraction asymmetry (ε ≈ 0.68, Snare), (3) Institutional dependency lock (ε ≈ 0.45, Tangled Rope). These should be separate stories linked via network.affects_constraints, but are presented here as a single story because the partnership label conflates them. The mandatrophy is resolved by acknowledging that 'Cold War intelligence partnership' is not a single constraint but a bundle of asymmetric power relations wearing a coordination label. The classification does not answer 'what is this partnership?' but rather 'from whose perspective and measuring what?' From the hegemonic apparatus: Rope. From the junior partner government: Tangled Rope. From the field agent: Snare. The analytical observer's Mountain is a false summit — treating the power asymmetry as inevitable when it is actually contingent on Cold War institutional structures that no longer provide functional justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_versus_coerced_partnership,
    'Do junior partner states genuinely consent to intelligence partnerships, or does consent collapse under security threat credibility?',
    'Historical counterfactual analysis: what partnership terms would junior partners negotiate without military/economic coercion? Archival evidence of coercive pressure during negotiation. Post-Cold War partnership renegotiation patterns.',
    'If genuinely voluntary: partnership may be Rope with modest extraction. If coerced: extraction mechanisms are structural suppression, not negotiated coordination; classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_versus_coerced_partnership, empirical, 'Whether partnership consent survives removal of military threat').

omega_variable(
    intelligence_benefit_asymmetry,
    'What fraction of shared intelligence actually benefits junior partners vs. primarily serves hegemonic strategic interests?',
    'Declassified intelligence summaries; comparative analysis of threat intelligence provided to junior partners vs. strategic intelligence extracted from them. Field operation casualty patterns comparing operations benefiting junior vs. hegemonic interests.',
    'If benefit roughly symmetric: Tangled Rope classification sustained. If heavily skewed toward hegemonic benefit: classification shifts to Snare for junior partner perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intelligence_benefit_asymmetry, empirical, 'Distribution of intelligence benefits across partnership').

omega_variable(
    espionage_within_alliance,
    'Does hegemonic intelligence apparatus systematically spy on junior partners under cover of partnership liaison relationships?',
    'Snowden revelations, declassified counterintelligence reports, admission patterns when espionage is publicly revealed. Correlation between liaison office locations and signals intelligence collection targets.',
    'If systematic: suppression mechanism is enhanced (junior partners are doubly trapped — cannot exit partnership without losing security, cannot trust partner not to exploit intelligence access). Extraction becomes pure rather than mixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(espionage_within_alliance, empirical, 'Systematic espionage against nominal allies').

omega_variable(
    cold_war_necessity_sunset,
    'When did the functional purpose (containing Soviet expansion) become obsolete, and why have partnership structures persisted?',
    'Temporal analysis: compare partnership extraction/coordination balance pre-1989 vs. post-1991. Institutional budget tracking for partnership maintenance. Leadership interviews on partnership rationale post-Cold War.',
    'If structures persist primarily through inertia: theater_ratio should be higher and coordination_benefit lower in post-Cold War period; classification may degrade to Piton for hegemonic view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cold_war_necessity_sunset, empirical, 'Functional obsolescence of Cold War partnership justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cold_war_intelligence_partnerships, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cold_tr_t0, cold_war_intelligence_partnerships, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cold_tr_t20, cold_war_intelligence_partnerships, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cold_tr_t40, cold_war_intelligence_partnerships, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(cold_be_t0, cold_war_intelligence_partnerships, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cold_be_t20, cold_war_intelligence_partnerships, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cold_be_t40, cold_war_intelligence_partnerships, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cold_war_intelligence_partnerships, enforcement_mechanism).
narrative_ontology:affects_constraint(cold_war_intelligence_partnerships, nato_alliance_dependency).
narrative_ontology:affects_constraint(cold_war_intelligence_partnerships, five_eyes_intelligence_asymmetry).
narrative_ontology:affects_constraint(cold_war_intelligence_partnerships, signals_intelligence_technical_dependency).

% DUAL FORMULATION NOTE:
% Cold War intelligence partnerships are downstream of the broader bipolar geopolitical structure (containment doctrine, Soviet threat credibility) but represent a distinct structural constraint. The constraint persists post-Cold War despite upstream threat atrophy, indicating institutional inertia rather than ongoing functional justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cold_war_intelligence_partnerships, organized, 0.55).
constraint_indexing:directionality_override(cold_war_intelligence_partnerships, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
