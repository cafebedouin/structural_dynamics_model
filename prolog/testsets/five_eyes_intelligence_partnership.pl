% ============================================================================
% CONSTRAINT STORY: five_eyes_intelligence_partnership
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_five_eyes_intelligence_partnership, []).

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
 *   constraint_id: five_eyes_intelligence_partnership
 *   human_readable: Five Eyes Intelligence Partnership and Asymmetric Intelligence Extraction
 *   domain: geopolitical/intelligence_cooperation
 *
 * SUMMARY:
 *   The Five Eyes intelligence partnership (USA, UK, Canada, Australia, New
 *   Zealand) is a post-WWII multilateral intelligence coordination mechanism
 *   that has evolved from Cold War counter-intelligence cooperation into a
 *   global signals intelligence apparatus with substantial asymmetries in
 *   collection capacity, benefit distribution, and legal accountability. The
 *   constraint exhibits structural characteristics of Tangled Rope at the
 *   partnership level (genuine coordination function for threat assessment
 *   alongside asymmetric extraction) but classifies as Snare from the
 *   perspective of non-partner states and surveilled populations with no exit
 *   options. The partnership's extractiveness has increased over the 75-year
 *   interval as surveillance technology capability expanded and legal
 *   immunity frameworks hardened. Theater ratio reflects that significant
 *   partnership activity is now performative alliance maintenance rather than
 *   active threat coordination. The constraint is simultaneously a rational
 *   security mechanism (smaller nations genuinely benefit from shared
 *   intelligence and collective defense), an extractive power structure
 *   (asymmetric surveillance and collection favor the leading SIGINT power),
 *   and an institutional relic (Cold War justifications persist despite
 *   changed threat landscape). Mandatrophy is resolved by recognizing that
 *   Five Eyes is a genuine hybrid: it coordinates threat assessment for
 *   members while extracting surveillance capacity from non-members and
 *   domestic populations.
 *
 * KEY AGENTS:
 *   - Surveilled Populations / Non-Partner States: Primary victims (powerless/trapped) — bear full surveillance burden with no transparency, no exit, no ability to contest collection
 *   - Smaller Five Eyes Partners (Australia, Canada, New Zealand): Mixed position (organized/constrained) — benefit from threat intelligence and collective security but face constrained exit and asymmetric intelligence sharing
 *   - Domestic Democratic Publics: Mixed position (moderate/constrained) — benefit from coordinated security but face extraction through surveillance with minimal meaningful consent mechanisms due to classification
 *   - Leading SIGINT Power (USA primary, UK secondary): Primary beneficiary (institutional/arbitrage) — maximizes intelligence collection, maintains alliance legitimacy, has exit option of unilateral operation
 *   - Intelligence Agencies: Beneficiary actors (institutional/arbitrage) — extract institutional power and budget justification from partnership, have carve-out from legal liability
 *   - Privacy and Transparency Movements: Organized challengers (organized/constrained) — see sunset pathway through encryption adoption and norms change but face high suppression from classification and security deference
 *   - Post-WWII Alliance Architecture: Institutional inertia mechanism — maintains Five Eyes through bureaucratic momentum and classified treaty obligations regardless of contemporary threat assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(five_eyes_intelligence_partnership, 0.68).
domain_priors:suppression_score(five_eyes_intelligence_partnership, 0.78).
domain_priors:theater_ratio(five_eyes_intelligence_partnership, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(five_eyes_intelligence_partnership, extractiveness, 0.68).
narrative_ontology:constraint_metric(five_eyes_intelligence_partnership, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(five_eyes_intelligence_partnership, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(five_eyes_intelligence_partnership, tangled_rope).
narrative_ontology:human_readable(five_eyes_intelligence_partnership, "Five Eyes Intelligence Partnership and Asymmetric Intelligence Extraction").
narrative_ontology:topic_domain(five_eyes_intelligence_partnership, "geopolitical/intelligence_cooperation").

domain_priors:requires_active_enforcement(five_eyes_intelligence_partnership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(five_eyes_intelligence_partnership, leading_sigint_nation).
narrative_ontology:constraint_beneficiary(five_eyes_intelligence_partnership, intelligence_agencies).
narrative_ontology:constraint_victim(five_eyes_intelligence_partnership, citizen_privacy).
narrative_ontology:constraint_victim(five_eyes_intelligence_partnership, smaller_partner_nations).
narrative_ontology:constraint_victim(five_eyes_intelligence_partnership, global_signal_intercept_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED POPULATIONS (SNARE) — Citizens and non-Five Eyes states bear full surveillance burden with no exit, no transparency, and no ability to organize resistance. The partnership creates a global signal-intercept apparatus with minimal coordination benefit to targets. Maximum extraction, maximum suppression through classification and legal immunity.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALLER PARTNER INTELLIGENCE COMMUNITIES (TANGLED ROPE) — Australia, Canada, and New Zealand genuinely benefit from intelligence sharing and coordinated threat assessment, but face constrained exit: withdrawal from Five Eyes would isolate their security posture and damage critical alliances. They contribute data collection and analysis while receiving asymmetric benefits favoring the leading signals intelligence power. Active enforcement through NATO-linked security protocols maintains the asymmetry.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LEADING SIGINT POWER (ROPE) — The partnership functions as coordination for this actor: sharing threat intelligence with aligned partners, distributing collection burden, and maintaining legitimacy through multilateral framing. This agent experiences the constraint as enabling cooperative security coordination. Low effective extraction because the agent has arbitrage options (could exit and operate unilaterally, or expand to other intelligence partnerships). Net beneficiary position.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC DEMOCRATIC PUBLICS (TANGLED ROPE) — Citizens of Five Eyes nations benefit from coordinated security coordination and threat detection, but face constrained exit from surveillance architecture due to legal immunities, classification barriers, and security-clearance gatekeeping. The partnership both protects and extracts from the same populations. Active enforcement through legislation and court deference to executive power maintains the asymmetry.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALLIANCE ARCHITECTURE (PITON) — Five Eyes is institutionally inertial: originally justified as Cold War counter-intelligence coordination, it persists through classified treaty obligations and bureaucratic momentum despite changed threat landscape. The theater_ratio reflects that much of the partnership's activity is performative alliance maintenance rather than addressing contemporary threats. The institutional structure has become self-perpetuating.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVACY AND TRANSPARENCY MOVEMENTS (SCAFFOLD) — Organized civil society (transparency NGOs, investigative journalists, privacy advocates) sees Five Eyes as a temporary coordination failure with a potential sunset: encryption adoption, decentralized communication networks, and global data privacy norms (GDPR, similar frameworks) are building exit pathways that reduce intelligence collection feasibility. High suppression is tolerated because advocates see a realistic path to policy change through norms accumulation and technology-enabled exit.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, some intelligence coordination is inherent to any state system — nations must gather information about threats, and multilateral arrangements are more efficient than unilateral collection. This perspective frames Five Eyes as an immutable feature of strategic statecraft. However, the structural data reveals this as a false summit: the extractiveness (0.68) and suppression (0.78) are contingent on specific institutional choices (legal immunity, classification authority, surveillance technology adoption), not laws of nature.
constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(five_eyes_intelligence_partnership_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(five_eyes_intelligence_partnership, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(five_eyes_intelligence_partnership, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(five_eyes_intelligence_partnership, TR),
    TR >= 0.70.

:- end_tests(five_eyes_intelligence_partnership_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The partnership extracts substantial surveillance data and strategic advantage for leading partners while distributing coordination costs and legal liability to smaller partners and public surveillance targets. The value reflects that Five Eyes does provide genuine intelligence benefit to member nations (distinguishing it from pure extraction), but the benefit distribution is asymmetric and the surveillance scope exceeds what would be necessary for coordination alone. The 75-year trajectory (0.35→0.68) reflects technology-driven expansion of collection capacity and legal framework hardening that shifted the partnership from balanced coordination to asymmetric extraction. Suppression (0.78): Very high. Five Eyes operates with extreme suppression mechanisms: classification authority prevents public knowledge of scope and methods, legal immunities shield operators from liability, security-clearance gatekeeping prevents accountability scrutiny, and international law ambiguity enables collection without clear legal constraint. This is among the highest suppression scores because the constraint operates with minimal public contestation capability. Theater ratio (0.55): Moderate. Significant partnership activity is now performative — maintaining alliance legitimacy, justifying budget allocation, demonstrating NATO cohesion — rather than responding to genuine threat changes. But this is lower than pure piton theater because substantial actual intelligence production and threat assessment occurs within the partnership.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. Surveilled populations see pure extraction (Snare) with no coordination benefit to themselves — they are targets, not participants. Smaller partners see mixed coordination and extraction (Tangled Rope) — genuine intelligence sharing alongside asymmetric benefit and constrained exit. The leading SIGINT power sees pure coordination (Rope) — legitimate multilateral threat assessment enabling efficient intelligence distribution. Domestic publics see tangled mixed benefit (Tangled Rope) — they benefit from coordinated security but are also surveillance targets, and the classification prevents them from meaningfully consenting. The alliance architecture itself appears as institutional inertia (Piton) — maintained through bureaucratic momentum and classified justification rather than contemporary threat assessment. Privacy movements see a sunset pathway (Scaffold) — encryption and norms change will eventually reduce collection feasibility, making the current suppression temporary rather than permanent. The analytical observer risks naturalizing the partnership as immutable statecraft (Mountain) — 'all states must gather intelligence, multilateral coordination is more efficient' — but the structural data reveals this as false summit: the specific extractiveness level and suppression mechanism are contingent on technology choices, legal frameworks, and institutional design decisions, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural relationship to the intelligence extraction flow. The leading SIGINT power (institutional, arbitrage options) has low d ≈ 0.05-0.15: they experience the partnership as enabling, have exit options, and are net beneficiaries. Smaller partners (organized, constrained) have moderate d ≈ 0.55-0.65: they benefit from intelligence sharing but face constrained exit due to alliance dependence and security isolation risk. Domestic publics (moderate, constrained) have moderate-high d ≈ 0.65-0.75: they benefit from security coordination but are also extraction targets and cannot contest collection due to classification. Surveilled non-partner populations (powerless, trapped) have maximum d ≈ 0.95: they are pure targets with no exit or contestation capacity. The schema automatically derives these d values from the beneficiary/victim declarations and exit options; the commentary articulates the structural reasoning.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing Five Eyes as a genuine Tangled Rope hybrid with perspectival variation. The constraint BOTH coordinates threat intelligence AND extracts surveillance asymmetrically. The extraction exists not because of inefficiency but because asymmetric surveillance is a primary function of the partnership — it enables leading powers to maintain strategic intelligence advantage. The coordination function is real (smaller nations genuinely benefit from threat assessment and collective intelligence), but it coexists with systematic extraction that favors leading powers and targets non-partners. This dual function is not a sign of misclassification; it is the defining structure of the constraint. The mandatrophy resolves by accepting that Five Eyes is Tangled Rope from the partnership-member perspective (where both coordination and asymmetric extraction are visible) and Snare from the global surveilled population perspective (where only extraction is visible). The engine correctly computes this perspectival distribution: member nations see mixed classification; non-members see pure snare. The analytically synthetic position is that Five Eyes is a deliberately asymmetric coordination mechanism — it coordinates security for members while extracting intelligence from non-members — and this dual structure is not a bug but the actual design logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_immunity_boundary,
    'Where is the boundary between legitimate intelligence collection and illegal surveillance?',
    'International law development (ICC jurisdiction over surveillance crimes); domestic courts ruling on FISA authority limits; treaty renegotiation establishing surveillance boundaries',
    'If boundary moves inward (restricts surveillance): extractiveness drops to 0.35-0.45, reclassifies as Rope or Tangled Rope with low extraction. If boundary expands: extractiveness rises above 0.75, confirms Snare classification from most perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_immunity_boundary, conceptual, 'Legal and normative boundaries of legitimate intelligence collection').

omega_variable(
    threat_coordination_necessity,
    'How much of Five Eyes intelligence coordination is driven by genuine mutual threat assessment versus maintaining alliance leverage and strategic dominance?',
    'Comparative analysis of threat assessment outputs vs actual geopolitical outcomes; declassified review of coordination decisions; structural analysis of which partner countries'' threat assessments drive policy change',
    'If threat-driven (70%+): Rope classification is dominant, extractiveness drops to 0.30-0.40. If leverage-driven (50%+): Snare classification dominates for smaller partners, extractiveness rises to 0.72+.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_coordination_necessity, empirical, 'Proportion of coordination driven by genuine threat vs strategic leverage').

omega_variable(
    encryption_adoption_timeline,
    'Will widespread end-to-end encryption adoption meaningfully degrade Five Eyes collection capacity within 20 years?',
    'Technological adoption curves for encrypted communication; measurement of collection capability degradation in high-encryption adoption regions; intelligence community statements on encryption impact',
    'If yes (high confidence): scaffold perspective confirmed, sunset is structural, extractiveness will decline to 0.35-0.50 by 2045. If no: scaffold is aspirational, suppression becomes more extractive, reclassifies toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encryption_adoption_timeline, empirical, 'Whether encryption adoption will meaningfully reduce Five Eyes collection capacity').

omega_variable(
    asymmetric_intelligence_benefit,
    'Is intelligence sharing genuinely reciprocal or systematically asymmetric across the Five Eyes partnership?',
    'Comparison of classified intelligence production and sharing rates; analysis of which partner nations'' collections are distributed to others; declassified review of intelligence dissemination patterns',
    'If reciprocal: Rope classification for all partners becomes dominant, extractiveness drops 0.40-0.55. If asymmetric: Snare for smaller partners confirmed, extractiveness rises to 0.75+.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_intelligence_benefit, empirical, 'Actual reciprocity of intelligence sharing within Five Eyes').

omega_variable(
    democratic_accountability_gap,
    'Can democratic publics meaningfully consent to or contest Five Eyes surveillance if the scope and methods are classified?',
    'Comparative analysis of democratic consent mechanisms (parliamentary oversight, public debate feasibility) when key facts are classified; international norm development around classified surveillance accountability',
    'If meaningful accountability possible: extractiveness for domestic publics drops to 0.40-0.50, Tangled Rope with low suppression. If accountability impossible: extractiveness rises to 0.72+, reclassifies toward Snare for domestic populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_accountability_gap, conceptual, 'Whether classified surveillance permits democratic accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(five_eyes_intelligence_partnership, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(five_tr_t0, five_eyes_intelligence_partnership, theater_ratio, 0, 0.35).
narrative_ontology:measurement(five_tr_t25, five_eyes_intelligence_partnership, theater_ratio, 25, 0.45).
narrative_ontology:measurement(five_tr_t50, five_eyes_intelligence_partnership, theater_ratio, 50, 0.55).
narrative_ontology:measurement(five_tr_t75, five_eyes_intelligence_partnership, theater_ratio, 75, 0.62).

% Extraction over time
narrative_ontology:measurement(five_be_t0, five_eyes_intelligence_partnership, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(five_be_t25, five_eyes_intelligence_partnership, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(five_be_t50, five_eyes_intelligence_partnership, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(five_be_t75, five_eyes_intelligence_partnership, base_extractiveness, 75, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(five_eyes_intelligence_partnership, enforcement_mechanism).
narrative_ontology:affects_constraint(five_eyes_intelligence_partnership, domestic_surveillance_legal_framework).
narrative_ontology:affects_constraint(five_eyes_intelligence_partnership, international_intelligence_accountability).
narrative_ontology:affects_constraint(five_eyes_intelligence_partnership, encryption_policy_conflict).
narrative_ontology:affects_constraint(five_eyes_intelligence_partnership, alliance_dependence_asymmetry).

% DUAL FORMULATION NOTE:
% Five Eyes as a unified constraint decomposes into distinct structural stories at different observables. The partnership-level constraint (this story) examines the multilateral coordination and asymmetric intelligence extraction. Upstream constraints include specific SIGINT collection programs (NSA surveillance scope, UK GCHQ capacity) with higher extractiveness; downstream constraints include domestic legal frameworks that implement Five Eyes authority (FISA, Communications Act authorities) and international accountability gaps that Five Eyes exploitation creates. These form a constraint family linked by institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(five_eyes_intelligence_partnership, institutional, 0.08).
constraint_indexing:directionality_override(five_eyes_intelligence_partnership, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
