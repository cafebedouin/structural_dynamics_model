% ============================================================================
% CONSTRAINT STORY: voting_system_integrity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voting_system_integrity, []).

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
 *   constraint_id: voting_system_integrity
 *   human_readable: Voting System Integrity as Institutional Constraint
 *   domain: political/democratic_governance
 *
 * SUMMARY:
 *   Voting system integrity presents as a coordination mechanism for
 *   democratic governance but functions simultaneously as an extraction and
 *   suppression apparatus. The constraint exhibits structural tension between
 *   legitimate verification needs and incumbent political advantage through
 *   voter suppression, registration barriers, and proprietary election
 *   technology. From the perspective of election administration authorities
 *   and technology vendors, the integrity constraint is a pure coordination
 *   mechanism enabling distributed voting and centralized result aggregation.
 *   From the perspective of marginalized voters, disenfranchised groups, and
 *   excluded communities, the same constraint operates as a snare:
 *   identification requirements, registration deadlines, provisional ballot
 *   uncertainty, and strategic polling place closures create systematic
 *   barriers to participation. Voting rights organizations perceive a tangled
 *   hybrid: genuine coordination function (enabling mass democratic
 *   participation) alongside extractive enforcement mechanisms (voter purges,
 *   provisional ballot disqualification, ID requirements concentrating on
 *   specific demographics). The legacy voting infrastructure exhibits high
 *   theater — security audits, chain-of-custody procedures, machine
 *   certification, and post-election audits generate confidence signals
 *   rather than technical verification. The analytical observer risks
 *   naturalizing contingent institutional choices (centralized proprietary
 *   systems, closed-source counting software) as immutable features of
 *   electoral systems, when alternative architectures (voter-verifiable
 *   ballots, open-source systems, distributed audit mechanisms) exist.
 *
 * KEY AGENTS:
 *   - Disenfranchised Voters: Primary victims (powerless/trapped) — bear full suppression cost through identification barriers, registration deadlines, provisional ballot uncertainty, and poll closures
 *   - Marginalized Communities: Primary victims (powerless/trapped) — systematically targeted by voter purges, polling place reduction, language access failures, and demographic-targeted ID requirements
 *   - Election Administration Authorities: Primary beneficiaries (institutional/arbitrage) — control voting procedures, technology selection, enforcement, and timing; benefit from centralized authority and standardized systems
 *   - Incumbent Political Parties: Secondary beneficiaries (institutional/arbitrage) — benefit from suppression mechanisms that concentrate on opposing demographics; use administrative authority to enforce partisan advantage
 *   - Voting Technology Vendors: Secondary beneficiaries (institutional/arbitrage) — profit from proprietary systems, create vendor lock-in, benefit from security theater requiring regular system upgrades
 *   - Election Observers/Poll Watchers: Constrained coordinators (moderate/constrained) — enable distributed verification but face access restrictions and lack enforcement authority
 *   - Voting Rights Organizations: Organized victims (organized/constrained) — provide genuine coordination and monitoring function but constrained by litigation barriers and slow institutional reform
 *   - Legacy Voting Infrastructure: Institutional actor (institutional/arbitrage) — persists through inertia despite vulnerabilities; generates theater through procedural compliance rather than technical security
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voting_system_integrity, 0.58).
domain_priors:suppression_score(voting_system_integrity, 0.65).
domain_priors:theater_ratio(voting_system_integrity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voting_system_integrity, extractiveness, 0.58).
narrative_ontology:constraint_metric(voting_system_integrity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(voting_system_integrity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voting_system_integrity, tangled_rope).
narrative_ontology:human_readable(voting_system_integrity, "Voting System Integrity as Institutional Constraint").
narrative_ontology:topic_domain(voting_system_integrity, "political/democratic_governance").

domain_priors:requires_active_enforcement(voting_system_integrity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voting_system_integrity, electoral_administration).
narrative_ontology:constraint_beneficiary(voting_system_integrity, incumbent_political_parties).
narrative_ontology:constraint_beneficiary(voting_system_integrity, centralized_vote_counting_systems).
narrative_ontology:constraint_victim(voting_system_integrity, voter_confidence).
narrative_ontology:constraint_victim(voting_system_integrity, election_accessibility).
narrative_ontology:constraint_victim(voting_system_integrity, electoral_contestability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (SNARE) — Trapped by identification requirements, registration deadlines, provisional ballot uncertainty, and poll closures. Cannot exit the voting system; bears full suppression cost through barriers to participation and inability to verify ballot integrity independently. No alternative mechanism to ensure their vote counts.
constraint_indexing:constraint_classification(voting_system_integrity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY (SNARE) — Structurally trapped by geographic isolation, transportation barriers, language access failures, and targeted voter suppression. Suppression mechanisms (voter purges, polling place reduction, ID requirements) concentrate on this group. No credible exit option; no alternative mechanism for political representation.
constraint_indexing:constraint_classification(voting_system_integrity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ELECTION OBSERVER (TANGLED ROPE) — Constrained by access restrictions, observation-only mandates, and lack of enforcement authority, but also benefits from coordination function: observers enable distributed verification and reduce unilateral manipulation. Mixed experience — genuine coordination value alongside significant constraints on agency.
constraint_indexing:constraint_classification(voting_system_integrity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTION ADMINISTRATION AUTHORITY (ROPE) — Benefits from centralized authority and standardized procedures. Experiences the integrity constraint as coordination: ballot design, voter registration, poll worker training all serve legitimate coordination functions. Net beneficiary with significant arbitrage options (can modify procedures, change technology, adjust timelines). Low experienced extraction.
constraint_indexing:constraint_classification(voting_system_integrity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: VOTING RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized actors (ACLU, League of Women Voters, election integrity advocates) provide coordination function through monitoring, litigation, and norm-building. But constrained by legal barriers, funding limitations, and slow institutional change. Genuine coordination benefit alongside extraction through captured/incremental reform pathways.
constraint_indexing:constraint_classification(voting_system_integrity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY VOTING INFRASTRUCTURE (PITON) — Outdated mechanical and electronic systems persist through institutional inertia despite known vulnerabilities. Theater ratio high: security audits, chain-of-custody procedures, machine certification, and post-election audits are largely performative — they generate confidence signals rather than genuine technical verification. Systems maintained because alternatives haven't fully replaced them, not because they function optimally.
constraint_indexing:constraint_classification(voting_system_integrity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some verification gap between voting and outcome certification is inherent to distributed systems: the gap between individual ballot casting and aggregate result announcement is a structural feature of any vote-counting process. This perspective risks naturalizing contingent institutional arrangements (centralized counting, closed-source systems, delayed audits) as immutable properties of electoral systems themselves.
constraint_indexing:constraint_classification(voting_system_integrity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voting_system_integrity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(voting_system_integrity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(voting_system_integrity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(voting_system_integrity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(voting_system_integrity, TR),
    TR >= 0.70.

:- end_tests(voting_system_integrity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint generates significant asymmetric benefits for electoral administrations and incumbent parties through voter suppression mechanisms, provisional ballot disqualification, voter purges, and proprietary system control. However, the extraction is not maximal because substantial coordination function exists (organizing distributed voting requires genuine infrastructure) and some jurisdictions implement relatively open systems. The measured value reflects that extraction mechanisms are real and systematic but not total. Suppression (0.65): High. Structural barriers to voting participation are substantial: identification requirements, registration deadlines, provisional ballot processes, polling place closures, language access failures, voter purge mechanisms, and transportation barriers all concentrate on marginalized populations. Suppression is enforced through bureaucratic rather than explicit coercive mechanisms, but effects are equivalent to legal barriers. Theater ratio (0.68): High. Post-election audits, chain-of-custody procedures, machine certification, and observer access are largely performative: they generate confidence signals without providing independent verification of actual ballot integrity or counting accuracy. Proprietary systems prevent meaningful audit. Security procedures exist to satisfy appearance standards rather than achieve technical verification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Election administration sees Rope (coordination). Incumbent parties see Rope (benefit). Disenfranchised voters see Snare (extraction without benefit). Marginalized communities see Snare (systematic targeting). Voting rights organizations see Tangled Rope (mixed). Legacy infrastructure sees Piton (theater). The analytical observer risks Mountain (naturalization). This gap is diagnostic: the 'integrity' framing successfully obscures extraction by making suppression mechanisms appear as technical requirements rather than political tools. The gap reveals how beneficiaries (administrations, incumbents) perceive the same structure as neutral coordination while victims perceive it as oppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the asymmetric structural relationship to the constraint. Election administration authorities and incumbent parties occupy beneficiary positions with high exit options (they can modify procedures, change technology, adjust timelines) — low d values producing negative effective extraction (they experience the constraint as coordination, not burden). Disenfranchised voters and marginalized communities occupy victim positions with zero exit options (they cannot avoid the voting system and cannot escape barriers) — high d values (0.95+) producing maximum f(d) and maximum experienced extraction. Voting rights organizations occupy constrained positions with some exit options (litigation, advocacy, organizing) but significant barriers to enforcement — moderate d values (0.55-0.65) producing moderate-high experienced extraction. The pipeline computes d from these context parameters; beneficiaries experience coordination, victims experience extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The constraint avoids mandatrophy by exhibiting genuine coordinate coordination function — organizing distributed voting IS difficult and requires standardization. However, the classification depends entirely on whether we measure pure coordination (Rope) or measure coordination + suppression (Tangled Rope). The engine resolves this by including both beneficiary (coordination function) and victim (suppression mechanism) perspectives. The tangled_rope classification is justified because both coordination AND asymmetric extraction are present: beneficiaries genuinely gain coordination value; victims genuinely experience extraction without coordination benefit. The constraint is not pure extraction (Snare) because administrations benefit from genuine procedural requirements. The constraint is not pure coordination (Rope) because suppression mechanisms concentrate on opposition demographics, proving intentional extraction. The mandatrophy is resolved: the constraint is Tangled Rope from the analytical perspective, with perspectival variation for different agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_tradeoff_threshold,
    'What level of voting accessibility must be sacrificed to achieve optimal vote verification and ballot secrecy?',
    'Comparative analysis of electoral systems: mail voting + post-election audits (high accessibility, delayed verification) vs in-person voting with real-time verification (lower accessibility, faster verification). Empirical comparison of fraud rates vs disenfranchisement rates across regimes.',
    'If threshold favors accessibility: identification requirements and provisional ballot rules constitute extractive suppression (Snare classification confirmed). If threshold favors verification: high suppression is necessary coordination cost (Rope classification). Current empirical data suggests accessibility is sacrificed without corresponding security gain (extraction hypothesis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_tradeoff_threshold, empirical, 'Accessibility vs verification tradeoff threshold').

omega_variable(
    voter_confidence_mechanism,
    'Does the integrity constraint serve voter confidence (coordination) or incumbent electoral control (extraction)?',
    'Temporal analysis: do voter confidence measures increase with integrity mechanisms (audits, observer access, transparency)? Or does confidence depend only on outcome alignment with polling? Comparison across jurisdictions with high vs low transparency.',
    'If confidence depends on transparency: integrity mechanisms are genuine coordination. If confidence depends on outcome: integrity mechanisms are theater masking extraction. If confidence decreases with transparency (as voters learn about vulnerability): system is actively extractive through confidence management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_confidence_mechanism, empirical, 'Whether integrity mechanisms drive or mask voter confidence').

omega_variable(
    suppression_structural_vs_intentional,
    'Are voter suppression mechanisms (ID requirements, registration deadlines, provisional ballots) structural byproducts of integrity needs or intentional extraction tools?',
    'Comparative institutional analysis: do jurisdictions with higher verification standards also have higher disenfranchisement? Or do high-access systems (mail voting) achieve equal verification through post-election audits? Historical analysis of policy adoption: when were suppression mechanisms introduced relative to integrity claims?',
    'If structural byproduct: suppression is necessary cost (reduces extraction value). If intentional: suppression is the extraction mechanism (increases extraction value, classification remains Snare). Current data: suppression mechanisms concentrate on demographic groups unlikely to support incumbents, suggesting intentionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_intentional, empirical, 'Whether suppression is structural byproduct or intentional extraction').

omega_variable(
    decentralized_verification_feasibility,
    'Can voter-verifiable paper ballots combined with post-election audits provide equivalent security to centralized machine counting with proprietary systems?',
    'Technical security analysis: cryptographic auditing, chain-of-custody requirements, auditable random samples. Operational feasibility study: cost, training requirements, timeline constraints.',
    'If feasible: current reliance on centralized systems is contingent institutional choice, not structural necessity (undermines mountain perspective, supports tangled_rope/scaffold). If infeasible: some centralization is structural requirement (supports rope/mountain perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralized_verification_feasibility, empirical, 'Feasibility of decentralized verification alternatives').

omega_variable(
    partisan_capture_extent,
    'To what extent does electoral administration enforcement capture serve incumbent party interests vs genuine integrity maintenance?',
    'Pattern analysis: do enforcement actions (purges, ID challenges, provisional ballot invalidation) correlate with electoral margins and demographic targeting? Comparison of enforcement intensity across party control jurisdictions. Analysis of appeals and litigation outcomes.',
    'If capture is high: the constraint functions as a Snare for opposition voters (extraction mechanism). If capture is low: the constraint functions as Rope (genuine coordination). If capture varies by jurisdiction: network structure becomes apparent (different institutional effectiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_capture_extent, empirical, 'Extent of partisan capture in electoral administration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voting_system_integrity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vsi_tr_t0, voting_system_integrity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vsi_tr_t10, voting_system_integrity, theater_ratio, 10, 0.58).
narrative_ontology:measurement(vsi_tr_t20, voting_system_integrity, theater_ratio, 20, 0.68).
narrative_ontology:measurement(vsi_tr_t5, voting_system_integrity, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(vsi_be_t0, voting_system_integrity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vsi_be_t10, voting_system_integrity, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vsi_be_t20, voting_system_integrity, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(vsi_be_t5, voting_system_integrity, base_extractiveness, 5, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voting_system_integrity, resource_allocation).
narrative_ontology:affects_constraint(voting_system_integrity, voter_registration_databases).
narrative_ontology:affects_constraint(voting_system_integrity, provisional_ballot_invalidation).
narrative_ontology:affects_constraint(voting_system_integrity, voter_purge_mechanisms).
narrative_ontology:affects_constraint(voting_system_integrity, voter_identification_requirements).

% DUAL FORMULATION NOTE:
% Voting system integrity decomposes into multiple structurally distinct constraints with different ε values: voter_registration_databases (ε≈0.42, coordination database), provisional_ballot_invalidation (ε≈0.68, extractive suppression), voter_purge_mechanisms (ε≈0.72, targeted extraction), voter_identification_requirements (ε≈0.55, mixed barrier/coordination). The parent constraint aggregates these mechanisms and their cumulative extraction effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(voting_system_integrity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
