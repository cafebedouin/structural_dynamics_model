% ============================================================================
% CONSTRAINT STORY: ballot_access_barriers_latam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ballot_access_barriers_latam, []).

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
 *   constraint_id: ballot_access_barriers_latam
 *   human_readable: Ballot Access Barriers in Latin America
 *   domain: political/electoral/governance
 *
 * SUMMARY:
 *   Ballot access barriers in Latin America create a structural tension
 *   between the coordination function of defining a bounded electoral arena
 *   and the extractive function of excluding challengers to incumbent
 *   coalitions. Across the region, formal requirements for ballot
 *   access—signature collection thresholds (ranging from 10,000 to 200,000),
 *   filing fees ($2,000 to $50,000 USD equivalent), documentation standards,
 *   and electoral commission vetting—ostensibly serve administrative purposes
 *   but function asymmetrically: incumbent parties and dominant coalitions
 *   clear these barriers routinely; grassroots movements, new entrants, and
 *   marginalized communities face arbitrary enforcement, selective
 *   application, and effectively insurmountable costs. The constraint
 *   exhibits high suppression (0.72) because alternatives to ballot access
 *   are foreclosed: political participation outside formal electoral channels
 *   is constrained by weak party-building capacity, limited campaign finance,
 *   and legal restrictions on unregistered organizing. Theater ratio (0.68)
 *   reflects that formal neutrality in ballot access administration masks
 *   political discretion—electoral commissions frame decisions as technical
 *   or procedural while applying standards selectively. The extractiveness
 *   trajectory (0.35→0.58 over the interval) indicates accumulation of
 *   barriers: signature requirements have increased, digital filing systems
 *   have been introduced without reducing other barriers, and commission
 *   scrutiny of documentation has intensified.
 *
 * KEY AGENTS:
 *   - Grassroots Movements: Primary victims (powerless/trapped) — face signature collection barriers, filing fees, documentation requirements with no meaningful alternative to formal ballot access
 *   - Incumbent Coalition: Primary beneficiary (institutional/arbitrage) — controls electoral commission interpretation, benefits from pre-existing ballot access, can arbitrage between formal rules and informal enforcement
 *   - Opposition Parties: Secondary actor (moderate/constrained) — have organizational infrastructure and some resources to clear barriers, but must pay extraction cost; benefit from some coordination function (defined electoral arena)
 *   - Electoral Commission: Implementing institution (institutional/arbitrage) — maintains performative neutral process while enabling selective enforcement; benefits from formal authority to gate access
 *   - Electoral Reform Coalition: Organized interveners (organized/mobile) — OAS, IIDH, domestic NGOs, regional organizations pushing for simplified requirements, digital signatures, fee reduction; see sunset pathway through normalization of reform
 *   - Marginalized Communities: Victim class (powerless/identity_locked) — face compounded barriers: language requirements for documents, geographic barriers to signature collection, exclusion from formal political processes by design; identity-locked through criminalization of alternative organizing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ballot_access_barriers_latam, 0.58).
domain_priors:suppression_score(ballot_access_barriers_latam, 0.72).
domain_priors:theater_ratio(ballot_access_barriers_latam, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ballot_access_barriers_latam, extractiveness, 0.58).
narrative_ontology:constraint_metric(ballot_access_barriers_latam, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ballot_access_barriers_latam, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ballot_access_barriers_latam, tangled_rope).
narrative_ontology:human_readable(ballot_access_barriers_latam, "Ballot Access Barriers in Latin America").
narrative_ontology:topic_domain(ballot_access_barriers_latam, "political/electoral/governance").

domain_priors:requires_active_enforcement(ballot_access_barriers_latam).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ballot_access_barriers_latam, incumbent_parties).
narrative_ontology:constraint_beneficiary(ballot_access_barriers_latam, dominant_coalitions).
narrative_ontology:constraint_victim(ballot_access_barriers_latam, grassroots_movements).
narrative_ontology:constraint_victim(ballot_access_barriers_latam, new_entrants).
narrative_ontology:constraint_victim(ballot_access_barriers_latam, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRASSROOTS MOVEMENT (SNARE) — Faces signature collection requirements (50,000-200,000 signatures), filing fees ($5,000-$50,000 USD equivalent), documentation barriers, and arbitrary electoral commission decisions. No meaningful alternative to ballot access; trapped in a system that forecloses entry. Experiences maximum extraction with no exit pathway.
constraint_indexing:constraint_classification(ballot_access_barriers_latam, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION PARTY (TANGLED ROPE) — Benefits from some structural coordination function (ballot access rules do define the electoral arena and enable predictable competition), but faces asymmetric barriers. Can mobilize resources and existing organizational infrastructure to clear thresholds, but at significant cost. Has some agency but must pay an extraction tax to participate. Suppression is high but not absolute.
constraint_indexing:constraint_classification(ballot_access_barriers_latam, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT COALITION (ROPE) — Experiences ballot rules as pure coordination mechanism. Incumbent parties benefit from pre-existing ballot access, familiarity with regulatory processes, and control over electoral commissions in many contexts. Can arbitrage between formal rules and informal enforcement. Net beneficiary with minimal extraction cost.
constraint_indexing:constraint_classification(ballot_access_barriers_latam, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM COALITION (SCAFFOLD) — International organizations (OAS, IIDH), domestic NGOs, and cross-party reform blocs are systematizing alternative pathways: digital signature collection, reduced filing fees, post-election reimbursement, expedited vetting. Reform has sunset logic — as democratic consolidation matures and regional coordination increases, barrier reduction becomes institutionalized norm. High agency and visible exit pathway.
constraint_indexing:constraint_classification(ballot_access_barriers_latam, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ELECTORAL COMMISSION APPARATUS (PITON) — Administrative machinery for processing ballot access claims is largely performative in many jurisdictions: formally neutral processes (signature verification, document review) are applied selectively and lack transparent standards. Decisions often reflect political pressure rather than clear rules. The apparatus persists through institutional inertia — replaced by neither restructured capacity nor openly acknowledged political gatekeeping.
constraint_indexing:constraint_classification(ballot_access_barriers_latam, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some ballot access threshold is inherent to electoral administration: states must have canonical ballots, and infinite candidate access creates logistical impossibility. This perspective treats minimum access barriers as a natural structural law. However, the base properties contradict pure mountain classification — suppression (0.72) and extractiveness (0.58) reveal contingent institutional choices, not immutable limits. The engine will flag this as false summit.
constraint_indexing:constraint_classification(ballot_access_barriers_latam, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ballot_access_barriers_latam_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ballot_access_barriers_latam, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ballot_access_barriers_latam, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ballot_access_barriers_latam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ballot_access_barriers_latam, TR),
    TR >= 0.70.

:- end_tests(ballot_access_barriers_latam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Incumbent parties extract benefits through the entire interval by excluding competitors during the verification window and by maintaining asymmetric enforcement. The extractiveness has increased over time (0.35→0.58) as barriers have been refined and formalized—signature requirements have grown, filing fees have increased in real terms, and digital systems have been introduced without reducing other barriers. The constraint combines genuine coordination function (defining the electoral arena) with significant asymmetric extraction (disadvantaging new entrants). Suppression (0.72): High. Multiple mechanisms: signature collection requires sustained mobilization, filing fees represent prohibitive cost for grassroots organizations, documentation standards exclude those without formal identification, electoral commission processes lack transparency and are subject to political pressure, and alternatives to formal ballot access are legally restricted or face police enforcement. Suppression is structural (cost barriers are real) and internalized (grassroots movements often believe they cannot compete and do not attempt to clear thresholds). Theater ratio (0.68): High. Electoral commission processes are formally neutral but substantively political—signature verification, document review, and eligibility determination follow stated rules but are applied selectively. The theater has increased as processes have become more technical-seeming while enforcement remains opaque. The administrative machinery performs neutrality while enabling partisan gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The grassroots movement sees a Snare (maximum extraction, no exit). The opposition party sees Tangled Rope (mixed coordination and extraction, constrained exit). The incumbent sees Rope (pure coordination, arbitrage exit). The reform coalition sees Scaffold (temporary problem, sunset pathway via normalization of simpler access rules). The electoral commission sees Piton (performative neutral process maintained through institutional inertia). The analytical observer risks seeing a Mountain (ballot definition requires administrative gates)—but the structural data reveals contingency. The perspectival gap widens over time as extractiveness increases (theater_ratio rises, barriers accumulate). This gap is diagnostic: if all actors converged on a single classification, the constraint would be purely coordinating or purely extracting. The gap's persistence across 10 years of measurement indicates that the constraint's hybrid character (coordination + extraction) is structural, not transitional.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (incumbent_parties, dominant_coalitions) occupies institutional power with arbitrage exit options—they can afford filing fees, can mobilize signatures through party infrastructure, and have direct or indirect influence over commission decisions. Their directionality (d ≈ 0.10) yields low effective extraction because they experience the constraint as enabling coordination, not as extraction. The victim groups (grassroots_movements, new_entrants, marginalized_communities) occupy powerless or moderate power with trapped or constrained exit options—signature collection requires volunteer mobilization they cannot afford to hire out, filing fees represent unavailable resources, and documentation requirements may literally exclude them (undocumented status, absence of formal identification). Their directionality (d ≈ 0.85-0.95) yields high effective extraction. The opposition parties occupy moderate power with constrained exit—they can pay and mobilize but face higher costs than incumbents, and their exit options are limited by the ballot system itself (no alternative path to executive office). Their directionality (d ≈ 0.55) yields medium-high extraction. This asymmetry is the core of the constraint's hybrid character: genuine coordination function for defining the electoral arena exists alongside systematic extraction from powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint's classification as Tangled Rope rather than pure Rope (coordination) or pure Snare (extraction) is justified by the presence of both genuine coordination function (defining the electoral arena, establishing canonical ballots, preventing logistical chaos) AND asymmetric extraction (benefiting incumbents, harming new entrants). The coordination function is evidenced by the fact that opposition parties benefit from clear rules and defined electoral procedures—they oppose modification not elimination of ballot access requirements. The extraction function is evidenced by the asymmetric application of requirements and the accumulation of barriers over time. If the constraint were pure Rope, all perspectives would classify similarly and extractiveness would be lower (≤0.45). If the constraint were pure Snare, beneficiaries would not experience coordination benefits and would support barrier reduction—but incumbents often resist simplification of access rules, indicating they perceive coordination value alongside extraction. The Tangled Rope classification resolves this: coordination and extraction coexist, and different power positions experience different ratios of each. The theater ratio (0.68) confirms that formal procedure provides cover for partisan gatekeeping—the ritual of 'neutral administration' masks the reality of selective enforcement. This is diagnostic of Tangled Rope: active enforcement (electoral commission decisions) maintains an appearance of impartiality while enabling asymmetric outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arbitrary_enforcement_vs_rule_application,
    'Are signature collection and filing requirements applied consistently and transparently, or are they selective enforcement mechanisms for political gatekeeping?',
    'Audit of ballot access decisions with analysis of approval rates by party/movement; comparison of documented requirements vs. actual application; tracking of electoral commission decision appeals and reversal rates',
    'If consistently applied: constraint reclassifies as lower-extraction Rope or Scaffold (genuine coordination with fair entry cost). If selectively applied: confirms Snare/Tangled Rope classification and reveals suppression mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arbitrary_enforcement_vs_rule_application, empirical, 'Whether ballot access requirements are consistently applied or selectively enforced').

omega_variable(
    absolute_barrier_vs_cost_barrier,
    'Do ballot access barriers foreclose entry entirely (impossible to clear thresholds), or do they impose high costs that some actors can overcome?',
    'Time-series analysis of successful ballot access by non-incumbent actors; measurement of actual resource requirements vs. available funding for grassroots organizations; documentation of cases where barriers were cleared vs. insurmountable',
    'If purely cost barriers: victims experience Tangled Rope or Scaffold (not Snare). If absolute foreclosure: confirms Snare classification for powerless agents. If mixed by context: suggests constraint family decomposition (separate stories for different country contexts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_barrier_vs_cost_barrier, empirical, 'Whether barriers are cost-imposing or absolutely foreclosing').

omega_variable(
    electoral_reform_sustainability,
    'Will ballot access reforms (simplified signature collection, reduced fees, digital filing) persist once implementation begins, or will incumbent pressure reverse them?',
    'Track implementation and rollback of reform measures across Latin American countries; analyze whether reforms introduced in one cycle remain in subsequent cycles; correlate persistence with change in incumbent party control',
    'If persistent: Scaffold classification is validated — reforms represent genuine sunset of the constraint. If reversed: reforms are performative and theater_ratio remains high; constraint may reclassify as Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_reform_sustainability, empirical, 'Whether ballot access reforms persist or are reversed by incumbent pressure').

omega_variable(
    signature_collection_authenticity,
    'Can signature collection requirements be met through legitimate grassroots mobilization, or are they structured to require centralized resources (paid signature collectors, established party infrastructure)?',
    'Analysis of actual signature collection methods; comparison of per-signature cost for grassroots vs. incumbent-backed campaigns; assessment of whether small organizations can achieve targets through volunteer effort alone',
    'If achievable through grassroots effort: constraint is a coordination threshold (Rope/Tangled Rope). If requires paid infrastructure: suppression mechanism confirmed and extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signature_collection_authenticity, empirical, 'Whether signature collection can be accomplished by grassroots volunteers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ballot_access_barriers_latam, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ballot_tr_t0, ballot_access_barriers_latam, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ballot_tr_t5, ballot_access_barriers_latam, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ballot_tr_t10, ballot_access_barriers_latam, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ballot_tr_t8, ballot_access_barriers_latam, theater_ratio, 8, 0.66).

% Extraction over time
narrative_ontology:measurement(ballot_be_t0, ballot_access_barriers_latam, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ballot_be_t5, ballot_access_barriers_latam, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ballot_be_t10, ballot_access_barriers_latam, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ballot_be_t2, ballot_access_barriers_latam, base_extractiveness, 2, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ballot_access_barriers_latam, enforcement_mechanism).
narrative_ontology:affects_constraint(ballot_access_barriers_latam, campaign_finance_inequality_latam).
narrative_ontology:affects_constraint(ballot_access_barriers_latam, media_access_concentration_latam).
narrative_ontology:affects_constraint(ballot_access_barriers_latam, voter_suppression_latam).

% DUAL FORMULATION NOTE:
% Ballot access barriers are structurally coupled to campaign finance constraints and media access barriers—all three restrict entry to electoral competition. However, they are decomposed into separate stories because they have different ε values and different institutional mechanisms. Ballot access (ε=0.58) involves electoral commission gatekeeping; campaign finance (ε≈0.65) involves asymmetric spending access; media access (ε≈0.52) involves broadcasting regulation. The network reflects that degradation in one constraint exacerbates others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ballot_access_barriers_latam, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
