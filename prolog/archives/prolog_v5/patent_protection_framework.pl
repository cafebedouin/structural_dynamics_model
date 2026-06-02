% ============================================================================
% CONSTRAINT STORY: patent_protection_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patent_protection_framework, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: patent_protection_framework
 *   human_readable: Patent Protection Framework
 *   domain: intellectual_property/innovation_policy
 *
 * SUMMARY:
 *   The patent protection framework exists as a globally-enforced constraint
 *   on knowledge use, designed to incentivize innovation by granting
 *   time-limited monopolies to inventors. The system has evolved from
 *   national frameworks toward international harmonization (TRIPS, bilateral
 *   trade agreements) that create a unified global enforcement mechanism. It
 *   exhibits a fundamental tension: patents genuinely coordinate ownership
 *   claims and licensing relationships (rope function), but simultaneously
 *   extract from those who cannot afford licensed access or who face blocking
 *   patents in complex technology domains (snare and tangled-rope functions).
 *   The constraint's extractiveness (0.58) and suppression (0.62) reflect
 *   this hybrid character. Base extractiveness has risen from 0.35 (20 years
 *   ago) to 0.58 as patent terms have extended, enforcement mechanisms have
 *   strengthened, and patent thickets have accumulated in critical technology
 *   domains. Theater ratio (0.48) indicates the patent examination process
 *   maintains an appearance of rigorous quality control while actual
 *   verification of novelty and non-obviousness is weak — increasing backlogs
 *   and declining examination depth mean many invalid patents issue and
 *   persist through litigation risk rather than merit.
 *
 * KEY AGENTS:
 *   - Patent Holders: Primary beneficiary (institutional/arbitrage) — capture monopoly rents during patent term, establish licensing revenue streams, gain competitive advantage through exclusion
 *   - Generic Drug Manufacturers: Primary victim (powerless/trapped) — cannot produce life-saving medications at cost-effective prices; face legal enforcement, economic barriers to patent challenges, geographic scope limitations
 *   - Subsequent Innovators: Secondary victim (moderate/constrained) — face patent thickets, design-around costs, licensing fees; also benefit from protecting their own future innovations
 *   - Developing Nation Populations: Structural victim (powerless/trapped) — face pricing barriers to essential medicines; lack political power to negotiate compulsory licensing effectively
 *   - Open Source Coalition: Organized secondary actor (organized/constrained) — benefits from copyleft reciprocal licensing; constrained by patent enforcement in implementation domains
 *   - Patent Office System: Institutional actor (institutional/arbitrage) — maintains examination apparatus; sees its own process as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choice as inevitable law of innovation economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patent_protection_framework, 0.58).
domain_priors:suppression_score(patent_protection_framework, 0.62).
domain_priors:theater_ratio(patent_protection_framework, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patent_protection_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(patent_protection_framework, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(patent_protection_framework, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patent_protection_framework, tangled_rope).
narrative_ontology:human_readable(patent_protection_framework, "Patent Protection Framework").
narrative_ontology:topic_domain(patent_protection_framework, "intellectual_property/innovation_policy").

domain_priors:requires_active_enforcement(patent_protection_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patent_protection_framework, patent_holders).
narrative_ontology:constraint_beneficiary(patent_protection_framework, large_technology_firms).
narrative_ontology:constraint_beneficiary(patent_protection_framework, pharmaceutical_companies).
narrative_ontology:constraint_victim(patent_protection_framework, generic_drug_manufacturers).
narrative_ontology:constraint_victim(patent_protection_framework, developing_nation_populations).
narrative_ontology:constraint_victim(patent_protection_framework, open_source_developers).
narrative_ontology:constraint_victim(patent_protection_framework, subsequent_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERIC DRUG MANUFACTURERS (SNARE) — Trapped by patent enforcement mechanisms that prevent production of life-saving medications at affordable costs. Suppression operates through legal enforcement (TRIPS agreements), economic barriers to patent challenges, and geographic scope of enforcement. No meaningful exit option; bear full cost of monopoly pricing while population health deteriorates. Maximum experienced extraction.
constraint_indexing:constraint_classification(patent_protection_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSEQUENT INNOVATORS (TANGLED ROPE) — Constrained by patent landscapes that require design-around costs, patent searches, and licensing fees. But also benefit from the patent system's protection of their own future innovations. Mixed extraction and coordination: the system both enables and blocks their work depending on whose patents dominate their domain.
constraint_indexing:constraint_classification(patent_protection_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATENT HOLDERS (ROPE) — Experience the patent system as pure coordination: it establishes who owns what innovation, enables licensing revenue, and provides legal standing against competitors. Net beneficiary. Arbitrage options allow selective enforcement and licensing strategies.
constraint_indexing:constraint_classification(patent_protection_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (TANGLED ROPE) — Organized actors (Creative Commons, GPL advocates, scientific societies) see patents as coordination through reciprocal licensing (GPL copyleft) while simultaneously experiencing extraction through patent thickets that block novel implementation approaches. Constrained by patent enforcement; benefit from building alternative coordination norms (open-source copyleft, creative commons).
constraint_indexing:constraint_classification(patent_protection_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT OFFICE SYSTEM (PITON) — The patent examination and enforcement apparatus is substantially degraded: examination backlogs are severe, prior art searches are incomplete, obvious patents are regularly issued, and defense against invalid patents requires expensive litigation. Theater ratio (0.48) reflects that the system appears to perform rigorous quality control while actual verification is weak. The system persists through institutional inertia and because alternatives (copyright alternatives, trade secret reliance) haven't fully replaced it.
constraint_indexing:constraint_classification(patent_protection_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some protection mechanism for innovation incentives appears inherent to markets: without exclusivity, copying becomes immediate and innovation underinvests. This perspective sees patent systems as natural law — inevitable under any innovation economy. However, the structural data reveals this as a false summit: the contingent choice to grant TIME-LIMITED monopolies (patent protection) naturalizes what is actually a policy instrument with many alternatives (prizes, open-source subsidies, trade-secret protection).
constraint_indexing:constraint_classification(patent_protection_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patent_protection_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patent_protection_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patent_protection_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patent_protection_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patent_protection_framework, TR),
    TR >= 0.70.

:- end_tests(patent_protection_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the observation interval. The patent system's base extraction has increased because: (1) patent terms have extended (pharmaceutical term extensions, design patent duration increases); (2) enforcement mechanisms have strengthened (TRIPS implementation, bilateral enforcement provisions); (3) patent thickets have accumulated in biotechnology and semiconductors, raising blocking risk; (4) litigation costs for patent defense have increased, raising barriers to exit for small firms. The rise from 0.35 to 0.58 reflects that what began as a reasonable innovation incentive has accumulated into a rent-extraction apparatus. Suppression (0.62): Moderate-high and stable. Suppression operates through legal enforcement mechanisms (TRIPS Article 61, criminal penalties), economic barriers (cost of patent challenges, licensing fees), and geographic barriers (enforcement varies by jurisdiction but tends toward harmonized global enforcement). Barriers to exit include legal prohibitions on manufacturing patented products, costs of design-around, and reduced licensing availability in developing nations. Theater ratio (0.48): Moderate and slightly rising. Patent examination claims to assess novelty, non-obviousness, and enabling disclosure but operates with severe constraints: examination backlogs (200,000+ pending applications in USPTO), declining examination hours per patent, limited prior-art search depth, and weak technological expertise in examiners. The system maintains theater through formal procedures while actual verification of patent quality is weak. The rise from 0.40 to 0.48 reflects increasing examination burden and declining rigor.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap exists between patent holders (Rope) and generic manufacturers (Snare). Patent holders genuinely see the system as solving coordination — 'who owns this innovation?' — and experience no extraction because they benefit. Generic manufacturers genuinely see the system as blocking access to essential goods — 'why can't we manufacture this drug at cost?' — and experience maximum extraction because they are powerless and trapped. The analytical observer's natural law perspective (Mountain) is a false summit: it naturalizes the contingent choice to grant monopolies by treating it as inherent to innovation. The open-source coalition perspective (Tangled Rope) reveals that copyleft licensing can provide both coordination (establishing reciprocal obligations) and partial extraction (restricting closed-source implementations). The subsequent innovators perspective (Tangled Rope) shows that the same patent system simultaneously incentivizes and blocks innovation depending on patent density in their domain. The patent office perspective (Piton) reveals that the system's actual function (quality control of patents) is degraded while its formal procedure (examination process) maintains theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural position relative to the patent system. Patent holders have low d (0.05-0.15) because they are beneficiaries with arbitrage options — they can choose when to patent, when to license, when to enforce, when to abandon patents. Generic manufacturers have high d (0.90-0.98) because they are trapped victims with no legal exit option — manufacturing a patented drug is criminalized. Subsequent innovators have moderate d (0.55-0.70) because they are constrained victims with partial exit options — they can design around, pay licensing fees, or wait for expiration. Open-source coalitions have moderate d (0.45-0.60) because they are organized actors with some agency — they can choose copyleft strategies, work in open domains, or build alternative mechanisms. The piton classification for the patent office derives from theater ratio (0.48) being non-trivial but less than 0.70, combined with the system's fundamental weakness (low verification), making it a degraded institution maintained through inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in patent protection is resolved by recognizing that the system performs MULTIPLE structurally distinct functions simultaneously, and these functions have different extractiveness profiles. (1) Coordination function (Rope): Patents establish clear ownership claims and enable licensing. This is coordination at base extraction ~0.10-0.15 with minimal suppression. (2) Incentive function (Tangled Rope): Patents motivate innovation by promising temporary monopoly rents. This is coordination + extraction at base extraction ~0.30-0.50 with moderate suppression. (3) Blocking function (Snare): Patents prevent subsequent innovation in crowded technology domains through patent thickets. This is pure extraction at base extraction ~0.60-0.80 with high suppression. The observation interval shows all three functions operating simultaneously, with the blocking function (Snare) increasingly dominant as patent density has accumulated. The mandatrophy resolves by showing that the claimed type (Tangled Rope, extractiveness 0.58) correctly represents the AGGREGATE effect of all three functions operating together: coordination + extraction + blocking = tangled rope at this aggregation level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_threshold,
    'What patent term length and enforcement strength actually maximize innovation output vs. what term length maximizes incumbent rent extraction?',
    'Econometric analysis of R&D investment vs. patent term length across jurisdictions; comparison of innovation rates pre- and post-TRIPS harmonization; measurement of generic entry delays post-expiration',
    'If optimal term is 7 years: current 20-year terms extract rent beyond innovation incentive. If term is 20 years: current enforcement is appropriate and victims'' constraints are justified by innovation benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_threshold, empirical, 'Relationship between patent term length and actual innovation output').

omega_variable(
    cumulative_innovation_vs_blocking,
    'Do patent thickets in complex technologies (semiconductors, biotechnology) block cumulative innovation more than they incentivize it?',
    'Citation analysis of blocking patents; comparison of innovation cycles in patent-heavy vs patent-light domains; measurement of design-around costs as percentage of R&D budget',
    'If blocking > incentivizing: patent system is net negative for innovation in complex tech — reclassifies toward Snare. If incentivizing > blocking: patent system is net positive — reclassifies toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_innovation_vs_blocking, empirical, 'Whether patents block or incentivize cumulative innovation').

omega_variable(
    alternative_coordination_mechanisms,
    'Can open-source copyleft, prize systems, or trade-secret protection achieve equivalent innovation incentives with lower suppression?',
    'Comparison of innovation output in copyleft domains (Linux, open-source toolchains) vs patent-protected domains; measurement of prize-funded vs patent-funded innovation outcomes; analysis of trade-secret reliance across industries',
    'If alternatives are viable: patent system is a contingent choice, not natural law — suppression values should increase (ratified extraction). If alternatives fail: patent system is necessary cost — suppression values are justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Viability of alternative innovation incentive mechanisms').

omega_variable(
    development_nation_compulsory_license_effectiveness,
    'Can compulsory licensing (TRIPS Article 31) and Doha flexibilities actually reduce extraction for generic drug manufacturers and developing nations?',
    'Analysis of successful compulsory license cases (India, Thailand, South Africa); measurement of access improvement post-license; comparison of generic drug availability and cost changes',
    'If effective: exit option exists (mobile), reclassifies toward Tangled Rope. If ineffective (firms refuse to operate under compulsory terms): exit option is illusory, reclassifies toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(development_nation_compulsory_license_effectiveness, empirical, 'Effectiveness of TRIPS compulsory licensing in reducing extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patent_protection_framework, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patent_tr_t0, patent_protection_framework, theater_ratio, 0, 0.4).
narrative_ontology:measurement(patent_tr_t10, patent_protection_framework, theater_ratio, 10, 0.44).
narrative_ontology:measurement(patent_tr_t20, patent_protection_framework, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(patent_be_t0, patent_protection_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(patent_be_t10, patent_protection_framework, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(patent_be_t20, patent_protection_framework, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patent_protection_framework, information_standard).
narrative_ontology:boltzmann_floor_override(patent_protection_framework, 0.12).
narrative_ontology:affects_constraint(patent_protection_framework, drug_pricing_monopoly).
narrative_ontology:affects_constraint(patent_protection_framework, open_source_patent_thickets).
narrative_ontology:affects_constraint(patent_protection_framework, technology_transfer_barriers).

% DUAL FORMULATION NOTE:
% Patent protection framework decomposes into three structurally distinct claims with different extractiveness profiles: (1) patent_coordination (ε=0.12, Rope) — establishing ownership and licensing; (2) innovation_incentive_function (ε=0.35, Tangled Rope) — motivating R&D through monopoly rents; (3) patent_thicket_blocking (ε=0.75, Snare) — preventing downstream innovation through densely overlapping claims. The aggregate framework combines all three functions simultaneously. Upstream: ehrenfest_phase_space (foundational intellectual property concept); Downstream: drug_pricing_monopoly (pharmaceutical application), open_source_patent_thickets (technology domain application).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(patent_protection_framework, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
