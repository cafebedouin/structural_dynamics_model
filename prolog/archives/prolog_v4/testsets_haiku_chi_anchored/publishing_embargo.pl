% ============================================================================
% CONSTRAINT STORY: publishing_embargo
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publishing_embargo, []).

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
 *   constraint_id: publishing_embargo
 *   human_readable: Academic Publishing Embargo
 *   domain: social/academic_publishing
 *
 * SUMMARY:
 *   Academic publishing embargoes represent a structural tension between the
 *   coordination function of peer-reviewed journals (quality assurance,
 *   priority establishment, citation standards) and the extractive
 *   gatekeeping that delays public access to findings. The embargo constraint
 *   exhibits characteristics of both coordination mechanisms (rope) and pure
 *   extraction (snare/tangled rope) depending on the observer's structural
 *   position. For early-career researchers and time-critical domains
 *   (pandemic response, public health), embargoes function as pure extraction
 *   — they suppress findings precisely when dissemination matters most, while
 *   researchers have no exit option without career damage. For commercial
 *   publishers, embargoes coordinate peer review and distribute editorial
 *   authority while capturing subscription revenue. For the global research
 *   community, embargoes both enable quality assurance and extract value
 *   through delayed access. The theater_ratio (0.68) reflects degradation of
 *   the embargo mechanism itself: preprint servers, institutional
 *   repositories, and researcher-direct dissemination strategies are making
 *   embargo enforcement increasingly performative. The constraint appears to
 *   be in transition from functional coordination mechanism (rope) toward
 *   institutional inertia (piton), with residual extraction (tangled rope)
 *   visible from institutional perspectives.
 *
 * KEY AGENTS:
 *   - Early Career Researchers: Primary victim (powerless/trapped) — career advancement depends on journal placement; cannot share findings before official publication without violating embargo terms
 *   - Commercial Publishers: Primary beneficiary (institutional/arbitrage) — embargo enforces temporal monopoly on dissemination and creates subscription demand; coordinates peer review coordination function
 *   - Public Health Emergencies: Secondary victim (moderate/constrained) — time-critical findings (pandemic treatments, vaccine efficacy, disease vectors) are suppressed during embargo windows when rapid response is needed
 *   - Global Research Community: Mixed stakeholder (organized/mobile) — benefits from peer-review quality control and citation standards; experiences extraction through delayed access and journal gatekeeping
 *   - Journal Editorial System: Institutional actor (institutional/constrained) — administers embargo enforcement through behavioral compliance; piton perspective: performative ritual maintained through inertia
 *   - Open Science Coalition: Organized alternatives (organized/mobile) — preprint servers (arXiv, bioRxiv, medRxiv), open-access mandates, and registered reports providing alternative pathways with explicit sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing embargo as necessary for quality assurance when alternatives demonstrate coordination without gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publishing_embargo, 0.52).
domain_priors:suppression_score(publishing_embargo, 0.65).
domain_priors:theater_ratio(publishing_embargo, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publishing_embargo, extractiveness, 0.52).
narrative_ontology:constraint_metric(publishing_embargo, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(publishing_embargo, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publishing_embargo, tangled_rope).
narrative_ontology:human_readable(publishing_embargo, "Academic Publishing Embargo").
narrative_ontology:topic_domain(publishing_embargo, "social/academic_publishing").

domain_priors:requires_active_enforcement(publishing_embargo).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publishing_embargo, commercial_publishers).
narrative_ontology:constraint_beneficiary(publishing_embargo, journal_gatekeepers).
narrative_ontology:constraint_victim(publishing_embargo, early_career_researchers).
narrative_ontology:constraint_victim(publishing_embargo, public_health_responsiveness).
narrative_ontology:constraint_victim(publishing_embargo, global_scientific_collaboration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Cannot share findings before publication without violating embargo terms; career advancement depends on journal placement; no exit option without severe cost. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(publishing_embargo, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH EMERGENCIES (SNARE) — Critical findings (pandemic treatments, vaccine efficacy data, disease vectors) are suppressed during embargo windows precisely when time-sensitive response is needed. Constrained by journal-driven publication timelines, not by scientific necessity. d≈0.88, f(d)≈1.35, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(publishing_embargo, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL RESEARCH COMMUNITY (TANGLED ROPE) — Benefits from peer-review coordination (quality control, citation standards, research validation). Simultaneously experiences extraction through delayed dissemination, journal access costs, and gatekeeping. d≈0.58, f(d)≈0.80, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL PUBLISHER (ROPE) — Embargo enforces temporal monopoly on distribution; coordinates peer review and editorial services; captures subscription revenue during embargo window. Experiences constraint as enabling their coordination function. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(publishing_embargo, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNAL EDITORIAL SYSTEM (PITON) — Embargo enforcement relies on behavioral compliance (researchers honor terms) rather than technical enforcement. The theater ratio is high: embargo persists through cultural inertia despite preprint alternatives (arXiv, bioRxiv, medRxiv). theater_ratio=0.68 reflects that much editorial gatekeeping is performative in the preprint era. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.27.
constraint_indexing:constraint_classification(publishing_embargo, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE COALITION (SCAFFOLD) — Preprint servers (arXiv, bioRxiv, medRxiv), open-access mandates, and registered reports provide alternative verification pathways with explicit sunset logic. Organized agents (funding agencies, universities, researcher collectives) are migrating away from embargo-dependent publication. d≈0.42, f(d)≈0.43, σ=1.2 → χ≈0.22. Theater declining as alternatives mature.
constraint_indexing:constraint_classification(publishing_embargo, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COORDINATION VIEW (ROPE) — From civilizational distance, embargo is a coordination mechanism solving real problems: maintaining publication incentives, funding review quality assurance, and establishing priority claims. The constraint appears as pure coordination without extractive machinery. However, structural data (ε=0.52, suppression=0.65) contradicts this — the engine will flag as false summit if observer naturalizes embargo as necessary coordination.
constraint_indexing:constraint_classification(publishing_embargo, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publishing_embargo_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publishing_embargo, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publishing_embargo, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publishing_embargo, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publishing_embargo, TR),
    TR >= 0.70.

:- end_tests(publishing_embargo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The embargo captures subscription value and enforces temporal monopoly on distribution, but this extraction is intertwined with genuine coordination functions (peer review, editorial curation, citation standards). The ε value reflects that extractiveness is not as pure as commercial gatekeeping (ε≈0.65-0.75) because journals do provide coordination value; but it is higher than pure coordination mechanisms (ε≤0.35) because the temporal monopoly extracts beyond the cost of coordination services. Suppression (0.65): High. Early-career researchers face severe career risks from violating embargo terms; preprint-first strategies still carry reputation costs in traditional disciplines; institutional repositories and researcher-direct dissemination remain socially risky despite technical feasibility. Public health response timelines are suppressed precisely when critical. Suppression is sustained by cultural/institutional norms, not by technical barriers. Theater ratio (0.68): High and increasing. Embargo enforcement relies on behavioral compliance rather than technical enforcement — researchers honor terms through cultural inertia despite preprint alternatives. The theater has increased over the measurement interval as preprint normalization has advanced (arXiv: 1991-present; bioRxiv: 2013-present; medRxiv: 2019-present) yet embargo cultural power persists. This trajectory is diagnostic of piton degradation: the constraint's functional necessity declines while performative maintenance increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence. The early-career researcher and public health systems see snare: they cannot exit, their findings are suppressed at the moment of maximum value, and the constraint extracts via career risk and institutional gatekeeping. The commercial publisher sees rope: embargo coordinates peer review and distribution, and the constraint enables their coordinating function. The global research community sees tangled rope: they benefit from quality assurance but experience extraction through delayed access. The journal editorial system sees piton: the ritual persists through inertia despite preprint alternatives making embargo technically obsolete. The open science coalition sees scaffold: preprint pathways provide a real sunset mechanism for embargo-dependent publication. The analytical observer risks seeing rope (necessary coordination) when structural data reveals tangled rope or snare (extraction disguised as coordination). The perspectival gap widens over time as preprint normalization advances — the constraint's functional justification erodes while its extractive effects persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Early career researchers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction from the most vulnerable agent. Public health emergencies: Victim + constrained (by publication timelines, not by research necessity) → d≈0.88, f(d)≈1.35. High extraction from a systemic victim. Global research community: Mixed beneficiary/victim + mobile (can migrate to preprints) → d≈0.58, f(d)≈0.80. Moderate extraction; increasing mobility reduces effective d. Commercial publishers: Beneficiary + arbitrage (can relax embargo, implement open access, negotiate alternative models) → d≈0.08, f(d)≈-0.10. Net beneficiary; low directionality toward victimhood. Journal editorial system: Institutional + constrained (cannot unilaterally abandon embargo without publisher pressure) → d≈0.35, f(d)≈0.32. Moderate directionality; institutional actor trapped by business model inertia. Open science coalition: Organized + mobile (can migrate researchers and funding to preprint-first norms) → d≈0.42, f(d)≈0.43. Low-moderate extraction; organized agency reduces effective d. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Observer perspective; risks naturalizing contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in embargo classification arises from the tension between the coordination function that justifies embargo (peer review, priority establishment, quality assurance) and the extractive gatekeeping that the constraint actually enforces (temporal monopoly, subscription capture, suppression of time-critical findings). RESOLUTION: The constraint is genuinely tangled rope, not a false rope classification masking snare. Evidence: (1) Peer review coordination is a real function — preprint servers do NOT eliminate the need for editorial curation and quality assurance; (2) Extraction is also real — the temporal monopoly extracts value beyond the cost of coordination services, and early-career researchers experience genuine career risk from circumventing embargo; (3) The two functions are structurally coupled — the publisher's ability to coordinate peer review is monetized through the subscription monopoly that embargo enables. The mandatrophy is resolved by recognizing that tangled rope is the correct type: the constraint both enables coordination AND extracts asymmetrically. The respectable question is not 'is embargo coordination or extraction?' but 'can the coordination function be decoupled from the extractive monopoly?' Evidence from the open science coalition suggests YES — preprint servers provide coordination without temporal monopoly. The scaffold perspective captures this: embargo is tangled rope with a real sunset mechanism as preprints mature. The theater_ratio (0.68) and rising trajectory indicate that embargo enforcement is increasingly performative precisely because the coordination function is becoming decoupled from the extractive mechanism. This is the signature of piton emergence from tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preprint_quality_sufficiency,
    'Do preprint servers provide equivalent quality assurance to traditional peer review for establishing priority and validating claims?',
    'Longitudinal analysis of preprint-to-publication correction rates vs journal-direct publication error rates; citation impact and replication outcomes correlated with publication pathway',
    'If equivalent: scaffold sunset is real — embargo is contingent institutional arrangement, not necessary. If inferior: embargo remains functionally justified, classification shifts toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preprint_quality_sufficiency, empirical, 'Whether preprints provide sufficient quality assurance without embargo').

omega_variable(
    time_criticality_variance,
    'What fraction of embargoed findings have time-criticality (pandemic response, public health, safety) that conflicts with embargo windows?',
    'Historical analysis of embargoed publications that subsequently became policy-relevant; comparison of time-to-publication vs policy decision windows in health, safety, and environmental domains',
    'If >20% of publications have time-criticality conflicts: snare classification dominates (suppression serves pure gatekeeping). If <5%: tangled_rope classification holds (extraction is byproduct of coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(time_criticality_variance, empirical, 'Fraction of embargoed research with time-critical applications').

omega_variable(
    enforcement_mechanism_degradation,
    'Is embargo enforcement becoming technically obsolete as researchers adopt preprint-first strategies and institutional repositories bypass journal-gated distribution?',
    'Tracking adoption rates of preprint-first workflows by institution and discipline; measurement of effective enforcement through citation lag and knowledge diffusion timing',
    'If enforcement degrading: piton classification confirmed — embargo persists through inertia, not functional necessity. If enforcement stable: institutional power prevents alternative pathways from fully replacing embargoed publication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_degradation, empirical, 'Whether embargo enforcement is degrading through preprint adoption').

omega_variable(
    publisher_coordination_necessity,
    'Are commercial publishers providing coordination functions (peer review, editorial curation, quality assurance) that cannot be replicated by open-science infrastructure?',
    'Comparative analysis of peer review quality, editorial standards, and citation impact across journal-published vs preprint-published research; cost-benefit analysis of publisher services vs open-science alternatives',
    'If publishers essential: rope classification justified — embargo is byproduct of necessary coordination. If replicable: tangled_rope or snare classification strengthened — embargo is pure rent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publisher_coordination_necessity, empirical, 'Whether publisher coordination functions are irreplaceable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publishing_embargo, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubemb_tr_t0, publishing_embargo, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pubemb_tr_t5, publishing_embargo, theater_ratio, 5, 0.54).
narrative_ontology:measurement(pubemb_tr_t10, publishing_embargo, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pubemb_be_t0, publishing_embargo, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pubemb_be_t5, publishing_embargo, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(pubemb_be_t10, publishing_embargo, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publishing_embargo, information_standard).
narrative_ontology:affects_constraint(publishing_embargo, journal_subscription_monopoly).
narrative_ontology:affects_constraint(publishing_embargo, academic_credential_gatekeeping).
narrative_ontology:affects_constraint(publishing_embargo, open_access_implementation).

% DUAL FORMULATION NOTE:
% The publishing embargo decomposes into two structurally distinct constraints: (1) peer_review_coordination (ε≈0.15, Rope) — the quality assurance and priority-establishment functions of editorial gatekeeping; (2) temporal_distribution_monopoly (ε≈0.58, Snare) — the embargo's suppression of preprint dissemination and enforcement of journal-exclusive distribution windows. These are empirically entangled through institutional practice but structurally separable. The embargo as a whole (ε=0.52) represents their hybrid: the coordination function justifies institutional enforcement, while the monopoly extracts value beyond coordination costs. Preprint normalization is decoupling them: coordination migrates to preprint peer commentary; monopoly persists through cultural/career inertia (piton degradation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(publishing_embargo, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
