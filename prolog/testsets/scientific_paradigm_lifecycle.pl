% ============================================================================
% CONSTRAINT STORY: scientific_paradigm_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scientific_paradigm_lifecycle, []).

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
 *   constraint_id: scientific_paradigm_lifecycle
 *   human_readable: The Crisis of a Scientific Paradigm
 *   domain: scientific/sociological
 *
 * SUMMARY:
 *   A scientific paradigm in crisis exhibits Kuhn's description of
 *   accumulated anomalies, loss of confidence in established theory, and
 *   proliferation of competing alternatives. However, the crisis phase is
 *   also a period of institutional extraction: the paradigm-defending elite
 *   use their control over journals, funding, and academic positions to
 *   suppress anomaly research, extend the life of the dominant theory, and
 *   delay the transition to a new paradigm. This constraint models that
 *   institutional extraction mechanism. The crisis is not merely a
 *   epistemological fact — anomalies accumulating, theory weakening — but a
 *   structural mechanism where gatekeepers extract career and funding
 *   benefits from suppressing disconfirming evidence. Extractiveness (0.58)
 *   reflects the real costs borne by anomaly researchers: blocked
 *   publications, reduced funding, reputational damage, and delayed career
 *   advancement. Suppression (0.68) reflects the institutional barriers:
 *   journal gatekeeping, funding body composition, tenure committee
 *   conservatism, and the risk of advocating for minority positions. Theater
 *   ratio (0.64) reflects that peer review during crisis phases becomes
 *   performative: reviewers are often paradigm defenders, anonymous review
 *   masks in-group evaluation, and the apparatus maintains legitimacy through
 *   procedure while functioning as gatekeeping. The constraint is a tangled
 *   rope at the base level because the paradigm infrastructure provides
 *   genuine coordination benefits (shared methods, common language,
 *   collective problem-solving) alongside the extraction mechanism.
 *   Researchers do benefit from paradigm membership — access to journals,
 *   funding, community — but the paradigm crisis phase weaponizes that access
 *   against researchers who observe disconfirming data.
 *
 * KEY AGENTS:
 *   - Early Anomaly Researchers: Primary victims (powerless/trapped) — publish anomalies, face rejection, career damage, no escape
 *   - Mid-Career Paradigm Skeptics: Secondary victims (moderate/constrained) — constrained by tenure/funding/reputation; have some agency but significant suppression
 *   - Paradigm Defending Elite: Primary beneficiaries (institutional/arbitrage) — control journals, funding, textbook canonicality; benefit from crisis extension
 *   - Journal Gatekeepers: Secondary beneficiaries (institutional/arbitrage) — maintain publication authority by filtering anomalies; preserve their role as arbiters of legitimacy
 *   - Established Funding Bodies: Secondary beneficiaries (institutional/arbitrage) — reward paradigm-consistent research; risk-averse allocation protects existing research programs
 *   - Scientific Community Governance Coalition: Organized reformers (organized/constrained) — arXiv, open peer review, alternative funding; building sunset mechanisms that reduce gatekeeping
 *   - Field Measurement Reliability: Victim (powerless/trapped) — contaminated by suppressed anomalies that should inform collective knowledge; bears cost of delayed paradigm shift
 *   - Emergent Theory Developers: Victims (moderate/constrained) — alternative theoretical frameworks lack funding, publication venues, and scientific legitimacy during suppression phase
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing crisis phase dynamics as inherent to scientific method
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scientific_paradigm_lifecycle, 0.58).
domain_priors:suppression_score(scientific_paradigm_lifecycle, 0.68).
domain_priors:theater_ratio(scientific_paradigm_lifecycle, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scientific_paradigm_lifecycle, tangled_rope).
narrative_ontology:human_readable(scientific_paradigm_lifecycle, "The Crisis of a Scientific Paradigm").
narrative_ontology:topic_domain(scientific_paradigm_lifecycle, "scientific/sociological").

domain_priors:requires_active_enforcement(scientific_paradigm_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, paradigm_defending_elite).
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, journal_gatekeepers).
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, established_funding_bodies).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, anomaly_researchers).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, field_measurement_reliability).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, emergent_theory_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY ANOMALY RESEARCHER (SNARE) — Trapped by publication gatekeeping, funding exclusion, and career risk of challenging consensus. Cannot exit without professional destruction. Bears maximum extraction cost while generating evidence that destabilizes the paradigm but receives no credit or career advancement. The constraint is a pure extraction mechanism against researchers who observe data contradicting dominant theory.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PARADIGM SKEPTIC (TANGLED ROPE) — Constrained by institutional position, tenure committees, grant review panels dominated by paradigm defenders. Benefits from the paradigm's infrastructure (journals, conferences, methodology) but faces extraction through reduced funding, publication bias, and reputational cost. Has some agency through alternative venues and collaboration networks, but significant suppression. Mixed coordination (uses paradigm's tools) and extraction (blocked from advancement).
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARADIGM DEFENDING ELITE (ROPE) — Benefits from journal editorship, funding authority, and textbook canonicality. Experiences the constraint as coordination: defending the paradigm against anomalies maintains their institutional position and research program viability. Net beneficiary with maximum arbitrage options — can publish freely, secure funding, shape discourse. Low effective extraction experienced because this agent's power aligns with constraint maintenance.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SCIENTIFIC COMMUNITY GOVERNANCE COALITION (SCAFFOLD) — Organized actors (preprint servers, open-peer-review platforms, alternative funding agencies) are building distributed verification mechanisms that reduce paradigm gatekeeping power. See the crisis phase as temporary coordination failure with sunset: post-paradigm scientific governance (decentralized publication, open data, broad peer engagement) diminishes the elite's extraction mechanism. The constraint has a sunset clause — formal mechanisms replacing gatekeeping reduce suppression over 15-25 year horizon.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW APPARATUS (PITON) — Traditional anonymous peer review in the crisis phase is largely performative: reviewers are often paradigm defenders evaluating anomaly research, and the ritual masks what is functionally an in-group veto mechanism. Review persists through institutional inertia despite low actual verification quality for paradigm-challenging work. Theater ratio high (0.64+) — the apparatus maintains legitimacy through procedure (blind review, editorial independence) while functioning as gatekeeping. Low chi despite high extraction: the apparatus sees its own function as degraded and sustains itself through theatrical compliance rather than efficacy.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some resistance to paradigm-threatening anomalies is inherent to how science operates: consensus immunity to disconfirming data is a structural feature of how large research communities protect shared frameworks. This perspective risks naturalizing the crisis phase as an immutable property of scientific method. However, the base properties contradict the mountain gate — extractiveness (0.58) exceeds the mountain ceiling (0.25), and suppression (0.68) exceeds the mountain ceiling (0.05). The engine will classify this as a false summit, revealing that the 'inherent to science' framing naturalizes what is actually a contingent institutional arrangement (concentrated funding, journal monopolies, career-advancement barriers).
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scientific_paradigm_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scientific_paradigm_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scientific_paradigm_lifecycle, TR),
    TR >= 0.70.

:- end_tests(scientific_paradigm_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts significant value from anomaly researchers. Early-career researchers who publish findings contradicting the paradigm face publication rejection, loss of funding, and career damage. The original v1.0 estimate of 0.72 was too high — not all anomaly research is suppressed equally, and some paradigm-defending journals accept contradictory data with appropriate caveats. The reduced value (0.58) reflects that suppression is systematic but not total, and that some researchers can succeed by working within alternative venues and networks. However, the average extraction is substantial: the suppression mechanism extracts career time, publishing opportunity, and funding from researchers whose data is inconvenient to the paradigm. Suppression (0.68): Significant institutional barriers exist to presenting paradigm-challenging anomalies. Publication bias against negative results and anomalies; reviewer gatekeeping; funding agencies weighted toward paradigm-consistent proposals; tenure committees rewarding paradigm-aligned publication records; conference abstract selection biased toward consensus research. Suppression is high but not total (0.68 vs 1.0) because alternative venues exist (preprints, specialty journals, conferences run by skeptics), though they have lower prestige and reach. Theater ratio (0.64): Peer review in the crisis phase exhibits high performativity. The apparatus maintains legitimacy through procedure (blind review, editorial independence, conflict-of-interest policies) while functioning as in-group gatekeeping. Reviewers are often paradigm defenders; the anonymity masks rather than eliminates bias; the editorial process is presented as neutral while the reviewer pool is self-selected for paradigm consistency. The theater has increased over the measurement interval (0.35 → 0.64) as crisis deepens and defenders become more defensive about the paradigm's viability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full spectrum of classification based on structural position. The paradigm defending elite see a Rope — they are coordinating research efforts and defending the shared framework that enables their work. Anomaly researchers see a Snare — they are trapped by gatekeeping and face career destruction for publishing inconvenient findings. Mid-career skeptics see a Tangled Rope — they benefit from paradigm infrastructure but are suppressed when they question consensus. The open-science coalition sees a Scaffold — preprint servers and open peer review provide alternative pathways that sunset the gatekeeping mechanism within 15-25 years. The peer review apparatus sees itself as a Piton — maintaining legitimacy through procedure while its functional efficacy has degraded. The civilizational analytical observer risks seeing a Mountain — crisis resistance is 'just how science works' — but this naturalizes what is actually a contingent institutional arrangement (concentrated funding, journal monopolies, career risk). The perspectival gap measures the fundamental asymmetry: the beneficiary experiences coordination, the victim experiences extraction, the observer risks naturalizing the extraction as law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position. The paradigm defending elite have low d (high f(d) close to institutional canon ≈ -0.12) — they are beneficiaries with maximum arbitrage options; the sigmoid maps beneficiary status to negative experienced extraction, meaning they experience the paradigm as subsidizing their careers. Anomaly researchers have high d (≈0.95, toward the powerless/trapped canonical value ≈ 1.42) — they are victims with no exit, trapped by career dependence on academic science; the sigmoid maps this to maximum experienced extraction. Mid-career skeptics have intermediate d (≈0.55-0.65) — they have some agency through alternative venues and collaboration, but significant constraint from institutional position. The derivation chain (beneficiary/victim + exit options → d → f(d) → χ) produces measured extraction values that vary by perspective: the elite experience effectively negative χ (the constraint subsidizes them), while anomaly researchers experience high χ (maximum extraction). This explains the perspectival gap without invoking observables or measurement basis — the extraction is real, structurally determined, and inherent to the asymmetry between gatekeepers and researchers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing genuine coordination (the paradigm does provide shared methodology, language, and problem-solving infrastructure) from exploitation (that infrastructure is weaponized against researchers who observe disconfirming data). The tangled rope classification correctly identifies both functions. During normal science (pre-crisis), the paradigm operates closer to pure Rope — researchers benefit from shared framework with minimal suppression. During crisis, the same framework becomes a Tangled Rope as gatekeepers use it to suppress anomalies. The constraint's extractiveness increases over the measurement interval (0.28 → 0.58) as the crisis deepens and defending becomes more aggressive. The theater ratio also increases (0.35 → 0.64), indicating that the coordination function declines in efficacy while the suppression function becomes more performative. This temporal trajectory clarifies that the constraint is not a pure Snare (which would have constant high extraction) but a degrading Rope that becomes increasingly extractive as crisis intensifies. The mandatrophy is resolved by recognizing that the classification changes over the paradigm lifecycle: early crisis is Rope, deepening crisis becomes Tangled Rope, and if gatekeeping completely fails then the field transitions to a different constraint (paradigm shift mechanism). The base classification (Tangled Rope) is appropriate for the current interval measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anomaly_significance_threshold,
    'What density of confirmed anomalies constitutes a genuine paradigm crisis versus normal puzzle-solving within established theory?',
    'Longitudinal analysis of anomaly publication rates, replication success rates across independent labs, and correlation with shifts in research funding and student enrollment',
    'If threshold is low (few anomalies): current crisis classification stands. If threshold is high (many anomalies required): crisis phase may be misdiagnosed, and constraint is actually a mechanism for healthy resistance to spurious disconfirmation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anomaly_significance_threshold, empirical, 'Threshold for distinguishing crisis from normal puzzle-solving').

omega_variable(
    gatekeeper_intentionality,
    'Are paradigm-defending gatekeepers deliberately suppressing anomalies or unconsciously filtering based on paradigm-consistent evaluation criteria?',
    'Qualitative analysis of reviewer comments, editorial decisions for papers with identical methods but different conclusions; randomized identity-obscured reviewer studies; archival analysis of decision processes during previous paradigm shifts',
    'If intentional: constraint is malicious snare requiring intervention. If unconscious: constraint is structural problem requiring institutional redesign (not behavior change). Affects appropriate policy response and legal/career-consequence framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeper_intentionality, conceptual, 'Whether suppression is intentional versus structural filtering').

omega_variable(
    alternative_framework_maturity,
    'Is the emerging theory sufficiently developed and predictive to function as a replacement paradigm, or is it still inchoate speculation?',
    'Comparative assessment of: novel predictions made by emerging framework vs paradigm-defending framework; match of novel predictions to subsequently obtained data; consistency across anomalies; ability to integrate prior paradigm''s empirical successes',
    'If mature: crisis phase is justified and paradigm shift is healthy. If inchoate: suppression may be rational epistemic conservatism, and constraint may protect field from pursuing false leads. Misdiagnosis affects whether crisis should be accelerated or moderated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framework_maturity, empirical, 'Development level of alternative theoretical framework').

omega_variable(
    funding_path_dependency,
    'Is suppression of anomalies primarily driven by path-dependent funding allocation (large grants to established paradigm research), or by epistemic conviction?',
    'Analysis of funding decisions when masked from researcher identity/affiliation; comparison of approval rates for identical research plans across funding bodies with different paradigm-defender representation; simulation of funding allocation under alternative institutional structures',
    'If path-dependent funding dominates: low-cost institutional restructuring (randomized review, rotating grant committees) could reduce suppression without changing scientific standards. If epistemic conviction dominates: suppression reflects genuine scientific disagreement and may be appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_path_dependency, empirical, 'Relative contribution of funding path-dependency versus epistemic conviction to suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scientific_paradigm_lifecycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paradigm_tr_t0, scientific_paradigm_lifecycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(paradigm_tr_t5, scientific_paradigm_lifecycle, theater_ratio, 5, 0.52).
narrative_ontology:measurement(paradigm_tr_t10, scientific_paradigm_lifecycle, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(paradigm_be_t0, scientific_paradigm_lifecycle, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(paradigm_be_t5, scientific_paradigm_lifecycle, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(paradigm_be_t10, scientific_paradigm_lifecycle, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scientific_paradigm_lifecycle, information_standard).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, paradigm_shift_mechanism).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, publication_bias_filtering).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, funding_allocation_conservatism).

% DUAL FORMULATION NOTE:
% The scientific paradigm crisis decomposes into three related constraints: (1) paradigm_lifecycle (this story) models the institutional extraction mechanism during crisis, (2) paradigm_shift_mechanism models the transition process when crisis becomes severe enough to force change, and (3) publication_bias_filtering and funding_allocation_conservatism are upstream constraints that enable the gatekeeping function. The paradigm lifecycle constraint has higher extractiveness (0.58) than the publication bias constraint (≈0.35) because the crisis phase weaponizes the bias mechanism against dissenters, amplifying the extraction effect. These constraints form a family linked through institutional causation: bias mechanisms → gatekeeping → paradigm crisis → shift dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
