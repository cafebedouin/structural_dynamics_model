% ============================================================================
% CONSTRAINT STORY: consensus_without_truth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consensus_without_truth, []).

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
 *   constraint_id: consensus_without_truth
 *   human_readable: The Social Cohesion Mirage
 *   domain: social/political/informational
 *
 * SUMMARY:
 *   The Social Cohesion Mirage is a constraint that emerges when a community
 *   achieves total or near-total agreement on a shared narrative that is
 *   factually false or decoupled from physical reality. This constraint
 *   operates through suppression of alternative claims (testimony, evidence,
 *   alternative interpretations) combined with institutional reinforcement of
 *   the false narrative as truth. The defining feature is that dissent is not
 *   merely disagreed with — it is treated as contamination, heresy, or
 *   pathology that must be eliminated to preserve social unity. The
 *   constraint extracts epistemic independence from its victims (dissenting
 *   agents, the external epistemic commons) and replaces it with narrative
 *   coherence. Unlike simple misinformation (which individuals can correct
 *   through exposure to contrary evidence), this constraint is enforced
 *   through social institutions and suppression mechanisms that degrade the
 *   very epistemic channels through which corrections could propagate. The
 *   theater ratio rises over time as verification institutions (peer review,
 *   fact-checking, scientific societies) become performers of legitimation
 *   rather than truth-seekers. Narrative custodians (political leadership,
 *   media, educational authorities) experience the constraint as a
 *   coordination mechanism that enables rapid collective action and political
 *   legitimacy. Dissenting individuals experience it as pure extraction with
 *   zero exit options. The external observer experiences it as contamination
 *   of the shared epistemic commons.
 *
 * KEY AGENTS:
 *   - Dissenting Individual: Primary victim (powerless/trapped) — bears social ostracism, institutional retaliation, economic exclusion; has zero exit options within the consensus zone
 *   - Epistemic Commons / External Observer: Secondary victim (moderate/constrained) — shared knowledge is degraded by false consensus claims; moderate power to contest but constrained by suppression mechanisms
 *   - Narrative Custodians: Primary beneficiaries (institutional/arbitrage) — political leadership, media, educational institutions that maintain and enforce the false narrative; experience it as coordination mechanism for social unity
 *   - Partially Captured Elite: Complex agent (powerful/mobile) — scientists, journalists, intellectuals initially capable of contesting the consensus but embedded in institutions rewarding conformity; experience mixed coordination and extraction
 *   - Verification Theater System: Institutional actor (institutional/arbitrage) — peer review, fact-checking, scientific institutions that have degraded into performers of legitimation; maintain illusion of truth-seeking while enforcing narrative
 *   - Counter-Epistemic Coalition: Organized opposition (organized/constrained) — independent media, decentralized verification networks, epistemic sanctuaries in other jurisdictions; building alternative verification pathways with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consensus_without_truth, 0.58).
domain_priors:suppression_score(consensus_without_truth, 0.72).
domain_priors:theater_ratio(consensus_without_truth, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consensus_without_truth, extractiveness, 0.58).
narrative_ontology:constraint_metric(consensus_without_truth, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(consensus_without_truth, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consensus_without_truth, snare).
narrative_ontology:human_readable(consensus_without_truth, "The Social Cohesion Mirage").
narrative_ontology:topic_domain(consensus_without_truth, "social/political/informational").

domain_priors:requires_active_enforcement(consensus_without_truth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consensus_without_truth, narrative_custodians).
narrative_ontology:constraint_beneficiary(consensus_without_truth, institutional_authorities).
narrative_ontology:constraint_victim(consensus_without_truth, epistemic_commons).
narrative_ontology:constraint_victim(consensus_without_truth, external_reality_correspondence).
narrative_ontology:constraint_victim(consensus_without_truth, dissenting_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING INDIVIDUAL (SNARE) — Powerless agent caught in consensus apparatus. Bearing full costs of nonconformity (social ostracism, economic exclusion, institutional retaliation) with zero exit options. Trapped within geographic/institutional bounds. Maximum experienced extraction — dissent triggers suppression mechanisms designed to enforce consensus coherence.
constraint_indexing:constraint_classification(consensus_without_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXTERNAL OBSERVER / EPISTEMIC COMMONS (SNARE) — Cannot exit from being contaminated by false consensus claims. Moderate power to contest (some epistemic autonomy, global communication) but constrained by the consensus's self-reinforcing verification mechanisms. Bears the cost of degraded shared knowledge. The constraint extracts truth from the commons and replaces it with coherent falsehood.
constraint_indexing:constraint_classification(consensus_without_truth, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NARRATIVE CUSTODIANS (ROPE) — Institutional actors (media, political leadership, educational authorities) who maintain consensus narrative. Experience the constraint as coordination mechanism: unified messaging reduces transaction costs of governance, enables rapid collective action, and provides political legitimacy. Beneficiaries with arbitrage options — can exit by abandoning narrative, but have zero incentive to do so. See consensus-without-truth as solving the collective action problem of social unity.
constraint_indexing:constraint_classification(consensus_without_truth, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARTIALLY CAPTURED ELITE (TANGLED ROPE) — Powerful agents (scientists, journalists, intellectuals) who initially had capacity to contest the false consensus but are now embedded in coordination structures that reward narrative conformity. Experience mixed coordination (unified messaging enables collaboration on large-scale projects) and extraction (career penalties for contradiction). Mobile enough to exit but face severe opportunity costs. See the constraint as both functional and corrupting.
constraint_indexing:constraint_classification(consensus_without_truth, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: VERIFICATION THEATER SYSTEM (PITON) — Institutions designed to verify truth (peer review, fact-checking, scientific method) have degraded into performers of legitimation for the consensus. Theater ratio is high: verification ceremonies proceed (journals publish, fact-checkers publish, institutions validate) but are decoupled from reality correspondence. The system persists through inertia — alternatives haven't fully replaced it, and institutional actors benefit from maintaining the theater. Piton classification derives from theater_ratio exceeding 0.68.
constraint_indexing:constraint_classification(consensus_without_truth, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COUNTER-EPISTEMIC COALITION (SCAFFOLD) — Organized agents (independent media, decentralized verification networks, epistemic sanctuaries in other jurisdictions) building alternative verification pathways. See the false consensus as a temporary institutional failure with a sunset: distributed networks, transparent data, and jurisdictional fragmentation are creating pathways that bypass consensus coherence and re-establish reality correspondence. Constraint experiences moderate effective extraction because coalition has agency and exit options are multiplying. Sunset logic: as communication decentralizes, the consensus enforcement mechanism weakens.
constraint_indexing:constraint_classification(consensus_without_truth, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consensus_without_truth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consensus_without_truth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consensus_without_truth, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consensus_without_truth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consensus_without_truth, TR),
    TR >= 0.70.

:- end_tests(consensus_without_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The false consensus extracts epistemic independence and reality correspondence from dissenting agents and the broader epistemic commons. The extraction rate is substantial — the constraint destroys the ability of contradictory claims to gain institutional credibility or propagate through mainstream channels. However, it is not the maximum (0.70+) because external reality always maintains some pressure (predictive failures, unexplained anomalies, heterodox communities outside the consensus zone). Suppression (0.72): High. The constraint requires active enforcement through multiple channels: social ostracism, institutional retaliation, career penalties, educational indoctrination, media gatekeeping, and normalization of consensus-enforcement as a civic duty. Dissent is not tolerated as disagreement but treated as contamination. Theater ratio (0.68): High. Verification institutions have degraded into performers of legitimation. Journals publish papers that confirm the narrative; fact-checkers debunk contradictions to the narrative; scientific societies validate the consensus; educational institutions teach it as settled fact. The performative apparatus persists because institutional actors benefit from maintaining it, even though its truth-seeking function has atrophied. The theater ratio has increased from 0.42 to 0.68 over the 30-year interval as the constraint has matured and institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival divide between beneficiaries and victims. The narrative custodians see Rope (coordination mechanism for social unity, enabling collective action). The dissenting individual sees pure Snare (maximum extraction with zero exit). The external observer sees Snare (epistemic commons contaminated, no escape). The partially captured elite see Tangled Rope (mixed coordination and extraction, but with real tension). The verification theater system see itself as Rope (serving epistemic function) but the analytical observer classifies it as Piton (degraded, performative, maintained by inertia). The counter-epistemic coalition see Scaffold (temporary institutional failure with sunset, as decentralized networks create alternative verification pathways). The perspectival gaps reveal that the constraint's coherence from the beneficiary perspective (Rope) is incompatible with its reality from the victim perspective (Snare). This gap is the diagnostic signature of the constraint: what appears as coordination to those enforcing it appears as pure extraction to those bearing costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from structural relationship to the constraint. Narrative custodians are beneficiaries with arbitrage exit options — they can abandon the false narrative and face institutional consequences, but have zero incentive to do so. Derived d is low (approximately 0.12-0.18) because they benefit and can exit. Dissenting individuals are victims with trapped exit — they cannot leave the consensus zone without severe costs, and cannot escape the suppression mechanisms. Derived d is high (approximately 0.92-0.98) because they are targets with no exit. The epistemic commons is a victim without agency — it is contaminated by false claims and cannot defend itself. Partially captured elites occupy the middle: powerful agents with mobile exit options who are victims of the coordination incentive structure. Their d is moderate (approximately 0.55-0.65). The verification theater system experiences arbitrage exit (institutional actors could abandon the legitimation function) but no incentive to do so, producing low d (approximately 0.10-0.20). The counter-epistemic coalition is organized with constrained (not mobile) exit — they are building alternatives but remain embedded in the broader ecosystem. Derived d is approximately 0.50-0.60. These directionality values feed into the sigmoid f(d) to produce effective extractiveness χ experienced by each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint distinguishes genuine coordination (Rope perspective of narrative custodians) from extraction (Snare perspective of dissenting individuals) by examining what happens to agents who exit the consensus. Narrative custodians claim they are solving a coordination problem (achieving social unity, enabling collective action). But the true test: can a dissenting individual exit and achieve the same coordination benefits independently? Answer: no. The dissenting individual who rejects the false consensus and seeks to coordinate with external reality gets excluded from the institutional structures that provide those benefits. This reveals that the 'coordination' is actually selective extraction: the narrative custodians have created a coordination mechanism that benefits themselves while extracting from those who reject their narrative. The constraint is a Snare dressed as Rope. The mandatrophy is resolved by noting that genuine coordination solves a collective action problem for all participants (or at least provides exit options for those who disagree). This constraint creates a collective action problem for dissenting agents (how to coordinate outside the consensus zone while facing suppression) while appearing to solve the general collective action problem (social unity). The resolution: classify from the perspective of someone who rejects the narrative. If they face extraction with no exit, it's a Snare. If they can exit with acceptable costs, it's Rope. Here, dissent is treated as pathology, which establishes the extraction mechanism. False natural law detection: The analytical observer might claim that false consensus is an 'inherent feature of human groups' (natural law), which would classify the constraint as Mountain. The structural data contradicts this: the constraint requires active suppression (institutional enforcement, media gatekeeping, social ostracism). Without suppression, consensus degrades. Therefore, it is not a natural law but an artifact of institutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_stability_threshold,
    'What percentage of the population must accept the false consensus for suppression mechanisms to sustain it? Is there a tipping point below which consensus collapse accelerates?',
    'Historical analysis of consensus collapse events (Flat Earth → Spherical, Geocentrism → Heliocentrism, Soviet reliability mythology); measurement of critical mass thresholds in contemporary false consensuses',
    'If threshold < 60%: consensus is unstable and will degrade rapidly. If threshold > 75%: consensus can persist indefinitely through suppression of minority. Affects whether constraint is Snare (durable) or Scaffold (collapsing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_stability_threshold, empirical, 'Critical mass threshold for consensus maintenance').

omega_variable(
    reality_contact_degradation_rate,
    'How quickly does a false consensus degrade the shared correspondence to external reality? Is the extraction rate of ''truth capital'' measurable?',
    'Measurement of predictive failures over time: decisions made under the false consensus that fail to achieve intended effects; accumulation of unexplained anomalies; gap between consensus predictions and observable outcomes',
    'If degradation is rapid (5-10 years): consensus becomes brittle and self-refuting. If slow (30+ years): extraction can persist across generations. Affects whether suppression mechanisms are sufficient to maintain the snare or whether reality friction will break it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reality_contact_degradation_rate, empirical, 'Rate of reality correspondence degradation').

omega_variable(
    enforcer_internalization_mechanism,
    'Do suppression mechanisms work primarily through external coercion or through internalization of consensus norms? If internalized, can suppression be bypassed by denying its psychological reality?',
    'Analysis of dissent suppression methods; measurement of psychological vs material costs of nonconformity; study of whether consensus-aware agents (who know it''s false but enforce it anyway) represent stable equilibrium or a precursor to collapse',
    'If primarily external: suppression is brittle and vulnerable to coordination of dissent. If primarily internalized: suppression is robust but produces cognitive dissonance that accumulates. Affects trajectory of the constraint and viability of counter-epistemic coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcer_internalization_mechanism, conceptual, 'Whether suppression is externalized or internalized').

omega_variable(
    institutional_dependency_inversion,
    'Are the narrative custodians actually dependent on the false consensus, or do they maintain it instrumentally? If dependent, can they exit without institutional collapse?',
    'Analysis of institutional incentive structures; study of what occurs when consensus shifts (do institutions collapse or adapt rapidly?); examination of institutions that have survived consensus reversals',
    'If truly dependent: beneficiaries are actually trapped victims (misclassified). Constraint would downgrade to a symmetric Tangled Rope or mutual Snare. If instrumental: beneficiaries are genuine arbitrage-enabled agents. Affects directionality derivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_dependency_inversion, empirical, 'Whether institutions depend on or merely benefit from false consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consensus_without_truth, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cwt_tr_t0, consensus_without_truth, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cwt_tr_t15, consensus_without_truth, theater_ratio, 15, 0.55).
narrative_ontology:measurement(cwt_tr_t30, consensus_without_truth, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(cwt_be_t0, consensus_without_truth, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cwt_be_t15, consensus_without_truth, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cwt_be_t30, consensus_without_truth, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consensus_without_truth, information_standard).
narrative_ontology:affects_constraint(consensus_without_truth, epistemic_gatekeeping).
narrative_ontology:affects_constraint(consensus_without_truth, institutional_legitimation_theater).
narrative_ontology:affects_constraint(consensus_without_truth, dissent_suppression_apparatus).

% DUAL FORMULATION NOTE:
% The Social Cohesion Mirage decomposes into three structurally distinct constraints: (1) Epistemic Gatekeeping (ε≈0.35, Rope/Snare hybrid) — control of what claims can be published or broadcast; (2) Institutional Legitimation Theater (ε≈0.42, Piton) — degradation of verification institutions into performance; (3) Dissent Suppression Apparatus (ε≈0.65, Snare) — enforcement mechanisms that eliminate alternative voices. This story models the unified constraint as experienced from community-level perspective. Decomposed stories model specific institutional mechanisms. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consensus_without_truth, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
