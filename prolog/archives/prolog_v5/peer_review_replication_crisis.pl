% ============================================================================
% CONSTRAINT STORY: peer_review_replication_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peer_review_replication_crisis, []).

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
 *   constraint_id: peer_review_replication_crisis
 *   human_readable: Peer Review Replication Crisis: Institutional Coordination and Extraction Hybrid
 *   domain: scientific_methodology/institutional_epistemology
 *
 * SUMMARY:
 *   The peer review replication crisis represents a structural constraint
 *   that coordinates disciplinary knowledge production while simultaneously
 *   extracting career and publication benefits from novel-claim originators
 *   and gatekeeping against replication researchers. Over the past 20 years,
 *   the constraint has degraded: theater ratio has increased from 0.55 to
 *   0.78 as publication volumes have accelerated while reviewer time per
 *   manuscript has declined; base extractiveness has increased from 0.38 to
 *   0.58 as positive-result bias has intensified and replication funding has
 *   remained suppressed. This is a classic tangled rope: genuine coordination
 *   function (error filtering, quality signaling) coupled with asymmetric
 *   extraction (novel claims rewarded, replications penalized). The system
 *   exhibits all six DR types from different structural positions, making it
 *   a diagnostic exemplar for how institutional constraints embed multiple
 *   contradictory perspectives. From the replication researcher's view, it is
 *   a snare with no exit. From the journal publisher's view, it is a rope
 *   coordinating expertise and reputation. From the peer review institution's
 *   view, it is a piton — a degraded ritual persisting through inertia. From
 *   the open science coalition's view, it is a scaffold with an achievable
 *   sunset. The crisis itself emerges not from peer review's inherent design
 *   but from the compression of four structural tensions: (1) exponential
 *   growth in submission volume without proportional growth in reviewer
 *   capacity, (2) journal economics that reward impact metrics over accuracy,
 *   (3) academic career structures that incentivize novelty over
 *   verification, and (4) institutional inertia that treats peer review as an
 *   unchallengeable legitimacy marker.
 *
 * KEY AGENTS:
 *   - Replication Researchers: Primary victims (powerless/trapped) — face career penalties, limited publishing outlets, and funding scarcity for replication work; bear full cost of system while gaining minimal career reward
 *   - Early-Career Scientists: Primary victims (powerless/trapped) — pressured by tenure-track evaluation periods and postdoc time limits to produce novel positive results; cannot afford career risk of replication work
 *   - Novel-Claim Originators: Primary beneficiaries (institutional/arbitrage) — capture citation advantage, first-mover prestige, and publication priority during verification window; can exit to alternative venues if rejected
 *   - High-Impact Journal Publishers: Secondary beneficiaries (institutional/arbitrage) — benefit from novel-claim concentration, higher citation counts, premium pricing and prestige; coordinate discipline while extracting gatekeeping rents
 *   - Disciplinary Research Community: Mixed (moderate/constrained) — benefits from quality coordination function but constrained by publication timelines, reviewer availability, and selection bias against replication
 *   - Funding Agencies: Mixed (institutional/constrained) — coordinate research allocation and quality control while simultaneously extracting through gatekeeping and bias toward novel claims; constrained by legacy metrics
 *   - Peer Review Institution: Performative (institutional/constrained) — persists through institutional inertia and regulatory assumptions rather than functional verification capacity; increasingly theatrical as volume exceeds reviewer capacity
 *   - Open Science Coalition: Organized reformers (organized/constrained) — building alternative pathways (preprints, registered reports, open data) with sunset logic; retain agency but constrained by institutional legacy metrics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent limits to scientific progress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peer_review_replication_crisis, 0.58).
domain_priors:suppression_score(peer_review_replication_crisis, 0.65).
domain_priors:theater_ratio(peer_review_replication_crisis, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peer_review_replication_crisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(peer_review_replication_crisis, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(peer_review_replication_crisis, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peer_review_replication_crisis, tangled_rope).
narrative_ontology:human_readable(peer_review_replication_crisis, "Peer Review Replication Crisis: Institutional Coordination and Extraction Hybrid").
narrative_ontology:topic_domain(peer_review_replication_crisis, "scientific_methodology/institutional_epistemology").

domain_priors:requires_active_enforcement(peer_review_replication_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peer_review_replication_crisis, high_impact_journal_publishers).
narrative_ontology:constraint_beneficiary(peer_review_replication_crisis, novel_claim_originators).
narrative_ontology:constraint_beneficiary(peer_review_replication_crisis, citation_advantage_seekers).
narrative_ontology:constraint_victim(peer_review_replication_crisis, replication_researchers).
narrative_ontology:constraint_victim(peer_review_replication_crisis, field_epistemic_commons).
narrative_ontology:constraint_victim(peer_review_replication_crisis, early_career_scientists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPLICATION RESEARCHER (SNARE) — Trapped in a system where publishing negative or replication results carries severe career penalties. Limited journal outlets accept replication studies. Funding agencies prioritize novel findings. Career advancement depends on citations, which replication work generates sparsely. Cannot exit without abandoning research career. Experiences maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(peer_review_replication_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER SCIENTISTS (SNARE) — Trapped by pressure to produce novel, positive-result publications within limited postdoc windows and during tenure-track evaluation periods. Publishing replication failures risks being labeled as unoriginal or unsuccessful. Career survival depends on securing grants and publications that peer review gatekeepers control. Exit would require abandoning scientific career. Experiences structural extraction with suppression of alternative pathways.
constraint_indexing:constraint_classification(peer_review_replication_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DISCIPLINARY RESEARCH COMMUNITY (TANGLED ROPE) — Benefits from peer review's coordination function: quality control, error detection, and epistemic filtering. Also bears extraction costs: publication delays, gatekeeping by journal editors and peer reviewers, bias toward positive results and novel claims over replication. Constrained by publication timelines and reviewer availability but retains some collective agency through open science initiatives and alternative publication venues. Experiences genuine mixed coordination-extraction dynamic.
constraint_indexing:constraint_classification(peer_review_replication_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-IMPACT JOURNAL PUBLISHERS (ROPE) — Benefits from peer review's coordination function: establishes reputation, attracts submissions, commands high subscription and access fees. Selection bias toward novel, positive results maximizes citation counts and impact metrics. Can arbitrage by offering fast-track publication for confirmatory studies, shifting between venues, or negotiating open-access models. Experiences the system as coordination: peer review provides the filtering service that justifies premium pricing and prestige. No forced participation.
constraint_indexing:constraint_classification(peer_review_replication_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE PEER REVIEW INSTITUTION (PITON) — The peer review process itself has become largely performative. Theater ratio (0.78) reflects that peer review claims to verify reproducibility and catch errors, but actual error detection is limited by reviewer expertise, time constraints, lack of raw data access, and inability to physically replicate experiments. The institution persists through regulatory inertia (tenure committees, funding agencies, hiring committees assume peer review means quality), not through functional verification capacity. Reviewers spend median 3-5 hours per paper; actual replication takes months or years. Theater has increased as journals have accelerated publication timelines while increasing manuscript volume per reviewer.
constraint_indexing:constraint_classification(peer_review_replication_crisis, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE AND PREPRINT MOVEMENT (SCAFFOLD) — Organized agents (preprint servers, open data initiatives, registered reports, journal transparency mandates) are constructing alternative verification pathways with explicit sunset logic. ArXiv, bioRxiv, medRxiv enable rapid dissemination and distributed scrutiny. Registered reports pre-commit methodology, reducing selection bias. Open-data and open-code mandates enable post-publication verification. These mechanisms have lower theater and enable faster feedback loops than traditional peer review. Organized agents retain agency but remain constrained by institutional inertia — most hiring and funding decisions still privilege peer-reviewed publication. Sunset timeline: 15-25 years for open-science norms to fully replace traditional gatekeeper model.
constraint_indexing:constraint_classification(peer_review_replication_crisis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale and universal scope, this perspective risks treating the replication crisis as an inherent property of complex science: 'Discovery claims always outpace verification; that's just how science works.' This naturalizes what is actually a contingent institutional arrangement (publication incentives, career structures, journal economics, review resource constraints). The engine's false summit detector will flag this as naturalization: the mountain classification fails when structural data reveals controllable institutional factors driving the crisis.
constraint_indexing:constraint_classification(peer_review_replication_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: FUNDING AGENCIES AND GRANT REVIEW SYSTEMS (TANGLED ROPE) — Coordinate research allocation and quality control (genuine coordination function) while simultaneously extracting through gatekeeping and bias toward novel, fundable claims over replication and verification work. Funding committees face resource constraints that force selectivity; peer review of grants reinforces novel-result bias. Some agency (growing replication funding, registered reports support, open science initiatives) but remains constrained by legacy metrics (citation counts, impact factors) that reward novel positive results. Experiences mixed coordination-extraction dynamic distinct from journal publishers.
constraint_indexing:constraint_classification(peer_review_replication_crisis, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peer_review_replication_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peer_review_replication_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peer_review_replication_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peer_review_replication_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peer_review_replication_crisis, TR),
    TR >= 0.70.

:- end_tests(peer_review_replication_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system extracts meaningful career and publication value from novel-claim originators while suppressing replication researchers' advancement. The extraction is not total because some replication funding exists, some journals publish negative results, and open science alternatives are emerging. The trajectory from 0.38 to 0.58 over 20 years reflects intensifying positive-result bias as publication volume has accelerated. Suppression (0.65): High. Replication researchers face structural barriers: limited journal outlets (most top journals reject low-acceptance-rate papers), funding bias toward novel work, citation penalty for negative results, and career risk during evaluation periods. These are not absolute (some journals now publish replications, some funding exists) but are pervasive and systematic. Theater ratio (0.78): Very high. Peer review claims to verify reproducibility and catch errors, but actual verification capacity is limited. Median time per review is 3-5 hours; actual replication takes months or years. Reviewers lack access to raw data, cannot verify computational code, cannot inspect lab equipment or protocols. Theater has increased dramatically: 20 years ago journals published fewer papers and reviewers had more time; now the ratio of papers-to-reviewer-hours has tripled. The performative element is increasingly obvious — the theater is rising not because peer review is becoming more theatrical, but because the constraints on reviewer time are making the gap between review claims and review reality more apparent.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap centers on whether peer review functions as genuine coordination or hidden extraction. Novel-claim originators and journal publishers experience the system as rope: legitimate, beneficial coordination that enables expertise matching and prestige signaling. Replication researchers and early-career scientists experience the same system as snare: coercive gatekeeping with career penalties and no exit. The disciplinary community occupies middle ground: tangled rope. Both benefits and costs are real. The peer review institution sees itself as a piton: reviewers recognize the theater but lack alternative procedures and face institutional expectations of peer review. The open science coalition sees a scaffold: the constraint can be superseded within a generation if current alternative mechanisms mature. The false mountain perspective naturalizes this arrangement as inherent to science, masking contingent institutional factors (career incentive structures, journal economics, review resource allocation) that could be redesigned.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline accounts for how different agents experience the same constraint through different f(d) functions. A novel-claim originator (institutional/arbitrage/beneficiary) has low d from the beneficiary-arbitrage combination, producing f(d) ≈ 0.02 — they experience the constraint almost entirely as coordination benefit, with minimal personal extraction. A replication researcher (powerless/trapped/victim) has d ≈ 0.95, producing f(d) ≈ 1.42 — they experience maximum extraction. Both are measured against the same ε = 0.58 base extractiveness. The chi formula χ = ε × f(d) × σ(S) produces: beneficiary chi ≈ 0.58 × 0.02 × 1.2 ≈ 0.01 (perceived as pure coordination), versus victim chi ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (perceived as severe extraction). Same constraint, radically different experienced reality. This computation models how institutional power differentials create perception gaps: the beneficiary literally does not experience the extraction that the victim bears, not because they are irrational but because the constraint structure delivers different f(d) to different positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint genuinely exhibits tangled rope structure: (1) Coordination function is real — peer review does catch some errors, does provide quality signaling, does coordinate expertise allocation across disciplines. (2) Asymmetric extraction is also real — novel-claim originators benefit while replication researchers are penalized; high-impact journals extract gatekeeping rents; career structures incentivize novelty over accuracy. (3) Active enforcement is required — without institutional structures that presume peer review's legitimacy and enforce its gatekeeping power, both the coordination and extraction mechanisms would collapse. Removing peer review entirely (false negative) would eliminate valuable quality signals. Treating it as pure coordination (false positive) ignores documented selection biases and career penalties for replication work. The tangled rope classification accurately captures that both functions are structural, not one masquerading as the other. The constraint has degraded from stronger coordination with manageable extraction (20 years ago) toward weaker coordination (theater rising) with intensifying extraction (novel-result bias increasing). The scaffold perspectives suggest a real exit pathway: open science mechanisms can recover coordination function while reducing extraction, with an estimated 15-25 year sunset horizon. The false natural law perspective reflects institutional identity lock: academics and institutions defend peer review as unchallengeable legitimacy marker, not as a functional mechanism that could be improved or replaced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_cost_threshold,
    'What is the true cost-benefit crossover point where conducting replication becomes economically rational for funding agencies and institutions?',
    'Economic analysis of replication cost vs field-wide cost of operating on false positives; calculation of break-even false positive rate that justifies mandatory replication budget',
    'If crossover < 5% false positive rate: current 0.65 suppression of replication funding is economically irrational and easily reversible. If crossover > 20%: suppression may reflect genuine resource scarcity, requiring structural redesign rather than norm change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_cost_threshold, empirical, 'Economic threshold where replication becomes cost-justified').

omega_variable(
    selection_bias_mechanism_decomposition,
    'Is the positive-result bias primarily a gate-keeping mechanism (journals and reviewers actively suppress negatives) or a supply-side effect (researchers self-select toward novel positive results due to career incentives)?',
    'Historical comparison of rejection rates for replication and negative-result papers between high-impact and low-impact journals; analysis of author behavior when publishing incentives change; longitudinal study of field response to registered reports and pre-registration',
    'If primarily gatekeeping: system can be fixed by norm change and journal policy. If primarily supply-side career incentives: requires structural change in academic hiring, funding, and evaluation systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_bias_mechanism_decomposition, empirical, 'Whether positive bias is gatekeeping or career incentive driven').

omega_variable(
    peer_review_error_detection_capacity,
    'What is the actual error detection rate of peer review (sensitivity and specificity) for catching false positives, methodological flaws, and fabrication in complex experimental and computational work?',
    'Post-hoc analysis of papers with documented replication failures; comparison with false positive rates in preprint and non-peer-reviewed work; expert assessment of whether peer reviewers'' comments would have prevented failures',
    'If sensitivity < 20%: peer review provides minimal quality control and is largely performative (piton classification correct). If sensitivity > 50%: peer review functions as meaningful coordination mechanism (rope classification from beneficiary perspective justified). Result directly determines whether theater_ratio should be higher or lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peer_review_error_detection_capacity, empirical, 'Peer review error detection rate and specificity').

omega_variable(
    preprint_distributed_scrutiny_effectiveness,
    'Does distributed preprint scrutiny (comments, discussion, post-publication review) actually achieve error detection comparable to or better than traditional peer review, particularly for complex experimental claims?',
    'Longitudinal tracking of error rates in preprints with high comment activity vs journal-published work; analysis of timing: do preprint comments catch errors before or after fabrication? Do community scrutiny patterns differ by field discipline?',
    'If effective: scaffold perspective is structurally sound and sunset is achievable within 15-20 years. If ineffective for specialized claims: open-science model solves visibility but not verification, maintaining suppression of replication resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_distributed_scrutiny_effectiveness, empirical, 'Whether preprint communities effectively detect errors').

omega_variable(
    institutional_identity_lock_in_peer_review,
    'Have academic institutions and journal publishers internalized peer review as a core component of institutional identity and legitimacy, making it resistant to replacement even if alternatives prove functionally superior?',
    'Analysis of institutional rhetoric and policy statements around peer review; study of resistance patterns when preprints and open science alternatives are offered; examination of whether institutions defend peer review based on function or on identity/prestige claims',
    'If primarily identity-locked: institutions will resist functional alternatives due to cognitive capture rather than structural necessity. Sunset timeline extends to 25+ years requiring cultural shift, not just technical innovation. If primarily functional: transition to open science can accelerate if alternatives demonstrably work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_in_peer_review, conceptual, 'Whether peer review defense is identity-based or function-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peer_review_replication_crisis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prrc_tr_t0, peer_review_replication_crisis, theater_ratio, 0, 0.55).
narrative_ontology:measurement(prrc_tr_t10, peer_review_replication_crisis, theater_ratio, 10, 0.68).
narrative_ontology:measurement(prrc_tr_t20, peer_review_replication_crisis, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(prrc_be_t0, peer_review_replication_crisis, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prrc_be_t10, peer_review_replication_crisis, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(prrc_be_t20, peer_review_replication_crisis, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peer_review_replication_crisis, enforcement_mechanism).
narrative_ontology:affects_constraint(peer_review_replication_crisis, publication_bias_positive_results).
narrative_ontology:affects_constraint(peer_review_replication_crisis, academic_career_tenure_incentives).
narrative_ontology:affects_constraint(peer_review_replication_crisis, journal_impact_factor_metric_goodhart).
narrative_ontology:affects_constraint(peer_review_replication_crisis, research_funding_novelty_bias).

% DUAL FORMULATION NOTE:
% The peer review replication crisis decomposes into at least two structurally distinct constraints with different ε values: (1) peer_review_replication_crisis (this story, ε=0.58) — the institutional coordination-extraction hybrid as primary phenomenon, (2) peer_review_theater_ratio_degradation (ε=0.40, separate story) — the increase in performative review as manuscript volume accelerates. The current story focuses on extraction and beneficiary-victim dynamics; the theater story focuses on institutional inertia and procedural degradation. Both should be linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(peer_review_replication_crisis, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
