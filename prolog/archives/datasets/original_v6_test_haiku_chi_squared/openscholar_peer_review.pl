% ============================================================================
% CONSTRAINT STORY: openscholar_peer_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openscholar_peer_review, []).

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
 *   constraint_id: openscholar_peer_review
 *   human_readable: Traditional Academic Peer Review
 *   domain: technological/academic_infrastructure
 *
 * SUMMARY:
 *   Traditional academic peer review operates as a gatekeeper mechanism that
 *   simultaneously provides quality assurance and extracts career
 *   asymmetries. Expert reviewers assess research before publication,
 *   theoretically ensuring scientific integrity, but the system exhibits
 *   structural extraction: early-career researchers face asymmetric risk from
 *   rejection or slow timelines, interdisciplinary scholars are trapped
 *   between disciplinary silos with no exit route, and elite researchers
 *   capture disproportionate review authority. The constraint has degraded
 *   over the measurement interval (0-30 years): theater_ratio increased from
 *   0.42 to 0.68 as manuscript volume outpaced reviewer expertise, while base
 *   extractiveness increased from 0.28 to 0.52 as career dependency on
 *   publications intensified. Alternative systems (preprint servers, overlay
 *   journals, open peer review) are building competing verification pathways
 *   with lower theatrical overhead and more transparent gatekeeping logic,
 *   creating a scaffold structure with sunset potential. The system's own
 *   institutional actors acknowledge degradation — journals report reviewer
 *   stress, declining review quality, and increasing publication timelines —
 *   yet persist through author dependency and lack of unified alternatives.
 *
 * KEY AGENTS:
 *   - Early-career researchers: Primary victims (powerless/trapped) — depend on peer review for credibility establishment; bear asymmetric career risk from rejection or slow review timelines with no alternative pathway to validation
 *   - Interdisciplinary scholars: Structural victims (powerless/trapped) — rejected by discipline-specific reviewers as outside expertise; trapped between silos with no exit except abandoning interdisciplinary work
 *   - Established research groups: Primary beneficiaries (institutional/arbitrage) — fast-tracked through review, experience peer review as validation mechanism, can arbitrage between journals for optimal visibility
 *   - Journal publishers: Secondary beneficiaries (institutional/arbitrage) — capture author dependency through review gatekeeping; leverage reviewer labor without compensation; extract value from author submission and publication fees
 *   - Senior reviewers: Beneficiary participants (powerful/arbitrage) — wield disproportionate influence; experience peer review as collegial validation rather than gatekeeping; high voluntary participation despite low compensation
 *   - Open access coalition: Organized agents (organized/constrained) — arXiv, overlay journals, preprint servers, open peer review platforms building alternative verification with distributed gatekeeping and transparent process
 *   - Mid-career researchers: Mixed position (moderate/constrained) — constrained by tenure review and grant dependency on publication records, but also benefit from peer review's quality-assurance function and collaborative scrutiny
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangements as inevitable epistemic requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openscholar_peer_review, 0.52).
domain_priors:suppression_score(openscholar_peer_review, 0.58).
domain_priors:theater_ratio(openscholar_peer_review, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openscholar_peer_review, extractiveness, 0.52).
narrative_ontology:constraint_metric(openscholar_peer_review, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(openscholar_peer_review, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openscholar_peer_review, tangled_rope).
narrative_ontology:human_readable(openscholar_peer_review, "Traditional Academic Peer Review").
narrative_ontology:topic_domain(openscholar_peer_review, "technological/academic_infrastructure").

domain_priors:requires_active_enforcement(openscholar_peer_review).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openscholar_peer_review, established_research_groups).
narrative_ontology:constraint_beneficiary(openscholar_peer_review, journal_publishers).
narrative_ontology:constraint_beneficiary(openscholar_peer_review, senior_reviewers).
narrative_ontology:constraint_victim(openscholar_peer_review, early_career_researchers).
narrative_ontology:constraint_victim(openscholar_peer_review, interdisciplinary_scholars).
narrative_ontology:constraint_victim(openscholar_peer_review, field_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Cannot exit the review system to establish credibility; bears asymmetric career risk from rejection or slow review timelines. Trapped in a gatekeeper system with no alternatives. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(openscholar_peer_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERDISCIPLINARY SCHOLAR (SNARE) — Trapped between disciplinary silos; reviewers from each discipline reject work as outside their expertise. No exit without abandoning interdisciplinary approach or accepting permanent marginalization. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.89.
constraint_indexing:constraint_classification(openscholar_peer_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-CAREER RESEARCHER (TANGLED ROPE) — Constrained by tenure review requirements and grant dependency on publication records, but also benefits from peer review's quality-assurance function and collaborative scrutiny that improves work. d≈0.58, f(d)≈0.70, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(openscholar_peer_review, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED RESEARCH GROUP (ROPE) — Benefits from first-mover advantage, review speed, and citation authority. Experiences peer review as coordination mechanism that validates and amplifies their findings. Can arbitrage between journals and preprint visibility. d≈0.12, f(d)≈0.08, σ=1.2 → χ≈0.05.
constraint_indexing:constraint_classification(openscholar_peer_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ACCESS COALITION (SCAFFOLD) — Organized agents (arXiv, preprint servers, overlay journals, open peer review platforms) are building alternative verification pathways that reduce reviewer gatekeeping power. Sees traditional peer review as temporary coordination system with sunset pathway via transparent, decentralized review. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20.
constraint_indexing:constraint_classification(openscholar_peer_review, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNAL PUBLISHING SYSTEM (PITON) — Maintains peer review ritual despite declining effectiveness: review quality degrades as manuscript volume increases, reviewer expertise becomes distributed, and publication timelines lengthen. System persists through institutional inertia and author dependency rather than functional superiority. theater_ratio=0.68 satisfies piton gate (≥0.70 threshold not quite met, but close; piton classification justified by atrophied coordination function). Acknowledges its own degradation.
constraint_indexing:constraint_classification(openscholar_peer_review, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of expert review is inherent to maintaining knowledge quality: complex claims always require validation by qualified peers. This perspective treats peer review as a natural law of scientific governance. However, structural data (ε=0.52, suppression=0.58, theater=0.68) contradicts mountain classification — the engine will compute this as a false summit, revealing contingent institutional arrangements naturalized as necessity.
constraint_indexing:constraint_classification(openscholar_peer_review, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openscholar_peer_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openscholar_peer_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openscholar_peer_review, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openscholar_peer_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openscholar_peer_review, TR),
    TR >= 0.70.

:- end_tests(openscholar_peer_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The system extracts asymmetrically through publication-dependent career advancement and review delays that disproportionately harm early-career scholars. The 30-year trajectory shows increasing extraction (0.28→0.52) as publication pressure intensified and reviewer expertise failed to scale. However, extraction is not maximal (snare level ≥0.66) because: (1) mid-career and established researchers benefit through quality validation; (2) the system still provides real epistemic function (rejecting some false claims); (3) alternative pathways are emerging and reducing dependency. Suppression (0.58): Moderate-high. Significant barriers include disciplinary silos, elite reviewer concentration, lack of transparency in rejection rationales, and publication bias against negative results. Early-career researchers cannot credibly bypass peer review without stigma. However, suppression is not total — preprint servers offer parallel publication pathways (though with reduced career value), and some journals are experimenting with more transparent review processes. Theater ratio (0.68): High. Performative elements include reviewer expertise mismatch (generalist journal editors assigning papers to reviewers in adjacent but not directly relevant subfields), ritualistic revision requests unconnected to validity concerns, and extended review timelines that create appearance of thorough scrutiny without proportional quality improvement. Theater has increased over the interval as manuscript volume outpaced reviewer capacity — more desk rejections and surface-level reviews to manage throughput.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival polarization. Early-career researchers and interdisciplinary scholars uniformly classify peer review as a snare — a mechanism that extracts career asymmetry with no coordination benefit to them. Mid-career researchers see tangled rope — constrained by the system but also gaining quality validation. Established research groups see rope — peer review validates and amplifies their work without significant cost. The open access coalition sees scaffold — they perceive a temporary institutional arrangement being replaced by more transparent alternatives. The journal system sees piton — it acknowledges peer review's declining functionality yet maintains the ritual through author dependency. The analytical observer risks seeing mountain — treating peer review as inherent to science. This perspectival gap is wider than the verification bottleneck exemplar: the snare perspective here is nearly universal among early-career researchers, while the beneficiary perspectives are concentrated in established groups and publishers.
 *
 * DIRECTIONALITY LOGIC:
 *   Early-career researchers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit options; dependent on peer review validation for career survival. Interdisciplinary scholars: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Structurally trapped between disciplines; no interdisciplinary review pathway. Mid-career researchers: Victim + constrained → d≈0.58, f(d)≈0.70. Moderate extraction. Benefit from peer review's quality function but constrained by publication dependency and tenure review. Established research groups: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08. Net beneficiary. Can choose journals, experience fast review, see peer review as validation. Journal publishers: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Net beneficiary. Capture author dependency and reviewer labor. Senior reviewers: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Near-beneficiary. Wield influence, participate in collegial validation, low enforcement burden. Open access coalition: Organized + constrained → d≈0.35, f(d)≈0.32. Low extraction. Constrained by need to build credibility against traditional system but have agency and see clear sunset pathway. Journal system: Institutional + arbitrage → d≈0.10, f(d)≈0.02. Piton classification driven by theater gate (0.68), not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that peer review is structurally a TANGLED ROPE masquerading as MOUNTAIN through institutional naturalization. The claimed type reflects its hybrid coordination-extraction structure: it provides real epistemic function (coordination benefit) while extracting asymmetric career leverage (extraction mechanism). The false summit (mountain perspective) arises when analytical observers naturalize the institutional arrangement as inevitable ('some expert review is necessary'). This conflates necessary epistemic function with a specific institutional implementation. The decomposition: (1) epistemic necessity of expert scrutiny is mountain-level true; (2) peer review's specific architecture (elite gatekeeping, reviewer anonymity, journal-mediated access, extraction of author fees and labor) is historically contingent and increasingly extractive; (3) the scaffold perspective validates that alternative architectures (overlay journals, decentralized review, preprint + public commentary) can provide the epistemic function without proportional extraction. The mandatrophy is resolved by separating the necessary epistemic constraint (expert review of complex claims) from the institutional implementation (traditional journal peer review). The JSON assigns ε=0.52 to the institutional mechanism, not to expertise review per se. The false summit detector will correctly identify the mountain perspective as contingent naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    review_quality_vs_throughput_tradeoff,
    'Is the decline in peer review quality driven by inherent cognitive limits on expert attention or by extractive publisher incentives to maximize acceptance rates and reviewer volume?',
    'Comparative analysis of journal incentive structures: journals with profit-sharing, nonprofit status, and reviewer compensation; correlation between review compensation and review quality metrics (cited as problematic/influential)',
    'If cognitive limits dominant: peer review system is mountain-adjacent (necessary constraint). If publisher incentives dominant: system is tangled rope (fixable via incentive redesign).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_quality_vs_throughput_tradeoff, empirical, 'Whether review quality decline is inherent or incentive-driven').

omega_variable(
    alternative_verification_sufficiency,
    'Do decentralized preprint + public commenting systems (arXiv, ScienceOpen, overlay journals) provide equivalent epistemic validation compared to traditional gatekeeping?',
    'Longitudinal tracking of retraction rates, citation impact, and error detection latency for papers published via traditional vs alternative pathways; community satisfaction surveys across career stages',
    'If equivalent: scaffold perspective validated — transition to open models is achievable. If insufficient: snare perspective may be understated; decentralized systems inadequate for quality control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether decentralized alternatives provide equivalent epistemic validation').

omega_variable(
    interdisciplinary_boundary_insolubility,
    'Is the failure of peer review for interdisciplinary work a contingent reviewer pool problem (fixable by training) or a structural insolubility of cross-disciplinary expertise?',
    'Analysis of interdisciplinary funding success rates under different review models: traditional discipline-specific panels vs integrated review teams; correlation between reviewer diversity and acceptance rates for boundary-crossing work',
    'If contingent: snare classification may be too severe; targeted fixes (interdisciplinary panels, training) could reduce trap severity. If structural: snare classification is robust; alternative systems must natively support boundary work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interdisciplinary_boundary_insolubility, empirical, 'Whether interdisciplinary review failure is fixable or structural').

omega_variable(
    elite_capture_quantification,
    'What fraction of peer review slots are filled by researchers in the top 5% of citation impact? Does this concentration represent functional expertise (appropriate for quality control) or structural gatekeeping (extractive)?',
    'Network analysis of review assignments; correlation between reviewer citation rank and review recommendations; analysis of non-elite researchers'' acceptance rates under elite vs diverse reviewer pools',
    'If < 10% concentration: diversity adequate, snare classification may overstate trap severity. If > 40% concentration: elite capture confirmed, snare/tangled rope classifications robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_capture_quantification, empirical, 'Degree of elite concentration in peer review').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openscholar_peer_review, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ospeer_tr_t0, openscholar_peer_review, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ospeer_tr_t15, openscholar_peer_review, theater_ratio, 15, 0.58).
narrative_ontology:measurement(ospeer_tr_t30, openscholar_peer_review, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(ospeer_be_t0, openscholar_peer_review, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ospeer_be_t15, openscholar_peer_review, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(ospeer_be_t30, openscholar_peer_review, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openscholar_peer_review, information_standard).
narrative_ontology:affects_constraint(openscholar_peer_review, verification_bottleneck).
narrative_ontology:affects_constraint(openscholar_peer_review, publication_bias).
narrative_ontology:affects_constraint(openscholar_peer_review, academic_career_dependency).

% DUAL FORMULATION NOTE:
% Peer review decomposes into two structurally distinct constraints: (1) epistemic_expert_validation (ε≈0.05, Mountain) — the necessary function of expert scrutiny for complex claims; (2) openscholar_peer_review (ε=0.52, Tangled Rope) — the specific institutional implementation via journal gatekeeping with extractive career asymmetries. This story addresses the institutional implementation. The epistemic necessity is captured separately as a mountain constraint with universal applicability. The network link shows how the peer review implementation (high theater, increasing extraction) contaminates the epistemic validation function upstream and creates publication bias downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openscholar_peer_review, powerful, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
