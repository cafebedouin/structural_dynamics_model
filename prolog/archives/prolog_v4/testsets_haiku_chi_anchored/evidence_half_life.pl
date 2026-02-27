% ============================================================================
% CONSTRAINT STORY: evidence_half_life
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evidence_half_life, []).

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
 *   constraint_id: evidence_half_life
 *   human_readable: The Epistemic Decay Constant
 *   domain: technological/scientific/legal
 *
 * SUMMARY:
 *   The epistemic decay constant creates a structural trap where the speed of
 *   information turnover and the ease of digital alteration cause the
 *   functional half-life of evidence to fall below the time required for
 *   judicial or scientific verification. In legal contexts, evidence must
 *   survive authentication and chain-of-custody verification before trial — a
 *   process measured in months to years. In scientific contexts, evidence
 *   must survive peer review, replication attempts, and archival preservation
 *   — a process measured in years to decades. Meanwhile, digital artifacts
 *   degrade through bit rot, format obsolescence, server failure, and
 *   deliberate alteration at timescales measured in months to years. This
 *   creates a fundamental asymmetry: the evidence needed to prove a claim
 *   disappears before verification can occur. The constraint extracts from
 *   defendants, accused persons, and the scientific record, while being
 *   experienced as a coordination problem by institutions tasked with
 *   verification. The theater ratio has risen over two decades as digital
 *   complexity has outpaced institutional capacity — the elaborate procedures
 *   of chain-of-custody and expert testimony have become increasingly
 *   performative for artifacts that cannot be verified by courtroom ritual.
 *
 * KEY AGENTS:
 *   - Accused/Defendant: Primary victim (powerless/trapped) — must provide evidence that has already decayed or been altered; cannot exit the temporal constraint
 *   - Scientific Record and Epistemic Commons: Primary victim (powerless/trapped) — publications and datasets decay faster than peer review can validate them; bear the cost of degraded knowledge base
 *   - Legal and Scientific Institutions: Mixed actor (institutional/constrained) — both benefit from the decay (preserves their verification monopoly) and are constrained by it (cannot verify what has decayed); main extractors
 *   - Digital Preservation Coalition: Secondary actor (organized/arbitrage) — arXiv, DataCite, blockchain-based timestamping, open-source forensics; can exit by deploying standards
 *   - Traditional Forensic/Peer Review Bureaucracy: Institutional actor maintaining piton (institutional/arbitrage) — chain-of-custody procedures and expert testimony persist as theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent infrastructure choices as immutable laws of information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evidence_half_life, 0.58).
domain_priors:suppression_score(evidence_half_life, 0.65).
domain_priors:theater_ratio(evidence_half_life, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evidence_half_life, extractiveness, 0.58).
narrative_ontology:constraint_metric(evidence_half_life, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(evidence_half_life, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evidence_half_life, snare).
narrative_ontology:human_readable(evidence_half_life, "The Epistemic Decay Constant").
narrative_ontology:topic_domain(evidence_half_life, "technological/scientific/legal").

% --- Structural relationships ---
narrative_ontology:constraint_victim(evidence_half_life, truth_seeking_institutions).
narrative_ontology:constraint_victim(evidence_half_life, defendants_and_accused).
narrative_ontology:constraint_victim(evidence_half_life, scientific_record).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCUSED/DEFENDANT (SNARE) — Cannot exit the constraint that evidence degrades faster than verification can occur. Trapped by time itself. Evidence that would exonerate must survive digital alteration detection, chain-of-custody verification, and technical authentication within a window narrower than the evidence's natural decay half-life. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.95.
constraint_indexing:constraint_classification(evidence_half_life, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC RECORD (SNARE) — The epistemic commons faces systemic degradation. Publications, datasets, and experimental records decay (bit rot, server obsolescence, format incompatibility) faster than peer review can validate them. Replication requires evidence that no longer exists in verifiable form. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.99.
constraint_indexing:constraint_classification(evidence_half_life, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: VERIFICATION INSTITUTIONS (TANGLED ROPE) — Courts, journals, and forensic labs benefit from the decay constant: it creates demand for their verification services and justifies procedural complexity. But they are also constrained by the same decay — they must perform verification within shrinking windows or declare evidence inadmissible. Coordination function: establishing standards for evidence preservation and authentication. Extraction: monopolizing the right to declare what counts as 'verified.' d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DIGITAL PRESERVATION COALITION (ROPE) — Open-source forensics, blockchain-based timestamping, standardized data formats, and distributed archival systems see the decay constant as a pure coordination problem. Building consensus on preservation protocols, authentication standards, and interoperable evidence formats. Arbitrage: coalition members can migrate between competing preservation ecosystems. d≈0.25, f(d)≈0.15, σ=1.2 → χ≈0.10.
constraint_indexing:constraint_classification(evidence_half_life, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL VALIDATION BUREAUCRACY (PITON) — Chain-of-custody procedures, forensic standards, and court rules of evidence persist through institutional inertia despite being designed for physical, not digital, media. Theater ratio=0.68: the elaborate procedures (sealing evidence, witness testimony, expert cross-examination) are largely performative for digital artifacts — an altered hard drive image cannot be detected by courtroom ritual. The functional verification capacity has atrophied while the theatrical procedures persist. d≈0.10, f(d)≈-0.02, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(evidence_half_life, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, information decay (data corruption, format obsolescence, loss of context) is an immutable feature of physical reality and computational systems: entropy always increases, media always degrade, and no archive lasts forever. This perspective sees the half-life decay as a natural limit on evidence persistence, not an institutional problem. However, the structural data (ε=0.58, suppression=0.65, theater=0.68) contradicts the mountain classification — the engine will compute this as a false summit, revealing that much of the 'decay' is contingent on choices about preservation infrastructure and authentication protocols, not laws of nature.
constraint_indexing:constraint_classification(evidence_half_life, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evidence_half_life_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evidence_half_life, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evidence_half_life, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evidence_half_life, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evidence_half_life, TR),
    TR >= 0.70.

:- end_tests(evidence_half_life_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The decay constant extracts from the accused (who cannot produce evidence that has decayed) and from the scientific record (which cannot validate claims against evidence that no longer exists in verifiable form). The extraction is not as severe as classical snares because preservation infrastructure CAN be built — it is not inherently impossible. However, the default institutional path (do nothing, let decay happen) systematically favors those who benefit from unverifiable claims and those with power to destroy or alter evidence. The value reflects that extraction is substantial and structural but not absolute. Suppression (0.65): High. Multiple barriers prevent escape from the decay constant: no individual can unilaterally preserve evidence (requires institutional infrastructure), replication requires artifacts that no longer exist, and challenging evidence authenticity requires forensic expertise that few possess. Format lock-in (proprietary file formats that cannot be read without proprietary software), media obsolescence (floppy disks, optical media), and server failure create high exit costs. Theater ratio (0.68): High. This reflects the gap between the performative procedures (chain-of-custody, expert testimony, peer review checklist) and their actual verification capacity. A cryptographically altered digital image cannot be detected by cross-examining a witness. A dataset with corrupted metadata cannot be validated by reading a peer review checklist. The elaborate institutional procedures have become theater as the artifacts they govern have become digital.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The accused sees pure snare — evidence disappears and they cannot prove innocence. The scientific record sees pure snare — evidence decays and claims cannot be replicated or verified. The verification institutions see tangled rope — they coordinate the definition of acceptable evidence but also maintain barriers to alternative verification methods. The digital preservation coalition sees rope — a pure coordination problem solvable by standards and open-source tools. The traditional bureaucracy sees piton — their procedures persist as theater. The analytical observer risks seeing mountain — but the structural data reveals this is a false summit. The gap between the accused's snare and the institution's tangled rope reflects the asymmetry: institutions have resources to preserve some evidence (selectively) and can arbitrage between preservation standards, while the accused has no such capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Accused/Defendant: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction. Cannot preserve evidence, cannot prove innocence with degraded evidence. Scientific record: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction. Abstract collective cannot organize, cannot exit, cannot verify claims. Legal/scientific institutions: Beneficiary + constrained → d≈0.55, f(d)≈0.75. The constraint serves them (creates demand for their services, justifies their gatekeeping role) but also constrains them (they must verify within shrinking windows). Digital preservation coalition: Organized + arbitrage → d≈0.25, f(d)≈0.15. Coalition can exit by standardizing preservation; low effective extraction because they have agency. Traditional bureaucracy: Institutional + arbitrage → d≈0.10, f(d)≈-0.02. Piton classification from theater ratio, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective is a false summit because much decay is contingent on infrastructure choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here concerns whether the decay constant is an immutable feature of information (mountain) or a contingent institutional failure (snare). The false summit detector flags the mountain perspective: if the constraint were truly immutable (like entropy or the speed of light), then preservation infrastructure should not matter. But the structural data show that extractiveness (0.58) is heavily contingent on whether preservation standards exist, whether institutions are incentivized to maintain them, and whether the accused/researchers have access to cryptographic anchoring and distributed verification. This reveals that much of the decay is not natural but engineered — the constraint persists because institutions maintain the decay-friendly infrastructure (proprietary formats, centralized archives, short retention policies) and resist alternatives. The snare perspective (accused cannot prove innocence because evidence decayed) and the rope perspective (digital preservation coalition can solve this with standards) cannot both be fully true unless the institutions have incentives to maintain decay. The mandatrophy resolves by acknowledging: (1) some decay is natural (immutable), (2) some decay is institutional policy (extractive), and (3) the current ratio is heavily weighted toward institutional maintenance of decay. The decay constant is not a mountain — it is a snare that naturalizes itself by claiming to be.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    half_life_vs_verification_window,
    'What is the actual median half-life of digital evidence (photographic, video, audio, data) and how does it compare to the median time required for judicial or scientific verification?',
    'Longitudinal analysis of evidence repositories; tracking of successful vs failed authentication attempts over time; correlation between evidence age and verification success rates',
    'If half-life > verification window: constraint is coordination (Rope). If half-life < verification window by >2x: constraint is pure extraction (Snare). If approximately equal: constraint is high-extraction coordination (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(half_life_vs_verification_window, empirical, 'Comparison of evidence decay half-life to verification time requirements').

omega_variable(
    intentional_vs_natural_decay,
    'What fraction of evidence decay is due to intentional destruction/alteration (extraction mechanism) vs natural degradation (immutable limit)?',
    'Forensic analysis of decay patterns; detection of anomalous (intentional) vs natural degradation signatures; comparison of decay rates for protected vs unprotected archives',
    'If >80% intentional: constraint is pure snare (extraction through destruction). If >60% natural: constraint approaches mountain (immutable limit). If mixed: tangled rope (both coordination problem and extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_vs_natural_decay, empirical, 'Proportion of evidence decay attributable to intentional destruction vs natural causes').

omega_variable(
    cryptographic_anchor_sufficiency,
    'Can cryptographic timestamping and distributed verification (blockchain-style, but not necessarily blockchain) actually extend the functional half-life of digital evidence to exceed verification timescales?',
    'Pilot deployment of cryptographic anchoring in legal and scientific contexts; measurement of authentication success rates and cost per verified artifact',
    'If successful: rope perspective confirmed — problem is coordination, solvable by standard adoption. If unsuccessful: snare persists because attackers can compromise the verification infrastructure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cryptographic_anchor_sufficiency, empirical, 'Whether cryptographic solutions can extend evidence half-life beyond verification requirements').

omega_variable(
    institutional_incentive_structure,
    'Do legal and scientific institutions have structural incentives to delay or obstruct the deployment of preservation technology that would reduce the decay constant?',
    'Historical analysis of technology adoption; comparison of jurisdictions with strong preservation mandates vs those without; analysis of institutional resistance to open-source forensics and standard-based evidence handling',
    'If true: constraint is snare (institutions actively maintaining decay to preserve power). If false: constraint is coordination problem (institutions want better verification but lack coordination mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_structure, conceptual, 'Whether institutions benefit from and resist solutions to evidence decay').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evidence_half_life, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ehl_tr_t0, evidence_half_life, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ehl_tr_t10, evidence_half_life, theater_ratio, 10, 0.52).
narrative_ontology:measurement(ehl_tr_t20, evidence_half_life, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ehl_be_t0, evidence_half_life, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ehl_be_t10, evidence_half_life, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(ehl_be_t20, evidence_half_life, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evidence_half_life, information_standard).
narrative_ontology:affects_constraint(evidence_half_life, digital_format_obsolescence).
narrative_ontology:affects_constraint(evidence_half_life, institutional_archive_concentration).
narrative_ontology:affects_constraint(evidence_half_life, forensic_authentication_bottleneck).

% DUAL FORMULATION NOTE:
% The epistemic decay constant is downstream of three distinct constraints: format obsolescence (technical degradation), archive concentration (institutional chokepoint), and forensic bottleneck (verification capacity). Each has its own ε value. This story captures the composite effect — how all three together create the half-life asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evidence_half_life, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
