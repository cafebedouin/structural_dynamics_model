% ============================================================================
% CONSTRAINT STORY: taxonomy_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taxonomy_drift, []).

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
 *   constraint_id: taxonomy_drift
 *   human_readable: The Semantic Slippage Trap
 *   domain: social/linguistic/bureaucratic
 *
 * SUMMARY:
 *   The semantic slippage trap occurs when a governing system's definitions
 *   (legal categories, administrative classifications, regulatory
 *   definitions) drift away from ground-truth reality while remaining
 *   formally enforced. Subjects must either falsify their circumstances to
 *   fit obsolete categories or face bureaucratic penalties. The constraint
 *   operates through two mechanisms: (1) technical: as reality evolves,
 *   definitions become incompletely descriptive; (2) intentional: actors with
 *   interests in regulatory exemption or evasion actively resist definition
 *   revision. The combination creates a structural extraction mechanism where
 *   beneficiaries (administrators, capture interests) profit from category
 *   instability while victims (subjects, practitioners, the epistemic
 *   commons) bear the cost. Theater ratio (0.68) reflects constant
 *   performative updates to the taxonomy (revisions, task forces, stakeholder
 *   consultations) that leave fundamental structure intact — the ritual of
 *   updating replaces actual definition improvement. Extractiveness (0.58)
 *   reflects that the gap between definition and reality creates regulatory
 *   space for privileged actors to exploit: a company can claim exemption
 *   from a rule because the rule's definition doesn't capture their actual
 *   business model; a population can be systematically undercounted because
 *   the census categories don't reflect lived reality.
 *
 * KEY AGENTS:
 *   - Actual Subject Population: Primary victim (powerless/trapped) — must fit themselves into categories or suffer administrative consequences
 *   - Field Practitioners: Secondary victim (moderate/constrained) — see the drift daily; face pressure to falsify records or resign
 *   - Bureaucratic Administrators: Primary beneficiary (institutional/arbitrage) — benefit from stable (if obsolete) systems; exit easily through institutional reassignment
 *   - Regulatory Capture Interests: Secondary beneficiary (powerful/mobile) — actively preserve semantic drift to maintain regulatory exemptions
 *   - Legacy Classification Systems: Institutional actor (institutional/arbitrage) — persist through inertia and theater despite eroded function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice as inevitable property of formal systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taxonomy_drift, 0.58).
domain_priors:suppression_score(taxonomy_drift, 0.65).
domain_priors:theater_ratio(taxonomy_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taxonomy_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(taxonomy_drift, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(taxonomy_drift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taxonomy_drift, snare).
narrative_ontology:human_readable(taxonomy_drift, "The Semantic Slippage Trap").
narrative_ontology:topic_domain(taxonomy_drift, "social/linguistic/bureaucratic").

domain_priors:requires_active_enforcement(taxonomy_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taxonomy_drift, bureaucratic_administrators).
narrative_ontology:constraint_beneficiary(taxonomy_drift, regulatory_capture_interests).
narrative_ontology:constraint_victim(taxonomy_drift, actual_subject_population).
narrative_ontology:constraint_victim(taxonomy_drift, field_practitioners).
narrative_ontology:constraint_victim(taxonomy_drift, ground_truth_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped within administrative categories that no longer reflect ground reality. Cannot exit without legal/bureaucratic consequences. Definitions are enforced from above; subjects have no voice in revision. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(taxonomy_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD PRACTITIONERS (SNARE) — Know the real-world details that definitions miss. Face constant pressure to fit ground-truth observations into obsolete categories or falsify records. Career risk for reporting semantic drift; constrained exit. d≈0.88, f(d)≈1.28, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(taxonomy_drift, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUREAUCRATIC ADMINISTRATORS (ROPE) — Benefit from stable category systems. Semantic drift is invisible from within the system; definitions are treated as self-maintaining. Exit is easy (reassignment, promotion); institutional continuity is guaranteed. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(taxonomy_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY CAPTURE INTERESTS (TANGLED ROPE) — Actively maintain semantic drift to prevent regulation of their activities. Benefit from category systems that exclude them from scrutiny (e.g., platform companies as 'publishers' vs 'utilities'). Provide apparent coordination function (industry participation in standard-setting), but genuine coordination goal is self-exemption. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(taxonomy_drift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY CLASSIFICATION SYSTEMS (PITON) — Taxonomy persists long after its functional purpose has eroded. Maintained through institutional theater: constant attestations that the system 'works,' performative updates that don't change underlying logic, compliance rituals disconnected from real outcomes. theater_ratio=0.68 captures persistent administrative overhead with minimal actual classification function. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(taxonomy_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, all classification systems inevitably drift from ground reality as the world changes faster than definitions can update. This is presented as an immutable property of formal systems. However, the structural data (ε=0.58, suppression=0.65, beneficiaries actively maintaining drift) contradicts the mountain classification. This is a false summit: semantic drift is not an inevitable law but a contingent outcome of institutional power imbalance.
constraint_indexing:constraint_classification(taxonomy_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taxonomy_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taxonomy_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taxonomy_drift, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taxonomy_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taxonomy_drift, TR),
    TR >= 0.70.

:- end_tests(taxonomy_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The gap between definition and reality enables extraction. Administrators enjoy stable categories and exemption from accountability for false descriptions. Capture interests profit from regulatory gaps. Subjects pay the cost of misclassification (denied services, inappropriate treatment, legal penalties). The extractiveness increased from 0.32 to 0.58 over the interval as awareness of the gap grew but institutional resistance to revision strengthened. Suppression (0.65): Significant. Subjects have no formal mechanism to propose definition changes; field practitioners face retaliation for reporting drift; alternative definitions (proposed by advocacy groups, practitioners) are dismissed as non-expert; only official bodies can authorize revision, and they move slowly. Theater ratio (0.68): High and increasing. The taxonomy generates constant theater: public consultations that don't change outcomes, technical 'working groups' that produce no substantive revisions, attestations that the system is 'under review,' compliance certifications that mask the definition-reality gap. Theater increased as the gap became increasingly obvious and officials needed visible activity to maintain credibility.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits classic snare characteristics from the victim side (subject population, field practitioners) and rope from the beneficiary side (administrators). The powerful regulatory interests see Tangled Rope because they participate in the governance ritual while actively maintaining the drift that benefits them. The legacy system itself is a Piton — it persists through institutional theater despite eroded real function. The analytical observer risks the false summit of treating taxonomy drift as an inevitable property of formal systems rather than a contingent outcome of institutional power imbalance. The perspectival gap between administrators (who see a functioning system) and subjects (who see a trap) is maximal: the same taxonomy appears as natural administrative infrastructure from above and as structurally deceptive from below.
 *
 * DIRECTIONALITY LOGIC:
 *   Subject population: Victim + trapped → d≈0.93, f(d)≈1.40. Maximal extraction. Field practitioners: Victim + constrained → d≈0.88, f(d)≈1.28. High extraction with slight agency. Bureaucratic administrators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; low effective extraction because they experience the constraint as a coordination tool. Regulatory capture interests: Both beneficiary (through preserved regulatory gap) and victim (through nominal compliance obligations) + mobile → d≈0.35, f(d)≈0.35. Moderate effective extraction because they have agency and alternatives. Legacy system: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit detector active.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival differentiation. The constraint is genuinely a Snare from the subject population's and field practitioner's perspective — they are trapped in a system they cannot exit and whose parameters they cannot change. It is genuinely a Rope from the administrator's perspective — they experience it as a coordination and communication tool. The regulatory capture interests experience it as Tangled Rope because they have both coordination function (participating in standard-setting) and extraction benefit (maintained regulatory gap). No single type is correct; the constraint's structural reality is the presheaf of all perspectives. The false summit (mountain view) is caught by the engine because the structural data shows clear beneficiaries and victims — not an immutable property of nature but an outcome of institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drift_intentionality,
    'Is semantic drift primarily a technical lag problem (definitions cannot keep pace) or an intentional extraction mechanism (definitions are preserved to extract)?',
    'Analysis of update frequency, who proposes revisions, who blocks them, correlation between semantic drift and regulatory advantage for specific interests',
    'If lag: constraint is Rope or Scaffold (coordination problem). If intentional: constraint is Tangled Rope or Snare (extraction mechanism). Current evidence (regulatory capture, asymmetric update resistance) suggests significant intentionality component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drift_intentionality, empirical, 'Whether semantic drift is technical lag or intentional extraction').

omega_variable(
    ground_truth_accessibility,
    'Can ground-truth reality be formalized into machine-readable definitions, or is there an inherent gap between reality and any formal system?',
    'Historical case studies: definitions that successfully captured reality vs those that failed; attempts at probabilistic/fuzzy definitions vs binary taxonomies; AI classification performance on contested boundary cases',
    'If formalization is possible: drift is a failure of institutional will (Snare/Tangled Rope). If inherent gap exists: some drift is inevitable (Scaffold/Rope). This determines whether the constraint is fundamentally extractive or inherently coordinal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ground_truth_accessibility, conceptual, 'Whether ground-truth reality can be formalized into definitions').

omega_variable(
    victim_coalition_threshold,
    'At what scale of harm do subject populations achieve sufficient coalition power to force taxonomy revision, breaking the snare?',
    'Historical revisions: which ones were imposed top-down vs driven by subject coalitions; critical mass thresholds; effectiveness of organized resistance vs individual complaint mechanisms',
    'If low threshold: snare classification is unstable, with periodic ruptures into negotiated Tangled Rope. If high threshold: snare persists indefinitely due to coordination barriers among victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_threshold, empirical, 'Coalition threshold for forcing taxonomy revision').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taxonomy_drift, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taxdrift_tr_t0, taxonomy_drift, theater_ratio, 0, 0.38).
narrative_ontology:measurement(taxdrift_tr_t10, taxonomy_drift, theater_ratio, 10, 0.54).
narrative_ontology:measurement(taxdrift_tr_t20, taxonomy_drift, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(taxdrift_be_t0, taxonomy_drift, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(taxdrift_be_t10, taxonomy_drift, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(taxdrift_be_t20, taxonomy_drift, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taxonomy_drift, information_standard).
narrative_ontology:affects_constraint(taxonomy_drift, regulatory_capture).
narrative_ontology:affects_constraint(taxonomy_drift, census_miscount).
narrative_ontology:affects_constraint(taxonomy_drift, platform_classification_arbitrage).

% DUAL FORMULATION NOTE:
% Semantic slippage is upstream of specific regulatory capture scenarios but represents a distinct structural mechanism. The constraint's extractiveness (0.58) reflects the general institutional dynamics of definition drift; specific instances (census categories, platform exemptions, labor classification) have their own ε values reflecting domain-specific capture intensity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taxonomy_drift, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
