% ============================================================================
% CONSTRAINT STORY: theory_of_visitors
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theory_of_visitors, []).

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
 *   constraint_id: theory_of_visitors
 *   human_readable: The Theory of Visitors (Relationship Transience)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Theory of Visitors is a psychological and institutional constraint
 *   that reframes all human relationships as inherently temporary presences.
 *   The theory performs a dual function: it coordinates institutional care
 *   systems by allowing caregivers to manage emotional labor without
 *   depletion, while simultaneously extracting relational security from
 *   individuals who internalize it as a protective strategy against
 *   abandonment. The constraint has increased in sophistication and adoption
 *   over the past two decades, particularly in institutional settings (foster
 *   care, prisons, institutional medicine, corporate environments). Its
 *   theater ratio has risen (0.35→0.64) as the theory has become rationalized
 *   through pop-psychology frameworks ("detachment is maturity,"
 *   "non-attachment is enlightenment") that mask its coordination-extraction
 *   hybrid. The theory exhibits all six classification types depending on the
 *   observer's structural position: it is Snare to the powerless individual
 *   seeking permanence, Tangled Rope to the caregiver who uses it to prevent
 *   burnout while denying care recipients relational consistency, Rope to the
 *   institution that benefits from reduced emotional overhead, Scaffold to
 *   the organized attachment-informed movement building alternatives, Piton
 *   to the individual who adopted it as psychological protection and now
 *   maintains it through inertia, and genuinely hybrid Tangled Rope to the
 *   analytical observer who sees both coordination and extraction functions
 *   operating simultaneously.
 *
 * KEY AGENTS:
 *   - Individuals seeking permanence: Primary victims (powerless/trapped) — internalize transience as protective; lose relational capacity; experience chronic anticipatory grief
 *   - Institutional caregivers: Secondary beneficiaries + secondary victims (moderate/constrained) — use theory to prevent burnout; simultaneously deny recipients relational consistency; constrained by institutional norms
 *   - Institutional care systems: Primary beneficiaries (institutional/arbitrage) — reduce emotional labor overhead; coordinate large populations without relational commitment; capture efficiency gains
 *   - Attachment-informed movement: Organized agents (organized/mobile) — developmental psychologists, trauma therapists, reform advocates building evidence-based relational healing alternatives; see theory as temporary structure with exit path
 *   - Early adopters who now maintain theory: Piton agents (moderate/constrained) — perform acceptance of transience while experiencing hidden grief; maintain theory through social validation despite degraded function
 *   - Analytical observer: Civilizational view (analytical/analytical) — recognizes dual coordination-extraction function; distinguishes legitimate caregiver protection from contingent victim suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theory_of_visitors, 0.58).
domain_priors:suppression_score(theory_of_visitors, 0.68).
domain_priors:theater_ratio(theory_of_visitors, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theory_of_visitors, extractiveness, 0.58).
narrative_ontology:constraint_metric(theory_of_visitors, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(theory_of_visitors, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theory_of_visitors, tangled_rope).
narrative_ontology:human_readable(theory_of_visitors, "The Theory of Visitors (Relationship Transience)").
narrative_ontology:topic_domain(theory_of_visitors, "social/psychological").

domain_priors:requires_active_enforcement(theory_of_visitors).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theory_of_visitors, emotional_detachment_practitioners).
narrative_ontology:constraint_beneficiary(theory_of_visitors, institutional_care_systems).
narrative_ontology:constraint_victim(theory_of_visitors, individuals_seeking_permanence).
narrative_ontology:constraint_victim(theory_of_visitors, attachment_formation_capacity).
narrative_ontology:constraint_victim(theory_of_visitors, relational_commitment_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL SEEKING PERMANENCE (SNARE) — The person who internalizes the theory and becomes unable to invest in relationships. Trapped by the perpetual expectation of loss. Suppression operates through emotional preemption: the theory prevents formation of secure attachment bonds. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. High extraction with no coordination benefit.
constraint_indexing:constraint_classification(theory_of_visitors, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CAREGIVER IN INSTITUTIONAL SETTINGS (TANGLED ROPE) — The institutional caregiver (teacher, social worker, therapist) who uses the theory to manage emotional labor and prevent burnout, but simultaneously deprives vulnerable populations of relational consistency. Constrained by institutional norms and emotional labor requirements. Coordination function: prevents caregiver depletion through emotional distance. Extraction function: denies recipients of care the relational stability needed for healing. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(theory_of_visitors, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL CARE SYSTEM (ROPE) — The organization (hospital, foster care system, prison) that benefits from the theory as a coordination mechanism for managing large populations without the overhead of genuine relational commitment. Staff retention improves when emotional detachment is normalized. d≈0.08, f(d)≈-0.08, σ=1.1 → χ≈-0.06. Net beneficiary through reduced coordination costs.
constraint_indexing:constraint_classification(theory_of_visitors, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ATTACHMENT-INFORMED MOVEMENT (SCAFFOLD) — Organized agents (developmental psychologists, trauma therapists, reform advocates) who see the theory as a temporary institutional structure being replaced by relational healing models. d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.16. Low extraction because the movement has agency and documents an exit path through empirical evidence of relational repair.
constraint_indexing:constraint_classification(theory_of_visitors, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INDIVIDUAL WHO ADOPTED THE THEORY (PITON) — A person who previously believed in relational permanence, internalized the theory as protection, and now maintains it through institutional inertia and social validation despite its degraded function. theater_ratio≈0.64: The person performs acceptance of transience while experiencing hidden grief. The theory persists not because it works (it doesn't prevent abandonment pain) but because the alternative (vulnerability) feels riskier. d≈0.65, f(d)≈0.98, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(theory_of_visitors, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the theory performs two functions simultaneously: (1) coordination — it does reduce institutional overhead and caregiver burnout through emotional distance, solving a real resource allocation problem; (2) extraction — it privatizes the emotional labor problem by offloading it onto the psychological resilience of the vulnerable, who must internalize transience as protective rather than as collective institutional failure. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.45. The constraint is genuinely hybrid.
constraint_indexing:constraint_classification(theory_of_visitors, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theory_of_visitors_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theory_of_visitors, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theory_of_visitors, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theory_of_visitors, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(theory_of_visitors, TR),
    TR >= 0.70.

:- end_tests(theory_of_visitors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The theory extracts relational security, emotional vulnerability, and attachment formation capacity from those who internalize it. The extraction is significant but not maximal because the theory also performs a genuine coordination function (caregiver burnout prevention). The value increased from 0.38→0.58 over the interval as the theory became normalized and rationalized through psychological discourse that presents it as maturity rather than as institutional necessity. Suppression (0.68): High but not extreme. The theory suppresses alternative framings (permanence is possible, attachment is healthy) through institutional enforcement, social validation, and therapeutic language that repackages transience as enlightenment. The suppression is structural: individuals face real caregiver capacity constraints that the theory explains. However, the suppression is contingent — different resource allocation (more caregivers, lower population ratios) would eliminate the institutional demand for the theory. Theater ratio (0.64): Moderately high. The theory has increasingly performative content: individuals perform acceptance of transience ("I knew they would leave") while experiencing hidden anticipatory grief. Institutional caregivers perform emotional distance while experiencing genuine caregiver stress. The theater increased from 0.35→0.64 as pop-psychology frameworks rationalized the theory, creating a gap between the normative rhetoric ("non-attachment is wisdom") and the actual psychological function ("institutional triage of emotional labor").
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The powerless individual sees Snare: pure extraction with no coordination benefit to themselves. The institutional caregiver sees Tangled Rope: genuine coordination (burnout prevention) mixed with extraction (denial of relational consistency to vulnerable populations). The institution sees Rope: legitimate coordination mechanism for managing population at scale. The attachment-informed movement sees Scaffold: a temporary institutional structure being replaced by relational healing models with lower emotional overhead (evidence-based trauma therapy, community-based care, lower population ratios). The individual who adopted the theory sees Piton: the theory persists through social inertia and hidden grief despite no longer functioning as psychological protection. The analytical observer sees Tangled Rope: the theory genuinely performs both coordination (solves institutional caregiver depletion) and extraction (offloads emotional labor problem onto vulnerable populations), but the boundary between legitimate coordination and unjust extraction depends on whether the caregiver-to-population ratio is necessary or contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals seeking permanence: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum directionality toward extraction. They cannot exit the constraint (relational needs are biological) and bear full cost of suppressed attachment capacity. Institutional caregivers: Mixed (secondary beneficiary + secondary victim) + constrained → d≈0.72, f(d)≈1.12. High extraction directionality because the constraint extracts from their emotional availability to care recipients, but they also benefit from reduced burnout, making them not purely victim. Institutional care systems: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. They capture efficiency gains and population coordination benefits while avoiding relational commitment overhead. Attachment-informed movement: Organized + mobile → d≈0.35, f(d)≈0.28. Low extraction directionality because the movement has agency and documented exit paths (evidence-based alternatives). Early adopters: Mixed (previous beneficiary who benefited from theory as protection, now victim of its inertial persistence) + constrained → d≈0.65, f(d)≈0.98. The psychological reversal — the theory no longer protects — drives high directionality despite moderate power. Analytical observer: d≈0.58, f(d)≈0.78. The midpoint reflects genuine uncertainty about whether the coordination function justifies the extraction, depending on institutional necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THROUGH INSTITUTIONAL NECESSITY DISTINCTION: The mandatrophy (coordination vs extraction) is resolved by recognizing that the theory performs both functions contingently. The coordination function (caregiver burnout prevention) is real and necessary under certain institutional conditions (insufficient staffing, high trauma loads, inadequate resources). The extraction function (suppression of relational formation in vulnerable populations) is also real but would be unnecessary if institutional conditions improved (higher caregiver ratios, shorter care durations, community-based models). The constraint is Tangled Rope when institutional resources are genuinely scarce (coordination function is necessary). The constraint becomes pure Snare when institutions artificially maintain scarcity to justify the theory (extraction for efficiency gains beyond what necessity requires). The Scaffold perspective (organized attachment-informed movement) provides the empirical path: relational healing models with lower emotional overhead demonstrate that institutional care does not require emotional distance at this level. The increasing theater ratio (0.35→0.64) indicates the theory is shifting toward extraction-dominant: the rationalization of transience as psychological maturity suggests institutions are using the theory to justify insufficient resources rather than to solve genuine caregiver depletion. The constraint is not yet pure Snare because some institutional caregivers do experience genuine burnout, but the trajectory suggests ongoing extraction accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attachment_biology_vs_choice,
    'Is the Theory of Visitors describing a psychological choice individuals can make, or is it overriding biological attachment needs that cannot be disabled?',
    'Longitudinal studies tracking attachment outcomes in individuals who adopt the theory vs control populations; neurobiological markers of secure attachment in practitioners of emotional detachment; replication studies of relational healing outcomes',
    'If attachment is biological: the theory is a Snare that suppresses essential development (high suppression gate confirmed). If attachment is culturally constructed: the theory might be reclassifiable as a Rope (lower suppression). This determines whether the constraint is truly extractive or coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attachment_biology_vs_choice, empirical, 'Whether transience preference overrides biological attachment needs').

omega_variable(
    institutional_necessity_threshold,
    'What is the minimum viable ratio of caregiver-to-vulnerable-population that would require emotional detachment theory to prevent caregiver depletion?',
    'Comparative analysis of burnout rates across different caregiver ratios; measurement of emotional labor costs in high-ratio vs low-ratio settings; identification of threshold where relational consistency becomes physiologically unsustainable for staff',
    'If threshold is high (few caregivers per population): theory is structurally necessary coordination (Rope classification justified). If threshold is low (adequate ratio prevents depletion): theory is contingent extraction (Snare/Tangled Rope justified). This distinguishes legitimate institutional constraint from policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_necessity_threshold, empirical, 'Caregiver-to-population ratio threshold requiring emotional detachment').

omega_variable(
    theory_cultural_generalization,
    'Is the Theory of Visitors a universal structural feature of human psychology, or is it a culturally specific adaptation that emerged in particular institutional contexts?',
    'Cross-cultural comparison of attachment styles and transience narratives; historical analysis of when the theory emerged in each cultural/institutional context; identification of communities that reject the theory without psychological harm',
    'If universal: the theory might approach Mountain classification (natural law of human vulnerability). If cultural: the theory is revealed as contingent Snare/Tangled Rope with alternatives available. This affects whether suppression is seen as inherent or as imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theory_cultural_generalization, conceptual, 'Whether transience theory is universal or culturally contingent').

omega_variable(
    relational_repair_feasibility,
    'Can individuals who have internalized the Theory of Visitors recover relational capacity and form secure attachments, or does early adoption cause permanent psychological restructuring?',
    'Therapeutic outcome studies tracking relational recovery in individuals who abandon the theory; neuroplasticity studies of attachment system retraining; longitudinal follow-up of former emotional detachment practitioners',
    'If recovery is feasible: the Scaffold perspective is correct — the theory is temporary and replaceable. If permanent: the theory''s suppression mechanism is more severe than measured, approaching irreversible extraction (higher suppression gate). This determines whether the constraint should be reclassified as higher-extractiveness Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_repair_feasibility, empirical, 'Whether relational capacity recovers after transience theory internalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theory_of_visitors, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tov_tr_t0, theory_of_visitors, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tov_tr_t10, theory_of_visitors, theater_ratio, 10, 0.52).
narrative_ontology:measurement(tov_tr_t20, theory_of_visitors, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(tov_be_t0, theory_of_visitors, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tov_be_t10, theory_of_visitors, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(tov_be_t20, theory_of_visitors, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theory_of_visitors, enforcement_mechanism).
narrative_ontology:affects_constraint(theory_of_visitors, relational_grief_suppression).
narrative_ontology:affects_constraint(theory_of_visitors, institutional_caregiver_depletion).
narrative_ontology:affects_constraint(theory_of_visitors, attachment_security_formation).

% DUAL FORMULATION NOTE:
% The Theory of Visitors connects three upstream constraints: (1) relational_grief_suppression — the theory enables suppression of anticipatory grief through psychological reframing; (2) institutional_caregiver_depletion — the theory is a response to genuine caregiver burnout but simultaneously exacerbates it by preventing relational repair; (3) attachment_security_formation — the theory structurally interferes with secure attachment development. Each has its own ε value and perspective set. The visitors theory unifies these as a single institutional justification, but they are structurally distinct constraints with different resolution pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(theory_of_visitors, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
