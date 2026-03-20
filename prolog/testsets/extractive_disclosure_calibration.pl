% ============================================================================
% CONSTRAINT STORY: extractive_disclosure_calibration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extractive_disclosure_calibration, []).

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
 *   constraint_id: extractive_disclosure_calibration
 *   human_readable: Extractive Disclosure Calibration as Rational Wariness
 *   domain: social_psychology/gender_studies/relational_architecture
 *
 * SUMMARY:
 *   The extractive disclosure calibration constraint operates at the
 *   intersection of relational architecture and gendered emotional labor
 *   norms. Subjects who have experienced disclosure being weaponized —
 *   vulnerabilities used in arguments, private information shared to third
 *   parties, emotional needs framed as excessive — rationally calibrate their
 *   disclosure thresholds downward in subsequent relationships. This wariness
 *   is then pathologized as 'trust issues' or 'emotional unavailability,'
 *   creating a double extraction: the original weaponization plus the framing
 *   of the defense mechanism as pathology. The constraint is downstream of
 *   gendered_disclosure_asymmetry (the upstream rope that coordinates
 *   disclosure norms) but represents a distinct structural phenomenon: the
 *   weaponization pattern and its rationalization. The theater_ratio (0.52)
 *   reflects that therapeutic interventions often address the symptom
 *   (wariness) through mutual vulnerability exercises while ignoring the
 *   structural cause (asymmetric consequences of disclosure). The constraint
 *   shows modest cyclical dynamics: extractiveness rises during relationship
 *   formation (when disclosure pressure is highest), drops slightly during
 *   stable periods, then rises again during conflict or dissolution (when
 *   prior disclosures are weaponized).
 *
 * KEY AGENTS:
 *   - Disclosure Subjects (Trapped): Primary victims (powerless/trapped) — economically or socially dependent on relationships where disclosure is weaponized; cannot exit without severe cost; wariness is rational but pathologized
 *   - Disclosure Subjects (Constrained): Secondary victims (moderate/constrained) — have exit options but face high costs; experience mixed coordination (genuine intimacy) and extraction (weaponization asymmetry)
 *   - Extractive Actors: Primary beneficiaries (institutional/arbitrage) — benefit from asymmetric disclosure norms; can exit to new relationships; experience weaponization as legitimate relational negotiation
 *   - Relational Trust Commons: Abstract victim (powerless/trapped) — collective good of relational trust degraded by weaponization pattern; cannot organize or exit
 *   - Feminist Therapeutic Framework: Organized agents (organized/mobile) — building alternative relational architectures with consent-based norms and trauma-informed practice; see constraint as temporary with generational sunset
 *   - Traditional Couples Therapy: Institutional actor (institutional/constrained) — maintains 'mutual vulnerability' framework despite recognizing re-traumatization risk; piton perspective reflects degraded function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees coordination function (disclosure enables intimacy) and extraction mechanism (asymmetric weaponization) as structurally entangled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extractive_disclosure_calibration, 0.58).
domain_priors:suppression_score(extractive_disclosure_calibration, 0.68).
domain_priors:theater_ratio(extractive_disclosure_calibration, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extractive_disclosure_calibration, extractiveness, 0.58).
narrative_ontology:constraint_metric(extractive_disclosure_calibration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(extractive_disclosure_calibration, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extractive_disclosure_calibration, snare).
narrative_ontology:human_readable(extractive_disclosure_calibration, "Extractive Disclosure Calibration as Rational Wariness").
narrative_ontology:topic_domain(extractive_disclosure_calibration, "social_psychology/gender_studies/relational_architecture").

domain_priors:requires_active_enforcement(extractive_disclosure_calibration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extractive_disclosure_calibration, extractive_actors).
narrative_ontology:constraint_victim(extractive_disclosure_calibration, disclosure_subjects).
narrative_ontology:constraint_victim(extractive_disclosure_calibration, relational_trust_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCLOSURE SUBJECT (SNARE) — Trapped by relational dependency and internalized obligation to emotional labor. Cannot exit relationships where disclosure is weaponized without losing economic support, social network, or custody arrangements. Wariness is rational response to repeated weaponization, but the framing of wariness as pathology ('trust issues,' 'emotional unavailability') compounds the extraction by pathologizing the defense mechanism itself.
constraint_indexing:constraint_classification(extractive_disclosure_calibration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISCLOSURE SUBJECT WITH EXIT OPTIONS (TANGLED ROPE) — Has economic independence and social mobility but faces high costs to exit: career damage from being labeled 'difficult,' loss of shared friend networks, internalized guilt from violating relational norms. The constraint coordinates genuine intimacy needs while extracting through the asymmetry: disclosure flows one direction, vulnerability is weaponized selectively. Mixed experience — some benefit from relational connection, significant extraction from the weaponization pattern.
constraint_indexing:constraint_classification(extractive_disclosure_calibration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTIVE ACTOR (ROPE) — Benefits from asymmetric disclosure norms. Experiences the constraint as coordination: partner's emotional labor provides relational stability and conflict resolution without reciprocal vulnerability. Can exit to new relationships when current partner's wariness increases. The weaponization of prior disclosures (using shared vulnerabilities in arguments, sharing private information to third parties, framing wariness as pathology) is experienced as legitimate relational negotiation, not extraction.
constraint_indexing:constraint_classification(extractive_disclosure_calibration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEMINIST THERAPEUTIC FRAMEWORK (SCAFFOLD) — Organized agents (trauma-informed therapy, relational equity frameworks, consent-based communication norms) see the constraint as temporary. Building alternative relational architectures where wariness is recognized as rational calibration, not pathology. Sunset mechanism: as therapeutic literacy spreads and younger cohorts adopt consent-based norms, the weaponization pattern loses cultural legitimacy. Estimated sunset: 15-25 years for generational norm shift.
constraint_indexing:constraint_classification(extractive_disclosure_calibration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL COUPLES THERAPY (PITON) — The 'both partners need to be more vulnerable' framework persists through institutional inertia despite recognizing that it often re-traumatizes subjects with histories of weaponized disclosure. Theater ratio reflects that the intervention (mutual vulnerability exercises) addresses the symptom (wariness) while ignoring the structural cause (asymmetric consequences of disclosure). Maintained because alternatives haven't fully replaced it in clinical training, not because it resolves the underlying extraction.
constraint_indexing:constraint_classification(extractive_disclosure_calibration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint coordinates genuine intimacy needs (disclosure enables relational depth) while extracting through gendered asymmetry in consequences. The correlation between prior weaponization and current wariness is empirically robust. The framing of wariness as pathology rather than rational calibration is the extraction mechanism — it pathologizes the defense while preserving the attack. This is not a natural law of relationships but a contingent cultural pattern maintained by asymmetric enforcement of emotional labor norms.
constraint_indexing:constraint_classification(extractive_disclosure_calibration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extractive_disclosure_calibration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extractive_disclosure_calibration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extractive_disclosure_calibration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(extractive_disclosure_calibration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(extractive_disclosure_calibration, TR),
    TR >= 0.70.

:- end_tests(extractive_disclosure_calibration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The weaponization of disclosure creates career and relational costs for subjects (being labeled 'difficult,' 'closed-off,' or 'damaged'), while extractive actors benefit from emotional labor without reciprocal vulnerability. The pathologization framing compounds the extraction by making the rational defense mechanism itself a liability. However, extraction is not maximal — some subjects have exit options, and some relationships do not weaponize disclosure. Suppression (0.68): High. Barriers to exit include economic dependency, custody arrangements, social network loss, and internalized obligation to emotional labor norms. The framing of wariness as pathology creates additional suppression — subjects who resist disclosure are labeled as having 'trust issues,' which itself becomes a relational liability. But suppression is not total — feminist therapeutic frameworks and consent-based communication norms are creating alternative pathways. Theater ratio (0.52): Moderate. Traditional therapeutic interventions (mutual vulnerability exercises, 'learning to trust again' frameworks) address the symptom while ignoring the structural cause. The intervention is partly functional (some subjects do benefit from guided disclosure in safe contexts) but substantially performative (the framework assumes the problem is the subject's wariness rather than the weaponization pattern). The theater has increased modestly over the interval as therapeutic language has been adopted without structural change in weaponization rates.
 *
 * PERSPECTIVAL GAP:
 *   The extractive actor sees rope — the constraint coordinates relational stability through partner's emotional labor, and the weaponization is experienced as legitimate conflict negotiation. The trapped disclosure subject sees snare — cannot exit, bears full cost of weaponization, and the defense mechanism is pathologized. The constrained disclosure subject sees tangled rope — genuine intimacy coordination entangled with weaponization extraction. The feminist therapeutic framework sees scaffold — building alternative norms with a generational sunset. Traditional couples therapy sees piton — its own intervention is degraded but persists through institutional inertia. The analytical observer sees tangled rope at the civilizational level — the constraint coordinates real intimacy needs while extracting through gendered asymmetry, and the pathologization framing is the mechanism that preserves the extraction while naturalizing it as individual pathology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Trapped disclosure subjects (powerless/trapped + victim) experience maximum extraction — high d → high f(d) → high chi. They bear the full cost of weaponization plus the pathologization of their defense mechanism, with no exit option. Constrained disclosure subjects (moderate/constrained + victim) experience moderate extraction — they have exit options but face high costs, and they receive some benefit from genuine relational connection. Extractive actors (institutional/arbitrage + beneficiary) experience low or negative extraction — they benefit from asymmetric disclosure norms and can exit when partners become 'too closed-off.' The relational trust commons (powerless/trapped + victim) experiences maximum extraction as an abstract collective good with no advocate. Organized agents building alternative frameworks (organized/mobile) experience low extraction — they have agency and see a structural exit path through norm change. The analytical observer sees the entanglement of coordination and extraction — disclosure does enable intimacy (genuine coordination function) but the asymmetry in weaponization consequences creates structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that wariness is not pathology but rational calibration to measured risk. The 'trust issues' framing is the extraction mechanism — it pathologizes the defense while preserving the attack. The analytical classification as tangled rope (not snare) reflects that the constraint does coordinate genuine intimacy needs (disclosure enables relational depth) while extracting through asymmetric weaponization. The perspectival gap between the extractive actor's rope and the trapped subject's snare is the diagnostic signal: the same structural phenomenon appears as coordination from the beneficiary position and pure extraction from the powerless victim position. The scaffold perspective (feminist therapeutic framework) is structurally real — consent-based norms and trauma-informed practice are creating alternative relational architectures — but the sunset timeline is generational, not immediate. The piton perspective (traditional couples therapy) reveals that the dominant therapeutic intervention addresses the symptom while ignoring the cause, maintained through institutional inertia rather than effectiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wariness_threshold_calibration,
    'What level of prior weaponization justifies what level of current wariness without being pathologized as ''trust issues''?',
    'Longitudinal tracking of disclosure-weaponization incidents and subsequent relational outcomes; comparison of wariness levels to actual risk of re-weaponization',
    'If wariness is under-calibrated to actual risk: subjects are re-traumatized. If over-calibrated: genuine intimacy opportunities are lost. The pathologization framing assumes over-calibration without measuring actual risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wariness_threshold_calibration, empirical, 'Optimal wariness calibration relative to measured weaponization risk').

omega_variable(
    therapeutic_framework_effectiveness,
    'Do trauma-informed relational frameworks actually reduce weaponization rates or merely shift the framing without changing behavior?',
    'Comparison of weaponization incident rates in relationships where both partners have trauma-informed training vs traditional couples therapy vs no intervention; control for selection effects',
    'If effective: scaffold sunset is real — new norms are structural. If ineffective: therapeutic literacy is itself theater, and the scaffold perspective is aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_framework_effectiveness, empirical, 'Whether trauma-informed frameworks reduce weaponization or just reframe it').

omega_variable(
    gendered_asymmetry_magnitude,
    'How much of the disclosure weaponization pattern is gendered vs general power asymmetry?',
    'Cross-tabulation of weaponization rates by gender, power differential, and economic dependency; comparison of same-gender vs different-gender dyads controlling for power',
    'If primarily gendered: the constraint is downstream of gendered_disclosure_asymmetry and inherits its structural properties. If primarily power-based: gender is a proxy for economic/social power, and the constraint operates independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gendered_asymmetry_magnitude, empirical, 'Proportion of weaponization asymmetry attributable to gender vs power').

omega_variable(
    internalized_pathologization,
    'Do subjects internalize the ''trust issues'' framing even when their wariness is objectively calibrated to risk?',
    'Self-report measures of wariness justification vs objective measurement of prior weaponization incidents; comparison of subject''s perceived pathology to therapist''s assessment controlling for actual risk history',
    'If internalized: the extraction is doubled — subjects carry the pathologization with them even after exiting the extractive relationship. If not internalized: the framing is external pressure only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_pathologization, empirical, 'Whether pathologization framing is internalized by subjects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extractive_disclosure_calibration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edc_tr_t0, extractive_disclosure_calibration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(edc_tr_t3, extractive_disclosure_calibration, theater_ratio, 3, 0.45).
narrative_ontology:measurement(edc_tr_t6, extractive_disclosure_calibration, theater_ratio, 6, 0.52).
narrative_ontology:measurement(edc_tr_t9, extractive_disclosure_calibration, theater_ratio, 9, 0.5).

% Extraction over time
narrative_ontology:measurement(edc_be_t0, extractive_disclosure_calibration, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(edc_be_t3, extractive_disclosure_calibration, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(edc_be_t6, extractive_disclosure_calibration, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(edc_be_t9, extractive_disclosure_calibration, base_extractiveness, 9, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extractive_disclosure_calibration, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of gendered_disclosure_asymmetry (the upstream rope that coordinates disclosure norms). The upstream constraint has low extractiveness (ε ≈ 0.28) reflecting that disclosure norms do coordinate genuine intimacy needs. This constraint has higher extractiveness (ε = 0.58) reflecting the weaponization pattern and pathologization framing. The two constraints are structurally distinct: the upstream coordinates disclosure; this constraint extracts through weaponization of what was disclosed. They form a constraint family linked by network.affects_constraints in the upstream file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
