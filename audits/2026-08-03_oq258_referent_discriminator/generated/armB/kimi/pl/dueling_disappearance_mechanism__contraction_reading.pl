% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction reading of the
 *   dueling_disappearance_mechanism kernel: the claim that dueling did not
 *   merely fall to institutional substitution or overdetermined causes, but
 *   became culturally unthinkable because dignity-culture axioms displaced
 *   honor-culture axioms as the moral substrate of elite social life. The
 *   constraint is the resulting dignity-culture substrate itself—treated by
 *   this reading as an irreversible, naturalized order (a claimed mountain).
 *   Honor-culture practitioners, whose framework became illegible, compose
 *   the victim set. The bourgeois professional class, whose capital form is
 *   protected and elevated by the new axioms, composes the beneficiary set.
 *   The divergence between the mountain claim and the presence of
 *   beneficiaries and victims is deliberate: the engine will measure whether
 *   this substrate operates as a genuine natural law or as a false summit
 *   serving class interests.
 *
 * KEY AGENTS:
 *   - Honor-culture practitioners (gentlemen, officers, aristocrats): Primary targets—experience the constraint as the silencing of their identity framework (organized/generational/identity_locked).
 *   - Bourgeois professional class: Primary normative beneficiaries—experience the constraint as stable background enabling their form of status accumulation (organized/generational/mobile).
 *   - Historical sociologists: Analytical observers—reconstruct the mechanism without bearing its costs or collecting its benefits (analytical/civilizational/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.25).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.25).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '3784be3b-b452-4e53-b405-355ea381b43c').
narrative_ontology:cs_kernel_codification('3784be3b-b452-4e53-b405-355ea381b43c', implicit).
narrative_ontology:cs_authority_grounding('3784be3b-b452-4e53-b405-355ea381b43c', practice).
narrative_ontology:cs_interpretation_layer_present('3784be3b-b452-4e53-b405-355ea381b43c').
narrative_ontology:cs_reading_relation('3784be3b-b452-4e53-b405-355ea381b43c', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('3784be3b-b452-4e53-b405-355ea381b43c', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('3784be3b-b452-4e53-b405-355ea381b43c', foundational, honor_retribution_categorically_impermissible).
narrative_ontology:cs_axiom_status(honor_retribution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('3784be3b-b452-4e53-b405-355ea381b43c', honor_retribution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('3784be3b-b452-4e53-b405-355ea381b43c', foundational, moral_worth_institutionally_mediated).
narrative_ontology:cs_axiom_status(moral_worth_institutionally_mediated, holdable).
narrative_ontology:cs_axiom_grounding('3784be3b-b452-4e53-b405-355ea381b43c', moral_worth_institutionally_mediated, deontological).
narrative_ontology:cs_reference_frame('3784be3b-b452-4e53-b405-355ea381b43c', dignity_culture_axiomatic_equilibrium).
narrative_ontology:cs_drift_state('3784be3b-b452-4e53-b405-355ea381b43c', post_victorian_consolidation, gap(stable, minor, false)).
narrative_ontology:cs_created_at('3784be3b-b452-4e53-b405-355ea381b43c', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, dignity_culture_axiom_set).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, honor_culture_obsolescence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen, officers, and aristocrats whose social standing and self-concept depended on honor-culture codes, including the duel as a legitimate mechanism for dispute resolution. As dignity-culture norms displaced honor-culture axioms in the nineteenth century, their framework became illegible: the practices that once conferred status now produced social death, professional ruin, and legal jeopardy. Exit from the constraint means abandoning an identity fused to lineage, masculine honor, and face-to-face retribution; for many this was psychologically and socially unthinkable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    organized, generational, identity_locked, continental).

% Emergent social stratum whose status depends on educational credentials, professional licensure, and institutional affiliation rather than lineage or lethal honor. The dignity-culture axioms that render dueling unthinkable simultaneously elevate their form of capital—self-control, deferral, bureaucratic procedure—and protect them from honor-culture violence. They do not actively administer the constraint but are its primary normative beneficiaries.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, continental).

% Analyze the transition from honor to dignity culture through archival, demographic, and discursive evidence. They operate outside the constraint's direct cost/benefit structure, reconstructing the mechanism of disappearance from a position that does not depend on either culture for status.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dignity-culture axioms solve the problem of social order among armed elites by replacing lethal, reputation-based dispute resolution with internalized self-control and deferral to institutional arbitration.
% TRANSFER_FUNCTION: Moves social standing and legal protection from those whose status depends on honor-culture codes to those who operate within dignity-culture institutions; extracts social viability from honor-culture practitioners by rendering their normative framework illegible.
% ABSENT_VOICES: Honor-culture practitioners who continued to view dueling as a legitimate moral necessity are absent from the dignity-culture discourse that wrote the history of the transition; their objections survive only in marginalized memoirs, regional holdouts, and later anthropological reconstruction.
% DISAPPEARANCE_RATIONALE: If the axiomatic unthinkability of dueling vanished overnight, honor-culture frameworks would regain immediate social legibility, lethal dispute resolution would re-enter the repertoire of status groups, and the modern institutional edifice built on dignity-culture premises—contract law, professional discipline, bureaucratic procedure—would face foundational legitimation crises.
% FOUNDING_PROBLEM: Chronic lethal violence and unstable status competition among armed elites in the early modern period, which honor-culture norms regulated but could not suppress.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists outside the honor-culture tradition attest that dueling rates fell precipitously in the nineteenth century; however, the claim that this transition was driven by axiomatic displacement rather than institutional substitution is contested by institutionalist historians who attribute the decline to courts, banking, and libel law.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low but non-zero (0.25) because the constraint does not extract material rents; it extracts social viability from honor-culture practitioners by rendering their normative framework illegible. Suppression is low (0.25) because enforcement is largely internalized and diffuse—courts and professional bodies intervene only at the margins. Accessibility collapse is very high (0.90) because, once dignity-culture axioms are internalized, honor-culture alternatives become nearly unthinkable. Resistance is low (0.20) because the constraint is hegemonic; honor-culture holdouts are marginalized and their resistance is coded as pathology rather than dissent. The measurement series show a modest rise in extractiveness as the new culture consolidates, a flat theater ratio (not performative), and a falling suppression requirement as internalization replaces external enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The honor-culture practitioner seat and the bourgeois-professional seat should compute differently. From the practitioner position, the constraint is experienced as a forced abandonment of an identity-fused framework—effective extraction is amplified by identity-locked exit and high scope. From the bourgeois position, the constraint is experienced as the natural background of civilized life, with effective extraction damped to near zero or inverted into subsidy. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners are declared victims with identity-locked exit, which drives directionality toward the full-target end (high d). Their spatial scope is continental, amplifying effective extraction. The bourgeois professional class is declared beneficiaries with mobile exit, driving directionality toward the beneficiary end (low d). The historical sociologist is analytical and excluded from the cost/benefit flow entirely. No override is needed because the structural derivation matches the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—chronic lethal violence among armed elites—is arguably still live in attenuated form, so the constraint's persistence is not yet mandatrophy. However, if the problem were ever fully solved while the substrate persisted purely by inertia, the combination of dead founding problem and world-rearranging disappearance verdict would flag a zombie. Because this reading claims mountain, persistence is expected regardless of mandate status; the mountain type absorbs the zombie signal into its natural-law claim. The test is whether the metrics and beneficiary/victim structure corroborate that claim or expose it as a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_culture_naturality,
    'Is dignity culture a genuine irreversible moral substrate (mountain), or a socially constructed normative regime serving the interests of an emergent professional and bourgeois class?',
    'Comparative historical analysis of whether dignity-culture norms emerged independently across societies or tracked the rise of specific institutional actors (state courts, capitalism, professional associations).',
    'If the latter, the mountain claim is a false summit and the constraint reclassifies as tangled rope or snare; if the former, the victim set is reinterpreted as collateral damage of moral progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_culture_naturality, conceptual, 'Whether dignity culture is natural law or class construction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Does the unthinkability of dueling operate primarily through internalized shame (internalized suppression) or through external structural sanctions (legal prohibition, professional ostracism)?',
    'Archival analysis of dueling prosecutions, professional expulsions, and memoirs of honor-culture holdouts to measure the relative weight of internal versus external coercion across the interval.',
    'If primarily internalized, effective suppression is higher than structural measures suggest and the constraint behaves as an identity-locked mountain; if external, the constraint is closer to an actively enforced scaffold or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    reading_kernel_contest,
    'Does the contraction reading''s emphasis on axiomatic displacement foreclose the institutional displacement reading, or can both mechanisms coexist as co-causes?',
    'Historiographic synthesis assessing whether institutional substitution (courts, libel law) is sufficient without cultural unthinkability, or vice versa; cross-referencing regional variation where institutions changed before culture or culture before institutions.',
    'If institutional substitution is sufficient, the contraction reading overstates the mountain-like irreversibility of dignity culture; if cultural displacement is necessary, the institutional reading understates the constraint''s substrate character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Structural relationship between contraction and institutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(duel_tr_t10, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(duel_tr_t50, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(duel_be_t10, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(duel_be_t50, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 50, 0.23).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 60, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(duel_su_t10, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(duel_su_t40, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(duel_su_t50, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 60, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The dueling_disappearance_mechanism kernel decomposes into three structurally distinct readings: contraction_reading (axiomatic displacement producing a claimed mountain), institutional_displacement_reading (institutional substitution producing coordination/extraction dynamics), and overdetermined_composite_reading (causal overdetermination with multiple independent mechanisms). Each reading carries a different epsilon, beneficiary/victim structure, and constraint type. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
