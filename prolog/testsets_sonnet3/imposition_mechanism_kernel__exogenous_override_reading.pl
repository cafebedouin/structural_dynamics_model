% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Norm Regime (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   A centralizing or conquering state imposes a new normative regime — a
 *   legal code, a language standard, a dress or religious mandate — by force,
 *   backed by garrisons, courts, and inspectorates, rather than through prior
 *   cultural adoption. This story instantiates the exogenous_override_reading
 *   of the imposition_mechanism_kernel: the claim that legitimacy in these
 *   episodes derives structurally from the state's monopoly on violence and
 *   its capacity for sustained monitoring, not from the norm having won
 *   cultural acceptance. Enforcement costs stay high and largely flat because
 *   compliance is conditional on continued surveillance rather than
 *   internalized; theater ratio rises slowly as the regime ages and some
 *   genuine administrative habituation sets in alongside ongoing coercion,
 *   but suppression itself does not meaningfully decay across the interval,
 *   consistent with a norm whose hold remains extraction-dependent rather
 *   than accepted.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: Primary agenda-setter (institutional/arbitrage) — declares and enforces the norm without waiting for acceptance
 *   - loyalist_administrative_class: Primary beneficiary (organized/mobile) — captures office and status by early compliance
 *   - local_customary_authorities: Displaced authority (moderate/constrained) — loses jurisdiction to the imposed order
 *   - noncompliant_subject_populations: Primary target (powerless/trapped) — bears direct sanction for continued prior practice
 *   - cultural_minority_communities: Concentrated target (powerless/trapped) — entire prior normative order is targeted
 *   - state_enforcement_apparatus: Coercive instrument (institutional/analytical) — the monitoring presence that makes compliance conditional
 *   - future_historians: Analytical observer (analytical/analytical) — assesses whether later 'acceptance' was genuine or path-dependent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.71).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.86).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norm Regime (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'a8d7dedf-4af9-4e50-bad7-dec4be46b708').
narrative_ontology:cs_kernel_codification('a8d7dedf-4af9-4e50-bad7-dec4be46b708', distributed).
narrative_ontology:cs_authority_grounding('a8d7dedf-4af9-4e50-bad7-dec4be46b708', extraction).
narrative_ontology:cs_interpretation_layer_present('a8d7dedf-4af9-4e50-bad7-dec4be46b708').
narrative_ontology:cs_reading_relation('a8d7dedf-4af9-4e50-bad7-dec4be46b708', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8d7dedf-4af9-4e50-bad7-dec4be46b708', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('a8d7dedf-4af9-4e50-bad7-dec4be46b708', foundational, legitimacy_derives_from_violence_monopoly).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_violence_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('a8d7dedf-4af9-4e50-bad7-dec4be46b708', legitimacy_derives_from_violence_monopoly, empirically_contingent).
narrative_ontology:cs_axiom('a8d7dedf-4af9-4e50-bad7-dec4be46b708', secondary, compliance_is_monitoring_conditional).
narrative_ontology:cs_axiom_status(compliance_is_monitoring_conditional, holdable).
narrative_ontology:cs_axiom_grounding('a8d7dedf-4af9-4e50-bad7-dec4be46b708', compliance_is_monitoring_conditional, empirically_contingent).
narrative_ontology:cs_reference_frame('a8d7dedf-4af9-4e50-bad7-dec4be46b708', coercive_monopoly_legitimacy).
narrative_ontology:cs_drift_state('a8d7dedf-4af9-4e50-bad7-dec4be46b708', post_enforcement_withdrawal, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a8d7dedf-4af9-4e50-bad7-dec4be46b708', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, loyalist_administrative_class).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, local_customary_authorities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, noncompliant_subject_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, cultural_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the new norm (a legal code, a language mandate, a religious or dress requirement) and backs it with police, courts, garrisons, and administrative sanction. Does not wait for or depend on popular acceptance; monitors compliance directly and punishes deviation. Its legitimacy claim rests on having successfully imposed order, not on the norm being culturally endorsed.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus, beneficiary).

% Local officials, translators, and enforcers who adopt the imposed norm early and are rewarded with office, land, or status. Their position depends on the norm's persistence and on their proximity to state power, not on genuine belief in the norm's legitimacy.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, loyalist_administrative_class, beneficiary,
    organized, biographical, mobile, national).

% Traditional leaders, elders, or clergy whose prior authority rested on cultural acceptance rather than coercive enforcement. The new norm displaces their jurisdiction; they can comply and lose standing, resist and face suppression, or attempt quiet parallel enforcement of the old norm at continuous risk of state reprisal.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_customary_authorities, payer,
    moderate, generational, constrained, regional).

% Ordinary subjects who continue prior practice out of habit, belief, or inaccessibility of the new norm. Face fines, corporal punishment, land confiscation, or exclusion from legal protection for noncompliance. Have no meaningful exit short of flight, which is itself heavily policed.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, noncompliant_subject_populations, payer,
    powerless, biographical, trapped, regional).

% Groups whose entire prior normative order (language, family law, ritual calendar) is superseded wholesale. Compliance requires abandoning practices central to group identity; resistance invites concentrated suppression, since the state treats their difference as the primary target of the imposition.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, cultural_minority_communities, payer,
    powerless, generational, trapped, regional).

% Police, military, and inspectorate personnel who carry out monitoring and punishment. Their continued deployment is what makes compliance conditional on surveillance rather than internalized — withdraw them and compliance measurably drops.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_enforcement_apparatus, agenda_setter,
    institutional, immediate, analytical, national).

% Assess after the fact whether the imposed norm outlasted the coercive apparatus that installed it, and whether apparent later 'acceptance' was genuine internalization or path-dependent adaptation to an irreversible fait accompli.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, future_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, state-legible normative order (legal code, language, administrative practice) enabling centralized taxation, conscription, and jurisdiction across a previously heterogeneous population.
% TRANSFER_FUNCTION: Moves jurisdictional authority, land, office, and legal standing from local customary authorities and noncompliant populations to the central state and the loyalist class that administers the new norm on its behalf.
% ABSENT_VOICES: Local customary authorities and cultural minority communities are structurally excluded from the norm-setting process itself — they are objects of the imposition, not parties to negotiating its terms; their objections appear only as 'resistance' to be suppressed, not as input.
% DISAPPEARANCE_RATIONALE: If state enforcement vanished, loyalist administrators and the central apparatus insist the norm would persist because it has become customary; local customary authorities and cultural minority communities insist compliance is conditional on monitoring and that withdrawal of coercion would see rapid reversion to prior practice — the two sides dispute which world is the counterfactual one.
% FOUNDING_PROBLEM: A newly centralizing or newly conquering state faced a fragmented normative landscape (multiple customary law systems, dialects, ritual calendars) that made uniform taxation, conscription, and jurisdiction administratively impossible.
% FOUNDING_PROBLEM_CORROBORATION: The central state apparatus and loyalist administrative class attest the problem was administrative incoherence and that it has been genuinely solved by successful unification. Ethnographic and legal-historical accounts from outside the benefiting parties — missionary and colonial-administrator field reports, and postcolonial legal historians studying compliance patterns after garrison withdrawal — corroborate that compliance tracked monitoring intensity rather than internalized acceptance, supporting the contested reading.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, contested).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and suppression (0.86) are both authored high and suppression is the higher and more stable of the two, reflecting that this reading's defining structural claim is that persistence rides on coercive capacity, not cultural buy-in — suppression is a raw structural property here and is not scaled by scope or power in the engine's arithmetic, only extraction is. Accessibility collapse is moderate (0.4) rather than near-mountain levels: alternatives (customary law, prior practice) remain cognitively and practically available to subjects even though acting on them is dangerous — this is precisely what distinguishes an imposed-and-resisted norm from a naturalized one. Resistance is authored high (0.78) because this reading's core claim is that the norm meets active, ongoing opposition rather than having been metabolized.
 *
 * PERSPECTIVAL GAP:
 *   From the central_state_apparatus and loyalist_administrative_class seats, the arrangement resolves as functional coordination successfully imposed — order was achieved where fragmentation previously reigned. From local_customary_authorities and the subject populations, the identical structure computes as extraction backed by force: their prior standing, land, and practice were transferred to state-aligned actors under threat of sanction. The engine should register this divergence directly from the differing power/exit declarations, not from any claim reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Central state apparatus and loyalist administrators sit near the beneficiary end: institutional or organized power, mobile or arbitrage-grade exit, direct collection of jurisdictional and status rents. Local customary authorities, noncompliant populations, and cultural minority communities sit near the target end: constrained-to-trapped exit, moderate-to-powerless standing, and they are the named victims bearing displaced authority, sanction, and identity-cost respectively. State enforcement apparatus is the instrument rather than a beneficiary in its own right — it is declared agenda_setter but analytically distinct from the political beneficiaries it serves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative incoherence across fragmented customary orders) is authored as contested rather than flatly dead or live: the beneficiary seats treat it as solved and superseded by successful unification, while corroborating outside evidence (missionary/administrator field reports, postcolonial legal-historical compliance studies) supports a status-quo-serving-power reading in which coercion, not cultural acceptance, is doing the ongoing work. This is exactly the mismatch the R5 consumer is built to catch: founding_problem_status=contested paired with disappearance_verdict=contested flags the arrangement for scrutiny rather than letting either side's self-report settle the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_selection,
    'Is the exogenous_override_reading the structurally correct account of this norm-imposition episode, or does the empirical record better fit endogenous_climb_reading or hybrid_legitimation_reading?',
    'Compliance-trajectory analysis after garrison or enforcement withdrawal: rapid reversion supports exogenous_override; persistence without monitoring supports endogenous_climb; partial persistence correlated with elite symbolic adoption supports hybrid_legitimation.',
    'If the compliance record shows persistence independent of monitoring, this story''s high-suppression, coercion-dependent characterization is wrong for this episode and the case belongs to a sibling reading instead — extractiveness and suppression values would need to move to a different constraint file, not be adjusted in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Which kernel reading (exogenous override vs. endogenous climb vs. hybrid legitimation) best fits the specific historical episode this story is authored against.').

omega_variable(
    sibling_reading_structural_delta,
    'What specific structural element do the three sibling readings of imposition_mechanism_kernel disagree about?',
    'Compare each reading''s authored suppression trajectory and founding_problem_corroboration: exogenous_override predicts flat-to-rising suppression with contested corroboration; endogenous_climb predicts declining suppression with beneficiary-external corroboration of genuine acceptance; hybrid predicts declining suppression concentrated among elite/symbolic adopters with slower diffusion elsewhere.',
    'Locates the disagreement precisely in the mechanism of legitimacy (violence-monopoly vs. bottom-up adoption vs. symbolic-transfer-plus-incentive) rather than in the fact of the norm''s imposition, which all three readings share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Where exactly the three kernel readings diverge structurally, for cross-reading comparison.').

omega_variable(
    naturalization_over_generations,
    'Does a norm imposed by exogenous override eventually become structurally indistinguishable from an endogenously-climbed norm once enough generations pass without living memory of the imposition?',
    'Track suppression_requirement and resistance metrics across multiple generational cohorts post-imposition; convergence toward near-zero suppression with declining resistance and no reversion upon enforcement lapse would indicate genuine naturalization.',
    'If naturalization occurs, this reading''s classification would need re-evaluation at long time horizons — the tangled_rope classification (coordination-plus-extraction under active enforcement) could shift toward rope as enforcement becomes vestigial, which is a distinct question from whether the ORIGINAL imposition was coercive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalization_over_generations, empirical, 'Whether coercively imposed norms can converge structurally with organically adopted norms given sufficient time, independent of their founding mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(impo_tr_t24, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(impo_tr_t32, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(impo_be_t24, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(impo_be_t32, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 8, 0.88).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 16, 0.87).
narrative_ontology:measurement(impo_su_t24, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(impo_su_t32, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of imposition_mechanism_kernel, each instantiating a structurally distinct claim about how a new norm acquired compliance: exogenous_override_reading (this file — coercion-derived legitimacy, high sustained suppression), endogenous_climb_reading (bottom-up adoption preceding state mandate, low suppression), and hybrid_legitimation_reading (symbolic authority transfer plus institutional incentive, intermediate suppression concentrated among elite adopters). Per the ε-invariance principle each reading carries its own ε and is not to be averaged with siblings; they are linked here for contamination-propagation and cross-reading comparison only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
