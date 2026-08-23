% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Reading: Pharaoh as Obligated Steward
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the reciprocity_reading of the
 *   maat_order_principle kernel: Ma'at is not merely divine radiance from an
 *   infallible king, nor a flatly distributed duty of all stations, but a
 *   mutual obligation structure in which Pharaoh must provide justice,
 *   stability, and redistributive resource management to maintain cosmic and
 *   social order. Under this reading, Pharaoh is structurally accountable to
 *   Ma'at; failure legitimates resistance or withdrawal of support. The
 *   reading competes with the divine_mandate_reading (Pharaoh embodies Ma'at
 *   by definition) and the distributed_maintenance_reading (order is
 *   everyone's responsibility). The authored metrics assume moderate
 *   extraction because the reciprocity norm caps what can be demanded and
 *   provides a moral exit via resistance, though the structural asymmetry
 *   between the palace and the peasantry remains significant.
 *
 * KEY AGENTS:
 *   - Pharaoh: Primary agenda_setter (institutional/identity_locked) â administers Ma'at, collects surplus, and is cosmologically bound to return justice and stability.
 *   - Temple estate: Beneficiary/agenda_setter (institutional/identity_locked) â interprets Ma'at, receives endowments, and transmits the ideological framework.
 *   - Administrative elite: Beneficiary (powerful/constrained) â manages extraction and benefits from rank and stability.
 *   - Rural cultivators: Primary payer (powerless/constrained) â produce surplus and taxes, receive conditional protection.
 *   - Corvee laborers: Payer (powerless/trapped) â bear the heaviest physical extraction with least leverage.
 *   - Modern historians: Observer (analytical/analytical) â adjudicate which reading the evidence supports.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.42).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.45).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Reading: Pharaoh as Obligated Steward").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '98237e59-809f-4f3d-b645-5d868e0907d6').
narrative_ontology:cs_kernel_codification('98237e59-809f-4f3d-b645-5d868e0907d6', fixed_text).
narrative_ontology:cs_authority_grounding('98237e59-809f-4f3d-b645-5d868e0907d6', lineage).
narrative_ontology:cs_interpretation_layer_present('98237e59-809f-4f3d-b645-5d868e0907d6').
narrative_ontology:cs_reading_relation('98237e59-809f-4f3d-b645-5d868e0907d6', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('98237e59-809f-4f3d-b645-5d868e0907d6', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('98237e59-809f-4f3d-b645-5d868e0907d6', foundational, pharaoh_reciprocal_obligation).
narrative_ontology:cs_axiom_status(pharaoh_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('98237e59-809f-4f3d-b645-5d868e0907d6', pharaoh_reciprocal_obligation, deontological).
narrative_ontology:cs_axiom('98237e59-809f-4f3d-b645-5d868e0907d6', foundational, popular_resistance_legitimate).
narrative_ontology:cs_axiom_status(popular_resistance_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('98237e59-809f-4f3d-b645-5d868e0907d6', popular_resistance_legitimate, deontological).
narrative_ontology:cs_reference_frame('98237e59-809f-4f3d-b645-5d868e0907d6', reciprocal_cosmic_order).
narrative_ontology:cs_drift_state('98237e59-809f-4f3d-b645-5d868e0907d6', late_period_collapse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98237e59-809f-4f3d-b645-5d868e0907d6', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_estate).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, administrative_elite).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, rural_cultivators).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the office responsible for maintaining Ma'at through the dispensation of justice, agricultural stability, and redistributive resource management. The office is fused with divine kingship; failure to uphold obligations risks cosmological disorder, elite defection, and popular resistance. Collects surplus, loyalty, and symbolic legitimation from the arrangement.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh, beneficiary).

% Interprets, ritualizes, and transmits Ma'at through cultic and scribal practice. Receives land endowments, labor allocations, and offerings from the redistributive system. Its authority depends on the continuity of the cosmological order that the reciprocity reading guarantees.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_estate, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, temple_estate, agenda_setter).

% Manages regional taxation, labor musters, and judicial administration on behalf of the crown. Benefits from stability, office-holding perquisites, and social rank. Exit is limited by land tenure and network dependence on the central apparatus.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, administrative_elite, beneficiary,
    powerful, biographical, constrained, national).

% Produce agricultural surplus and pay taxes in grain and livestock. Receive adjudication, flood protection, and famine relief in return. The reciprocity norm provides a moral vocabulary for protesting excessive extraction, but structural alternatives are limited by geography and land tenure.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, rural_cultivators, payer,
    powerless, biographical, constrained, regional).

% Conscripted for seasonal state projects including irrigation works, quarrying, and monument construction. Compensation is subsistence-level. Their labor sustains the infrastructure that the reciprocity reading promises as public goods, but they bear the highest physical cost and have the least leverage.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, immediate, trapped, national).

% Analyze the textual, archaeological, and comparative evidence for whether Ma'at operated as genuine reciprocity, as elite ideology, or as a distributed social ethic. Their assessments influence which reading of the kernel contemporary scholarship privileges.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, modern_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Nile Valley civilization by centralizing resource storage, irrigation maintenance, judicial oversight, and large-scale labor mobilization under a cosmological-political framework that ties legitimacy to performance.
% TRANSFER_FUNCTION: Moves agricultural surplus and labor from rural cultivators and corvee workers to the pharaonic state, temple estates, and administrative elite in exchange for promised justice, stability, and redistributive protection.
% ABSENT_VOICES: Women's formal voice in Ma'at adjudication is structurally muted; foreign populations and marginal communities are outside the reciprocal compact; future usurpers can only voice dissent through successful rebellion rather than institutional channels.
% DISAPPEARANCE_RATIONALE: If the Ma'at reciprocity principle vanished, the redistributive economy loses its legitimating framework. Subjects withhold labor and taxes, administrative elites fragment into regional competitors, and the centralized order collapses into foreign domination or civil war.
% FOUNDING_PROBLEM: Nile Valley agriculture required centralized coordination for flood prediction, irrigation repair, famine storage, and labor mobilization that village-level kinship organization could not sustain against ecological volatility.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological evidence of state granaries and canal networks corroborates the coordination need. However, Egyptological revisionists and comparative anthropologists from outside the pharaonic beneficiary set argue that non-hierarchical societies managed comparable hydraulic regimes, suggesting the centralized solution was politically chosen rather than environmentally necessitated.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.42 (moderate) because the reciprocity norm imposes a genuine ceiling: excessive extraction triggers the same moral logic that justifies resistance. Suppression is 0.45 because enforcement relies partly on legitimacy and partly on administrative and military coercion. Theater ratio begins low (0.20) when the Old Kingdom reciprocity was arguably more functional, but rises to 0.55 by the Ptolemaic period as ritual performance increasingly substitutes for substantive redistribution. Accessibility collapse is moderate (0.40): alternatives such as local strongmen or foreign rule persist. Resistance is 0.45: peasant flight, work stoppages, and dynastic overthrow are historically attested when reciprocity is perceived to fail.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic seat experiences the constraint as sacred duty and genuine coordination; failure risks cosmic chaos and elite defection. The cultivator seat experiences it as obligatory taxation and labor whose legitimacy is contingent on the ruler's performance. The structural asymmetry is that the cultivator's judgment of failure is voiced through costly resistance or flight, while the Pharaoh's judgment is institutionalized in law and cult. The engine should compute a wide divergence between the low-d beneficiary seats and the high-d payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh, temple estate, and administrative elite are structural beneficiaries: they collect surplus, labor, and legitimacy. Their identity_locked or constrained exit means they are fused to the constraint. Rural cultivators and corvee laborers are payers: they transfer grain and labor. The reciprocity reading gives them a conditional moral claim, but structurally their directionality sits near the target end because the transfer flows from them and their exit options are limited. No override is needed because the structural derivation captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading prevents mislabeling the constraint as a pure Snare because the coordination function (resource distribution, justice, flood management) is genuine and the agenda_setter is itself constrained by the kernel. It prevents mislabeling as a pure Rope because the extraction is asymmetric, actively enforced, and the payer seats bear costs that exceed their proportional benefits. If the reciprocity norm atrophied into pure performance without redistribution, the constraint would drift toward Piton or Snare; the temporal measurements show this drift beginning in the Late Period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_enforceability,
    'Is the reciprocity constraint on Pharaoh structurally enforceable by subjects before successful resistance, or is it only a post-hoc rhetorical justification for rebellion?',
    'Comparative historical analysis of pre-rebellion institutional checks (e.g., judicial appeals, provincial councils) versus purely post-facto rebel propaganda.',
    'If enforceable only post-hoc, the effective extraction is higher and the reciprocity reading functions partly as an ideological cover for extraction; if structurally enforceable, the constraint approaches a genuine coordination equilibrium with bidirectional accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_enforceability, empirical, 'Whether Ma''at reciprocity was an active constraint or a retrospective legitimating narrative.').

omega_variable(
    committer_reading_delta,
    'Does adopting the reciprocity reading rather than the divine_mandate reading change the measured extraction ceiling by making Pharaoh a payer-of-obligation rather than a pure beneficiary?',
    'Textual analysis of Instructions to Kings, rebel narratives, and administrative papyri to determine whether mutual obligation or unilateral divine radiance is the primary framing.',
    'If the textual record is ambiguous, the kernel remains under-determined and the reciprocity reading is one plausible construction among several, lowering confidence in the moderate extraction score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_delta, conceptual, 'Uncertainty about how the textual kernel under-determines the reciprocity extraction ceiling.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is compliance driven by internalized cosmological belief in Ma''at or by the coercive apparatus of tax collection and labor impressment?',
    'Archaeological and papyrological comparison of enforcement outlays (police, military) versus votive and religious practice indicating genuine belief.',
    'If compliance is primarily internalized, the authored suppression metric overstates external coercion and the constraint''s stability is higher than the raw score suggests; if primarily coercive, the reciprocity norm is not actually constraining behavior and the constraint is more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in Ma''at compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_recip_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(maat_recip_tr_t500, maat_order_principle__reciprocity_reading, theater_ratio, 500, 0.25).
narrative_ontology:measurement(maat_recip_tr_t1000, maat_order_principle__reciprocity_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(maat_recip_tr_t2000, maat_order_principle__reciprocity_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(maat_recip_tr_t2500, maat_order_principle__reciprocity_reading, theater_ratio, 2500, 0.45).
narrative_ontology:measurement(maat_recip_tr_t3000, maat_order_principle__reciprocity_reading, theater_ratio, 3000, 0.55).

% Extraction over time
narrative_ontology:measurement(maat_recip_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_recip_be_t500, maat_order_principle__reciprocity_reading, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(maat_recip_be_t1000, maat_order_principle__reciprocity_reading, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(maat_recip_be_t2000, maat_order_principle__reciprocity_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(maat_recip_be_t2500, maat_order_principle__reciprocity_reading, base_extractiveness, 2500, 0.6).
narrative_ontology:measurement(maat_recip_be_t3000, maat_order_principle__reciprocity_reading, base_extractiveness, 3000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(maat_recip_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(maat_recip_su_t500, maat_order_principle__reciprocity_reading, suppression_requirement, 500, 0.45).
narrative_ontology:measurement(maat_recip_su_t1000, maat_order_principle__reciprocity_reading, suppression_requirement, 1000, 0.35).
narrative_ontology:measurement(maat_recip_su_t2000, maat_order_principle__reciprocity_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(maat_recip_su_t2500, maat_order_principle__reciprocity_reading, suppression_requirement, 2500, 0.55).
narrative_ontology:measurement(maat_recip_su_t3000, maat_order_principle__reciprocity_reading, suppression_requirement, 3000, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Ma'at' conflates three structurally distinct constraints: divine mandate (unilateral top-down legitimacy), distributed maintenance (flat social responsibility), and reciprocity (mutual obligation with bounded extraction). They are modeled as a constraint family linked by network edges, not as a single story with multiple interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
