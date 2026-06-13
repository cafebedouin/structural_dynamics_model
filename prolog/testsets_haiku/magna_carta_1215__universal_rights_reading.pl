% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Universal Rights Reading: Due Process as Natural Constitutional Law
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'Magna Carta 1215': the universal-rights reading. Under this reading,
 *   Clause 39 ('no free man shall be... punished... except by the lawful
 *   judgment of his peers or by the law of the land') is interpreted as
 *   establishing a transhistorical principle of due process applicable to ALL
 *   persons subject to state power, not merely to feudal barons or property
 *   holders. The constraint is claimed as a MOUNTAIN — a natural
 *   constitutional law that emerges from reason and necessity, not from
 *   negotiation or power. Extractiveness is low (0.31) because the constraint
 *   protects rather than extracts; suppression is minimal (0.18) because the
 *   constraint's naturalness makes active enforcement unnecessary — it
 *   persists as principle, not as administered coercion. The measurement
 *   series models the constraint's increasing recognition and codification in
 *   national constitutions and international law from 1215 (near-zero
 *   institutional adoption) through the present (global constitutional norm).
 *   Theater ratio remains low because the constraint's function (protection
 *   through law) has not been substantially displaced by performative
 *   compliance.
 *
 * KEY AGENTS:
 *   - all_persons_subject_to_state_power (beneficiary, universal, trapped exit) — the constraint protects all who face state coercion
 *   - state_power_apparatus (payer, institutional) — bound to exercise coercion only through lawful procedure
 *   - legal_interpreters_natural_law_tradition (beneficiary, institutional) — vindicate their theoretical commitments through this reading
 *   - baronial_privilege_reading_advocates (excluded, institutional) — would object that universalizing a feudal settlement misrepresents history
 *   - living_document_reading_advocates (excluded, institutional) — would contest the reading's claim to stable transhistorical meaning
 *   - historical_scholarship_community (observer, institutional) — documents the historical text's elite scope (free men < 5% of 1215 population) and the reading's tension with historical fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.31).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.18).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, mountain).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Universal Rights Reading: Due Process as Natural Constitutional Law").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:emerges_naturally(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '82ddd98c-3068-4648-983d-e002fab4e1e7').
narrative_ontology:cs_kernel_codification('82ddd98c-3068-4648-983d-e002fab4e1e7', fixed_text).
narrative_ontology:cs_authority_grounding('82ddd98c-3068-4648-983d-e002fab4e1e7', lineage).
narrative_ontology:cs_interpretation_layer_present('82ddd98c-3068-4648-983d-e002fab4e1e7').
narrative_ontology:cs_reading_relation('82ddd98c-3068-4648-983d-e002fab4e1e7', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('82ddd98c-3068-4648-983d-e002fab4e1e7', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('82ddd98c-3068-4648-983d-e002fab4e1e7', foundational, universal_due_process_natural_law).
narrative_ontology:cs_axiom_status(universal_due_process_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('82ddd98c-3068-4648-983d-e002fab4e1e7', universal_due_process_natural_law, deontological).
narrative_ontology:cs_axiom('82ddd98c-3068-4648-983d-e002fab4e1e7', foundational, free_men_equals_all_persons).
narrative_ontology:cs_axiom_status(free_men_equals_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('82ddd98c-3068-4648-983d-e002fab4e1e7', free_men_equals_all_persons, deontological).
narrative_ontology:cs_reference_frame('82ddd98c-3068-4648-983d-e002fab4e1e7', natural_due_process_principle).
narrative_ontology:cs_drift_state('82ddd98c-3068-4648-983d-e002fab4e1e7', contemporary_constitutional_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('82ddd98c-3068-4648-983d-e002fab4e1e7', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons_subject_to_state_power).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, legal_traditions_grounded_in_natural_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, legal_interpreters_natural_law_tradition).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_power_apparatus).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, universal_due_process_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, individual_rights_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, Clause 39 ('no free man shall be... punished... except by lawful judgment') extends to every person governed by state authority, regardless of property, status, or origin. The constraint guarantees that arbitrary detention and punishment are impossible — not as negotiated privilege but as an irreducible constitutional fact. Exit is literal nonexistence of the state, making this an absolute protection.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons_subject_to_state_power, beneficiary,
    powerless, civilizational, trapped, universal).

% Under this reading, the state is bound by due process universally. It cannot arbitrarily detain, punish, or execute any person; every exercise of coercive power must pass through lawful procedure. This is not a negotiated concession to particular groups but an absolute structural constraint on how state power operates. The reading treats Clause 39 as establishing an invariant condition, not a privilege for any subset.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_power_apparatus, payer,
    institutional, civilizational, analytical, universal).

% Jurists and legal scholars working within natural-law and universal-rights traditions vindicate their theoretical commitments through this reading. They point to Magna Carta as proof that due process is not a modern invention but an ancient constitutional principle, grounded in nature and reason, not in negotiated power. Their legitimacy as interpreters rests on this reading's coherence.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_interpreters_natural_law_tradition, beneficiary,
    institutional, civilizational, analytical, global).

% Scholars and jurists who read Magna Carta as a feudal contract limited to contracting parties are excluded from the conversation this reading constitutes. They would argue the charter was a negotiated settlement between King and barons, not a transhistorical statement of universal principle. Their exclusion reflects the reading's core commitment to universal applicability.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, baronial_privilege_reading_advocates, excluded,
    institutional, civilizational, analytical, global).

% Scholars who treat Magna Carta as an adaptive constitutional substrate whose meaning evolves through interpretive tradition would contest this reading's claim to stability and universality. They see rights as constructed through institutional practice over time, not as immutable principles. Their exclusion reflects disagreement about whether constitutional meaning is fixed or historical.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, living_document_reading_advocates, excluded,
    institutional, civilizational, analytical, global).

% Medieval historians document that Clause 39 applied only to 'free men' (less than 5% of the population in 1215), not to serfs, women, or other unfree persons. This reading's claim to universality requires treating the historical text's limited scope as an incompleteness to be transcended rather than a fixed boundary. Historians observe the tension between the reading's universalist premise and the historical artifact's elite framing.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historical_scholarship_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single standard of due process applicable to all state actors and all persons: no arbitrary detention, no extrajudicial punishment. Coordinates legal systems around a common principle that legitimate power must operate through law, not will.
% TRANSFER_FUNCTION: Transfers authority to constrain arbitrary state power from unilateral executive decision to lawful procedure. The constraint moves the locus of legitimate coercion from the ruler's will to the rule of law applied equally.
% ABSENT_VOICES: Medieval historians and those who read Magna Carta as a feudal settlement between elite parties are structurally excluded from the conversation this reading constitutes. They would object that universalizing a feudal privilege document misrepresents the historical record and that 'free men' was never intended to mean all persons. Their exclusion reflects the reading's core claim about transhistorical meaning.
% DISAPPEARANCE_RATIONALE: If this constraint (the universal application of due process) disappeared overnight, state power would revert to arbitrary detention and extrajudicial punishment as legitimate tools of rule. Legal systems worldwide build their legitimacy on this principle; its disappearance would require entire reconstructions of courts, criminal procedure, and the rule of law itself.
% FOUNDING_PROBLEM: Arbitrary state power over the person: rulers exercising coercive force (detention, punishment, execution) without legal process or predictable limits, leaving subjects defenseless against tyranny.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, constitutional courts in democracies, and the UN Convention Against Torture all cite Magna Carta as a founding precedent for the principle that no person may be arbitrarily detained or punished. Non-beneficiary witnesses (historians, comparative legal scholars, authoritarian regimes that resist the principle) attest the founding problem remains live — the problem persists wherever due process is weakened or abandoned. The principle's universality is contested by those who read Magna Carta narrowly, but the founding problem it addresses is observable in contemporary rights violations.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, ExtMetricName, E),
    domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(magna_carta_1215__universal_rights_reading),
    narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The universal-rights reading claims that Clause 39 establishes a natural constitutional principle discoverable by reason: that no person may be arbitrarily detained or punished, regardless of status. This is a MOUNTAIN claim — it asserts the principle emerges from necessity and natural law, not from feudal negotiation or particular institutional choice. Extractiveness is minimal (0.31 at interval end) because the constraint protects against extraction rather than enabling it; the slight rise over 800 years reflects not growing extraction but increasing codification and institutional adoption, which require minimal enforcement infrastructure because the principle's naturalness makes compliance expected. Suppression is correspondingly low (0.18) because naturalness eliminates the need for coercive maintenance. Accessibility collapse is very high (0.89) because once the principle is understood and recognized, alternatives (arbitrary rule, extrajudicial punishment) become almost unthinkable in legal discourse. Resistance is moderate (0.22) because the principle faces resistance from authoritarian regimes and historical skeptics, but this resistance is external to the principle's internal logic — the principle itself meets little resistance from those who accept its premises. Theater ratio is low (0.12) because the constraint's function (protecting due process) has not been substantially replaced by performance — courts still do materially protect persons through legal procedure. The measurement series captures the constraint's glacial movement from near-nonexistent institutional codification (1215–1400s) through gradual constitutional incorporation (1600s–1800s) to near-universal recognition (1900s–present), modeled as increasing societal acceptance of the universal-rights reading.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is minimal under this reading: all persons benefit equally from the protection (they are universally protected by due process), and the state bears a uniform constraint (it must follow lawful procedure for everyone). The constraint does not create asymmetric extraction because both beneficiaries and the apparatus that enforces it understand the constraint as a natural necessity, not a negotiated advantage. However, the reading itself sits in radical perspectival tension with the baronial-privilege reading: from the baronial seat, Magna Carta is a feudal settlement protecting elite property; from the universal-rights seat, it is a transhistorical statement of human dignity. This is not a difference in power or exit options between seats within one constraint — it is a difference in which constraint they see at all. The engine should compute convergence within this reading (universal protection, no asymmetric directionality) and flagged divergence between readings (different ε, different beneficiary sets, different constraint types).
 *
 * DIRECTIONALITY LOGIC:
 *   Under the universal-rights reading, directionality does not follow the typical power-based pattern. All persons subject to state power are beneficiaries with high directionality toward benefit (d near 0.0) because the constraint protects them without extracting from them. The state apparatus is the structural payer, but its 'cost' is not extraction — it is the necessity of operating through law rather than will. This is a symmetric situation (d near 0.5) rather than asymmetric extraction, because the state's constraint is presented as an intrinsic feature of legitimate rule, not as a rent-extraction mechanism. No directionality override is needed because the derivation chain (beneficiaries = all persons, victims = none explicitly, power atoms across all levels, exit = universal constraint = trapped exit for all) produces the right d naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The universal-rights reading does not present a mandatrophy case. The founding problem (arbitrary state power over the person) remains demonstrably live — authoritarian regimes and rights violations persist where the principle is weakened. The constraint's function (protecting persons through due process) has not atrophied; it remains the primary function courts and legal systems claim to serve. Theater ratio remains low because performance has not substantially displaced function. The reading avoids false-mandate diagnosis because the constraint's actual operation (legal protection through procedure) still serves the stated founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_meaning,
    'Is Clause 39''s due-process principle a natural law discoverable by reason, or is it a constructed meaning attributed to an elite feudal settlement by later interpreters?',
    'Comparison with non-English medieval texts and legal traditions: do other feudal societies independently arrive at universal due-process principles, or is the principle''s universality a distinctly English / Common-Law interpretive product? Historical analysis of 13th-century legal thought vs. modern universal-rights thought.',
    'If the principle is natural law, the mountain classification holds and the constraint is genuinely non-extractive. If the principle is a constructed post-hoc reading, the constraint may be a false summit — a rhetorical framing that benefits legal interpreters who claim descent from universal principles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_meaning, conceptual, 'Whether due process is natural law or interpretive construction').

omega_variable(
    original_scope_vs_claimed_universality,
    'What was the original scope of ''free men'' in 1215 (approximately 5% of the population: property-holding men), and does the reading''s claim to universality require dismissing or transcending the historical limitation?',
    'Historical scholarship on medieval English society; comparison with how other historical documents that originally applied to elites (e.g., US Constitution) have been universalized through amendment and interpretation.',
    'If the reading requires transcending the historical text''s elite scope, it is a reinterpretation — the principle is projected onto the text rather than derived from it. This affects whether the mountain claim is justified: mountains should emerge naturally, not require layers of reinterpretation to reach universal scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_scope_vs_claimed_universality, empirical, 'Gap between historical scope and claimed universality').

omega_variable(
    natural_law_authority_grounding,
    'If due process is natural law, what authority grounds the claim that reason and nature yield this specific principle? Is the authority theological (divine law), philosophical (Lockean natural rights), or something else?',
    'Examination of which natural-law theories the universal-rights reading actually relies on; tracing the lineage of authority claims in constitutional scholarship.',
    'Different authority groundings (theological vs. secular reason) would shift the constraint''s relationship to institutions and interpretation. A theological grounding might make the constraint less contestable; a secular-reason grounding makes it vulnerable to empirical or philosophical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_authority_grounding, conceptual, 'The epistemic foundation of natural-law authority for due process').

omega_variable(
    suppression_asymmetry_across_regimes,
    'Does the measured suppression (0.18, low) reflect genuine naturalness of the principle, or does it reflect the fact that this reading is authored from within constitutional-law traditions that already accept universal due process?',
    'Observation of suppression intensity in authoritarian regimes that explicitly reject the universal-rights reading and treat due process as a Western imposition or unnecessary restraint on state power. If suppression is higher in those contexts, the low 0.18 figure reflects position-contingency, not principle-invariance.',
    'If suppression varies by regime and ideological position, the constraint''s naturalness is compromised — it requires active suppression of alternatives in contexts where it is contested. This would suggest the mountain classification is relative to the observer''s position, not invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_asymmetry_across_regimes, empirical, 'Whether low suppression reflects naturalness or observer-position contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(magn_tr_t0, projected).
narrative_ontology:measurement(magn_tr_t100, magna_carta_1215__universal_rights_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t200, magna_carta_1215__universal_rights_reading, theater_ratio, 200, 0.07).
narrative_ontology:measurement_basis(magn_tr_t200, observed).
narrative_ontology:measurement(magn_tr_t400, magna_carta_1215__universal_rights_reading, theater_ratio, 400, 0.09).
narrative_ontology:measurement_basis(magn_tr_t400, observed).
narrative_ontology:measurement(magn_tr_t600, magna_carta_1215__universal_rights_reading, theater_ratio, 600, 0.11).
narrative_ontology:measurement_basis(magn_tr_t600, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_1215__universal_rights_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(magn_be_t0, projected).
narrative_ontology:measurement(magn_be_t100, magna_carta_1215__universal_rights_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t200, magna_carta_1215__universal_rights_reading, base_extractiveness, 200, 0.15).
narrative_ontology:measurement_basis(magn_be_t200, observed).
narrative_ontology:measurement(magn_be_t400, magna_carta_1215__universal_rights_reading, base_extractiveness, 400, 0.24).
narrative_ontology:measurement_basis(magn_be_t400, observed).
narrative_ontology:measurement(magn_be_t600, magna_carta_1215__universal_rights_reading, base_extractiveness, 600, 0.29).
narrative_ontology:measurement_basis(magn_be_t600, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_1215__universal_rights_reading, base_extractiveness, 800, 0.31).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__universal_rights_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.18).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'Magna Carta 1215'. The kernel is the text of Clause 39 and the question of how to interpret 'free men': as feudal barons only (baronial-privilege reading) or as all persons (universal-rights reading). The living-document reading treats the original scope as legitimately evolving through interpretive practice. Each reading is a separate constraint story with its own ε, beneficiary set, and type. They are linked via network.affects_constraints: the universal-rights reading (this file) claims that the principle of due process is natural and universal; the baronial-privilege reading claims it was a feudal settlement; the living-document reading claims the original meaning has been legitimately reconstituted through institutional practice. The ε values differ substantially (universal-rights: 0.31 because protection is non-extractive; baronial-privilege: likely higher extractiveness if read as elite privilege; living-document: depends on how practice is read). This family decomposition follows the ε-invariance principle: different readings have different observable characteristics and therefore different ε values. They are not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
