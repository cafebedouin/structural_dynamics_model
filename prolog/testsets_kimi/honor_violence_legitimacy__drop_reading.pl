% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Aristocratic Dueling Honor Code â Drop Reading
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Under the drop reading of the honor_violence_legitimacy kernel, dueling
 *   persisted as a structurally legitimate mechanism for aristocratic honor
 *   restoration well into the modern period, but its practice frequency
 *   declined sharply because external costsâstate prosecution, mortality
 *   risk, economic disruptionârose faster than the honor benefits.
 *   Conceptually, dueling remained thinkable; the honor-violence nexus was
 *   not redefined away (contra the contraction reading), but was priced out
 *   of regular use. This constraint story models the aristocratic honor code
 *   as a commitment system that coordinates elite identity while extracting
 *   asymmetric costs from individual participants and their kin.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_community: agenda_setter (organized/constrained) â maintains the code through social recognition and ostracism
 *   - dueling_participants: payer (moderate/constrained) â bear death and injury risk
 *   - centralizing_state: observer (institutional/analytical) â prosecutes duels, offers legal alternative
 *   - affected_kin_network: excluded/payer (powerless/trapped) â bear grief and economic loss without voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.62).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.58).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Aristocratic Dueling Honor Code â Drop Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '03ea4e2d-5424-4ed9-9103-0cda22be751b').
narrative_ontology:cs_kernel_codification('03ea4e2d-5424-4ed9-9103-0cda22be751b', implicit).
narrative_ontology:cs_authority_grounding('03ea4e2d-5424-4ed9-9103-0cda22be751b', practice).
narrative_ontology:cs_interpretation_layer_present('03ea4e2d-5424-4ed9-9103-0cda22be751b').
narrative_ontology:cs_reading_relation('03ea4e2d-5424-4ed9-9103-0cda22be751b', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('03ea4e2d-5424-4ed9-9103-0cda22be751b', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('03ea4e2d-5424-4ed9-9103-0cda22be751b', foundational, honor_violence_legitimacy_preserved).
narrative_ontology:cs_axiom_status(honor_violence_legitimacy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('03ea4e2d-5424-4ed9-9103-0cda22be751b', honor_violence_legitimacy_preserved, empirically_contingent).
narrative_ontology:cs_reference_frame('03ea4e2d-5424-4ed9-9103-0cda22be751b', aristocratic_honor_practice).
narrative_ontology:cs_drift_state('03ea4e2d-5424-4ed9-9103-0cda22be751b', modern_state_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('03ea4e2d-5424-4ed9-9103-0cda22be751b', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, aristocratic_honor_community).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, affected_kin_network).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively enforces the aristocratic honor code through social recognition, ostracism, and status allocation. Defines what constitutes a satisfiable insult and legitimate recourse. Members cannot exit the constraint without ceasing to be recognized as aristocratic; the code constitutes their collective identity.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, aristocratic_honor_community, agenda_setter,
    organized, generational, constrained, national).

% Individual gentlemen who bear the immediate life-and-death risk of the duel. Challenged parties face a structurally coerced choice between physical harm and social death (ostracism). Their exit is constrained by the twin pressures of aristocratic peer enforcement and state prohibition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, dueling_participants, payer,
    moderate, biographical, constrained, national).

% Monopolizes legitimate violence and prosecutes dueling as criminal homicide or assault. Offers judicial alternatives for dispute resolution but lacks standing within the aristocratic honor framework to redefine what counts as honorable. Observes and suppresses from outside the normative community.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, centralizing_state, observer,
    institutional, generational, analytical, national).

% Wives, children, parents, and dependents who bear the economic and emotional costs of mortality and injury but possess no voice in the honor code's operation. Their exclusion is structural: the aristocratic public sphere does not recognize kin objections as legitimate inputs into honor adjudication.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, affected_kin_network, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, affected_kin_network, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially codified mechanism for restoring honor and maintaining status hierarchy within the aristocratic class, channeling potentially unregulated feud into bounded, rule-governed single combat.
% TRANSFER_FUNCTION: Moves life-and-death risk, bodily injury, and familial grief from individual challenged gentlemen to the aristocratic collective's status-maintenance account, while concentrating social credit on those who demonstrate honor through willingness to fight.
% ABSENT_VOICES: Women, dependents, medical professionals coerced into attendance, and the centralizing bureaucratic state are excluded from the aristocratic honor conversation; they bear mortality, disorder, and legal jeopardy but have no standing to revise the code.
% DISAPPEARANCE_RATIONALE: Without the constraint, aristocratic masculine identity would lose a primary mechanism for status signaling and boundary maintenance; insult would migrate into legal, commercial, or trivial registers, fundamentally reorganizing elite sociability and the architecture of aristocratic distinction.
% FOUNDING_PROBLEM: In a decentralized society with weak sovereign judicial penetration, no authoritative mechanism existed to adjudicate status injury among armed elites, producing open-ended blood vengeance and feud.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state archives attest that centralized judicial systems replaced private honor violence by the nineteenth century; the aristocratic beneficiaries alone assert the founding problem persists, while penal codes and court records from outside the honor community confirm its obsolescence.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.62) because the honor code concentrates life-and-death risk on individual participants even as the practice grows rare. Suppression (0.58) reflects the combined force of aristocratic social enforcement and escalating state prosecution. Theater_ratio rises to 0.45 as duels become rarer and more ceremonial, with challenge, negotiation, and apology substituting for combat. Accessibility_collapse is high (0.78): once inside the honor framework, alternatives such as legal recourse or ignoring an insult collapse socially. Resistance (0.55) comes from state prosecution, religious condemnation, and familial opposition. Measurements share one time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic honor seat, the constraint is a necessary coordination mechanism preserving a distinctive moral community and preventing status anarchy; from the participant and kin seats, it is an asymmetric risk transfer enforced by social death; from the state seat, it is an obsolete violence practice to be suppressed. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic honor community sits near the beneficiary end (d low): the constraint subsidizes their collective status maintenance and boundary policing. Individual dueling participants sit near the target end (d high): they bear the concentrated life-and-death costs of the honor mechanism. Affected kin are even closer to full target due to powerlessness and trapped exit. The centralizing state's analytical exit places it outside the constraint's directionality capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâweak sovereign adjudication of elite insultâhas been superseded by centralized legal systems. Yet the constraint persists because it has been repurposed as an identity-coordination mechanism for aristocratic distinction. The drop reading captures this persistence as structural legitimacy plus external suppression, avoiding the false choice between pure coordination (rope) and pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the historical decline of dueling better explained by external cost pressures (drop reading), conceptual redefinition of honor (contraction reading), or their simultaneous operation (composite reading)?',
    'Comparative historical analysis measuring the relative explanatory power of legal suppression versus semantic shifts in honor discourse across multiple European jurisdictions.',
    'If the drop reading is correct, the constraint is a suppressed but live coordination mechanism; if contraction, it is a dissolved cognitive frame; if composite, the two mechanisms are inseparable and the drop reading is incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Which kernel reading best explains dueling''s decline').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dueling driven by state coercion (structural) or by internalized bourgeois values that made dueling unthinkable (internalized)?',
    'Examine dueling rates across jurisdictions with varying state enforcement intensity but shared cultural circuits; if rates diverge with enforcement intensity rather than cultural proximity, suppression is structural.',
    'If internalized, the drop reading overstates external costs and understates conceptual change, blurring the boundary with the contraction reading and raising effective extraction through cognitive lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural state suppression versus internalized value change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_drop_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(honor_drop_tr_t20, honor_violence_legitimacy__drop_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(honor_drop_tr_t40, honor_violence_legitimacy__drop_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(honor_drop_tr_t60, honor_violence_legitimacy__drop_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(honor_drop_tr_t80, honor_violence_legitimacy__drop_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(honor_drop_tr_t100, honor_violence_legitimacy__drop_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(honor_drop_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(honor_drop_be_t20, honor_violence_legitimacy__drop_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(honor_drop_be_t40, honor_violence_legitimacy__drop_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(honor_drop_be_t60, honor_violence_legitimacy__drop_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(honor_drop_be_t80, honor_violence_legitimacy__drop_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(honor_drop_be_t100, honor_violence_legitimacy__drop_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(honor_drop_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(honor_drop_su_t20, honor_violence_legitimacy__drop_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(honor_drop_su_t40, honor_violence_legitimacy__drop_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(honor_drop_su_t60, honor_violence_legitimacy__drop_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(honor_drop_su_t80, honor_violence_legitimacy__drop_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(honor_drop_su_t100, honor_violence_legitimacy__drop_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Drop reading of the honor_violence_legitimacy kernel, focusing on external cost suppression while preserving conceptual availability. Sibling readings include contraction (conceptual redefinition) and composite (overdetermined decline). This reading influences the composite reading by supplying one of its two causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
