% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Dueling Honor Satisfaction Mechanism (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the decline reading of the contested
 *   kernel 'honor_satisfaction_mechanism'. Under this reading, dueling did
 *   not become cognitively unthinkable (contraction) nor survive via multiple
 *   overlapping logics (composite), but rather weakened along a continuous
 *   gradient: enforcement capacity decayed, social costs rose, and the
 *   practice retreated to a fringe of traditionalist circles where it
 *   persists as performance. The constraint is structurally a pitonâa
 *   former coordination-and-extraction mechanism whose primary function has
 *   atrophied, leaving behind theatrical maintenance by an agenda-setting
 *   subculture that no longer captures concentrated benefits but cannot
 *   abandon the identity-fused practice. No concentrated beneficiary is
 *   declared, consistent with piton logic.
 *
 * KEY AGENTS:
 *   - Traditional honor circles (agenda_setter, organized, identity_locked): administer the dueling code and transmit it across generations, fused with their own social existence.
 *   - Fringe participants (payer, moderate, identity_locked): bear the physical and legal costs of the remaining duels, unable to exit without repudiating aristocratic masculine identity.
 *   - Bourgeois civic order (observer, institutional, analytical): criminalizes and stigmatizes the practice but does not invest in final eradication.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.28).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.33).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Dueling Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '0804e632-25ed-4df1-b078-1655e5dc5ebd').
narrative_ontology:cs_kernel_codification('0804e632-25ed-4df1-b078-1655e5dc5ebd', implicit).
narrative_ontology:cs_authority_grounding('0804e632-25ed-4df1-b078-1655e5dc5ebd', practice).
narrative_ontology:cs_interpretation_layer_present('0804e632-25ed-4df1-b078-1655e5dc5ebd').
narrative_ontology:cs_reading_relation('0804e632-25ed-4df1-b078-1655e5dc5ebd', honor_satisfaction_mechanism__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('0804e632-25ed-4df1-b078-1655e5dc5ebd', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('0804e632-25ed-4df1-b078-1655e5dc5ebd', foundational, decline_without_cognitive_erasure).
narrative_ontology:cs_axiom_status(decline_without_cognitive_erasure, holdable).
narrative_ontology:cs_axiom_grounding('0804e632-25ed-4df1-b078-1655e5dc5ebd', decline_without_cognitive_erasure, empirically_contingent).
narrative_ontology:cs_axiom('0804e632-25ed-4df1-b078-1655e5dc5ebd', secondary, fringe_practice_sustains_normative_kernel).
narrative_ontology:cs_axiom_status(fringe_practice_sustains_normative_kernel, holdable).
narrative_ontology:cs_axiom_grounding('0804e632-25ed-4df1-b078-1655e5dc5ebd', fringe_practice_sustains_normative_kernel, conventional).
narrative_ontology:cs_reference_frame('0804e632-25ed-4df1-b078-1655e5dc5ebd', aristocratic_honor_society).
narrative_ontology:cs_drift_state('0804e632-25ed-4df1-b078-1655e5dc5ebd', fringe_status_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0804e632-25ed-4df1-b078-1655e5dc5ebd', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, fringe_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the unwritten code of honor, train seconds, and organize the rituals that frame a duel as legitimate. Their social identity is constituted by this role; leaving would mean dissolving the group, not merely changing a policy. They no longer command mainstream society but still govern the micro-world of aristocratic masculinity.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, traditional_honor_circles, agenda_setter,
    organized, generational, identity_locked, regional).

% Young officers and gentry who still enter the dueling ground when challenged. They risk death, injury, and criminal prosecution, and gain little beyond the avoidance of shame within the honor-bound circle. Their families and civilian careers suffer, but refusing a challenge would mean expulsion from the identity they were raised to occupy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, fringe_participants, payer,
    moderate, biographical, identity_locked, regional).

% The consolidated legal and civic apparatus that treats dueling as an archaic crime against public order. It prosecutes sporadically, more to symbolize modern norms than to eradicate a practice it considers irrelevant. It could eliminate the remnant through sustained enforcement but judges the cost unjustified against the marginal frequency.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_civic_order, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, to provide a self-regulating, extra-legal mechanism for restoring social standing after an insult, using ritualized violence to demonstrate commitment and terminate disputes without feud.
% TRANSFER_FUNCTION: Moves physical risk, legal jeopardy, and social stigma from the challenged party to the duelist, while transferring symbolic status maintenance to the honor circle that sanctions the duel.
% ABSENT_VOICES: The bourgeois legalist majority that rejected honor culture entirely, and the women and dependents who bore the secondary costs of injury or widowhood, were largely excluded from the norm-setting discourse; their absence allowed the practice to persist as a male-aristocratic conversation.
% DISAPPEARANCE_RATIONALE: For the fringe participants and honor circles, the practice still organizes identity, status, and masculine aristocratic belonging; its disappearance would force a reorganization of their social world. For the wider society, the practice is already marginal, so its disappearance would leave most arrangements unchanged. The verdict is therefore contested between seats.
% FOUNDING_PROBLEM: The endemic problem of aristocratic honor disputes in a society without a centralized, legitimate mechanism for restoring reputation after public insult, and where feud or assassination threatened social order.
% FOUNDING_PROBLEM_CORROBORATION: Bourgeois civic observers and legal historians attest that the founding problem of honor satisfaction was superseded by court systems and social norms; the honor circles themselves do not corroborate the problem as still live, instead framing the practice as tradition rather than necessity.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is high (0.72) because remaining duels are ritual performances detached from mainstream status competition; the practice persists as identity theater rather than functional coordination. Extractiveness is low (0.28) because participation is marginal and frequency has collapsed. Suppression is moderate-low (0.33) because the honor circles can no longer suppress alternatives such as legal recourse or bourgeois apology. Resistance is high (0.78) because the state and broader society actively penalize and stigmatize the practice. Accessibility collapse is low (0.25) because alternatives are abundant and well-understood. All temporal measurements share a single time grid to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (traditional honor circles) experiences the constraint as identity maintenance and cultural preservation; the payer seat (fringe participants) experiences it as a dangerous, costly inheritance they cannot refuse without social death within the subculture. The observer seat (bourgeois civic order) sees an archaic nuisance that persists only because no one is harmed enough to mount a final eradication campaign.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary is declared because the honor circles do not capture material rents or external gains from the practice; their 'benefit' is identity reproduction confined to a shrinking fringe. Fringe participants are the clear victims, bearing physical and legal risk. Directionality is near-target for participants and near-neutral for honor circles. Because there is no concentrated beneficiary capturing extraction, the constraint is classified as a piton rather than a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemârestoring honor without feudâwas solved by legal and normative modernization. The constraint persists beyond its mandate, but not because any party extracts enough to defend it; rather, it persists through inertia and identity fusion. This prevents mislabeling it as a live tangled rope (which would require active enforcement serving a coordination function) or as a snare (which would require a capturer).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_validity,
    'Is the decline readingâweak but conceptually availableâthe best model, or does the contraction reading (cognitive unthinkability) or composite reading (multiple mechanisms) better fit the evidence?',
    'Comparative archival analysis of how duelists and their audiences described the practice: if described as ''unthinkable'' or ''ridiculous'', contraction fits; if described as ''imprudent'' or ''too costly'', decline fits; if multiple logics coexist, composite fits.',
    'Determines whether this constraint is a piton (decline), a dissolved rope (contraction), or a tangled rope (composite).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Which reading of the dueling kernel is structurally true.').

omega_variable(
    social_cost_vs_enforcement_decline,
    'Did dueling fade primarily because the honor circles lost capacity to enforce it, or because the bourgeois social penalty for participation became prohibitive?',
    'Quantitative analysis of duel frequency against indicators: court prosecutions, newspaper ridicule, club expulsions versus duel outcomes.',
    'If enforcement collapse drove the decline, the theater ratio is lower than if social stigma alone kept it alive; if social stigma dominated, the constraint persists as pure performance at the fringe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_vs_enforcement_decline, empirical, 'Mechanism of decline: enforcement decay versus social cost rise.').

omega_variable(
    fringe_persistence_motive,
    'What function, if any, does the remaining fringe performance of dueling serve for its agenda_settersâpure nostalgia, identity boundary maintenance, or latent coordination?',
    'Ethnographic and historical study of late-stage dueling circles to determine whether duels still resolve disputes or only signal in-group belonging.',
    'If latent coordination persists, the constraint may be a tangled rope rather than a piton; if pure signaling, piton classification is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_persistence_motive, empirical, 'Whether fringe dueling retains any coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_decline_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(honor_decline_tr_t10, honor_satisfaction_mechanism__decline_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(honor_decline_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(honor_decline_tr_t30, honor_satisfaction_mechanism__decline_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(honor_decline_tr_t40, honor_satisfaction_mechanism__decline_reading, theater_ratio, 40, 0.69).
narrative_ontology:measurement(honor_decline_tr_t50, honor_satisfaction_mechanism__decline_reading, theater_ratio, 50, 0.72).

% Extraction over time
narrative_ontology:measurement(honor_decline_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(honor_decline_be_t10, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(honor_decline_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(honor_decline_be_t30, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(honor_decline_be_t40, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(honor_decline_be_t50, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(honor_decline_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(honor_decline_su_t10, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(honor_decline_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(honor_decline_su_t30, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(honor_decline_su_t40, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(honor_decline_su_t50, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 50, 0.33).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one reading of the honor_satisfaction_mechanism kernel, decomposed per the Îµ-invariance principle. The decline reading (this file), contraction reading, and composite reading are structurally distinct claims with different epsilon values and should not be averaged or merged. The kernel is the practice of dueling as honor satisfaction; the readings differ on whether the mechanism declined, contracted cognitively, or operated via multiple overlapping logics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
