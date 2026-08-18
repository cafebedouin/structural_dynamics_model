% ============================================================================
% CONSTRAINT STORY: virtue_performance_as_exculpation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_virtue_performance_as_exculpation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: virtue_performance_as_exculpation
 *   human_readable: Legible Care-Performance as Community Exculpation Signal
 *   domain: social_epistemology/domestic_power
 *
 * SUMMARY:
 *   In a household where a wife declines slowly under a husband's care, the
 *   community's shared rule for reading domestic behavior treats his most
 *   legible acts of devotion — bringing broth before the doctor arrives,
 *   standing in visible grief at the deathbed, publicly declining his
 *   contractual right to return a wife no longer capable of labor — as
 *   affirmative proof against suspicion, rather than as neutral or as
 *   circumstantial evidence of exactly the proximity and control that
 *   opportunity for harm requires. The rule genuinely solves a coordination
 *   problem (how to read private disposition from public behavior absent
 *   other evidence), which is why it persists and why the community defends
 *   it; but the same rule is structurally exploitable by exactly the person
 *   it is meant to screen, since he alone controls which acts are performed
 *   where witnesses can see them. This constraint is downstream of
 *   diagnostic_taxonomy_blind_spot: the medical taxonomy's inability to name
 *   slow domestic poisoning as a distinct category removes the one channel
 *   (independent physician suspicion) that could otherwise interrupt the
 *   community's inference before it hardens into consensus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(virtue_performance_as_exculpation, 0.72).
domain_priors:suppression_score(virtue_performance_as_exculpation, 0.68).
domain_priors:theater_ratio(virtue_performance_as_exculpation, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(virtue_performance_as_exculpation, extractiveness, 0.72).
narrative_ontology:constraint_metric(virtue_performance_as_exculpation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(virtue_performance_as_exculpation, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(virtue_performance_as_exculpation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(virtue_performance_as_exculpation, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(virtue_performance_as_exculpation, tangled_rope).
narrative_ontology:human_readable(virtue_performance_as_exculpation, "Legible Care-Performance as Community Exculpation Signal").
narrative_ontology:topic_domain(virtue_performance_as_exculpation, "social_epistemology/domestic_power").

domain_priors:requires_active_enforcement(virtue_performance_as_exculpation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(virtue_performance_as_exculpation, foma_silovich).
narrative_ontology:constraint_victim(virtue_performance_as_exculpation, marfa_osipovna).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(virtue_performance_as_exculpation, future_wives_in_household).
narrative_ontology:constraint_vindicates(virtue_performance_as_exculpation, dutiful_husband_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs the visible acts of care the village recognizes as proof of a good husband — bringing broth before the doctor arrives, standing formally at the deathbed, and publicly forgoing his legal right to return a wife who can no longer work. Each act simultaneously gives him unsupervised physical access to her food, drink, and body and converts that same access, in the eyes of witnesses, into evidence he could not have wished her harm. He controls which acts are performed in view of neighbors and which are not.
narrative_ontology:constraint_stakeholder(virtue_performance_as_exculpation, foma_silovich, beneficiary,
    moderate, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(virtue_performance_as_exculpation, foma_silovich, agenda_setter).

% Confined to the sickbed, dependent on her husband for food, medicine, and physical handling during her decline. She has no independent channel to report suspicion of her own condition, no legal standing to contest her husband's account, and no exit from the household even in principle — a wife who becomes non-productive can be returned by the husband, but she cannot leave him. Her decline is narrated entirely through his testimony and his visible gestures of devotion.
narrative_ontology:constraint_stakeholder(virtue_performance_as_exculpation, marfa_osipovna, payer,
    powerless, immediate, trapped, local).

% Neighbors, in-laws, and the local priest observe the household from outside and collectively produce the community's verdict on whether foul play is plausible. They apply a shared inferential rule — visible tenderness signals innocence — without access to what happens inside the sickroom when no one is present. Their judgment forecloses further inquiry; they are also excluded from the only evidence that would test the rule.
narrative_ontology:constraint_stakeholder(virtue_performance_as_exculpation, village_witnesses, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(virtue_performance_as_exculpation, village_witnesses, excluded).

% Called to examine Marfa Osipovna periodically but arrives after the household's narrative is already set by the husband's performance. His diagnostic categories (see the linked diagnostic_taxonomy_blind_spot constraint) lack a slot for slow domestic poisoning presenting as generic wasting illness, so his findings corroborate rather than interrupt the exculpatory reading. He would object to being used as a rubber stamp if he understood the pattern, but the taxonomy he works within does not surface it to him.
narrative_ontology:constraint_stakeholder(virtue_performance_as_exculpation, attending_physician, excluded,
    moderate, immediate, constrained, regional).

% Not yet in the household, but structurally positioned to inherit the same vulnerability: any successor wife who becomes ill under his care will be read through the same inferential rule, since the rule attaches to the performance of caretaking, not to this particular case. The precedent set here lowers the evidentiary bar a future occurrence would need to clear.
narrative_ontology:constraint_stakeholder(virtue_performance_as_exculpation, future_wives_in_household, payer,
    powerless, generational, trapped, local).

% The informal body that would adjudicate a formal accusation of wrongdoing within the household, should one ever be raised. They rely on the same community consensus the visible-care performance manufactures, so in practice they never reach the question because the underlying suspicion is extinguished before it reaches them.
narrative_ontology:constraint_stakeholder(virtue_performance_as_exculpation, village_elders_council, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(virtue_performance_as_exculpation, foma_silovich).
narrative_ontology:fixing_cost_class(virtue_performance_as_exculpation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The community's inferential rule solves a genuine, recurring problem: distinguishing negligent or malicious spouses from devoted ones with limited direct evidence, using visible behavior as a cheap, generally-reliable proxy for private disposition in a setting with no other verification mechanism.
% TRANSFER_FUNCTION: The rule transfers evidentiary benefit-of-the-doubt from the person under suspicion to the person performing care, and correspondingly transfers epistemic risk — the cost of being wrong about causal opportunity — onto the incapacitated, voiceless party who cannot testify to her own experience.
% ABSENT_VOICES: Marfa Osipovna herself, whose account of what happens during unwitnessed administrations of broth and medicine never enters the record; and the attending physician's diagnostic uncertainty, which is resolved by social consensus before it can be independently investigated.
% DISAPPEARANCE_RATIONALE: If the exculpatory reading of visible caretaking vanished, the same acts — feeding, administering medicine, private physical access during decline — would be read as opportunity requiring scrutiny rather than as evidence of innocence. Suspicion would attach to proximity itself, inquiries would follow domestic deaths more often, and the calculus for anyone contemplating harm under cover of dutiful appearance would change substantially.
% FOUNDING_PROBLEM: Villages needed some way to sort devoted spousal care from neglect or abuse without formal investigative machinery, using publicly observable behavior as the only available signal.
% FOUNDING_PROBLEM_CORROBORATION: Village witnesses and the elders' council attest the rule still solves a live problem of scarce evidence. The attending physician, reflecting after the fact, and later regional medical inquiry into domestic poisoning cases attest that the rule has become exploitable precisely because it is legible and performable — the signal it relies on can be manufactured by the person it is meant to screen, which no party benefiting from the current reading acknowledges.
narrative_ontology:disappearance_verdict(virtue_performance_as_exculpation, world_rearranges).
narrative_ontology:founding_problem_status(virtue_performance_as_exculpation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(virtue_performance_as_exculpation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-17',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(virtue_performance_as_exculpation, 'none', 1).
narrative_ontology:epsilon_provenance(virtue_performance_as_exculpation, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(virtue_performance_as_exculpation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(virtue_performance_as_exculpation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(virtue_performance_as_exculpation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.38 to 0.72) as the exculpatory reading compounds — each additional performed act of care further forecloses scrutiny of the prior ones, so the inferential shield strengthens precisely as the causal opportunity it should have flagged accumulates. Theater ratio tracks upward in parallel (0.30 to 0.61) because an increasing share of the husband's caretaking behavior is performed for witnessed effect rather than for Marfa's benefit — the broth brought 'before the doctor's visit' is timed for observability. Suppression is real but moderate rather than extreme (0.68 at end) because it operates through social consensus and evidentiary foreclosure rather than direct coercive threat; resistance is low (0.35) because the victim has no voice in the process and the community has no incentive to doubt a rule that also protects them when their own behavior is scrutinized.
 *
 * PERSPECTIVAL GAP:
 *   From Foma's seat, the pattern is genuine, functioning coordination — a socially legible way of demonstrating a virtue he may sincerely believe he possesses, or at minimum a way of avoiding the reputational and legal costs of appearing negligent. From Marfa's seat, the identical acts are indistinguishable from the mechanism of her own harm: the same hand that steadies the cup is the hand with sole discretion over what is in it. The engine's seat-level divergence here is the central structural fact the story exists to surface, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Foma Silovich sits near the full-beneficiary end: he administers the constraint (chooses which acts to perform visibly), collects its benefit (community-conferred innocence), and faces no structural cost from its operation. Marfa Osipovna sits at the full-target end: trapped, powerless, and unable to generate any competing narrative — the constraint's entire evidentiary weight is built from acts performed on her body without her testimony ever entering the record. Village witnesses occupy an intermediate position: they are not extracted from directly, but their agenda-setting role in applying the rule makes them structurally complicit administrators of an inference they did not design to be gamed.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (using visible behavior to infer private disposition absent other evidence) was arguably adequate when domestic life was genuinely opaque to any other verification. It has not been formally repealed, but its founding assumption — that legible caretaking correlates with benign intent strongly enough to license inference — has been substantially undermined by the very cases (this one included) where caretaking is manufactured for evidentiary effect. Classifying this as tangled_rope rather than pure snare preserves the fact that the rule still performs a real coordination function for the vast majority of households where no foul play occurs; collapsing it to snare would erase that base rate and make the corpus blind to genuinely devoted spouses who would be wrongly re-cast as suspects under a purely extractive reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    base_rate_of_genuine_devotion,
    'In what fraction of cases does visible spousal caretaking accurately track a benign disposition versus a manufactured performance concealing harm?',
    'Retrospective analysis of domestic death investigations where independent evidence (toxicology, witness testimony obtained after the fact, confession) later established ground truth, compared against the community''s contemporaneous verdict.',
    'A high base rate of genuine devotion supports retaining the coordination function (tangled_rope) with reformed evidentiary weight; a low base rate suggests the rule has drifted toward pure exploitation and the classification should move toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(base_rate_of_genuine_devotion, empirical, 'Whether the exculpatory inference tracks reality often enough to justify its coordination framing.').

omega_variable(
    performability_of_the_signal,
    'Is the signal (visible caretaking) cheap enough to fake that its evidentiary value should be discounted regardless of base rate?',
    'Compare the cost/effort of performing the legible acts (broth timing, public grief, declining the return-right) against the cost of actually caring for a declining spouse without the performative framing — a wide gap between the two indicates the signal is decoupled from the underlying disposition it claims to indicate.',
    'If the signal is cheap to fake, the community''s inferential rule is a Goodhart-style proxy failure independent of any individual''s intent, which would push the classification toward snare regardless of base rate; if costly to fake, the coordination function is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performability_of_the_signal, conceptual, 'Whether legible care-performance is a reliable signal or a gameable proxy.').

omega_variable(
    diagnostic_blind_spot_dependency,
    'How much of this constraint''s persistence depends on the upstream diagnostic taxonomy''s inability to name domestic poisoning, versus persisting independently through social inference alone?',
    'Compare outcomes in jurisdictions or eras where diagnostic categories for slow poisoning existed and were applied, against this setting where they do not, holding the social inference rule constant.',
    'If the constraint would persist even with a corrected diagnostic taxonomy, the two constraints are more structurally independent than the network edge suggests; if the exculpatory reading collapses once physicians can name the pattern, the upstream constraint is doing most of the causal work and this constraint is largely parasitic on it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_blind_spot_dependency, empirical, 'Degree of causal dependency between this constraint and the linked diagnostic taxonomy blind spot.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(virtue_performance_as_exculpation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(virt_tr_t0, virtue_performance_as_exculpation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(virt_tr_t4, virtue_performance_as_exculpation, theater_ratio, 4, 0.38).
narrative_ontology:measurement(virt_tr_t8, virtue_performance_as_exculpation, theater_ratio, 8, 0.46).
narrative_ontology:measurement(virt_tr_t12, virtue_performance_as_exculpation, theater_ratio, 12, 0.52).
narrative_ontology:measurement(virt_tr_t16, virtue_performance_as_exculpation, theater_ratio, 16, 0.56).
narrative_ontology:measurement(virt_tr_t20, virtue_performance_as_exculpation, theater_ratio, 20, 0.59).
narrative_ontology:measurement(virt_tr_t24, virtue_performance_as_exculpation, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(virt_be_t0, virtue_performance_as_exculpation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(virt_be_t4, virtue_performance_as_exculpation, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(virt_be_t8, virtue_performance_as_exculpation, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(virt_be_t12, virtue_performance_as_exculpation, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(virt_be_t16, virtue_performance_as_exculpation, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(virt_be_t20, virtue_performance_as_exculpation, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(virt_be_t24, virtue_performance_as_exculpation, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(virt_su_t0, virtue_performance_as_exculpation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(virt_su_t4, virtue_performance_as_exculpation, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(virt_su_t8, virtue_performance_as_exculpation, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(virt_su_t12, virtue_performance_as_exculpation, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(virt_su_t16, virtue_performance_as_exculpation, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(virt_su_t20, virtue_performance_as_exculpation, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(virt_su_t24, virtue_performance_as_exculpation, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(virtue_performance_as_exculpation, identity_coordination).
narrative_ontology:boltzmann_floor_override(virtue_performance_as_exculpation, 0.08).
narrative_ontology:affects_constraint(virtue_performance_as_exculpation, diagnostic_taxonomy_blind_spot).

% DUAL FORMULATION NOTE:
% This constraint and diagnostic_taxonomy_blind_spot form a two-member family describing the same underlying event (a wife's suspicious decline) from two structurally distinct angles: diagnostic_taxonomy_blind_spot (upstream, claimed snare) concerns the medical profession's categorical inability to name slow domestic poisoning as a distinct diagnosis, which removes the one channel of expert scrutiny that could interrupt lay inference. virtue_performance_as_exculpation (this story, claimed tangled_rope) concerns the community's social-inferential rule that reads visible caretaking as exculpatory. The two are linked rather than merged because they have different ε profiles, different beneficiary/victim mechanics (one operates through professional taxonomy, the other through lay social inference), and different remediation paths (taxonomic reform for the former, evidentiary norm reform for the latter) — collapsing them into one story would violate the ε-invariance principle by averaging two structurally distinct extraction mechanisms into one number.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
