% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor-Satisfaction via Dueling — Gradual Decline Reading
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This story instantiates the decline_reading of the
 *   honor_satisfaction_mechanism kernel: dueling as a practice that
 *   persisted, gradually thinning in frequency and social reach, until it
 *   reached fringe status by the turn of the twentieth century. On this
 *   reading the mechanism itself never became cognitively unthinkable (that
 *   is the contraction_reading, a different constraint) and no discrete
 *   bundle of replacement mechanisms displaced it in a coordinated fashion
 *   (that is the composite_reading, also a different constraint). Instead,
 *   the same practice, with the same basic structure, simply occurred less
 *   and less often as its social utility eroded relative to its risk, its
 *   legal cost, and the availability of substitutes, without ever crossing a
 *   sharp category boundary. Epsilon here is authored as declining but never
 *   zero and never sharply discontinuous — a smooth attrition curve, not a
 *   cliff.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.28).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.35).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor-Satisfaction via Dueling — Gradual Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '43569692-196b-40bd-825b-8fbb11ae9571').
narrative_ontology:cs_kernel_codification('43569692-196b-40bd-825b-8fbb11ae9571', implicit).
narrative_ontology:cs_authority_grounding('43569692-196b-40bd-825b-8fbb11ae9571', practice).
narrative_ontology:cs_interpretation_layer_present('43569692-196b-40bd-825b-8fbb11ae9571').
narrative_ontology:cs_reading_relation('43569692-196b-40bd-825b-8fbb11ae9571', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('43569692-196b-40bd-825b-8fbb11ae9571', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('43569692-196b-40bd-825b-8fbb11ae9571', foundational, practice_persists_as_continuous_gradient).
narrative_ontology:cs_axiom_status(practice_persists_as_continuous_gradient, holdable).
narrative_ontology:cs_axiom_grounding('43569692-196b-40bd-825b-8fbb11ae9571', practice_persists_as_continuous_gradient, empirically_contingent).
narrative_ontology:cs_axiom('43569692-196b-40bd-825b-8fbb11ae9571', secondary, single_mechanism_sufficient_explanation).
narrative_ontology:cs_axiom_status(single_mechanism_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('43569692-196b-40bd-825b-8fbb11ae9571', single_mechanism_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_reference_frame('43569692-196b-40bd-825b-8fbb11ae9571', aristocratic_honor_code_at_peak_frequency).
narrative_ontology:cs_drift_state('43569692-196b-40bd-825b-8fbb11ae9571', fringe_status_1900, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43569692-196b-40bd-825b-8fbb11ae9571', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, dueling_seconds_and_code_arbiters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, dueling_participants_and_families).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, gradualist_norm_change_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the duel and its threat to police status boundaries and settle insults without recourse to courts that would expose them to public humiliation or plebeian jurisdiction. As the century progresses, fewer of them actually fight, but the class continues to invoke the code of honor rhetorically and socially even as participation thins.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_class, beneficiary,
    powerful, generational, constrained, national).

% Administers the informal rules — codes of honor, negotiation of terms, adjudication of whether an apology suffices instead of a fight. Their function persists past the point where most disputes are actually fought, increasingly resolving matters through negotiated apology rather than combat, which is itself evidence of decline rather than transformation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_seconds_and_code_arbiters, agenda_setter,
    organized, biographical, constrained, national).

% Individual men who receive or issue a challenge bear real risk of death or injury, and their families bear grief and reputational fallout regardless of outcome. Over the measured decline, the frequency with which any given insult escalates to an actual duel falls, but for the participants unlucky enough to be swept into a still-live instance, the mechanism remains as lethal and as socially compulsory as ever.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_participants_and_families, payer,
    moderate, immediate, trapped, local).

% Increasingly criminalizes dueling and offers civil and criminal alternatives for resolving insult, but for most of the decline period exercises only selective, inconsistent enforcement — prosecutions are rare, sentences light, elite defendants frequently pardoned. Their gradually hardening but long-underenforced opposition is a background condition of the decline rather than its cause on this reading.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_authorities, excluded,
    institutional, generational, analytical, national).

% Tracks incident counts, prosecution rates, and participation demographics across decades to characterize the trajectory as gradual frequency decline rather than sudden category collapse or multi-mechanism replacement — the reading this story instantiates.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially recognized procedure for resolving accusations of dishonor among status-conscious peers, substituting a bounded, rule-governed ritual for unbounded private violence or feud.
% TRANSFER_FUNCTION: Moves risk of death or injury onto the individual challengers and their families while the social capital of 'honor successfully defended' accrues to the aristocratic class and its self-image; seconds and arbiters extract social standing from administering the code.
% ABSENT_VOICES: State legal authorities object throughout but enforce only sporadically for most of the period; the participants' own families, who bear the grief regardless of outcome, are rarely consulted before a challenge is issued and have no formal voice in whether it proceeds.
% DISAPPEARANCE_RATIONALE: By the fringe-status endpoint of the decline, most social and legal arrangements no longer depend on the mechanism functioning — courts, dueling laws, and insurance-adjacent honor codes have already substituted for it in most disputes. But for the small residual population where it still operates, its sudden disappearance would remove the only socially legible script available to them, which is why historians dispute whether the late-stage mechanism is still load-bearing or merely vestigial.
% FOUNDING_PROBLEM: In a status order without reliable third-party enforcement of personal honor, insult and its public airing threatened a gentleman's entire social standing; dueling offered a self-administered, honor-preserving remedy that avoided both feud escalation and dependence on courts seen as beneath the aristocratic class's dignity.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and court records external to the honor class attest that by the mid-to-late decline period, functioning civil and criminal remedies for defamation and assault were widely available and increasingly used by the same social strata that had once dueled — corroborating that the founding problem (absence of adequate third-party remedy) was substantially solved well before dueling's final fringe-status collapse, which participants and seconds themselves rarely acknowledged, continuing to frame each individual instance as still-necessary.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness falls gradually across the interval (0.45 to 0.28) tracking declining incident frequency and the corresponding shrinkage of the population actually bearing the mechanism's risk. Theater ratio rises steadily (0.20 to 0.55) because as actual dueling incidents become rarer, an increasing share of the surrounding activity — codes, seconds, honor rhetoric — persists as performance and reputational signaling rather than genuine risk-bearing practice; this is exactly the piton signature (a residual structure maintained by inertia and theatrical invocation after its core function has thinned). Suppression (enforcement pressure against dueling) rises modestly early and then plateaus, reflecting inconsistent, non-escalating state opposition rather than a decisive enforcement crackdown — consistent with gradual decline rather than abrupt category foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic honor class and the seconds/arbiters who administer the code are beneficiaries — they extract social standing and status-policing function from the mechanism's continued (if declining) availability, at low personal risk in most instances. The individual participants and their families are the payers — for whichever fraction of insults still escalate to combat, they bear the lethal risk personally and immediately, and this risk does not decline for them individually even as its population-level frequency falls. This is the core seat divergence: at the aggregate/class level the constraint looks like a fading residue; at the level of the individual man who is actually challenged in 1870, the mechanism is exactly as binding and dangerous as it would have been in 1780.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absence of a dignified, third-party dispute remedy) was substantially solved well before the practice's final fringe collapse, per legal-historical record external to the honor class itself. Classifying this as a piton rather than a still-functioning rope prevents mistaking the late-stage performative invocation of honor codes for genuine ongoing coordination — the mechanism's late-period persistence is inertial theater layered over a residual core, not evidence that the coordination problem it solves is still live for the class as a whole. The status is 'dead' founding problem with persisting (declining) apparatus, which is the textbook piton signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_vs_contraction_boundary,
    'Is the observed frequency decline evidence of gradual attrition (this reading) or the tail end of a sharper cognitive category collapse that had already occurred earlier and is merely being measured here as a lagging incident count?',
    'Compare incident-frequency curves against contemporaneous discourse analysis: if elite writers stop being able to conceive of dueling as a live option well before incident counts reach zero, that supports contraction_reading; if incident counts fall smoothly while dueling remains discursively available as a threat or option throughout, that supports this decline_reading.',
    'If contraction is the better-supported structural account, this story''s claimed_type and epsilon trajectory would need to be re-derived as measuring a lagging artifact of an already-completed category shift rather than an independent decline process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_vs_contraction_boundary, conceptual, 'Whether measured frequency decline is a genuine gradual process or a lagging signature of prior category collapse.').

omega_variable(
    single_vs_composite_mechanism,
    'Did a single unified honor-satisfaction mechanism decline smoothly, or did distinct sub-mechanisms (aristocratic honor code, bourgeois respectability norms, state legal deterrence, insurance-like reputational substitutes) each decline or emerge on independent schedules that only look like one smooth curve in aggregate?',
    'Disaggregate incident data by social class, region, and stated cause of resolution (apology, arbitration, legal remedy, duel) across the interval; a single smooth mechanism should show homogeneous decline across strata, while a composite mechanism should show staggered, class-differentiated transition points.',
    'If the composite reading is empirically better supported, the decline observed here is a statistical artifact of aggregating several structurally distinct constraints, and this story''s single-epsilon-trajectory framing would be invalid for the underlying reality even though it remains valid as one authored reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_vs_composite_mechanism, empirical, 'Whether the smooth decline curve masks multiple independently-evolving sub-mechanisms.').

omega_variable(
    class_beneficiary_vs_natural_fade,
    'Does the aristocratic honor class actively benefit from the mechanism''s continued (declining) availability as a status-policing tool, or is its persistence better explained as simple cultural lag with no identifiable beneficiary actively sustaining it?',
    'Examine whether aristocratic institutions (clubs, codes of conduct, social sanction for refusing a challenge) actively penalize non-participation during the decline period, versus whether the mechanism simply goes unused without anyone enforcing its continuation.',
    'If no active beneficiary enforcement is found, the piton classification strengthens (pure inertia, no capturer) and gain_flow should be reconsidered toward diffuse; if active enforcement by the honor class is found, the constraint drifts toward tangled_rope territory instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_beneficiary_vs_natural_fade, empirical, 'Whether the aristocratic class actively sustains the declining mechanism or merely fails to actively dismantle it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1780, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1780, 0.2).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(hono_tr_t1820, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1820, 0.35).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1840, 0.42).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1860, 0.48).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1880, 0.52).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1780, 0.45).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(hono_be_t1820, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1820, 0.36).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1840, 0.32).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1860, 0.3).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1880, 0.29).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1780, 0.5).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(hono_su_t1820, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1820, 0.38).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1840, 0.36).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1860, 0.35).
narrative_ontology:measurement(hono_su_t1880, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the honor_satisfaction_mechanism kernel, decomposed per the ε-invariance principle rather than authored as a single constraint with an ambiguous decline/contraction/composite parameter. decline_reading models smooth frequency attrition with epsilon falling gradually and continuously; contraction_reading (sibling) models the same historical episode as a discrete cognitive category collapse; composite_reading (sibling) models it as several independently-operating mechanisms whose aggregate looks like decline but is structurally plural. All three are linked here so contamination/coupling analysis can trace how evidence bearing on one reading's plausibility propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
