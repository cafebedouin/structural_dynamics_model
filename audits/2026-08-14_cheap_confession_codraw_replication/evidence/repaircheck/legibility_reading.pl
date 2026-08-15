% ============================================================================
% CONSTRAINT STORY: legibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legibility_reading, []).

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
 *   constraint_id: legibility_reading
 *   human_readable: Confession Legibility Mechanism (Epistemic Reading)
 *   domain: epistemology/discourse_norms
 *
 * SUMMARY:
 *   The mechanism under analysis converts private silence into public,
 *   interpretable evidence about confession. This reading takes the essay's
 *   thesis at face value and non-deflationarily: the mechanism secures
 *   VISIBILITY of a cost, not payment of it. Confession remains exactly as
 *   costly to the confessing agent as it always was; what changes is purely
 *   epistemic — an outside observer's inferential position with respect to
 *   non-confession. This story is one of three readings of the
 *   commitment_cost_location kernel. The sibling readings
 *   (enforcement_deflation_reading, temporal_identity_reading) treat the
 *   mechanism as, respectively, quietly reducing real enforcement pressure
 *   behind a legibility facade, or restructuring the confessor's
 *   identity/commitments over time. This reading explicitly rejects both of
 *   those framings: extractiveness is held flat and low across the interval
 *   because nothing about the underlying cost structure moves — only
 *   theater_ratio drifts slightly upward, representing a secondary risk that
 *   the visibility mechanism itself accumulates some performative
 *   interpretive apparatus (commentary, meta-commentary on what silence
 *   'really' means) without touching the base fact that confession costs are
 *   invariant.
 *
 * KEY AGENTS:
 *   - confessing_agent: bears the unchanged cost of confession (moderate/constrained) — the mechanism does not touch their burden
 *   - outside_observers: gain inferential power at zero cost — the primary beneficiary of the legibility conversion
 *   - epistemic_community: organized beneficiary that accumulates durable evidentiary resources over time
 *   - silent_non_confessors: excluded voice whose silence is now read as signal, whether or not that reading is warranted
 *   - philosophical_observer: analytical seat holding the reading distinction against the sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legibility_reading, 0.28).
domain_priors:suppression_score(legibility_reading, 0.35).
domain_priors:theater_ratio(legibility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legibility_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legibility_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(legibility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legibility_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legibility_reading, rope).
narrative_ontology:human_readable(legibility_reading, "Confession Legibility Mechanism (Epistemic Reading)").
narrative_ontology:topic_domain(legibility_reading, "epistemology/discourse_norms").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legibility_reading, '70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d').
narrative_ontology:cs_kernel_codification('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', distributed).
narrative_ontology:cs_authority_grounding('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', distributed).
narrative_ontology:cs_reading_relation('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', legibility_reading__enforcement_deflation_reading, coexists_with).
narrative_ontology:cs_reading_relation('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', legibility_reading__temporal_identity_reading, influences).
narrative_ontology:cs_axiom('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', foundational, confession_cost_invariance).
narrative_ontology:cs_axiom_status(confession_cost_invariance, holdable).
narrative_ontology:cs_axiom_grounding('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', confession_cost_invariance, empirically_contingent).
narrative_ontology:cs_axiom('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', secondary, silence_as_pure_inferential_signal).
narrative_ontology:cs_axiom_status(silence_as_pure_inferential_signal, holdable).
narrative_ontology:cs_axiom_grounding('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', silence_as_pure_inferential_signal, conventional).
narrative_ontology:cs_reference_frame('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', confession_as_cost_invariant_private_act).
narrative_ontology:cs_drift_state('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', post_legibility_mechanism_introduction, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('70ae53ab-4d20-42d6-acd7-c5d1b03c2f2d', '').
narrative_ontology:cs_kernel_id(legibility_reading, commitment_cost_location).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legibility_reading, outside_observers).
narrative_ontology:constraint_beneficiary(legibility_reading, epistemic_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legibility_reading, confessing_agent).
narrative_ontology:constraint_vindicates(legibility_reading, confession_cost_invariance_thesis).
narrative_ontology:constraint_vindicates(legibility_reading, silence_as_inferential_signal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears exactly the same private cost of confession as before the mechanism existed — the act of disclosure is neither cheaper nor more expensive. What has changed is that their choice to confess or stay silent is now legible to others; silence itself has become informative rather than opaque. They cannot make their non-confession unreadable without exiting the discourse entirely.
narrative_ontology:constraint_stakeholder(legibility_reading, confessing_agent, payer,
    moderate, immediate, constrained, local).

% Previously could not distinguish 'nothing to confess' from 'chose not to confess' — both looked like silence. Now they can update on non-confession as evidence, because the mechanism has converted private silence into public, interpretable signal. They pay nothing for this and gain inferential power at no cost to the confessor's actual burden.
narrative_ontology:constraint_stakeholder(legibility_reading, outside_observers, beneficiary,
    moderate, immediate, mobile, regional).

% The broader community of interpreters (critics, historians, downstream reasoners) gains a durable evidentiary resource: a record of who did and did not confess, which now carries evidentiary weight it previously lacked. This does not depend on anyone paying more; it depends only on visibility increasing.
narrative_ontology:constraint_stakeholder(legibility_reading, epistemic_community, beneficiary,
    organized, generational, mobile, regional).

% Agents who have nothing to confess or who decline to confess for unrelated reasons are now read as if their silence means something, even though the cost structure of confession has not changed for them. They would object that the mechanism treats their silence as signal when it may be noise, but this objection does not alter the actual cost of confessing — it only concerns how their non-confession is interpreted.
narrative_ontology:constraint_stakeholder(legibility_reading, silent_non_confessors, excluded,
    moderate, immediate, trapped, local).

% Analyzes the mechanism's actual achievement: it does not touch the cost of confession at all. It is purely an epistemic conversion — private silence becomes public evidence. The observer's task is to keep this reading distinct from readings that claim the mechanism deflates enforcement or restructures identity over time.
narrative_ontology:constraint_stakeholder(legibility_reading, philosophical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legibility_reading, diffuse).
narrative_ontology:fixing_cost_class(legibility_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine inference problem: without the mechanism, an observer cannot distinguish an agent who has nothing to confess from one who is silently withholding. The mechanism converts an otherwise indistinguishable private state into a publicly interpretable signal, letting the community update correctly on non-confession.
% TRANSFER_FUNCTION: Nothing material transfers. What moves is information: the mechanism relocates non-confession from the class of 'uninterpretable noise' to the class of 'interpretable evidence.' No cost, payment, or resource shifts from any party to any other.
% ABSENT_VOICES: Silent non-confessors would object that their silence is being read as signal when for many of them it reflects nothing evidentiary at all (they simply have nothing to say); they are structurally present but not consulted on how their silence gets interpreted.
% DISAPPEARANCE_RATIONALE: If the legibility mechanism vanished, the cost of confession would be completely unaffected — confessing agents would pay exactly what they always paid. What would change is that outside observers would lose the ability to distinguish meaningful silence from empty silence, degrading the community's inferential resources. Whether this counts as 'the world rearranges' or 'stays the same' is genuinely contested: confessors see no change, observers see a real loss.
% FOUNDING_PROBLEM: Observers historically could not tell whether an agent's silence reflected a genuine absence of wrongdoing/relevant fact or a deliberate, costly choice to withhold confession — both states were externally indistinguishable.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of language and discourse analysts outside the confessing population (who neither confess nor benefit materially from confessions) corroborate that the inferential gap between 'nothing to confess' and 'withheld confession' was and remains a live problem in evidentiary and testimonial contexts; this is not merely asserted by the beneficiary community of observers.
narrative_ontology:disappearance_verdict(legibility_reading, contested).
narrative_ontology:founding_problem_status(legibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(legibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legibility_reading, 0.28, 'claude-sonnet-5', 'omega_production_confession_kernel_20260814_211528', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_reading_tests).
:- end_tests(legibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low and flat (0.28, unchanging across the interval) because this reading's entire claim is that the mechanism does not extract anything additional from the confessing agent — it only changes what can be inferred from their behavior. Suppression is moderate (0.35) reflecting the residual pressure that comes from being legible at all (one cannot make silence unreadable once the mechanism exists), but this is a byproduct of visibility, not of a coercive cost increase. Theater ratio drifts gently upward (0.12 to 0.22) to represent accumulating interpretive/meta-commentary activity around what silence signifies, which is a secondary and modest phenomenon relative to the mechanism's core epistemic function.
 *
 * PERSPECTIVAL GAP:
 *   The confessing_agent's seat and the outside_observers' seat compute very differently under this reading: from the confessor's side, nothing has changed — same cost, same choice. From the observer's side, everything has changed — silence is now legible. The engine should show this as a genuine seat divergence without requiring either seat's experience to be wrong; both are correct readings of a purely informational shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (outside_observers, epistemic_community) are declared because they gain inferential capacity without bearing any of the confession's cost — pure epistemic subsidy, d near the beneficiary end. The confessing_agent is a payer only in the sense that they always were: the payment is unchanged, so directionality for that seat reflects the pre-existing cost, not a new extraction introduced by legibility. No victims are declared under this reading because the reading's thesis is precisely that nobody pays anything new — the beneficiary/victim structure of confession itself is unchanged, per the kernel's expected structural delta.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mandatrophy failure mode of treating increased legibility as if it were increased extraction: a naive read might see rising social pressure around confession and conclude the mechanism has become more coercive over time. This reading corrects that by holding extractiveness flat and attributing any apparent intensification to the theater_ratio (interpretive apparatus) rather than to the underlying cost structure, which is the epistemically honest place to locate the change per the essay's thesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legibility_vs_deflation_indistinguishability,
    'Can an observer empirically distinguish ''the mechanism only increased legibility, cost unchanged'' (this reading) from ''the mechanism increased legibility BECAUSE enforcement quietly weakened, making visibility a substitute for cost'' (the enforcement_deflation_reading)?',
    'Track enforcement outcomes independently of confession rates over the interval: if punishment/consequence severity for confessed violations stays constant while legibility increases, this reading is supported; if enforcement severity declines in step with legibility gains, the sibling reading is supported instead.',
    'If the two readings are empirically indistinguishable from the available evidence, that under-determination is itself the finding — the kernel''s contest is not resolvable by the confession record alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legibility_vs_deflation_indistinguishability, empirical, 'Whether legibility gains can be separated from a possible concurrent enforcement deflation.').

omega_variable(
    silence_signal_calibration,
    'Is silence-as-signal actually well-calibrated, or does the mechanism create a legibility structure that misreads genuinely uninformative silence (nothing to confess) as if it were withheld confession?',
    'Compare base rates: among agents who are silent, what fraction actually have something to confess versus nothing at all? If the mechanism cannot distinguish these populations, its inferential gain is illusory for a subset of cases.',
    'If miscalibrated, the mechanism''s claimed epistemic achievement overstates its actual inferential value, and the excluded silent_non_confessors'' objection becomes substantively correct rather than merely a grievance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(silence_signal_calibration, empirical, 'Whether the mechanism''s core inferential claim holds up against actual base rates of silence-with-nothing-to-confess.').

omega_variable(
    reading_choice_framing,
    'Is the legibility reading the correct primary framing, or does treating the mechanism''s function as ''purely epistemic'' itself presuppose that cost and visibility are cleanly separable — which the temporal_identity_reading denies (it holds that being made visible over time IS a cost, via identity restructuring)?',
    'This is a conceptual/framing question, not resolvable by new data alone; it depends on whether one treats reputational/identity effects of visibility as a distinct cost category or as downstream consequences of a cost-neutral epistemic event.',
    'If visibility itself constitutes a cost (per the temporal_identity_reading), then this reading''s claim of a completely cost-neutral epistemic conversion would need revision, though the two readings could still coexist as describing different aspects (informational vs. identity-constitutive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_framing, conceptual, 'Whether the epistemic/cost separation this reading relies on is itself contestable framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legibility_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legibility_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t4, legibility_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(legi_tr_t8, legibility_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(legi_tr_t12, legibility_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(legi_tr_t16, legibility_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(legi_tr_t20, legibility_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(legi_tr_t24, legibility_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legibility_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t4, legibility_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(legi_be_t8, legibility_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(legi_be_t12, legibility_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(legi_be_t16, legibility_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(legi_be_t20, legibility_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(legi_be_t24, legibility_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legibility_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legibility_reading, enforcement_deflation_reading).
narrative_ontology:affects_constraint(legibility_reading, temporal_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the commitment_cost_location kernel. legibility_reading holds ε flat and low, attributing the mechanism's entire effect to observer-side inference gain rather than any change in confessor-side cost. enforcement_deflation_reading (sibling) authors a different ε trajectory reflecting a claim that legibility substitutes for weakening enforcement. temporal_identity_reading (sibling) authors extraction through an identity-restructuring lens rather than a static cost lens. All three share the same underlying phenomenon (a confession legibility mechanism) but are structurally distinct constraints per the ε-invariance principle, since each assigns a different mechanism and a different extraction profile to the same surface phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
