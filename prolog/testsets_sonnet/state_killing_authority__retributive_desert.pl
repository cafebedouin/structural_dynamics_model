% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive Desert Reading of State Killing Authority
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates ONLY the retributive-desert reading of the
 *   state-killing-authority kernel: murderers are held to forfeit their right
 *   to life as a direct consequence of the act, and proportional punishment
 *   (lex talionis) is presented as requiring death for death, independent of
 *   whether execution deters future murders. The deterrence_instrument
 *   reading (justification contingent on empirical prevention of future harm)
 *   and the categorical_abolition reading (state killing is inherently
 *   impermissible regardless of crime) are sibling constraints with their own
 *   ε, their own stakeholders, and their own classification — they are not
 *   blended into this file. This reading's structural delta from the kernel:
 *   the murdered person enters the beneficiary set posthumously via a
 *   vindication claim (not as an acting agent — modeled here as a non-agent
 *   beneficiary), the condemned person exits the ordinary rights-holder set
 *   via the forfeiture doctrine (modeled as exit_options: trapped and role:
 *   payer rather than as a beneficiary of any residual protection), and the
 *   state's authority is grounded in a proportionality norm rather than in
 *   any claim about outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.42).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.68).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive Desert Reading of State Killing Authority").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '05969d7d-3e5c-45bd-8759-4fec397e4045').
narrative_ontology:cs_kernel_codification('05969d7d-3e5c-45bd-8759-4fec397e4045', distributed).
narrative_ontology:cs_authority_grounding('05969d7d-3e5c-45bd-8759-4fec397e4045', distributed).
narrative_ontology:cs_reading_relation('05969d7d-3e5c-45bd-8759-4fec397e4045', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('05969d7d-3e5c-45bd-8759-4fec397e4045', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('05969d7d-3e5c-45bd-8759-4fec397e4045', foundational, murder_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murder_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('05969d7d-3e5c-45bd-8759-4fec397e4045', murder_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('05969d7d-3e5c-45bd-8759-4fec397e4045', foundational, proportionality_requires_equivalent_punishment).
narrative_ontology:cs_axiom_status(proportionality_requires_equivalent_punishment, holdable).
narrative_ontology:cs_axiom_grounding('05969d7d-3e5c-45bd-8759-4fec397e4045', proportionality_requires_equivalent_punishment, deontological).
narrative_ontology:cs_axiom('05969d7d-3e5c-45bd-8759-4fec397e4045', secondary, justification_independent_of_deterrent_effect).
narrative_ontology:cs_axiom_status(justification_independent_of_deterrent_effect, holdable).
narrative_ontology:cs_axiom_grounding('05969d7d-3e5c-45bd-8759-4fec397e4045', justification_independent_of_deterrent_effect, deontological).
narrative_ontology:cs_reference_frame('05969d7d-3e5c-45bd-8759-4fec397e4045', lex_talionis_proportional_restraint).
narrative_ontology:cs_drift_state('05969d7d-3e5c-45bd-8759-4fec397e4045', contemporary_wrongful_conviction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05969d7d-3e5c-45bd-8759-4fec397e4045', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumous_desert).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_surviving_family).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_moral_order_claimants).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, wrongfully_convicted_persons).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, capital_defense_indigent_class).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportional_desert_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, moral_equality_of_persons_via_equivalent_punishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital statutes, prosecutes capital cases, and carries out executions. Justifies the practice as restoring moral balance disturbed by murder — the murderer's forfeiture of the right to life is treated as following necessarily from the act itself, not from any calculation of future benefit. Controls charging decisions, clemency review, and execution protocol.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_capital_punishment_authority, agenda_setter,
    institutional, generational, analytical, national).

% Face execution as the proportional consequence of a capital conviction. Under this reading their claim to continued life is treated as forfeited by the act of murder itself, prior to and independent of any question of deterrent effect. Appeals and clemency petitions are the only available exit, and both are structurally narrow once conviction and forfeiture are established under the retributive frame.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, national).

% Cannot act, but the retributive reading treats the murdered person's desert claim as vindicated by the equivalence of punishment to harm — 'death for death' is presented as restoring a moral equation the murder disturbed. This is a non-agent beneficiary: the vindication accrues to a claim about the victim, not to an actor who collects anything.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumous_desert, beneficiary,
    powerless, immediate, analytical, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims_posthumous_desert).

% Frequently invoked as the concrete beneficiaries of proportional punishment — the execution is presented as delivering the closure or moral satisfaction the retributive frame promises. Their actual experience of the outcome varies widely and does not always match the framing used to justify the sentence on their behalf.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_surviving_family, beneficiary,
    moderate, biographical, constrained, local).

% Bear the irreversible cost of the desert-forfeiture logic when the underlying factual premise — that this person committed the murder — is wrong. Because the retributive framework treats the forfeiture as automatic upon the finding of guilt, there is no proportionality correction available once the sentence is carried out; exoneration after execution corrects nothing.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, wrongfully_convicted_persons, payer,
    powerless, immediate, trapped, national).

% Disproportionately represented among the condemned due to resource asymmetries in capital defense. The retributive framework's claim that forfeiture follows from the act, not from process quality, obscures how unevenly the finding of the triggering act is actually established across defendants of different means.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, capital_defense_indigent_class, payer,
    powerless, biographical, trapped, national).

% The doctrine of proportional desert itself is vindicated each time the sentence is carried out — a non-agent beneficiary representing the retributive tradition's claim to correctly describe justice, distinct from any actor who collects material benefit.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_moral_order_claimants, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, retributive_moral_order_claimants).

% Argues the forfeiture premise is itself the thing in dispute — that no act renders a right to life forfeit — and is treated within the retributive framework not as a party to be persuaded on the merits of desert but as holding a different, foreclosed premise. Their objection to the forfeiture claim is heard in public debate but does not enter the retributive framework's own adjudication once guilt is established.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_advocacy_coalition, excluded,
    organized, generational, constrained, national).

% Review capital sentences for procedural and constitutional defects and can grant clemency, but generally do not revisit the underlying proportionality premise that murder forfeits the right to life — their review operates inside the retributive frame rather than against it.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, appellate_and_clemency_review_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, publicly legible standard of proportional response to the gravest crime, intended to signal that the value of a life taken is treated as commensurate only with the life of the person who took it — a coordination function around what counts as an adequate societal response to murder.
% TRANSFER_FUNCTION: Moves the condemned person's continued life (and, in the surrounding apparatus, years of appellate and defense resources) into the satisfaction of a proportionality claim asserted on behalf of the victim and the moral order the doctrine claims to vindicate. Nothing material returns to the victim; the transfer is symbolic-proportional, not restorative.
% ABSENT_VOICES: The abolitionist coalition disputes the forfeiture premise itself and is present in public debate but structurally outside the retributive framework's own adjudicative process, which treats forfeiture as settled once guilt is found. Wrongfully convicted persons, definitionally, cannot be heard after execution.
% DISAPPEARANCE_RATIONALE: If the retributive-desert justification for capital punishment were withdrawn, capital sentencing regimes that rely on it would lose their primary doctrinal ground (distinct from deterrence-based justification, which survives independently on separate empirical claims) — legislatures and courts would need to re-ground capital statutes in deterrence or abandon them, and pending forfeiture-premised sentences would require re-justification.
% FOUNDING_PROBLEM: Pre-modern and early-modern retributive theory sought a principled limit on punishment — proportionality (lex talionis) was framed as a restraint on arbitrary or excessive vengeance, not merely as license for it: punishment should not exceed the harm done.
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists within the tradition (Kant's heirs, some contemporary desert theorists) attest the proportionality-as-restraint problem remains live and philosophically load-bearing. Wrongful-conviction exoneration data, compiled by innocence-project researchers and legal scholars outside the retributivist tradition, corroborates that the forfeiture premise as actually administered functions less as a restraint on excess and more as an irreversible sorting mechanism correlated with defense-resource asymmetry rather than culpability alone — an attestation from outside the beneficiary set that the founding restraint function has been substantially displaced by an administrative sorting function.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).
:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme: the doctrine does deliver a genuine, if contested, coordination function — a publicly legible standard limiting punishment to proportionality rather than open-ended vengeance — but it also extracts irreversibly from a class (condemned persons generally, and wrongfully convicted persons specifically) whose situation the doctrine treats as settled once guilt is found. Suppression is substantial (0.68) because the forfeiture premise forecloses further proportionality argument once conviction occurs — there is no mechanism within the retributive frame itself for revisiting the forfeiture judgment, only appellate review of process. Theater ratio is moderate and rising (0.18→0.31) reflecting that a growing share of the apparatus (extended appellate process, clemency ritual) increasingly performs the appearance of careful proportionality assessment without altering outcomes once the forfeiture premise is triggered. Accessibility collapse is moderate (0.40) — legislative and judicial alternatives to the retributive framing (deterrence-only justification, abolition) remain live and contested in the political process, unlike a genuine natural-law collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   The state capital punishment authority is the agenda_setter administering the doctrine but is not itself a material beneficiary in the ordinary sense — no rent is collected — so it sits at moderate directionality rather than at the extreme beneficiary end; its interest is doctrinal/institutional rather than extractive. Condemned persons and wrongfully convicted persons sit at the full-target end: trapped exit options and irreversible consequence. Victims' surviving families are declared beneficiaries per the doctrine's own framing, but the situation text notes their actual experience diverges from the framing used to justify sentencing on their behalf — this gap is intentional and left for the engine's directionality computation rather than resolved in prose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was proportionality-as-restraint on arbitrary vengeance; the founding_problem_status is contested precisely because the mechanism that was meant to LIMIT punishment (do not exceed the harm) has, per wrongful-conviction data corroborated outside the retributivist tradition, become instead a sorting mechanism correlated with defense-resource asymmetry. This is the mismatch the six-questions R5 interview is built to surface: founding_problem_status=contested plus disappearance_verdict=world_rearranges signals a live capture/drift question rather than a settled zombie-mandate finding — it does not by itself resolve whether the doctrine still serves its founding restraint function or has substituted an administrative sorting function for it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_premise_naturalness,
    'Is the claim that a murderer forfeits the right to life a discoverable moral fact (making the retributive reading closer to a mountain-like natural-law claim) or a constructed doctrinal choice that benefits identifiable institutional and doctrinal interests (the state authority, the retributive tradition itself)?',
    'No empirical resolution mechanism exists for a purely deontological claim; the question can only be narrowed by tracking whether the doctrine''s own tradition (via axiom status changes, e.g. moving from holdable to overridden) revises the forfeiture premise in light of wrongful-conviction evidence, which is an empirical input to a conceptual dispute.',
    'If treated as constructed rather than discovered, the beneficiary declarations (retributive_moral_order_claimants) become the operative structural fact and the classification leans further toward tangled_rope; if treated as a genuine moral discovery, the classification pressure shifts toward a more mountain-like reading, though the schema''s mountain gate would still require emerges_naturally and this story does not claim that.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forfeiture_premise_naturalness, conceptual, 'Whether forfeiture-of-life-upon-murder is a discovered moral fact or a constructed, interest-serving doctrine.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the retributive_desert reading''s premise diverge from the deterrence_instrument reading''s premise, given that both readings can justify the identical practice (execution) from different grounds — is the boundary purely in the justificatory structure, or does it also produce different sentencing outcomes in practice (e.g., cases where deterrence data would counsel against execution but desert-forfeiture would still require it)?',
    'Comparative analysis of jurisdictions/eras where the two justificatory logics diverge in practice — e.g., capital sentencing in cases with clear deterrence-nullifying facts (elderly, terminally ill, or already-incapacitated offenders) tests whether retributive desert logic alone still demands execution where deterrence logic would not.',
    'If the two readings produce identical practice in all cases, the kernel decomposition into separate stories is justificatory-only; if they diverge in practice, the decomposition also tracks a real difference in who is targeted and when, strengthening the case for treating them as genuinely separate constraints with separate victim sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the retributive and deterrence readings diverge only in justification or also in practical sentencing outcomes.').

omega_variable(
    posthumous_beneficiary_status,
    'Can a deceased person coherently be modeled as a beneficiary of a doctrine''s operation, or does ''posthumous vindication'' collapse into a claim about the living (surviving family, the moral order, the state) wearing the victim''s name?',
    'Philosophical analysis of posthumous interests/harms literature; alternatively, survey data on whether surviving families report the vindication framing as matching their actual experience of the outcome.',
    'If posthumous vindication collapses into a claim about living parties, the non-agent beneficiary entries (murder_victims_posthumous_desert, retributive_moral_order_claimants) should be understood as proxies for the state authority''s and tradition''s own interest, sharpening the reading toward tangled_rope; if posthumous interests are coherent in their own right, the current non-agent modeling stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posthumous_beneficiary_status, conceptual, 'Whether posthumous vindication is a coherent independent beneficiary claim or a proxy for living parties'' interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__retributive_desert, theater_ratio, 8, 0.21).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__retributive_desert, theater_ratio, 16, 0.24).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__retributive_desert, theater_ratio, 24, 0.27).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__retributive_desert, theater_ratio, 32, 0.29).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__retributive_desert, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__retributive_desert, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__retributive_desert, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__retributive_desert, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__retributive_desert, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__retributive_desert, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__retributive_desert, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__retributive_desert, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__retributive_desert, 0.1).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the state_killing_authority kernel. retributive_desert (this file) grounds authority in proportional forfeiture independent of outcome; deterrence_instrument grounds authority in empirically contingent future-harm prevention; categorical_abolition denies the authority exists under any grounding. Each reading has its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
