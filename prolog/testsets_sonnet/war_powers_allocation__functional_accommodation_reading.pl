% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Functional Accommodation (Context-Dependent Authority Reading)
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This story instantiates the functional accommodation reading of the war
 *   powers kernel: authority allocation between Congress and the president is
 *   treated as a sliding scale keyed to operational context rather than a
 *   fixed categorical rule — imminent threats permit unilateral executive
 *   action, while prolonged campaigns are held to require congressional
 *   authorization. Unlike the congressional_primacy_reading (which treats any
 *   use of force beyond immediate self-defense as requiring prior
 *   authorization as a constitutional floor) and the
 *   inherent_executive_reading (which treats the commander-in-chief clause as
 *   granting standing authority to deploy force without prior authorization
 *   at all), this reading's defining feature is that the APPLICABLE RULE
 *   ITSELF is contingent on a characterization the executive controls. The
 *   ambiguity zone between 'imminent' and 'prolonged,' and between 'defense'
 *   and 'campaign,' is not incidental noise in this reading — it is the
 *   reading's central structural feature, and it is precisely what the WPR's
 *   60/90-day reporting clock and its history of non-invocation demonstrates
 *   in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.52).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Functional Accommodation (Context-Dependent Authority Reading)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '02119a08-5000-4227-a977-1edb4e48a615').
narrative_ontology:cs_kernel_codification('02119a08-5000-4227-a977-1edb4e48a615', distributed).
narrative_ontology:cs_authority_grounding('02119a08-5000-4227-a977-1edb4e48a615', distributed).
narrative_ontology:cs_reading_relation('02119a08-5000-4227-a977-1edb4e48a615', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('02119a08-5000-4227-a977-1edb4e48a615', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('02119a08-5000-4227-a977-1edb4e48a615', foundational, authority_allocation_tracks_operational_context).
narrative_ontology:cs_axiom_status(authority_allocation_tracks_operational_context, holdable).
narrative_ontology:cs_axiom_grounding('02119a08-5000-4227-a977-1edb4e48a615', authority_allocation_tracks_operational_context, instrumental).
narrative_ontology:cs_axiom('02119a08-5000-4227-a977-1edb4e48a615', secondary, temporal_scope_thresholds_are_administrable).
narrative_ontology:cs_axiom_status(temporal_scope_thresholds_are_administrable, holdable).
narrative_ontology:cs_axiom_grounding('02119a08-5000-4227-a977-1edb4e48a615', temporal_scope_thresholds_are_administrable, empirically_contingent).
narrative_ontology:cs_reference_frame('02119a08-5000-4227-a977-1edb4e48a615', contextual_functional_balance).
narrative_ontology:cs_drift_state('02119a08-5000-4227-a977-1edb4e48a615', post_war_on_terror_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('02119a08-5000-4227-a977-1edb4e48a615', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_apparatus).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_accountability_interest).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, deployed_military_personnel).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, functional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, operational_context_dependent_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines, in the moment, whether a given operation counts as an 'imminent threat' response (unilateral) or a 'prolonged campaign' (requiring authorization). Controls the classification that determines which rule applies, and has strong incentive to characterize actions as falling in the unilateral category for as long as possible. Faces no external check on the initial characterization; congressional pushback typically arrives only after facts on the ground are established.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Holds the constitutional authorization power in principle but must contest the executive's operational characterization after deployment has already occurred, when withdrawal or defunding carries visible political and strategic costs. The ambiguity zone between 'imminent' and 'prolonged' is precisely the terrain on which its formal authority is eroded without ever being formally denied.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress_as_institution, payer,
    institutional, biographical, constrained, national).

% Operates within whichever characterization the executive settles on, benefiting from the operational flexibility the ambiguity affords — can plan and execute without waiting on legislative timelines, and can frame mission scope incrementally to stay inside the 'imminent threat' window as long as politically useful.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the direct physical risk of whatever operation results from the authority contest, regardless of which branch's characterization ultimately prevails or how long the legal ambiguity persists. Have no voice in the classification dispute that determines the legal basis, duration, or oversight of the mission they are executing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, deployed_military_personnel, payer,
    powerless, immediate, trapped, regional).

% The diffuse public interest in knowing, in real time, which branch is accountable for a military action and under what legal authority is degraded whenever operational characterization substitutes for a clear categorical rule; there is no organized agent representing this interest directly, only its erosion.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_accountability_interest, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(war_powers_allocation__functional_accommodation_reading, public_accountability_interest).

% Consistently declines to adjudicate war powers disputes between the political branches under the political question doctrine, leaving the ambiguity zone without a neutral arbiter who could resolve contested characterizations. Their absence from the dispute is structural, not incidental — it is what allows the ambiguity to persist unresolved.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_judiciary, excluded,
    institutional, generational, analytical, national).

% Study the pattern of executive characterization choices across historical conflicts (Korea, Vietnam, Kosovo, Libya, Syria) and document how the functional/contextual framing has, in practice, tracked executive convenience more consistently than any principled temporal or scope threshold.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides operational flexibility for the executive to respond to fast-moving military situations without waiting on a legislative process too slow for time-sensitive threats, while nominally preserving Congress's role once an engagement's scope and duration become clear.
% TRANSFER_FUNCTION: Moves the practical locus of war-initiation authority from Congress toward the executive whenever a characterization dispute exists, by keeping the applicable rule contingent on facts the executive controls and characterizes first.
% ABSENT_VOICES: The federal judiciary is structurally excluded via the political question doctrine and never adjudicates the contested characterization; deployed personnel and the diffuse public accountability interest have no seat in the classification dispute despite bearing its costs most directly.
% DISAPPEARANCE_RATIONALE: The executive and national security apparatus would treat disappearance of the contextual accommodation as a return to paralyzing categorical rules requiring authorization for any deployment, damaging response speed to genuine emergencies. Congress and accountability advocates would treat its disappearance as a return to a clear bright-line rule restoring meaningful legislative check. The parties dispute not just the value of the arrangement but what would fill its absence.
% FOUNDING_PROBLEM: The 1973 War Powers Resolution attempted to reconcile constitutional text assigning the declaration of war to Congress with the operational reality that modern military threats sometimes require response faster than legislative deliberation allows.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch legal counsel across administrations of both parties attest the functional accommodation remains necessary given the pace of modern threats. Independent scholarship (Congressional Research Service reports, cross-administration case studies by war-powers scholars outside the executive) documents that the contextual test has been invoked to authorize unilateral action in situations — Libya 2011, Syria strikes, sustained drone campaigns — that plausibly fall outside any imminent-threat window, suggesting the founding problem has been used to justify a scope well beyond it.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, contested).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that the accommodation is genuinely used both ways historically, but the historical record (Korea, Vietnam, Grenada, Panama, Kosovo, Libya, Syria, sustained drone campaigns) shows the characterization choice consistently resolving toward whichever result the executive prefers, with no neutral party enforcing the temporal/scope line. Suppression (0.58) is a raw structural fact: the political question doctrine actively suppresses judicial resolution of the categorical dispute, which is what allows the ambiguity to persist rather than being tested and settled. Theater ratio (0.46) captures that reporting requirements and consultation rituals under the WPR are performed even when the underlying characterization dispute is never substantively contested by Congress — a rising theater ratio over the interval tracks the routinization of notification-without-authorization-fight as normal practice. Accessibility collapse is moderate (0.4) rather than high because Congress does formally retain the power of the purse and could, in principle, force the issue — the alternative is not foreclosed, merely costly to exercise. Resistance (0.55) reflects periodic congressional pushback (War Powers Resolution passage itself, later invocations during Iraq/Syria debates) that never fully resolves the underlying ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's seat, the functional accommodation is a rope: genuine coordination solving a real problem (response speed vs. deliberative process) with reasonable flexibility. From Congress's seat and from the accountability-interest seat, the same structure computes closer to tangled rope or worse: a coordination story (fast response to genuine emergencies) providing cover for a structural transfer of practical war-initiation authority that Congress cannot recover except at high political cost. The engine's per-seat computation is expected to diverge along exactly this line — that divergence is the substantive content of authoring this as its own reading rather than folding it into either sibling.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch sits at the beneficiary end: it sets the initial characterization, bears low cost from the ambiguity, and benefits from operational flexibility. Congress sits toward the target end structurally: its formal constitutional authority is real but exercised only reactively, after the executive's characterization has already shaped facts on the ground. Deployed personnel and the diffuse public accountability interest are the clearest targets — trapped exit options, no voice in the classification dispute, and they bear the downstream cost (physical risk, degraded oversight) regardless of who wins the characterization contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (matching legislative process speed to modern threat tempo) remains partially live — some threats genuinely require faster response than any legislative authorization process could provide. But the accommodation's actual operation has drifted well past that founding problem: sustained multi-year campaigns and repeated strikes have been characterized as falling within 'imminent threat' latitude, which the founding problem does not justify. This is not full mandatrophy (the founding problem is not simply dead) but a live case of scope creep riding on an unresolved founding justification — the founding_problem_status is authored as contested rather than dead precisely because genuine emergency cases exist alongside documented overreach cases within the same doctrinal category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    characterization_authority_asymmetry,
    'Is the fact that the executive branch controls the initial operational characterization (imminent vs. prolonged) itself a neutral feature of institutional competence (the executive has better real-time information) or a structural design flaw that guarantees systematic authority drift toward the executive regardless of the doctrine''s stated symmetry?',
    'Comparative historical analysis of characterization outcomes across administrations of both parties and across genuinely varied threat types — if the executive''s characterization tracks objective threat features (imminence, defensive posture) rather than political convenience, the asymmetry is functional; if it tracks convenience, it is structural extraction.',
    'If structural, this reading is closer to a tangled rope with a built-in ratchet toward inherent_executive_reading in practice regardless of its formal text; if functional, the accommodation is closer to a genuine rope solving an information-asymmetry coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(characterization_authority_asymmetry, empirical, 'Whether executive control of the imminent/prolonged characterization is neutral competence allocation or structural extraction.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the functional accommodation reading itself a stable, independently-motivated constitutional position, or is it the position both branches converge on precisely because it avoids forcing a resolution between the congressional_primacy_reading and the inherent_executive_reading — i.e., is ambiguity the point, chosen because a clear ruling in either direction would be costly to whichever branch loses?',
    'Examine whether either branch has ever proposed replacing the functional test with a bright-line rule when doing so would clearly favor its own institutional interest, and whether such proposals were abandoned once the branch anticipated losing the resulting fixed rule.',
    'If the ambiguity is itself the negotiated equilibrium (rather than a genuine attempt at a principled middle position), the functional_accommodation_reading is best understood as a modus vivendi that both branches maintain because neither wants the risk of the other reading being formally adopted — which would reclassify this constraint''s coordination function as substantially thinner than the stated founding problem suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the contextual/functional test is a genuine constitutional position or a mutually convenient ambiguity-preservation equilibrium between the two sibling readings.').

omega_variable(
    judicial_abstention_as_structural_choice,
    'Does the federal judiciary''s political-question abstention from war powers disputes reflect genuine institutional incompetence to resolve such questions, or a structural choice that itself benefits the status quo distribution of authority by removing the one actor capable of forcing a categorical resolution?',
    'Compare judicial willingness to adjudicate analogous separation-of-powers disputes (e.g., appointments, impoundment) where a clear precedent-setting ruling was rendered, against the consistent abstention pattern specific to war powers.',
    'If abstention is itself a structural choice rather than genuine incapacity, the exclusion of the judiciary from this dispute is not a neutral gap but part of what sustains the ambiguity zone this reading depends on — raising the suppression component of this constraint beyond what is currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_abstention_as_structural_choice, conceptual, 'Whether judicial abstention under the political question doctrine is genuine incapacity or a structural choice sustaining the ambiguity zone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 1973, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement(war__tr_t1983, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1983, 0.3).
narrative_ontology:measurement(war__tr_t1993, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1993, 0.34).
narrative_ontology:measurement(war__tr_t2003, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(war__tr_t2013, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2024, 0.46).

% Extraction over time
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1973, 0.34).
narrative_ontology:measurement(war__be_t1983, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1983, 0.4).
narrative_ontology:measurement(war__be_t1993, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(war__be_t2003, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2003, 0.46).
narrative_ontology:measurement(war__be_t2013, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2013, 0.49).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(war__su_t1983, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1983, 0.45).
narrative_ontology:measurement(war__su_t1993, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1993, 0.48).
narrative_ontology:measurement(war__su_t2003, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(war__su_t2013, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'war powers allocation' concept per the ε-invariance principle: congressional_primacy_reading (categorical, authorization-required floor), functional_accommodation_reading (this story — context-dependent sliding scale), and inherent_executive_reading (categorical, standing executive authority). Each has a distinct ε and distinct beneficiary/victim structure because each reading licenses a structurally different allocation of practical authority. This story's ε (0.52) sits between the two categorical siblings by design — it is the reading whose defining feature is that the applicable rule is itself contingent, which is a different structural claim from either bright-line position, not an average of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
