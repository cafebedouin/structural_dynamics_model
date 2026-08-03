% ============================================================================
% CONSTRAINT STORY: fiat_efficacy_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiat_efficacy_kernel_flat_control, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fiat_efficacy_kernel_flat_control
 *   human_readable: Efficacy Claim of Unenacted Political Speech-Acts (Fiat/Simulation)
 *   domain: debate_theory/political_philosophy
 *
 * SUMMARY:
 *   Six distinct discursive traditions — competitive fiat debate,
 *   hypothetical policy scholarship, prefigurative protest, philosophical
 *   axiomatics, legislative simulation, and shadow/mock policy tribunals —
 *   converge on the same load-bearing claim: that a speech-act which does not
 *   bind the state nonetheless does real transformative work. This flat story
 *   treats that convergent claim as ONE constraint rather than decomposing it
 *   into the six distinct 'readings' of the mechanism (deliberative
 *   rehearsal, discursive shift, coalition-signal, conceptual clarification,
 *   predictive modeling, moral standing-taking). Authored at the substrate
 *   level, the claim presents as coordination (rope): a genuine low-cost
 *   arena for testing ideas before the stakes of enactment. But the metrics
 *   tell a more mixed story — extraction is moderate and rising (0.38 at
 *   T=40), because the institutions that produce and certify fiated/simulated
 *   speech (debate leagues, policy-simulation journals, protest
 *   organizations) increasingly capture prestige, funding, and legitimacy
 *   from the practice while the intended material beneficiaries see no
 *   correspondingly rising relief. Theater ratio is also moderate and rising
 *   (0.42), consistent with a practice whose self-certifying success criteria
 *   (a good ballot, a cited model, a well-attended march) drift from its
 *   founding external criteria (actual legislative or material change).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiat_efficacy_kernel_flat_control, 0.38).
domain_priors:suppression_score(fiat_efficacy_kernel_flat_control, 0.24).
domain_priors:theater_ratio(fiat_efficacy_kernel_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiat_efficacy_kernel_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(fiat_efficacy_kernel_flat_control, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(fiat_efficacy_kernel_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fiat_efficacy_kernel_flat_control, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fiat_efficacy_kernel_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiat_efficacy_kernel_flat_control, rope).
narrative_ontology:human_readable(fiat_efficacy_kernel_flat_control, "Efficacy Claim of Unenacted Political Speech-Acts (Fiat/Simulation)").
narrative_ontology:topic_domain(fiat_efficacy_kernel_flat_control, "debate_theory/political_philosophy").

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(fiat_efficacy_kernel_flat_control, fiat_efficacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiat_efficacy_kernel_flat_control, academic_debate_community).
narrative_ontology:constraint_beneficiary(fiat_efficacy_kernel_flat_control, policy_scholars).
narrative_ontology:constraint_beneficiary(fiat_efficacy_kernel_flat_control, activist_organizations).
narrative_ontology:constraint_beneficiary(fiat_efficacy_kernel_flat_control, philosophy_departments).
narrative_ontology:constraint_victim(fiat_efficacy_kernel_flat_control, communities_awaiting_material_relief).
narrative_ontology:constraint_victim(fiat_efficacy_kernel_flat_control, novice_debaters_and_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fiat_efficacy_kernel_flat_control, novice_debaters_and_students).
narrative_ontology:constraint_victim(fiat_efficacy_kernel_flat_control, activist_organizations).
narrative_ontology:constraint_vindicates(fiat_efficacy_kernel_flat_control, non_binding_speech_has_transformative_value).
narrative_ontology:constraint_vindicates(fiat_efficacy_kernel_flat_control, simulation_can_substitute_for_enactment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs competitive debate leagues, curricula, and journals built entirely on the premise that arguing a fiated policy ('the United States federal government should...') produces real argumentative, pedagogical, and civic skill even though no debater's ballot ever compels legislation. The community sets the norms for what counts as a 'good' fiated case and controls entry into the practice through judging and coaching credentials.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, academic_debate_community, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(fiat_efficacy_kernel_flat_control, academic_debate_community, agenda_setter).

% Publish hypothetical policy advocacy and simulated legislative models as scholarship, building careers, tenure cases, and grant portfolios on the claim that modeling a policy's consequences shapes eventual real policy uptake. Their exit from the claim would mean abandoning entire subfields (policy simulation, wargaming, model legislation drafting).
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, policy_scholars, beneficiary,
    institutional, generational, mobile, national).

% Stage protests, mock tribunals, and shadow legislation with no formal binding force, arguing that the act of publicly demanding a policy shifts the discursive terrain, builds coalitions, and pressures officials indirectly. They also bear costs when the promised downstream transformation fails to materialize and morale/resources are spent on symbolic acts instead of direct material aid.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, activist_organizations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fiat_efficacy_kernel_flat_control, activist_organizations, payer).

% Teach and publish on hypothetical imperatives, thought experiments, and axiomatic commitments as if the mere articulation of a coherent normative claim does philosophical work independent of any enactment. Their disciplinary standing depends on this claim being taken seriously as more than academic exercise.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, philosophy_departments, beneficiary,
    institutional, civilizational, mobile, national).

% Are the intended beneficiaries of the policies being simulated, argued, or fiated — housing reform, climate legislation, criminal justice change — but receive no material relief while the discourse about efficacy continues. They bear the deferred cost when 'the argument was made' substitutes for 'the policy was enacted,' and have no seat in adjudicating whether the discourse is actually building toward their relief or merely performing concern.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, communities_awaiting_material_relief, payer,
    powerless, biographical, trapped, local).

% Invest years of unpaid labor mastering the norms of fiated argument on the promise that the skill transfers to real civic efficacy and personal advancement. Some genuinely gain transferable skill; others discover the practice is closed-loop — legible only within the competitive circuit that created the norms — and their labor was consumed by the institution's own reproduction.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, novice_debaters_and_students, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(fiat_efficacy_kernel_flat_control, novice_debaters_and_students, beneficiary).

% Are the notional audience whose behavior fiated advocacy, simulation, and protest are meant to eventually move, but are rarely present in the rooms where the efficacy claim is adjudicated. Whether they are actually moved by any of this activity is asserted by the practitioners, not tested against the officials' own account of what changes their votes.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, elected_officials_and_agencies, excluded,
    powerful, biographical, analytical, national).

% Study whether simulated or fiated advocacy measurably precedes or causes real policy change, or whether the correlation practitioners cite is post-hoc self-selection (people who would have won anyway also happen to have practiced debate/scholarship). They have no stake in the claim's truth either way.
narrative_ontology:constraint_stakeholder(fiat_efficacy_kernel_flat_control, independent_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-cost, low-risk arena in which competing normative and policy claims can be tested, refined, and rehearsed without the stakes of binding enactment — allowing argument quality, coalition-building, and conceptual clarification to develop faster than the formal legislative or judicial process permits.
% TRANSFER_FUNCTION: Moves institutional prestige, grant funding, pedagogical legitimacy, and activist morale toward the practitioners and institutions that produce fiated/simulated speech-acts, while the material relief nominally at stake (housing, climate, justice outcomes) remains deferred to the communities the speech-acts are ostensibly about.
% ABSENT_VOICES: The elected officials and agencies who are the notional target of persuasion are almost never asked whether fiated advocacy, academic simulation, or symbolic protest actually changed their calculus — the efficacy claim is validated internally, by the community that benefits from believing it, rather than externally by the audience it claims to move. The communities awaiting material relief are also structurally absent from the adjudication of whether the discourse is working.
% DISAPPEARANCE_RATIONALE: If fiated/simulated political speech vanished overnight, the debate community, policy-simulation subfields, and much of normative philosophy would lose their institutional rationale and rearrange substantially — but whether the material world (actual legislation, actual relief) would rearrange at all is exactly the contested claim; practitioners assert transformative spillover, independent analysts find the causal link largely unproven, and the intended beneficiary communities report no detectable difference either way.
% FOUNDING_PROBLEM: Formal political power (voting, litigation, legislation) is slow, exclusionary, and often inaccessible to those without standing, resources, or citizenship — fiated and simulated speech-acts were developed to let ideas, arguments, and proposed policies be tested, refined, and advocated by people and about situations the formal process would otherwise ignore or delay indefinitely.
% FOUNDING_PROBLEM_CORROBORATION: Academic debate coaches and policy scholars (the direct beneficiaries) attest the founding problem remains live and that the practice measurably develops civic and analytical capacity. Independent policy analysts, examining outcome data across activist campaigns and simulated-policy literature, report inconsistent and largely unmeasured causal linkage between the speech-act and downstream enactment — corroboration from outside the beneficiary set is thin, and no representative of the awaiting-relief communities has been queried in the practitioner literature itself.
narrative_ontology:disappearance_verdict(fiat_efficacy_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(fiat_efficacy_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fiat_efficacy_kernel_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(fiat_efficacy_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(fiat_efficacy_kernel_flat_control, 0.38, 'claude-sonnet-5', 'fiat_efficacy_kernel_2026_20260803_102258', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiat_efficacy_kernel_flat_control_tests).
:- end_tests(fiat_efficacy_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Suppression is low (0.24) because no one is coerced into believing fiat/simulation is efficacious — the norm propagates by voluntary institutional participation, not force. Accessibility collapse is moderate-low (0.35): exiting the belief that fiated speech matters is fully available to any observer, and skeptical scholarship exists, but within each producing institution (debate leagues, academic disciplines) the belief functions as a near-total professional precondition — you cannot coach, publish, or organize within these traditions while publicly denying the practice's efficacy, so collapse is locally severe even though globally mild. Resistance is moderately high (0.55): independent policy analysts, skeptical practitioners, and burned-out activists actively contest the efficacy claim, which is itself evidence this is not a settled mountain but a maintained position.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (academic debate community, policy scholars), this looks like rope: a genuine, low-risk coordination mechanism that develops real skill and models real policy consequences. From the payer seat (communities awaiting relief, novice debaters), the same practice can look like a tangled or extractive arrangement: years or generations of discursive activity accrue prestige and funding to the producing institutions while the promised downstream transformation remains permanently on the horizon. The engine should register this divergence directly from the structural data — high power/mobile exit for beneficiaries versus powerless/trapped-or-constrained exit for payers — without either the claim or the commentary forcing a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The producing institutions (debate community, policy scholars, philosophy departments, activist organizations) are structural beneficiaries — the efficacy claim is their institutional oxygen, whether or not the claim is externally validated. Communities awaiting material relief and novice debaters/students who invest years in the practice are the structural payers — they bear the deferred or unpaid cost of a claim whose validation is internal to the producing institutions rather than tested against the target audience (elected officials) or the intended beneficiaries. Elected officials, the notional audience, are excluded from the adjudication entirely — their actual responsiveness to fiated/simulated speech is asserted, not measured, by the community that benefits from the assertion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (formal political power is slow and exclusionary; simulated/fiated speech lets excluded actors test and advocate ideas anyway) may remain partly live — marginalized groups still lack direct legislative access — but the practice's institutional apparatus (competitive leagues, tenure-bearing subfields, protest-organization budgets) has grown substantially independent of whether the founding problem is actually being solved. Classifying this as a flat rope-claiming constraint rather than a snare prevents mislabeling genuine coordination value (real skill development, real conceptual clarification, real coalition-building) as pure extraction; but the rising extractiveness and theater trajectories, plus the founding_problem_status of 'contested' with thin external corroboration, flag that some fraction of the practice has drifted from coordination toward self-perpetuating institutional reproduction — exactly the ambiguity the omega variables below are meant to hold open rather than resolve by authorial fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_causal_link_unresolved,
    'Does unenacted political speech (fiat, simulation, protest, axiom) actually cause downstream material or policy change, or does the correlation practitioners cite reflect selection effects (people who would have succeeded anyway also happen to engage in these practices)?',
    'Longitudinal, controlled comparison of policy outcomes and civic-capacity development between matched cohorts who did and did not engage in fiated debate, policy simulation, or prefigurative activism, holding prior political access and resources constant.',
    'If the causal link is real and substantial, the constraint is closer to genuine rope — a low-cost mechanism producing real downstream value. If the correlation is largely selection or self-certification, the constraint is closer to a tangled rope or piton — institutional reproduction dressed as civic mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficacy_causal_link_unresolved, empirical, 'Whether fiat/simulation efficacy is a real causal mechanism or a self-certifying institutional narrative.').

omega_variable(
    site_of_transformation_ambiguity,
    'Is the actual site of transformation the speaker (skill/capacity building), the audience (elected officials whose calculus shifts), the discourse itself (what becomes sayable/thinkable), or the producing institution (which gains legitimacy/funding regardless of external effect)?',
    'This is exactly what the six original readings each answer differently; a flat authoring of the shared commitment necessarily leaves the site-of-transformation question open rather than adjudicated, since resolving it would require decomposing into the reading-specific constraint stories.',
    'If the site is the speaker or discourse, the practice looks more like genuine coordination (rope) even absent official responsiveness. If the site is claimed to be the audience but the audience is never actually measured, the claim risks being a Mountain-flavored assertion (treated as self-evidently true) resting on an unverified empirical premise — a false summit pattern worth flagging even without formal FSM triggering here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(site_of_transformation_ambiguity, conceptual, 'Whether the flat commitment obscures a genuine disagreement about who or what is actually transformed.').

omega_variable(
    beneficiary_capture_vs_genuine_service,
    'Are the producing institutions (debate leagues, policy-simulation journals, activist orgs, philosophy departments) genuinely in service of the communities and problems they claim to address, or have they substantially captured the discourse of efficacy to sustain their own funding, prestige, and reproduction independent of external validation?',
    'Track whether producing-institution budgets, publication counts, and participation numbers grow independent of any measurable movement on the founding problem (e.g., does fiated climate-policy debate activity correlate with actual climate policy movement, or only with debate-circuit growth?).',
    'High independence between institutional growth and founding-problem resolution would support reclassification toward tangled_rope or piton at the institutional seat, while leaving individual-level skill-building value (rope-like) intact at the participant seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_service, empirical, 'Whether institutional growth in fiat/simulation practice tracks or has decoupled from the founding problem it claims to serve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiat_efficacy_kernel_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiat_tr_t0, fiat_efficacy_kernel_flat_control, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fiat_tr_t8, fiat_efficacy_kernel_flat_control, theater_ratio, 8, 0.3).
narrative_ontology:measurement(fiat_tr_t16, fiat_efficacy_kernel_flat_control, theater_ratio, 16, 0.34).
narrative_ontology:measurement(fiat_tr_t24, fiat_efficacy_kernel_flat_control, theater_ratio, 24, 0.37).
narrative_ontology:measurement(fiat_tr_t32, fiat_efficacy_kernel_flat_control, theater_ratio, 32, 0.4).
narrative_ontology:measurement(fiat_tr_t40, fiat_efficacy_kernel_flat_control, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(fiat_be_t0, fiat_efficacy_kernel_flat_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fiat_be_t8, fiat_efficacy_kernel_flat_control, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(fiat_be_t16, fiat_efficacy_kernel_flat_control, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(fiat_be_t24, fiat_efficacy_kernel_flat_control, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(fiat_be_t32, fiat_efficacy_kernel_flat_control, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(fiat_be_t40, fiat_efficacy_kernel_flat_control, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fiat_efficacy_kernel_flat_control, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This is the FLAT construction of a substrate that a companion set of six stories decomposes into distinct kernel readings (deliberative-rehearsal, discursive-shift, coalition-signal, conceptual-clarification, predictive-modeling, and standing-taking mechanisms). This story deliberately does NOT author cs_structure.reading_relations or axioms and does NOT list sibling constraint_ids in affects_constraints, per the construction-perturbation control instructions: it authors the shared commitment as one constraint, letting contestation surface only through perspectival stakeholder divergence and the omegas above, not through kernel-reading machinery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
