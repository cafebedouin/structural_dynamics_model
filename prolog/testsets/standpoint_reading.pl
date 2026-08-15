% ============================================================================
% CONSTRAINT STORY: standpoint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_standpoint_reading, []).

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
 *   constraint_id: standpoint_reading
 *   human_readable: Standpoint-Weighted Reading of Positional Disagreement (Institutional Credibility Discounting)
 *   domain: epistemology/institutional_analysis
 *
 * SUMMARY:
 *   This constraint is the standpoint reading of a contested kernel about how
 *   positional disagreement should count as evidence in institutional
 *   adjudication (e.g., a caseworker's account versus a parent's account of
 *   the same home visit). The standpoint reading holds that the marginalized
 *   position possesses asymmetric epistemic advantage — it sees structural
 *   features of the arrangement (how discretion is actually exercised, the
 *   gap between stated and applied policy) that are, in principle, invisible
 *   from the beneficiary position. On this reading, the standing credibility
 *   default — which treats the institutional account as baseline and
 *   discounts the subordinate account — is not a neutral pooling of two
 *   symmetric inputs but an active transfer of adjudicative weight from the
 *   party with structural access to the party with structural power. ε is
 *   authored for the standing credibility-default arrangement as this reading
 *   sees it: high, active, and actively enforced through institutional review
 *   mechanisms — not for the corrective-weighting alternative this reading
 *   endorses, which would show near-zero extraction by construction.
 *
 * KEY AGENTS:
 *   - institutional_managers: primary beneficiary (institutional/arbitrage) — collects the credibility default
 *   - marginalized_reporters: primary victim (powerless/trapped) — bears the discounted-testimony transfer
 *   - corrective_weighting_advocates: reform agenda-setter (moderate/constrained) — argues for corrective weighting but lacks adjudicative authority
 *   - credentialed_evaluators: secondary beneficiary (institutional/arbitrage) — holds standing credibility excess
 *   - reviewing_tribunals: analytical observer (institutional/analytical) — adjudicates without formally recognizing the default as bias
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(standpoint_reading, 0.68).
domain_priors:suppression_score(standpoint_reading, 0.71).
domain_priors:theater_ratio(standpoint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(standpoint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(standpoint_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(standpoint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(standpoint_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(standpoint_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(standpoint_reading, tangled_rope).
narrative_ontology:human_readable(standpoint_reading, "Standpoint-Weighted Reading of Positional Disagreement (Institutional Credibility Discounting)").
narrative_ontology:topic_domain(standpoint_reading, "epistemology/institutional_analysis").

domain_priors:requires_active_enforcement(standpoint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(standpoint_reading, '286ac99a-3d15-4d0f-bf82-61cefe992c39').
narrative_ontology:cs_kernel_codification('286ac99a-3d15-4d0f-bf82-61cefe992c39', distributed).
narrative_ontology:cs_authority_grounding('286ac99a-3d15-4d0f-bf82-61cefe992c39', practice).
narrative_ontology:cs_interpretation_layer_present('286ac99a-3d15-4d0f-bf82-61cefe992c39').
narrative_ontology:cs_reading_relation('286ac99a-3d15-4d0f-bf82-61cefe992c39', standpoint_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('286ac99a-3d15-4d0f-bf82-61cefe992c39', standpoint_reading__proceduralist_reading, influences).
narrative_ontology:cs_reading_relation('286ac99a-3d15-4d0f-bf82-61cefe992c39', standpoint_reading__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('286ac99a-3d15-4d0f-bf82-61cefe992c39', foundational, positional_advantage_is_asymmetric_not_pooled).
narrative_ontology:cs_axiom_status(positional_advantage_is_asymmetric_not_pooled, holdable).
narrative_ontology:cs_axiom_grounding('286ac99a-3d15-4d0f-bf82-61cefe992c39', positional_advantage_is_asymmetric_not_pooled, empirically_contingent).
narrative_ontology:cs_axiom('286ac99a-3d15-4d0f-bf82-61cefe992c39', foundational, credibility_deficit_requires_corrective_not_neutral_weighting).
narrative_ontology:cs_axiom_status(credibility_deficit_requires_corrective_not_neutral_weighting, holdable).
narrative_ontology:cs_axiom_grounding('286ac99a-3d15-4d0f-bf82-61cefe992c39', credibility_deficit_requires_corrective_not_neutral_weighting, deontological).
narrative_ontology:cs_reference_frame('286ac99a-3d15-4d0f-bf82-61cefe992c39', credibility_default_as_neutral_baseline).
narrative_ontology:cs_drift_state('286ac99a-3d15-4d0f-bf82-61cefe992c39', post_fricker_epistemic_injustice_scholarship, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('286ac99a-3d15-4d0f-bf82-61cefe992c39', '').
narrative_ontology:cs_kernel_id(standpoint_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(standpoint_reading, institutional_managers).
narrative_ontology:constraint_beneficiary(standpoint_reading, credentialed_evaluators).
narrative_ontology:constraint_victim(standpoint_reading, marginalized_reporters).
narrative_ontology:constraint_victim(standpoint_reading, structurally_subordinate_parties).
narrative_ontology:constraint_vindicates(standpoint_reading, epistemic_injustice_thesis).
narrative_ontology:constraint_vindicates(standpoint_reading, credibility_deficit_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the position from which the arrangement's official account is authored — case files, policy assessments, incident reports. Their vantage point structurally cannot register certain features of the arrangement (how discretion is actually exercised on the ground, what compliance costs subordinate parties bear) because those features are only visible from the subordinate position. They benefit from a credibility default that treats their account as the baseline against which the marginalized report is measured for plausibility, and they administer the mechanisms (case review, appeals processes, professional judgment standards) that adjudicate disputes in their own epistemic favor.
narrative_ontology:constraint_stakeholder(standpoint_reading, institutional_managers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(standpoint_reading, institutional_managers, agenda_setter).

% A parent contesting a caseworker's account of a home visit, a patient contesting a clinician's account of informed consent, a tenant contesting a landlord's account of habitability. From this position, structural features of the arrangement are visible that are not visible from the manager's seat: the informal norms actually governing discretion, the gap between stated policy and applied practice, the selective attention the institution pays to certain cues. Testimony from this position is discounted by default — treated as less credible, more emotional, less reliable — even when it is the only vantage that registers the structural feature in question. Exit means losing the service, the custody arrangement, or the housing; there is no symmetric alternative channel.
narrative_ontology:constraint_stakeholder(standpoint_reading, marginalized_reporters, payer,
    powerless, biographical, trapped, local).

% Ombudspersons, legal aid advocates, and standpoint-informed reviewers who argue institutional adjudication should apply corrective weighting toward the marginalized report rather than pooling both accounts as symmetric inputs. They set the agenda for reform proposals (bias training, structured credibility audits, mandated corroboration-seeking) but are themselves excluded from final adjudicative authority, which institutions retain.
narrative_ontology:constraint_stakeholder(standpoint_reading, corrective_weighting_advocates, agenda_setter,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(standpoint_reading, corrective_weighting_advocates, excluded).

% Professionals whose credentials (clinical, legal, social-work) are treated as an epistemic multiplier — their trained judgment is presumptively weighted above lay testimony even on matters where the credential confers no special access to the disputed structural fact. This credibility excess is a standing asset that does not require active claiming; it accrues by default whenever a dispute is adjudicated.
narrative_ontology:constraint_stakeholder(standpoint_reading, credentialed_evaluators, beneficiary,
    institutional, civilizational, arbitrage, national).

% Courts, appeals boards, and oversight bodies that adjudicate disputes between the manager's account and the marginalized reporter's account. Under the standpoint reading they are asked to recognize the credibility default itself as a structural bias requiring correction, not a neutral background condition — a stance most tribunals do not currently formally adopt.
narrative_ontology:constraint_stakeholder(standpoint_reading, reviewing_tribunals, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(standpoint_reading, institutional_managers).
narrative_ontology:fixing_cost_class(standpoint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The underlying institutional process (casework, clinical care, tenancy administration) does solve a genuine coordination problem: someone must adjudicate disputed facts and produce an actionable determination under time and resource constraints.
% TRANSFER_FUNCTION: Moves credibility, and through it material outcomes (custody, treatment continuation, housing security), from the structurally subordinate party's account to the structurally advantaged party's account whenever the two conflict, by default and prior to any weighing of the specific evidence.
% ABSENT_VOICES: The marginalized reporters themselves are frequently not present as credible testifiers in their own adjudication — their account is filtered through the manager's summary, or discounted in the room even when they are physically present. Corrective-weighting advocates are present as reform voices but hold no adjudicative authority.
% DISAPPEARANCE_RATIONALE: If the credibility default reversed or was neutralized overnight, adjudicators would have to seek independent corroboration before crediting the institutional account over the subordinate account, appeals processes would need restructured evidentiary standards, and outcomes currently resolved by default in favor of the manager's report would become genuinely contested — a substantial rearrangement of who wins disputed-fact adjudications.
% FOUNDING_PROBLEM: Institutions built formal adjudication processes to resolve disputed factual accounts efficiently and to protect professional discretion from being overridden by every contested claim — a genuine problem when case volume is high and not every dispute can receive full independent investigation.
% FOUNDING_PROBLEM_CORROBORATION: Fricker's testimonial-injustice framework and subsequent standpoint-theory scholarship, drawing on interviews and case studies from outside the institutions being described, attest that the credibility default systematically tracks social position rather than epistemic access — this is corroboration from academic observers external to the benefiting institutions. Institutional managers themselves attest the founding problem (efficient, non-disruptive adjudication) remains live and justifies the current default; they do not corroborate the standpoint critique.
narrative_ontology:disappearance_verdict(standpoint_reading, world_rearranges).
narrative_ontology:founding_problem_status(standpoint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(standpoint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(standpoint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(standpoint_reading, 0.68, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(standpoint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(standpoint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(standpoint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the standing transfer of adjudicative weight from the marginalized reporter's account to the manager's account whenever they conflict, without independent investigation — a real cost borne repeatedly across disputed cases. Suppression (0.71) is high because exit from the adjudication process itself is typically foreclosed (the parent cannot simply decline the caseworker's evaluation and expect the custody matter to resolve favorably) and because the credibility default operates prior to any hearing of the specific facts, foreclosing the corrective channel by default rather than by explicit ruling. Theater ratio (0.42) captures that some genuine coordination function (efficient dispute resolution under volume constraints) is real, but a rising share of the adjudicative apparatus increasingly performs neutrality (structured interviews, standardized rubrics) while the underlying default persists uncorrected.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional managers and credentialed evaluators are declared beneficiaries: the credibility default is a standing asset that accrues to their accounts without requiring active claiming, deriving low directionality (near the beneficiary end). Marginalized reporters are declared victims with trapped exit options — no symmetric alternative adjudicative channel exists — deriving high directionality (near the full-target end). The engine should treat their high suppression and lack of institutional power as amplifying effective extraction relative to a mobile or arbitrage-capable target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (efficient adjudication under resource constraints) is genuinely live in the sense that some volume-driven triage remains necessary; the standpoint reading does not claim the coordination function has vanished. What it claims has drifted is that the mechanism originally justified by triage necessity has calcified into a standing credibility default that outlives any efficiency justification — the classification as tangled_rope rather than pure snare preserves the genuine coordination kernel (someone must adjudicate) while still naming the asymmetric extraction riding on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standpoint_advantage_verifiability,
    'Is the claimed epistemic advantage of the marginalized position empirically verifiable as structural (the position genuinely accesses information unavailable from the beneficiary position), or is it a normative posture asserted independent of any specific verifiable epistemic asymmetry?',
    'Case-level audits comparing what the marginalized report specifically identified against later-verified structural facts the institutional account missed or mischaracterized, across a sample of contested adjudications.',
    'If the advantage is verifiable and systematic, the standpoint reading''s victim/beneficiary structure is empirically grounded rather than purely normative, strengthening the tangled_rope classification''s asymmetric-extraction gate. If it is not systematically verifiable, the reading risks becoming an unfalsifiable presumption that could itself function as a credibility excess for advocates rather than reporters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standpoint_advantage_verifiability, empirical, 'Whether standpoint epistemic advantage is a verifiable structural asymmetry or an unfalsifiable normative posture.').

omega_variable(
    kernel_framing_choice,
    'Is the underlying kernel best framed as a dispute about evidentiary weighting procedure (which this story treats it as) or as a dispute about the legitimacy of institutional discretion itself (a broader framing under which ''positional disagreement'' is a symptom rather than the core claim)?',
    'Compare classification outcomes if the kernel were reframed around discretion-legitimacy rather than evidence-weighting: does the beneficiary/victim structure and ε value shift materially?',
    'If the broader discretion-legitimacy framing were adopted, institutional_managers might be reclassified as agenda_setters exercising delegated authority rather than pure beneficiaries, and the extraction locus would shift from the credibility default specifically to the discretion grant generally — a different, though related, constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative framing of the kernel around discretion-legitimacy versus evidence-weighting, and its effect on classification.').

omega_variable(
    reform_capture_risk,
    'Could institutionalized corrective-weighting mechanisms (mandated bias training, structured credibility audits) themselves become a new site of credibility excess for the advocates who administer them, reproducing the same structural dynamic under a different label?',
    'Longitudinal tracking of outcomes after corrective-weighting reforms are implemented: does adjudicative weight shift durably toward marginalized reporters, or does a new credentialed layer (bias auditors, corrective-weighting specialists) accrue the discretion instead?',
    'If reform capture occurs, the tangled_rope classification would persist under the reformed arrangement with different named beneficiaries rather than resolving into a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_capture_risk, preference, 'Whether corrective-weighting reforms durably shift extraction or merely relocate it to a new credentialed layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(standpoint_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stan_tr_t0, standpoint_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stan_tr_t8, standpoint_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(stan_tr_t16, standpoint_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(stan_tr_t24, standpoint_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(stan_tr_t32, standpoint_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(stan_tr_t40, standpoint_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stan_be_t0, standpoint_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stan_be_t8, standpoint_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(stan_be_t16, standpoint_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(stan_be_t24, standpoint_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(stan_be_t32, standpoint_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(stan_be_t40, standpoint_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stan_su_t0, standpoint_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stan_su_t8, standpoint_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(stan_su_t16, standpoint_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(stan_su_t24, standpoint_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(stan_su_t32, standpoint_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(stan_su_t40, standpoint_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(standpoint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(standpoint_reading, 0.1).
narrative_ontology:affects_constraint(standpoint_reading, pragmatist_reading).
narrative_ontology:affects_constraint(standpoint_reading, proceduralist_reading).
narrative_ontology:affects_constraint(standpoint_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the kernel 'positional_disagreement_as_evidence,' each authored as a separate ε-invariant constraint per the decomposition principle. The standpoint reading authors high extraction and an asymmetric beneficiary/victim structure because it treats the credibility default as an active, non-neutral transfer. The pragmatist and proceduralist readings would author much lower extraction, treating the same underlying dispute-resolution process as either outcome-optimizing or procedurally fair without positional correction. The instrumentalist reading would author extraction keyed to predictive-utility weighting rather than positional access. All four share the same underlying institutional adjudication process as their referent but diverge sharply in beneficiary/victim structure and ε because they diverge on what counts as legitimate evidentiary weighting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
