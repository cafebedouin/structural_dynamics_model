% ============================================================================
% CONSTRAINT STORY: standpoint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Standpoint-Corrective Reading of Positional Disagreement (Manager/Parent Case)
 *   domain: epistemology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the standpoint reading of a contested kernel:
 *   when a marginalized party (a parent subject to institutional oversight)
 *   and an institutional agent (a manager or caseworker) disagree about the
 *   character of their interaction, is the disagreement symmetric input to be
 *   pooled, or asymmetric testimony that should be corrected toward the
 *   marginalized report because the marginalized position sees structural
 *   features the beneficiary position cannot see in principle? The standpoint
 *   reading holds the latter: the parent's aggregated experience of repeated
 *   institutional contacts reveals a pattern (a policy's disparate operation,
 *   a credibility asymmetry, a routine practice with disparate effects) that
 *   is structurally invisible from the manager's case-by-case vantage point,
 *   not merely under-reported by the manager. The manager's inability to see
 *   this pattern is not a personal failing to be corrected by better
 *   listening in the individual case; it is a structural feature of the
 *   position itself. This reading is one of four siblings under the kernel
 *   positional_disagreement_as_evidence: the pragmatist reading treats the
 *   disagreement as raw material to be adjudicated by practical consequences;
 *   the proceduralist reading treats it as resolved by following fair
 *   procedure regardless of outcome; the instrumentalist reading treats each
 *   report as an instrument whose reliability is assessed independent of the
 *   reporter's position. This story authors ONLY the standpoint reading as
 *   its own constraint, with its own epsilon, beneficiary/victim structure,
 *   and classification — the sibling readings are separate constraints linked
 *   via network.affects_constraints, not alternative measurements folded into
 *   this one.
 *
 * KEY AGENTS:
 *   - institutional_manager: agenda_setter/beneficiary (institutional/mobile) — sets evidentiary defaults, cannot see the pattern from position
 *   - credentialed_evaluators: beneficiary (organized/mobile) — receive unearned credibility excess
 *   - marginalized_parent: payer (powerless/trapped) — bears credibility deficit despite superior positional access to the pattern
 *   - similarly_positioned_families: payer (powerless/trapped) — aggregate corroboration structurally prevented
 *   - corrective_weighting_advocates: observer/excluded (moderate/constrained) — argue for reform from outside the dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(standpoint_reading, 0.68).
domain_priors:suppression_score(standpoint_reading, 0.72).
domain_priors:theater_ratio(standpoint_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(standpoint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(standpoint_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(standpoint_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(standpoint_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(standpoint_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(standpoint_reading, tangled_rope).
narrative_ontology:human_readable(standpoint_reading, "Standpoint-Corrective Reading of Positional Disagreement (Manager/Parent Case)").
narrative_ontology:topic_domain(standpoint_reading, "epistemology/institutional_analysis").

domain_priors:requires_active_enforcement(standpoint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(standpoint_reading, '0de0063d-5dc7-4cbb-b128-f9e02f76d477').
narrative_ontology:cs_kernel_codification('0de0063d-5dc7-4cbb-b128-f9e02f76d477', distributed).
narrative_ontology:cs_authority_grounding('0de0063d-5dc7-4cbb-b128-f9e02f76d477', distributed).
narrative_ontology:cs_reading_relation('0de0063d-5dc7-4cbb-b128-f9e02f76d477', standpoint_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0de0063d-5dc7-4cbb-b128-f9e02f76d477', standpoint_reading__proceduralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0de0063d-5dc7-4cbb-b128-f9e02f76d477', standpoint_reading__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('0de0063d-5dc7-4cbb-b128-f9e02f76d477', foundational, positional_asymmetry_grounds_corrective_weighting).
narrative_ontology:cs_axiom_status(positional_asymmetry_grounds_corrective_weighting, holdable).
narrative_ontology:cs_axiom_grounding('0de0063d-5dc7-4cbb-b128-f9e02f76d477', positional_asymmetry_grounds_corrective_weighting, deontological).
narrative_ontology:cs_axiom('0de0063d-5dc7-4cbb-b128-f9e02f76d477', foundational, marginalized_standpoint_has_privileged_structural_access).
narrative_ontology:cs_axiom_status(marginalized_standpoint_has_privileged_structural_access, holdable).
narrative_ontology:cs_axiom_grounding('0de0063d-5dc7-4cbb-b128-f9e02f76d477', marginalized_standpoint_has_privileged_structural_access, empirically_contingent).
narrative_ontology:cs_reference_frame('0de0063d-5dc7-4cbb-b128-f9e02f76d477', credentialed_testimony_default).
narrative_ontology:cs_drift_state('0de0063d-5dc7-4cbb-b128-f9e02f76d477', post_standpoint_critique_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0de0063d-5dc7-4cbb-b128-f9e02f76d477', '').
narrative_ontology:cs_kernel_id(standpoint_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(standpoint_reading, institutional_manager).
narrative_ontology:constraint_beneficiary(standpoint_reading, credentialed_evaluators).
narrative_ontology:constraint_victim(standpoint_reading, marginalized_parent).
narrative_ontology:constraint_victim(standpoint_reading, similarly_positioned_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the arrangement's rules of evidence — decides what counts as a legitimate complaint, which reports get investigated, whose account is treated as the baseline description of what happened. Occupies a structural position from which certain features of the arrangement (how the rules disadvantage the family, how the institution's routine operation produces the harm being complained about) are not visible, because the manager's daily practice does not require seeing them to function successfully in the role. Can rotate out of the position or be reassigned without losing standing.
narrative_ontology:constraint_stakeholder(standpoint_reading, institutional_manager, agenda_setter,
    institutional, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(standpoint_reading, institutional_manager, beneficiary).

% Professionals (caseworkers, clinicians, administrators) whose credentials are treated as the default source of credibility when their account conflicts with the parent's. Their testimony receives an unearned credibility excess simply by virtue of institutional position, independent of their actual epistemic access to the disputed facts.
narrative_ontology:constraint_stakeholder(standpoint_reading, credentialed_evaluators, beneficiary,
    organized, biographical, mobile, regional).

% Reports a pattern in how the institution treats her family that the institution's staff do not report, because the pattern is only visible from the position of being subject to repeated, aggregated institutional contacts rather than from the position of handling one case at a time. Her testimony is discounted by a credibility deficit tracking her social position rather than her actual reliability. Cannot exit the institutional relationship without losing access to a service her family needs (custody proceedings, benefits, schooling).
narrative_ontology:constraint_stakeholder(standpoint_reading, marginalized_parent, payer,
    powerless, immediate, trapped, local).

% Other families in structurally similar positions whose aggregated pattern-testimony would corroborate the individual parent's account, but who are not consulted as a class and whose individually-discounted reports never get pooled into a corroborating pattern because the institution processes complaints one case at a time.
narrative_ontology:constraint_stakeholder(standpoint_reading, similarly_positioned_families, payer,
    powerless, generational, trapped, regional).

% Researchers and advocates who argue, following Fricker and standpoint theorists, that the parent's report should receive corrective epistemic weighting precisely because her structural position gives her access to features of the arrangement invisible from the manager's seat. They are not parties to the specific dispute and their framework is not built into the institution's actual evidentiary procedure — they observe and argue for reform from outside it.
narrative_ontology:constraint_stakeholder(standpoint_reading, corrective_weighting_advocates, observer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(standpoint_reading, corrective_weighting_advocates, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(standpoint_reading, institutional_manager).
narrative_ontology:fixing_cost_class(standpoint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The institution coordinates case handling by assigning default evidentiary weight to credentialed staff testimony, which lets it process large caseloads without re-litigating each disputed fact from scratch.
% TRANSFER_FUNCTION: Moves credibility — and with it, the practical power to have one's account of events treated as authoritative — from the structurally marginalized reporting party to the structurally advantaged institutional party, systematically and not by accident of the individual case.
% ABSENT_VOICES: Similarly positioned families whose aggregated testimony would corroborate the pattern are never consulted as a class; the institution's case-by-case processing structurally prevents their reports from ever being pooled into a corroborating whole.
% DISAPPEARANCE_RATIONALE: If the default credibility asymmetry disappeared and corrective weighting toward structurally marginalized testimony were adopted instead, case outcomes affecting custody, benefits, and service access would shift measurably; the manager's discretionary authority to define the baseline account of events would be curtailed; and complaint patterns currently invisible in aggregate would become visible and actionable.
% FOUNDING_PROBLEM: Institutions built triage and evidentiary defaults (crediting credentialed staff accounts by default) to process large caseloads efficiently without re-investigating every disputed claim from first principles.
% FOUNDING_PROBLEM_CORROBORATION: The institution's own administrators attest the default is still necessary for caseload efficiency. Standpoint theorists and advocacy researchers, external to the institution and not benefiting from the default, attest that the efficiency rationale has calcified into a structural credibility asymmetry that no longer tracks actual epistemic reliability — this corroboration comes from academic and advocacy sources outside the benefiting institutional parties.
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
 *   Extractiveness (0.68) reflects the systematic transfer of credibility and practical decision-making power from the marginalized party to the institutional party, sustained not by superior evidence but by structural position. Suppression (0.72) is high because the arrangement requires active enforcement — institutional procedures that default to crediting staff accounts, and no formal channel that aggregates dispersed individual complaints into corroborating patterns — for the asymmetry to persist despite growing standpoint-theoretic critique. Theater ratio (0.40) reflects increasing performative gestures toward 'listening to families' and 'trauma-informed practice' that do not alter the underlying evidentiary default. Accessibility collapse (0.5) is moderate: individual families rarely have practical alternative venues for redress, but advocacy and legal-aid pathways exist, so alternatives have not fully collapsed. Resistance (0.6) is substantial and rising: families, advocates, and some reform-minded staff actively contest the default. Metrics are authored on one shared time grid.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional_manager and credentialed_evaluators sit near the beneficiary end of directionality: the credibility default subsidizes their accounts without requiring them to establish superior epistemic access. The marginalized_parent and similarly_positioned_families sit near the target end: their reports are discounted by position rather than reliability, and their trapped exit options (dependence on the institution for services) prevent arbitrage away from the asymmetry. This is the structural core of the standpoint reading: the parent's marginalized position is not merely a source of noisier data but of asymmetric epistemic ADVANTAGE regarding certain structural features (the disparate pattern of institutional treatment) that the manager cannot access in principle from an internal, case-by-case vantage point — yet the credibility default runs the opposite direction from where the epistemic advantage actually lies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (efficient caseload processing via evidentiary defaults) may remain partially live for pure administrative efficiency, but the standpoint reading holds that the specific credibility asymmetry has outlived any efficiency justification and now functions as an extraction mechanism transferring practical power to institutional actors. Classifying this as tangled_rope (rather than pure snare) preserves the genuine coordination function — some default is needed to process cases at scale — while naming the asymmetric extraction riding on top of it. A pure snare framing would deny that any coordination problem exists at all, which is not defensible; a pure rope framing would deny the asymmetric extraction, which the standpoint theorists' evidence base directly contradicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_advantage_vs_bias,
    'Is the parent''s superior positional access to the institutional pattern a genuine epistemic advantage (per Fricker/standpoint theory), or could it equally be explained as motivated perception arising from an adversarial relationship with the institution?',
    'Triangulation with independent third-party audits of the institution''s actual case-handling patterns across many families; if the aggregate pattern the parent describes is independently verifiable, standpoint advantage is corroborated over motivated-perception explanations.',
    'If the pattern is independently verifiable, the standpoint reading''s classification (asymmetric testimony requiring corrective weighting) is strongly supported. If unverifiable, the pragmatist or instrumentalist sibling readings'' treatment of the disagreement as symmetric or consequence-adjudicated becomes more defensible for this specific case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_advantage_vs_bias, empirical, 'Whether the parent''s positional report reflects genuine structural insight or adversarial bias.').

omega_variable(
    reading_selection_underdetermination,
    'Is the standpoint reading the uniquely correct framework for this dispute, or does the same disagreement admit the pragmatist, proceduralist, or instrumentalist framings equally well given the available evidence?',
    'None fully resolves this — it is a live epistemological and political dispute among the four readings'' proponents, not an empirical question with a determinate answer.',
    'If standpoint framing is contested as merely one defensible lens among several, the tangled_rope classification (asymmetric extraction riding on genuine coordination) is itself reading-relative rather than a fact independent of which reading is adopted — this is expected and is the reason each reading is authored as a separate constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_underdetermination, conceptual, 'Whether the standpoint reading''s framing is uniquely warranted or one of several coherent lenses on the same dispute.').

omega_variable(
    corrective_weighting_calibration,
    'How much corrective weighting toward the marginalized report is warranted, and does over-correction risk creating a symmetric but inverted credibility distortion?',
    'Comparative institutional design studies where corrective-weighting procedures have been piloted, measuring downstream accuracy of case outcomes against independently verified facts.',
    'If correction calibrated to actual positional epistemic advantage improves outcome accuracy, the standpoint reading''s practical remedy is validated; if it merely shifts the asymmetry without improving accuracy, the reading''s diagnostic claim may be sound while its remedy is not.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corrective_weighting_calibration, empirical, 'Whether corrective weighting as a remedy is well-calibrated to the actual epistemic asymmetry it targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(standpoint_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stan_tr_t0, standpoint_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stan_tr_t4, standpoint_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(stan_tr_t8, standpoint_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(stan_tr_t12, standpoint_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(stan_tr_t16, standpoint_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(stan_tr_t20, standpoint_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(stan_tr_t24, standpoint_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(stan_be_t0, standpoint_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stan_be_t4, standpoint_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(stan_be_t8, standpoint_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(stan_be_t12, standpoint_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(stan_be_t16, standpoint_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(stan_be_t20, standpoint_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(stan_be_t24, standpoint_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stan_su_t0, standpoint_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stan_su_t4, standpoint_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(stan_su_t8, standpoint_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(stan_su_t12, standpoint_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(stan_su_t16, standpoint_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stan_su_t20, standpoint_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stan_su_t24, standpoint_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(standpoint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(standpoint_reading, 0.1).
narrative_ontology:affects_constraint(standpoint_reading, pragmatist_reading).
narrative_ontology:affects_constraint(standpoint_reading, proceduralist_reading).
narrative_ontology:affects_constraint(standpoint_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the kernel positional_disagreement_as_evidence, each authored as a separate constraint with its own epsilon, beneficiary/victim structure, and classification per the epsilon-invariance principle. The standpoint_reading names asymmetric beneficiaries/victims and classifies as tangled_rope; the pragmatist_reading, proceduralist_reading, and instrumentalist_reading treat the same underlying disagreement through different evidentiary lenses and are expected to produce different ANDPOINT structural data and possibly different classifications. All four are linked bidirectionally via affects_constraints to preserve the constraint-family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
