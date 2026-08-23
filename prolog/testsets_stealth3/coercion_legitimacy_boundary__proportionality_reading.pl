% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Pathogen-Proportional Coercion Legitimacy Boundary (Proportionality Reading)
 *   domain: public health policy/medical ethics/constitutional law
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the
 *   coercion-legitimacy kernel: the claim that state coercion over medical
 *   intervention is legitimate to the degree that the specific pathogen's
 *   severity and transmission dynamics warrant it — measles (high
 *   transmissibility, serious complication risk) justifies school-entry
 *   mandates; seasonal influenza (lower per-case severity in most seasons)
 *   does not justify general mandates, though contested occupational cases
 *   persist. The constraint under examination is the standing arrangement of
 *   pathogen-indexed coercion adjudication as actually practiced: statutory
 *   school-entry laws, healthcare employment rules, emergency declarations,
 *   and the exemption machinery around them. Epsilon is authored for THAT
 *   arrangement as this reading assesses it — not for the reading's endorsed
 *   ideal, and not averaged with sibling readings, which are separate
 *   constraints in separate files. Time points index years 2000 (T=0) through
 *   2024 (T=24); the shared grid captures steady-state operation, the crisis
 *   spike of 2020, and partial post-emergency relaxation.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda setter (institutional/constrained) — designates triggering pathogens, enforces, receives compliance and administrative authority
 *   - mmr_mandate_objectors: primary target (moderate/constrained) — bears school-entry coercion for the paradigm justified case
 *   - healthcare_workers_flu_mandated: target at the contested margin (organized/constrained) — bears coercion the reading's own logic places below the general-population threshold
 *   - post_attenuation_mandate_subjects: drift-lagged targets (moderate/trapped) — bore mandates past their epidemiological warrant
 *   - immunocompromised_patients and infants_too_young_to_vaccinate: protected beneficiaries (powerless/trapped) — receive coverage without bearing compliance
 *   - vaccine_manufacturers: incidental collector (institutional/arbitrage) — converts designations into guaranteed demand
 *   - low_trust_minority_communities: excluded voice (powerless/trapped) — bears enforcement contact, absent from threshold-setting
 *   - courts_reviewing_mandate_challenges: analytical observer (institutional/analytical) — adjudicates the boundary's outer limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.48).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.52).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Pathogen-Proportional Coercion Legitimacy Boundary (Proportionality Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public health policy/medical ethics/constitutional law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'a07c6bd3-577a-407f-974c-e1f52bd0868a').
narrative_ontology:cs_kernel_codification('a07c6bd3-577a-407f-974c-e1f52bd0868a', distributed).
narrative_ontology:cs_authority_grounding('a07c6bd3-577a-407f-974c-e1f52bd0868a', expertise).
narrative_ontology:cs_interpretation_layer_present('a07c6bd3-577a-407f-974c-e1f52bd0868a').
narrative_ontology:cs_reading_relation('a07c6bd3-577a-407f-974c-e1f52bd0868a', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('a07c6bd3-577a-407f-974c-e1f52bd0868a', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('a07c6bd3-577a-407f-974c-e1f52bd0868a', foundational, coercion_warrant_tracks_epidemiological_severity).
narrative_ontology:cs_axiom_status(coercion_warrant_tracks_epidemiological_severity, holdable).
narrative_ontology:cs_axiom_grounding('a07c6bd3-577a-407f-974c-e1f52bd0868a', coercion_warrant_tracks_epidemiological_severity, empirically_contingent).
narrative_ontology:cs_axiom('a07c6bd3-577a-407f-974c-e1f52bd0868a', foundational, case_by_case_adjudication_over_blanket_rules).
narrative_ontology:cs_axiom_status(case_by_case_adjudication_over_blanket_rules, holdable).
narrative_ontology:cs_axiom_grounding('a07c6bd3-577a-407f-974c-e1f52bd0868a', case_by_case_adjudication_over_blanket_rules, conventional).
narrative_ontology:cs_reference_frame('a07c6bd3-577a-407f-974c-e1f52bd0868a', epidemiologically_calibrated_police_power).
narrative_ontology:cs_drift_state('a07c6bd3-577a-407f-974c-e1f52bd0868a', post_pandemic_emergency_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('a07c6bd3-577a-407f-974c-e1f52bd0868a', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, infants_too_young_to_vaccinate).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, healthcare_workers_flu_mandated).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, mmr_mandate_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, post_attenuation_mandate_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designate which pathogens trigger mandatory vaccination, quarantine, or exclusion rules; set exemption criteria; enforce through school-entry requirements and employment conditions. Each designation converts individual decision-rights into administrative authority lodged in these agencies, and each enforcement action yields compliance they do not otherwise command. Exit would mean returning designation power to legislatures or courts, which they resist.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, beneficiary).

% Parents and adult students subject to school-entry measles vaccination requirements who decline on conscientious or religious grounds. They bear exclusion from public schooling or costly private and home-schooling workarounds. A subset belongs to communities where refusal is fused with religious or communal identity, making the personal cost of compliance social as well as physical. Litigation and exemption campaigns are their main levers.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, mmr_mandate_objectors, payer,
    moderate, biographical, constrained, national).

% Nurses, physicians, and allied staff required by employers or state rules to accept annual influenza vaccination as a condition of patient-facing work. Because seasonal influenza sits near the low-severity edge of what this framework's logic would justify for the general population, their requirement is a standing threshold dispute: professional associations and unions argue the coercion outruns the warrant, while infection-control bodies cite patient vulnerability. Exit means leaving clinical roles.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, healthcare_workers_flu_mandated, payer,
    organized, biographical, constrained, national).

% Workers and students carrying mandates adopted during the high-severity phase of a pandemic pathogen that subsequently attenuated. Official declarations and policy rescissions lagged viral evolution, so they bore employment and enrollment conditions past the point the original severity data supported. Their recourse was individual compliance or job loss; collective revision depended on slow administrative review.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, post_attenuation_mandate_subjects, payer,
    moderate, biographical, trapped, national).

% People who cannot safely receive certain vaccines and depend on surrounding community coverage for protection. They receive the protective effect of others' mandated compliance while bearing none of the compliance burden, and they have no substitute for the coverage they depend on.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Pre-vaccination-age infants protected by the coverage of everyone around them, concentrated in outbreak zones where a single introduction spreads rapidly. Caregivers act on their behalf; the protection arrives or fails locally based on neighborhood and school coverage rates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, infants_too_young_to_vaccinate, beneficiary,
    powerless, immediate, trapped, local).

% Producers whose products gain guaranteed demand floors wherever mandates attach. They neither set the severity thresholds nor run enforcement, but every designation converts uncertain market demand into reliable procurement, and liability protections in most regimes insulate them from the downstream disputes. They can relocate production or portfolio emphasis across jurisdictions at will.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Communities carrying historical trauma from coercive public health and medical practices, who have the most contact with enforcement machinery and the least presence in the advisory committees and hearings where thresholds and exemption rules are set. They would press for equity safeguards, transparent enforcement criteria, and community representation, but the conversation happens without them.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, low_trust_minority_communities, excluded,
    powerless, generational, trapped, national).

% Judicial bodies adjudicating challenges to compulsory vaccination and quarantine under a century-old precedent lineage balancing state protective power against individual liberty. Their rulings recalibrate the outer limits of the boundary but do not set day-to-day thresholds; they take testimony from all other seats and publish reasoning that becomes the framework's public justification.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, courts_reviewing_mandate_challenges, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the herd-immunity free-rider problem while rationing state coercive authority: it concentrates mandates where transmission dynamics make individual refusal collectively dangerous and withholds them where the collective stakes are low, preserving public consent and legitimacy capital for the high-stakes cases.
% TRANSFER_FUNCTION: Moves bodily decision-rights from individuals and parents to state health authorities in proportion to assessed pathogen threat; moves compliance costs onto mandate subjects; distributes reduced transmission risk to the whole population, concentrated on those who cannot protect themselves.
% ABSENT_VOICES: Low-trust minority communities bear disproportionate enforcement contact but are not seated in threshold-setting advisory bodies; people with medical contraindications caught at exemption margins lack organized representation; future patients affected by today's threshold placements have no seat anywhere.
% DISAPPEARANCE_RATIONALE: If the pathogen-proportional boundary vanished overnight, the vacuum fills with one of the rival defaults: either all medical coercion loses its principled defense and school-entry and occupational mandates collapse case by case, or coercion becomes unbounded by severity and extends to every transmissible condition. School attendance law, healthcare employment policy, and emergency-preparedness statutes all reorganize around whichever default prevails.
% FOUNDING_PROBLEM: Compulsory smallpox vaccination at the turn of the twentieth century forced the question of when state protective power may override bodily autonomy without destroying public consent; the proportionality answer — calibrate coercion to the demonstrated severity and transmissibility of the specific pathogen — was built to reconcile collective disease control with liberal limits on police power.
% FOUNDING_PROBLEM_CORROBORATION: Live status is corroborated from outside the benefiting parties: appellate courts continue to decide mandate challenges under the precedent lineage, bioethics journals publish competing proportionality analyses after every new pathogen, and legislative hearings revisit threshold statutes. Agency self-attestation that the problem persists is interested and weighted accordingly; the judicial and scholarly record independently confirms each new pathogen reopens the calibration question.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the arrangement genuinely tracks pathogen characteristics much of the time, but threshold disputes, declaration lag, and uneven exemption administration produce real extraction concentrated at the margins — healthcare-worker influenza mandates and post-attenuation pandemic mandates are the load-bearing examples. Suppression (0.52) is structural: school exclusion, employment conditions, and penalty schedules, moderated by genuinely available exemption routes and exit-by-exit-cost. Theater (0.25) reflects a mostly functional analytic core (seroprevalence data, R0 estimation, severity review) with a ritual layer of performative hearings and symbolic compliance campaigns. Accessibility collapse is low-moderate (0.42): rival boundary principles remain live in courts and legislatures, and within-rule alternatives (exemptions, private arrangements) persist at real cost. Resistance (0.58) is sustained: litigation, exemption movements, union pushback, and political backlash all contest placements. The temporal series runs on one shared grid (T=0..24 at 4-year steps) across all three tracked metrics; the 2020 spike and subsequent relaxation form a crisis-driven cycle driven by exogenous pathogen events, not intermittent reinforcement — the oscillation is a side effect of viral evolution colliding with slow administrative revision, and the base_properties scalars are measured at the end-state (T=24, post-relaxation). Suppression_requirement is tracked because the story specifically traces enforcement-capacity change: emergency powers built up rapidly in 2020 and were partially dismantled afterward.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From public_health_authorities' position the boundary is a calibrated instrument they operate competently and each designation is evidence-based necessity. From healthcare_workers_flu_mandated's position the same instrument misfires at the margin — their coercion lacks the warrant the framework's own logic requires, and their organized status makes that objection audible. From post_attenuation_mandate_subjects' position the instrument failed on timing: the warrant expired before the mandate did. Courts see manageable line-drawing; the excluded communities see an enforcement machine whose design they never shaped. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Public_health_authorities sit near the beneficiary end (declare the triggers, collect the compliance and the authority; constrained exit keeps them invested). Vaccine_manufacturers sit nearest the full-beneficiary pole: pure collectors with arbitrage-grade exit, bearing none of the enforcement burden. Immunocompromised_patients and infants_too_young_to_vaccinate are trapped beneficiaries — maximal subsidy from the constraint, no ability to purchase it individually. The three payer groups sit near the target end: mmr_mandate_objectors bear coercion the reading itself deems justified (their burden is the price of a coordination they benefit from indirectly, which tempers effective extraction), while healthcare_workers_flu_mandated and post_attenuation_mandate_subjects bear coercion the reading's own logic classifies as unwarranted — their extraction is amplified by the framework's internal inconsistency. Low_trust_minority_communities are excluded rather than coordinated; their absence is commentary-grade and feeds no directionality arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — every novel pathogen reopens the calibration question, and the R5 interview records live status corroborated by courts and bioethics scholarship outside the benefiting parties. Live-status combined with a world_rearranges disappearance verdict produces no mandatrophy mismatch flag, correctly: this is not a dead mandate kept alive by inertia. The temporal series nevertheless isolates LOCALIZED mandatrophy episodes — mandates outliving their warrants during declaration lag — visible as the gap between the 2020 extraction peak and the 2024 relaxation. Reading the constraint as a whole-scale piton would mistake those episodes for the structure; reading it as a pure snare would erase the genuine coordination function that the measles case demonstrates. The tangled_rope claim preserves both facts: real coordination, real asymmetric extraction, active enforcement holding the seam.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the coercion_legitimacy_boundary kernel; what structurally changes if a sibling reading displaces it?',
    'Track doctrinal adoption: if courts or legislatures adopt public_health_primary''s unconditional outweighing test, or bodily_autonomy_primary''s categorical bar, the governing constraint changes identity — observable in statutory amendments and controlling precedent.',
    'Under public_health_primary the victim set stops varying by pathogen (every transmissible condition becomes potentially compellable) and epsilon rises; under bodily_autonomy_primary the coercion set empties entirely and the payer seats vanish. This file''s moderate, pathogen-varying profile exists only while the proportionality reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: which reading of the boundary kernel governs, and what each displacement would restructure.').

omega_variable(
    severity_threshold_placement,
    'Where on the continuous severity and transmissibility distributions does a pathogen cross from ''voluntary with encouragement'' to ''compellable''?',
    'Comparative analysis of designation decisions across jurisdictions and pathogens: if placements track measured R0 and complication rates consistently, thresholds are stable; if equivalent pathogens receive opposite designations, placement is politically contingent.',
    'Every marginal pathogen''s mandate subjects change victim status with the threshold; epsilon rises with each contested placement because adjudication discretion is where extraction concentrates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_placement, conceptual, 'Discrete thresholds cut a continuous epidemiological space; the cut points are doing unstated normative work.').

omega_variable(
    severity_vs_transmissibility_weighting,
    'Should legitimacy scale primarily with per-case severity (mortality, complication rate) or with transmission dynamics (R0, attack rate) — and does the answer flip seasonal influenza?',
    'Structured elicitation or revealed-preference analysis of actual designations: influenza carries enormous aggregate burden through sheer case volume but modest per-case severity; measles is the reverse. Which axis dominates in practice reveals the operative weighting.',
    'If aggregate burden dominates, influenza crosses the threshold and the healthcare_worker payer group''s coercion becomes warranted (their extraction claim collapses); if per-case severity dominates, the current profile holds. The victim set''s composition turns on this weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_vs_transmissibility_weighting, conceptual, 'The two axes of the proportionality formula trade off, and the trade-off is unresolved.').

omega_variable(
    variant_drift_adjudication_lag,
    'Does coercion legitimacy track the pathogen as assessed at declaration time, or the pathogen as it currently is — and how long may mandates persist after the underlying severity data shifts?',
    'Compare rescission timelines against genomic and seroprevalence records: measure the lag between documented attenuation and formal mandate withdrawal across jurisdictions.',
    'If legitimacy properly tracks the current pathogen, declaration-lag coercion is pure extraction accumulation (the 2020-2024 hump in the temporal series is structural, not episodic); if declaration-time assessment governs, the lag is a defensible transition cost. Determines whether the drift-lagged payer group is a design flaw or an inherent price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(variant_drift_adjudication_lag, empirical, 'Whether the framework''s warrant expires with the pathogen''s severity or with the paperwork revising it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(coer_tr_t0, observed).
narrative_ontology:measurement(coer_tr_t4, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(coer_tr_t4, observed).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(coer_tr_t8, observed).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(coer_tr_t12, observed).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(coer_tr_t16, observed).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(coer_tr_t20, observed).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(coer_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(coer_be_t0, observed).
narrative_ontology:measurement(coer_be_t4, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(coer_be_t4, observed).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(coer_be_t8, observed).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(coer_be_t12, observed).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement_basis(coer_be_t16, observed).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(coer_be_t20, observed).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(coer_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(coer_su_t0, observed).
narrative_ontology:measurement(coer_su_t4, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(coer_su_t4, observed).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement_basis(coer_su_t8, observed).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement_basis(coer_su_t12, observed).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement_basis(coer_su_t16, observed).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(coer_su_t20, observed).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(coer_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, resource_allocation).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family decomposition: the colloquial debate over 'vaccine mandates' conflates three structurally distinct constraints instantiating one kernel. This story (proportionality_reading) authors a scaling rule with a pathogen-varying victim set and moderate epsilon. The public_health_primary sibling authors an unconditional outweighing test — universal potential victim set, higher epsilon. The bodily_autonomy_primary sibling authors a categorical bar — empty coercion set, epsilon near zero for the coercion seats by construction. Each file carries its own stable epsilon over the same standing arrangement; linking them via network edges lets contamination analysis trace how a doctrinal shift in one reading propagates legitimacy conditions to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
