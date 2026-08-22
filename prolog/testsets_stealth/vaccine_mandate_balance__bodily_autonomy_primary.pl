% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: State-Compelled Medical Intervention Regime (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This file instantiates ONE reading — bodily_autonomy_primary — of the
 *   contested kernel vaccine_mandate_balance. The standing arrangement under
 *   contest (the epsilon referent, fixed across all readings of this kernel)
 *   is the state-compelled medical intervention regime: school-entry
 *   mandates, occupational conditions in healthcare and beyond, and
 *   emergency-powers compulsion, together with their enforcement machinery.
 *   Assessed by this reading's own lights, that regime extracts bodily
 *   self-determination categorically: whatever the collective benefit, a
 *   non-consensual intervention is a non-consensual intervention, so epsilon
 *   is authored high and indexed to coercion mechanisms rather than to
 *   penalty magnitudes. Per the epsilon-invariance principle, the colloquial
 *   label 'the vaccine mandate debate' decomposes into three structurally
 *   distinct constraints over this one referent: this file (autonomy-primary,
 *   high epsilon, victim set = the coerced), public_health_primary (low
 *   epsilon, victim set collapses toward the unvaccinated-exposed
 *   vulnerable), and proportionality_reading (intermediate, victim set =
 *   those coerced below threshold). The siblings are separate files linked
 *   via network.affects_constraints; their differing epsilon values over the
 *   shared referent are reading-indexed values, not measurement error. KEY
 *   AGENTS (by structural relationship): - unvaccinated_coerced_individuals:
 *   primary target (moderate/constrained) — bears the compelled intervention
 *   and its exclusion penalties - immunocompromised_medically_vulnerable:
 *   declared beneficiary (powerless/trapped) — receives the herd-protection
 *   the regime produces - public_health_authorities: agenda-setter
 *   (institutional/arbitrage) — designs, administers, and enforces; captures
 *   authority and penalty receipts - institutional_employers_schools:
 *   enforcer-beneficiary (institutional/mobile) — imposes conditions,
 *   offloads liability - vaccinated_compliant_majority: incidental
 *   beneficiary (moderate/mobile) — supplies the compliance the regime rides
 *   on - coercive_medicine_survivors: excluded voice (powerless/trapped) —
 *   absent from deliberation, carrying the historical record of normalized
 *   compulsion - courts_constitutional_reviewers: analytical observer
 *   (institutional/analytical) — sets the legitimacy boundary. Claim/metric
 *   independence: claimed_type is authored from this reading's structural
 *   read (genuine coordination function plus categorical asymmetric
 *   extraction under active enforcement = tangled_rope); the metrics are
 *   authored as descriptive of the regime's actual operation; the engine
 *   computes per-seat classifications and any divergence from the claim is
 *   the datum, not a defect.
 *
 * KEY AGENTS:
 *   - unvaccinated_coerced_individuals: primary target (moderate/constrained) — bears bodily compulsion, exclusion, and termination; some fraction identity-locked in refusal
 *   - immunocompromised_medically_vulnerable: declared beneficiary (powerless/trapped) — depends on surrounding coverage; cannot exit exposure except by isolation
 *   - public_health_authorities: agenda-setter (institutional/arbitrage) — sets coverage targets and exemption criteria; collects authority, budget, and penalties
 *   - institutional_employers_schools: secondary enforcer and beneficiary (institutional/mobile) — converts mandate authority into liability insulation and operational continuity
 *   - vaccinated_compliant_majority: incidental beneficiary (moderate/mobile) — bears minor burden, receives reduced transmission risk, sustains legitimacy by consent
 *   - coercive_medicine_survivors: excluded voice (powerless/trapped) — historical witnesses against normalized medical compulsion, absent from advisory tables
 *   - courts_constitutional_reviewers: analytical observer (institutional/analytical) — adjudicates the police-power/personal-liberty boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.84).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.56).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.84).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "State-Compelled Medical Intervention Regime (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '2439bc5b-99bb-45ff-88c3-727e409bf7dd').
narrative_ontology:cs_kernel_codification('2439bc5b-99bb-45ff-88c3-727e409bf7dd', formalized).
narrative_ontology:cs_authority_grounding('2439bc5b-99bb-45ff-88c3-727e409bf7dd', lineage).
narrative_ontology:cs_interpretation_layer_present('2439bc5b-99bb-45ff-88c3-727e409bf7dd').
narrative_ontology:cs_reading_relation('2439bc5b-99bb-45ff-88c3-727e409bf7dd', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('2439bc5b-99bb-45ff-88c3-727e409bf7dd', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('2439bc5b-99bb-45ff-88c3-727e409bf7dd', foundational, bodily_integrity_requires_affirmative_consent).
narrative_ontology:cs_axiom_status(bodily_integrity_requires_affirmative_consent, holdable).
narrative_ontology:cs_axiom_grounding('2439bc5b-99bb-45ff-88c3-727e409bf7dd', bodily_integrity_requires_affirmative_consent, deontological).
narrative_ontology:cs_axiom('2439bc5b-99bb-45ff-88c3-727e409bf7dd', foundational, collective_welfare_never_trumps_informed_refusal).
narrative_ontology:cs_axiom_status(collective_welfare_never_trumps_informed_refusal, holdable).
narrative_ontology:cs_axiom_grounding('2439bc5b-99bb-45ff-88c3-727e409bf7dd', collective_welfare_never_trumps_informed_refusal, deontological).
narrative_ontology:cs_reference_frame('2439bc5b-99bb-45ff-88c3-727e409bf7dd', absolute_bodily_sovereignty).
narrative_ontology:cs_drift_state('2439bc5b-99bb-45ff-88c3-727e409bf7dd', contemporary_public_health_emergency_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2439bc5b-99bb-45ff-88c3-727e409bf7dd', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_medically_vulnerable).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, institutional_employers_schools).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vaccinated_compliant_majority).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, herd_immunity_threshold_theory).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, police_power_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, utilitarian_harm_prevention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decline a mandated medical intervention and face escalating consequences: school and workplace exclusion, licensure conditions, fines, terminated employment. Some hold refusal as settled conscience or faith and experience the choice as renouncing identity rather than declining a service; others would accept the intervention if conditions were priced transparently. Exit runs through relocation, homeschooling, job change, or exemption processes of uneven availability, each carrying real cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    moderate, biographical, constrained, national).

% Cannot mount protective responses to vaccination and depend on surrounding coverage to reduce exposure. Mandate regimes name them as the principal justified beneficiaries; they receive the protective effect whether or not they endorse the coercion used to produce it. Their alternative is isolation, which trades physical safety for social existence, so they cannot exit the exposure question at all.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_medically_vulnerable, beneficiary,
    powerless, immediate, trapped, local).

% Design coverage targets, define exemption criteria, and operate enforcement. Compulsion availability brings authority, budget, crisis discretion, and penalty receipts; judicial narrowing removes instruments but rarely the office. They can shift between voluntary campaigns and mandates as politics allow, and coordinate standards across jurisdictions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Hospitals, universities, and school systems impose vaccination as a condition of entry or employment, converting mandate authority into liability insulation, insurance positioning, and outbreak-containment cost transfer onto individuals. They adopt or drop conditions with personnel consequences but little existential risk to themselves.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, institutional_employers_schools, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, institutional_employers_schools, agenda_setter).

% Accept the intervention voluntarily and receive reduced transmission risk plus uninterrupted access; their direct burden is minor. Their compliance supplies the coverage the mandate rides on, and their polling support sustains the regime's political legitimacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccinated_compliant_majority, beneficiary,
    moderate, biographical, mobile, national).

% Survivors of historical state medical coercion — sterilization programs, non-consensual experimentation — carry the lived record of what bodily compulsion becomes once normalized. They hold no seat on immunization advisory bodies; their testimony reaches deliberation only obliquely through ethics scholarship and apology commissions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, coercive_medicine_survivors, excluded,
    powerless, generational, trapped, national).

% Adjudicate the boundary between police power and personal liberty, setting the doctrinal threshold at which compulsion stands or falls. They neither collect the regime's products nor bear its interventions; their output is the legitimacy line itself, revisable case by case.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, courts_constitutional_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: vaccination carries individual costs and risks while its benefits are diffuse and shared, so voluntary uptake undersupplies the coverage needed to interrupt transmission; compulsion internalizes the externality and coordinates coverage toward the herd-immunity threshold.
% TRANSFER_FUNCTION: Moves bodily compliance and its associated risk-bearing from mandate-refusing individuals into the population's immune commons; moves penalty and exclusion costs from refusers to enforcing authorities and institutions; moves residual disease risk away from protected institutions and onto those who refuse.
% ABSENT_VOICES: Survivors of historical coercive medicine are absent from contemporary mandate deliberation; seated at the table, they would contest the state's standing as custodian of bodily consent and force the precautionary history into the record. Minority-faith objectors similarly lack seats on immunization advisory committees that draft the exemption criteria governing them.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, coverage would sag below interruption thresholds in identifiable pockets, institutions would scramble to rebuild private condition policies, litigation dockets would surge in both directions, and outbreak risk to the medically vulnerable would rise measurably — the arrangements of schools, hospitals, and health departments visibly depend on the regime's continuing operation.
% FOUNDING_PROBLEM: Recurrent epidemic threats — smallpox, polio, measles, and most recently a novel coronavirus — in which voluntary uptake fell below the threshold needed to interrupt transmission, exposing communities, and especially the medically vulnerable, to preventable severe disease and death.
% FOUNDING_PROBLEM_CORROBORATION: Independently attested outside the benefiting parties: pre-vaccine mortality and morbidity series in historical demography, ongoing outbreak surveillance reported by academic epidemiology, and judicial findings in mandate litigation that accept the factual reality of contagion while contesting remedies. No serious party disputes that the founding problem exists or is recurrent; the parties dispute exclusively whether it licenses compulsion — which is the kernel this reading instantiates.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.84 because this reading counts compelled bodily intervention as near-total extraction of bodily self-determination regardless of penalty size — the violation is categorical, so the metric tracks the mechanism (compulsion), not its intensity dial. Suppression is 0.56: enforcement is real and structural (school and workplace exclusion, licensure conditions, fines, termination) but exits partially persist — jurisdictional arbitrage, homeschooling, private employment, variable exemption regimes — so the constraint does not fully close the option space. Theater ratio is 0.33: the regime functionally raises coverage, but a growing share of its activity is performative — booster requirements persisting past marginal benefit, solidarity rituals, exemption gauntlets designed to deter rather than process — and the temporal series shows that layer thickening after the acute emergency. Accessibility collapse is 0.45 (alternatives survive in attenuated form, characteristic of a contested construct rather than a natural law); resistance is 0.7 (litigation, exemption movements, electoral backlash, and organized refusal — the highest-resistance profile a health instrument routinely meets). The three measurement series run on ONE shared nine-point grid; the suppression_requirement series deliberately traces the enforcement ratchet-and-release cycle across the emergency arc (build-up to 0.74 at peak, partial stand-down to 0.56) — the oscillation is documented rather than smoothed, and its ratchet component (whether each cycle resets to a higher baseline) is carried by the precedent_ratchet_irreversibility omega rather than asserted. Suppression is authored as a raw structural property; only extractiveness gets scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the coerced payer seat, the regime presents as enforced extraction of the body itself — near-full-target directionality, amplified by trapped or identity-locked exit, plausibly computing snare-flavored at that seat. From the immunocompromised beneficiary seat, the same structure presents as life-protecting coordination it cannot provision for itself. From the agenda-setter seat, it presents as legitimate crisis governance and routine preventive administration. None of these perceptions is authored as a classification; the engine derives each from power, exit, and directional position. The gap between the payer seat's computed type and the beneficiary seats' computed type is precisely the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: unvaccinated_coerced_individuals are the declared victims with constrained exit (and an identity-locked subset), placing them near d = 1.0 — full target — with effective extraction amplified by national scope and verification difficulty. immunocompromised_medically_vulnerable are declared beneficiaries with trapped exit: they receive the regime's protective product and cannot arbitrage it, sitting near d = 0.0 — full subsidy — which is the honest structural fact even though this reading denies the coercion producing their subsidy is legitimate. public_health_authorities combine agenda-setting with concentrated receipts (authority, budget, penalties), keeping their d low but their capture legible. institutional_employers_schools and vaccinated_compliant_majority sit low-to-mid: real benefit, minor burden. The excluded survivors' seat feeds the consensus-provenance check, not directionality. No directionality overrides are authored: the beneficiary/victim-plus-exit derivation captures every seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrent epidemic threat when voluntary coverage undershoots interruption thresholds — is live and externally corroborated, so no mandatrophy resolution is declared at the regime level: the arrangement has not outlived its function, and mandatrophy_resolved is left false. Instance-level atrophy is nonetheless visible in the theater_ratio series (measures persisting past marginal benefit after the acute phase), which is the signature the lifecycle detector should flag without reclassifying the whole regime. The classification discipline cuts both ways: the genuine, externally attested coordination function (free-rider-prone coverage of a public good) blocks labeling the regime a snare despite high epsilon — the coordination story is not cover, it is real and merely outweighed in this reading's moral accounting — while the categorical extraction of bodily integrity blocks labeling it a rope. Tangled_rope is the honest structural read from this seat. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: aligned, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the bodily_autonomy_primary reading of kernel vaccine_mandate_balance; what structurally changes if a sibling reading (public_health_primary, proportionality_reading) is adopted instead?',
    'Classify the sibling story files over the identical referent (standing mandate regimes) and compare victim sets, epsilon, and per-seat types across the three readings.',
    'public_health_primary shrinks the victim set toward empty and drops epsilon toward the coordination floor; proportionality_reading partitions mandates into proportional and disproportionate subclasses with intermediate epsilon. The disagreement is located in the victim-set boundary and the permissibility axiom, not in the facts of coverage or contagion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: same referent, three constraints, three epsilon values.').

omega_variable(
    immunocompromised_victim_status,
    'Are immunocompromised people exposed by others'' non-vaccination victims of THIS constraint, or does the reading''s risk-acceptance premise correctly exclude them from the victim set?',
    'Counterfactual exposure attribution: compare outbreak burden reaching the medically vulnerable under voluntary-coverage regimes versus mandate regimes, and test whether liberty''s known externalized risk constitutes harm the constraint imposes or risk inherent to a liberty-respecting order.',
    'Counting them as victims would enlarge the victim set and force the reading to reconcile inviolability with lethal third-party exposure; excluding them (current authoring, per the reading''s structural delta) confines victims to the coerced and keeps epsilon indexed purely to compulsion mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_victim_status, conceptual, 'Victim-set boundary: exposure-risk bearer versus coerced body.').

omega_variable(
    conditionality_coercion_boundary,
    'Where does priced conditionality become compulsion — do employment terms, school-entry conditions, and benefit ties count as state compulsion when formal alternatives remain open?',
    'Constructive-compulsion doctrinal analysis combined with labor-market evidence on how real the nominally voluntary alternatives are (monopsony power, credential gating, geographic concentration of employers).',
    'A broader boundary pushes effective extraction above the authored 0.84 and pulls the computed classification toward snare; a narrower boundary lowers it toward proportionality territory. The classification of borderline mandate instruments flips on this line.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_coercion_boundary, conceptual, 'Compulsion threshold: mandate versus conditioned option.').

omega_variable(
    precedent_ratchet_irreversibility,
    'Does each emergency normalization of medical compulsion permanently lower the threshold for the next imposition (a ratchet), or do post-emergency repeals restore the prior baseline?',
    'Cross-jurisdiction longitudinal comparison of mandate scope before, during, and after successive public-health emergencies, controlling for disease severity.',
    'A confirmed ratchet supports a rising long-run extraction trajectory and strengthens the entrenchment reading of the suppression series; baseline restoration supports a cyclical model in which enforcement oscillates around a stable mean without secular growth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_ratchet_irreversibility, empirical, 'Whether emergency compulsion leaves a permanent residue of lowered thresholds.').

omega_variable(
    refusal_identity_lock,
    'Is sustained refusal driven by structural accommodation available to refusers, or by identity fusion with refusal itself?',
    'Post-repeal behavior tracking: if refusers accept the intervention once penalties lift, refusal was strategic and exit was real; if refusal persists cost-free, it is identity-constituted and exit was never the binding constraint.',
    'Identity-locked refusers sit nearer the full-target end of directionality (higher effective extraction per seat); strategic refusers retain arbitrage-grade exit and compute lower. The mixture determines how much of the payer seat''s extraction is amplified by lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refusal_identity_lock, empirical, 'Structural versus identity-constituted basis of sustained refusal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_bap_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.16).
narrative_ontology:measurement(vmb_bap_tr_t3, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 3, 0.18).
narrative_ontology:measurement(vmb_bap_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.21).
narrative_ontology:measurement(vmb_bap_tr_t9, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 9, 0.24).
narrative_ontology:measurement(vmb_bap_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.34).
narrative_ontology:measurement(vmb_bap_tr_t15, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 15, 0.36).
narrative_ontology:measurement(vmb_bap_tr_t18, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 18, 0.37).
narrative_ontology:measurement(vmb_bap_tr_t21, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 21, 0.35).
narrative_ontology:measurement(vmb_bap_tr_t25, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 25, 0.33).

% Extraction over time
narrative_ontology:measurement(vmb_bap_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(vmb_bap_be_t3, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 3, 0.7).
narrative_ontology:measurement(vmb_bap_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.73).
narrative_ontology:measurement(vmb_bap_be_t9, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 9, 0.76).
narrative_ontology:measurement(vmb_bap_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.88).
narrative_ontology:measurement(vmb_bap_be_t15, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 15, 0.87).
narrative_ontology:measurement(vmb_bap_be_t18, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 18, 0.86).
narrative_ontology:measurement(vmb_bap_be_t21, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 21, 0.85).
narrative_ontology:measurement(vmb_bap_be_t25, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 25, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(vmb_bap_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(vmb_bap_su_t3, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(vmb_bap_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(vmb_bap_su_t9, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(vmb_bap_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(vmb_bap_su_t15, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(vmb_bap_su_t18, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(vmb_bap_su_t21, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(vmb_bap_su_t25, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 25, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, resource_allocation).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate debate' decomposes, per the epsilon-invariance principle, into three readings of one kernel, each a separate story with its own epsilon, victim set, and classification over the SHARED referent of standing mandate regimes. This file is the bodily_autonomy_primary member (epsilon 0.84, victims = the coerced). Siblings: vaccine_mandate_balance__public_health_primary (historically dominant; low epsilon; doctrinal victories in the Jacobson lineage shape the operating environment of the other two, making it the upstream member) and vaccine_mandate_balance__proportionality_reading (intermediate epsilon; partitions instruments into proportional and disproportionate subclasses). Every member links the others via affects_constraints; the decomposition note appears in all three files. The epsilon differences are reading-indexed values over a fixed referent (OQ-26), not observables switching within one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
