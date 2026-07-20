% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Moral Status â Welfare Reading (Sentient-Use Framework)
 *   domain: applied ethics / animal studies / legal philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the animal moral
 *   status kernel: animals are sentient, suffering should be minimized, and
 *   regulated use is permissible. It is presented as a coordination mechanism
 *   that prevents cruelty while permitting economically and socially valuable
 *   use. The structural reality is that it simultaneously coordinates public
 *   trust and industry practice while asymmetrically extracting suffering
 *   from animals who remain property. The claim is tangled_rope; the metrics
 *   are authored independently to reflect moderate extraction, moderate
 *   suppression, and rising theater as 'humane' labels are increasingly
 *   decoupled from animal experience.
 *
 * KEY AGENTS:
 *   - regulated_industries: Primary beneficiary (powerful/constrained) â gains social license and continued market access
 *   - welfare_organizations: Secondary beneficiary/agenda-setter (organized/constrained) â gains legitimacy by working inside the use frame
 *   - animals_in_regulated_use: Primary target (powerless/trapped) â bears suffering under 'humane' protocols
 *   - abolitionist_advocates: Excluded voice (moderate/mobile) â rejects use premise, kept off policy tables
 *   - animal_welfare_regulators: Agenda-setter (institutional/constrained) â enforces the welfare floor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.4).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.5).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Moral Status â Welfare Reading (Sentient-Use Framework)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied ethics / animal studies / legal philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '34d5186d-6c61-4d8b-94ac-c7f13d405881').
narrative_ontology:cs_kernel_codification('34d5186d-6c61-4d8b-94ac-c7f13d405881', formalized).
narrative_ontology:cs_authority_grounding('34d5186d-6c61-4d8b-94ac-c7f13d405881', lineage).
narrative_ontology:cs_interpretation_layer_present('34d5186d-6c61-4d8b-94ac-c7f13d405881').
narrative_ontology:cs_reading_relation('34d5186d-6c61-4d8b-94ac-c7f13d405881', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('34d5186d-6c61-4d8b-94ac-c7f13d405881', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('34d5186d-6c61-4d8b-94ac-c7f13d405881', foundational, sentience_commands_welfare_duty).
narrative_ontology:cs_axiom_status(sentience_commands_welfare_duty, holdable).
narrative_ontology:cs_axiom_grounding('34d5186d-6c61-4d8b-94ac-c7f13d405881', sentience_commands_welfare_duty, deontological).
narrative_ontology:cs_axiom('34d5186d-6c61-4d8b-94ac-c7f13d405881', foundational, regulated_use_morally_permissible).
narrative_ontology:cs_axiom_status(regulated_use_morally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('34d5186d-6c61-4d8b-94ac-c7f13d405881', regulated_use_morally_permissible, conventional).
narrative_ontology:cs_reference_frame('34d5186d-6c61-4d8b-94ac-c7f13d405881', regulated_use_with_welfare_floor).
narrative_ontology:cs_drift_state('34d5186d-6c61-4d8b-94ac-c7f13d405881', contemporary_abolitionist_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('34d5186d-6c61-4d8b-94ac-c7f13d405881', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumer_public).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate animal agriculture, research, and entertainment within welfare-regulated boundaries. Receive continued social license and market access because the welfare frame legitimizes use. Exit to unregulated practice risks public backlash; exit to plant-based models threatens sunk capital.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_industries, beneficiary,
    powerful, biographical, constrained, global).

% Monitor facilities, campaign for higher standards, and collect donations and institutional standing within the use-permissible frame. Their legitimacy depends on accepting that use itself is not the problem. Challenging use would mean losing their seat at regulatory tables.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, welfare_organizations, agenda_setter).

% Draft and enforce anti-cruelty and husbandry standards. Operate under legislative mandates that presuppose permissible use. Can raise or lower standards but cannot abolish the underlying industries without statutory overhaul.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Sentient beings in farms, laboratories, and entertainment systems. Subject to practices labeled 'humane' that still involve confinement, mutilation without anesthesia, and slaughter. No exit from the system; their interests are represented only through proxy welfare metrics.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, local).

% Reject all animal use as exploitation. Excluded from policy-making bodies that treat use as a settled background condition. Their arguments are treated as extremist or impractical within the regulatory window, so their voice does not shape the standard-setting process.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, biographical, mobile, national).

% Purchase animal products with the moral comfort that welfare regulations prevent cruelty. Alternatives exist but require active lifestyle change; most remain in the default system.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumer_public, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a stable social equilibrium among animal-using industries, consumers, and reform advocates by establishing a normative floor that permits continued use while promising to minimize suffering.
% TRANSFER_FUNCTION: Moves social license and economic continuity to regulated industries, institutional legitimacy and funding to welfare organizations, and moral comfort to consumers; transfers suffering, confinement, and death to animals under 'humane use' protocols.
% ABSENT_VOICES: Abolitionist advocates who regard all use as inherent violation are structurally excluded from regulatory tables that presuppose use-permissibility. Their objections are treated as outside the actionable policy window.
% DISAPPEARANCE_RATIONALE: If the welfare-use framework vanished overnight, the social license sustaining mass animal industries would fracture. Markets would polarize between abolitionist pressure and a race-to-the-bottom in unregulated cruelty, and the current compromise equilibrium would collapse.
% FOUNDING_PROBLEM: Unregulated industrial and urban animal use produced visible cruelty and public revulsion during early modernization, threatening social stability and industry legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians and sociologists document 19th- and 20th-century cruelty crises that prompted statutory welfare laws. Abolitionist scholars and some ethicists outside the beneficiary set contest that the founding problem was solved; they argue welfare regulation perpetuates the deeper problem of property status and functions as legitimation.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.40) is low-to-moderate because the constraint does reduce some suffering relative to unregulated use, but the industrial scale of use means massive aggregate extraction persists. Suppression (0.50) reflects the active exclusion of abolitionist frames from policy and the legal reinforcement of use-permissibility. Theater ratio (0.45) captures the growing gap between welfare marketing ('humane,' 'cruelty-free') and actual practices. Accessibility collapse (0.55) indicates that while abolitionist alternatives exist, they are culturally and economically marginalized. Resistance (0.45) comes from abolitionist movements and from some industry sectors resisting compliance costs. The measurement series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (regulators, industries, welfare organizations) experience the constraint as a genuine and necessary coordination device that prevents worse outcomes. The payer seat (animals) experiences only the extraction. The excluded seat (abolitionists) experiences the constraint as an illegitimate legitimization of violence. The engine computes divergent per-seat types from this structural asymmetry; the authored claim does not resolve the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulated industries and consumer public are structural beneficiaries (low d): they receive social license, product availability, and moral comfort. Welfare organizations are secondary beneficiaries with moderate-low d because their standing is contingent on the constraint. Animals are full targets (high d): they bear the costs of confinement and death with no exit. Abolitionists are not in the derivation chain for d because they are excluded from the constraint's operation rather than governed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by its explicit victim set and active enforcement requirement. Without the victim set, it might compute as a rope (pure coordination). Without the coordination function (genuine cruelty reduction relative to no law), it would be a snare. The founding problem is contested rather than dead, so piton classification is not warranted despite the moderate theater ratio. The mandate has not fully atrophied because the coordination function still partially works, but the asymmetric extraction is structurally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_vs_rights_framing,
    'Does sentience ground a duty to minimize suffering within use, or a right against use that makes all instrumentalization a violation?',
    'Comparative jurisprudential analysis and neuroethics research establishing whether welfare metrics can ever be sufficient for sentient interests.',
    'If sentience generates rights against use, this constraint is a snare legitimizing extraction; if it generates only welfare duties, the constraint is a rope or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_rights_framing, conceptual, 'Whether the normative implication of sentience is welfare or rights.').

omega_variable(
    industry_capture_of_standards,
    'Are welfare standards and inspection regimes genuinely protective, or have they been captured by regulated industries to minimize cost and maximize social license?',
    'Cross-jurisdictional comparison of welfare outcomes against industry lobbying expenditure; whistleblower and undercover audit data.',
    'If capture is high, the coordination story is cover and effective extraction is higher than authored; this would push classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_of_standards, empirical, 'Regulatory capture ambiguity in welfare standard-setting.').

omega_variable(
    suffering_quantification_in_use,
    'Can suffering be adequately measured and minimized within systems that retain slaughter, confinement, and invasive research as core practices?',
    'Integration of nociception and ethology research with welfare audit data; longitudinal health and behavioral metrics in regulated facilities.',
    'If use itself precludes welfare, the constraint''s coordination claim is false and the extraction is structural rather than incidental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suffering_quantification_in_use, empirical, 'Whether humane use is an oxymoron under empirical scrutiny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anwelf_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anwelf_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(anwelf_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(anwelf_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(anwelf_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(anwelf_tr_t50, animal_moral_status__welfare_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(anwelf_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anwelf_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(anwelf_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(anwelf_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(anwelf_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(anwelf_be_t50, animal_moral_status__welfare_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(anwelf_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anwelf_su_t10, animal_moral_status__welfare_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(anwelf_su_t20, animal_moral_status__welfare_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(anwelf_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(anwelf_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(anwelf_su_t50, animal_moral_status__welfare_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three structurally distinct readings. This welfare reading instantiates the regulated-use-with-suffering-minimization claim. The abolitionist reading treats all use as violation. The property reading treats animals as bare resources. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
