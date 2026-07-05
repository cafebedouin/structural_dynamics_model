% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Common Article 3 Contextual-Necessity Reading (Enhanced Interrogation Discretion)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the 'contextual necessity' reading of the
 *   Common Article 3 kernel: the baseline of humane treatment persists as a
 *   default, but security agencies retain discretion to override it when they
 *   classify a situation as imminently threatening national security. Under
 *   this reading, 'humane' is not a fixed floor but a determination made by
 *   the same institution that benefits from a permissive determination. The
 *   individual-level coercion at the detainee seat rises sharply over the
 *   interval (accessibility_collapse and suppression both climb toward the
 *   high end) while institutional-level resistance from courts and
 *   international bodies grows more slowly and lags behind — the classic
 *   asymmetry of a necessity override: the party being reclassified has no
 *   voice in the reclassification, and review arrives, if at all, years after
 *   the operative harm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.68).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.71).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Common Article 3 Contextual-Necessity Reading (Enhanced Interrogation Discretion)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '81311c05-9831-4f29-8c8e-d84f65dcf329').
narrative_ontology:cs_kernel_codification('81311c05-9831-4f29-8c8e-d84f65dcf329', fixed_text).
narrative_ontology:cs_authority_grounding('81311c05-9831-4f29-8c8e-d84f65dcf329', extraction).
narrative_ontology:cs_interpretation_layer_present('81311c05-9831-4f29-8c8e-d84f65dcf329').
narrative_ontology:cs_reading_relation('81311c05-9831-4f29-8c8e-d84f65dcf329', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('81311c05-9831-4f29-8c8e-d84f65dcf329', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('81311c05-9831-4f29-8c8e-d84f65dcf329', foundational, necessity_can_override_baseline_protection).
narrative_ontology:cs_axiom_status(necessity_can_override_baseline_protection, holdable).
narrative_ontology:cs_axiom_grounding('81311c05-9831-4f29-8c8e-d84f65dcf329', necessity_can_override_baseline_protection, instrumental).
narrative_ontology:cs_axiom('81311c05-9831-4f29-8c8e-d84f65dcf329', secondary, executing_agency_holds_first_line_interpretive_discretion).
narrative_ontology:cs_axiom_status(executing_agency_holds_first_line_interpretive_discretion, holdable).
narrative_ontology:cs_axiom_grounding('81311c05-9831-4f29-8c8e-d84f65dcf329', executing_agency_holds_first_line_interpretive_discretion, conventional).
narrative_ontology:cs_reference_frame('81311c05-9831-4f29-8c8e-d84f65dcf329', geneva_conventions_common_article_3_baseline).
narrative_ontology:cs_drift_state('81311c05-9831-4f29-8c8e-d84f65dcf329', post_war_on_terror_declassification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('81311c05-9831-4f29-8c8e-d84f65dcf329', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, executive_branch_officials).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, interrogation_program_contractors).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainee_family_members).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, rank_and_file_interrogators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, rank_and_file_interrogators).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, national_security_primacy_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, necessity_defense_in_wartime_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as 'humane' in practice through internal legal memoranda and classified guidance, then administers the interrogation program under that definition. Can invoke necessity findings that reclassify specific detainees or techniques whenever an operational case is made. Faces almost no external verification of its own necessity determinations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Authorizes the necessity override through legal opinions and policy directives, gaining operational flexibility and political cover ('we did everything necessary to prevent an attack') without personally bearing the treatment costs imposed on detainees.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, executive_branch_officials, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, executive_branch_officials, agenda_setter).

% Provides specialized interrogation training and personnel under contract, compensated for techniques that would be unavailable under an absolute-prohibition reading. Can exit the contract relationship if the legal framework shifts; the framework's persistence is a revenue condition, not a survival condition, for this seat.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, interrogation_program_contractors, beneficiary,
    organized, biographical, mobile, national).

% Held incommunicado and subjected to techniques justified as necessary by the agency's own classified necessity finding. Has no access to independent review of that finding, no counsel present during interrogation, and no exit from the facility. The 'context-dependent' standard is applied to them by the same party that benefits from applying it expansively.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, immediate, trapped, local).

% Typically unaware of detention location, treatment, or legal status. Would object to the necessity override's application to their relative but have no standing, no information, and no forum in which to be heard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainee_family_members, excluded,
    powerless, biographical, trapped, local).

% Ordered to apply techniques authorized under the necessity finding, absorbing legal and psychological liability if the classification is later reversed by courts or successor administrations. Some individuals benefit professionally from participation (promotion, specialized pay) while bearing the downstream legal exposure the agency that issued the order does not.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, rank_and_file_interrogators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, rank_and_file_interrogators, beneficiary).

% Would assess the necessity finding against Common Article 3's non-derogable core but are denied access to detention facilities, classified legal opinions, and interrogation records. Their objections are documented in reports but carry no enforcement mechanism against the agencies making the determinations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% Periodically reviews habeas petitions and classification disputes years after the fact, sometimes ordering disclosure or finding techniques unlawful in retrospect. Its review is structurally delayed past the period in which the necessity finding does its operative work.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, domestic_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides interrogation agencies a workable operational standard that avoids the paralysis of an absolute rule in scenarios agencies characterize as imminent-threat, by letting the executing institution itself determine when the baseline yields to necessity.
% TRANSFER_FUNCTION: Moves discretion over detainee treatment from a fixed, externally-verifiable minimum standard to the security agency administering detention; moves legal and physical risk from the authorizing institutions to the detainees subjected to reclassified treatment and to the interrogators who carry out the orders.
% ABSENT_VOICES: Detainees, their families, and international human rights bodies are structurally excluded from the necessity determination itself — the classification that would strip them of protection is made without their participation, often without their knowledge, and reviewed (if at all) only after the treatment has occurred.
% DISAPPEARANCE_RATIONALE: If the contextual-necessity override disappeared and Common Article 3's baseline applied without exception, security agencies would lose the legal instrument that currently authorizes enhanced interrogation programs; detention practices, legal defense postures for personnel, and contractor relationships built around 'lawful enhanced technique' categories would have to be dismantled or renegotiated entirely.
% FOUNDING_PROBLEM: Interrogators facing what they characterized as imminent, catastrophic threats (ticking-bomb scenarios) argued that a rigid absolute prohibition would forbid using available intelligence-gathering methods even when lives were plausibly at stake, and sought a legal mechanism to authorize otherwise-prohibited techniques in defined emergencies.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and the officials who authorized the programs attest the necessity scenarios were real and the override was operationally required. Independent post-hoc investigations (legislative committee reports, released declassified findings, and international human rights body assessments — all outside the benefiting institutions) attest that documented invocations of the necessity override in practice extended far beyond any genuine imminent-threat scenario, and that no rigorous evidence emerged that the techniques produced intelligence unobtainable by lawful means.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.71) are high because the override transfers discretion from a verifiable external standard to an interested internal actor, and that transfer is maintained through classification, incommunicado detention, and delayed judicial review — active suppression mechanisms, not passive drift. Theater ratio (0.42) reflects that a substantial share of the apparatus (legal memoranda, internal 'humane treatment' compliance reviews) functions to document the necessity finding's legitimacy rather than to constrain the treatment itself. Accessibility collapse (0.58) and resistance (0.55) are mid-range rather than mountain-like, because this is a constructed legal reading contested by courts, treaty bodies, and dissenting officials within the same agencies — alternatives (the absolute-prohibition reading) are actively argued, not foreclosed by nature.
 *
 * PERSPECTIVAL GAP:
 *   From the security-agency seat, this reading looks like necessary operational flexibility — a rope solving the ticking-bomb coordination problem between intelligence needs and legal constraint. From the high-value-detainee seat, the identical structure is enforced extraction: a classification made by an interested party, applied without their participation, with no independent check during the period it matters. The engine should compute these seats to diverge sharply because the directionality inputs diverge sharply: the agenda-setter seat has arbitrage-grade exit and full authorship of the classification; the payer seat is trapped and has zero voice in the classification that governs it.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies and executive officials are declared beneficiaries: they gain the operational tool and bear none of the physical cost. Interrogation contractors are declared beneficiaries with mobile exit — the arrangement is a revenue opportunity they could exit if it collapsed, not an identity or survival dependency. High-value detainees are declared victims with trapped exit — they cannot leave, appeal, or contest the necessity finding that governs their treatment. Rank-and-file interrogators sit dual-positioned: they benefit professionally from participation but pay in downstream legal and psychological exposure the authorizing institutions do not share, which is why they carry both payer and beneficiary roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imminent-threat scenarios outstripping the absolute rule's flexibility) is contested rather than dead or clearly live: proponents maintain genuine ticking-bomb scenarios still occur; independent investigations found that documented invocations routinely extended to non-imminent, non-catastrophic interrogation contexts. This is precisely the mismatch the R5 genealogy interview is built to surface — founding_problem_status is authored as contested, and the disappearance_verdict (world_rearranges) shows real institutional dependency exists regardless of whether the founding justification still holds. Classifying this as tangled_rope rather than snare or rope preserves the fact that a genuine coordination function (avoiding legal paralysis in truly exigent cases) exists alongside a genuine, asymmetric extraction machinery (detainees who cannot contest the classification that governs them) — collapsing either the coordination or the extraction component would mislabel the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_reviewability,
    'Should the security agency''s necessity determination be subject to real-time, independent (non-agency) review before the override takes effect, or is post-hoc judicial review sufficient to preserve the baseline''s protective function?',
    'Comparative analysis of detention regimes that require contemporaneous independent authorization (e.g., judicial warrant models) versus those relying solely on retrospective habeas review, measured against documented rates of technique-use outside genuinely imminent scenarios.',
    'If contemporaneous independent review is structurally required for the override to remain within IHL''s protective purpose, this reading collapses toward the proportionality_balancing reading; if post-hoc review is accepted as sufficient, this reading remains a distinct, stable structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_determination_reviewability, conceptual, 'Whether contemporaneous independent review is a structural requirement of a lawful necessity override, or whether this reading can stand on retrospective review alone.').

omega_variable(
    genuine_versus_manufactured_necessity,
    'In the documented historical record, what proportion of invoked necessity findings corresponded to genuine imminent-threat scenarios versus routine interrogation reclassified under the necessity label for operational convenience?',
    'Declassification and independent audit of necessity-finding case files against contemporaneous intelligence assessments, comparing the threat characterization made at authorization time to what was later established.',
    'A high proportion of manufactured or retroactively-unsupported findings would support reclassifying this reading''s effective operation as snare rather than tangled_rope — i.e., that the coordination story is cover rather than a genuine, if contested, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_versus_manufactured_necessity, empirical, 'Whether the necessity override''s real-world invocation record supports a genuine coordination function or is predominantly extraction dressed as necessity.').

omega_variable(
    kernel_reading_selection_authority,
    'Who has the legitimate authority to select among the absolute_prohibition, contextual_necessity, and proportionality_balancing readings of Common Article 3 for a given state''s practice — treaty interpretation bodies, domestic courts, or the executing security agency itself?',
    'Analysis of treaty interpretation doctrine (VCLT Article 31-32 practice) and comparative state practice on who has historically been treated as the authoritative interpreter of Common Article 3''s ambiguity.',
    'If security agencies lack legitimate interpretive authority over their own governing standard, the contextual_necessity reading''s operation is itself an unauthorized self-authorization, strengthening the case that its effective classification should shift toward snare; if agencies are recognized as legitimate first-line interpreters subject to later review, tangled_rope remains the better fit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'Whether the security agency invoking this reading has legitimate interpretive authority over the kernel, or is exercising authority it was never granted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t4, humane_treatment_standard__contextual_necessity, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(huma_tr_t4, observed).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__contextual_necessity, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(huma_tr_t8, observed).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(huma_tr_t12, observed).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__contextual_necessity, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(huma_tr_t16, observed).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__contextual_necessity, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(huma_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t4, humane_treatment_standard__contextual_necessity, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(huma_be_t4, observed).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__contextual_necessity, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(huma_be_t8, observed).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(huma_be_t12, observed).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__contextual_necessity, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(huma_be_t16, observed).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__contextual_necessity, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(huma_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t4, humane_treatment_standard__contextual_necessity, suppression_requirement, 4, 0.6).
narrative_ontology:measurement_basis(huma_su_t4, observed).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__contextual_necessity, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(huma_su_t8, observed).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(huma_su_t12, observed).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__contextual_necessity, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(huma_su_t16, observed).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__contextual_necessity, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(huma_su_t24, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=24
narrative_ontology:measurement(huma_grid_01, humane_treatment_standard__contextual_necessity, accessibility_collapse(class), 0, 0.3).
narrative_ontology:measurement(huma_grid_02, humane_treatment_standard__contextual_necessity, accessibility_collapse(class), 24, 0.6).
narrative_ontology:measurement(huma_grid_03, humane_treatment_standard__contextual_necessity, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(huma_grid_04, humane_treatment_standard__contextual_necessity, accessibility_collapse(individual), 24, 0.75).
narrative_ontology:measurement(huma_grid_05, humane_treatment_standard__contextual_necessity, accessibility_collapse(organizational), 0, 0.2).
narrative_ontology:measurement(huma_grid_06, humane_treatment_standard__contextual_necessity, accessibility_collapse(organizational), 24, 0.45).
narrative_ontology:measurement(huma_grid_07, humane_treatment_standard__contextual_necessity, accessibility_collapse(structural), 0, 0.4).
narrative_ontology:measurement(huma_grid_08, humane_treatment_standard__contextual_necessity, accessibility_collapse(structural), 24, 0.58).
narrative_ontology:measurement(huma_grid_09, humane_treatment_standard__contextual_necessity, resistance(class), 0, 0.25).
narrative_ontology:measurement(huma_grid_10, humane_treatment_standard__contextual_necessity, resistance(class), 24, 0.5).
narrative_ontology:measurement(huma_grid_11, humane_treatment_standard__contextual_necessity, resistance(individual), 0, 0.15).
narrative_ontology:measurement(huma_grid_12, humane_treatment_standard__contextual_necessity, resistance(individual), 24, 0.1).
narrative_ontology:measurement(huma_grid_13, humane_treatment_standard__contextual_necessity, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(huma_grid_14, humane_treatment_standard__contextual_necessity, resistance(organizational), 24, 0.55).
narrative_ontology:measurement(huma_grid_15, humane_treatment_standard__contextual_necessity, resistance(structural), 0, 0.3).
narrative_ontology:measurement(huma_grid_16, humane_treatment_standard__contextual_necessity, resistance(structural), 24, 0.55).
narrative_ontology:measurement(huma_grid_17, humane_treatment_standard__contextual_necessity, stakes_inflation(class), 0, 0.3).
narrative_ontology:measurement(huma_grid_18, humane_treatment_standard__contextual_necessity, stakes_inflation(class), 24, 0.55).
narrative_ontology:measurement(huma_grid_19, humane_treatment_standard__contextual_necessity, stakes_inflation(individual), 0, 0.4).
narrative_ontology:measurement(huma_grid_20, humane_treatment_standard__contextual_necessity, stakes_inflation(individual), 24, 0.85).
narrative_ontology:measurement(huma_grid_21, humane_treatment_standard__contextual_necessity, stakes_inflation(organizational), 0, 0.25).
narrative_ontology:measurement(huma_grid_22, humane_treatment_standard__contextual_necessity, stakes_inflation(organizational), 24, 0.5).
narrative_ontology:measurement(huma_grid_23, humane_treatment_standard__contextual_necessity, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(huma_grid_24, humane_treatment_standard__contextual_necessity, stakes_inflation(structural), 24, 0.5).
narrative_ontology:measurement(huma_grid_25, humane_treatment_standard__contextual_necessity, suppression(class), 0, 0.35).
narrative_ontology:measurement(huma_grid_26, humane_treatment_standard__contextual_necessity, suppression(class), 24, 0.6).
narrative_ontology:measurement(huma_grid_27, humane_treatment_standard__contextual_necessity, suppression(individual), 0, 0.5).
narrative_ontology:measurement(huma_grid_28, humane_treatment_standard__contextual_necessity, suppression(individual), 24, 0.85).
narrative_ontology:measurement(huma_grid_29, humane_treatment_standard__contextual_necessity, suppression(organizational), 0, 0.3).
narrative_ontology:measurement(huma_grid_30, humane_treatment_standard__contextual_necessity, suppression(organizational), 24, 0.55).
narrative_ontology:measurement(huma_grid_31, humane_treatment_standard__contextual_necessity, suppression(structural), 0, 0.45).
narrative_ontology:measurement(huma_grid_32, humane_treatment_standard__contextual_necessity, suppression(structural), 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.1).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the humane_treatment_standard kernel. absolute_prohibition treats Common Article 3 as non-derogable (near-mountain, minimal beneficiary structure); contextual_necessity (this story) treats the baseline as override-able by agency necessity determination (tangled_rope, substantial extraction concentrated on trapped detainees); proportionality_balancing occupies a structured middle position (a bounded balancing test rather than unilateral discretion). Each carries its own epsilon and its own beneficiary/victim structure; they are not the same constraint viewed three ways — the reading determines who holds discretion, and that determination changes the victim set (contextual_necessity narrows the protected class to exclude anyone the agency itself deems a necessity case).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
