% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality-Scaled Coercion Legitimacy Boundary (Measles/Flu Distinction)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   A proportionality-based boundary distinguishes legitimate coercive public
 *   health authority from illegitimate overreach: diseases with high
 *   transmission (R₀ > ~10), high mortality (CFR > ~0.1%), and no effective
 *   non-coercive alternatives (measles, polio, diphtheria pre-vaccine
 *   coverage) justify mandates; diseases with lower severity (seasonal
 *   influenza, endemic coronavirus) do not. The boundary is CLAIMED as
 *   tangled_rope (coordination of herd immunity + constraint on refusers) but
 *   the measurement profile shows substantial extractiveness (0.58 terminal)
 *   and rising theater (0.18→0.41) as disease threat recedes and
 *   institutional authority persists. This constraint is one reading of a
 *   contested kernel: bodily_autonomy_primary reading would forbid all
 *   coercion; public_health_primary reading would allow coercion for any
 *   collective benefit; this proportionality reading attempts to split the
 *   difference via threshold. The claim/metric gap reflects the reading's
 *   central tension: it claims to be a limit on coercion, but the metrics
 *   show coercion mechanisms intensifying as the founding disease threat
 *   declines.
 *
 * KEY AGENTS:
 *   - disease_control_authorities: Institutional agenda-setter; operates the epidemiological calculus that sets the proportionality boundary; high exit from coercion authority
 *   - vaccine_refusers_high_severity_pathogen: Powerless targets for high-R₀/high-CFR diseases (measles, polio); trapped exit; subject to full coercive apparatus
 *   - vaccine_refusers_low_severity_pathogen: Moderate-power refusers for lower-severity diseases; constrained exit; experience reduced coercion by proportionality logic but elevated social suppression
 *   - population_with_herd_immunity_threshold_protection: Organized beneficiaries; enjoy coercion-purchased immunity; do not bear coercion cost
 *   - medical_autonomy_advocates: Excluded from the proportionality logic; argue categorical bodily-autonomy protection
 *   - epidemiological_measurement_institutions: Observer seat; provide the empirical inputs (R₀, CFR) that the boundary rides on; their measurements are contestable and create discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.58).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.72).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality-Scaled Coercion Legitimacy Boundary (Measles/Flu Distinction)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '3b45a680-e6c9-41bd-a72e-7833af14d102').
narrative_ontology:cs_kernel_codification('3b45a680-e6c9-41bd-a72e-7833af14d102', fixed_text).
narrative_ontology:cs_authority_grounding('3b45a680-e6c9-41bd-a72e-7833af14d102', lineage).
narrative_ontology:cs_interpretation_layer_present('3b45a680-e6c9-41bd-a72e-7833af14d102').
narrative_ontology:cs_reading_relation('3b45a680-e6c9-41bd-a72e-7833af14d102', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('3b45a680-e6c9-41bd-a72e-7833af14d102', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_axiom('3b45a680-e6c9-41bd-a72e-7833af14d102', foundational, coercion_requires_proportionality_threshold).
narrative_ontology:cs_axiom_status(coercion_requires_proportionality_threshold, holdable).
narrative_ontology:cs_axiom_grounding('3b45a680-e6c9-41bd-a72e-7833af14d102', coercion_requires_proportionality_threshold, deontological).
narrative_ontology:cs_axiom('3b45a680-e6c9-41bd-a72e-7833af14d102', foundational, severity_transmission_distinguish_legitimate_coercion).
narrative_ontology:cs_axiom_status(severity_transmission_distinguish_legitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('3b45a680-e6c9-41bd-a72e-7833af14d102', severity_transmission_distinguish_legitimate_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('3b45a680-e6c9-41bd-a72e-7833af14d102', proportionality_constrained_coercion).
narrative_ontology:cs_drift_state('3b45a680-e6c9-41bd-a72e-7833af14d102', contemporary_endemic_disease_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b45a680-e6c9-41bd-a72e-7833af14d102', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, disease_control_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, population_with_herd_immunity_threshold_protection).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusers_high_severity_pathogen).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusers_low_severity_pathogen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusers_low_severity_pathogen).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, proportionality_principle_constitutional_law).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, harm_principle_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Health agencies and legislatures that set vaccination mandates based on disease severity, transmission rate (R₀), and mortality data. They claim the authority to coerce vaccination when collective harm prevention meets a proportionality threshold: high-R₀, high-CFR diseases (measles, polio) justify mandates; lower-severity diseases (seasonal flu, endemic coronavirus) do not. They operate the epidemiological calculus that defines the boundary and enforce it through vaccine requirements for school attendance, employment, or travel.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, disease_control_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who refuse vaccination for high-mortality, high-transmission diseases (e.g., measles pre-vaccine era CFR ~0.2%, R₀ ~12–18). They experience coercion: legal mandate, school/employment exclusion, or quarantine; refusal costs them participation in civil society. Their exit options are severely constrained — migration to non-requiring jurisdictions is expensive and rare; identity-fusion with anti-vaccination ideology compounds the trap. The constraint defines them as legitimate extraction targets under the proportionality logic.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusers_high_severity_pathogen, payer,
    powerless, biographical, trapped, national).

% Individuals who refuse vaccination for lower-severity diseases (seasonal flu IFR ~0.1% in general population, R₀ ~1.3; endemic coronavirus post-2022 IFR ~0.03–0.1%). Under proportionality framing, they experience LESS coercion or none: mandates are not justified by the pathogen's severity profile, so refusal carries no legal penalty in many jurisdictions. However, they remain subject to social pressure (employer incentives, healthcare setting restrictions, travel friction) and receive reduced protection if herd immunity is not achieved. The boundary places them just outside legitimacy, but institutional inertia and persistent social suppression keep friction high.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusers_low_severity_pathogen, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusers_low_severity_pathogen, beneficiary).

% People whose infection risk is reduced by high vaccination coverage (herd immunity threshold ~95% for measles, ~40–60% for seasonal flu depending on variant). They benefit from the coercive mandates applied to refusers when coverage is high enough; they also benefit from the refusal-allowance for low-severity diseases because population-level outcomes remain acceptable. They do not directly bear the coercion cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, population_with_herd_immunity_threshold_protection, beneficiary,
    organized, generational, mobile, national).

% Physicians, bioethicists, and patient advocates who argue that bodily autonomy is categorically inviolable regardless of collective benefit. They are structurally excluded from the proportionality calculus: the constraint's logic presupposes that collective harm can outweigh individual refusal when severity is sufficient. They would argue that ANY coercion is a category error and that the boundary should be set at zero coercion. Their voice appears in testimony and ethical debate but does not shape the constraint's enforcement.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, medical_autonomy_advocates, excluded,
    moderate, biographical, constrained, national).

% CDC, WHO, national health agencies, and disease surveillance systems that measure R₀, case fatality rates, transmission dynamics, and vaccine efficacy. They provide the empirical inputs that the proportionality boundary rides on. Their measurements are treated as objective facts by the agenda-setters but are themselves contestable: R₀ estimates shift, severity reassessments change threshold placement, and measurement uncertainty creates discretion in where the boundary sits.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, epidemiological_measurement_institutions, observer,
    institutional, generational, analytical, global).

% Elected and appointed bodies that codify the proportionality boundary into law and adjudicate whether specific diseases clear the threshold. They are formally accountable and can revise the boundary when science or politics shifts, but they are also politically constrained: during epidemiological crises, legislatures often defer to health agencies, and courts have traditionally used rational-basis review for public health mandates, which is extremely deferential.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, disease_control_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves population immunity thresholds for severe communicable diseases by mandating vaccination when the disease's severity, transmission rate, and population mortality risk exceed a proportionality threshold. Solves the collective-action problem of free-ridership on herd immunity: individuals benefit from high coverage without personal risk if they refuse, creating underinvestment in vaccination unless mandates align incentives.
% TRANSFER_FUNCTION: Moves bodily autonomy and choice from vaccine refusers to disease control authorities and the protected population, but only for pathogens meeting the severity threshold. Refusers bear coercion (legal mandate, exclusion, quarantine) for high-R₀/high-mortality diseases; lower-severity diseases do not carry the same coercive transfer (the boundary is the point of the constraint).
% ABSENT_VOICES: Vaccine refusers are present as payers but have no seat at agenda-setting. Medical autonomy advocates (bioethicists, patient autonomy movements) and bodily-autonomy-primary legal traditions are structurally excluded from the proportionality calculus — they argue the entire boundary is illegitimate, not that it is drawn in the wrong place. Historically marginalized communities with justified medical distrust are present as de facto payers but typically lack institutional advocacy seats.
% DISAPPEARANCE_RATIONALE: If the proportionality-scaled boundary vanished and coercion authority reverted to case-by-case negotiation or zero-coercion defaults, vaccination coverage for high-severity diseases would drop significantly (historical analogues: pre-mandate measles vaccination was ~50% US coverage, post-mandate rose to ~95%+). Population immunity thresholds would be breached for measles, polio, and diphtheria in many jurisdictions; outbreaks would recur in previously protected populations. Herd immunity benefits would collapse; the constraint is constitutive of the current disease control regime.
% FOUNDING_PROBLEM: In the 20th century, endemic measles, polio, diphtheria, and pertussis caused hundreds of thousands of deaths annually and severe long-term disability; vaccination alone did not achieve sufficient coverage to eliminate transmission because of free-riding, vaccine hesitancy, and access barriers. The proportionality-scaled coercion boundary was constructed to solve this collective-action problem: make vaccination mandatory for high-risk diseases when the collective harm from non-vaccination demonstrably exceeds the autonomy cost to individuals.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest that high-severity diseases remain live threats and that mandates are still necessary for herd immunity maintenance. However, epidemiologists and policy analysts outside the benefiting (disease control) camp increasingly attest that the founding problem — uncontrolled measles, polio transmission — is solved (measles eliminated in the Americas; polio near-eradicated globally). The status is contested because the founding problem is PARTIALLY solved: for measles, polio, the founding emergency is historical; for newly emerged pathogens (mpox, novel influenza), the problem reasserts. Legislative hearings and independent epidemiological analysis show disagreement over whether the constraint's persistent operation is a response to live threat or institutional inertia.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).

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
 *   The extractiveness trajectory (0.35→0.58) reflects the constraint's maturation and institutional persistence as founding disease threats recede historically. Early in the interval (t=0–5, measles and polio as active large-scale threats), extractiveness is moderate because the proportionality logic is genuinely constraining and broadly accepted. As diseases are eliminated or become rare (t=10–15, measles declared eliminated in the Americas, polio near-eradicated globally), the founding justification weakens but institutional authority persists and extends to lower-severity pathogens (pandemic influenza response, mpox response, endemic coronavirus mandates). Theater rises sharply (0.18→0.41) as the constraint's operation becomes increasingly performative — security reviews and procedural trappings (epidemiological committees, risk modeling) defend mandates for diseases where the threat is attenuated and discretion is high. Suppression plateaus (0.58→0.72) because the core coercive machinery (legal mandate, exclusion, quarantine) requires steady enforcement to maintain coverage as voluntary compliance declines and alternative vaccines or treatments become available. The measurements show a constraint in drift: the proportionality reading is being tested and potentially breached as authorities apply mandates to lower-severity pathogens and institutional inertia sustains coercion even where the proportionality threshold is not clearly met.
 *
 * PERSPECTIVAL GAP:
 *   From the disease_control_authorities seat, the proportionality boundary is a genuine restraint on their power: they claim they only mandate vaccination when scientific evidence (R₀, CFR) justifies it, and they voluntarily refrain from mandating low-severity diseases. From the vaccine_refusers_low_severity_pathogen seat, the constraint is experienced as arbitrary: they see the same institutional machinery (government coercion, social exclusion, institutional pressure) applied to them as to high-severity refusers, with the scientific justification (lower severity) presented as sufficient restraint but not as protection from coercion. The boundary is experienced as a line that should protect them but does not. The medical_autonomy_advocates seat experiences the constraint as illegitimate at any threshold — the proportionality reading is, from their standpoint, a spurious restraint on what is categorically impermissible. The engine should compute these divergences: the agenda-setter experiences tangled_rope (genuine coordination with acceptable asymmetry); the low-severity refusers compute snare (coercion without justification); the autonomy advocates compute snare (coercion is itself the extraction, not the transfer it is coordinating).
 *
 * DIRECTIONALITY LOGIC:
 *   Disease control authorities as agenda-setters sit at low directionality (d near 0.0, beneficiary end): they set the rules, face no coercion themselves, and collect the benefit of high coverage and disease control. High-severity refusers are full targets (d near 1.0): trapped by the boundary, subject to coercion, powerless to negotiate. Low-severity refusers sit near middle (d ~0.6–0.7): constrained exit and social suppression, but not the direct legal coercion of high-severity refusers; they experience the constraint as asymmetric but claim some legitimacy from the proportionality logic. The herd-immunity-beneficiary population sits at low-to-medium directionality (d ~0.3–0.4): they benefit from high coverage without bearing coercion cost themselves, but they are also slightly constrained by residual transmission risk and social friction. Overrides are not needed: the structural data (beneficiary/victim declarations + power + exit) should derive the directional spread automatically. The spatial scope (national) applies the constraint uniformly across jurisdictions, which might warrant a moderate scope-based amplification of effective extraction, but the core d derivation should track the stakeholder data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status (live/dead/contested) mismatch with disappearance_verdict (world_rearranges) suggests mandatrophy is emerging or already present: the founding problem (uncontrolled measles, polio deaths) is DEAD in many jurisdictions (measles eliminated, polio near-eradicated) but the constraint (the proportionality-scaled coercion authority) persists and is applied to lower-severity diseases where the founding emergency does not apply. The constraint's FUNCTION (disease control) is partly solved; its FORM (coercive authority, mandatory vaccination) persists because institutional inertia, political constituencies, and public expectations have calcified around it. Mandatrophy is NOT resolved (no agent benefits enough to maintain it; no agent is hurt enough to fix it) — disease control authorities benefit from the authority, so they are beneficiaries, not victims. The constraint does not meet the piton profile (beneficiary + inertial operation). Instead, it shows mutation: from genuine tangled_rope (proportionality actually constrains, disease threat is live) toward a state where the proportionality boundary is breached and the constraint operates as extractive authority justified by residual rhetoric rather than current threat (snare or public-health-primary, not proportionality). The theater_ratio rise (0.18→0.41) and suppression persistence (0.72 at interval end) support this reading of mutation, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold_determination,
    'What epidemiological metrics (R₀, CFR, hospitalization rate, long-term disability risk) define the proportionality boundary between coercible and non-coercible diseases? Where is the threshold scientifically and normatively located?',
    'Explicit threshold-setting by health authorities (e.g., CFR > X% and R₀ > Y justify mandates), empirical measurement of outcomes under different threshold placements, and comparative analysis across jurisdictions with different boundary locations.',
    'The boundary is currently implicit and shift-prone; diseases like mpox, novel coronavirus variants, and seasonal influenza sit near the edge and are classified differently across jurisdictions and over time. An explicit, stable threshold would resolve victim-set ambiguity; an implicit threshold creates discretion and allows political/ideological pressure to shift the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_determination, empirical, 'Where the proportionality threshold is scientifically located and how it should be operationalized.').

omega_variable(
    autonomy_override_legitimacy,
    'Does collective benefit (measured in prevented deaths and disabilities) provide legitimate grounds to override individual bodily autonomy, and if so, at what calculus? Is the proportionality reading genuinely distinct from public-health-primary, or does it presuppose the same autonomy-subordination logic?',
    'Constitutional and ethical analysis comparing the proportionality reading with the bodily-autonomy-primary and public-health-primary readings; empirical study of actual coercion mechanisms and their harms; jurisdictional comparative analysis of mandate policies and outcomes.',
    'If override legitimacy is questioned fundamentally, the proportionality reading collapses into the bodily-autonomy-primary reading (zero coercion for any disease) or is reframed as public-health-primary (coercion for collective benefit is always potentially legitimate, no threshold). If override legitimacy is affirmed, the constraint stands but the threshold location remains open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_override_legitimacy, conceptual, 'Whether proportionality-scaled coercion is normatively distinct from blanket autonomy-override or categorical bodily-autonomy protection.').

omega_variable(
    measurement_uncertainty_discretion,
    'Epidemiological measurements of R₀, CFR, and transmission dynamics carry confidence intervals and methodological uncertainty. How much discretion does this uncertainty create in threshold placement, and how is discretion controlled?',
    'Sensitivity analysis of threshold placement to measurement uncertainty ranges; audit of historical threshold shifts and their relationship to new data vs. political pressure; explicit decision rules for discretion resolution (e.g., conservative threshold placement favoring autonomy when uncertain).',
    'If measurement uncertainty is large relative to the threshold location (e.g., a disease''s CFR lies within the CI for the boundary), the constraint''s operation becomes contestable and politically controllable. Suppression and theater_ratio rise if authorities can nudge the boundary without explicit justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_uncertainty_discretion, empirical, 'How epidemiological uncertainty translates into discretion in proportionality threshold placement.').

omega_variable(
    kernel_reading_distinctness,
    'Is the proportionality reading structurally distinct from the public-health-primary reading, or does it presuppose the same subordination of autonomy to collective benefit and differ only in claiming restraint via threshold?',
    'Theoretical analysis of the readings'' core axioms (see cs_structure.axioms below); comparison of boundary locations under each reading when novel pathogens emerge; examination of whether proportionality advocates accept the threshold when it forbids mandates they favor.',
    'If the readings are genuinely distinct, the constraint stands as a real boundary; if proportionality collapses into public-health-primary when tested, the proportionality reading is a performative restraint rather than a structural difference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether the proportionality reading is structurally distinct from public-health-primary or merely a restrained version of the same principle.').

omega_variable(
    historical_mandate_drift,
    'Have proportionality-based mandates drifted toward higher coercion for lower-severity diseases over time (theater_ratio and suppression rising)? If so, is the drift driven by changed epidemiology or by institutional pressure to expand authority?',
    'Temporal analysis of mandate scope and severity thresholds across decades; comparison of historical mandate decisions with contemporaneous epidemiological profiles; study of legislative intent and authority agency rationale statements over time.',
    'Evidence of drift without epidemiological justification would indicate the constraint is mutating from proportionality toward public-health-primary and that suppression mechanisms are hardening to maintain political support for mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_mandate_drift, empirical, 'Whether the constraint''s operation has drifted beyond the proportionality boundary over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(coer_tr_t0, observed).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(coer_tr_t5, observed).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(coer_tr_t10, observed).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(coer_tr_t15, observed).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(coer_tr_t20, observed).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(coer_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(coer_be_t0, observed).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(coer_be_t5, observed).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(coer_be_t10, observed).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(coer_be_t15, observed).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(coer_be_t20, observed).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(coer_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(coer_su_t0, observed).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(coer_su_t5, observed).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(coer_su_t10, observed).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(coer_su_t15, observed).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(coer_su_t20, observed).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(coer_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, vaccine_uptake_collective_action).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, herd_immunity_threshold_sufficiency).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the coercion_legitimacy_boundary kernel. The proportionality_reading instantiates a threshold-based boundary intended to distinguish legitimate (high-severity pathogens) from illegitimate (low-severity pathogens) coercion. The bodily_autonomy_primary and public_health_primary readings offer alternative kernels framings: categorical protection vs. categorical authority. All three readings affect the same downstream constraints (vaccine uptake, herd immunity thresholds) but through different mechanisms. The proportionality reading's claim to structural restraint is empirically testable via the theater_ratio and suppression trajectories — rising theater with no epidemiological justification indicates the boundary is being breached. The network link documents that these three stories compete to explain the same institutional structure and that their classification divergences are the measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
