% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Gate
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the PROPORTIONALITY READING of the
 *   vaccine_mandate_balance kernel. It asserts that compulsory vaccination is
 *   permissible only when disease severity, transmission kinetics, and
 *   vaccine safety meet strict proportionality thresholds, and when
 *   exemptions remain robust. The reading sits between two sibling positions:
 *   bodily_autonomy_primary (no mandate can override consent) and
 *   public_health_primary (collective protection overrides individual consent
 *   when vulnerable populations face lethal exposure). The
 *   proportionality_reading attempts to honor both commitments: protect
 *   vulnerable populations from preventable death AND preserve individual
 *   bodily integrity by restricting mandates to cases where the disease
 *   threat genuinely justifies coercion. The constraint's structure is
 *   TANGLED ROPE: it coordinates disease control (genuine problem-solving for
 *   vulnerable populations) while extracting the cost of vaccine adverse
 *   events onto rare individuals and constraining hesitant-but-willing
 *   individuals' choice. Extraction is moderate (0.38) because the
 *   constraint's legitimacy depends on evidence-based thresholds, not
 *   arbitrary authority — when thresholds are not met, mandates are ruled
 *   illegitimate and the extraction vanishes. Suppression is moderate (0.42)
 *   because the constraint relies on institutional enforcement (employment,
 *   school access) but hesitant individuals retain exit options (geographic
 *   mobility, institutional withdrawal, documented exemption). Resistance is
 *   high (0.71) because vaccine hesitancy has become ideologically loaded and
 *   the boundary between principled refusal and identity-based refusal is
 *   blurred.
 *
 * KEY AGENTS:
 *   - public_health_authority: Sets thresholds, enforces mandates when proportionality criteria are met, bears political cost of justification.
 *   - vulnerable_populations: Benefit from mandate-driven vaccination rates but are powerless to enforce the mandate themselves; depend on others' compliance.
 *   - vaccine_hesitant_uncompelled: Pay the constraint's direct cost (mandated vaccination or institutional exit) when thresholds are met; benefit from exemption robustness when thresholds are not met.
 *   - vaccine_adverse_event_bearers: Powerless victims of rare but serious harms; their cost-bearing depends on the vaccine safety threshold being met (constraint legitimacy depends on accurate safety measurement).
 *   - courts_and_constitutional_review: Observational seat; their decisions on mandate proportionality reshape the threshold calibration.
 *   - bodily_autonomy_advocates: Excluded from this reading's framework; their core claim (no mandate justified) is replaced by 'mandate justified when threshold met.'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.38).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.42).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Gate").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '99961ab8-7d19-4697-9172-445391611f8b').
narrative_ontology:cs_kernel_codification('99961ab8-7d19-4697-9172-445391611f8b', formalized).
narrative_ontology:cs_authority_grounding('99961ab8-7d19-4697-9172-445391611f8b', expertise).
narrative_ontology:cs_interpretation_layer_present('99961ab8-7d19-4697-9172-445391611f8b').
narrative_ontology:cs_reading_relation('99961ab8-7d19-4697-9172-445391611f8b', vaccine_mandate_balance__bodily_autonomy_primary, influences).
narrative_ontology:cs_reading_relation('99961ab8-7d19-4697-9172-445391611f8b', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_axiom('99961ab8-7d19-4697-9172-445391611f8b', foundational, mandate_justified_when_proportionality_met).
narrative_ontology:cs_axiom_status(mandate_justified_when_proportionality_met, holdable).
narrative_ontology:cs_axiom_grounding('99961ab8-7d19-4697-9172-445391611f8b', mandate_justified_when_proportionality_met, empirically_contingent).
narrative_ontology:cs_axiom('99961ab8-7d19-4697-9172-445391611f8b', foundational, robust_exemptions_preserve_bodily_autonomy).
narrative_ontology:cs_axiom_status(robust_exemptions_preserve_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('99961ab8-7d19-4697-9172-445391611f8b', robust_exemptions_preserve_bodily_autonomy, deontological).
narrative_ontology:cs_reference_frame('99961ab8-7d19-4697-9172-445391611f8b', evidence_based_threshold_mandate_framework).
narrative_ontology:cs_drift_state('99961ab8-7d19-4697-9172-445391611f8b', contemporary_post_covid_contestation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('99961ab8-7d19-4697-9172-445391611f8b', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, disease_control_infrastructure).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_uncompelled).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, general_public_unvaccinated).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_adverse_event_bearers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, general_public_unvaccinated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers disease surveillance and vaccination policy. Under this reading, must conduct epidemiological assessment of disease severity, transmission kinetics, and vaccine safety before imposing mandates. Enforces proportionality thresholds by refusing to authorize mandates below the threshold and defending mandates that meet it. Bears the administrative cost of threshold assessment and the political cost of mandate justification to the public. Their exit is analytical — they observe constraints; they do not exit them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Immunocompromised, very young, elderly, and others for whom disease infection carries high mortality or severe morbidity risk. Depend on community transmission thresholds below herd immunity breakpoints to avoid infection. Benefit from mandates that protect the vaccinated-population threshold. Cannot exit the constraint; their protection depends on others' compliance. Face lethal risk if unvaccinated population grows.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Would prefer to remain unvaccinated but face mandate enforcement when proportionality thresholds are met: employment barriers, school attendance blocks, or institutional access restrictions where disease transmission risk is high. Under this reading, exemptions are available if the mandate satisfies proportionality thresholds — but if the thresholds are met, the constraint activates and they must comply or exit their institutional position. Their exit options are institutional (leaving employment/school) or geographic (moving to non-mandating jurisdictions), not legal challenge (proportionality determination is evidence-based).
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_uncompelled, payer,
    moderate, biographical, constrained, national).

% Experience rare but serious adverse events from vaccination (myocarditis, neurological outcomes, thrombosis). Under this reading, the mandate's legitimacy depends on vaccine safety meeting a threshold; bearers pay the cost of rare harms while the benefit accrues to the vaccinated population collectively. They cannot exit vaccination if the mandate applies; their recourse is post-event compensation claims or exemption through documented contraindication (e.g., prior adverse reaction). Their exit is medical (documented contraindication grounds exemption), not volitional.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_adverse_event_bearers, payer,
    powerless, biographical, trapped, national).

% Benefit from high vaccination rates reducing transmission risk, but may be subject to mandate if disease parameters cross thresholds. They experience constraint asymmetrically: if they remain unvaccinated and the threshold is met, they face enforcement; if threshold is not met, the constraint does not activate. Their exit options include vaccination (compliance, reducing personal risk), geographic exit (moving to lower-transmission jurisdictions), or institutional exit (leaving mandatory-participation settings like schools or healthcare work).
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, general_public_unvaccinated, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, general_public_unvaccinated, payer).

% Conducts and publishes research on disease severity, transmission dynamics, vaccine efficacy, and adverse event rates. Provides the evidentiary basis for threshold calibration and mandate justification. Their role is to measure and report; they have no power to enforce but generate the data the agenda-setter uses to determine when proportionality thresholds are met. Their exit is analytical; they publish findings independently.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, epidemiological_research_community, observer,
    institutional, generational, analytical, global).

% Adjudicate whether mandates meet constitutional proportionality requirements: whether the demonstrated disease severity, transmission risk, vaccine safety, and necessity genuinely warrant compulsion. Their decisions reshape the threshold calibration and enforcement regime by invalidating mandates that fall below proportionality or striking down restrictions that prevent evidence-based mandates. Their exit is analytical; they issue decisions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, courts_and_constitutional_review, observer,
    institutional, generational, analytical, national).

% Would argue that individual consent is inviolable and no proportionality calculus can override bodily integrity, but operate outside this reading's framework. Their core premise (consent is categorical) is directly contested by this reading's core premise (proportionality allows mandate under threshold conditions). They are excluded from designing or calibrating the proportionality assessment itself; they can litigate outcomes but not participate in threshold-setting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent, evidence-based decision gate for vaccination policy that balances collective protection (achieving herd immunity for vulnerable populations) against individual bodily autonomy. The coordination problem it solves: how to decide whether mandatory vaccination is justified for a given pathogen at a given time, such that the decision is public, reproducible, and grounded in disease parameters rather than arbitrary authority. The constraint is a decision framework, not a mandate itself — it specifies conditions under which mandates become legitimate.
% TRANSFER_FUNCTION: Moves the cost of rare vaccine adverse events from the vaccinated population collectively onto individuals unlucky enough to experience them, in exchange for a community-level reduction in transmission risk (benefit flows to vulnerable populations). The constraint also transfers decision authority from individuals (who would choose vaccination or not) to epidemiological evidence (which determines whether the threshold is met), and enforcement authority from preference to mandate when thresholds are crossed. The payers are vaccine-hesitant individuals (who lose autonomy when thresholds are met) and vaccine-adverse-event bearers (who absorb rare but serious harms). The beneficiaries are vulnerable populations (who gain protection) and the general vaccinated population (who gain reduced transmission risk).
% ABSENT_VOICES: Individuals who would refuse vaccination on religious or ideological grounds without engaging the proportionality reasoning (their objection is to any mandate, not to mandates that fail proportionality). The bodily-autonomy-primary reading would argue that no proportionality calculus can override consent; this reading excludes that debate by accepting proportionality as the decision framework. People living in low-income countries with low disease burden but high institutional pressure to vaccinate against imported threats are largely absent from threshold-setting (their voices are not in the room where proportionality is calibrated). Vaccine manufacturers are absent from this reading's framework (though their safety data feeds the threshold calculation); they would argue adverse events are overstated.
% DISAPPEARANCE_RATIONALE: If proportionality-based mandate authority vanished, vaccination policy would fragment: some jurisdictions would adopt public-health-primary (collective benefit overrides consent), others would adopt bodily-autonomy-primary (no mandates), and vulnerable populations would face radically different protection levels depending on local policy drift. The constraint itself enables a middle path; removing it forces the binary choice it was designed to avoid. Vaccination rates would diverge dramatically across jurisdictions. Vulnerable populations in autonomy-first jurisdictions would face higher disease risk; vulnerable populations in public-health-first jurisdictions would gain stronger protection but lose bodily autonomy guarantees. The vaccine-hesitant would no longer face proportionality-based enforcement (their constraint would vanish if autonomy-primary prevails, or would become absolute if public-health-primary prevails).
% FOUNDING_PROBLEM: Vaccination policy swing between two indefensible poles: absolute mandates (treating all diseases as smallpox-level threats, imposing coercion for low-risk pathogens) and absolute refusals (offering no protection to vulnerable populations even in lethal outbreaks). The founding problem is the need for a decision framework that acknowledges both individual bodily integrity and collective vulnerability, grounded in the actual severity and transmissibility of each pathogen. The problem pre-dates COVID but became vivid during COVID: some jurisdictions mandated vaccination for seasonal influenza (low-severity endemic disease) while others refused to mandate even for measles outbreaks (high-severity, low-vaccination-rate scenarios). Both approached look indefensible in retrospect.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts (U.S. Supreme Court, EU Court of Human Rights, Canadian Supreme Court) have endorsed proportionality as the operative test for vaccine mandate legitimacy, citing both bodily autonomy and public health as constraints. Epidemiological researchers attesting to pathogen variability confirm that the founding problem is real: disease severity and transmission kinetics vary enormously across pathogens (smallpox R0≈6, CFR≈30%; seasonal influenza R0≈1.2-1.6, CFR≈0.1%), making one-size-fit-all mandate policy indefensible. Public health authorities in multiple jurisdictions acknowledge the problem exists but dispute whether proportionality thresholds are administrable (they worry thresholds will be litigated endlessly and delay necessary mandates). Disease control advocates and epidemiologists from outside benefiting public health institutions (independent research organizations, academic medical centers, vaccine-safety monitoring networks) attest the framework prevents reckless mandates while preserving the option for evidence-based ones. Bodily autonomy advocates dispute the corroboration, arguing that no threshold justifies compelled bodily intervention — but their disagreement is with the framework itself, not with the founding problem's reality.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.38 (moderate, lower than raw utility would suggest) because the constraint's legitimacy is evidence-dependent. A mandate for seasonal influenza (low severity, low transmission risk, low adverse event rate) would fail proportionality and thus be illegitimate, removing the extraction. A mandate for smallpox during an outbreak (high severity, high transmission, low adverse event rate) would pass proportionality and the extraction would be legitimate. The measurement series shows extractiveness drifting slightly upward from 0.32 to 0.38 over the interval, likely reflecting the historical pattern where threat perception normalizes upward and thresholds are unconsciously relaxed (baseline disease reality doesn't change, but the urgency framing does). Theater ratio remains low-to-moderate (0.22) because the constraint's enforceable component is the epidemiological measurement, not theatrical performance — authorities actually conduct disease surveillance, safety monitoring, and public justification. Suppression is moderate (0.42) because institutional enforcement (employment conditions, school access) is real and coercive, but exit options exist (geographic, vocational, documented exemption through contraindication) — the hesitant are not trapped, they are constrained. Resistance is high (0.71) because vaccine hesitancy has acquired ideological identity markers; even when proportionality is met and a mandate is legitimate, resistance persists because the hesitancy is no longer merely preference-based but identity-fused. Accessibility collapse is moderate (0.58) because alternatives to vaccination exist in principle (geographic exit, institutional exit) but are costly — they are not gone, just expensive. The constraint operates through threshold conditioning: its entire legitimacy structure depends on threshold-meeting being true. This makes it unusually vulnerability-prone to measurement error, institutional capture, and threshold drift.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_authority seat computes the constraint as legitimate coordination (collective disease control, evidence-based, transparent thresholds). The vaccine_hesitant_uncompelled seat computes it as enforced extraction (mandatory bodily intervention, institutional coercion, thresholds they did not consent to). The vulnerable_populations seat computes it as protection (they benefit from high vaccination rates and have no exit option; the constraint saves their lives). The courts_and_constitutional_review seat computes it as a legal test: does this mandate meet proportionality, yes or no? Each seat has a coherent reading of the same structure. The engine computes per-seat directionality from the structural data: the authority is d≈0.2 (beneficiary-ish, sets the rules, analyzes the thresholds), the hesitant are d≈0.8 (target, pay the institutional cost when thresholds are met), vulnerable populations are d≈0.0 (full beneficiary, trapped, protected), vaccine-adverse-event-bearers are d≈0.95 (full target, powerless, non-consenting victims of rare harms). These directional positions are NOT authored — they derive from the beneficiary/victim declarations + power + exit. The divergence the engine computes is the seat-specific classification: what reads as Tangled Rope from the authority seat (coordination + enforcement) reads as Snare from the hesitant seat (pure extraction + institutional capture) and as protective Rope from the vulnerable-population seat (no suppression of their preferences, only coordination gain).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: vulnerable_populations and disease_control_infrastructure (public health capacity, institutional authority to make evidence-based decisions). Vulnerable populations are d≈0.0 (they benefit from the constraint, cannot exit, have no costs imposed directly). Disease_control_infrastructure is abstract (treated as vindicated_proposition, not agent), but the structural beneficiary is the public health authority administering it. Victims: vaccine_hesitant_uncompelled (pay the constraint's enforcement cost — institutional barriers, vaccination mandate). Their d is high (~0.8) because they pay the institutional cost when thresholds are met, have identity-locked exit (vaccine hesitancy is often identity-fused), and cannot opt out if the mandate applies. However, their victim status is CONDITIONAL: it only activates when thresholds are met. If disease severity is low and proportionality thresholds are not met, no mandate applies and there is no extraction — the hesitant are not victims. This conditional victim status is a key feature of the proportionality reading and is captured through the measurement series: extractiveness rises when disease threat rises (thresholds more likely to be met), falls when threat falls. The hesitant are not unconditional payers; they are conditional payers whose status depends on epidemiological evidence. Directionality overrides: vaccine_adverse_event_bearers are not listed as victims in base_properties because they are not the mandate's targets — they are unintended harms of vaccination. But they are victims of the constraint because they bear a cost (adverse events) that is accepted in the proportionality calculus. No override is needed; the structural data (adverse events as a measurable, rare outcome entering the threshold calculation) captures this without explicit victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint risks mandatrophy (founding problem dead but arrangement persists) if vaccine hesitancy becomes so widespread that no mandate can achieve herd immunity even when proportionality thresholds are met. The founding problem (how to decide when to mandate) remains live as long as the decision gate is contested. But mandatrophy would emerge if: (1) the founding problem (decision-making framework needed) is solved and the proportionality structure is institutionalized (no longer contested, now routine procedure), AND (2) the constraint persists even when disease severity is low and thresholds are not met — i.e., authorities invoke proportionality language to justify mandates that fail proportionality tests in fact. The measurement series shows stable extractiveness (rising then flattening), not accumulating extraction, suggesting mandatrophy is not yet manifest. The theater_ratio remains low, suggesting the constraint's enforceable component (epidemiological measurement) is not yet theatrical — authorities genuinely conduct the assessments. The high resistance (0.71) indicates the constraint is not yet uncontested enough to be performative. Mandatrophy would manifest as theater_ratio rising (more time spent justifying the constraint than implementing it), extractiveness rising while disease severity falls (extraction decoupled from evidence), and resistance falling (opposition becomes futile and gives up). None of those are present yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_empirical_contestation,
    'What specific values for disease severity (case fatality rate, severe morbidity incidence), transmission kinetics (R₀, secondary attack rate), and vaccine safety (adverse event rates, confidence intervals) constitute a ''met'' threshold versus ''unmet'' threshold?',
    'Expert panel consensus (CDC, WHO, national health authorities) on numerical threshold values for each component. Cross-national calibration studies examining where thresholds diverge and why (population demographics, healthcare capacity, variant dominance). Post-hoc review of mandate decisions against historical disease parameters to establish the implicit thresholds authorities used.',
    'High uncertainty in threshold calibration permits authorities to either suppress mandates below evidence-justified levels (under-enforcement) or impose mandates on mild pathogens (overreach). If thresholds can be operationalized with high confidence, the constraint becomes administrable proportionality; if not, it becomes a procedural cover for pre-existing policy preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_empirical_contestation, empirical, 'Whether the proportionality thresholds are operationally definable or remain subjective.').

omega_variable(
    exemption_robustness_vs_mandate_failure,
    'When exemptions are ''robust'' (broad accessibility, low bureaucratic friction, genuine escape hatch), do they undermine the mandate''s effectiveness to the point of defeating its public health purpose?',
    'Comparative analysis of mandate-with-exemptions versus mandate-without-exemptions across jurisdictions and pathogens. Empirical examination of exemption uptake rates and their relationship to achieved vaccination coverage. Case studies of jurisdictions that tightened exemptions to preserve mandate function and observed health outcomes before/after.',
    'If robust exemptions consistently reduce vaccination coverage below herd immunity thresholds, the constraint becomes Goodhart-vulnerable: the proportionality framework is met, exemptions are robust, yet the public health outcome fails. The constraint would then collapse into either mandate-without-exemptions (foreclosing the proportionality reading''s core commitment to robust exit) or acceptance of endemic disease in non-exempt populations (foreclosing the beneficiary protection goal). This omega documents whether the constraint''s two legs can coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_vs_mandate_failure, empirical, 'Whether robust exemptions are compatible with mandate-based disease control.').

omega_variable(
    reading_vs_bodily_autonomy_primary_foreclosure,
    'Does the proportionality reading''s acceptance of threshold-based mandates logically foreclose the bodily-autonomy-primary reading, or do they remain coherently available to different parties?',
    'Philosophical analysis of the core axioms. If the bodily-autonomy-primary reading asserts ''no mandate can be justified under any proportionality calculus,'' then proportionality_reading''s axiom ''mandates are justified when thresholds are met'' logically contradicts and forecloses it. If bodily-autonomy-primary merely asserts ''individual consent is normally decisive'' (allowing threshold exceptions), the readings coexist.',
    'If foreclosure is real, this reading and bodily-autonomy-primary cannot coexist in a single legal or ethical framework — courts and policymakers must choose between them. If coexistence is real, they remain sibling readings available to different parties. The resolution determines whether proportionality_reading is a true compromise or a disguised subordination of bodily autonomy to public health.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_bodily_autonomy_primary_foreclosure, conceptual, 'Whether proportionality and bodily autonomy readings logically foreclose each other or remain coexistent.').

omega_variable(
    measurement_basis_and_institutional_power,
    'Who controls the measurement of disease severity, transmission risk, and vaccine safety — and does that institutional power corrupt the proportionality assessment itself?',
    'Governance analysis of research funding, publication incentives, and regulatory authority. Examination of cases where public health authorities'' measurements diverged from independent research (e.g., adverse event rate disagreements between regulatory agencies and standalone research teams). Analysis of whether funding sources or institutional positions predicted measurement outcomes.',
    'If the agenda-setter (public health authority) also controls the measurements that determine whether thresholds are met, the proportionality framework becomes capture-vulnerable: the threshold assessment is not evidence-driven but outcome-driven (the authority measures what it wants to find). The constraint would then provide legitimacy cover for predetermined mandates rather than genuine proportionality review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_basis_and_institutional_power, empirical, 'Whether measurement authority is institutionally independent or captured by the agenda-setter.').

omega_variable(
    kernel_reading_contest_proportionality_vs_public_health_primary,
    'This constraint instantiates the proportionality_reading of the vaccine_mandate_balance kernel. The public_health_primary reading would assert that collective protection overrides individual consent when herd immunity is necessary to protect vulnerable populations. Do these readings coexist in live debate, or does one logically foreclose the other?',
    'Examination of judicial, legislative, and institutional statements from jurisdictions that have endorsed each reading. Analysis of whether a single court or authority has held both readings simultaneously or whether they are consistently divided across different parties.',
    'This omega documents the contested kernel itself: whether proportionality_reading and public_health_primary are alternative frameworks available to different parties (coexist_with) or whether proportionality_reading''s threshold concept logically influences public_health_primary by requiring justification that public_health_primary treats as unnecessary (influences). The resolution shapes how to interpret the cs_structure.reading_relations field.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_proportionality_vs_public_health_primary, conceptual, 'Relationship between proportionality_reading and public_health_primary sibling reading.').

omega_variable(
    internalized_suppression_in_vaccine_hesitancy,
    'Is the resistance to mandates (measured as high resistance: 0.71) structural (legal barriers, enforcement costs) or internalized (hesitant individuals have incorporated the authority''s framing of vaccination as obligatory, making exit psychologically costly)?',
    'Post-exemption or post-mandate-removal studies: if resistance falls sharply when mandates are lifted, suppression was largely structural. If resistance persists and new vaccine hesitancy arises through ideological identity fusion with non-compliance, suppression is internalized. Qualitative research on how vaccine-hesitant individuals describe their constraints (institutional barriers vs. identity and dignity violation).',
    'If internalized, the effective suppression is higher than the structural measure suggests — the constraint carries forward in individual choice-making even after institutional enforcement is removed. This would strengthen the Tangled Rope classification (coordination goal + internalized extraction). If structural, enforcement cessation would rapidly normalize vaccination rates among the hesitant, suggesting the constraint is more Rope-like (coordination with temporary enforcement needed to overcome initial coordination failure).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_in_vaccine_hesitancy, empirical, 'Whether vaccine-mandate suppression is structural enforcement or internalized identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__proportionality_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(vacc_tr_t5, observed).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__proportionality_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t10, observed).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__proportionality_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(vacc_tr_t15, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_balance__proportionality_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(vacc_be_t5, observed).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(vacc_be_t10, observed).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement_basis(vacc_be_t15, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(vacc_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(vacc_su_t5, observed).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(vacc_su_t10, observed).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement_basis(vacc_su_t15, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(vacc_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel admits three structurally distinct constraint readings: bodily_autonomy_primary (categorical consent protection), proportionality_reading (threshold-based mandate), and public_health_primary (collective benefit overrides consent). Each reading instantiates a different constraint with different beneficiaries, victims, extraction profiles, and ε values. The proportionality_reading is the middle-ground attempt to preserve both individual and collective commitments. All three remain live as institutional positions held by different jurisdictions and courts; they are linked via the kernel, not unified into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
