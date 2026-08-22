% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulated Catastrophe as Competence Maintenance (Simulation-Sufficiency Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Regulatory mandate requires periodic simulated catastrophe exercises as
 *   proof of operational competence maintenance. This constraint instantiates
 *   the simulation-sufficiency reading of the exercise-as-competence kernel:
 *   the claim is that high-fidelity simulators constitute genuine exercise of
 *   the competence required for real crisis response, and that fidelity of
 *   simulation determines how well operators retain real-crisis judgment. The
 *   constraint benefits regulatory agencies and institutional risk managers
 *   (simplified compliance verification, quantifiable metrics, distributed
 *   liability). It extracts from field operators (who pass simulators but
 *   face real-stakes gaps) and from exposed populations (who depend on
 *   operators trained in simulators but not in real catastrophe). This is NOT
 *   a pure coordination story (Rope) — the coordination function is real
 *   (uniform, scalable competence verification), but the extraction is
 *   asymmetric and active enforcement is required to maintain
 *   simulator-compliance mandates against operator and researcher skepticism.
 *   This reading coexists with two sibling readings that contest whether
 *   simulation is sufficient: the lived_catastrophe_necessity_reading (only
 *   real catastrophe exercises true competence) and the hybrid_decay_reading
 *   (simulation exercises procedure but not judgment-under-stakes).
 *
 * KEY AGENTS:
 *   - Regulatory agencies: institutional, agenda-setter, sets drill mandates and performance metrics
 *   - Institutional risk managers: powerful, beneficiary/payer, operate under mandate and benefit from simplified compliance
 *   - Field operators: moderate, payer, identity-locked to profession, certified by simulators but undertrained for real catastrophe
 *   - Exposed populations: powerless, payer, trapped in geographic exposure, harmed by response gaps
 *   - Simulation vendors: powerful, beneficiary, incentivized to sell fidelity not necessarily competence
 *   - Catastrophe survivors: moderate, observer, speak after disaster with evidence of simulator gaps
 *   - Advocates for lived training: moderate, excluded, barred from mandate-setting because their position would invalidate the sufficiency claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.71).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulated Catastrophe as Competence Maintenance (Simulation-Sufficiency Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '7e42bc57-120d-4c9c-b0ea-05a363cdc87b').
narrative_ontology:cs_kernel_codification('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', fixed_text).
narrative_ontology:cs_authority_grounding('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', extraction).
narrative_ontology:cs_interpretation_layer_present('7e42bc57-120d-4c9c-b0ea-05a363cdc87b').
narrative_ontology:cs_reading_relation('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', foundational, simulation_fidelity_preserves_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_preserves_competence, holdable).
narrative_ontology:cs_axiom_grounding('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', simulation_fidelity_preserves_competence, empirically_contingent).
narrative_ontology:cs_axiom('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', foundational, procedural_performance_metrics_measure_crisis_readiness).
narrative_ontology:cs_axiom_status(procedural_performance_metrics_measure_crisis_readiness, holdable).
narrative_ontology:cs_axiom_grounding('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', procedural_performance_metrics_measure_crisis_readiness, empirically_contingent).
narrative_ontology:cs_axiom('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', secondary, judgment_under_stakes_trainable_via_simulator).
narrative_ontology:cs_axiom_status(judgment_under_stakes_trainable_via_simulator, holdable).
narrative_ontology:cs_axiom_grounding('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', judgment_under_stakes_trainable_via_simulator, empirically_contingent).
narrative_ontology:cs_reference_frame('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', simulator_sufficiency_framework).
narrative_ontology:cs_drift_state('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', post_disaster_accountability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e42bc57-120d-4c9c-b0ea-05a363cdc87b', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, institutional_risk_managers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, field_operators_undertrained_by_simulation).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, populations_exposed_to_degraded_response).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, institutional_risk_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce drill mandates, define acceptable simulator fidelity, audit drill completion and performance metrics against regulatory standards. Justify simulator-based competence verification as reliable and scalable. Collect compliance data and report readiness to political leadership. Benefit from a standardized, quantifiable, auditable competence metric that shifts liability away from regulatory oversight toward simulator fidelity and operator compliance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Operate crisis-response institutions (hospitals, fire departments, emergency management agencies, nuclear facilities) under regulatory mandate to conduct regular drills and maintain operator certification. Benefit from a clear, repeatable compliance checklist and predictable audit cycles that reduce legal and regulatory uncertainty. Also bear cost of maintaining simulator infrastructure, scheduling drills, and training staff. Cannot exit without facing regulatory violation, license suspension, and reputational damage. Prefer simulator-based competence accounting because it provides documented proof of readiness and distributes liability to simulation vendors and individual operator performance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, institutional_risk_managers, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, institutional_risk_managers, payer).

% Required to complete periodic simulator drills (quarterly, annually, depending on sector) and pass performance thresholds. Once certified, are legally and professionally recognized as competent for crisis response. Simulator training is routine, manageable, and produces passing scores. When actual catastrophe occurs, discover gaps between simulator fidelity and real conditions: communication systems behave unexpectedly, environmental stresses not modeled in simulator, decision-making under genuine stakes differs radically from simulator scoring. Blamed for inadequate performance and blamed for 'not using their simulator training.' Cannot easily exit profession (identity as operator, economic dependence, years of training investment) without confronting that their certification is false assurance. The identity_lock is psychological: professional identity fused with regulatory certification, which is itself predicated on simulator sufficiency.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, field_operators_undertrained_by_simulation, payer,
    moderate, biographical, identity_locked, local).

% Geographically or occupationally exposed to catastrophic risk (earthquake in populated city, industrial accident near residential area, pandemic in dense region). Depend on emergency-response operators for evacuation, rescue, treatment, communication. Response operators are simulator-certified but have not been real-catastrophe-tested. When actual disaster strikes, response gaps manifest as delayed evacuation, miscommunication, resource misallocation, coordination failures. These gaps correlate with simulator-training insufficiency: operators know procedures but not real-stakes judgment. Populations have no exit option (cannot choose where to live without abandoning livelihoods); they bear the difference between certified-competence and actual-competence as direct harm.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, populations_exposed_to_degraded_response, payer,
    powerless, immediate, trapped, local).

% Design, build, and sell simulators to regulatory-mandated institutions. Benefit from institutional mandate to maintain and upgrade simulation capability; this creates recurring revenue and expands market. Have exit options (sell simulators to other sectors, including entertainment and training outside safety-critical domains) but maintain strong margins in mandatory-drill market. Incentive structure is misaligned: vendors profit from simulators that are sold as high-fidelity and produce impressive performance metrics, not necessarily from simulators that actually preserve real-crisis competence. Higher reported simulator fidelity = more contract value, but correlation with actual-competence is not measured or contracted.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% After disaster, document actual emergency response and identify failures. Conduct post-hoc analysis showing simulator-trained operators made predictable errors that real-stakes experience would have prevented: poor judgment under genuine time pressure, inability to adapt to conditions not in simulator, miscommunication in high-noise environment. Provide testimony to legislative committees and publish research. Voice is marginal in regulatory mandate-setting because testimony comes after disaster (too late to prevent harm) and research challenges benefiting institutions. Excluded from ex-ante policy design.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, catastrophe_survivors_and_researchers, observer,
    moderate, biographical, analytical, local).

% Face pressure from regulatory agencies, political leadership, and public expectation to demonstrate institutional readiness for catastrophe. Also face liability exposure: if disaster response fails, leadership is investigated for negligence or inadequate preparation. Simulator-based competence metrics provide documented, auditable, quantifiable proof that the institution did everything required to maintain readiness. This documentation shifts liability from leadership's oversight (did we really maintain competence?) to simulator's fidelity (does the simulator represent the real situation?) and to the operator's compliance (did they pass the certified drill?). Prefer this framework because it transforms a subjective, difficult accountability question (are we actually ready?) into a procedural, documented one (did we complete the certified drill and pass the metrics?).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, senior_leadership_of_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Argue that actual, real-stakes crisis experience is necessary to maintain true judgment and decision-making capability; simulators train procedures and muscle memory but not the cognitive and emotional adaptation required under genuine stakes. Would advocate for maintaining a cadre of experienced operators by: deploying trained operators to actual disasters (disaster relief, emergency deployments, mutual aid responses to regional crises), rotating operators through live-exercise events with genuine consequences, or creating high-stakes tabletop exercises where operator decisions have real downstream effects. Structurally excluded from regulatory mandate-setting and institutional policy design because admitting that real-stakes experience is necessary would imply that simulators are insufficient, which would invalidate the current regulatory framework and require massive institutional restructuring. Their voice is suppressed not by explicit censorship but by institutional design: they are not seated at mandate-setting tables; their research is marginalized as 'impractical' or 'too expensive'; post-disaster they are quoted but then forgotten until the next disaster.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, advocates_for_lived_training, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_agencies).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, standardized, auditable competence-verification system across dispersed, autonomous institutions: instead of relying on institution-specific, ad-hoc operator experience or informal knowledge transfer, all operators meet the same simulator-based performance threshold. Simplifies regulatory verification (auditors measure drill completion and performance metrics, not subjective judgment of readiness). Creates comparable readiness metrics across regions, sectors, and institutions. Enables scalable, repeatable competence maintenance without requiring every institution to maintain expensive real-crisis exercise programs.
% TRANSFER_FUNCTION: Transfers institutional liability for competence gaps away from institutional leadership (and regulatory agencies) onto simulator fidelity and operator performance: if an operator is certified, failure is the operator's or the simulator's problem, not the institution's management problem. Transfers economic resources from institutions to simulation vendors (recurring licensing, infrastructure, maintenance). Transfers attention and effort from operators' real-world crisis experience toward standardized drill schedules and performance metrics. In actual catastrophe, transfers risk from certified-but-untrained operators to the populations they serve.
% ABSENT_VOICES: Advocates for lived-catastrophe training and experienced-operator rotation are excluded from mandate-setting; they speak only after disaster occurs, when the institutional framework is locked in. Operators who doubt simulator sufficiency lack formal channels to contest certification standards; their skepticism is treated as non-compliance. Populations exposed to catastrophe are unrepresented in regulatory design; their voice enters only through post-disaster litigation and emergency-response investigations. Simulation vendors are beneficiaries who have no voice in mandate-setting (they design to spec) but their profit motive creates incentive misalignment that is not exposed until post-disaster analysis.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency mandate disappeared overnight, institutions would face immediate pressure to restore operator competence through alternative means: either expensive, continuous real-stakes crisis-exercise programs (which most cannot afford and most would resist), or reliance on aging, experience-based operator cohorts (which regulatory agencies would pressure institutions to replace with simulator-trained newcomers). Regulatory agencies would lose their quantifiable compliance metric and would face direct accountability for competence assurance, which they currently distribute across simulators and operators. Leadership would face unmediated liability for competence failures. The constraint's removal would fundamentally reorganize how institutions balance simulator investment, operator experience cycles, real-crisis participation, and liability exposure — it would force the underlying coordination problem (maintaining crisis-response competence) back into visibility as a genuinely difficult institutional challenge, rather than allowing it to be solved by measurement and documentation.
% FOUNDING_PROBLEM: Crisis-response competence degrades over time without systematic activation; institutions cannot maintain readiness through institutional memory or informal knowledge transfer alone; real catastrophes are rare enough that ad-hoc, experience-based training cannot provide reliable competence verification at scale. Early regulators needed a systematic, scalable, measurable way to verify that crisis-response competence was being maintained across dispersed institutions without requiring every institution to participate in real-crisis response.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and senior institutional leadership attest the founding problem is live and simulators are the solution: they cite declining operator experience (fewer operators have been through real crises due to improved preparedness and prevention), aging of experienced operator cohorts, and the need for scalable competence verification. Post-disaster investigators and emergency-response researchers attest the founding problem persists even with simulator mandates: competence atrophies in ways that simulators do not measure (judgment-under-genuine-stakes, adaptation to unforeseen conditions, emotional resilience). Legislative testimony from operators in failed disaster responses (Hurricane Katrina emergency responders, Fukushima operators, COVID-19 hospital crisis teams) documents specific gaps between simulator performance and actual-stakes decision-making; this testimony comes from outside the benefiting parties (regulators and risk managers). Research on expertise shows that deliberate practice in artificial conditions does not preserve judgment-under-high-stakes as effectively as periodic real-stakes activation.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint transfers institutional liability away from leadership and creates a gap between certified-but-undertrained operators and actual-crisis performance. The gap grows when simulators are well-marketed but real catastrophe reveals their limitations. Suppression is elevated (0.71) because the constraint's persistence requires excluding and discrediting the lived-catastrophe reading: regulatory agencies suppress research showing simulator insufficiency; they enforce audit compliance over real-crisis experience. Theater ratio is high (0.58 at interval end) because the core function — maintaining competence — is increasingly performed by metrics and compliance documents rather than actual judgment-under-stakes. The measurement series show extractiveness and theater rising over the interval: as simulators become more sophisticated (higher fidelity by appearance), the gap between simulator performance and real-crisis judgment widens because operators and institutions mistake fidelity-of-simulation for fidelity-of-competence. Suppression plateaus after t=24 because regulatory enforcement stabilizes once simulators become normalized and operator skepticism is marginalized. The constraint exhibits Goodhart's Law dynamics: the measure (simulator performance) becomes the target, displacing the actual target (real-crisis competence).
 *
 * PERSPECTIVAL GAP:
 *   Regulatory agencies and senior leadership compute this as Rope or even Mountain — natural coordination evolution. Field operators and catastrophe researchers compute it as Snare — extraction disguised as competence assurance. Simulation vendors sit between: they benefit from mandates (Rope for them) but have no stake in whether simulators truly preserve competence. The engine computes per-seat type from power/exit/directionality: regulators (institutional power, analytical exit) see coordination; field operators (moderate power, identity-locked exit) see extraction and suppression; exposed populations (powerless, trapped) see pure victimization. This divergence is the signal — it is not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies: d ≈ 0.2 (beneficiary, institutional power, analytical exit — low directionality toward extraction, can reframe mandate anytime). Institutional risk managers: d ≈ 0.35 (beneficiary + payer, powerful, constrained exit — benefit from compliance simplicity but pay for simulation infrastructure). Field operators: d ≈ 0.78 (victims, moderate power, identity-locked exit — high directionality, cannot exit profession without losing identity/income, extraction is masked as competence standard). Exposed populations: d ≈ 0.95 (victims, powerless, trapped, bear real-catastrophe risk from undertrained operators, cannot leave geographic exposure). Simulation vendors: d ≈ 0.1 (beneficiary, powerful, arbitrage exit — sales revenue but can pivot to other sectors). The identity-locking of field operators is critical: they believe they are certified-competent by regulators; exiting that identity frame requires confronting the false assurance, which is psychologically costly. Suppression mechanism is partly structural (regulatory enforcement, audit machinery) and partly internalized (operators' professional identity fused with regulatory certification).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: competence does degrade without systematic exercise. But this reading claims the problem is SOLVED by simulators, which the measurement data and post-disaster analysis contest. The theater ratio rising above 0.5 (reaching 0.58) indicates proxy goals replacing real function: simulator completion rates and performance metrics increasingly stand in for actual-stakes competence, and regulators optimize for the metric rather than the outcome. This is classic mandatrophy: the mandate (conduct high-fidelity simulators) persists and grows more sophisticated, but the underlying coordination problem (maintain real-crisis competence) is increasingly unmet. The founding_problem_status is 'contested' precisely because this reading asserts simulators solve it while the lived_catastrophe_necessity reading asserts they cannot. The constraint's persistence is enforced (regulators mandate drills, audit compliance, certify operators) not because the coordination outcome is working (post-disaster evidence shows it is not) but because the regulatory and institutional apparatus benefits from simplified, quantifiable compliance. This is a Tangled Rope whose extractive tail grows as simulators sophisticate without actually solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_competence_correlation,
    'Does high simulator fidelity accurately predict competence in actual crisis response, or do simulators train procedure while judgment-under-stakes requires real stakes to develop and retain?',
    'Post-disaster analysis comparing simulator performance metrics (final-stage scores, procedure completion rates) against actual-crisis response performance in the same operators, controlling for time since simulator drill. Natural experiment from jurisdictions that mandate lived-crisis exercises alongside simulators.',
    'High correlation supports the reading; low/zero correlation would shift competence accounting toward lived-catastrophe necessity or hybrid models. This determines whether the extracted value (institutional liability transferred, compliance simplified) aligns with real risk reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_competence_correlation, empirical, 'Whether simulator fidelity predicts real-crisis competence retention.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is field-operator skepticism about simulator sufficiency suppressed by external enforcement (audit machinery, certification threat) or by internalized belief in regulatory authority (professional identity fused with regulatory certification)?',
    'Post-exit trajectory: if operators who leave the profession report persistence of belief in simulator-competence correlation, suppression is internalized; if they quickly recognize the gap once regulatory authority is removed, suppression was structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and victims carry the suppression after exit. If structural, removing regulatory machinery would quickly surface operator doubt and destabilize the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether measured suppression is structural or internalized in operator identity.').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Can the simulation_sufficiency_reading and lived_catastrophe_necessity_reading coexist within a single regulatory framework, or does admitting real-stakes necessity logically foreclose the claim that simulators are sufficient?',
    'Regulatory design experiment: can a framework mandate both simulators AND maintained operator rotation through real-crisis response roles (disaster relief, emergency deployment) without contradiction? If yes, readings coexist; if regulators treat the two as mutually exclusive, foreclosure is operational.',
    'If truly coexistent, a hybrid mandate combining both is optimal. If one forecloses the other, the framework must choose, and the choice reveals which reading''s axioms the authority structure actually holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Whether the simulation-sufficiency and lived-catastrophe readings logically foreclose or coexist.').

omega_variable(
    false_summit_regulatory_naturalization,
    'Is the claim that ''simulation fidelity determines competence'' a natural law of learning psychology, or a regulatory framing that benefits institutional risk managers by simplifying liability?',
    'Cross-sector analysis: does the simulation-sufficiency reading hold in fields (surgery, piloting, military command) where post-performance data is rich and accountability is high? Or is it accepted most readily in sectors where real-outcome data is sparse or post-disaster analysis is delayed (emergency management, disaster response)?',
    'If the reading is field-dependent, it is a constructed regulatory choice, not a universal principle. This supports FSM (false summit detection): a competence claim that benefits identifiable institutional actors and is resisted by field operators may be a false natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_regulatory_naturalization, empirical, 'Whether simulation-competence sufficiency is universal principle or regulatory construct benefiting specific institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(exer_tr_t32, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 32, 0.59).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(exer_be_t32, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(exer_su_t32, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.18).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% The exercise_as_competence_maintenance kernel decomposes into three constraint stories, each instantiating a different reading of what constitutes genuine competence-exercise. The simulation_sufficiency_reading (this file) treats high-fidelity simulators as sufficient; the lived_catastrophe_necessity_reading treats only actual crisis experience as sufficient; the hybrid_decay_reading treats simulation as adequate for procedure but insufficient for judgment-under-stakes. These are not three perspectives on one constraint — they are three structurally distinct constraints with different ε values, different beneficiary/victim sets, and different foundational axioms. The simulation_sufficiency_reading authors ε as moderate-high (0.62) because the coordination function is real but increasingly masked by extraction. The lived_catastrophe_necessity_reading would author ε as very high (0.85+) because the constraint extracts by substituting false assurance for real competence. The hybrid_decay_reading would author ε as moderate (0.55) because it acknowledges both components but claims simulator-only training is inadequate. All three remain live positions in the regulatory and organizational-learning literature; their coexistence is an ongoing institutional contest, not a resolved question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
