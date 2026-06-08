% ============================================================================
% CONSTRAINT STORY: simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_fidelity_threshold, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Safety-Critical Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint instantiates a specific reading of the contested kernel
 *   'catastrophe_proxy_sufficiency' — the question of whether simulation can
 *   adequately proxy real catastrophic stress for competence validation. This
 *   reading (simulation_fidelity_threshold) holds that competence retention
 *   depends on simulation crossing a fidelity threshold where
 *   stress/uncertainty matches real catastrophe, and that sufficiency is
 *   fundamentally technology-dependent rather than categorical or universal.
 *   The constraint exhibits mixed extraction and coordination: simulation
 *   technology vendors and training infrastructure operators benefit from
 *   establishing validated fidelity standards (coordination function), but
 *   the process of defining and enforcing those standards creates asymmetric
 *   power, where vendors define sufficiency metrics that operating personnel
 *   cannot independently verify or contest. Operating crews face a binary
 *   certification gate: exceed the fidelity threshold and competence is
 *   validated; fall below it and certification is denied, regardless of
 *   actual competence in other dimensions. The technology-dependence means
 *   that as simulation capabilities improve, the fidelity threshold rises,
 *   creating a ratchet effect where organizations must continuously invest in
 *   upgrading simulation to maintain the same competence validation standard.
 *   The extractiveness has increased over the 15-year measurement interval as
 *   simulation-based certification has matured and consolidated as the
 *   primary training pathway, reducing alternative assessment methods and
 *   deepening organizational lock-in to vendor ecosystems.
 *
 * KEY AGENTS:
 *   - Operating Personnel: Primary victim (powerless/trapped) — face binary sufficiency gate defined by vendors; cannot exit certification regime without losing operational eligibility
 *   - Simulation Technology Vendors: Primary beneficiary (institutional/arbitrage) — define fidelity standards and capture expanding markets across industries; have high mobility and can pivot domains
 *   - Regulatory Authority: Secondary beneficiary/constrained coordinator (organized/constrained) — has genuine coordination function (validating training) but is captured by vendor expertise asymmetry; cannot independently verify fidelity claims
 *   - Operating Organization Leadership: Moderate beneficiary (powerful/mobile) — benefits from validated safety training and reduced liability; has mobile exit options through vendor choice but constrained by regulatory path-dependence
 *   - Safety Science Research Community: Constrained coordinator (moderate/constrained) — generates evidence about fidelity-competence transfer but is constrained by funding sources and empirical barriers to validating near-catastrophic stress
 *   - Legacy Certification Framework: Institutional theater (institutional/arbitrage) — older authority structures persist through institutional inertia; largely performative as simulation has replaced experience-based certification
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the constructed fidelity threshold as an immutable law of training transfer rather than a contingent institutional boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_fidelity_threshold, 0.48).
domain_priors:suppression_score(simulation_fidelity_threshold, 0.42).
domain_priors:theater_ratio(simulation_fidelity_threshold, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_fidelity_threshold, extractiveness, 0.48).
narrative_ontology:constraint_metric(simulation_fidelity_threshold, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(simulation_fidelity_threshold, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_fidelity_threshold, tangled_rope).
narrative_ontology:human_readable(simulation_fidelity_threshold, "Simulation Fidelity Threshold for Safety-Critical Competence Retention").
narrative_ontology:topic_domain(simulation_fidelity_threshold, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_fidelity_threshold, 'd4aa5337-30fa-4e02-a110-5388ba0a1e9e').
narrative_ontology:cs_kernel_codification('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', fixed_text).
narrative_ontology:cs_authority_grounding('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', extraction).
narrative_ontology:cs_interpretation_layer_present('d4aa5337-30fa-4e02-a110-5388ba0a1e9e').
narrative_ontology:cs_reading_relation('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', simulation_fidelity_threshold__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', simulation_fidelity_threshold__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', simulation_fidelity_threshold__hybrid_degradation_reading, influences).
narrative_ontology:cs_axiom('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', foundational, fidelity_sufficiency_technology_dependent).
narrative_ontology:cs_axiom_status(fidelity_sufficiency_technology_dependent, holdable).
narrative_ontology:cs_axiom_grounding('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', fidelity_sufficiency_technology_dependent, empirically_contingent).
narrative_ontology:cs_axiom('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', foundational, binary_certification_gate_instantiates_vendor_capture).
narrative_ontology:cs_axiom_status(binary_certification_gate_instantiates_vendor_capture, holdable).
narrative_ontology:cs_axiom_grounding('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', binary_certification_gate_instantiates_vendor_capture, deontological).
narrative_ontology:cs_reference_frame('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', simulation_as_legitimate_proxy_mechanism).
narrative_ontology:cs_drift_state('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4aa5337-30fa-4e02-a110-5388ba0a1e9e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, training_infrastructure_operators).
narrative_ontology:constraint_victim(simulation_fidelity_threshold, operating_personnel).
narrative_ontology:constraint_victim(simulation_fidelity_threshold, safety_margin_robustness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, regulatory_authority).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, operating_organization_leadership).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, safety_science_research_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operating crews in aviation, nuclear, maritime, and military contexts require continuous certification of competence through simulation. They face a binary gate: simulation performance above the fidelity threshold certifies competence; below the threshold results in decertification and loss of operational eligibility. They cannot exit the simulation regime without losing their jobs. The fidelity standard is defined by vendors and regulators, not by operators. As fidelity standards rise with technology improvements, operators must continuously retrain at increasingly demanding simulation levels, regardless of their actual competence in real operations. They bear the cost of training time and the anxiety of binary certification pressure.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, operating_personnel, payer,
    powerless, biographical, trapped, global).

% Vendors design, build, and sell the simulation systems that establish fidelity standards. They define what 'sufficient fidelity' means through the technical specifications of their systems. They benefit from expanding fidelity requirements across all domains (aviation, nuclear, maritime, military, energy, healthcare) because each domain increment increases demand for their products. They have high mobility — they can pivot between industries and geographic regions. They capture the margin between actual training benefit and the fidelity standard they define. Their business model aligns with rising fidelity thresholds, creating market expansion incentives.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, simulation_technology_vendors, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(simulation_fidelity_threshold, simulation_technology_vendors, beneficiary).

% Safety regulators establish and enforce certification standards. They mandate that simulation-based training must reach fidelity thresholds before operators are certified. They have a genuine coordination function: they ensure training actually prevents catastrophic incidents. However, they are constrained by technical expertise asymmetry — they cannot independently validate fidelity claims and must rely on vendor expertise and research community evidence. They benefit from delegating fidelity definition to vendors because it allows them to enforce standards without developing their own simulation capacity. They also benefit from the legitimacy provided by technology-based standards — fidelity thresholds appear objective and science-based. They are captured by vendor ecosystem dependence and cannot easily contest vendor definitions without losing regulatory credibility.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, regulatory_authority, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simulation_fidelity_threshold, regulatory_authority, beneficiary).

% Organizations that employ operators (airlines, nuclear plants, shipping companies, militaries) benefit from validated safety training. Simulation-based certification reduces liability exposure and meets regulatory requirements efficiently. They have mobile options: they can choose between simulation vendors, negotiate fidelity standards with regulators, or invest in alternative training modalities. However, they are partially captured by regulatory path-dependence — deviating from standard simulation approaches risks regulatory friction. They also benefit from the availability of certified operators trained through simulation. They experience the constraint as coordination of safety risk across their workforce, but with awareness that fidelity standards are rising and training costs are increasing.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, operating_organization_leadership, beneficiary,
    powerful, biographical, mobile, global).

% Researchers study the relationship between simulation stress and real-world competence transfer. They generate empirical evidence about fidelity sufficiency and train transfer. They have a coordination function: they produce scientific basis for fidelity standards. However, they are constrained by funding sources that often come from vendors or organizations with vested interests in favorable findings. They also face fundamental empirical barriers — they cannot ethically induce catastrophic stress in human subjects to validate simulation sufficiency. They benefit from the constraint through research funding and career advancement based on publications. They are constrained by methodological limitations that prevent them from definitively validating or falsifying fidelity claims.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, safety_science_research_community, beneficiary,
    moderate, generational, constrained, national).

% Older authority structures that certified competence through demonstrated experience in high-stress operations have been largely replaced by simulation-based certification. These frameworks persist theatrically — examiners maintain the fiction that simulation 'represents' real catastrophe adequately, but the functional authority has shifted to vendors who define simulation specifications. The legacy framework is substantially performative: it certifies that training occurred but cannot validate that competence will transfer to real catastrophe because real catastrophe cannot be induced experimentally without ceasing to be training.
narrative_ontology:constraint_stakeholder(simulation_fidelity_threshold, legacy_certification_framework, observer,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(simulation_fidelity_threshold, legacy_certification_framework).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation technology provides effective training in high-stress scenarios without incurring the cost and risk of real-world catastrophic incidents. This solves the genuine coordination problem of how to maintain operator competence in catastrophic scenarios that occur rarely enough that experience-based learning is inefficient.
% TRANSFER_FUNCTION: Fidelity standards transfer definition authority from operators and organizations to simulation vendors and regulators. Vendors transfer market expansion to themselves through rising fidelity requirements. Regulators transfer certification validation burden to vendors while maintaining regulatory legitimacy. Operating personnel transfer time and career security (binary certification pressure) to the system.
% ABSENT_VOICES: Operators in smaller organizations, older industries without mature simulation infrastructure (maritime, energy), and developing-economy operators who cannot afford simulation investment. Also absent: operators who have experienced near-miss incidents where simulation training proved inadequate — their evidence is suppressed by certification frameworks that treat simulation as categorically sufficient. Also absent: engineers or scientists who study fundamental limits on simulation-to-reality transfer but whose findings contradict vendor-friendly fidelity narratives.
% DISAPPEARANCE_RATIONALE: If the simulation fidelity threshold constraint disappeared overnight (regulators stopped enforcing fidelity standards, vendors lost market incentive to raise thresholds, organizations reverted to experience-based training), the world would substantially rearrange. Organizations would face either much higher training costs (maintaining expensive real-world catastrophic scenarios) or accept lower competence validation (experience-based assessment with inherent sampling bias). Operator career paths would change. Vendors would lose a major market segment. Regulators would lose their primary enforcement mechanism for training standards. This arrangement is institutional, not natural — it depends on specific regulatory choices, vendor ecosystem stability, and organizational path-dependence.
% FOUNDING_PROBLEM: How can organizations ensure operator competence in catastrophic scenarios that occur too rarely for experience-based learning? Real-world catastrophic incidents cannot be induced for training purposes without becoming actual catastrophes. Simulation technology offered a solution: replicate the stress profile of catastrophes in a controlled environment without actual danger.
% FOUNDING_PROBLEM_CORROBORATION: Safety regulators (FAA, NRC, IMO), independent operators (pilot unions, nuclear operator associations), third-party safety research organizations. Vendors are interested parties and their corroboration is discounted. Research community corroboration is mixed and constrained by methodological limitations.
narrative_ontology:disappearance_verdict(simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(simulation_fidelity_threshold, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Operating crews face a binary sufficiency gate: competence retention is validated only if simulation stress matches real-world catastrophe thresholds. The fidelity requirement is non-negotiable for certification but the standard is enforced by technology vendors who define 'sufficient match.' Crews cannot exit the simulation regime without losing certification and career viability. No alternative assessment pathway exists at comparable organizational scale. Maximum extraction — trapped in a certification machine they cannot influence.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Safety regulators have a genuine coordination function: they ensure training fidelity is actually validated against catastrophe-level stress. But the regulator is also constrained by reliance on vendor-defined metrics for fidelity assessment. The regulator both coordinates safety (genuine function) and extracts vendor capture (asymmetric power). They benefit from delegating fidelity definition to vendors but bear reputational cost if competence retention fails. Constrained by the technical expertise asymmetry.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Vendors define and sell the simulation technology that establishes fidelity sufficiency. They experience the constraint as coordination — they are solving the problem of translating real-world catastrophe into training scenarios. They have arbitrage options: they can pivot between industries (aviation, nuclear, maritime, military) and benefit from expanding fidelity requirements across all domains. Net beneficiary through technology lock-in and expanding market. The fidelity threshold is their product specification.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Leadership at the organizational level benefits from validated safety training — they reduce liability exposure and meet regulatory requirements efficiently through simulation. They have mobile options: they can choose simulation vendors, negotiate fidelity standards, or invest in alternative training modalities. They experience the constraint as coordination of safety risk across the workforce. However, they are partially captured by vendor ecosystem and regulatory path-dependence. Moderate experience of the constraint as coordination.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Researchers study the relationship between simulation stress and real-world competence transfer. They have a coordination function: they generate empirical evidence about fidelity sufficiency. But they are also constrained by funding sources (often from vendors or organizations with vested interests in certain fidelity conclusions) and by the challenge of studying catastrophic stress empirically. They benefit from the constraint through research funding but bear costs of methodological limitations. Constrained by epistemic barriers to validating their own findings.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Traditional certification frameworks defined competence through demonstrated experience in high-stress scenarios. As simulation has become prevalent, these frameworks have been largely replaced by simulation-based certification, but the older authority structure persists theatrically — examiners still maintain the fiction that simulation 'represents' real catastrophe adequately. The framework is substantially performative: it certifies that training occurred and that candidates demonstrated responses in simulated stress, but it cannot certify that competence will actually transfer to real catastrophe because real catastrophe cannot be experimentally induced. The theater ratio remains high because the alternative (acknowledging fundamental limits on simulation validation) is institutionally unacceptable.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a universal analytical perspective, a fundamental epistemic gap exists: real catastrophe cannot be replicated in simulation without ceasing to be a simulation (the catastrophe becomes real, and the training becomes emergency response, not preparation). The fidelity threshold necessarily reflects this unbridgeable gap — any claimed sufficiency is a constructed boundary, not a discovered law. This perspective risks naturalizing the binary sufficiency gate as an immutable constraint of training-transfer science, when it is actually a contingent institutional choice about acceptable risk tolerance. The engine will evaluate this as a false summit.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_fidelity_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_fidelity_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_fidelity_threshold, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(simulation_fidelity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(simulation_fidelity_threshold, TR),
    TR >= 0.70.

:- end_tests(simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high and rising over the interval. The constraint begins with lower extractiveness (0.28) when simulation is supplementary to experience-based training — operating personnel have alternatives for competence validation. As simulation-based certification consolidates as the primary pathway, extractiveness rises to 0.55 by year 15. The extraction is not maximal because simulation does provide genuine safety benefit (coordination function exists), but the benefit is asymmetric: vendors capture the margin between actual fidelity sufficiency and the fidelity standard they define. Suppression (0.42): Moderate. Operating personnel face barriers to exit through certification requirements and regulatory lock-in, but not absolute barriers — organizations can theoretically maintain experience-based training or develop alternative assessment methods (high cost, low regulatory acceptance). Suppression rises slightly with path-dependence but remains lower than physical confinement. Theater ratio (0.38): Moderate but rising. The constraint has genuine coordination content (simulation does improve safety training over no training), so it is not purely theatrical. However, theater increases over time as vendors layer increasingly complex fidelity metrics that cannot be empirically validated against real catastrophe. By year 15, a significant portion of 'fidelity assessment' is performative — simulators must match stress profiles that cannot be validated without inducing actual catastrophe.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between powerless trapped operators and institutional beneficiaries. Operators see binary certification pressure (Snare-ish) with no exit — they must continuously retrain to maintain certification as fidelity standards rise. Vendors see market growth and technology sales expansion (Rope-ish) — they coordinate safety and profit from expanding market. Regulators see coordination and risk mitigation (Rope-ish) but are captured by vendor expertise and cannot independently verify fidelity claims (Tangled Rope drift). Organization leadership has mobile options and benefits from liability reduction (Rope-ish) but is constrained by regulatory path-dependence and industry norms (Tangled Rope drift). The research community has genuine evidence-generation function (Rope-ish) but is constrained by funding sources and empirical barriers (Tangled Rope drift). The legacy framework persists as theater (Piton) — the authority structure is degraded but maintained by institutional inertia. The analytical observer risks treating the fidelity threshold as natural law (Mountain) — an unbridgeable epistemic gap between simulation and catastrophe — when it is actually a negotiated institutional boundary shaped by vendor interests and regulatory choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from structural position: power + exit options + beneficiary/victim status. Operating personnel (powerless/trapped/victim) experience maximum d ≈ 1.0 — high directionality toward extraction. Simulation vendors (institutional/arbitrage/beneficiary) experience d ≈ 0.0 — low directionality, or negative (they are subsidized by the arrangement). Regulatory authority (organized/constrained/mixed) experiences d ≈ 0.45-0.55 — they coordinate but are also captured, producing tangled rope classification. Organization leadership (powerful/mobile/partial beneficiary) experiences d ≈ 0.35 — they have agency and benefit from safety validation, but are partially locked into vendor ecosystem. The piton perspective (legacy certification framework) experiences high arbitrage exit (d ≈ 0.1) because the framework is already obsolete and maintained only by institutional inertia. The analytical mountain perspective risks d ≈ 0.0 (treating the constraint as a natural law with no beneficiary) — the false summit detector will evaluate whether beneficiaries exist (they do: vendors, regulators), suggesting the mountain classification is a naturalization of institutional choices.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy by identifying its dual function: (1) genuine coordination — simulation technology improves safety training and competence retention compared to no training or experience-only training; (2) constructed sufficiency — the specific fidelity threshold and binary certification gate benefit vendors and regulators by shifting validation burden and creating path-dependence. The mandate (validating competence retention) is still live and functional, but the implementation (technology-dependent binary threshold) has become path-dependent and extractive. The constraint is not a zombie mandate because simulation genuinely serves safety. But it is a captured mandate because the beneficiaries have shaped the specific implementation to lock in their structural advantages. Mandatrophy_resolved is NOT set to true because the mandate is still functional; the capture is structural, not terminal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_validation_empirical_accessibility,
    'Can the sufficiency of simulation fidelity be empirically validated without inducing actual catastrophe in experimental subjects?',
    'Meta-analysis of near-miss incidents in which operators trained to a fidelity threshold encountered unexpected real-world variation; correlation between pre-incident simulation stress levels and actual incident severity; post-incident performance assessment comparing operators trained at different fidelity levels',
    'If validation is possible: constraint reclassifies toward Rope (genuine coordination achievable). If validation is fundamentally impossible: constraint remains Tangled Rope at best, with irreducible asymmetry between vendors (who define fidelity) and operators (who cannot verify fidelity). Analytical mountain perspective becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_validation_empirical_accessibility, empirical, 'Whether fidelity sufficiency can be empirically validated').

omega_variable(
    technology_dependence_inversion,
    'Is technology-dependent sufficiency a feature (allowing calibrated improvement as simulation capabilities advance) or a bug (locking organizations into vendor-defined standards)?',
    'Historical analysis of organizations that have changed simulation vendors; examination of whether new vendors were adopted because they offered superior fidelity or for other reasons (cost, market pressure, regulatory relief); measurement of competence retention differences across vendor transitions',
    'If feature: constraint reclassifies toward organizational agency and mobile exit options (Rope from leadership perspective strengthens). If bug: constraint deepens toward snare classification — organizations become locked into specific vendor ecosystems and cannot exit without losing certification. Technology-dependence becomes a lock-in mechanism rather than a flexibility mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_dependence_inversion, empirical, 'Whether technology-dependence is an improvement or lock-in mechanism').

omega_variable(
    categorical_vs_continuous_sufficiency_boundary,
    'Is fidelity sufficiency actually a binary gate (above threshold = competence retained; below threshold = failure) or a continuous function where partial fidelity offers degraded but non-zero benefit?',
    'Analysis of operator performance in real-world variations not encountered in training; measurement of competence degradation curves as real-world conditions deviate from simulated scenarios; identification of whether fidelity failures produce cliff-edge performance drops or gradual degradation',
    'If binary: constraint''s theater-ratio is reduced (binary gates are easier to verify and less performative). If continuous: vendors have incentive to arbitrarily define ''sufficient'' fidelity to maximize their market; constraint''s extractiveness increases and theaters ratio rises (continuous functions allow more performance cover-stories). This omega directly affects the claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_continuous_sufficiency_boundary, empirical, 'Whether sufficiency is binary or continuous').

omega_variable(
    catastrophe_definition_authority,
    'Who has epistemic authority to define what counts as ''real catastrophe'' for simulation calibration: operators, vendors, regulators, safety scientists, or a combination?',
    'Analysis of historical incidents in which real-world catastrophe differed from regulatory/vendor definitions; review of how fidelity standards were adjusted after major incidents; examination of whether different stakeholders have produced competing definitions of catastrophe-level stress that align with their structural interests',
    'If authority is distributed: constraint experiences pressure toward Rope (multiple stakeholders can contest vendor definitions). If authority concentrates with vendors: constraint deepens toward Snare (vendors define sufficiency unilaterally, operators cannot contest). If authority rests with regulators but they delegate to vendors: constraint becomes captured Tangled Rope (regulator loses independent validation capacity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_definition_authority, empirical, 'Authority over catastrophe definition for simulation calibration').

omega_variable(
    reading_kernel_contest,
    'Is this constraint fundamentally a coordination problem (simulation technology enabling better safety training — Rope reading) or a constructed sufficiency gate that benefits vendors and regulators by shifting validation burden (Snare reading)?',
    'This is the core contested kernel (catastrophe_proxy_sufficiency). Sibling readings (simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading, hybrid_degradation_reading) offer structurally different interpretations of whether the fidelity threshold represents genuine coordination or vendor-captured extraction. This omega documents the kernel contest itself.',
    'Classification from empirical perspective (powerless/biographical/trapped/global) depends on which reading is instantiated. All six omegas above are resolvable empirically; this omega documents that the perspectival classification gap itself — whether the constraint is Rope, Snare, or Tangled Rope — is rooted in competing readings of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Kernel reading: simulation fidelity as coordination vs. constructed sufficiency gate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_fidelity_threshold, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simfid_tr_t0, simulation_fidelity_threshold, theater_ratio, 0, 0.28).
narrative_ontology:measurement(simfid_tr_t5, simulation_fidelity_threshold, theater_ratio, 5, 0.32).
narrative_ontology:measurement(simfid_tr_t10, simulation_fidelity_threshold, theater_ratio, 10, 0.38).
narrative_ontology:measurement(simfid_tr_t15, simulation_fidelity_threshold, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(simfid_be_t0, simulation_fidelity_threshold, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(simfid_be_t5, simulation_fidelity_threshold, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(simfid_be_t10, simulation_fidelity_threshold, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(simfid_be_t15, simulation_fidelity_threshold, base_extractiveness, 15, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(simfid_su_t0, simulation_fidelity_threshold, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(simfid_su_t5, simulation_fidelity_threshold, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(simfid_su_t10, simulation_fidelity_threshold, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(simfid_su_t15, simulation_fidelity_threshold, suppression_requirement, 15, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_fidelity_threshold, attachment_coordination).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, hybrid_degradation_reading).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, operator_certification_path_dependence).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, vendor_fidelity_standards_capture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'catastrophe_proxy_sufficiency'. The sibling readings (simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading, hybrid_degradation_reading) offer structurally distinct interpretations of whether fidelity thresholds represent coordination, insufficiency, or acceptable tradeoff. This reading claims fidelity is technology-dependent (ε ≈ 0.48 — moderate extraction due to vendor power + regulatory capture) and instantiates Tangled Rope. The sibling readings have different ε values reflecting their different beneficiary/victim structures and classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simulation_fidelity_threshold, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
