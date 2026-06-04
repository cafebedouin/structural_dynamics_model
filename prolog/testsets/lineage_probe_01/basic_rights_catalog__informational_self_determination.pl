% ============================================================================
% CONSTRAINT STORY: basic_rights_catalog__informational_self_determination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_rights_catalog__informational_self_determination, []).

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
 *   constraint_id: basic_rights_catalog__informational_self_determination
 *   human_readable: Informational Self-Determination (Constitutional Right to Data Control)
 *   domain: constitutional_law/fundamental_rights/data_protection
 *
 * SUMMARY:
 *   The German Constitutional Court's 1983 census decision
 *   (Volkszählungsentscheidung) announced a new right drawn from old
 *   constitutional text: informational self-determination, grounded in
 *   dignity and personality (Article 2(1) and 1(1) of the Basic Law). The
 *   constraint captures the tension between individuals' claim to control
 *   information about themselves and the administrative and commercial
 *   systems that depend on data extraction. The right was judicially created
 *   decades before data protection technology matured — the court recognized
 *   the threat at the moment when mass digitization, networked databases, and
 *   surveillance capacity became structurally inevitable. This constraint is
 *   one reading of a contested constitutional kernel: the basic rights
 *   catalog. Other readings emphasize the essence guarantee (no right may be
 *   hollowed to nothing), objective values order (rights radiate into private
 *   law), and proportionality doctrine (all rights subject to four-step
 *   balancing). The informational self-determination reading privileges
 *   dignity and control as the foundational claim; it competes with
 *   proportionality reading over whether the right can be balanced away.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary beneficiary and victim (powerless/trapped pre-litigation, organized/constrained post-litigation) — individuals whose personal information is collected and processed without meaningful consent or control
 *   - Constitutional Court: Institutional architect (institutional/arbitrage) — announces the right and establishes its enforceability; benefits from legitimacy and authority
 *   - Data Protection Authority: Regulatory coordinator (organized/mobile) — enforces the right and mediates between subjects and processors; experiences coordination benefit
 *   - Administrative Agencies (Census Bureau, Tax Authority, Social Services): Institutional victim (institutional/constrained) — bears the cost of consent requirements, audit obligations, and data minimization mandates; benefits from continued data processing for service delivery
 *   - Private Data Collectors (Platforms, Commerce): Institutional victim (institutional/arbitrage) — faces consent and transparency requirements; derives high commercial benefit from data; has exit options (relocation, alternative business models, regulatory arbitrage)
 *   - International Data Governance Coalition: Organized agents (organized/constrained) — EU bodies, international organizations, transnational NGOs building alternative frameworks; see the right as temporary scaffolding toward a mature global regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_rights_catalog__informational_self_determination, 0.58).
domain_priors:suppression_score(basic_rights_catalog__informational_self_determination, 0.62).
domain_priors:theater_ratio(basic_rights_catalog__informational_self_determination, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_rights_catalog__informational_self_determination, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_rights_catalog__informational_self_determination, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(basic_rights_catalog__informational_self_determination, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_rights_catalog__informational_self_determination, tangled_rope).
narrative_ontology:human_readable(basic_rights_catalog__informational_self_determination, "Informational Self-Determination (Constitutional Right to Data Control)").
narrative_ontology:topic_domain(basic_rights_catalog__informational_self_determination, "constitutional_law/fundamental_rights/data_protection").

domain_priors:requires_active_enforcement(basic_rights_catalog__informational_self_determination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_rights_catalog__informational_self_determination, '033fe8f0-a414-48ed-97eb-8f968a9268e5').
narrative_ontology:cs_kernel_codification('033fe8f0-a414-48ed-97eb-8f968a9268e5', formalized).
narrative_ontology:cs_authority_grounding('033fe8f0-a414-48ed-97eb-8f968a9268e5', lineage).
narrative_ontology:cs_interpretation_layer_present('033fe8f0-a414-48ed-97eb-8f968a9268e5').
narrative_ontology:cs_reading_relation('033fe8f0-a414-48ed-97eb-8f968a9268e5', basic_rights_catalog__essence_guarantee, coexists_with).
narrative_ontology:cs_reading_relation('033fe8f0-a414-48ed-97eb-8f968a9268e5', basic_rights_catalog__objective_values_order, influences).
narrative_ontology:cs_reading_relation('033fe8f0-a414-48ed-97eb-8f968a9268e5', basic_rights_catalog__proportionality_doctrine, forecloses).
narrative_ontology:cs_axiom('033fe8f0-a414-48ed-97eb-8f968a9268e5', foundational, dignity_requires_informational_control).
narrative_ontology:cs_axiom_status(dignity_requires_informational_control, holdable).
narrative_ontology:cs_axiom_grounding('033fe8f0-a414-48ed-97eb-8f968a9268e5', dignity_requires_informational_control, deontological).
narrative_ontology:cs_axiom('033fe8f0-a414-48ed-97eb-8f968a9268e5', foundational, data_processing_subordinate_to_consent).
narrative_ontology:cs_axiom_status(data_processing_subordinate_to_consent, holdable).
narrative_ontology:cs_axiom_grounding('033fe8f0-a414-48ed-97eb-8f968a9268e5', data_processing_subordinate_to_consent, conventional).
narrative_ontology:cs_reference_frame('033fe8f0-a414-48ed-97eb-8f968a9268e5', dignity_as_personality_control).
narrative_ontology:cs_drift_state('033fe8f0-a414-48ed-97eb-8f968a9268e5', contemporary_algorithmic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('033fe8f0-a414-48ed-97eb-8f968a9268e5', '').
narrative_ontology:cs_kernel_id(basic_rights_catalog__informational_self_determination, basic_rights_catalog).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_rights_catalog__informational_self_determination, data_subjects).
narrative_ontology:constraint_beneficiary(basic_rights_catalog__informational_self_determination, constitutional_court).
narrative_ontology:constraint_victim(basic_rights_catalog__informational_self_determination, administrative_agencies).
narrative_ontology:constraint_victim(basic_rights_catalog__informational_self_determination, private_data_collectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individual faces extraction of personal information without consent or control mechanism. No exit from data collection (captured during routine administrative interaction). Court's declaration of the right is aspirational but enforcement mechanisms are weak: data subject has no practical recourse once information is collected and shared. Suppression is structural — administrative machinery is built to extract data, and alternatives (opting out, anonymization) are not genuine options.
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED DATA SUBJECT / ADVOCACY COALITION (TANGLED ROPE) — Once the right is established and litigation machinery activates, data subjects can challenge collection practices. Benefits: legal standing, discovery, injunctive remedies. Costs: litigation is resource-intensive, high barriers to individual action, collective action requires organizing (which itself reveals personal data). Mixed coordination (data protection norms) and extraction (data processing still occurs during the challenge process). Moderate experienced extraction because the organized subject has partial agency.
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DATA PROTECTION AUTHORITY (ROPE) — Independent regulatory body (e.g., Bundesdatenschutzbeauftragte) enforces the right and coordinates compliance across sectors. Experiences the constraint as coordination: establishing standards, conducting audits, issuing guidance. Low extraction experienced because authority has exit options (can investigate, sanction, or redirect enforcement) and benefits from the right's existence (institutional legitimacy, functional authority). Net coordination benefit.
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ADMINISTRATIVE AGENCY (TANGLED ROPE) — Census authority, tax agency, or social services department faces both coordination and extraction. Coordination function: data integration enables efficient service delivery and fraud prevention. Extraction: must now obtain consent, maintain security, honor deletion requests, and comply with audits. Suppression from the agency's perspective comes from mandatory transparency and loss of control over data use. Benefits from continued data processing; victimized by the right's enforcement costs. Active enforcement required to prevent workarounds.
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVATE DATA COLLECTOR / PLATFORM (TANGLED ROPE) — Commercial entities derive coordination benefit from data (personalization, fraud detection, network effects). Extraction: the right imposes consent requirements, transparency duties, and data portability obligations that reduce data's commercial value and increase compliance costs. High arbitrage capacity — firms can relocate, use differential national standards, or transition to alternative revenue models. Suppression is moderate (regulatory workarounds exist; compliance is costly but navigable). Beneficiary of data accumulation; victim of consent requirements.
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL DATA GOVERNANCE COALITION (SCAFFOLD) — EU data protection standard-setters, international organizations (UN, OECD), and transnational NGOs see the right as temporary scaffolding for a sunset transition: from unconsented extraction to a mature international data governance regime with federated consent, decentralized identity, and algorithmic transparency. Extraction is moderate because the coalition has agency and perceives an exit path — new technologies and regulatory harmonization will eventually replace the census-era data collection model. Theater ratio is high (international standard-setting is performative before adoption).
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective grounded in dignity theory, informational self-determination is an immutable corollary of personhood: one cannot be a moral agent without some threshold control over how one is represented in systems that affect one's life. This perspective sees the right as emerging naturally from the concept of dignity itself, not as a contingent policy choice. However, this classification is vulnerable to false-summit detection: the right was NOT recognized in early constitutional law; it was judicially created (1983) in response to specific technological and administrative pressures. The 'inalienable' framing may naturalize what is actually a mid-20th-century invention.
constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_rights_catalog__informational_self_determination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_rights_catalog__informational_self_determination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_rights_catalog__informational_self_determination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_rights_catalog__informational_self_determination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint suppresses unconsented personal data extraction and establishes individual control as a prerequisite to processing. But the extraction is not absolute (category of snare) because administrative and commercial processing continues with consent, legitimate interest, or lawful basis — the right does not eliminate data collection, it conditions it on control and transparency. The measurement trajectory (0.78 → 0.68 → 0.58) reflects increasing legal clarity, enforcement capacity, and normalization of consent practices over the 10-year interval. Suppression (0.62): Moderate-high. Structural barriers exist: data subjects face high costs to exercise the right (litigation, proving violation, organizing), administrative/commercial workarounds (legitimate interest doctrine, secondary processing), and information asymmetries (complex terms of service). But suppression is declining as enforcement mechanisms mature and digital consent tools become standardized. The measurement trajectory (0.75 → 0.68 → 0.62) reflects this decline as the constraint's enforcement infrastructure matures. Theater ratio (0.48): Moderate-low, declining. The right's announcement was symbolic — the court created it rather than discovering it in the text. But the theater is low relative to other constitutional rights because the court backed the right with concrete enforceability (private right of action, data protection authority authority, specific remedies like erasure and portability). The measurement trajectory (0.62 → 0.55 → 0.48) reflects that as enforcement mechanisms mature and compliance becomes routine, the theatrical element (announced-but-not-enforced) decreases.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a deep perspectival gap between the beneficiary's experience (the right is essential, enforceable, and protective) and the victim's experience (the right provides only limited remedies and leaves most data extraction untouched). The data subject and administrative agency occupy opposite poles: the subject sees the constraint as liberatory but weak; the agency sees it as burdensome but manageable. The scaffold perspective (international coalition) occupies a third position: they see the right as temporary scaffolding — necessary now, but eventually obsolete when mature data governance infrastructure replaces consent-based control. The natural law view risks naturalizing a historical contingency: informational self-determination was not a recognized right in 1960, and many democracies have not adopted it. Calling it a natural law corollary of dignity may hide the fact that it is a 1983 innovation that could have been addressed through different mechanisms (property rights in data, tort liability, contract law). The false-summit detector is necessary to reveal this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The data subject as primary victim experiences high extractiveness (d ≈ 0.85 in the trapped configuration) because they bear the cost of unconsented processing and lack exit options within the constraint. The administrative agency as primary victim experiences moderate extractiveness (d ≈ 0.65 in the constrained configuration) because they face compliance costs but retain institutional capacity and legitimate processing bases. The data collector experiences low extractiveness (d ≈ 0.35 in the arbitrage configuration) because they have exit options (relocation, alternative models) and can structure processing within the right's framework. The court and data protection authority, as beneficiaries of the right's existence (institutional legitimacy, functional authority), experience negative effective extraction (d ≈ 0.15). The scaffold perspective (international coalition) experiences moderate extraction (d ≈ 0.55) because they have agency through standard-setting but also bear costs of building alternative infrastructure. The natural law perspective (analytical observer) derives d from the analytical power atom's canonical value, but the false-summit detection mechanism will highlight the tension: if the right is truly inalienable, why was it not recognized before 1983? This tension is routed to the omega variables rather than resolved within the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by anchoring the right to a specific kernel (dignity and personality) rather than left-floating it as an abstract 'fundamental right.' The mandatrophy resolution shows that the right is not automatically a mountain (natural law) nor automatically subject to proportionality balancing (which would allow democratic limitation). Instead, this reading locks the right to dignity — making it subject to the essence guarantee (essence doctrine reading) but not to ordinary proportionality balancing (which would apply only within the domain the right does not preempt). The tangled_rope classification at multiple perspectives reflects the constraint's hybrid nature: it provides genuine coordination benefit (data integration enables service delivery) alongside extraction (unconsented processing). The scaffold perspective suggests a sunset trajectory: as technology matures and international standards converge, the consent-based model may be replaced by infrastructure-based governance (differential privacy, federated learning, algorithmic transparency). This trajectory is consistent with the measurement decline in extractiveness and theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_as_kernel_or_policy,
    'Is informational self-determination an inalienable corollary of human dignity (natural law reading), or a contingent policy response to 20th-century data accumulation technology that could have been addressed differently?',
    'Historical analysis of pre-census constitutional jurisprudence and its capacity to address data protection; comparison with non-dignity-based data regimes (contractual consent, property rights in data, tort liability); examination of whether dignity theory can systematically derive the specific contours of informational self-determination (consent, erasure, portability) or whether these are policy choices layered onto dignity',
    'If inalienable: mountain classification confirmed; the constraint is not subject to proportionality balancing (essence guarantee reading forecloses proportionality doctrine reading). If contingent policy: false summit — the constraint is a tangled rope with deep institutional embeddedness, not a natural law; it can be rebalanced or repealed through political process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_as_kernel_or_policy, conceptual, 'Whether informational self-determination is dignity''s inalienable corollary or a contingent policy innovation').

omega_variable(
    enforcement_capacity_vs_stated_right,
    'Do existing enforcement mechanisms (data protection authorities, private right of action, administrative review) actually operationalize the stated right to informational self-determination, or do they provide only theatrical remedies?',
    'Longitudinal study of consent compliance rates and enforcement outcomes; analysis of remedies granted (injunctions, damages, corrective notices) vs. frequency of violation; comparison of stated right (absolute control) with actual remedy (notice, correction, limited damages); measurement of data subject awareness of and ability to exercise the right',
    'If enforcement is effective: tangled rope classification confirmed with lower theater ratio; the right functions as both coordination and constraint. If enforcement is theatrical: piton classification may be more accurate; the right persists institutionally but has atrophied from announced intention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_stated_right, empirical, 'Whether enforcement mechanisms operationalize the stated right or provide only theater').

omega_variable(
    technological_maturation_and_consent_model,
    'Does the consent-based model of informational self-determination remain adequate as data processing becomes automated, algorithmic, and context-dependent (behavioral targeting, inference, federated learning)?',
    'Analysis of consent effectiveness for algorithmic processing; study of whether individuals can meaningfully consent to inferences drawn from their data; assessment of whether consent is a technically defensible mechanism for machine learning vs. alternative approaches (regulatory specification, algorithmic transparency, differential privacy)',
    'If consent remains adequate: the right can persist unchanged. If consent becomes inadequate: the reading may transition toward proportionality or objective values framings (other sibling readings), or new rights (algorithmic due process, inference transparency) may supersede informational self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_maturation_and_consent_model, empirical, 'Adequacy of consent model for algorithmic and federated data processing').

omega_variable(
    cross_reading_axiom_collision,
    'Does the informational self-determination reading''s core axiom (data control as prerequisite to dignity) logically foreclose the proportionality doctrine reading (all rights subject to four-step balancing), or do these readings coexist as different framings of the same constitutional text?',
    'Doctrinal analysis of German constitutional jurisprudence: can proportionality testing be applied to informational self-determination without reducing the right to nothing (triggering the essence guarantee)? Examination of case law where proportionality limit was applied to data protection; assessment of whether balancing was doctrinally coherent or internally contradictory',
    'If axioms foreclose: the readings are incompatible; a single constitutional framework cannot hold both. If axioms coexist: the readings are different analytical entry points into the same right; a single framework can apply both (by confining proportionality above the essence floor).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_reading_axiom_collision, conceptual, 'Whether dignity-based data control axiom forecloses proportionality doctrine axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_rights_catalog__informational_self_determination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infoselfdet_tr_t0, basic_rights_catalog__informational_self_determination, theater_ratio, 0, 0.62).
narrative_ontology:measurement(infoselfdet_tr_t5, basic_rights_catalog__informational_self_determination, theater_ratio, 5, 0.55).
narrative_ontology:measurement(infoselfdet_tr_t10, basic_rights_catalog__informational_self_determination, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(infoselfdet_be_t0, basic_rights_catalog__informational_self_determination, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(infoselfdet_be_t5, basic_rights_catalog__informational_self_determination, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(infoselfdet_be_t10, basic_rights_catalog__informational_self_determination, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(infoselfdet_su_t0, basic_rights_catalog__informational_self_determination, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(infoselfdet_su_t5, basic_rights_catalog__informational_self_determination, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(infoselfdet_su_t10, basic_rights_catalog__informational_self_determination, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_rights_catalog__informational_self_determination, identity_coordination).
narrative_ontology:affects_constraint(basic_rights_catalog__informational_self_determination, essence_guarantee).
narrative_ontology:affects_constraint(basic_rights_catalog__informational_self_determination, objective_values_order).
narrative_ontology:affects_constraint(basic_rights_catalog__informational_self_determination, proportionality_doctrine).

% DUAL FORMULATION NOTE:
% The basic_rights_catalog kernel decomposes into four distinct constraint readings. The informational_self_determination reading (this story) focuses on the right to data control as a dignity prerequisite. The essence_guarantee reading focuses on the unapproachable core of all rights. The objective_values_order reading focuses on rights' horizontal radiation into private law. The proportionality_doctrine reading focuses on the method of balancing all rights. These readings have distinct ε values, distinct victims and beneficiaries, and distinct doctrinal trajectories. They are linked by kernel affinity, not by empirical or causal dependency. Each reading is a complete constraint story; network edges indicate doctrinal rivalry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_rights_catalog__informational_self_determination, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
