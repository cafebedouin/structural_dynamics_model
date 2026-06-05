% ============================================================================
% CONSTRAINT STORY: clinical_authority_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clinical_authority_topology, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: clinical_authority_topology
 *   human_readable: Clinical Authority Topology in Healthcare AI
 *   domain: healthcare_governance/medical_licensing/ai_regulation
 *
 * SUMMARY:
 *   The clinical authority topology is the legal and professional requirement
 *   that licensed physicians retain ultimate decision-making authority over
 *   AI-patient interactions, regardless of AI performance levels. This
 *   constraint is embedded in medical licensing laws, malpractice liability
 *   frameworks, regulatory approval pathways (FDA device classification, CE
 *   marking), hospital credentialing requirements, and insurance
 *   reimbursement structures. It appears across all major healthcare systems
 *   as an immutable feature of medical practice. The constraint is presented
 *   as a necessary protection for patient safety and a structural requirement
 *   of moral responsibility — someone must be accountable for life-or-death
 *   decisions, and that someone must be a licensed human professional.
 *   However, the constraint exhibits a false summit signature: it has
 *   identifiable beneficiaries (physicians who maintain professional monopoly
 *   and income; licensing boards that maintain gatekeeping authority;
 *   malpractice insurers who can assign liability to individuals), it shows
 *   rising extractiveness as AI performance improves (the gap between
 *   'physician adds value' and 'physician is legally required' widens), and
 *   it resists change even when empirical evidence suggests AI-alone pathways
 *   could be safe or superior for specific tasks. The analytical observer
 *   risks naturalizing this arrangement — treating the physician-authority
 *   requirement as an inherent feature of healthcare rather than as a
 *   contingent institutional structure that serves identifiable interests.
 *
 * KEY AGENTS:
 *   - Patients: Powerless/trapped — cannot access AI-direct care pathways within the regulated system; must receive care through physician-supervised channels regardless of preference or AI capability
 *   - AI Developers: Moderate/constrained — face regulatory approval pathways that structurally require physician oversight; cannot bring diagnostic or treatment AI to market without embedding it in physician-supervised workflows
 *   - Licensed Physicians: Institutional/arbitrage — primary beneficiaries; maintain professional monopoly, liability protection, and income streams; also experience the constraint as immutable (cannot delegate authority even if desired)
 *   - Medical Licensing Boards: Institutional/arbitrage — secondary beneficiaries; maintain gatekeeping authority and professional jurisdiction; structure licensing around individual practitioner accountability
 *   - Malpractice Insurers: Institutional/arbitrage — tertiary beneficiaries; liability assignment to individual physicians simplifies underwriting and claims management
 *   - Hospital Systems: Institutional/constrained — must structure AI deployment around physician authority to maintain accreditation and legal compliance; cannot experiment with alternative care models
 *   - Analytical Observer: Analytical/analytical — risks naturalizing the constraint as a necessary feature of moral responsibility or informed consent rather than recognizing it as a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clinical_authority_topology, 0.18).
domain_priors:suppression_score(clinical_authority_topology, 0.04).
domain_priors:theater_ratio(clinical_authority_topology, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clinical_authority_topology, extractiveness, 0.18).
narrative_ontology:constraint_metric(clinical_authority_topology, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(clinical_authority_topology, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(clinical_authority_topology, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(clinical_authority_topology, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clinical_authority_topology, mountain).
narrative_ontology:human_readable(clinical_authority_topology, "Clinical Authority Topology in Healthcare AI").
narrative_ontology:topic_domain(clinical_authority_topology, "healthcare_governance/medical_licensing/ai_regulation").

domain_priors:emerges_naturally(clinical_authority_topology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clinical_authority_topology, licensed_physicians).
narrative_ontology:constraint_beneficiary(clinical_authority_topology, medical_licensing_boards).
narrative_ontology:constraint_beneficiary(clinical_authority_topology, malpractice_insurers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (MOUNTAIN) — Patients cannot opt out of the physician-authority requirement. Whether receiving care from a physician using AI tools or seeking direct AI consultation, the legal and insurance framework mandates physician oversight. No alternative care pathways exist within the regulated system. The constraint appears as an immutable feature of how healthcare is structured.
constraint_indexing:constraint_classification(clinical_authority_topology, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AI DEVELOPER (MOUNTAIN) — Developers face regulatory approval pathways (FDA Class II/III devices, CE marking) that structurally require physician oversight for clinical deployment. Cannot bring diagnostic or treatment-recommendation AI to market without embedding it in physician-supervised workflows. The constraint is experienced as a fixed regulatory boundary, not a policy choice.
constraint_indexing:constraint_classification(clinical_authority_topology, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LICENSED PHYSICIAN (MOUNTAIN) — Physicians benefit from the constraint (professional monopoly, liability shield when following standard of care, income protection) but also experience it as immutable. Medical licensing is structured around individual practitioner accountability; malpractice law assigns liability to the physician regardless of AI involvement. Even physicians who would prefer to delegate more authority to AI systems cannot do so within the legal framework. The constraint is experienced as a fixed feature of professional practice, not as a policy that could be changed.
constraint_indexing:constraint_classification(clinical_authority_topology, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HOSPITAL SYSTEM (MOUNTAIN) — Healthcare institutions must structure AI deployment around physician authority to maintain accreditation, insurance contracts, and legal compliance. Cannot create AI-direct-to-patient pathways even if clinical outcomes were equivalent. The constraint is embedded in credentialing requirements, malpractice coverage terms, and regulatory compliance frameworks. Experienced as a structural necessity rather than a policy choice.
constraint_indexing:constraint_classification(clinical_authority_topology, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the requirement for human clinical authority appears to reflect deep structural features: the irreducibility of moral responsibility (someone must be accountable for life-or-death decisions), the limits of algorithmic transparency (black-box models cannot explain their reasoning in ways that satisfy informed consent), and the social contract of medicine (patients trust persons, not systems). However, the presence of identifiable beneficiaries (physicians, licensing boards, insurers) and the constraint's resistance to change even as AI performance improves suggests this may be a false summit — a contingent institutional arrangement naturalized as necessity.
constraint_indexing:constraint_classification(clinical_authority_topology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clinical_authority_topology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(clinical_authority_topology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(clinical_authority_topology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(clinical_authority_topology, ExtMetricName, E),
    domain_priors:suppression_score(clinical_authority_topology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(clinical_authority_topology),
    narrative_ontology:constraint_metric(clinical_authority_topology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(clinical_authority_topology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(clinical_authority_topology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low but non-zero and rising. The constraint extracts rents from patients (who pay for physician oversight that may not add clinical value) and from the healthcare system (which must structure workflows around physician authority even when AI could perform tasks autonomously). The extraction is modest because physician oversight does add value in many contexts — the constraint is not purely extractive. However, extractiveness is rising over the measurement interval as AI performance improves: the gap between 'physician oversight adds value' and 'physician oversight is legally required' widens. At T=0 (early AI deployment), physician oversight was almost always value-additive; at T=10 (mature AI systems), physician oversight is increasingly a legal requirement rather than a clinical necessity for specific tasks. Suppression (0.04): Very low. Alternatives to physician-supervised care are not violently suppressed — they are simply not legally available within the regulated healthcare system. Patients can access some health information and decision-support tools outside the clinical context (wellness apps, symptom checkers), but these are explicitly not medical care. The suppression is structural (legal barriers) rather than coercive. Theater ratio (0.15): Low but rising. Most physician oversight of AI systems is functional — physicians review AI recommendations, integrate them with patient history and preferences, and make final decisions. However, theater is increasing as AI systems become more capable: in some contexts (e.g., radiology image analysis, pathology slide review), the physician's role is shifting from 'expert who uses AI as a tool' to 'required signoff on AI output.' The rising theater ratio reflects the constraint's drift from coordination (physician adds value) toward extraction (physician is required by law). Accessibility collapse (0.92): Very high. The constraint is nearly universal within regulated healthcare systems. Patients, developers, hospitals, and even physicians themselves experience it as a fixed boundary. Resistance (0.08): Very low. The constraint is not actively contested by most stakeholders — it is accepted as a natural feature of medical practice. Emerges naturally (true): The constraint is presented as emerging from the inherent requirements of moral responsibility, informed consent, and patient safety rather than from policy choices or professional interests.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this constraint as mountain — it appears immutable from every structural position. Patients cannot opt out; developers cannot bypass regulatory requirements; physicians cannot delegate authority even if they want to; hospitals cannot restructure care pathways; the analytical observer sees deep structural necessity (moral responsibility, informed consent). This uniform classification is the signature of either a genuine natural law or a successfully naturalized false summit. The false summit detector evaluates: (1) Are there identifiable beneficiaries? Yes — physicians, licensing boards, insurers. (2) Does the constraint resist change even as the underlying justification weakens? Yes — regulatory frameworks have not adapted to AI performance improvements; physician-oversight requirements remain constant even for tasks where AI demonstrably matches or exceeds human performance. (3) Is the constraint presented as emerging from natural necessity rather than from policy choices? Yes — framed as inherent to moral responsibility and patient safety. The combination triggers false summit classification: the constraint is experienced as mountain from all perspectives, but the structural data reveals it as a contingent institutional arrangement that serves identifiable interests. The perspectival gap is not between different types (all see mountain) but between the experienced immutability and the structural contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation reveals the false summit structure. Licensed physicians are declared as beneficiaries (they maintain professional monopoly, liability protection, and income) with arbitrage exit options (they can choose whether to use AI tools, can practice in jurisdictions with different regulatory frameworks, can exit to non-clinical roles). This produces low d → low or negative chi: physicians experience the constraint as coordination or even as a burden (they are legally required to supervise AI even when they trust it). Patients are not declared as victims because the constraint is presented as protective — but they bear the costs (higher healthcare expenses, restricted access to AI-direct care). The absence of declared victims combined with the presence of declared beneficiaries triggers the false summit detector: a mountain with beneficiaries is a candidate for naturalized extraction. Medical licensing boards and malpractice insurers are also beneficiaries with arbitrage options (they can restructure liability frameworks, can operate in multiple jurisdictions, can exit to other regulatory domains). AI developers are moderate power with constrained exit (they face regulatory barriers but can develop for non-clinical markets or for jurisdictions with different rules). Hospital systems are institutional power with constrained exit (they must comply with licensing and accreditation requirements but have some flexibility in how they structure workflows). The analytical observer uses analytical power with analytical exit and risks seeing the constraint as a mountain — an immutable requirement of moral responsibility — when the structural data suggests it is a false summit: a contingent institutional arrangement that benefits identifiable agents and resists change even as the underlying justification (AI cannot perform safely without physician oversight) weakens.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that mountain classification can be either genuine (natural law) or false (naturalized extraction). The mandatrophy is not 'is this a mountain or something else?' but 'is this mountain real or constructed?' The false summit detector provides the resolution mechanism: a mountain with identifiable beneficiaries, rising extractiveness over time, and resistance to change despite weakening justification is a candidate for reclassification. The constraint's uniform mountain classification across all perspectives is not evidence of natural law — it is evidence of successful naturalization. The analytical observer's mountain perspective is the most dangerous: by treating physician authority as a structural necessity of moral responsibility, the observer naturalizes a contingent institutional arrangement and prevents recognition of the extraction mechanism. The omega variables document the irreducible uncertainties: Is moral responsibility genuinely irreducible to human agents? Is there a performance threshold at which physician oversight becomes purely extractive? Are regulatory frameworks driven by safety concerns or by guild interests? These questions cannot be resolved from within the mountain frame — they require recognizing the constraint as contingent and examining the beneficiary structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_responsibility_irreducibility,
    'Is human clinical authority a genuine requirement of moral responsibility, or can accountability be restructured to accommodate non-human decision-makers?',
    'Philosophical analysis of moral agency; legal experiments with algorithmic liability frameworks; cross-cultural comparison of accountability structures in high-stakes domains',
    'If irreducible: mountain classification confirmed — human authority is a structural necessity. If restructurable: false summit — the constraint naturalizes a contingent institutional choice that protects professional monopoly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_responsibility_irreducibility, conceptual, 'Whether moral responsibility requires human clinical authority').

omega_variable(
    performance_parity_threshold,
    'At what level of AI performance (if any) does the physician-oversight requirement become purely extractive rather than protective?',
    'Longitudinal outcome studies comparing physician-supervised AI, physician-alone, and (in jurisdictions that permit it) AI-alone diagnostic pathways; analysis of error rates, patient satisfaction, and cost-effectiveness',
    'If no threshold exists (human oversight always adds value): mountain confirmed. If threshold exists and is exceeded: constraint becomes snare — continued physician requirement extracts rents without improving outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_parity_threshold, empirical, 'Performance threshold at which physician oversight becomes extractive').

omega_variable(
    beneficiary_capture_mechanism,
    'Do licensing boards and malpractice frameworks resist AI autonomy because of genuine safety concerns or because of professional guild interests?',
    'Comparative institutional analysis: correlation between physician representation on regulatory boards and stringency of AI oversight requirements; analysis of regulatory responses to AI performance data; examination of lobbying patterns and regulatory capture indicators',
    'If safety-driven: mountain classification supported. If guild-driven: false summit — the constraint is maintained by beneficiaries who naturalize their interests as patient protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_mechanism, empirical, 'Whether regulatory resistance reflects safety or guild interests').

omega_variable(
    informed_consent_algorithmic_limit,
    'Is the requirement for physician interpretation of AI outputs a genuine necessity of informed consent, or could patients give informed consent to algorithmic recommendations directly?',
    'Empirical studies of patient comprehension: comparing understanding of physician-mediated AI explanations vs direct AI explanations; analysis of consent validity in contexts where patients interact with decision-support systems without physician intermediation',
    'If physician interpretation is necessary for valid consent: mountain supported. If patients can consent to algorithmic recommendations directly: the physician-intermediation requirement is a false summit protecting professional authority rather than patient autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_algorithmic_limit, empirical, 'Whether informed consent requires physician interpretation of AI').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clinical_authority_topology, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clin_auth_theater_t0, clinical_authority_topology, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clin_auth_theater_t5, clinical_authority_topology, theater_ratio, 5, 0.12).
narrative_ontology:measurement(clin_auth_theater_t10, clinical_authority_topology, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(clin_auth_extract_t0, clinical_authority_topology, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(clin_auth_extract_t5, clinical_authority_topology, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(clin_auth_extract_t10, clinical_authority_topology, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clinical_authority_topology, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is a candidate for decomposition if future analysis reveals structurally distinct claims: (1) the moral responsibility claim (someone must be accountable), (2) the informed consent claim (patients cannot consent to algorithmic recommendations directly), (3) the liability assignment claim (malpractice law requires individual human defendants). Each may have different epsilon values and different beneficiary structures. Current formulation treats them as a unified constraint because they are legally and institutionally bundled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
