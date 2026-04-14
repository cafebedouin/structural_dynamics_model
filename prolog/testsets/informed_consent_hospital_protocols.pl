% ============================================================================
% CONSTRAINT STORY: informed_consent_hospital_protocols
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informed_consent_hospital_protocols, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: informed_consent_hospital_protocols
 *   human_readable: Informed Consent Hospital Protocols
 *   domain: healthcare/medical_ethics
 *
 * SUMMARY:
 *   Informed consent hospital protocols present a structural paradox: they
 *   are designed to protect patient autonomy while simultaneously embedding
 *   institutional liability protection that can undermine authentic
 *   decision-making. The constraint exists at the intersection of medical
 *   necessity (complex decisions under time pressure), ethical obligation
 *   (respecting autonomous choice), and institutional risk management
 *   (documented protection against liability). From different positions
 *   within this structure, the same protocol appears as a snare that traps
 *   powerless patients, a coordination mechanism that protects informed
 *   patients, a performative ritual that has atrophied from its intended
 *   function, a temporary scaffold being replaced by better mechanisms, or a
 *   natural law reflecting irreducible medical complexity. The theater_ratio
 *   increase over 20 years (0.52 to 0.68) reflects how consent forms have
 *   become increasingly elaborate in legal language while declining in actual
 *   comprehension impact — institutions have weaponized the consent
 *   apparatus, creating more detailed documentation without improving
 *   authentic understanding. Simultaneously, extractiveness has crept upward
 *   (0.38 to 0.52) as hospitals have learned to use consent protocols
 *   defensively, shifting liability exposure from institutions to patients
 *   through documented 'consent' to procedures patients don't genuinely
 *   understand.
 *
 * KEY AGENTS:
 *   - Emergency patients in acute crisis: Primary victims (powerless/trapped) — face non-negotiable decisions with minimal comprehension window; consent is non-deferrable and conditions access to care
 *   - Chronically ill or routine-procedure patients: Secondary victims (moderate/constrained) — have time to read and ask questions but face social/institutional pressure and information asymmetry barriers
 *   - Vulnerable populations (cognitive impairment, language barriers, low health literacy): Victims (powerless/identity_locked) — structurally mobile but cognitively trapped by health literacy dependency; internalize belief that medical institutions always know better
 *   - Hospital legal/compliance departments: Primary beneficiaries (institutional/arbitrage) — benefit from liability protection and documented institutional protection; arbitrage exit option enables choice of procedural depth and form complexity
 *   - Hospital administration: Secondary beneficiary (institutional/arbitrage) — benefit from reduced litigation risk and institutional legitimacy; arbitrage option enables resource allocation choices
 *   - Patient rights advocates and medical ethicists: Organized agents (organized/mobile) — have genuine coordination interest in improving informed consent; mobile exit allows advocacy across institutions and research publication
 *   - Health literacy reformers: Organized agents (organized/constrained) — see alternatives but constrained by regulatory barriers and institutional inertia; pushing sunset toward interactive decision support
 *   - Clinical staff: Mixed position (powerful/mobile) — possess actual decision-making power but constrained by institutional protocols; benefit from consent documentation for liability protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informed_consent_hospital_protocols, 0.52).
domain_priors:suppression_score(informed_consent_hospital_protocols, 0.65).
domain_priors:theater_ratio(informed_consent_hospital_protocols, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informed_consent_hospital_protocols, extractiveness, 0.52).
narrative_ontology:constraint_metric(informed_consent_hospital_protocols, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(informed_consent_hospital_protocols, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informed_consent_hospital_protocols, tangled_rope).
narrative_ontology:human_readable(informed_consent_hospital_protocols, "Informed Consent Hospital Protocols").
narrative_ontology:topic_domain(informed_consent_hospital_protocols, "healthcare/medical_ethics").

domain_priors:requires_active_enforcement(informed_consent_hospital_protocols).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informed_consent_hospital_protocols, hospital_administration).
narrative_ontology:constraint_beneficiary(informed_consent_hospital_protocols, medical_liability_teams).
narrative_ontology:constraint_victim(informed_consent_hospital_protocols, patients_with_limited_health_literacy).
narrative_ontology:constraint_victim(informed_consent_hospital_protocols, emergency_decision_makers).
narrative_ontology:constraint_victim(informed_consent_hospital_protocols, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGENCY PATIENT (SNARE) — Patient in acute medical crisis faces non-negotiable treatment decisions with minimal comprehension window. Consent documents are legally mandated but functionally read as unilateral procedure authorization. Patient cannot exit without forgoing necessary care; cannot delay consent to gain understanding; cannot refuse to 'consent' without risking liability shield erosion for hospital. Maximum extraction with suppression: trapped by medical necessity, cognitive stress, and information asymmetry.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INFORMED PATIENT IN ROUTINE CARE (TANGLED ROPE) — Patient with time to read materials and ask questions receives genuine coordination benefit (protection via legal transparency) alongside extractive overhead (procedural burden, legal language barriers). The consent process serves dual function: protects patient autonomy AND protects hospital from liability claims. Patient can theoretically refuse, but constrained by fear of denial of care, social pressure, and belief that refusal signals non-cooperation.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PATIENT RIGHTS ADVOCATES (ROPE) — Medical ethics organizations, patient advocacy groups, and informed consent researchers have genuine coordination interest: improving informed consent serves all parties. Mobile exit option (can advocate for different standards, work across institutions, publish research). The constraint from their perspective is primarily coordination: establishing shared norms and verification standards for what 'informed' actually means. Low extraction.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: HOSPITAL LEGAL/COMPLIANCE (ROPE) — The informed consent protocol is experienced as a coordination solution to a genuine collective action problem: managing liability while respecting autonomy norms. Hospital has arbitrage exit (can choose procedural depth, form language sophistication, documentation rigor). Benefits from the constraint through liability protection and institutional legitimacy. Sees coordination benefit: shared norms reduce litigation risk and ethical exposure.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AUTONOMY RITUAL INSTITUTIONAL VIEW (PITON) — Consent forms have become largely theatrical: institutions maintain elaborate documented consent processes as performative compliance with autonomy norms, while real decision-making power remains concentrated in clinical staff and hospital protocols. High theater_ratio (0.68) reflects that the formal consent apparatus is substantially ornamental — the actual mechanism of patient protection (time, attention, genuine dialogue) is often absent. The ritual persists through institutional inertia and legal precedent, not because it reliably produces informed decisions.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HEALTH LITERACY REFORM MOVEMENT (SCAFFOLD) — Organized actors (patient educators, plain-language advocates, digital decision support innovators) see informed consent protocols as a temporary institutional form designed to be replaced by better mechanisms: video education, interactive decision aids, real-time comprehension checking, and participatory treatment planning. The constraint from this perspective has a sunset clause — structured consent documents are scaffolding for a transition to genuinely interactive shared decision-making. Exit is constrained by regulatory barriers but not impossible (institutions already pioneering alternatives).
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: WEALTHY/EDUCATED PATIENT (TANGLED ROPE) — Patient with health literacy, time to research, ability to seek second opinions, and legal resources can navigate informed consent as genuine coordination: receiving real protection and understanding. Mobile exit options (can change providers, negotiate terms, hire advocates). Experiences both coordination benefit (genuine informed choice) and extractive overhead (procedural burden, power asymmetry in medical knowledge). Gap from emergency/powerless perspective reveals how exit options modulate the experienced constraint.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER MEDICAL COMPLEXITY VIEW (MOUNTAIN) — From a civilizational view, the constraint appears immutable: medical decisions are necessarily complex, time-constrained, and involve unavoidable information asymmetry between medical experts and patients. The gap between patient comprehension and actual medical risk is an irreducible feature of medical practice, not a contingent institutional arrangement. However, the base properties (extractiveness 0.52, suppression 0.65, theater 0.68) contradict this classification — the engine will identify this as a false summit, revealing that 'complexity is inevitable' naturalizes institutional choices about resource allocation (time per patient, educational investment, decision support infrastructure) that are contingent rather than universal.
constraint_indexing:constraint_classification(informed_consent_hospital_protocols, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informed_consent_hospital_protocols_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informed_consent_hospital_protocols, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informed_consent_hospital_protocols, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informed_consent_hospital_protocols, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(informed_consent_hospital_protocols, TR),
    TR >= 0.70.

:- end_tests(informed_consent_hospital_protocols_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts institutional protection asymmetry: hospitals gain liability shielding from documented consent while patients bear the comprehension burden and autonomy risk. The extraction is not maximal because genuine consent can and does occur in some contexts (routine procedures with time for discussion), and the coordination function (protecting patients who achieve genuine understanding) is real. Suppression (0.65): Moderate-high. Multiple barriers prevent authentic comprehension: medical complexity, time constraints in emergency/routine settings, cognitive stress, health literacy gaps, institutional power asymmetry, and strategic design of forms to emphasize liability protection over clarity. However, suppression is not total — some patients achieve genuine understanding, and alternatives demonstrably increase comprehension in pilot studies. Theater_ratio (0.68): High and increasing. Consent forms have evolved into elaborate legal documents optimized for institutional protection rather than patient comprehension. The performative element dominates: detailed signature documentation persists as proof of 'informed consent' despite evidence that much of this information is not genuinely processed or understood by patients. The increase over 20 years reflects how institutions have responded to litigation risk by adding layers of documentation, not by improving comprehension mechanisms. This is Goodhart drift: the measurement (documented signature) has replaced the actual goal (informed decision-making), and institutions have optimized for the measurement.
 *
 * PERSPECTIVAL GAP:
 *   Emergency patients perceive mandatory consent as snare. Informed routine patients perceive coordination opportunity. Hospital legal perceives liability management (rope benefit). Advocates perceive coordination goal. Reformers perceive temporary scaffolding. Institutions perceive ritual compliance. Analysts risk perceiving inevitable complexity. The gap between emergency patient (snare) and hospital administrator (rope) is a class gap: same protocol, opposite experiences. This is not ambiguity about what the constraint 'really is' — both experiences are accurate from their positions. The constraint IS simultaneously extractive (for powerless) and coordinative (for institutional beneficiaries). The classification system captures this through indexed perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: hospital_administration and medical_liability_teams benefit from documented liability protection, reducing exposure to adverse outcome litigation. This protection is real and valuable. Victims: patients_with_limited_health_literacy bear comprehension burden and autonomy risk; emergency_decision_makers face time-constrained consent; vulnerable_populations face cognitive and structural barriers. Hospitals have arbitrage exit (can invest in education, change form design, implement decision support or not) — they choose institutional protection over comprehension investment. Patients have trapped or constrained exit (medical necessity, social pressure, belief that refusal signals non-cooperation). The asymmetry is not hidden — it's structural. Hospitals benefit from the constraint; patients with low health literacy bear costs. This drives the snare and tangled_rope classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that all six types are legitimate readings of the same institutional apparatus. The resolution is not 'which type is correct' but 'which position are you measuring from?' The emergency patient's snare experience is structurally real. The hospital's rope/coordination experience is structurally real. The reformer's scaffold sunset is structurally real — interactive alternatives demonstrably improve comprehension and decision quality. The piton observation (ritual degradation) is structurally real — theater_ratio increase shows documentation has replaced function. The institutional mountain perspective is a false summit — naturalizing contingent resource allocation choices as inevitable medical complexity. The constraint does not resolve by converging on one type; it resolves by showing that institutional position, exit options, and power determine which classification is accurate for that observer. The mandatrophy is resolved by the presheaf: the constraint's true structure is the set of all indexed perspectives, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comprehension_authenticity,
    'What percentage of signed consent represents genuine comprehension versus performative signature under duress?',
    'Post-signature comprehension testing; longitudinal tracking of patients'' actual understanding of risks/benefits/alternatives compared to documented consent; qualitative interviews with patients about decision-making process',
    'If >70% genuine comprehension: constraint functions as effective coordination (Rope dominates). If <30% genuine comprehension: constraint is primarily extractive (Snare/Tangled Rope dominates). The gap directly determines whether consent protocols protect or exploit patient autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comprehension_authenticity, empirical, 'Whether informed consent signatures represent genuine comprehension').

omega_variable(
    emergency_threshold_definition,
    'What portion of ''emergency'' procedures are truly non-deferrable versus administratively classified as emergencies to bypass informed consent depth?',
    'Comparative analysis of emergency designations across hospitals; correlation between emergency classification and median time available for consent discussion; audit of cases re-classified post-facto as non-emergent',
    'If administrative over-classification is >20%: suppression mechanism is institutional gaming (higher true suppression). If <5%: emergency exception is legitimate narrowing of the constraint. This determines whether suppression is structural (medical reality) or imposed (institutional choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_threshold_definition, empirical, 'Proportion of procedures classified as emergency to bypass consent depth').

omega_variable(
    liability_shield_asymmetry,
    'Does institutional liability reduction from documented consent genuinely equal patient protection from poor decision-making, or is the consent process optimized for institutional protection?',
    'Comparison of adverse outcome rates between well-documented-consent cases and poorly-documented cases; analysis of how consent documentation is used in litigation (institutional defense vs patient advocacy); correlation between hospital consent procedures and actual liability outcomes',
    'If liability reduction > patient protection: constraint is primarily extractive (asymmetric protection). If roughly equal: constraint is genuine coordination. If patient protection > liability reduction: consent functions as meaningful safeguard despite institutional motivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_shield_asymmetry, empirical, 'Whether consent documentation protects institutions more than patients').

omega_variable(
    health_literacy_dependency,
    'Is suppression primarily structural (medical complexity inherent to decision-making) or dependent on health literacy infrastructure?',
    'Comparison of comprehension rates in institutions with robust patient education vs minimal education; intervention studies measuring impact of plain-language forms, video decision aids, and comprehension checking on authentic understanding; longitudinal tracking of how patient education investment correlates with documented understanding',
    'If suppression is infrastructure-dependent: institutional underinvestment is the extraction mechanism — hospitals could reduce suppression significantly through resource allocation choices. If suppression is structural: the constraint is closer to mountain status. The answer determines whether tangled_rope classification understates hospital agency in the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(health_literacy_dependency, empirical, 'Whether suppression is structural or dependent on institutional resource allocation').

omega_variable(
    alternative_mechanism_feasibility,
    'Are genuinely interactive consent mechanisms (shared decision-making, real-time comprehension checking, video education, participatory treatment planning) operationally viable at hospital scale?',
    'Pilot studies implementing structured alternatives; cost-benefit analysis of enhanced consent mechanisms vs current protocols; measurement of patient satisfaction, comprehension, and decision quality outcomes; scalability assessment across different hospital types and patient populations',
    'If alternatives are feasible and superior: scaffold sunset clause is real, not aspirational. If alternatives are feasible but cost-prohibitive: institutional cost-cutting is the suppression mechanism (extractive choice). If alternatives are not operationally viable: current protocols are legitimate interim solutions (closer to rope/scaffold). The answer determines whether the constraint is terminal or truly temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_feasibility, empirical, 'Whether superior consent mechanisms are operationally viable and scalable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informed_consent_hospital_protocols, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iconsent_tr_t0, informed_consent_hospital_protocols, theater_ratio, 0, 0.52).
narrative_ontology:measurement(iconsent_tr_t10, informed_consent_hospital_protocols, theater_ratio, 10, 0.6).
narrative_ontology:measurement(iconsent_tr_t20, informed_consent_hospital_protocols, theater_ratio, 20, 0.68).
narrative_ontology:measurement(iconsent_tr_t5, informed_consent_hospital_protocols, theater_ratio, 5, 0.56).

% Extraction over time
narrative_ontology:measurement(iconsent_be_t0, informed_consent_hospital_protocols, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(iconsent_be_t10, informed_consent_hospital_protocols, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(iconsent_be_t20, informed_consent_hospital_protocols, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(iconsent_be_t5, informed_consent_hospital_protocols, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informed_consent_hospital_protocols, enforcement_mechanism).
narrative_ontology:affects_constraint(informed_consent_hospital_protocols, medical_paternalism_norms).
narrative_ontology:affects_constraint(informed_consent_hospital_protocols, health_literacy_dependency).
narrative_ontology:affects_constraint(informed_consent_hospital_protocols, hospital_liability_systems).

% DUAL FORMULATION NOTE:
% Informed consent protocols can be decomposed into structurally distinct constraints: (1) the legal liability protection mechanism (high ε, snare/tangled_rope for patients), (2) the autonomy coordination function (lower ε, rope/scaffold for advocates), (3) the ritual documentation apparatus (high theater, piton). This story treats them as a unified constraint because they operate through a single institutional apparatus (the consent form and process). Upstream: medical_paternalism_norms and health_literacy_dependency; downstream: hospital_liability_systems and treatment_refusal_coercion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informed_consent_hospital_protocols, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
