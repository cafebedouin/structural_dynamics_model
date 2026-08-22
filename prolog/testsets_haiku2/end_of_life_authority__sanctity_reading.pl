% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity of Life Reading: Prohibition on Assisted Dying
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint instantiates the sanctity-of-life reading of the
 *   end-of-life-authority kernel. The core premise is that human life
 *   possesses intrinsic moral value independent of subjective preference,
 *   suffering, or functional capacity, and therefore intentional life-ending
 *   is categorically prohibited regardless of an individual's competent
 *   choice or unbearable suffering. This reading competes with the autonomy
 *   reading (which grounds the right to die in individual self-determination)
 *   and with empirical evidence about slippery-slope expansion (where initial
 *   autonomy-based frameworks expand beyond terminal cases). The sanctity
 *   reading operates as both coordination (it establishes a bright-line rule
 *   protecting medical professions from having to adjudicate complex
 *   case-by-case choices) and extraction (it denies individuals in unbearable
 *   suffering the right to end their lives on their own terms, and it creates
 *   asymmetric coercion risk for vulnerable populations). The constraint is
 *   enforced through criminal law (prosecuting assisted dying), professional
 *   discipline (removing or criminalizing physicians who facilitate death),
 *   and institutional gatekeeping (medical systems that refuse to honor
 *   requests for assisted dying). The temporal measurements show rising
 *   extraction and rising suppression requirement over the interval,
 *   indicating that maintaining the constraint's prohibition has required
 *   increasing institutional effort and has become more extractive as
 *   autonomy-reading advocacy has intensified.
 *
 * KEY AGENTS:
 *   - Medical establishment: institutional agenda-setter; enforces the prohibition; benefits from unambiguous role clarity
 *   - Religious institutions: organized beneficiaries; provide grounding narrative; lobby for maintenance; do not administer directly
 *   - Disability advocacy (conservative faction): organized beneficiaries; frame prohibition as protection against coercion; partially bear its costs
 *   - Terminally ill with unbearable suffering: powerless payers; trapped in immediate horizon; experience constraint as denial of autonomy
 *   - Elderly economically disadvantaged: powerless payers; identity-locked; face coercion risk from poverty and marginalization
 *   - Disabled at coercion risk: moderate-power payers; constrained exit; ambiguous relationship to constraint (protection vs. denial)
 *   - Autonomy-reading advocates: organized excluded voices; their institutional exclusion is what enforcement maintains
 *   - Bioethics authorities: institutional agenda-setters; codify and defend the rule; maintain institutional coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity of Life Reading: Prohibition on Assisted Dying").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '1914658c-6955-406c-9946-07bfebfdba33').
narrative_ontology:cs_kernel_codification('1914658c-6955-406c-9946-07bfebfdba33', fixed_text).
narrative_ontology:cs_authority_grounding('1914658c-6955-406c-9946-07bfebfdba33', extraction).
narrative_ontology:cs_interpretation_layer_present('1914658c-6955-406c-9946-07bfebfdba33').
narrative_ontology:cs_reading_relation('1914658c-6955-406c-9946-07bfebfdba33', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('1914658c-6955-406c-9946-07bfebfdba33', end_of_life_authority__slippery_slope_mechanism, coexists_with).
narrative_ontology:cs_axiom('1914658c-6955-406c-9946-07bfebfdba33', foundational, life_intrinsic_value_unconditional).
narrative_ontology:cs_axiom_status(life_intrinsic_value_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('1914658c-6955-406c-9946-07bfebfdba33', life_intrinsic_value_unconditional, deontological).
narrative_ontology:cs_axiom('1914658c-6955-406c-9946-07bfebfdba33', foundational, intentional_death_categorically_impermissible).
narrative_ontology:cs_axiom_status(intentional_death_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('1914658c-6955-406c-9946-07bfebfdba33', intentional_death_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('1914658c-6955-406c-9946-07bfebfdba33', intrinsic_dignity_protection_framework).
narrative_ontology:cs_drift_state('1914658c-6955-406c-9946-07bfebfdba33', contemporary_autonomy_advocacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1914658c-6955-406c-9946-07bfebfdba33', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, medical_establishment).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_advocacy_organizations_conservative).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_unbearable_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_economically_disadvantaged).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disabled_at_coercion_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, palliative_care_professionals).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disability_advocacy_organizations_conservative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicians and hospital administrators enforce the prohibition by refusing or criminalizing assisted dying. They frame life preservation as the foundational duty; departing from this duty (even for individual choice) triggers professional sanction, liability, and criminal exposure. The constraint consolidates their institutional role as unambiguous guardians of life. They administer pain management and palliative care as the authorized alternatives.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious traditions that hold the sanctity of life as a foundational axiom benefit from legal prohibition: their moral claim becomes enforceable through state coercion. They do not administer the constraint directly but provide its grounding narrative and lobby for its maintenance. Exit from religious influence on policy is technically available (secular policy could adopt other grounds) but institutionally costly.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, beneficiary,
    organized, civilizational, mobile, national).

% Disability advocates who frame assisted dying as a threat to disabled people's right to live argue that the prohibition protects against coercive pressure on economically marginal disabled people. They benefit from a rule that rejects life-ending as a response to disability. However, they also bear part of the constraint's cost: some disabled people experience it as denying autonomy and trapping them in unbearable situations when their disability makes life genuinely unlivable by their own lights.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_advocacy_organizations_conservative, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, disability_advocacy_organizations_conservative, payer).

% Individuals facing terminal illness with unbearable pain or loss of function experience the prohibition as a constraint on their autonomy to end suffering on their own terms. They cannot exit the constraint (they are geographically bound by illness, institutionally bound by medical gatekeeping, and legally bound by the prohibition). Palliative alternatives exist but do not fully address the constraint they face: that their choice to die is not honored regardless of their competence or reasoning.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_unbearable_suffering, payer,
    powerless, immediate, trapped, local).

% Economically marginalized elderly individuals facing age-related decline, isolation, and poverty experience the constraint as particularly coercive. They lack resources for private end-of-life planning, depend on public medical systems that enforce the prohibition, and face internalized shame about being a burden. The prohibition's enforcement is felt most acutely by those with fewest alternatives (poorest palliative options, weakest social support, highest coercion risk from others wishing to avoid caregiving costs).
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_economically_disadvantaged, payer,
    powerless, immediate, identity_locked, local).

% Disabled people living in contexts of economic precarity, family strain, or institutional neglect face real coercive pressure to regard their lives as not worth living — not from the prohibition directly, but from external pressure. The prohibition's framing (life has intrinsic value, assisted dying is never justified) can clash with their lived experience of disability in non-supportive contexts. They carry an ambiguous relationship to the constraint: it protects against coerced death, but it also denies them autonomy if they genuinely wish to die and are competent to decide.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disabled_at_coercion_risk, payer,
    moderate, biographical, constrained, local).

% Advocates who hold that individual autonomy and self-determination ground a right to assisted dying are excluded from shaping the constraint. They argue that competent individuals should control the timing and manner of death when facing unbearable suffering. Their institutional exclusion (from medical practice, from policy-making on end-of-life care) is what the enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_reading_advocates, excluded,
    organized, generational, constrained, national).

% Palliative care as a medical specialty benefits from the sanctity-of-life reading because it is positioned as the only legitimate response to unbearable suffering. Investment in palliative care infrastructure, training, and professional recognition is justified under the prohibition. However, they also observe the constraint's limits: even excellent palliative care cannot eliminate all suffering for all patients, and some individuals will experience the constraint as denying legitimate choice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_professionals, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, palliative_care_professionals, observer).

% Bioethics commissions and legislative bodies that codify the sanctity-of-life reading into law and medical practice standards are enforcers of the constraint. They author the rule, defend it against the autonomy reading, and maintain its institutional coherence. Their authority derives from representing the collective will (stated as a commitment to life's intrinsic value) but operates through coercive mechanisms (criminal law, professional discipline, institutional policy).
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_commissions_authority, agenda_setter,
    institutional, generational, analytical, national).

% Researchers studying end-of-life outcomes, patient preferences, coercion risk, and the empirical effects of policy observe the constraint's operation. They document that prohibition has not eliminated end-of-life decision-making (it has driven it underground, into informal non-reporting, physician-assisted dying in secret) and that vulnerable populations bear asymmetric coercion risk both from the prohibition and from external pressure to die. Their findings feed the contestation but do not directly shape the constraint.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, empirical_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, medical_establishment).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a categorical ethical boundary: medicine's role is life preservation, never deliberate life-ending. This creates a bright-line rule that protects patients from physician-initiated death-hastening and protects medical institutions from having to adjudicate complex end-of-life choices on a case-by-case basis with unequal outcomes. The rule coordinates professional identity (physicians are healers, not death-deliverers) and institutional liability (clear prohibition reduces malpractice uncertainty).
% TRANSFER_FUNCTION: Moves decision-making authority from individual autonomy (the patient's choice of timing and manner of death) to institutional gatekeeping (physicians and bioethics authorities decide when dying is acceptable, usually by withdrawing life support rather than by active intervention). The constraint also transfers burden of unbearable suffering from institutional responsibility (society's obligation to provide good death) to individual endurance and hope. Religious and medical institutions extract legitimacy from being the guardians who protect life's value.
% ABSENT_VOICES: Individuals living with severe disability who regard their own lives as good and worry that assisted-dying frameworks might pressure them into seeing their lives as not worth living are partially excluded — they can voice concerns but not participate in setting the boundary. Individuals who genuinely wish to die, are competent, face unbearable suffering, and see the prohibition as denying their autonomy are systematically excluded — their voices are not invited into policy-setting; they are subjects of protection, not participants in governance. Autonomy-reading advocates and supporters of medical aid in dying are excluded from mainstream institutional authority.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished, medicine's entire institutional identity and liability structure would shift. Some individuals currently trapped in unbearable suffering would have a legal path to death they do not have now. Physicians' professional role would expand to include not only life-preservation but also death-facilitation under specified conditions. Vulnerable populations would face new coercion risks (pressure to choose death to avoid being a burden) alongside new autonomy rights. The constraint holds in place an entire institutional architecture (medical gatekeeping, religious moral authority, disability protection framing); its removal would force reorganization.
% FOUNDING_PROBLEM: Patients trapped in unbearable suffering had sought ways to end life, sometimes through desperate means (overdose, physician collusion, physician-assisted suicide in underground practice). Medical institutions lacked clear ethical boundaries and faced liability and moral confusion. The constraint was built to establish that medicine's duty is always life-preservation, never to facilitate death, and that life has intrinsic value independent of the patient's subjective evaluation of its worth.
% FOUNDING_PROBLEM_CORROBORATION: The medical establishment and religious institutions attest the founding problem remains live: physicians still face pressure to engage in end-of-life decision-making, vulnerable patients still face coercion to accept death as preferable to burden, and protecting life's inherent value remains essential to medicine's ethical integrity. Autonomy advocates, palliative care researchers, and patient advocates attest the founding problem has been substantially reframed: the problem is no longer uncontrolled end-of-life practice but rather the constraint's denial of individual autonomy and its creation of asymmetric coercion risk for economically vulnerable people. International comparative data from jurisdictions with medical aid in dying frameworks show both: reduced desperation and unregulated physician involvement (supporting the constraint's framing) AND stable outcomes for disabled people (contradicting coercion-risk predictions). The corroboration is mixed; no external party unanimously validates the constraint's framing.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 final) is high because the constraint denies individuals the right to control their death's timing and manner despite competent choice and unbearable suffering. The extraction is not monetary but autonomic: the right to self-determination over one's body and life. Suppression (0.72) is higher than extractiveness because maintaining the constraint requires active institutional enforcement — criminal law, professional discipline, and exclusion of dissenting voices — not merely passive preference. Theater ratio (0.28) is moderate-low because the coordination function (bright-line rule protecting medical professions) is genuine, but a growing share of enforcement activity defends the categorical prohibition against empirical evidence that some cases genuinely warrant individual choice. The measurements show rising extraction and suppression over time because autonomy-reading advocacy has intensified, requiring more active institutional defense of the sanctity reading. Accessibility collapse (0.79) is high because once individuals understand they have no legal path to assisted dying, alternatives genuinely collapse — black-market options are criminal, physician collusion is prosecuted, and self-harm becomes the only autonomous option for some. Resistance (0.62) is moderate because strong advocacy exists (autonomy advocates, some physicians, some patients) but the institutional weight of medical establishment, legal systems, and religious institutions suppresses this resistance through gatekeeping and criminalization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (medical establishment) experiences this as coordination because the constraint clarifies institutional roles and reduces liability uncertainty. The payer seats (terminally ill, elderly poor, disabled vulnerable) experience this as extraction because the constraint denies their autonomy and traps them in unbearable situations. The excluded seats (autonomy advocates) experience this as suppression because their voices are not merely absent but actively criminalized. These are not different opinions about the same constraint — they are the structural reality of asymmetric power and exit options. The engine will compute different types from different seats because the seats have genuinely different structural relationships to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical establishment (institutional power, arbitrage exit): d near 0.15 (beneficiary — they benefit from the clarity the constraint provides and have the exit option to practice in other jurisdictions or to reframe their role). Religious institutions (organized power, mobile exit): d near 0.25 (partial beneficiary — they benefit from the rule's grounding in their axiom but can exit by deprioritizing political influence on this issue). Terminally ill (powerless, trapped immediate): d near 0.95 (full target — they bear the extraction directly, cannot exit, and experience the constraint as denial of autonomy). Elderly economically disadvantaged (powerless, identity-locked): d near 0.92 (full target — they are trapped by illness, poverty, and social isolation; the constraint's coercive force is felt most acutely by them). Disabled at coercion risk (moderate power, constrained): d near 0.65 (moderate target — they experience the constraint ambiguously: protection against external coercion and denial of autonomy simultaneously). Autonomy advocates (organized, constrained): d near 0.78 (target — they are excluded from institutional authority, their practice is criminalized, and their voice is suppressed). The directionality derivation from beneficiary/victim + exit options + institutional power produces a highly asymmetric structure: beneficiaries have significant institutional power and exit options; payers have little power and are trapped or identity-locked.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem is live (unbearable suffering exists, patients still seek end-of-life control) but contested (autonomy advocates argue the founding problem is now the constraint's denial of autonomy, not uncontrolled end-of-life practice). The constraint is maintained by active institutional enforcement (criminal law, professional discipline, gatekeeping) not by participant preference — beneficiaries benefit enough to defend it, and payers are powerless enough to be trapped by it. This is the structural signature of a tangled_rope: genuine coordination function (bright-line rule for medical professions, protection of life's value) combined with asymmetric extraction (denial of autonomy to those in unbearable suffering). The constraint is not a snare (pure extraction would have no coordination function and would persist only by coercion without any beneficiary coordination logic). It is not a rope (pure coordination would lack the extraction component — beneficiaries and payers would be symmetric). The tangled_rope classification prevents the false elevation to mountain (the constraint is not natural law; it is a human choice enforced through institutional mechanisms) and prevents the false reduction to snare (there is a genuine coordination function, not merely cover for extraction). The mandatrophy question is whether the founding problem (uncontrolled end-of-life practice) persists or has been displaced by a new problem (constraint's denial of autonomy to those with unbearable suffering). The contestation is structural: different seats in the constraint see different problems and different solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_axiom_defensibility,
    'Is the sanctity reading''s foundational axiom (human life has intrinsic value independent of subjective experience) philosophically defensible, or is it a cover story for institutional capture of end-of-life authority?',
    'Philosophical analysis across multiple traditions (deontological, consequentialist, virtue ethics, relational ethics, disability justice frameworks). No empirical resolution; the question is whether the axiom withstands philosophical scrutiny or dissolves under it.',
    'If the axiom is indefensible (relational and subjective value frameworks are more coherent), the constraint''s legitimacy is undermined and the extraction component becomes more visible. If the axiom is defensible, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_axiom_defensibility, conceptual, 'Whether intrinsic-value axiology is coherent or whether the sanctity reading rests on a defensible philosophical foundation.').

omega_variable(
    coercion_risk_empirical_comparison,
    'Do vulnerable populations (elderly poor, disabled, isolated) face greater real coercion risk from external pressure (family, poverty, desperation) or from the availability of assisted dying as an option?',
    'Longitudinal research in jurisdictions with both restrictive (sanctity-based) and permissive (autonomy-based) frameworks; measurement of which population (those with and without the option) reports higher experience of coercive pressure to die.',
    'If vulnerable populations face greater coercion without the option (current sanctity-based regime), the constraint fails to protect its stated beneficiaries and traps them. If they face greater coercion with the option, the constraint correctly protects them. The evidence will determine whether victim designation is structurally accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_risk_empirical_comparison, empirical, 'Whether vulnerable populations are more coerced by external pressure or by option availability.').

omega_variable(
    institutionalization_vs_philosophical_commitment,
    'Is the sanctity reading maintained primarily by philosophical commitment to the intrinsic-value axiom, or primarily by institutional interests (medical profession clarity, religious institutional authority, litigation avoidance)?',
    'Audit of institutional communication: how do beneficiaries defend the constraint? How much emphasis on the intrinsic-value axiom vs. institutional efficiency? What happens when institutional incentives shift?',
    'High institutional interest and low philosophical grounding would suggest the constraint is more snare than tangled_rope. High philosophical grounding and low institutional interest would strengthen the rope component. Mixed results would confirm tangled_rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutionalization_vs_philosophical_commitment, empirical, 'Whether the constraint is sustained by philosophical axiom or by institutional interests.').

omega_variable(
    autonomy_reading_incompatibility_test,
    'Can a framework simultaneously honor the end-of-life-authority kernel while adopting the autonomy reading? Or do sanctity and autonomy readings logically foreclose each other?',
    'Examine jurisdictions and institutions that hold both sanctity and autonomy commitments (e.g., Catholic tradition holding both life''s intrinsic value AND conscience protection; disability traditions holding both protection against coercion AND autonomy respect). If they coexist in actual practice, they do not foreclose each other.',
    'If they coexist (coexists_with relation), the kernel is genuinely contested. If they foreclose (forecloses relation), the sanctity reading''s core premise directly contradicts autonomy''s. The relation type determines whether the readings are different policies or logically incompatible worldviews.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_reading_incompatibility_test, conceptual, 'Whether sanctity and autonomy readings are logically incompatible or live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(end__tr_t0, observed).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__sanctity_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(end__tr_t5, observed).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(end__tr_t10, observed).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__sanctity_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(end__tr_t15, observed).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(end__tr_t20, observed).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__sanctity_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(end__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(end__be_t0, observed).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__sanctity_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(end__be_t5, observed).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(end__be_t10, observed).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__sanctity_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(end__be_t15, observed).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(end__be_t20, observed).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__sanctity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(end__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(end__su_t0, observed).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__sanctity_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(end__su_t5, observed).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(end__su_t10, observed).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__sanctity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(end__su_t15, observed).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(end__su_t20, observed).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__sanctity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(end__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end-of-life-authority kernel decomposes into three structurally distinct constraint readings: (1) sanctity_reading — intrinsic value axiom grounds categorical prohibition; (2) autonomy_reading — individual choice grounds right to die; (3) slippery_slope_mechanism — empirical framework documenting how autonomy-based policies expand beyond intended scope. These three constraints share the referent (end-of-life authority arrangements) but have different ε values, different beneficiary/victim structures, and different measured types because they instantiate different readings of the kernel. Each reading is a coherent ε-invariant constraint. The sanctity reading influences autonomy reading by creating institutional pressure against autonomy-based practice; autonomy reading influences slippery_slope reading by being the framework whose expansion slippery_slope mechanism documents. All three are live readings held by different parties in contemporary bioethics debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
