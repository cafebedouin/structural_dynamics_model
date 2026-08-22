% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership via Medical Transition (Hybrid Reading)
 *   domain: social/legal/medical
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the sex/gender
 *   category kernel: category membership is determined by a combination of
 *   biological sex and successful completion of medical and social
 *   transition. Under this reading, trans women are conditionally admitted to
 *   the female category after demonstrating sustained gender identity through
 *   psychiatric evaluation and typically medical transition (hormone therapy,
 *   sometimes surgery). The constraint operates through medical gatekeeping
 *   institutions that control access to transition-affirming care and issue
 *   documentation required for legal reclassification. Non-transitioning
 *   trans individuals are structurally excluded—they have no pathway to
 *   category reclassification. The hybrid model presents itself as a
 *   progressive compromise between biology-only and identity-only readings,
 *   but it concentrates authority in medical institutions, imposes
 *   substantial costs on individuals, and creates a victim class of those
 *   excluded from recognition.
 *
 * KEY AGENTS:
 *   - medical_gatekeeping_institutions: institutional power, sets diagnostic criteria and gatekeeping procedures, controls access to transition-affirming medical care
 *   - medical_transition_seekers: moderate power, seek legal recognition by pursuing medical intervention, bear direct costs and gatekeeping delays, constrained exit (must comply to access category change)
 *   - non_transitioning_trans_individuals: powerless, identity-locked, systematically excluded from legal recognition, no alternative pathway
 *   - cisgender_category_holders: organized power, benefit from reinforced category boundaries and presumptive inclusion without medical proof
 *   - trans_women_post_transition: dual-positioned (beneficiary + payer), conditionally included but subject to medical surveillance and compliance costs
 *   - excluded advocates (feminist sex-essentialist and gender-identity-centered): organized power, structurally absent from institutional consensus-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership via Medical Transition (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social/legal/medical").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '0ee9c29b-1976-4b62-a3f8-70b6c92bbff2').
narrative_ontology:cs_kernel_codification('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', formalized).
narrative_ontology:cs_authority_grounding('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', extraction).
narrative_ontology:cs_interpretation_layer_present('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2').
narrative_ontology:cs_reading_relation('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', foundational, medical_transition_legitimacy_criterion).
narrative_ontology:cs_axiom_status(medical_transition_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', medical_transition_legitimacy_criterion, conventional).
narrative_ontology:cs_axiom('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', foundational, institutional_medical_gatekeeping_authority).
narrative_ontology:cs_axiom_status(institutional_medical_gatekeeping_authority, holdable).
narrative_ontology:cs_axiom_grounding('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', institutional_medical_gatekeeping_authority, instrumental).
narrative_ontology:cs_reference_frame('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', medical_transition_as_gender_reclassification_condition).
narrative_ontology:cs_drift_state('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', contemporary_decriminalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ee9c29b-1976-4b62-a3f8-70b6c92bbff2', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_category_holders).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, medical_transition_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, trans_women_post_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_post_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medical boards, psychiatrists, endocrinologists, and surgical specialists control access to transition-affirming care (hormone therapy, surgical procedures) and issue documentation (letters of medical necessity) required to change legal sex classification. They set diagnostic criteria, determine readiness, control gatekeeping timelines, and have institutional interest in maintaining their authority as the arbiters of legitimate gender transition. They justify the gate as protecting vulnerable individuals from irreversible decisions and ensuring genuine gender dysphoria diagnosis.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals seeking to medically transition (hormone replacement therapy, surgical procedures) in order to access legal sex reclassification under the hybrid model. They must navigate lengthy psychiatric evaluations, demonstrate consistent gender identity over defined periods, afford high medical costs, and comply with institutional requirements before legal recognition. They bear the direct costs of medical intervention, psychological surveillance, and gatekeeping delays; they face denial or indefinite deferral if evaluators judge them insufficiently dysphoric or stable.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_transition_seekers, payer,
    moderate, biographical, constrained, national).

% Transgender individuals who do not pursue medical transition (by choice, circumstance, health contraindication, or economic inability) have no pathway to legal sex category change under the hybrid model. They are legally and administratively locked into birth-assigned categories despite living as a different gender. They bear the costs of persistent misclassification—in identity documents, healthcare records, employment, and legal proceedings—while being excluded from the remedy (medical transition) that alone permits category reclassification.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, identity_locked, national).

% Cisgender individuals whose category membership is biologically and socially aligned. The hybrid model reinforces their category boundary by requiring medical/institutional validation for entry, which implicitly affirms biology as the default and transition as exceptional. They benefit from presumptive category inclusion (no medical proof required) and from a system that restricts access to category membership to those who prove sufficient dysphoria and commitment through medical channels.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_category_holders, beneficiary,
    organized, generational, arbitrage, national).

% Trans women who have completed or are undergoing medical transition gain legal sex reclassification and access to women's spaces, services, and identity documents. They also carry the ongoing burden of proving their membership through medical compliance (hormone levels, surgical status); many experience social stigma and institutional scrutiny that cisgender women do not face. They are included in law but conditionally—membership depends on maintaining medical protocol and institutional approval.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_post_transition, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, trans_women_post_transition, payer).

% Advocates holding a radical-feminist view that sex (reproductive category) is biologically immutable and socially foundational argue that the hybrid model's inclusion of medically transitioned individuals erodes the material reality of sex-based oppression and undermines sex-segregated spaces built to protect women from male socialization and violence. Their position is excluded from the medical/institutional consensus framing the hybrid model as legitimate, though their objections shape legal contestation and policy resistance.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, feminist_sex_essentialist_advocates, excluded,
    organized, generational, trapped, national).

% Advocates holding a gender-identity-centered reading that gender identity alone determines category membership argue the hybrid model is discriminatory gatekeeping that pathologizes trans identity by requiring medical proof of legitimacy. They are largely excluded from medical/institutional consensus setting but shape legal contests and pressure policy toward depathologization and identity-based recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_identity_advocacy_coalitions, excluded,
    organized, generational, trapped, national).

% Bioethicists, medical anthropologists, and human rights analysts examine whether gatekeeping on medical transition serves the safety of transition-seekers or functions primarily as extraction of authority/legitimacy by medical institutions. They document the psychological costs of gatekeeping delays, the financial barriers to medical access, and the differential impact on low-income individuals and communities of color.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_ethics_observers, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provide administrative verifiability for legal sex category change: establish medical transition (irreversible embodied commitment) as the evidence of genuine identity change, sufficient to justify updating identity documents without requiring only subjective self-attestation (which raises administrative verification concerns) or using only immutable biology (which excludes all trans individuals).
% TRANSFER_FUNCTION: Transfers institutional authority from individuals (self-identification) to medical gatekeeping bodies (diagnosis, treatment approval, documentation). Transfers costs from the state/service providers (recognizing category change based on self-report) to individuals (affording medical intervention, enduring psychiatric surveillance, accepting gatekeeping delays). Transfers social category inclusion (from presumptive for cis, to conditional for trans post-transition, to permanently excluded for non-transitioning trans).
% ABSENT_VOICES: Non-transitioning trans individuals cannot participate in the institutions (medical appointment, clinical evaluation, treatment planning, hormone monitoring) through which the hybrid model operates; their structural absence from the institutional machinery means they have no voice in the gatekeeping process. Gender-identity-centered advocates and feminist sex-essentialist advocates are excluded from medical/institutional consensus about the hybrid model's legitimacy; their positions appear only in legal contestation and legislative pressure, not in the institutional design.
% DISAPPEARANCE_RATIONALE: If the hybrid model and its gatekeeping apparatus vanished, legal jurisdictions would need to adopt alternative criteria for sex category recognition: either biology-only (reverting to the biology reading, excluding all trans individuals), identity-only (affirming the identity reading, admitting all self-identified individuals), or some other mixed standard. Different jurisdictions would make different choices. Medical institutions would lose gatekeeping authority and associated revenue; non-transitioning trans individuals would either gain recognition (under identity-only) or remain excluded (under biology-only). The administrative systems for documenting identity, healthcare records, statistical classification, and legal rights/duties would all require recoding. Individual life trajectories organized around medical transition for the purpose of legal recognition would reorganize toward alternative pathways or toward political contestation of category membership itself.
% FOUNDING_PROBLEM: Early legal and medical systems offered no pathway for individuals whose persistent gender identity diverged significantly from birth assignment to obtain legal recognition, leaving them misclassified in identity documents, vulnerable to disclosure, and unable to update records to reflect lived identity. Early systems relying only on identity self-identification lacked administrative verification of commitment, raising institutional concerns about false claims and fraudulent reclassification.
% FOUNDING_PROBLEM_CORROBORATION: Medical advocacy, legislative history, and some legal scholars attest the founding problem was live (individuals lacked recognition pathways and faced administrative/social harms from misclassification). Trans activists and gender-identity advocates attest the founding problem does not require the hybrid solution: evidence from identity-only jurisdictions demonstrates self-identification works administratively without creating verifiable fraud epidemics. Feminist sex-essentialist advocates dispute whether the founding problem even exists as framed (asserting the real problem is the legalization of sex category change itself). No consensus among external corroborators; the founding problem's status and the adequacy of the hybrid response are substantially contested.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim of tangled_rope is defended by the presence of genuine coordination (administrative verification of category membership) alongside asymmetric extraction (medical institutions gain authority and legitimate revenue; non-transitioning trans individuals are permanently excluded; medical transition seekers bear surveillance and cost burdens). Extractiveness is moderately high (0.68 at interval end) because the constraint serves medical institutional interests (gatekeeping authority, clinical revenue, pathologization of trans identity) and cisgender presumptive inclusion, not solely the coordination function. Suppression is high (0.72) because the constraint actively excludes non-transitioning individuals and suppresses the identity-based reading (by requiring medical proof as the condition of legitimacy). Theater ratio rises from 0.25 to 0.41 as medical evaluation becomes increasingly performative—psychiatric assessment of 'genuine' dysphoria is difficult to standardize, appointment timelines lengthen, and the functional coordination (documenting changed gender) becomes a smaller share of the overall activity. Accessibility collapse is moderate (0.62 at individual level, 0.74 at structural level) because medical gatekeeping creates real barriers, but some individuals do access transition and reclassification, and advocacy pressure occasionally lowers barriers or creates alternative pathways. Resistance is moderate (0.58 at organizational level) because trans advocacy organizations, some medical professionals, and some legal jurisdictions actively contest the hybrid model and push toward identity-based recognition. The asymmetry between individual-level suppression (0.72) and organizational-level resistance (0.62) reflects that individual trans people are constrained by identity lock and medical dependence, while organized coalitions can mount legal and political resistance.
 *
 * PERSPECTIVAL GAP:
 *   The medical institution seat and the excluded non-transitioning trans individual seat compute radically differently. From the medical institutional seat: the constraint solves a genuine coordination problem (how to administratively verify category change) and protects vulnerable individuals from irreversible decisions through gatekeeping oversight — a rope-like genuine coordination function with legitimate oversight costs. From the non-transitioning trans individual seat: the constraint is pure extraction dressed as protection — a snare that excludes them from any pathway to recognition while medical institutions profit from gatekeeping and cisgender categories are reinforced. From the medical transition seeker seat (constrained but not excluded): the constraint is a conditional rope — it offers access to reclassification (genuine coordination benefit) but at high cost and subject to gatekeeping delays (extraction component). The engine computes per-seat classification from structural data: the medical institutional agenda_setter with arbitrage-grade exit and concentration of authority computes toward beneficiary directionality; the non-transitioning trans individual with identity_locked exit computes toward full target. The claimed_type (tangled_rope) reflects the reading's own framing; the metric profile reflects the structure the reading instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions benefit from gatekeeping authority, institutional authority over a vulnerable population, and the pathologization of trans identity (d toward beneficiary end, ~0.15-0.25). Cisgender category holders benefit from presumptive inclusion and reinforced category boundaries (d symmetric to slightly beneficiary, ~0.35-0.45). Medical transition seekers are targets: they must comply with institutional requirements, bear costs, and face deferral/denial (d toward target end, ~0.70-0.75). Non-transitioning trans individuals are full targets: they are excluded entirely and have no remedy within the constraint's logic (d near 1.0, full target). Trans women post-transition occupy a dual position: they benefit from reclassification but remain conditionally included and subject to medical surveillance (d symmetric, ~0.50-0.60). The directionality_overrides are not needed here; the structural derivation from beneficiary/victim + power + exit aligns with the reading's actual operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of any legal pathway for trans individuals to obtain recognition) was live at the constraint's inception. The founding_problem_status is contested because medical advocacy and trans activism dispute whether the founding problem actually requires the hybrid solution: identity-only jurisdictions demonstrate that self-identification works administratively without the medical gatekeeping. The constraint persists not because the founding problem remains unsolved by available alternatives, but because medical institutions have captured the authority to define legitimate gender transition and because the gatekeeping serves interests beyond the coordination function (pathologization, clinical revenue, protection of sex-category distinction). The theater ratio rising to 0.41 indicates that psychiatric assessment increasingly functions as institutional gatekeeping theater rather than genuine clinical evaluation—the constraint is maintained by institutional inertia and the interests of the benefiting parties (medical institutions and cisgender category holders), not by the necessity of solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_institutional_gatekeeping,
    'Does the medical transition requirement serve the coordination function (verifying committed identity change for administrative purposes) or primarily serve medical institutional gatekeeping and pathologization?',
    'Comparative institutional analysis: jurisdictions that operate identity-only recognition systems and track outcomes (fraud rates, registration accuracy, equity impacts). Ethnographic study of psychiatric gatekeeping practices and clinical documentation standards. Economic analysis of clinical revenue and institutional incentives in medical transition.',
    'If the requirement serves primarily gatekeeping rather than coordination, the constraint should be reclassified from tangled_rope toward snare — the coordination function is the cover story, extraction is the mechanism. If the requirement serves genuine coordination, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_institutional_gatekeeping, empirical, 'Whether medical gatekeeping is functionally necessary for category administration or primarily serves institutional interests.').

omega_variable(
    non_transitioning_trans_exclusion_mechanism,
    'Is the exclusion of non-transitioning trans individuals an inherent structural feature of the hybrid model, or a policy choice that could admit identity-based recognition as an alternative pathway?',
    'Comparative legal analysis of hybrid systems that permit dual pathways (medical transition OR extended identity-based documentation) versus those that recognize only medical transition. Legal reform pilots admitting non-medical-transition individuals.',
    'If exclusion is inherent, the model functions as snare for non-transitioning individuals and cannot be reformed to include them. If the exclusion is a policy choice, it is a deliberate victim-set selection by the benefiting parties (medical institutions and cisgender category holders) and signals intentional extraction from a vulnerable population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_transitioning_trans_exclusion_mechanism, conceptual, 'Whether non-transitioning trans individuals are necessarily excluded or deliberately excluded.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Does the hybrid reading''s core claim (combination of biology and transition is necessary for legitimacy) logically foreclose the identity reading, or do these readings represent different parties'' lived commitments that can coexist legally and administratively?',
    'Philosophical analysis of logical entailment between the readings'' foundational axioms. Institutional history of how different readings emerged and interact (do they compete to replace one another, or do they persist as different institutional regimes?). Comparative jurisprudence examining whether the readings are genuinely incommensurate or represent emphasis differences within overlapping frameworks.',
    'If the readings foreclose each other logically, one must be false and the engine would classify the foreclosure relation. If they coexist as live positions, the relation is coexists_with and the kernel remains contested. This determines the cs_structure.reading_relations entry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the hybrid reading''s core premise logically rules out the identity reading or whether both can remain live simultaneously.').

omega_variable(
    suppression_internalization_in_non_transitioning_trans_individuals,
    'Is the measured suppression (0.72) of non-transitioning trans individuals structural (legal barriers, institutional exclusion, economic access barriers to medical transition) or internalized (acceptance of the hybrid model''s framing that their identity is less legitimate if not medically validated)?',
    'Post-legal-change measurement: if non-transitioning trans individuals gain identity-based legal recognition in comparative jurisdiction, does suppression/shame/identity doubt persist, or does it decline? Ethnographic interviews documenting internalization of medical-validation norms. Psychological assessment of identity stability among non-transitioning trans individuals in different legal regimes.',
    'If suppression is primarily structural, removing legal barriers would reduce the constraint''s effective extraction. If suppression is substantially internalized, the constraint''s psychological impact persists even after legal change — the gatekeeping has colonized the individual''s self-concept. This informs whether the constraint is reformable or requires deeper cultural/epistemic intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_non_transitioning_trans_individuals, empirical, 'Whether non-transitioning trans individuals'' suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(sex__tr_t5, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__hybrid_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(sex__tr_t15, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__hybrid_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(sex__tr_t25, observed).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sex__tr_t30, observed).
narrative_ontology:measurement(sex__tr_t35, sex_gender_category__hybrid_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(sex__tr_t35, observed).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__hybrid_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sex__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(sex__be_t5, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__hybrid_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(sex__be_t15, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__hybrid_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(sex__be_t25, observed).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(sex__be_t30, observed).
narrative_ontology:measurement(sex__be_t35, sex_gender_category__hybrid_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(sex__be_t35, observed).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__hybrid_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sex__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(sex__su_t5, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__hybrid_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(sex__su_t15, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__hybrid_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(sex__su_t25, observed).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__hybrid_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(sex__su_t30, observed).
narrative_ontology:measurement(sex__su_t35, sex_gender_category__hybrid_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(sex__su_t35, observed).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__hybrid_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(sex__su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(sex__grid_01, sex_gender_category__hybrid_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(sex__grid_02, sex_gender_category__hybrid_reading, accessibility_collapse(class), 40, 0.7).
narrative_ontology:measurement(sex__grid_03, sex_gender_category__hybrid_reading, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(sex__grid_04, sex_gender_category__hybrid_reading, accessibility_collapse(individual), 40, 0.62).
narrative_ontology:measurement(sex__grid_05, sex_gender_category__hybrid_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(sex__grid_06, sex_gender_category__hybrid_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(sex__grid_07, sex_gender_category__hybrid_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(sex__grid_08, sex_gender_category__hybrid_reading, accessibility_collapse(structural), 40, 0.74).
narrative_ontology:measurement(sex__grid_09, sex_gender_category__hybrid_reading, resistance(class), 0, 0.52).
narrative_ontology:measurement(sex__grid_10, sex_gender_category__hybrid_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(sex__grid_11, sex_gender_category__hybrid_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement(sex__grid_12, sex_gender_category__hybrid_reading, resistance(individual), 40, 0.38).
narrative_ontology:measurement(sex__grid_13, sex_gender_category__hybrid_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(sex__grid_14, sex_gender_category__hybrid_reading, resistance(organizational), 40, 0.62).
narrative_ontology:measurement(sex__grid_15, sex_gender_category__hybrid_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(sex__grid_16, sex_gender_category__hybrid_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(sex__grid_17, sex_gender_category__hybrid_reading, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(sex__grid_18, sex_gender_category__hybrid_reading, stakes_inflation(class), 40, 0.66).
narrative_ontology:measurement(sex__grid_19, sex_gender_category__hybrid_reading, stakes_inflation(individual), 0, 0.72).
narrative_ontology:measurement(sex__grid_20, sex_gender_category__hybrid_reading, stakes_inflation(individual), 40, 0.74).
narrative_ontology:measurement(sex__grid_21, sex_gender_category__hybrid_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(sex__grid_22, sex_gender_category__hybrid_reading, stakes_inflation(organizational), 40, 0.62).
narrative_ontology:measurement(sex__grid_23, sex_gender_category__hybrid_reading, stakes_inflation(structural), 0, 0.48).
narrative_ontology:measurement(sex__grid_24, sex_gender_category__hybrid_reading, stakes_inflation(structural), 40, 0.51).
narrative_ontology:measurement(sex__grid_25, sex_gender_category__hybrid_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(sex__grid_26, sex_gender_category__hybrid_reading, suppression(class), 40, 0.62).
narrative_ontology:measurement(sex__grid_27, sex_gender_category__hybrid_reading, suppression(individual), 0, 0.68).
narrative_ontology:measurement(sex__grid_28, sex_gender_category__hybrid_reading, suppression(individual), 40, 0.72).
narrative_ontology:measurement(sex__grid_29, sex_gender_category__hybrid_reading, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(sex__grid_30, sex_gender_category__hybrid_reading, suppression(organizational), 40, 0.55).
narrative_ontology:measurement(sex__grid_31, sex_gender_category__hybrid_reading, suppression(structural), 0, 0.54).
narrative_ontology:measurement(sex__grid_32, sex_gender_category__hybrid_reading, suppression(structural), 40, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three constraint stories (one per reading): biology_reading (chromosomal/reproductive sex as immutable category boundary, low extraction), hybrid_reading (combination of biology and medical transition, moderately extractive, this story), identity_reading (gender identity as sole criterion, contested regarding fraud/verification). All three share the same referent (the kernel: what determines category membership in law and administration) but differ in ε values, beneficiary structures, victim sets, and authority structures. They are linked bidirectionally via network.affects_constraints because changes to legal recognition regimes shift institutional power and resource allocation across all three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
