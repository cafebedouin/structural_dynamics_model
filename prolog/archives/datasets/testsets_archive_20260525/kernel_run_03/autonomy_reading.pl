% ============================================================================
% CONSTRAINT STORY: autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: autonomy_reading
 *   human_readable: Autonomy Reading of End-of-Life Decision Authority
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The autonomy reading of end-of-life decision authority claims that
 *   competent individuals possess sovereign authority to make decisions about
 *   their own death, including access to medical assistance in dying when
 *   suffering becomes unbearable. This reading represents one interpretation
 *   of a contested kernel—the deep question of who has authority to decide
 *   when a life should end—and coexists with alternative readings grounded in
 *   sanctity of life and vulnerability protection. The autonomy reading
 *   emerged as dominant in bioethics discourse during the 1960s-1980s shift
 *   toward patient rights and informed consent, becoming codified in
 *   legislation across jurisdictions (Netherlands 2002, Belgium 2002, Canada
 *   2016, Oregon 1997). However, it remains contested: religious frameworks,
 *   disability rights advocates, and conservative bioethicists argue that
 *   recognizing individual autonomy as sovereign creates structural pressures
 *   to eliminate vulnerable populations and undermines institutional
 *   protections against coercion. The constraint exhibits tangled rope
 *   characteristics: it possesses a genuine coordination function (clarifying
 *   the relationship between medical authority and patient agency) while also
 *   implementing asymmetric extraction (blocking access to death for those
 *   who have rationally chosen it, or conversely—depending on the
 *   reading—coercing death for those who wish to live). The extractiveness
 *   value (0.52) reflects that the constraint imposes genuine costs on those
 *   denied access while providing guidance and clarity to medical
 *   professionals and institutions.
 *
 * KEY AGENTS:
 *   - Competent Individuals Facing Terminal Illness: Primary potential beneficiaries (moderate-powerful/mobile or trapped depending on access) — seek authority over their own death and escape from prolonged suffering
 *   - Suffering Individuals Denied Access: Primary victims (powerless/trapped) — forced to continue living with unbearable suffering against their declared will; bear full extraction cost of the constraint
 *   - Healthcare Professionals: Secondary actors (moderate-institutional/constrained) — experience mixed coordination (clarity on ethical guidance) and extraction (career/legal risk, conflicting duty frameworks)
 *   - Medical Professional Organizations: Institutional beneficiaries (institutional/arbitrage) — adopt autonomy reading to align with human rights frameworks and reduce internal ethical conflict
 *   - Religious and Sanctity-Based Institutions: Institutional opposition (powerful/mobile) — oppose autonomy reading based on doctrinal commitments; extract authority over end-of-life decisions from individuals
 *   - Right-to-Die Advocacy Coalition: Organized agents (organized/constrained) — see the constraint as temporary; build political and legal pathways to institutionalize autonomy
 *   - Disability Rights Communities: Complex position (moderate-organized/constrained) — some embrace autonomy reading, others fear it enables coercive elimination of disabled lives
 *   - Vulnerable Populations (cognitive decline, depression, economic desperation): Potential targets (powerless-moderate/trapped or constrained) — at risk of coercive pressure under autonomy frameworks without robust safeguards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomy_reading, 0.52).
domain_priors:suppression_score(autonomy_reading, 0.65).
domain_priors:theater_ratio(autonomy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(autonomy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(autonomy_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomy_reading, tangled_rope).
narrative_ontology:human_readable(autonomy_reading, "Autonomy Reading of End-of-Life Decision Authority").
narrative_ontology:topic_domain(autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(autonomy_reading, formalized).
narrative_ontology:cs_authority_grounding(autonomy_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(autonomy_reading).
narrative_ontology:cs_kernel_id(autonomy_reading, end_of_life_decision_authority).
narrative_ontology:cs_reading_relation(autonomy_reading, sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation(autonomy_reading, vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom(autonomy_reading, foundational, rational_agency_sovereignty_principle).
narrative_ontology:cs_axiom_status(rational_agency_sovereignty_principle, holdable).
narrative_ontology:cs_axiom_grounding(autonomy_reading, rational_agency_sovereignty_principle, deontological).
narrative_ontology:cs_axiom(autonomy_reading, foundational, individual_capacity_over_institutional_authority).
narrative_ontology:cs_axiom_status(individual_capacity_over_institutional_authority, holdable).
narrative_ontology:cs_axiom_grounding(autonomy_reading, individual_capacity_over_institutional_authority, deontological).
narrative_ontology:cs_reference_frame(autonomy_reading, informed_consent_autonomy_framework).
narrative_ontology:cs_drift_state(autonomy_reading, contemporary_medical_ethics, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomy_reading, competent_individuals_exercising_self_determination).
narrative_ontology:constraint_beneficiary(autonomy_reading, healthcare_professionals_as_facilitators).
narrative_ontology:constraint_victim(autonomy_reading, suffering_prolonged_through_denial_of_access).
narrative_ontology:constraint_victim(autonomy_reading, individual_autonomy_constrained_by_institutional_gatekeeping).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING INDIVIDUAL (SNARE) — Trapped in terminal illness with intolerable suffering; institutional barriers (physician gatekeeping, legal prohibitions, family opposition) prevent access to death on their own terms. No exit options; bears full extraction cost as the constraint forces continuation of suffering against their declared will. Maximum experienced coercion.
constraint_indexing:constraint_classification(autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HEALTHCARE PROFESSIONAL (TANGLED ROPE) — Constrained by professional licensing, institutional policy, and legal exposure; also sees genuine coordination value in the autonomy reading (respecting patient agency is foundational to modern medical ethics). Mixed position: they benefit from clear ethical guidance (autonomy reading provides this), but face career and legal risks if they facilitate access. Moderate extraction and moderate coordination function.
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL ORGANIZATIONS (ROPE) — Institutions (medical associations, palliative care societies) benefit from adopting the autonomy reading: it clarifies ethical guidance, reduces internal conflict between competing duties, and aligns medicine with contemporary human rights frameworks. This perspective experiences the constraint as coordination—the autonomy reading solves the problem of how to reconcile professional obligations. Beneficiaries through arbitrage (legitimacy, alignment with evolving standards).
constraint_indexing:constraint_classification(autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SANCTITY COALITION (TANGLED ROPE) — Powerful institutions (faith-based healthcare systems, conservative bioethics frameworks) oppose the autonomy reading based on doctrinal commitments to life sanctity. They experience genuine coordination function (safeguarding vulnerable populations from coercion) alongside extractive enforcement (blocking access to death for those who have rationally chosen it). Mobile exit via private institutions in some jurisdictions, but high institutional investment in maintaining sanctity-based authority over end-of-life decisions.
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHT-TO-DIE COALITION (SCAFFOLD) — Organized agents (patient advocacy, terminal illness support groups, some physicians) see the autonomy reading as a sunset constraint on institutional gatekeeping. They frame the constraint as temporary: over time, as social consensus shifts toward recognizing competent end-of-life decisions, the institutional barriers (physician gatekeeping, legal prohibitions) are expected to erode. Low experienced extraction because the coalition has agency and sees a clear political/legal exit path (legislative change, institutional norm shift). Theater is low because the advocacy effort is transparent and directly targeted at changing the underlying rules.
constraint_indexing:constraint_classification(autonomy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, one might argue that respecting competent individual autonomy over death is a universal principle grounded in irreducible facts about human dignity, rational agency, and the inviolability of conscious choice. However, this perspective risks naturalizing what is actually a contestable normative reading. The engine will flag this as a false summit, revealing that the 'universal principle' framing masks a choice among competing readings (autonomy vs. sanctity vs. vulnerability protection).
constraint_indexing:constraint_classification(autonomy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The autonomy reading imposes significant costs on those denied access—individuals with terminal illness who seek death but face institutional, legal, or professional barriers. The value reflects that suppression of access is substantial, but the constraint also provides genuine coordination benefit (medical professionals have clarity on their ethical obligations). The measurement trajectory shows slight increase over time (0.48→0.52 across 20 years) reflecting incremental tightening of institutional gatekeeping even as legal frameworks in some jurisdictions shift toward permissive autonomy. Suppression (0.65): High. Substantial barriers exist to exercising autonomous end-of-life decisions: legal prohibitions in most jurisdictions, medical professional gatekeeping, institutional policies, family opposition, financial barriers to travel to permissive jurisdictions, and social stigma. These barriers prevent expression of autonomous choice for large populations. Theater ratio (0.38): Moderate-low. The autonomy reading is substantively grounded in arguments about respect for rational agency and bodily integrity; it is not purely performative. However, some theater exists in the form of philosophical disputation about what 'competence' means, legal formalities around decision-making capacity assessments, and procedural safeguards that may serve theater function (demonstrating deliberation) rather than preventing genuine harm. The relatively low theater reflects that the autonomy reading is directionally honest about its commitments, unlike the sanctity reading which maintains stronger theater through alternative framing (protecting 'vulnerable populations' while actually blocking access for rational agents).
 *
 * PERSPECTIVAL GAP:
 *   The most dramatic perspectival gap appears between the suffering individual denied access (snare) and the beneficiary perspective of medical professionals and institutions (rope). The individual experiences maximal coercion and no coordination benefit; the institution experiences both coordination clarity and extractive benefit from maintaining gatekeeping authority. The right-to-die coalition (scaffold) sees a temporary constraint with clear exit pathways, while religious institutions (tangled rope, powerful position) experience genuine conflict between their coordination function (protecting the vulnerable) and extractive enforcement (maintaining authority over decisions). The analytical observer risks seeing autonomy as a natural law (mountain), but the structural data reveals this as a false summit: autonomy is a normative reading grounded in particular historical and philosophical commitments, not an irreducible principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationship of each agent to THIS reading. Suffering individuals denied access have d ≈ 0.95 (full target—the constraint directly constrains their agency). Healthcare professionals have d ≈ 0.50-0.55 (mixed—they both benefit from clarity and face extraction through legal/career risk). Medical organizations have d ≈ 0.15 (beneficiary through arbitrage—alignment with human rights frameworks). Religious institutions have d ≈ 0.35 (modified beneficiary—they benefit from maintaining authority, but face pressure as social consensus shifts). Right-to-die advocates have d ≈ 0.40 (modest target through institutional barriers, but organized and mobile). The engine derives these values from the beneficiary/victim declarations and exit options, applying the sigmoid f(d) to compute effective extractiveness. The tangled rope classification reflects that chi values for this constraint range from high (snare perspective) to low (institutional beneficiary perspective) depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading resolves potential mandatrophy by clarifying that this constraint is ONE reading of a contested kernel, not a universal moral fact. The constraint cannot simultaneously be a mountain (natural law about human agency) and a tangled rope (negotiated institutional arrangement) from different perspectives—it can only be one if it IS genuinely one or the other. The committer frame clarifies: this is a reading, not a law. Therefore, the tangled rope classification is appropriate. The 'it's a natural law about autonomy' framing is the false summit that the analytical perspective risks. The omega variables document the structural ambiguities that prevent the autonomy reading from crystallizing into mountain status: the competence threshold question, the slippery slope empirical question, and the kernel reading vs. moral fact distinction. These irreducible uncertainties are constitutive of the constraint's nature as a contested normative reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_threshold_determination,
    'What level of cognitive capacity constitutes ''competence'' sufficient for sovereign authority over death?',
    'Comparative legal analysis of competence standards across jurisdictions; clinical psychology assessment of decision-making capacity in terminal illness; identification of systematic differences in how competence is operationalized.',
    'If threshold is high (requires maximal rationality): many individuals with genuine terminal agency are excluded, shifting classification toward snare for larger victim set. If threshold is low (respects minimal autonomous preference): vulnerability concerns rise, potentially strengthening sanctity arguments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_threshold_determination, conceptual, 'Definition and threshold of competence for death decision authority').

omega_variable(
    slippery_slope_empirical_grounding,
    'Does recognizing autonomy in end-of-life decisions empirically lead to non-voluntary euthanasia or coercive deaths, or does this risk remain theoretical?',
    'Longitudinal comparison of jurisdictions with autonomy-based frameworks (Netherlands, Belgium, Canada, Oregon) vs. restrictive frameworks; tracking of actual vs. predicted non-voluntary deaths; analysis of safeguards in permissive jurisdictions.',
    'If slope is steep (permissive autonomy leads to measurable coercion): sanctity reading gains structural support, and the autonomy reading shifts toward snare (extracting from vulnerable populations). If slope is minimal (empirical safeguards work): autonomy reading remains tangled_rope, and slippery-slope framing is revealed as extractive scaremongering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slippery_slope_empirical_grounding, empirical, 'Empirical validation of slippery slope risk in permissive autonomy frameworks').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this constraint one reading of a contested kernel (the authority structure over end-of-life decisions), or does it represent discovery of a moral fact (that competent autonomy IS sovereign)?',
    'Examination of the historical genesis of the autonomy reading: if it emerged as a contestable normative commitment (1960s bioethics shift toward patient rights), it is a reading; if it claims to have discovered a pre-existing moral truth, the framing itself is the omega. Compare with sanctity reading''s claim to ancient warrant vs. autonomy reading''s claim to modern rationality.',
    'If autonomy is a reading (committer frame): the constraint is one among coexisting alternatives, and the presheaf over all readings is the accurate model. If autonomy claims to be the discovered moral fact: the constraint becomes a false summit, naturalizing a particular reading as universal law. The question is structural and determines whether false-summit detection is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether autonomy reading is a contestable normative reading or discovery of moral fact').

omega_variable(
    institutional_gatekeeping_necessity,
    'Is physician gatekeeping of death decisions a necessary safeguard (preventing premature or coerced decisions) or an extractive mechanism (controlling who gets to decide)?',
    'Comparative study of decision regimes: autonomous request with professional facilitation (autonomy reading) vs. professional discretion with patient input (gatekeeping) vs. presumed consent with override option. Measure actual patient regret, coercion detection, and quality of life for terminal patients in each regime.',
    'If gatekeeping is necessary: suppression value should be higher, and the constraint shifts toward mountain (protecting vulnerable populations from premature death). If gatekeeping is extractive: suppression is unjustified, and the constraint becomes snare (coercing life continuation against individual will).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_necessity, empirical, 'Necessity and legitimacy of institutional gatekeeping over end-of-life decisions').

omega_variable(
    religious_authority_grounding_tension,
    'Can religious authority over end-of-life decisions coexist with secular liberal frameworks that recognize individual autonomy, or do they constitute incompatible readings that foreclose one another?',
    'Examination of pluralist democracies where both readings operate: do they maintain institutional separation (different healthcare systems), epistemic boundaries (religiously informed vs. secular frameworks), or do they clash directly over public policy? Identify where coexistence is stable vs. where foreclosure pressure appears.',
    'If they coexist: the constraint family includes both readings as live alternatives, and policy should accommodate both (pluralist framework). If they foreclose: one reading must prevail in any unified institutional framework, making the constraint a zero-sum political struggle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_authority_grounding_tension, conceptual, 'Compatibility of religious authority and secular autonomy readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t0, autonomy_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(auto_tr_t10, autonomy_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(auto_tr_t20, autonomy_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(auto_be_t0, autonomy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(auto_be_t10, autonomy_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(auto_be_t20, autonomy_reading, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(autonomy_reading, vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The autonomy reading is structurally distinct from its sibling readings in the end-of-life authority kernel. Each reading has its own ε value, beneficiary/victim structure, and perspectival classification. The sanctity reading (ε≈0.45, Tangled Rope) frames institutional protection against coercion as its coordination function; the vulnerability reading (ε≈0.55, Snare/Tangled Rope) makes vulnerable populations central to the victim set. All three readings share the same kernel but instantiate different constraints. They are linked through network.affects_constraints to enable constraint-family analysis and drift-state tracking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(autonomy_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
