% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Legitimate Health Intervention (Public Health Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The 'legitimate health intervention' kernel encodes a fundamental contest
 *   in bioethics and constitutional law: what grounds the state's authority
 *   to mandate medical interventions? This JSON instantiates ONE reading of
 *   that kernel — the public health primary reading — which derives
 *   legitimacy from measurable reduction in population-level
 *   morbidity/mortality and treats individual refusal as externality
 *   imposition (disease transmission risk). Under this reading, unvaccinated
 *   individuals enter the victim set as disease vectors whose refusal imposes
 *   costs on others (immunocompromised populations, disease control systems).
 *   Enforcement mechanisms (employment termination, healthcare access
 *   restrictions) are classified as legitimate proportional responses to the
 *   externality. The constraint exhibits tangled rope structure: genuine
 *   coordination function (achieving herd immunity) coexists with asymmetric
 *   extraction (enforcement costs concentrated on refusers). The measuring
 *   interval (t=0 to t=12) tracks the COVID-19 vaccine rollout and mandates,
 *   showing extractiveness escalation from initial voluntary uptake phase
 *   (0.32) to enforcement phase (0.58) and persistence at enforcement levels.
 *   The false summit detector flags the analytical observer's mountain
 *   perspective as naturalization: epidemiological facts (disease
 *   transmission) are distinct from policy legitimacy claims (enforcement is
 *   justified response), yet the mountain frame conflates them.
 *
 * KEY AGENTS:
 *   - Unvaccinated Refusers: Primary victims (powerless/trapped) — face employment loss, institutional access exclusion, travel restrictions. Bear full enforcement cost with no exit options.
 *   - Immunocompromised Populations: Primary beneficiaries (moderate/constrained) — depend on high population vaccination for herd immunity protection. Benefit from enforcement but also constrained by vaccine verification requirements.
 *   - Disease Control Authorities: Secondary beneficiary (institutional/arbitrage) — coordinate vaccination behavior to reach herd immunity thresholds. Experience constraint as coordination function, not extraction.
 *   - Pharmaceutical Corporations: Tertiary beneficiary (powerful/mobile) — capture government contracts, indemnification, and market expansion. Have global arbitrage options and greatest exit mobility.
 *   - Public Health Emergency Response Infrastructure: Temporary coalition (organized/constrained) — Emergency Use Authorization, mandate enforcement, distribution systems. See enforcement as time-limited crisis response with sunset conditions.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent enforcement decisions as epidemiological necessity. False summit candidate: naturalizes policy as law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.58).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Legitimate Health Intervention (Public Health Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, 'e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0').
narrative_ontology:cs_kernel_codification('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', formalized).
narrative_ontology:cs_authority_grounding('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', extraction).
narrative_ontology:cs_interpretation_layer_present('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0').
narrative_ontology:cs_reading_relation('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', foundational, population_mortality_reduction_sufficient_legitimacy).
narrative_ontology:cs_axiom_status(population_mortality_reduction_sufficient_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', population_mortality_reduction_sufficient_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', foundational, individual_refusal_as_externality_imposition).
narrative_ontology:cs_axiom_status(individual_refusal_as_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', individual_refusal_as_externality_imposition, empirically_contingent).
narrative_ontology:cs_reference_frame('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', population_disease_control_framework).
narrative_ontology:cs_drift_state('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', contemporary_post_peak_immunity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1c08f76-9f8e-40bc-aa6a-996ebfd35ca0', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, disease_control_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, bodily_autonomy_claimants).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, employment_terminated_unvaccinated).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED REFUSER (SNARE) — Faces employment termination, healthcare access restrictions, and institutional exclusion. Trapped by economic dependency and lack of employment alternatives. No genuine exit option; all alternatives are blocked. Maximum experienced extraction because suppression is structural and enforced, with minimal coordination benefit to this agent.
constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISEASE CONTROL AUTHORITY (ROPE) — Experiences the constraint as coordination: achieving population immunity thresholds requires coordinated vaccination behavior. The authority benefits from participation and sees the mechanism as solving a genuine collective action problem (free-rider problem in disease prevention). Lower suppression, higher coordination function from this perspective.
constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMUNOCOMPROMISED INDIVIDUAL (TANGLED ROPE) — Benefits from high population vaccination (depends on herd immunity for protection), but also constrained by enforcement mechanisms. Must navigate vaccine status verification requirements, medical surveillance, and potential discrimination. Mixed experience: genuine benefit from coordination but also constrained access and social stigma.
constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL CORPORATION (TANGLED ROPE) — Captures significant extraction through government contracts, indemnification clauses, and market expansion. Also coordinating genuine vaccine production infrastructure and distribution logistics. High economic benefit; exit options through jurisdictional arbitrage (operating across nations with different regulations). Represents institutional power balancing coordination and extraction benefits.
constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH EMERGENCY RESPONSE (SCAFFOLD) — Emergency Use Authorization mechanisms, vaccine mandates, and enforcement structures are explicitly framed as temporary crisis measures. Theater low because the intervention is direct action (injection/vaccination) not administrative ritual. Sunset condition: as population immunity or disease control metrics reach specified thresholds, enforcement mechanisms scheduled for phase-out. This perspective sees extractiveness as legitimately temporary.
constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EPIDEMIOLOGICAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, disease transmission is a physical/biological fact: an unvaccinated vector in a population of susceptible individuals will transmit pathogen with calculable probability. This perspective sees the constraint as emerging from natural law (epidemiology), not from human policy choice. However, the structural data contradicts pure mountain classification — enforcement mechanisms (employment termination, access exclusion) are policy choices, not natural laws. The engine identifies this as a false summit.
constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimate_health_intervention__public_health_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The public health primary reading measures extractiveness through enforcement mechanisms' intensity and impact on refusers. The t=0 baseline (0.32) reflects initial voluntary vaccination phase without coercion. At t=6 (major mandate implementation), extractiveness jumps to 0.58 reflecting employment termination, access restrictions, and institutional pressure. The plateau at t=12 indicates sustained enforcement level — the constraint has not degraded or escalated further, but remains at significant extraction. This is not maximum extraction (snare would show 0.66+) because: (a) some coordination function is genuine (herd immunity has real public health benefit), (b) exit options exist for some actors (healthcare workers could relocate, though costly), and (c) enforcement is not total (many jurisdictions did not mandate). Suppression (0.68): High. Structural barriers to refusal include employment dependency (cannot leave without financial hardship), institutional access (hospitals, schools, government services), and social stigma (internalized suppression). The measurement trajectory shows suppression building from initial low barrier (0.42 — few barriers at voluntary phase) to high barrier (0.68 — multiple compounding institutional channels), then remaining stable. Low theater (0.35) reflects that the intervention is direct action (vaccination/injection), not administrative ritual. Theater would be higher (0.7+) if the constraint were performative vaccination card-checking without medical effect, but under the public health primary reading, the intervention has direct biological function. Claimed type (tangled_rope): Justified by presence of both genuine coordination (herd immunity achievement) and asymmetric extraction (enforcement concentrated on refusers, benefits concentrated on immunocompromised and authorities).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the same enforcement mechanism is experienced as legitimate coordination by disease control authorities (rope perspective) and as pure extraction by unvaccinated refusers (snare perspective). The gap reveals the structural asymmetry: authorities coordinate the system; refusers are coordinated. The tangled rope classification sits between these poles, capturing that legitimate coordination coexists with genuine extraction. The immunocompromised perspective (tangled rope) shows how beneficiaries are also constrained — they benefit from herd immunity but are subject to vaccine verification requirements and medical surveillance. The pharmaceutical corporation perspective (tangled rope at powerful level) shows institutional asymmetry: corporations have arbitrage and exit options that individuals lack. The analytical observer's mountain perspective is a false summit: it naturalizes epidemiological facts (disease transmission exists) as justification for policy choices (employment mandates are necessary), collapsing the distinction between natural law and institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position: beneficiary status, exit options, and power level. Unvaccinated refusers: high d (0.89) from victim status + trapped exit (no alternatives without massive cost). Disease control authorities: low d (0.05) from beneficiary status + arbitrage exit (can adjust policy, coordinate alternatives). Immunocompromised: moderate-high d (0.72) from mixed beneficiary/victim status + constrained exit (benefit from vaccination but constrained by verification/surveillance requirements). Pharmaceutical corporations: very low d (-0.05) from beneficiary status + mobile exit (can operate across jurisdictions, exit if policy changes). The engine applies f(d) sigmoid function to compute experienced extractiveness chi per the formula χ = ε × f(d) × σ(S). Refusers experience high chi (trapped + victim + high d → amplified extraction). Authorities experience low chi (beneficiary + arbitrage → suppressed/negative extraction). The presheaf of perspectives shows how the same base extractiveness (0.58) maps to different experienced extractiveness across positions.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING ANALYSIS: This constraint resolves the mandatrophy for THIS reading by declaring unambiguously that legitimacy is POPULATION-LEVEL OUTCOME (measurable morbidity/mortality reduction). The sibling bodily_autonomy_primary reading would resolve the mandatrophy differently: legitimacy derives from INDIVIDUAL INFORMED CONSENT regardless of outcomes. The proportionality_reading would weight both axes but with proportionality gates. The three readings cannot simultaneously inhabit a single institutional framework at maximum force — they coexist as competing factions' commitments. This reading's mandate is: if population mortality will decrease by X%, enforcement is legitimate even if individual autonomy is constrained. It naturalizes the externality frame (refusal = harm to others) and subordinates autonomy to outcome measurement. The mandatrophy is RESOLVED via kernel decomposition: each reading gets its own constraint story with different ε, different victim/beneficiary sets, and different authority grounding. The engine's false summit detector will flag THIS reading's mountain perspective as naturalization; the bodily_autonomy_primary reading's mountain perspective (autonomy as inalienable right) will trigger FSM differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_severity_threshold_ambiguity,
    'At what disease severity threshold does collective benefit justify individual constraint? Is this threshold empirically determined or normatively chosen?',
    'Comparative analysis of enforcement decisions across disease types (COVID-19 vs seasonal flu vs measles) and time periods. Identify whether the threshold correlates with epidemiological data or with political/institutional factors.',
    'If empirically determined: the public health primary reading''s legitimacy is grounded in measurable outcomes. If normatively chosen: the reading naturalizes a political decision as epidemiological necessity. Classification shifts toward snare if threshold is revealed as arbitrary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disease_severity_threshold_ambiguity, empirical, 'Whether disease severity threshold for enforcement is empirically or normatively determined').

omega_variable(
    externality_imposition_calculus,
    'Does vaccine refusal genuinely impose externality (disease transmission risk) or does this claim depend on contestable assumptions about transmission probability, variant severity, and vaccine effectiveness durability?',
    'Systematic review of transmission data across variants, time periods, and vaccination status combinations. Distinguish between: (a) refuser poses measurable transmission risk vs (b) risk is marginal/zero under conditions and assumptions. Identify how risk assessment changed over constraint''s temporal interval.',
    'If externality is substantial and stable: public health primary reading''s core premise holds. If externality is marginal or time-dependent: the reading''s justification erodes, and the constraint appears as extraction (snare) rather than legitimate health coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_imposition_calculus, empirical, 'Whether vaccine refusal imposes genuine externality or depends on contestable epidemiological assumptions').

omega_variable(
    kernel_reading_contest_foreclosure,
    'Can a single institutional framework hold BOTH the public health primary reading (legitimacy from population mortality reduction) AND the bodily autonomy primary reading (legitimacy from informed consent regardless of outcomes) simultaneously?',
    'Constitutional/jurisprudential analysis: identify cases where both principles have been invoked; examine whether they coexist in the same legal framework or are treated as contradictory axioms. Track shifts in constitutional precedent over time.',
    'If both readings coexist: they represent different political factions'' commitments (coexists_with relation). If one forecloses the other: the engine reclassifies the relation as forecloses. The outcome determines whether this constraint''s legitimacy is contested or settled within a single framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_foreclosure, conceptual, 'Whether bodily autonomy and public health primary readings can coexist in a single institutional framework').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.68) structural (employment termination, institutional access barriers) or partly internalized (individuals internalize the constraint''s framing that refusal is selfish/harmful)?',
    'Post-enforcement analysis: track individuals who were subject to enforcement mechanisms (job loss, exclusion) and measure how many remain voluntary participants vs actively contest the constraint. If internalization is significant, constraint persists even after structural barriers are removed.',
    'If suppression is primarily structural: removing employment mandates/access restrictions significantly reduces effective suppression. If suppression is partly internalized: social stigma and shame persist independently of institutional enforcement, making exit costlier than baseline barriers. Classification stability hinges on suppression composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized components of measured suppression').

omega_variable(
    false_summit_naturalization_risk,
    'Does framing disease transmission as ''natural law'' (mountain perspective) obscure that enforcement mechanisms (employment termination, access restriction) are policy choices? Is epidemiology being conflated with policy legitimacy?',
    'Analytical decomposition: separate the biological fact (unvaccinated individuals can transmit disease) from the policy choice (employment mandates are legitimate response to that fact). Identify whether enforcement level correlates with epidemiological severity or with political/institutional factors.',
    'If enforcement is proportional to epidemiological threat: public health primary reading is defensible. If enforcement continues or escalates beyond epidemiological justification: the constraint is revealed as extraction (snare) disguised as natural law. The false summit detector triggers when beneficiaries exist but classification claims naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, conceptual, 'Risk that epidemiological necessity is conflated with policy legitimacy, naturalizing contingent enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_hp_theater_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legit_hp_theater_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.35).
narrative_ontology:measurement(legit_hp_theater_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(legit_hp_extract_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(legit_hp_extract_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(legit_hp_extract_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legit_hp_supp_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(legit_hp_supp_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(legit_hp_supp_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% The legitimate_health_intervention kernel decomposes into three structurally distinct constraints corresponding to three readings of the kernel. Each reading derives legitimacy differently: public_health_primary emphasizes measurable population outcomes; bodily_autonomy_primary emphasizes individual consent; proportionality_reading weights both with severity gates. Each reading generates different ε values, victim/beneficiary sets, and classification outcomes. The three stories are linked via network.affects_constraints because they share a kernel and influence each other's epistemic standing. When public_health_primary classification shows high χ, the authority of bodily_autonomy_primary is undermined in the same institutional framework; conversely, when bodily_autonomy_primary demonstrates that enforcement lacks individual consent, it creates pressure to reclassify public_health_primary as snare. The kernel contest is the structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, institutional, 0.05).
constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
