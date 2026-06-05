% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccination Mandate Legitimacy (Public Health Primary Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   State authority to compel vaccination is contested along the
 *   mandate_legitimacy_scope kernel, where three sibling readings produce
 *   radically different constraint classifications. This constraint
 *   instantiates the PUBLIC_HEALTH_PRIMARY reading: the state's duty to
 *   protect vulnerable populations from serious harm legitimates compulsory
 *   vaccination even when individual bodily autonomy preferences diverge.
 *   Under this reading, unvaccinated individuals generate externalities
 *   (transmission risk to immunocompromised, infants, elderly, those with
 *   medical contraindications) that create a moral duty to protect. The
 *   state's coercive authority is justified by the severity of harm to
 *   vulnerable populations when vaccination is absent. The constraint
 *   exhibits tangled_rope structure: it coordinates protection of vulnerable
 *   populations (genuine coordination function) while extracting compliance
 *   from vaccine-hesitant individuals through legal authority and enforcement
 *   (asymmetric extraction). The extractiveness value (0.58) reflects that
 *   the mandate burden falls disproportionately on those with medical
 *   autonomy concerns, while the protection benefit accrues to vulnerable
 *   populations who cannot defend themselves. Suppression (0.65) reflects
 *   substantial coercive mechanisms: employment mandates, movement
 *   restrictions, travel bans, medical licenses at risk, social exclusion.
 *   Theater ratio (0.35) is relatively low because the reading's legitimacy
 *   claim is functional (protecting identifiable vulnerable groups) rather
 *   than performative — the mandate is justified by substantive harm
 *   prevention, not by institutional ritual.
 *
 * KEY AGENTS:
 *   - Unvaccinated individuals: Primary targets of mandate coercion (powerless/trapped) — bear full suppression through legal penalties, employment restrictions, social exclusion
 *   - Immunocompromised populations: Hidden beneficiaries when mandate present; hidden victims when mandate absent (powerless/trapped) — depend entirely on others' vaccination compliance for protection
 *   - Infants and elderly: Vulnerable populations (powerless/trapped) — cannot vaccinate or have reduced vaccine efficacy; depend on herd immunity and surrounding vaccination
 *   - Public health authorities: Institutional beneficiaries (institutional/arbitrage) — gain legitimate authority to compel intervention based on protection rationale; can defend mandate through welfare argument
 *   - Medical autonomy norm: Contested structural principle (powerful/arbitrage) — serves coordination function (preserves consent and trust) but is subordinated by public health reading to collective protection duty
 *   - Healthcare system capacity: Organizational beneficiary (institutional/arbitrage) — reduced strain on hospitals and ICUs from preventable disease reduces operational extraction costs
 *   - Vaccine-hesitant communities: Moderate victims (moderate/constrained) — face career and social penalties; some have genuine medical concerns; some face informational barriers to understanding risk-benefit tradeoffs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.58).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.65).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccination Mandate Legitimacy (Public Health Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '7c83aa13-89c9-40a1-a0fe-d48adef8fff9').
narrative_ontology:cs_kernel_codification('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', distributed).
narrative_ontology:cs_authority_grounding('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', extraction).
narrative_ontology:cs_interpretation_layer_present('7c83aa13-89c9-40a1-a0fe-d48adef8fff9').
narrative_ontology:cs_reading_relation('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', foundational, state_duty_to_protect_vulnerable_from_serious_harm).
narrative_ontology:cs_axiom_status(state_duty_to_protect_vulnerable_from_serious_harm, holdable).
narrative_ontology:cs_axiom_grounding('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', state_duty_to_protect_vulnerable_from_serious_harm, deontological).
narrative_ontology:cs_axiom('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', foundational, unvaccinated_persons_bear_duty_to_protect).
narrative_ontology:cs_axiom_status(unvaccinated_persons_bear_duty_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', unvaccinated_persons_bear_duty_to_protect, deontological).
narrative_ontology:cs_reference_frame('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', liberal_state_protective_authority).
narrative_ontology:cs_drift_state('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', contemporary_autonomy_emphasis_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7c83aa13-89c9-40a1-a0fe-d48adef8fff9', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_and_elderly).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, healthcare_system_capacity).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_persons).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, medical_autonomy_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED INDIVIDUAL (SNARE) — Faces legal penalties, employment termination, or movement restrictions with minimal agency. Exit options are severely constrained: comply with injection or lose livelihood. No meaningful alternatives presented. Experiences maximum coercion; the suppression mechanism is structural (legal authority) and asymmetric (enforcement targets this agent, not the state).
constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH AUTHORITY (ROPE) — Perceives the mandate as a coordination mechanism solving a collective action problem: unvaccinated individuals create externalities (transmission to immunocompromised) that markets alone cannot address. The authority experiences the constraint as legitimate governance exercising protective duty. Arbitrage exit: the authority can defend the mandate through appeal to vulnerable populations' welfare. Net beneficiary of the institutional legitimacy the reading provides.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: IMMUNOCOMPROMISED POPULATION (SNARE, WITHOUT MANDATE) — This perspective imagines the counterfactual absence of the mandate: immunocompromised individuals face complete dependence on voluntary compliance from surrounding populations. No exit option exists; they cannot compel protection from unvaccinated contacts. Maximum extraction: their health and life depend on others' choices with no enforcement. This perspective reveals the hidden victim set — those who bear harm if the mandate is absent.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL AUTONOMY NORM (TANGLED ROPE) — The norm of informed consent and bodily integrity is genuinely valuable (coordination function: preserves individual agency, prevents medical coercion, grounds trust in medical institutions). But the public health primary reading subordinates this norm to collective protection duty, extracting legitimacy from the autonomy principle to justify overriding it. The norm functions as both coordination (mutual agreement on consent) and victim (overridden in emergencies). Powerful agents have arbitrage options: they can choose where to live, can advocate for exemptions, can frame the issue politically.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH EMERGENCY FRAMEWORK (SCAFFOLD) — This reading interprets the mandate as a temporary grant of coercive authority during genuine emergency (novel pathogen, high mortality, vaccine availability). The scaffold has built-in sunset logic: once herd immunity is achieved or the threat subsides, the mandate authority should sunset. Theater is low (the legitimacy claim is functional, not performative). The organized agents (public health boards, medical societies) experience this as constrained but defensible — high stakes justify strong measures temporarily.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — This reading is one instantiation of a contested kernel. The public health primary reading coordinates protection of vulnerable populations (genuine coordination function) while extracting legitimacy from competing autonomy norms (asymmetric extraction). From the civilizational analytical view, the constraint's type depends on which reading's premises are accepted: if public health protection is the primary duty, tangled rope. If bodily autonomy is primary, snare. If proportionality gates the entire analysis, rope or scaffold. The observer sees the reading-dependence of the classification itself.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandate_legitimacy_scope__public_health_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The public health primary reading imposes significant burdens on vaccine-hesitant individuals through employment mandates, movement restrictions, and social penalties. However, the extractiveness is not maximal (0.70+) because: (1) the burden is justified by protection of identifiable vulnerable populations (coordination function exists); (2) medical exemptions are typically available for genuine contraindications (suppression is not absolute); (3) the constraint is time-bounded by disease trajectory (assumes eventual herd immunity or pathogen evolution reduces transmission). The rising trajectory (0.35→0.52→0.58) reflects enforcement intensification as public health authorities face continued vaccine hesitancy and disease circulation. Suppression (0.65): Substantial. The coercive mechanisms are structural: legal authority to impose employment conditions, deny public service access, restrict movement. Suppression is asymmetric (targets vaccine-hesitant, not the state). Notably, the suppression does NOT apply to immunocompromised individuals who face the counterfactual harm of mandate absence (they have zero exit options in both scenarios, but mandate presence reduces their extraction). Theater ratio (0.35): Low-moderate. The legitimacy claim is functional (protecting vulnerable populations from serious harm) rather than performative. The mandate is justified by substantive outcomes (reduction in transmission to at-risk groups) rather than by ritual compliance or institutional theater. The slight rise (0.28→0.35) reflects some increase in rhetorical framing and symbolic authority deployment as the initial emergency transitions to endemic management, but the core justification remains outcome-focused.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_primary reading produces maximum perspectival variance. Unvaccinated individuals see a snare (maximum coercion, trapped exit, no meaningful alternatives). Public health authorities see a rope (coordination mechanism solving externality problem, legitimate authority exercised). Immunocompromised populations (in the counterfactual absence of the mandate) see a different snare — dependence on voluntary compliance with zero enforcement. The medical autonomy norm sees itself as tangled rope — valuable coordination principle being subordinated to collective duty. The analytical observer sees the reading-dependence: whether this constraint is rope, snare, tangled rope, or scaffold depends entirely on whether one accepts the public health primary reading's premise (state duty to protect vulnerable populations outweighs individual autonomy choice). Different readings of the kernel produce different classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) reflects the agent's structural position relative to the mandate constraint. Unvaccinated individuals face full targeting: beneficiary status is absent, exit options are trapped, power is powerless → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extractiveness. Public health authorities face full beneficiary status: institutional power, arbitrage exit, beneficiary declaration → d ≈ 0.00-0.15 → f(d) ≈ -0.12 to 0.02 → negative to minimal experienced extractiveness. Immunocompromised populations in the absence scenario face victim status with zero exit: powerless, trapped, full targeting → d ≈ 0.95 → maximum extraction. The medical autonomy norm faces mixed status: it provides coordination function but is overridden by the public health claim → d ≈ 0.60 → moderate experienced extractiveness. The derived directionality values reveal the hidden victim set (immunocompromised) that the public_health_primary reading foregrounds — by mandating vaccination, the constraint shifts extraction burden from immunocompromised individuals (in absence scenario) to vaccine-hesitant individuals (in mandate scenario), creating an asymmetric redistribution of harm.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy at the level of the contested kernel. The public_health_primary reading prevents misclassifying the mandate as pure extraction (snare) by foregrounding the genuine coordination function: protection of vulnerable populations from serious harm. If the coordination function were absent (if the mandate served only to suppress vaccine hesitancy without protecting anyone), the constraint would classify as snare. But because identifiable vulnerable populations genuinely depend on vaccination compliance, the tangled_rope classification is correct — the constraint coordinates genuine protection while extracting compliance from those with autonomy concerns. Mandatrophy resolution here depends on accepting the empirical premise that vulnerable populations would face severe extraction harm in the absence of the mandate (immunocompromised individuals have zero exit options and depend entirely on surrounding vaccination rates). If this premise is false (if vulnerable populations can protect themselves through isolation, treatment, post-exposure prophylaxis), then the mandate's coordination function collapses and the constraint becomes pure snare. The omega variable addressing alternative mitigation sufficiency is therefore crucial: if alternatives are sufficient, mandatrophy tips toward snare; if alternatives are insufficient, mandatrophy confirms tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_disease_severity,
    'What level of disease severity (mortality rate, hospitalization burden, vulnerable population size) triggers state authority to compel vaccination?',
    'Empirical epidemiological data on pathogen severity, population-level risk stratification, comparative analysis of mandate triggers across disease types (measles, COVID-19, pertussis, influenza)',
    'If threshold is high (>1% population mortality): mandates apply only to most severe threats; most reading disagreements dissolve through proportionality. If threshold is low (<0.1% excess mortality): mandates apply broadly; public health primary reading becomes dominant; bodily autonomy reading becomes foreclosed in practical governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_disease_severity, empirical, 'Empirical threshold for disease severity warranting mandate authority').

omega_variable(
    vaccine_safety_contraindication_scope,
    'How many individuals must have legitimate medical contraindications to vaccination before the mandate itself becomes unjustifiable (rather than carving out exemptions)?',
    'Clinical epidemiology: proportion of population with genuine absolute contraindications; comparative risk analysis (vaccine risk vs pathogen risk for contraindicated groups); feasibility of alternative protection (post-exposure prophylaxis, isolation protocols)',
    'If contraindicated population is <5%: mandate with medical exemptions is proportional (scaffold/rope). If >20%: universal mandate creates unjustifiable harm to significantly-sized group (proportionality reading forecloses public health primary); mandate must target subpopulations only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vaccine_safety_contraindication_scope, empirical, 'Proportion of population with medical contraindications to vaccination').

omega_variable(
    reading_kernel_foreclosure,
    'Does the public health primary reading logically foreclose the bodily autonomy reading, or can both coexist as different prioritizations within a single legal framework?',
    'Normative/philosophical analysis: Can a legal system simultaneously hold that (a) bodily integrity is a fundamental right AND (b) the state may compel vaccination when protection of vulnerable populations is at stake? Or do these premises contradict in a way that requires choosing one? Case law analysis: how have courts resolved conflicts between autonomy and public health duties?',
    'If mutually foreclosing: one reading must be rejected; the kernel resolves to winner-take-all. If coexistent: both readings remain live; legal frameworks balance them through proportionality doctrine (which itself is the third sibling reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether bodily autonomy and public health protection premises are mutually foreclosing or coexistent').

omega_variable(
    vulnerable_population_definition_ambiguity,
    'Who counts as a ''vulnerable population'' in need of protection — only those with medical vulnerability (immunocompromised, infants, elderly) or also those with social/economic vulnerability?',
    'Definition analysis: scope of ''serious harm'' in the mandate''s legitimacy claim. Healthcare policy review: which populations are actually protected by vaccine mandates vs which remain exposed through other mechanisms (social isolation, testing access, treatment availability). Empirical assessment: does mandate protect the defined vulnerable group or does it protect primarily the unvaccinated from infectious pressure?',
    'If narrow (medical vulnerability only): mandate targets protection of ~3-5% of population; proportionality framework applies strictly. If broad (social vulnerability included): mandate targets ~20-30% of population; scope of protected interest expands significantly; public health primary reading gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_definition_ambiguity, conceptual, 'Definition scope of ''vulnerable populations'' requiring protection').

omega_variable(
    alternative_mitigation_sufficiency,
    'Are less restrictive alternatives (testing, isolation protocols, targeted protection for vulnerable groups, post-exposure prophylaxis) sufficient to protect vulnerable populations without universal vaccination mandates?',
    'Comparative effectiveness analysis: epidemiological modeling of mandate vs alternatives under various disease transmission scenarios. Real-world case studies: jurisdictions using alternative approaches (Sweden''s focused protection model, Taiwan''s early containment strategy, UK''s risk-stratified approach). Feasibility assessment: resource requirements, compliance capacity, implementation success across socioeconomic strata.',
    'If alternatives are sufficient: proportionality reading gains precedence; public health primary reading loses mandate force; legitimate state authority narrows to targeted interventions. If alternatives fail structurally: public health primary reading is correct; bodily autonomy reading becomes subordinate to protection duty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_mitigation_sufficiency, empirical, 'Sufficiency of less restrictive alternatives to protect vulnerable populations').

omega_variable(
    reading_dependence_of_epsilon,
    'Is the extractiveness value (0.58) stable across different readings of the mandate kernel, or does it inherently depend on which reading''s moral premises are adopted?',
    'Comparative ε analysis: compute extractiveness under bodily autonomy primary reading (would assign d closer to 1.0 for unvaccinated individuals; would get higher ε), under proportionality reading (would condition ε on disease severity and alternative availability; would get lower ε). Check whether ε is property of physical/institutional facts or property of normative frame.',
    'If ε is reading-dependent: the constraint is fundamentally contested; no single ε value is frame-independent. If ε is frame-independent: the readings are empirically distinguishable; evidence can resolve which reading is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_dependence_of_epsilon, conceptual, 'Whether extractiveness value is reading-dependent or frame-independent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_pub_theater_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mandate_pub_theater_t6, mandate_legitimacy_scope__public_health_primary, theater_ratio, 6, 0.31).
narrative_ontology:measurement(mandate_pub_theater_t12, mandate_legitimacy_scope__public_health_primary, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(mandate_pub_extract_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mandate_pub_extract_t6, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(mandate_pub_extract_t12, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mandate_pub_supp_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mandate_pub_supp_t6, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(mandate_pub_supp_t12, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% The mandate_legitimacy_scope kernel decomposes into three structurally distinct constraint stories: public_health_primary (this file) centers protection duty and produces tangled_rope with ε=0.58; bodily_autonomy_primary centers autonomy violation and produces snare with ε=0.72; proportionality_reading gates both through disease severity and alternative availability, producing rope or scaffold depending on context. These are not the same constraint viewed from different angles — they instantiate different ε values and different beneficiary/victim sets by adopting different moral premises. The ε-invariance principle applies: if changing which reading you adopt changes the ε value substantially, you have multiple constraints (one per reading), not one constraint with multiple perspectives. Each reading must be authored as a separate JSON story and linked through this network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
