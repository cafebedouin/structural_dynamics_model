% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Mandate Legitimacy: Proportionality Reading (Disease Severity, Safety, Alternatives)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This is the PROPORTIONALITY READING of the contested kernel 'mandate
 *   legitimacy scope.' This reading asserts that vaccine mandate legitimacy
 *   depends on THREE conditional factors: (1) disease severity (CFR,
 *   hospitalization rate, R0, population vulnerability), (2) vaccine safety
 *   and efficacy margins (established through trials, post-market
 *   surveillance), and (3) availability of less restrictive alternatives
 *   (quarantine capacity, remote work feasibility, targeted protection of
 *   vulnerable groups). Under this reading, a measles mandate is legitimate
 *   (CFR 0.2%, highly contagious, no quarantine alternative in dense
 *   populations, safe vaccine); a seasonal influenza mandate is not (CFR
 *   0.01%, endemic circulation, remote work viable, myocarditis risk for
 *   young adults non-negligible). The constraint exhibits the full range of
 *   classification types depending on disease context and observer position.
 *   The proportionality reading is a middle position between two sibling
 *   readings: BODILY AUTONOMY PRIMARY (no mandate is legitimate regardless of
 *   severity) and PUBLIC HEALTH PRIMARY (state authority to compel
 *   vaccination is inherently legitimate to protect vulnerable populations).
 *   This reading instantiates proportionality as a normative commitment with
 *   conditional legitimacy, making victim set and extraction magnitude
 *   dependent on measurable disease parameters rather than settled in
 *   advance.
 *
 * KEY AGENTS:
 *   - Vaccine-Hesitant Individuals in High-Severity Pathogen Context: Primary victim (powerless/trapped) — no exit options when disease severity is genuine and alternatives unavailable
 *   - Vaccine-Hesitant Individuals in Low-Severity Pathogen Context: Secondary victim (powerless/mobile) — retain exit options when alternatives exist; extraction is reduced
 *   - Rare Adverse Reaction Sufferers: Primary victim (powerless/trapped) — bear concentrated harm; victim status is independent of pathogen severity and challenges proportionality judgment
 *   - Vulnerable Populations (immunocompromised, infants, elderly): Primary beneficiary (powerless/arbitrage) — protected by herd immunity; benefit is genuine but not symmetrical across pathogens
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — implements mandate, retains authority to adjust scope by disease parameters; benefits through prevention of serious outbreak harm
 *   - Proportionality-Governance Coalition: Secondary actor (organized/constrained) — international bodies, ethicists, commissions advocating for conditional mandate policy; has agency through evidence and norms
 *   - Analytical Observer: Universal perspective — risks naturalizing proportionality parameters as fixed laws of nature rather than contested normative commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.48).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.58).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Mandate Legitimacy: Proportionality Reading (Disease Severity, Safety, Alternatives)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '605fbd62-49b4-40a2-8a5a-a418b518b46d').
narrative_ontology:cs_kernel_codification('605fbd62-49b4-40a2-8a5a-a418b518b46d', distributed).
narrative_ontology:cs_authority_grounding('605fbd62-49b4-40a2-8a5a-a418b518b46d', distributed).
narrative_ontology:cs_reading_relation('605fbd62-49b4-40a2-8a5a-a418b518b46d', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('605fbd62-49b4-40a2-8a5a-a418b518b46d', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('605fbd62-49b4-40a2-8a5a-a418b518b46d', foundational, mandate_legitimacy_conditional_on_disease_parameters).
narrative_ontology:cs_axiom_status(mandate_legitimacy_conditional_on_disease_parameters, holdable).
narrative_ontology:cs_axiom_grounding('605fbd62-49b4-40a2-8a5a-a418b518b46d', mandate_legitimacy_conditional_on_disease_parameters, instrumental).
narrative_ontology:cs_axiom('605fbd62-49b4-40a2-8a5a-a418b518b46d', foundational, proportionality_judgment_requires_parameter_evidence).
narrative_ontology:cs_axiom_status(proportionality_judgment_requires_parameter_evidence, holdable).
narrative_ontology:cs_axiom_grounding('605fbd62-49b4-40a2-8a5a-a418b518b46d', proportionality_judgment_requires_parameter_evidence, deontological).
narrative_ontology:cs_reference_frame('605fbd62-49b4-40a2-8a5a-a418b518b46d', conditional_mandate_authority).
narrative_ontology:cs_drift_state('605fbd62-49b4-40a2-8a5a-a418b518b46d', post_pandemic_endemic_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('605fbd62-49b4-40a2-8a5a-a418b518b46d', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, protected_vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authority).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, rare_adverse_reaction_sufferers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANDATORY SUBJECT IN HIGH-SEVERITY PATHOGEN CONTEXT (SNARE) — When disease severity is high (measles: CFR 0.2%, R0 12-18) and alternatives are genuinely unavailable (crowded living conditions, no quarantine feasibility), the mandate traps the subject. Exit options are structurally absent: refusal results in functional exclusion from society, employment, education. The proportionality reading acknowledges this extraction but claims it is justified by disease parameters. Experienced chi is maximal for this agent class.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MANDATORY SUBJECT IN LOW-SEVERITY PATHOGEN CONTEXT (ROPE) — When disease severity is low (seasonal influenza: CFR 0.01%, endemic circulation), alternatives exist (remote work, masking, targeted protection of vulnerable groups), and vaccine safety margins are narrow (rare myocarditis risk in young adults), the constraint shifts structurally. The subject retains exit options: relocation to jurisdictions without mandate, occupational transition, temporary employment gap. The constraint becomes coordination (communicating risk information) rather than pure extraction. Experienced chi is moderate.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE WORKER IN MODERATE-SEVERITY CONTEXT (TANGLED ROPE) — Healthcare workers face constrained exit: career investment is high, relocation is costly, occupational retraining is time-consuming. They benefit from working in a protected patient environment (genuine coordination function: staff vaccination reduces nosocomial transmission, protecting vulnerable patients). But they also bear costs: occupational mandate removes choice, rare adverse reactions concentrate on this population segment, career penalty for vaccine hesitancy is real. The proportionality reading's constraint is genuinely hybrid — both coordination and extraction operate.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY / MANDATE IMPLEMENTER (ROPE) — The authority experiences the constraint as coordination: communicating vaccine requirements solves collective action problems (free-rider behavior, herd immunity thresholds). The authority retains exit options: adjusting mandate scope by pathogen severity, deploying graduated enforcement (education before mandate, incentives before coercion), sunset clauses when conditions improve. The proportionality reading is the authority's native framing — legitimacy depends on parameters that the authority claims to measure. Net beneficiary through coordination function.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROPORTIONALITY-GOVERNANCE COALITION (SCAFFOLD) — International bodies (WHO), ethicists, and public health commissions see the mandate legitimacy question as having a sunset: as disease severity declines (vaccination coverage rises, pathogen evolves, surveillance capacity improves), the justification for mandates erodes. The coalition has agency through advocacy, evidence production, and normative pressure. The constraint is temporary — explicitly conditional on disease parameters. Theater is moderate because proportionality tests are ostensibly objective (CFR data, safety surveillance, quarantine capacity assessment).
constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL-LAW FRAME (MOUNTAIN) — From a universalizing perspective, some degree of public health authority over infectious disease control is an immutable feature of how human populations survive communicable pathogens. The constraint appears as a natural law: epidemic control always requires some constraint on individual behavior; proportionality is merely a description of how that constraint scales. However, this perspective naturalizes what the proportionality reading treats as contingent — the specific parameters (CFR threshold, safety margin, alternative availability) that trigger legitimate mandate authority are not natural laws but normative commitments. False summit candidate.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandate_legitimacy_scope__proportionality_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. This value reflects the proportionality reading's core claim — extraction magnitude is conditional on disease parameters. The measurement trajectory shows this: at t0 (endemic measles context), ε = 0.35 (high severity, clear mandate legitimacy, victims are those without alternatives). At t5 (Omicron emergence, moderate severity/high transmissibility), ε = 0.48 (peak uncertainty: severity claims were contested, alternative availability was disputed, proportionality judgment was sticky — authorities resisted downward revision). At t10 (endemic transition), ε = 0.38 (proportionality judgment was updated: severity declined, alternatives became viable, mandate scope was reduced). This trajectory is the proportionality reading's signature — extractiveness oscillates with disease severity rather than remaining fixed. Suppression (0.58): Moderate-high. The proportionality reading acknowledges substantial suppression: mandates remove occupational, educational, and social exit options for those who refuse. But suppression is parametric — high in high-severity disease contexts (measles, polio), lower in low-severity contexts (influenza, RSV). Theater ratio (0.64): Moderate. The proportionality reading claims that legitimacy judgment is grounded in objective disease parameters (CFR, safety data, alternative availability), reducing theater. But governance practice shows significant theater: CFR claims are contested, safety interpretation is value-laden, alternative availability is assessed post-hoc. The moderate theater reflects the reading's aspirational objectivity vs. actual contestation.
 *
 * PERSPECTIVAL GAP:
 *   The proportionality reading generates the largest perspectival gap between high-severity and low-severity pathogen contexts. In high-severity (measles), the mandatory subject, healthcare worker, and even proportionality-governance coalition see mandate as broadly justified; extraction is acknowledged but treated as proportional to benefit. In low-severity (influenza), the same agents shift dramatically: mandatory subject sees snare/rope, healthcare worker sees tangled rope with weaker justification, coalition sees sunset as near. The analytical observer risks collapsing this into mountain ('all mandate legitimacy is inherent'), naturalizing the proportionality parameters. The sibling readings (bodily autonomy primary, public health primary) avoid this gap-risk by holding their position fixed across all contexts — for bodily autonomy primary, NO mandate is legitimate; for public health primary, ALL mandates are legitimate (when needed to protect vulnerable groups). The proportionality reading's strength is acknowledging the gap; its weakness is that the parameters determining which side of the gap applies are contested, not objective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies substantially across perspectives because exit options and beneficiary/victim status are pathogen-conditional under the proportionality reading. High-severity pathogen context: mandatory subject is trapped (d ≈ 0.95, f(d) ≈ 1.42, high experienced extraction χ). Low-severity pathogen context: mandatory subject is mobile (d ≈ 0.85, f(d) ≈ 1.15, moderate χ). Healthcare worker: constrained exit (d ≈ 0.58, f(d) ≈ 0.75, moderate χ). Authority: arbitrage exit (d ≈ 0.05, f(d) ≈ -0.12, negative χ, institutional beneficiary). The key feature: in the proportionality reading, d is NOT a fixed property of the agent but varies with the pathogen's severity, the vaccine's safety profile, and the availability of alternatives. An agent who is a 'victim' in high-severity contexts (trapped) may be a participant in low-severity contexts (mobile with genuine exit options). This parametric directionality is what distinguishes the proportionality reading from the sibling readings, which treat mandate legitimacy (or illegitimacy) as fixed regardless of pathogen parameters.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading resolves mandatrophy by treating victim set and extraction magnitude as conditional variables rather than fixed properties. The question 'Is a vaccine mandate legitimate?' is answered: 'It depends on disease severity, vaccine safety, and alternative availability.' This is not a dodge — it is a genuine structural claim. The constraint's legitimacy oscillates over time as disease parameters change. Measles mandate is snare-like (trapped subjects) when measles is actively circulating; it transitions to rope-like (coordination) when circulation is eliminated and mandates become preventive. Influenza mandate remains snare-like only in high-risk populations (elderly, immunocompromised); it is rope or scaffold for the general population (low severity, viable alternatives). The resolution rests on accepting that proportionality parameters are measurable and that authorities will update mandate scope when parameters change. If authorities maintain mandates past proportionality (e.g., enforcing measles mandate in measles-eliminated territory), the constraint morphs into snare regardless of the proportionality reading's claims — this is the core vulnerability. If authorities update scope responsively, the reading is vindicated. The mandatrophy is thus empirical: which behavior do real authorities exhibit?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold_specification,
    'What quantitative thresholds for CFR, hospitalization rate, and R0 distinguish ''sufficient severity'' from ''insufficient severity'' under the proportionality principle?',
    'International expert consensus on disease parameter thresholds; comparison of WHO guidance on mandate legitimacy across pathogens with established CFR/R0 data; analysis of which mandate decisions reversed after pathogen evolution (e.g., influenza, COVID-19 Omicron)',
    'If thresholds are formally specified and applied consistently: proportionality reading is operationalizable and reduces arbitrariness. If thresholds are implicit or vary jurisdictionally: the reading collapses into post-hoc justification (victim set becomes whoever loses the ad-hoc judgment). If thresholds differ across sibling readings: foreclosure relation may hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_specification, empirical, 'Quantitative disease severity threshold for proportionality judgment').

omega_variable(
    alternative_availability_measurement,
    'How is ''availability of less restrictive alternatives'' measured and verified in real jurisdictions applying the proportionality principle?',
    'Analysis of mandate decisions with documented consideration of alternatives (quarantine infrastructure, remote work capacity, targeted protection protocols); comparison of jurisdictions that implemented proportionality vs. non-proportional mandates; post-hoc assessment of whether alternatives actually worked when deployed',
    'If alternatives are rigorously assessed: proportionality reading is an active governance constraint. If alternatives assessment is perfunctory: the reading functions as cover story for uniform mandates, and the constraint becomes snare for all non-beneficiary perspectives. If alternatives prove infeasible in practice despite claims: victim classification expands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_availability_measurement, empirical, 'Whether alternatives to mandates are genuinely available and assessed').

omega_variable(
    safety_efficacy_evidence_dynamics,
    'How do evolving safety and efficacy data change mandate legitimacy under the proportionality principle? Who has authority to declare ''safety/efficacy no longer sufficient''?',
    'Timeline analysis: when did WHO, national health authorities, ethicists declare vaccines sufficiently safe/efficacious? When did safety signals emerge (myocarditis, rare adverse events)? When did efficacy wane (variant escape, durability)? What governance mechanism updated proportionality judgment in response?',
    'If authority to revise legitimacy judgment is clear and exercised: proportionality reading is a living constraint with conditional legitimacy. If judgment is sticky (authority resists revising downward despite evidence): proportionality becomes post-hoc rationalization, and constraint morphs toward snare. If different authorities disagree on evidence interpretation: sibling readings (bodily autonomy, public health primary) may coexist as valid rather than being foreclosed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safety_efficacy_evidence_dynamics, empirical, 'How safety/efficacy evidence updates mandate legitimacy assessment').

omega_variable(
    pathogen_specific_victim_set,
    'Is the victim set of mandate constraints truly pathogen-conditional (measles mandate has different victims than flu mandate), or does the proportionality principle collapse all mandates into a single victim set regardless of pathogen?',
    'Comparative analysis of mandate scope, enforcement intensity, and reversibility across high-CFR pathogens (measles, polio), moderate-severity (pertussis, varicella), and endemic low-severity (influenza, RSV). Assessment of whether victim burden scales with pathogen parameters or remains constant.',
    'If victim set is truly pathogen-conditional: proportionality reading is operationally distinct from sibling readings. If victim set is identical across pathogens: the proportionality framing is rhetorical, and the constraint collapses into bodily autonomy or public health primary reading depending on outcome preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_specific_victim_set, empirical, 'Whether mandate victim set is conditional on pathogen severity').

omega_variable(
    committer_frame__reading_contest,
    'Is the proportionality reading a coherent normative commitment, or does it constitute a middle position that is foreclosed by the logical structure of the kernel?',
    'Jurisprudential analysis: can a court or ethical body hold both ''bodily autonomy is fundamental'' and ''proportionality sometimes overrides it'' within a single framework? Or does accepting proportionality require abandoning bodily autonomy fundamentalism? Conversely, does accepting bodily autonomy fundamentalism logically foreclose proportionality judgment?',
    'If proportionality is foreclosed by bodily autonomy logic: reading_relation to bodily_autonomy_primary is ''forecloses'' (in opposite direction). If proportionality forecloses bodily autonomy: relation is ''forecloses''. If both coexist in contemporary jurisprudence: relation is ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame__reading_contest, conceptual, 'Whether proportionality reading is logically coherent or foreclosed by kernel commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mlsp_theater_t0_proportionality_framing, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(mlsp_theater_t5_emergency_theater, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 5, 0.64).
narrative_ontology:measurement(mlsp_theater_t10_normalization, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(mlsp_t0_measles_endemic, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mlsp_t5_omicron_emergence, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mlsp_t10_endemic_transition, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mlsp_supp_t0_initial, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(mlsp_supp_t5_peak_enforcement, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(mlsp_supp_t10_declining, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).

% DUAL FORMULATION NOTE:
% mandate_legitimacy_scope is a contested kernel with three structurally distinct readings. proportionality_reading instantiates the conditional/parametric position. bodily_autonomy_primary instantiates the fundamental-right position (mandate never legitimate). public_health_primary instantiates the state-authority position (mandate legitimate when needed for vulnerable protection). Each reading produces different ε values, different victim sets, and different classifications from key perspectives. They are not alternative measurements of one constraint but structurally distinct constraints generated by different normative commitments about the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, powerless, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
