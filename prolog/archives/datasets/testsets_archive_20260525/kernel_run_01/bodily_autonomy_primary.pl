% ============================================================================
% CONSTRAINT STORY: bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bodily_autonomy_primary, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bodily_autonomy_primary
 *   human_readable: Medical Intervention Without Consent as Structural Extraction
 *   domain: medical_ethics/public_health_policy/constitutional_law
 *
 * SUMMARY:
 *   This constraint models the bodily_autonomy_primary reading of the
 *   contested kernel coercion_legitimacy_boundary — the foundational
 *   constitutional question of whether medical intervention without consent
 *   is ever permissible, and under what conditions. This reading instantiates
 *   the axiom that bodily autonomy is categorically inviolable: no aggregate
 *   public health benefit can override an individual's right to refuse
 *   medical intervention on their own body. The reading acknowledges that
 *   public health emergencies create genuine coordination problems (epidemics
 *   do require collective action) but maintains that these problems must be
 *   solved through persuasion, incentive, and voluntary participation — never
 *   through coercion of medical procedures themselves. The constraint
 *   exhibits as a Snare (pure extraction with minimal coordination function)
 *   from the perspective of individuals refusing intervention, but as Rope
 *   (genuine coordination) from public health authorities' perspective and as
 *   Tangled Rope (mixed coordination and extraction) from immunocompromised
 *   populations who benefit from high vaccination rates but are harmed by
 *   erosion of institutional trust caused by coercive enforcement. The rising
 *   theater_ratio (0.38→0.55) reflects institutional drift: enforcement
 *   mechanisms become increasingly performative and punitive rather than
 *   epidemiologically targeted as the emergency context fades and
 *   institutional enforcement infrastructure persists through inertia.
 *
 * KEY AGENTS:
 *   - Individuals Refusing Intervention: Primary victim (powerless/trapped) — no exit options; faces coercion or exclusion; bears full extraction cost
 *   - Public Health Authorities: Primary beneficiary (institutional/arbitrage) — design and enforce mandates; concentrated extraction benefits; high exit flexibility
 *   - Immunocompromised Populations: Secondary victim-beneficiary (moderate/constrained) — benefit from high vaccination rates but harmed by mandate-driven institutional distrust; limited exit options
 *   - Medical Refuser Communities (Organized): Secondary actor (organized/constrained) — build alternative governance proposals; see sunset path through informed consent frameworks
 *   - Medical Licensing Bodies: Institutional actor (institutional/arbitrage) — maintain enforcement ritual; benefit from fine revenue and compliance signaling; degraded coordination function (piton)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing policy choice as biological necessity; framework reveals false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(bodily_autonomy_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bodily_autonomy_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bodily_autonomy_primary, snare).
narrative_ontology:human_readable(bodily_autonomy_primary, "Medical Intervention Without Consent as Structural Extraction").
narrative_ontology:topic_domain(bodily_autonomy_primary, "medical_ethics/public_health_policy/constitutional_law").

domain_priors:requires_active_enforcement(bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(bodily_autonomy_primary, formalized).
narrative_ontology:cs_authority_grounding(bodily_autonomy_primary, lineage).
narrative_ontology:cs_interpretation_layer_present(bodily_autonomy_primary).
narrative_ontology:cs_kernel_id(bodily_autonomy_primary, coercion_legitimacy_boundary).
narrative_ontology:cs_reading_relation(bodily_autonomy_primary, public_health_primary, forecloses).
narrative_ontology:cs_reading_relation(bodily_autonomy_primary, proportionality_reading, coexists_with).
narrative_ontology:cs_axiom(bodily_autonomy_primary, foundational, bodily_autonomy_categorically_protected).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorically_protected, holdable).
narrative_ontology:cs_axiom(bodily_autonomy_primary, foundational, coercive_medical_intervention_impermissible).
narrative_ontology:cs_axiom_status(coercive_medical_intervention_impermissible, holdable).
narrative_ontology:cs_reference_frame(bodily_autonomy_primary, enlightenment_bodily_integrity).
narrative_ontology:cs_drift_state(bodily_autonomy_primary, post_pandemic_mandate_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(bodily_autonomy_primary, vaccinated_populations).
narrative_ontology:constraint_victim(bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(bodily_autonomy_primary, bodily_autonomy_holders).
narrative_ontology:constraint_victim(bodily_autonomy_primary, medical_decision_refusers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL REFUSER (SNARE) — Faces coercive pressure to accept medical intervention. Exit options are severely constrained: employment loss, school exclusion, social stigma, legal penalties, or forced medical procedures. The constraint extracts obedience through suppression of alternatives. No genuine coordination function exists from this agent's perspective — only extraction of bodily submission. Maximum experienced extraction.
constraint_indexing:constraint_classification(bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMUNOCOMPROMISED POPULATIONS (TANGLED ROPE) — Structurally ambiguous position. These populations benefit from high vaccination rates (genuine coordination function — herd immunity protection). But they also bear costs: policy enforcement may rely on coercive mechanisms that undermine trust in public health institutions, increasing long-term non-compliance that ultimately reduces their protection. Asymmetric extraction (enforcement burden) combined with real coordination benefit (herd immunity). Constrained exit: cannot safely opt out of the health system.
constraint_indexing:constraint_classification(bodily_autonomy_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITIES (ROPE) — See the constraint as pure coordination: mandates align individual incentives with collective protection. From this perspective, the extraction (enforcement mechanisms, compliance monitoring) is justified as necessary coordination overhead. High arbitrage options (selective enforcement, alternative compliance pathways for compliant populations). The extraction runs toward this agent — they are the beneficiary of concentrated compliance extraction.
constraint_indexing:constraint_classification(bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH GOVERNANCE REFORMERS (SCAFFOLD) — Organized agents advocating for informed consent frameworks, transparency in mandate decisions, and sunset clauses on emergency powers. See the constraint as temporary (emergency public health measures) requiring structural transition to consent-based governance. Exit path is institutional: replace coercive mechanisms with trust-building, participatory decision-making, voluntary incentives. Scaffold classification reflects the sunset logic: as governance reform proceeds, the coercive mechanism should decline.
constraint_indexing:constraint_classification(bodily_autonomy_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL LICENSING BODIES (PITON) — Institutional actors maintaining enforcement ritual long after the emergency context that justified it has passed. Theater ratio 0.55 reflects that enforcement activity (license revocation, practice restrictions) is partly performative — designed to signal compliance to the public, not to optimize health outcomes. The mechanism persists through institutional inertia, generating revenue through fines/penalties. Extraction is sustained but the coordinative function has degraded.
constraint_indexing:constraint_classification(bodily_autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective on public health crises, some degree of coordinated intervention is a structural necessity: epidemics always require collective action, and individual refusal creates negative externalities that are irreducible features of the disease ecology. This perspective risks naturalizing what is actually a contingent policy choice. The engine's false summit detector will flag this as naturalization — the structural data shows identifiable beneficiaries (public health authorities) and victims (bodily autonomy holders), revealing that emergency necessity framing conceals extractive institutional arrangements.
constraint_indexing:constraint_classification(bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bodily_autonomy_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bodily_autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts obedience and bodily compliance from individuals with no genuine exit options. The beneficiary (public health authorities) concentrates control over compliance mechanisms and gains resources/authority. The measurement trajectory shows rising extractiveness over the interval (0.42→0.68), reflecting institutional capture: as the emergency context fades, enforcement mechanisms persist and intensify, suggesting institutional preference for control rather than epidemiological necessity. Theater ratio (0.55): Moderate-high. Enforcement activities include public communications, license restrictions, employment barriers, and social stigma that serve performative functions (signaling compliance to the public) in addition to direct compliance pressure. The ratio increases over time as enforcement infrastructure becomes more ritualized. Suppression (0.72): High. Multiple barriers constrain exit: legal penalties for non-compliance, employment consequences, school exclusion, medical discrimination, social ostracism, and the irreversibility of forced intervention. These are not merely costly — they are designed to eliminate alternatives. The suppression metric reflects that the constraint aims at preventing refusal through exhaustion of escape routes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits radically different types across perspectives despite identical base extractiveness. From the powerless refuser's view, it is a Snare — pure extraction, no coordination function, maximum experienced χ. From public health authorities' view, it is a Rope — genuine coordination (aligning individual and collective incentives toward vaccination), with extraction treated as necessary overhead. From immunocompromised populations' view, it is a Tangled Rope — they benefit from the coordination function (high vaccination rates protect them) but bear costs from the enforcement mechanism (institutional distrust reduces future participation). From reform coalitions' view, it is a Scaffold — a temporary emergency measure that should sunset as governance reforms toward consent-based approaches. From licensing bodies' view, it is a Piton — the coordination function has degraded into performative enforcement ritual sustained by institutional inertia. The analytical observer's mountain view risks naturalizing the policy choice as biological necessity — but the beneficiary declarations reveal it is a contingent institutional arrangement. This perspectival range demonstrates that the constraint is genuinely hybrid: not purely extraction, not purely coordination, but a mixture whose ratio changes depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural position relative to the constraint. Individuals refusing intervention are trapped with no exit — high d (≈0.95) → high f(d) (≈1.42) → high experienced χ. Public health authorities have arbitrage options (selective enforcement, alternative compliance pathways) and are beneficiaries — low d (≈0.15) → negative f(d) (≈-0.01) → the constraint runs toward them as a benefit. Immunocompromised populations face constrained exit (cannot opt out of health systems) and are structurally ambiguous (they benefit from high vaccination rates but are harmed by enforcement mechanisms that undermine trust) — moderate d (≈0.60) → f(d) (≈0.80) → moderate experienced extraction, but with asymmetry from the coordination benefit they receive. The perspectival gap emerges from these different d values: the same constraint extracts heavily from some agents while providing coordination benefit to others.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This reading's classification as Snare (from the powerless perspective) is justified by the measurable benefits flowing to public health authorities (extracted compliance, concentrated control, compliance-derived resources). The extraction is not incidental — it is the mechanism that sustains the mandate despite declining emergency justification. However, the Tangled Rope classification from immunocompromised perspectives shows genuine coordination function exists: high vaccination rates provide real protection. The mandatrophy resolves by acknowledging both: the constraint performs real coordination (vaccination benefits public health) AND extracts obedience asymmetrically (through coercion rather than persuasion). This is exactly the Tangled Rope definition — genuine coordination plus asymmetric extraction. The beneficiary declarations (public_health_authorities, vaccinated_populations) identify who concentrates extraction benefits; the victim declarations (unvaccinated_individuals, bodily_autonomy_holders) identify who bears costs. The rising extractiveness trajectory (0.42→0.68) indicates the coordination function is weakening (as emergency context fades) while extraction mechanisms intensify (institutional capture), pushing the constraint toward pure Snare from more perspectives over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_emergency_threshold,
    'What epidemiological severity threshold legitimizes suspension of consent requirements in public health mandates?',
    'Comparative analysis of historical emergency declarations: fatality rates, transmission dynamics, vaccine efficacy, and available alternatives at declaration time. Correlation between declared emergency criteria and actual public health outcomes.',
    'If threshold requires >5% mortality: many legitimate emergency interventions misclassified as extraction. If threshold <1% mortality: extractive policies labeled as justified emergencies. Different thresholds classify the same constraint differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_emergency_threshold, empirical, 'Epidemiological severity threshold for emergency public health authority').

omega_variable(
    coercive_vs_voluntary_efficacy,
    'Do coercive public health mandates achieve better health outcomes than voluntary incentive-based approaches when measured over the same time horizon?',
    'Comparative effectiveness research: vaccination rates, compliance duration, public trust in health institutions, long-term willingness to participate in future public health measures. Control for confounding factors (disease severity, availability of alternatives, communication quality).',
    'If coercive mandates produce superior outcomes: constraint reclassifies toward Tangled Rope (genuine coordination benefit justifies some asymmetry). If voluntary approaches equal or exceed coercive outcomes: constraint reclassifies toward pure Snare (extraction provides no health gain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_vs_voluntary_efficacy, empirical, 'Comparative health efficacy of coercive vs voluntary approaches').

omega_variable(
    institutional_capture_in_mandate_design,
    'To what extent do public health mandate mechanisms reflect genuine epidemiological necessity versus institutional preference for centralized control and compliance monitoring?',
    'Policy analysis comparing mandate design across jurisdictions with identical epidemiological contexts but different governance structures. Examination of enforcement mechanisms'' alignment with stated public health goals. Comparative analysis of outcomes under different governance models (participatory vs top-down).',
    'If mandates are optimally designed for stated goals: extraction is coordination cost (lower chi, possible reclassification to Tangled Rope). If designs reflect institutional convenience rather than epidemiological need: extraction is exploitative (higher chi, confirming Snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_in_mandate_design, empirical, 'Institutional capture in public health mandate design').

omega_variable(
    bodily_autonomy_reading_kernel_ambiguity,
    'Is bodily autonomy an inviolable foundational right or a contingent presumption that can be overridden by sufficiently severe collective threats?',
    'Jurisprudential analysis across constitutional traditions; examination of how different legal systems resolve autonomy vs collective protection when both cannot be satisfied; historical cases where the boundary was moved and what criteria were used.',
    'If bodily autonomy is inviolable: this reading forecloses the public_health_primary reading (the readings are logically incompatible in a single framework). If autonomy is a contingent presumption: readings coexist (different parties hold different hierarchies of values). This is the core constitutional ambiguity the kernel_coercion_legitimacy_boundary unresolved dispute pivots on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bodily_autonomy_reading_kernel_ambiguity, conceptual, 'Inviolability status of bodily autonomy in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bodily_autonomy_primary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bap_tr_t0, bodily_autonomy_primary, theater_ratio, 0, 0.38).
narrative_ontology:measurement(bap_tr_t2, bodily_autonomy_primary, theater_ratio, 2, 0.48).
narrative_ontology:measurement(bap_tr_t4, bodily_autonomy_primary, theater_ratio, 4, 0.55).
narrative_ontology:measurement(bap_tr_t6, bodily_autonomy_primary, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(bap_be_t0, bodily_autonomy_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bap_be_t2, bodily_autonomy_primary, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(bap_be_t4, bodily_autonomy_primary, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(bap_be_t6, bodily_autonomy_primary, base_extractiveness, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(bodily_autonomy_primary, proportionality_reading).
narrative_ontology:affects_constraint(bodily_autonomy_primary, informed_consent_requirement).
narrative_ontology:affects_constraint(bodily_autonomy_primary, vaccine_mandate_institutional_authority).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel has three structurally distinct constraint readings, each with different ε values and beneficiary/victim structures: bodily_autonomy_primary (ε=0.68, this constraint) prioritizes individual rights; public_health_primary (ε=0.42) prioritizes collective outcomes and justifies coercive intervention under emergency conditions; proportionality_reading (ε=0.55) balances both through requiring minimal restriction. These are not three measurements of the same constraint — they are three different constraints derived from three different readings of the same kernel. Each reading makes different assumptions about which values are foundational, leading to different classifications of coercive public health mandates. The three constraints form a family linked through network.affects_constraints, with measurement of each one's trajectory revealing how the constitutional dispute evolves over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
