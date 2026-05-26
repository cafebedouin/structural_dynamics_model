% ============================================================================
% CONSTRAINT STORY: harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harm_reduction_reading, []).

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
 *   constraint_id: harm_reduction_reading
 *   human_readable: Harm Reduction Authority Framework (Substance Control)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading of substance control authority positions
 *   continued drug use as inevitable (at least in the medium term) and
 *   grounds state authority not in use prevention but in disease and overdose
 *   reduction. This represents a fundamental reframing of what 'control'
 *   means: from preventing use to preventing harm from use. The reading
 *   instantiates one specific authority structure — health-based,
 *   service-provision dependent, surveillance-enabled, and deliberately
 *   temporary — that coexists with prohibition reading (which seeks to
 *   prevent use through criminalization) and legalization reading (which
 *   seeks to remove state control entirely). The harm reduction reading is
 *   distinctive in accepting both continued use AND state engagement, trading
 *   criminal enforcement for health authority. The constraint exhibits
 *   Tangled Rope characteristics because it genuinely coordinates disease
 *   prevention (measurably successful — 90% HIV reduction in Taiwan's needle
 *   exchange programs) while simultaneously extracting through mandatory
 *   state contact, health data surveillance, and relocation of users from
 *   criminal to medical control. Extractiveness (0.38) reflects moderate
 *   extraction: the public health coordination function is genuine and
 *   produces measurable outcomes, but the surveillance mechanisms and
 *   requirement for users to engage with the state to avoid criminalization
 *   create real suppression (0.42). Theater ratio (0.35) indicates low
 *   performativity — harm reduction services operate on directly measurable
 *   outcomes (HIV tests, overdose rescue interventions, medication adherence)
 *   rather than on symbolic compliance.
 *
 * KEY AGENTS:
 *   - Active Drug Users: Primary victims (powerless/trapped) — bears extraction through mandatory health contact and state surveillance while gaining access to life-saving services
 *   - Harm Reduction Service Providers: Primary beneficiaries (institutional/arbitrage) — gain authority, funding, and institutional legitimacy through service coordination; also bear constraint through suppression of alternative service models
 *   - Public Health Authority: Secondary beneficiary (institutional/arbitrage) — claims legitimate mandate through disease reduction outcomes; experiences constraint as pure coordination enabling their core mission
 *   - Law Enforcement / Criminal Justice System: Secondary victim (powerful/mobile) — loses direct enforcement jurisdiction but gains harm data; experiences constraint as coordinating public order while extracting through authority displacement
 *   - Abstinence-Based Treatment Establishment: Tertiary victim (institutional/constrained) — institutional interests in abstinence-only models are degraded by harm reduction's acceptance of continued use; maintains rhetoric without logical coherence (piton pattern)
 *   - Global Drug Policy Reform Coalition: Organized observer (organized/constrained) — views harm reduction as intentionally temporary scaffold toward full legalization; actively building exit pathways
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent reading's institutional choices as epidemiological inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harm_reduction_reading, 0.38).
domain_priors:suppression_score(harm_reduction_reading, 0.42).
domain_priors:theater_ratio(harm_reduction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harm_reduction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(harm_reduction_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(harm_reduction_reading, "Harm Reduction Authority Framework (Substance Control)").
narrative_ontology:topic_domain(harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(harm_reduction_reading, formalized).
narrative_ontology:cs_authority_grounding(harm_reduction_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(harm_reduction_reading).
narrative_ontology:cs_kernel_id(harm_reduction_reading, substance_control_authority).
narrative_ontology:cs_reading_relation(harm_reduction_reading, prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation(harm_reduction_reading, legalization_reading, influences).
narrative_ontology:cs_axiom(harm_reduction_reading, foundational, continued_use_inevitable_medium_term).
narrative_ontology:cs_axiom_status(continued_use_inevitable_medium_term, holdable).
narrative_ontology:cs_axiom_grounding(harm_reduction_reading, continued_use_inevitable_medium_term, empirically_contingent).
narrative_ontology:cs_axiom(harm_reduction_reading, foundational, disease_reduction_primary_mandate).
narrative_ontology:cs_axiom_status(disease_reduction_primary_mandate, holdable).
narrative_ontology:cs_axiom_grounding(harm_reduction_reading, disease_reduction_primary_mandate, deontological).
narrative_ontology:cs_reference_frame(harm_reduction_reading, health_authority_disease_control).
narrative_ontology:cs_drift_state(harm_reduction_reading, contemporary_legalization_pressure, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harm_reduction_reading, public_health_services).
narrative_ontology:constraint_beneficiary(harm_reduction_reading, harm_reduction_organizations).
narrative_ontology:constraint_victim(harm_reduction_reading, active_users_structural_position).
narrative_ontology:constraint_victim(harm_reduction_reading, treatment_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACTIVE DRUG USER (SNARE) — The harm reduction framework trades criminalization for mandatory health contact. Users face full extraction: continued use is now visible to the state (via needle programs, testing, medication enrollment), enabling both service provision AND surveillance/control. Exit options are severely constrained — exit the service and face re-criminalization; accept the service and accept state tracking. Suppression is structural: users cannot refuse health engagement without losing harm-reduction protections. The constraint operates as pure extraction from this position despite its public health framing.
constraint_indexing:constraint_classification(harm_reduction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HARM REDUCTION SERVICE PROVIDERS (TANGLED ROPE) — Service providers experience genuine coordination: they coordinate disease prevention (syringe programs, medication-assisted treatment, overdose rescue training) with user safety and public health access. But the services also extract: providers gain institutional authority, funding dependency, and data about users. They benefit from the constraint's legitimacy while accepting suppression of alternative service models (peer-led, non-medicalized, user-controlled). Moderate extraction because services genuinely reduce harm alongside their authority-building function.
constraint_indexing:constraint_classification(harm_reduction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — The institutional actor (department of health, CDC, public health boards) experiences the constraint as pure coordination: harm reduction is positioned as their primary mandate, enabling them to claim credibility through measurable outcomes (HIV reduction, overdose prevention, disease suppression). Extraction runs toward this agent in the form of authority legitimacy, but the coordination function is genuine — the state's interest in reducing disease aligns with users' interest in staying alive. Arbitrage exit options reflect the authority's ability to switch frameworks (decriminalization, treatment expansion) without material loss.
constraint_indexing:constraint_classification(harm_reduction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT / CRIMINAL JUSTICE (TANGLED ROPE) — Law enforcement experiences the harm reduction framework as a constraint that both enables and extracts from them. It enables coordination of public order (visible overdoses, associated crime reduced through medication and service engagement). But it extracts by reducing enforcement jurisdiction (users are now in health systems, not criminal systems), removing direct control mechanisms, and shifting risk to health services. The system can exit through re-criminalization (reverting to prohibition reading), but this is costly (loss of harm data, public health credibility). Moderate extraction reflects the hybrid: some coordination function (disease reduction aids public order) alongside loss of prior authority.
constraint_indexing:constraint_classification(harm_reduction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ABSTINENCE-BASED TREATMENT ESTABLISHMENT (PITON) — Traditional addiction treatment models (abstinence-first, 12-step, therapeutic communities) experience harm reduction as degrading their foundational premise: that abstinence is the only legitimate outcome. Harm reduction permits continued use under management, which contradicts abstinence frameworks. The establishment persists through institutional inertia and continued funding streams despite loss of logical coherence. Theater ratio is high because the establishment maintains abstinence rhetoric while actual practice increasingly incorporates harm reduction principles. Suppression of alternative models (medication-assisted treatment, peer-led recovery) is substantial.
constraint_indexing:constraint_classification(harm_reduction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL DRUG POLICY REFORM COALITION (SCAFFOLD) — Organized actors (drug policy reform NGOs, harm reduction international networks, public health coalitions) experience the harm reduction framework as a transitional structure with a sunset clause: as evidence accumulates and political pressure builds, full legalization + public health infrastructure becomes the endgame. The constraint's enforcement is temporary (harm reduction operates within prohibition's legal frame) and declining as legalization frameworks mature. Exit path is clear (decriminalization, legal medical access) and actively pursued. The coalition sees extraction as deliberately temporary — a stepping stone, not a permanent arrangement.
constraint_indexing:constraint_classification(harm_reduction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / EPIDEMIOLOGICAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal perspective treating the harm reduction framework as grounded in immutable epidemiological facts: unsafe drug use practices INHERENTLY generate disease transmission, overdose death, and public health burden. Authority derives from this natural fact — any system that acknowledges continued use while addressing disease must adopt the harm reduction structural pattern (surveillance-service hybrid). The constraints are the laws of epidemiology itself, not policy choices. However, this perspective naturalizes a contingent institutional framing and risks misidentifying the reading's commitments as universal requirements.
constraint_indexing:constraint_classification(harm_reduction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harm_reduction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harm_reduction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harm_reduction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(harm_reduction_reading, TR),
    TR >= 0.70.

:- end_tests(harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting the reading's core tension. The constraint genuinely coordinates disease prevention (measurable outcomes justify institutional investment), but the coordination occurs within a framework that mandates user engagement with the state. Users cannot access life-saving services without entering the surveillance apparatus. The extractiveness is not high (0.46+) because the health coordination function is substantial and measurable, not theatrical — harm reduction programs track concrete outcomes (HIV status, overdose survival, medication adherence). It is not low (0.25-) because the mandatory engagement and state visibility create real asymmetric control. Suppression (0.42): Moderate-high. Users face significant barriers to refusing the constraint without losing health access. Criminalization technically remains as the alternative, so users are suppressed between active use (with health services) and recriminalization (without services). Law enforcement is suppressed by loss of direct jurisdiction. Abstinence-based treatment is suppressed by institutional displacement. But suppression is not maximal because: (a) users can exit entirely (though at high cost), (b) harm reduction is explicitly framed as user-centered (permissive of continued use), and (c) the constraint is recognized as temporary in some jurisdictions. Theater ratio (0.35): Low. Harm reduction services measure success through epidemiological outcomes, not symbolic compliance. Syringe programs count sterile needles distributed and HIV infections prevented, not rehabilitation narratives. Medication-assisted treatment tracks opioid use levels and overdose survival, not abstinence declarations. The services are functionally focused rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The seven perspectives instantiate the full range of DR classification from identical base metrics (ε=0.38, suppression=0.42, theater=0.35, beneficiaries and victims declared). The active user sees Snare — maximum extraction through mandatory state contact. The service provider sees Tangled Rope — genuine disease coordination alongside institutional authority-building. The health authority sees Rope — pure coordination of their legitimate mandate. Law enforcement sees Tangled Rope — public order coordination alongside loss of enforcement power. Abstinence establishment sees Piton — degraded institutional model maintained through inertia and residual funding. The reform coalition sees Scaffold — intentionally temporary arrangement with clear sunset (legalization). The analytical observer risks seeing Mountain — an epidemiological inevitability. This perspectival explosion illustrates the core DR insight: a constraint's type is not intrinsic to the constraint but relative to the observer's position within it. The mandatrophy is resolved by recognizing that all seven classifications are accurate from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position. Active users as trapped victims with powerless status derive d ≈ 0.95 (maximum target of extraction). Harm reduction service providers as institutional beneficiaries with arbitrage exit derive d ≈ 0.15 (low because they benefit from the constraint). Public health authority similarly derives d ≈ 0.10 (beneficiary with institutional power and exit flexibility). Law enforcement derives d ≈ 0.60 (powerful agent with mobile exit but bearing extraction through jurisdiction loss). Abstinence-based treatment derives d ≈ 0.70 (institutional actor experiencing degradation of core premise, constrained exit). Drug policy reform coalition derives d ≈ 0.40 (organized constraint on current arrangement while building exit path). Analytical observer derives canonical d ≈ 0.73 (analytical position with high uncertainty). The engine computes effective extraction χ from these d values via the sigmoid f(d), scaled by scope modifier σ(national) = 1.0. The perspectival gap shows how the same constraint produces radically different classifications: Snare (trapped user), Tangled Rope (service provider, law enforcement), Rope (health authority), Piton (abstinence establishment), Scaffold (reform coalition), Mountain (analytical false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the resolution of mandatrophy through proper indexical reasoning. The apparent contradiction — is harm reduction coordination (Rope) or extraction (Snare) — dissolves when we recognize that it is both, from different positions. The active user (trapped, powerless) experiences extraction because they must accept state surveillance to access life-saving services. The health authority (institutional, arbitrage) experiences coordination because the constraint enables their primary mission (disease reduction) with measurable success. Law enforcement experiences mixed coordination-extraction (Tangled Rope) because the constraint both serves their public order interest (reduced overdose-related crime) and displaces their authority (users are in health systems, not criminal justice). No single type is incorrect; the constraint exhibits different structures from different positions. The mandatrophy is resolved by recognizing that indexical classification is not a puzzle to solve but a feature to describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surveillance_consent_boundary,
    'At what point does mandatory health contact for harm reduction become indistinguishable from criminal surveillance under different institutional framing?',
    'Comparative analysis of data sharing agreements, law enforcement access protocols, and user consent practices across jurisdictions; longitudinal tracking of whether health data has been used in criminal prosecution or deportation',
    'If data sharing is extensive and coerced: constraint reclassifies as Snare even from institutional perspective (surveillance + extraction without genuine health coordination). If data is genuinely firewalled: constraint holds Tangled Rope/Rope classification from institutional perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surveillance_consent_boundary, empirical, 'Distinguishing mandatory health surveillance from criminal control in harm reduction systems').

omega_variable(
    medication_substitution_equivalence,
    'Does medication-assisted treatment (MAT) substitute for use or simply displace use into a legalized/medicalized form, thereby relocating rather than reducing the fundamental constraint?',
    'Clinical outcome tracking (mortality, morbidity, social reintegration) comparing MAT to untreated use and abstinence; analysis of whether users experience MAT as liberation or substituted control',
    'If MAT genuinely enables exit from active use: constraint''s victim set narrows; classification shifts toward Rope/Scaffold from user perspective. If MAT relocates control: victim set persists; extractiveness increases because the substitution is coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medication_substitution_equivalence, empirical, 'Whether medication-assisted treatment substitutes for use or relocates dependence').

omega_variable(
    reading_committer_structure,
    'Does the harm reduction reading represent a genuine independent authority framework or merely a tactical modification of the prohibition reading''s core premise (state control of drug use)?',
    'Analysis of whether harm reduction authority explicitly claims the right to mandate users into health contact versus permission-based access; examination of whether users retain genuine exit options (leave the system entirely without criminal consequences) or face continued coercion under health framing',
    'If harm reduction is genuinely independent: the reading forecloses prohibition''s core premise that the state must prevent use through criminalization. If harm reduction accepts prohibition''s core (state must control use, only mechanism differs): the readings coexist on method, disagreeing only on enforcement style.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_structure, conceptual, 'Whether harm reduction represents independent authority framework or tactical modification of prohibition').

omega_variable(
    evidence_quality_taiwan_generalizability,
    'Do the 90% HIV reduction and other epidemiological successes in Taiwan''s needle exchange program represent evidence for harm reduction''s causal efficacy or selection effects (already-motivated users self-selecting into programs)?',
    'Quasi-experimental analysis using instrumental variables or natural experiments where harm reduction access was expanded unevenly across regions; comparison of HIV incidence trends before/after program implementation controlling for confounders',
    'If causal: harm reduction framework''s public health coordination function is genuine and substantial, supporting Rope classification from health authority perspective. If selection effects dominate: apparent success reflects user composition rather than program efficacy, inflating the coordination function''s perceived value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_quality_taiwan_generalizability, empirical, 'Whether Taiwan HIV reduction reflects causal efficacy or selection effects in harm reduction programs').

omega_variable(
    decriminalization_vs_legalization_logical_gap,
    'Is decriminalization (harm reduction''s stated endpoint) logically sufficient to transition to legalization, or does harm reduction framework''s acceptance of continued state surveillance create path dependency that forestalls full legalization?',
    'Historical analysis of jurisdictions that attempted decriminalization-to-legalization transitions; examination of whether harm reduction infrastructure becomes entrenched, creating institutional resistance to legalization frameworks that would displace the health services model',
    'If legalization follows logically: scaffold perspective''s sunset clause is real, and the reading influences toward legalization reading. If path dependency blocks legalization: harm reduction may be a permanent structural arrangement rather than transitional, reclassifying from Scaffold to Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decriminalization_vs_legalization_logical_gap, conceptual, 'Whether harm reduction logically enables transition to legalization or creates institutional path dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harm_reduction_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_red_tr_t0, harm_reduction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(harm_red_tr_t2, harm_reduction_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement(harm_red_tr_t4, harm_reduction_reading, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(harm_red_be_t0, harm_reduction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(harm_red_be_t2, harm_reduction_reading, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(harm_red_be_t4, harm_reduction_reading, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(harm_reduction_reading, prohibition_reading).
narrative_ontology:affects_constraint(harm_reduction_reading, legalization_reading).
narrative_ontology:affects_constraint(harm_reduction_reading, criminal_drug_enforcement).
narrative_ontology:affects_constraint(harm_reduction_reading, treatment_access_equity).

% DUAL FORMULATION NOTE:
% The harm reduction reading is one element of a constraint family structured by the substance_control_authority kernel. The prohibition reading (criminal enforcement, use prevention focus) and legalization reading (decriminalization, market access) are sibling readings of the same kernel, not separate constraints. Each reading has distinct extractiveness, distinct beneficiary/victim sets, and distinct temporal horizons. Network links indicate family membership: all three readings affect institutional positions and policy outcomes in criminal justice and health sectors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harm_reduction_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
