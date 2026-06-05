% ============================================================================
% CONSTRAINT STORY: prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prohibition_reading, []).

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
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: prohibition_reading
 *   human_readable: Prohibition and State Protection Through Criminalization
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading frames drug users as victims requiring state
 *   protection through criminalization and deterrence. Under this reading,
 *   users are trapped by addiction; the state's role is to constrain drug use
 *   through criminal consequences and enforcement, with the aim of protecting
 *   users from themselves. This reading dominates current U.S. drug policy,
 *   international drug control treaties, and law-enforcement institutional
 *   structure. However, the prohibition reading is one interpretation of a
 *   contested kernel — substance control authority — that generates
 *   fundamentally different constraints under harm-reduction and legalization
 *   readings. This story instantiates ONLY the prohibition reading: users as
 *   victims-requiring-protection, criminal enforcement as the protection
 *   mechanism, carceral infrastructure as the coordinating institution. The
 *   kernel context documents that alternative readings exist and that the
 *   contest between them is structural, not merely empirical.
 *
 * KEY AGENTS:
 *   - Drug users: Primary victims (powerless/trapped) — object of state protection via criminalization; experience maximum suppression and extraction
 *   - Marginalized communities: Secondary victims (moderate/constrained) — police enforcement concentrates in low-income neighborhoods; face both extraction via selective policing and genuine coordination via treatment infrastructure
 *   - Criminal enforcement apparatus: Primary beneficiary (organized/constrained) — derives budgets, job security, and institutional power from drug criminalization; extraction flows toward this institution
 *   - Public health authority: Coordinate institutional actor (institutional/arbitrage) — sees drug use as health problem requiring treatment coordination; operates orthogonally to criminal apparatus but both claim protection mandate
 *   - State policy authority: Mandating institution (institutional/constrained) — institutionally committed to prohibition as the unified framework; coordinates law enforcement, treatment, and international drug control treaties; constrained exit because abandoning prohibition requires institutional coherence
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — views prohibition as degraded institutional regime with high theater ratio sustained by inertia and institutional beneficiary interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prohibition_reading, 0.68).
domain_priors:suppression_score(prohibition_reading, 0.72).
domain_priors:theater_ratio(prohibition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(prohibition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(prohibition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prohibition_reading, snare).
narrative_ontology:human_readable(prohibition_reading, "Prohibition and State Protection Through Criminalization").
narrative_ontology:topic_domain(prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(prohibition_reading, formalized).
narrative_ontology:cs_authority_grounding(prohibition_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(prohibition_reading).
narrative_ontology:cs_kernel_id(prohibition_reading, substance_control_authority).
narrative_ontology:cs_reading_relation(prohibition_reading, harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation(prohibition_reading, legalization_reading, forecloses).
narrative_ontology:cs_axiom(prohibition_reading, foundational, drug_use_as_moral_failure_requiring_constraint).
narrative_ontology:cs_axiom_status(drug_use_as_moral_failure_requiring_constraint, holdable).
narrative_ontology:cs_axiom(prohibition_reading, foundational, criminal_deterrence_reduces_drug_use).
narrative_ontology:cs_axiom_status(criminal_deterrence_reduces_drug_use, overridden).
narrative_ontology:cs_reference_frame(prohibition_reading, total_prohibition_with_criminal_enforcement).
narrative_ontology:cs_drift_state(prohibition_reading, contemporary_harm_reduction_evidence_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prohibition_reading, criminal_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(prohibition_reading, carceral_infrastructure).
narrative_ontology:constraint_victim(prohibition_reading, drug_users).
narrative_ontology:constraint_victim(prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(prohibition_reading, public_health_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DRUG USER (SNARE) — Structurally trapped by addiction, legal prohibition, and criminal consequences. Substance use disorder is medicalized as requiring state intervention, but the intervention (criminalization) maximizes suppression through arrest, incarceration, and permanent record consequences. No exit option: treatment access is poor; criminalization blocks employment; incarceration creates recidivism cycle. Experiences maximum extraction via loss of freedom, economic opportunity, and social standing. The state's framing of users as objects of protection justifies the constraint, but the constraint's actual mechanism is coercive containment.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (TANGLED ROPE) — Police enforcement concentrates in low-income neighborhoods; drug arrests drive mass incarceration; incarceration disrupts family formation, employment, and intergenerational wealth. But communities also benefit from genuine harm reduction coordination (needle exchange, treatment infrastructure) where enforcement is lower. Mixed experience: real extraction through selective policing but also genuine coordination around health services. Exit is costly but theoretically possible through relocation or organizing; hence constrained rather than trapped.
constraint_indexing:constraint_classification(prohibition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY / TREATMENT INFRASTRUCTURE (ROPE) — From the treatment/public-health perspective, criminalization creates a genuine coordination problem: how to allocate limited treatment slots, coordinate medication-assisted treatment delivery, and manage overdose risk. Public health sees itself as solving legitimate collective action problems (overdose response, treatment access) without substantial extraction. This perspective treats the criminal apparatus as orthogonal to the actual health function. Net experience: coordination with minimal extraction because the health authority can arbitrage between treatment models and funding sources.
constraint_indexing:constraint_classification(prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT & CARCERAL APPARATUS (SNARE) — The enforcement apparatus benefits from criminalization through budget allocation, job security, and institutional power. Drug arrests generate prosecution, incarceration, and supervision revenue flows. The constraint extraction runs toward the carceral apparatus; they are the primary beneficiary. Organized status reflects institutional coordination; constrained exit reflects that abandoning drug enforcement would threaten institutional survival. From this perspective, users are the pure extraction target, and the constraint is maximally extractive (χ > 0.80) because enforcement has agency to sustain suppression.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE POLICY AUTHORITY / PROHIBITION MANDATE (TANGLED ROPE) — The state institutionally commits to prohibition as the mechanism of user 'protection.' This produces genuine coordination: the state coordinates law enforcement, treatment capacity, and international drug control agreements under a unified framework. But the coordination mechanism requires the extraction of users as the compliance target — users must be suppressed for the state's coordinating principle to function. The constraint is tangled: real coordination function (unified drug policy) plus asymmetric extraction (users bear the suppression cost). Exit for the state is constrained: abandoning prohibition would require institutional coherence across law enforcement, public health, and international treaties.
constraint_indexing:constraint_classification(prohibition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From civilizational view, the prohibition regime appears substantially degraded: harm reduction evidence contradicts deterrence theory; carceral expansion does not reduce drug use; treatment-first models show superior outcomes; yet criminalization persists through institutional inertia and path-dependent funding. Theater ratio (0.58) reflects that enforcement maintains the appearance of protection while producing harm accumulation. The constraint persists despite contradictory evidence because institutional actors benefit from its continuation and because the 'protection' framing obscures the extraction mechanism.
constraint_indexing:constraint_classification(prohibition_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prohibition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prohibition_reading, TR),
    TR >= 0.70.

:- end_tests(prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing over the 40-year interval. The constraint's core mechanism is criminalization — arrest, incarceration, permanent criminal record — which generates extraction flows toward carceral institutions while imposing suppression on users. Base extractiveness starts at 0.42 (early prohibition era with lower incarceration rates) and rises to 0.68 (mass incarceration era with accumulating criminalization). The trajectory reflects that prohibition's extraction mechanism has intensified as enforcement expanded. Suppression (0.72): Very high. Users face multiple suppression layers: addiction itself, legal prohibition, criminal consequences, employment discrimination, housing barriers, family dissolution, and exclusion from social safety nets. The suppression is structural — removing any single layer does not materially improve exit options because the constraint operates through accumulating barriers. Theater ratio (0.58): Moderate-high and increasing. Early prohibition claimed deterrence effectiveness; current regime's theater consists of: law-enforcement claims of protection rationale despite contradictory evidence, treatment-first evidence being ignored, harm reduction data being rejected, and the entire regime persisting despite clear demonstration that criminalization does not reduce drug use. The theater has increased as the gap between stated protection goal and demonstrated outcomes has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces dramatic perspectival divergence across agent positions. Users experience pure extraction (snare); the enforcement apparatus experiences institutional coordination and benefit (snare from their perspective as primary beneficiary). Public health sees coordination without extraction (rope). Marginalized communities experience mixed extraction and coordination (tangled rope). The state policy authority sees unified protection coordination (tangled rope). The analytical observer sees institutional degradation masked by protection narrative (piton). The perspectival gaps are not measurement artifacts — they reflect genuine structural differences in how agents relate to the constraint. The drug user is maximally trapped; the enforcement apparatus has agency and exit-resistance incentive; the public health authority can arbitrage between models; the analytical observer sees the entire regime as theater. These gaps are diagnostic: they reveal that what is framed as unified 'protection' is actually multiple structural positions with different experienced extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by beneficiary/victim status and exit options. Users: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ. Enforcement apparatus: beneficiary status + organized + constrained exit → d ≈ 0.20 → f(d) ≈ 0.02 → χ dampened or negative (they benefit, not extract from their own position). Marginalized communities: victim status + constrained exit → d ≈ 0.75 → f(d) ≈ 1.15 → high χ but not maximal. Public health: mixed (genuine coordination, some victim status from enforcement constraints) + arbitrage → d ≈ 0.50 → f(d) ≈ 0.65 → moderate χ. State policy authority: mixed beneficiary/victim (benefits from coordination, extracts from users) + constrained → d ≈ 0.55 → f(d) ≈ 0.75 → high χ. The directionality cascade shows that the constraint's extraction mechanism runs toward enforcement and policy authority while running away from users and marginalized communities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's extractiveness (0.68) does not trigger mandatrophy gate (> 0.70), but the structure is instructive. The prohibition reading resolves potential ambiguity between protection and extraction by explicitly embracing both: criminal constraint IS the protection mechanism; users experience both coordination (their drug use is being addressed) and extraction (they are incarcerated for the crime of needing protection). The tangled rope classification for the state policy authority captures this dual structure — genuine coordination exists (unified drug policy) alongside asymmetric extraction (users bear costs). The snare classification for users confirms that the extraction dominates their experience. The reading avoids mandatrophy by maintaining internal consistency: protection is operationalized as criminal constraint, and extraction follows from that choice. Alternative readings (harm reduction, legalization) would generate different extractions and different classifications by redefining what protection means and what users are (patients or consumers rather than criminals).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_agency_vs_victimhood,
    'Are drug users best understood as victims of the prohibition apparatus, or as agents whose choices the state legitimately constrains for protective purposes?',
    'Comparative analysis of outcomes under prohibition vs. decriminalization/harm reduction models. Longitudinal tracking of user trajectories, recidivism rates, employment outcomes, overdose mortality, and health system engagement across policy regimes.',
    'If users are primary victims: snare classification confirmed; prohibition is extractive apparatus justified by false protection narrative. If user agency and choice are the legitimate focus: prohibition may be reframed as legitimate constraint on harmful choice. The reading''s entire foundational premise depends on resolving this omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_agency_vs_victimhood, empirical, 'Fundamental question of user agency vs. state protection mandate').

omega_variable(
    deterrence_effectiveness,
    'Does criminal deterrence actually reduce drug use, or does it merely displace use and push it toward higher-risk behavior?',
    'International comparison of decriminalized jurisdictions (Portugal, Switzerland, parts of Canada) vs. prohibition jurisdictions on drug-use rates, treatment access, overdose mortality, and health outcomes. Econometric analysis of deterrence elasticity.',
    'If deterrence is effective: prohibition''s extraction is justified by its coordination benefit (reduced use). If deterrence fails: prohibition becomes pure extraction dressed in protective language. Current evidence contradicts deterrence effectiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_effectiveness, empirical, 'Whether criminal deterrence reduces drug use or merely displaces it').

omega_variable(
    reading_contestation_structure,
    'What structural features of the prohibition reading are contested by harm_reduction_reading and legalization_reading?',
    'Explicit comparison with sibling readings'' core premises. Mapping of which axioms each reading holds and which it rejects.',
    'This omega documents that THIS reading is one interpretation of a fundamentally contested kernel (substance_control_authority). The reading''s ε, beneficiary/victim structure, and classification all depend on accepting the foundational axiom that criminalization constitutes legitimate state protection. Rejecting this axiom forecloses the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_structure, conceptual, 'Structural nature of the contest between prohibition and harm-reduction/legalization readings').

omega_variable(
    incarceration_as_treatment_failure,
    'Is the high incarceration rate of drug users a policy failure (suggesting the constraint should be reclassified) or a feature of the constraint''s enforcement mechanism?',
    'Historical trajectory analysis: were incarceration rates an intended consequence of prohibition policy, or a side effect? Policy document analysis of sentencing guidelines and enforcement priorities. Comparison of incarceration outcomes under different statutory frameworks (mandatory minimums vs. discretionary sentencing).',
    'If incarceration is an unintended side effect: the constraint may be reframed as coordination with enforcement failure (tangled rope with poor execution). If incarceration is the mechanism: it confirms snare classification and reveals protection narrative as justification for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incarceration_as_treatment_failure, empirical, 'Whether incarceration is intended mechanism or policy failure').

omega_variable(
    moral_legitimacy_of_user_protection,
    'Is there a coherent moral principle under which the state legitimately constrains drug use for users'' own protection?',
    'Philosophical analysis of paternalism doctrine. Comparison with other domains (gambling, extreme sports, medical autonomy) where protection rationales succeed or fail. Analysis of whether the protection framework applies consistently or selectively.',
    'If coherent principle exists and is consistently applied: prohibition may be reframed as legitimate paternalism (weaker extraction claim). If principle is incoherent or selectively applied: protection narrative fails, and snare classification becomes definitive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_legitimacy_of_user_protection, preference, 'Moral coherence of state protection rationale for drug criminalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prohib_theater_t0, prohibition_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prohib_theater_t20, prohibition_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(prohib_theater_t40, prohibition_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(prohib_extractiveness_t0, prohibition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prohib_extractiveness_t20, prohibition_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(prohib_extractiveness_t40, prohibition_reading, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(prohibition_reading, legalization_reading).
narrative_ontology:affects_constraint(prohibition_reading, mass_incarceration_carceral_expansion).
narrative_ontology:affects_constraint(prohibition_reading, overdose_mortality_crisis).

% DUAL FORMULATION NOTE:
% The prohibition_reading is one of three structurally distinct constraints arising from the substance_control_authority kernel. harm_reduction_reading and legalization_reading are sibling constraint stories with different ε values, beneficiary/victim structures, and classifications. All three link through network.affects_constraints to represent the kernel contest. The prohibition reading upstream influences mass_incarceration_carceral_expansion (enforcement infrastructure) and downstream is influenced by overdose_mortality_crisis (empirical challenge to deterrence effectiveness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prohibition_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
