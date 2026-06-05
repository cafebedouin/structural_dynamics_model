% ============================================================================
% CONSTRAINT STORY: viability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viability_reading, []).

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
 *   constraint_id: viability_reading
 *   human_readable: Viability-Based Personhood Boundary (Reading: Moral Status at Fetal Viability)
 *   domain: moral_philosophy/bioethics/constitutional_law
 *
 * SUMMARY:
 *   The viability reading instantiates one coherent resolution of the
 *   contested personhood boundary: moral status emerges when the fetus
 *   acquires capacity for independent survival outside the womb,
 *   approximately at 24 weeks of gestation (variable by jurisdiction and
 *   medical capacity). This reading grounds a distinct constraint—a tangled
 *   rope that coordinates medical autonomy before viability while extracting
 *   post-viability restrictions on abortion. The viability boundary has deep
 *   institutional roots in obstetric practice (traditional obstetrics
 *   distinguished fetal-maternal treatment strategies at approximately this
 *   point) and in constitutional law (Roe v. Wade's trimester framework
 *   embedded this reading). However, the reading faces deep structural
 *   pressures: viability is technologically contingent (NICU capabilities
 *   determine the threshold, shifting it forward as technology improves),
 *   jurisdictionally variable, and philosophically contested. The constraint
 *   exhibits all six types from different perspectives, revealing how a
 *   single institutional boundary can be experienced as natural law
 *   (mountain), compromise structure (scaffold), coordination mechanism
 *   (rope), degraded ritual (piton), mixed extraction-coordination (tangled
 *   rope), and pure extraction (snare) depending on the observer's structural
 *   position and temporal horizon.
 *
 * KEY AGENTS:
 *   - Viable Fetus: Primary victim (powerless/trapped) — gains legal personhood at viability threshold; subject to state protection post-viability; cannot negotiate or exit biological relationship
 *   - Pregnant Woman: Primary affected agent (moderate/constrained) — retains autonomy pre-viability; faces constraints post-viability except for health exception; experiences both coordination (medical partnership) and extraction (viability restrictions)
 *   - Medical Practice Tradition: Primary beneficiary (institutional/arbitrage) — benefits from clear bright-line rule aligned with historical obstetric distinctions; preserves autonomy-centered practice framework pre-viability
 *   - Legal Compromise Framers: Secondary agent (organized/mobile) — framers of constitutional and statutory viability frameworks; experienced constraint as temporary scaffold during moral uncertainty
 *   - Institutional Viability Definition: Tertiary actor (institutional/arbitrage) — the measurable boundary itself (22-24 weeks); persists through institutional inertia despite technological contingency
 *   - Analytical Observer: Civilizational viewer (analytical/analytical) — risks naturalizing a technologically contingent institutional boundary as a natural law of biological development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viability_reading, 0.58).
domain_priors:suppression_score(viability_reading, 0.65).
domain_priors:theater_ratio(viability_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(viability_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(viability_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viability_reading, tangled_rope).
narrative_ontology:human_readable(viability_reading, "Viability-Based Personhood Boundary (Reading: Moral Status at Fetal Viability)").
narrative_ontology:topic_domain(viability_reading, "moral_philosophy/bioethics/constitutional_law").

domain_priors:requires_active_enforcement(viability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(viability_reading, fixed_text).
narrative_ontology:cs_authority_grounding(viability_reading, distributed).
narrative_ontology:cs_kernel_id(viability_reading, personhood_boundary).
narrative_ontology:cs_reading_relation(viability_reading, conception_reading, coexists_with).
narrative_ontology:cs_reading_relation(viability_reading, birth_reading, coexists_with).
narrative_ontology:cs_axiom(viability_reading, foundational, fetal_capacity_for_independence_threshold).
narrative_ontology:cs_axiom_status(fetal_capacity_for_independence_threshold, holdable).
narrative_ontology:cs_axiom_grounding(viability_reading, fetal_capacity_for_independence_threshold, empirically_contingent).
narrative_ontology:cs_axiom(viability_reading, foundational, moral_status_discontinuous_at_viability).
narrative_ontology:cs_axiom_status(moral_status_discontinuous_at_viability, holdable).
narrative_ontology:cs_axiom_grounding(viability_reading, moral_status_discontinuous_at_viability, deontological).
narrative_ontology:cs_reference_frame(viability_reading, medical_autonomy_bright_line).
narrative_ontology:cs_drift_state(viability_reading, contemporary_nicu_advancement_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viability_reading, pregnant_women).
narrative_ontology:constraint_beneficiary(viability_reading, medical_autonomy_tradition).
narrative_ontology:constraint_victim(viability_reading, viable_fetus).
narrative_ontology:constraint_victim(viability_reading, moral_status_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VIABLE FETUS (SNARE) — Once viability is achieved (≈24 weeks), this reading grants moral status that constrains the pregnant woman's autonomy. The viable fetus cannot exit the biological relationship and has no alternative. The constraint operates asymmetrically: the fetus bears the full cost of continued pregnancy if viability is reached, with minimal ability to negotiate or escape. This is pure extraction from the fetus's structural position — a transition from potential person to person with enforceable claims.
constraint_indexing:constraint_classification(viability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PREGNANT WOMAN (TANGLED ROPE) — Before viability, this reading preserves autonomy (coordination: woman coordinates her body, pregnancy, and medical decisions). After viability, the constraint imposes restrictions on abortion while preserving health exception routes (constrained exit). The pregnant woman experiences both coordination benefits (medical partnership for viable pregnancy support) and extraction (viability line restricts later-term abortion absent health exception). Asymmetric: enforcement mechanism protects the viable fetus's interests at the cost of the woman's medical autonomy.
constraint_indexing:constraint_classification(viability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL PRACTICE TRADITION (ROPE) — This reading preserves medical autonomy norms by locating the moral boundary at viability, which aligns with traditional distinctions in obstetrics (treatment as two patients before viability, two distinct patients after). The medical tradition coordinates around a bright-line rule. The tradition benefits from this reading because it respects long-standing practice (informed consent, woman as primary decision-maker pre-viability) while providing a principled break-point for post-viability restrictions. This is experienced as coordination, not extraction, within the medical context.
constraint_indexing:constraint_classification(viability_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL COMPROMISE FRAMERS (SCAFFOLD) — Viability reading functions as a temporary compromise structure in constitutional law (Roe v. Wade trimester framework was an instantiation of this reading). Framers experienced this as a scaffold: a coordination mechanism with a designed sunset — not permanent, but functional during a period of moral uncertainty and contested personhood definitions. The scaffold theater is low (≈0.40) because the viability boundary serves a genuine adjudicative function: it provides clarity on what the law permits before the larger personhood question is resolved in public discourse. However, the constraint is inherently unstable because the reading's foundational empirical claim (viability as a meaningful moral boundary) is continuously contested.
constraint_indexing:constraint_classification(viability_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL VIABILITY DEFINITION (PITON) — From a civilizational scale, the institutional definition of viability (22-24 weeks in different jurisdictions) has become substantially performative and inert. Viability is not a stable biological property — it depends on available technology (NICU capabilities, interventions), maternal health status, and varies by jurisdiction. As a institutional measurement, it persists as a bright-line legal rule (theater ratio high: ≈0.65 at this scale) despite lacking the biological stability the reading presumes. The definition continues because institutional actors have invested in the viability framework, not because it provides the principled boundary the reading advertises.
constraint_indexing:constraint_classification(viability_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (FALSE SUMMIT CANDIDATE) — This reading risks presenting viability as a natural boundary in biological development — as if the capacity for independent survival is an objective, unchanging feature of human development that grounds moral status by natural law. However, viability is technologically contingent (it shifts as NICU capabilities improve), jurisdictionally variable, and depends on resource availability. The engine will flag this perspective as a false summit: the 'natural' boundary naturalizes a reading that is actually an institutional convention grounded in beneficiary interests (the medical autonomy tradition benefits from this boundary; the reproductive autonomy tradition benefits from earlier boundaries).
constraint_indexing:constraint_classification(viability_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viability_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viability_reading, TR),
    TR >= 0.70.

:- end_tests(viability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The viability reading imposes genuine constraints on medical autonomy post-viability (health exception exists but requires judicial approval or balancing in many jurisdictions, creating friction and suppression of late-term abortion access). Before viability, extractiveness is lower (pregnant woman retains broad autonomy). The aggregate reflects that the reading creates asymmetric restrictions: the viable fetus gains protection, the pregnant woman gains restrictions. The value reflects moderate extraction because the reading embeds genuine coordination (medical treatment of viable pregnancy) alongside the extraction (viability restrictions). Suppression (0.65): High. Post-viability abortion faces substantial legal and medical barriers: many jurisdictions prohibit it entirely absent narrow health exceptions; hospital refusals are common; access is severely restricted. Pre-viability abortion faces lower suppression in permissive jurisdictions but higher suppression in restrictive ones. The aggregate reflects global variability in enforcement. Theater ratio (0.45): Moderate-low. The viability boundary serves a genuine adjudicative function (provides clarity on legal permissibility at different pregnancy stages) but embeds substantial performative content because the boundary is technologically contingent and medically ambiguous (viability varies by individual fetus, maternal health, available NICU level, jurisdiction). At institutional scale, the theater rises to 0.65 (piton perspective) because the definition becomes rigid despite lacking biological stability.
 *
 * PERSPECTIVAL GAP:
 *   The viable fetus and pregnant woman occupy opposite structural positions relative to this constraint: the viable fetus enters the victim set at viability threshold, gaining legal protection but no exit option (snare perspective); the pregnant woman transitions from broad autonomy (pre-viability) to constrained autonomy (post-viability), experiencing tangled rope. The medical tradition perceives rope (coordination around a bright-line rule that aligns with practice). The legal framers perceived scaffold (temporary compromise during moral uncertainty). The institutional viability definition itself becomes piton at civilizational scale (a persistent boundary that has lost biological grounding and persists through inertia). The analytical observer risks mountain classification (naturalizing the boundary as inherent to human development), which the engine will flag as a false summit because identifiable beneficiaries (medical autonomy tradition, certain reproductive-restriction movements) benefit from maintaining this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Pre-viability: pregnant women are primary beneficiaries with arbitrage-level exit options (can continue or terminate; can move jurisdictions; can access alternatives); d ≈ 0.10-0.20, producing negative or very low effective extraction χ. Viable fetus is victim with no exit (trapped); d ≈ 0.95, producing maximum experienced extraction. Medical tradition is beneficiary (institutional/arbitrage); d ≈ 0.05. Post-viability: pregnant woman becomes secondary victim (constrained exit — health exception exists but with high friction); d ≈ 0.65-0.75. Viable fetus remains victim (trapped); d ≈ 0.95. The reading's extraction profile is asymmetric: who benefits and who bears costs depends critically on whether viability has been reached, creating a discontinuous jump in directionality at the threshold.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_technological_contingency,
    'Is viability a biological fact or a technologically contingent institutional boundary?',
    'Historical analysis of viability threshold across decades; correlation with NICU technology development and resource availability; cross-jurisdictional comparison of viability definitions; examination of whether improved technology changes what we classify as viable independent life',
    'If technologically contingent: viability reading is not grounded in a natural boundary but in current institutional capacity — the boundary shifts as technology changes, making the reading''s foundational premise unstable. If stable biological property: the reading''s natural-law framing is justified. This shifts the classification from piton (contingent institution) to mountain (natural boundary) or vice versa.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(viability_technological_contingency, empirical, 'Whether viability is natural fact or technological contingency').

omega_variable(
    independent_survival_definition,
    'What constitutes ''independent survival outside the womb'' — complete autonomy, or merely survival with medical intervention at whatever level is available?',
    'Definitional analysis across bioethics literature; comparison with how ''viability'' is operationalized in medical practice (does NICU care count as independent? Does brief survival outside womb with immediate resuscitation count?); examination of whether any organism can survive truly independently without some form of care structure',
    'If ''independent'' means without ANY external support: few organisms meet this threshold, and viability becomes philosophically incoherent. If ''independent'' means with available medical care: viability is defined by institutional resources, not biology. This determines whether the reading is grounded in biological fact or institutional practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_survival_definition, conceptual, 'Meaning of ''independent survival'' in the viability definition').

omega_variable(
    moral_status_emergence_discontinuity,
    'Is moral status binary (emerges suddenly at viability) or continuous (gradually increases from conception or fertilization)?',
    'Philosophical argument structure analysis; examination of whether a discontinuous boundary can be coherently grounded in gradualist biological facts; comparison with competing readings'' treatment of this question; analysis of what makes a boundary ''real'' vs. pragmatically chosen',
    'If moral status is continuous: viability reading''s sharp boundary is arbitrary — drawn for institutional convenience, not grounded in moral reality. If moral status is binary: viability reading must demonstrate why viability is THE transition point, not conception or birth. This determines whether the reading is coherent or naturalizing an institutional choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_emergence_discontinuity, conceptual, 'Discontinuity vs. continuity of moral status emergence').

omega_variable(
    kernel_reading_contested_authority,
    'Which authority (medical, legal, philosophical, theological) adjudicates what counts as personhood for purposes of this constraint?',
    'Examination of which authority is grounding the viability reading in different contexts; analysis of conflict between authorities (medical viability definitions differ from legal ones; philosophical arguments differ from theological ones); determination of whether a single authority can settle the question or whether the constraint necessarily embeds multiple competing authorities',
    'If medical authority is primary: viability reading drifts as technology changes. If legal authority is primary: viability becomes a posited convention. If philosophical authority is primary: the reading must defend why viability is the morally relevant boundary. Different authority groundings produce different readings of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contested_authority, conceptual, 'Authority grounding for personhood boundary in this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viability_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(viab_tr_t0, viability_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(viab_tr_t3, viability_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(viab_tr_t6, viability_reading, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(viab_be_t0, viability_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(viab_be_t3, viability_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(viab_be_t6, viability_reading, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viability_reading, identity_coordination).
narrative_ontology:affects_constraint(viability_reading, conception_reading).
narrative_ontology:affects_constraint(viability_reading, birth_reading).
narrative_ontology:affects_constraint(viability_reading, abortion_access_restriction).
narrative_ontology:affects_constraint(viability_reading, maternal_health_exception).

% DUAL FORMULATION NOTE:
% The viability reading is one of three constraint stories decomposing the contested personhood boundary kernel. Each reading has distinct ε values reflecting different structural claims about where moral status emerges. The viability reading (ε=0.58, tangled rope) is downstream of the conception reading (ε ≈ 0.42, likely scaffold or rope) and the birth reading (ε ≈ 0.65, likely snare or tangled rope). The readings coexist in public discourse and different jurisdictions; none forecloses the others at the framework level, though individual parties will reject alternative readings. Link stories via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
