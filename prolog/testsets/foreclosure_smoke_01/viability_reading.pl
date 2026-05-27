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
 *   constraint_id: viability_reading
 *   human_readable: Moral Status Begins at Viability (Capacity for Independent Survival)
 *   domain: bioethics/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The viability reading of the personhood-boundary kernel holds that moral
 *   status begins when a fetus has the capacity for independent survival
 *   outside the womb, typically at approximately 24 weeks of gestation. This
 *   reading attempts to ground a constitutional and moral boundary in a
 *   biological fact (viability/capacity for independent survival) rather than
 *   in a contested normative premise about when personhood begins. Under this
 *   reading, pre-viability abortion is constitutionally protected as a
 *   pregnant person's reproductive freedom; post-viability abortion is
 *   subject to state regulation and restriction. The reading generates a
 *   tangled-rope constraint structure: genuine coordination (states gain
 *   clear regulatory authority, medical professionals gain clinical clarity)
 *   is bound together with asymmetric extraction (post-viability pregnant
 *   persons lose reproductive autonomy absolutely, regardless of health
 *   severity or life circumstances). The extractiveness has increased over 30
 *   years as periviable neonatal technology improves (shifting the empirical
 *   viability boundary earlier) while the legal boundary remains fixed at 24
 *   weeks, creating increasing tension between the nominal 'viability'
 *   justification and the actual legal rule. The theater ratio reflects that
 *   viability functions partly as a natural boundary and partly as an
 *   institutionally maintained rule — the boundary is politically sustained
 *   through repetition despite ongoing contestation and technological drift.
 *
 * KEY AGENTS:
 *   - Pregnant persons post-viability: Primary victim (powerless/trapped) — absolute loss of reproductive autonomy; bodily integrity overridden by state enforcement of fetal protection
 *   - Pregnant persons pre-viability: Secondary victim (moderate/constrained) — retain formal autonomy but face administrative burdens, waiting periods, counseling mandates, economic barriers
 *   - Viable fetuses: Beneficiary (moderate/constrained) — gain constitutional protection and state advocacy post-viability; no capacity to exercise or exit the constraint
 *   - State regulatory authority: Primary beneficiary (institutional/arbitrage) — gains clear jurisdictional boundary and enforcement power; experiences constraint as coordination mechanism
 *   - Medical professionals: Secondary beneficiary (moderate/mobile) — gain clinical clarity; experience modest extraction through documentation and legal liability
 *   - Reproductive rights advocates: Organized agent (organized/constrained) — see reading as temporary scaffold with sunset logic as periviable technology improves
 *   - Fetal-rights advocates: Organized agent (organized/constrained) — see reading as insufficient protection; push toward conception or early-development readings
 *   - Analytical observer: Observes the reading's theater — viability naturalizes what is institutionally constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viability_reading, 0.52).
domain_priors:suppression_score(viability_reading, 0.65).
domain_priors:theater_ratio(viability_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viability_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(viability_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(viability_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viability_reading, tangled_rope).
narrative_ontology:human_readable(viability_reading, "Moral Status Begins at Viability (Capacity for Independent Survival)").
narrative_ontology:topic_domain(viability_reading, "bioethics/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(viability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(viability_reading, '64c93817-1335-4288-9697-4e3d0e58a640').
narrative_ontology:cs_created_at('64c93817-1335-4288-9697-4e3d0e58a640', '').
narrative_ontology:cs_kernel_codification('64c93817-1335-4288-9697-4e3d0e58a640', formalized).
narrative_ontology:cs_authority_grounding('64c93817-1335-4288-9697-4e3d0e58a640', lineage).
narrative_ontology:cs_interpretation_layer_present('64c93817-1335-4288-9697-4e3d0e58a640').
narrative_ontology:cs_kernel_id(viability_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('64c93817-1335-4288-9697-4e3d0e58a640', conception_reading, coexists_with).
narrative_ontology:cs_reading_relation('64c93817-1335-4288-9697-4e3d0e58a640', birth_reading, coexists_with).
narrative_ontology:cs_axiom('64c93817-1335-4288-9697-4e3d0e58a640', foundational, viability_grounds_moral_status).
narrative_ontology:cs_axiom_status(viability_grounds_moral_status, holdable).
narrative_ontology:cs_axiom('64c93817-1335-4288-9697-4e3d0e58a640', foundational, capacity_for_independent_survival_is_relevant_to_personhood).
narrative_ontology:cs_axiom_status(capacity_for_independent_survival_is_relevant_to_personhood, holdable).
narrative_ontology:cs_reference_frame('64c93817-1335-4288-9697-4e3d0e58a640', biological_capacity_centered_personhood).
narrative_ontology:cs_drift_state('64c93817-1335-4288-9697-4e3d0e58a640', contemporary_periviable_medicine, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viability_reading, pregnant_persons_reproductive_autonomy).
narrative_ontology:constraint_beneficiary(viability_reading, state_regulatory_capacity).
narrative_ontology:constraint_victim(viability_reading, viable_fetuses).
narrative_ontology:constraint_victim(viability_reading, pregnant_persons_post_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON POST-VIABILITY (SNARE) — At viability (~24 weeks), abortion is prohibited or severely restricted to save the mother's life. The pregnant person is trapped: continuing pregnancy carries material health risks (gestational diabetes, preeclampsia, maternal mortality), bodily integrity constraints are absolute and non-negotiable, and the legal framework treats fetal life as a competing paramount interest. Exit is denied. Extraction is maximal — the state enforces fetal viability against the pregnant person's will and body.
constraint_indexing:constraint_classification(viability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE REGULATORY AUTHORITY (ROPE) — The viability standard provides a bright-line rule for regulatory enforcement. States can permit abortion up to viability without justification, and restrict post-viability with clear jurisdiction. The standard solves a coordination problem: it clarifies state authority, reduces litigation burden, and aligns regulatory capacity with biological development. States experience this as coordination, not extraction — the constraint enables governance rather than imposing costs.
constraint_indexing:constraint_classification(viability_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PREGNANT PERSON PRE-VIABILITY (TANGLED ROPE) — Pre-viability, the pregnant person retains reproductive autonomy and can access abortion. But access is not unrestricted: waiting periods, counseling requirements, parental consent, and resource barriers constrain choice. Genuine coordination exists — the person can terminate and go free — alongside asymmetric extraction: burdensome administrative and regulatory requirements, economic costs, and informational asymmetry about fetal development increase decision friction. Extraction is moderate but real.
constraint_indexing:constraint_classification(viability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: VIABLE FETUS AS DEVELOPING ENTITY (TANGLED ROPE) — The viability standard grants the fetus protectable interests at ~24 weeks. The constraint provides coordination: legal rules clarify when the state may regulate for fetal benefit. But the constraint also involves extraction: fetal interests override pregnant-person autonomy post-viability, regardless of health severity or life circumstances. The fetus is a beneficiary of the coordination rule structure but experiences no extraction; the constraint extracts from the pregnant person on behalf of the fetus.
constraint_indexing:constraint_classification(viability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL PROFESSIONALS (ROPE) — Viability provides clear clinical guidance: pre-viability, no state restriction on abortion; post-viability, restrictions apply. This is coordination for medical practice — physicians can offer care within defined parameters without legal ambiguity. Clinicians also experience extraction (malpractice risk if viability is misjudged, documentation burden, mandatory delay compliance), but the framework reduces uncertainty and enables practice. Net: coordination with moderate extraction layered atop.
constraint_indexing:constraint_classification(viability_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / VIABILITY AS NATURAL FACT (PITON) — From a civilizational perspective, viability (~24 weeks) might appear as a natural biological threshold: at that point, a fetus can survive outside the womb with medical support. This naturalizes viability as a pre-political fact, making the moral boundary seem objectively grounded. But the constraint's theater is high: viability is a moving technological target (periviable infants at 21 weeks are increasingly viable with NICU support), the mapping from biological capacity to moral status requires normative premises, and the boundary is politically sustained institutional practice, not discovered natural law. The piton classification reveals that viability is maintained as an objective boundary through repetition despite ongoing contestation.
constraint_indexing:constraint_classification(viability_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REPRODUCTIVE RIGHTS COALITION (SCAFFOLD) — Organized reproductive rights advocates see viability as a temporary compromise: a structure that permits pre-viability abortion (solving the immediate autonomy problem) while conceding the post-viability boundary (accepting state regulation). The constraint has built-in sunset logic: as NICU technology improves and periviability boundaries shift, the hard rule at 24 weeks becomes increasingly detached from actual survival capacity. The coalition sees this reading as structurally temporary — advocating for shift toward person-based frameworks that emphasize pregnant-person life and health over fetal viability per se. Extraction is accepted as part of a transition strategy.
constraint_indexing:constraint_classification(viability_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viability_reading, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. The reading permits pre-viability abortion but restricts post-viability absolutely, making it more extractive than a pure autonomy-based framework but less extractive than a conception-based framework. The extraction is concentrated post-viability: the pregnant person loses all autonomy regardless of health severity, making the effective extraction for that subset extremely high (snare-level). Pre-viability extraction is moderate (tangled rope) due to regulatory burdens and access barriers. The aggregate 0.52 reflects the weighted average across the viability threshold. Extraction has increased from 0.38 to 0.52 over 30 years as periviable neonatal care has improved (~3 weeks earlier viability is now possible than in 1995), making the hard boundary at 24 weeks increasingly misaligned with actual biological capacity, yet the legal rule has remained fixed. This drift suggests the reading is maintaining itself through institutional inertia rather than genuine tracking of biological development. Suppression (0.65): High. Multiple barriers exist to pre-viability abortion access (waiting periods, counseling mandates, parental consent, funding restrictions, clinic availability); post-viability abortion is legally prohibited in most jurisdictions except for mother's-life exceptions. Suppression includes informational asymmetry (state mandates emphasize fetal development, often with contested claims about fetal consciousness). Theater ratio (0.48): Moderate. The viability boundary has performative elements — it is presented as objective and biological while actually being maintained as a fixed rule despite technological drift. However, it also has genuine functional content: it does provide clear regulatory guidance and reduces litigation uncertainty. The theater has increased slightly from 0.35 to 0.48 as the gap between nominal 'viability' and actual technological capacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates a stark perspectival divergence. For pregnant persons post-viability, the constraint is a snare: absolute extraction with no exit. For the state, it is rope: pure coordination enabling clear governance. For pre-viability pregnant persons, it is tangled rope: some autonomy with regulatory friction. For viable fetuses, the reading bestows moral status and state protection but provides no exit option. For reproductive-rights advocates, the constraint is a temporary scaffold with a sunset built in — as periviable technology improves, the hard boundary at 24 weeks becomes increasingly unjustifiable. For fetal-rights advocates, the constraint is insufficiently protective (implicitly a snare from the fetus's perspective, or a rope only if fetal interests are what matter). The analytical observer sees the piton: viability is maintained through institutional repetition despite growing misalignment between the nominal biological criterion and actual technological development. The gap between the pregnant-person post-viability perspective (snare) and the state perspective (rope) is maximal: the state experiences the constraint as coordinating its own authority, while the pregnant person experiences the same constraint as absolute extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks each agent's structural relationship to the extraction flow. Pregnant persons post-viability are pure victims (d ≈ 0.95): they bear all the costs and have no exit. Their trapped exit option and powerless position maximizes f(d). Pregnant persons pre-viability are mixed (d ≈ 0.55): they retain formal autonomy but face barriers, and they benefit from the rule that permits them to access abortion at all. The state is a beneficiary (d ≈ 0.05): it gains regulatory clarity and enforcement authority. Viable fetuses are beneficiaries nominally (d ≈ 0.10) but have no agency to exit or exercise the protection. Medical professionals are mixed (d ≈ 0.45): they benefit from clinical clarity but face malpractice risk and regulatory burden. The viability reading's extraction mechanism is directional: it runs from post-viability pregnant persons toward fetal protection, with the state as the enforcing institution. The reading's legitimacy depends on the claim that this directional extraction is justified by the fetus's morally protectable interests — a claim that depends on foundational premises about what capacities ground moral status (contestable across the three sibling readings).
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This reading's extractiveness (0.52) exceeds 0.46, triggering mandatrophy requirements. The reading claims it resolves the mandatrophy by distinguishing coordination (clear regulatory boundaries) from extraction (loss of autonomy post-viability), and by grounding both in a biological fact (viability/capacity for independent survival). However, the resolution is contested: (1) Viability is not a pure biological fact but a normatively-laden concept that changes with technology, (2) the reading's own internal coherence is questionable — it does not uniformly apply the viability principle across hard cases (maternal health exceptions, fetal anomaly cases), and (3) the omega variables reveal irreducible uncertainties about whether fetal interests pre-viability are protectable and whether the reading is grounded in principles or is merely a practical compromise. The reading avoids mandatrophy reclassification only if we accept that the viability boundary is a genuine discovery (not an institutional construction), that fetal capacity for independent survival does ground moral status, and that post-viability extraction is justified by fetal interests. These are the axioms of the reading, and they are contested by the sibling readings. Full mandatrophy resolution would require empirical closure on whether viability is drifting technologically (omega: periviability_technological_drift) and conceptual clarity on what capacities ground moral status (omega: fetal_interests_before_viability). Neither is settled. The mandatrophy_resolved flag is `false` because the reading remains under active contestation and the theoretical commitments it requires are not universally held.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_as_biological_vs_normative_threshold,
    'Is viability a biological fact (independent of values) or a normative boundary (dependent on what capacities we decide to protect)?',
    'Examination of how viability definitions change with technology (periviable support at 21 weeks vs 24 weeks vs 28 weeks historically); analysis of whether other moral boundaries (birth, consciousness, rationality) map to biological facts or require normative specification',
    'If biological fact: viability reading is anchored in discoverable reality, limiting arbitrary boundary-drawing. If normative: the reading requires normative premises (why ''capacity for independent survival'' rather than ''capacity for consciousness'' or ''capacity for feeling pain''), making it contestable alongside sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_as_biological_vs_normative_threshold, conceptual, 'Whether viability is a biological or normative boundary').

omega_variable(
    pregnant_person_autonomy_vs_state_interest,
    'Does the pregnant person''s bodily autonomy interest outweigh the state''s interest in fetal protection post-viability?',
    'Comparative analysis of how different democracies resolve post-viability abortion cases involving severe maternal health risks, fetal anomaly incompatible with postnatal life, and socioeconomic hardship; empirical study of maternal mortality and morbidity in jurisdictions with different post-viability frameworks',
    'If autonomy outweighs: post-viability restrictions cannot be justified; viability reading becomes less sustainable. If state interest outweighs: viability reading''s asymmetry (pregnant-person loses autonomy post-viability) is justified. The reading''s fundamental extraction mechanism depends on how this is resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pregnant_person_autonomy_vs_state_interest, preference, 'Relative weight of pregnant-person autonomy vs state fetal interest').

omega_variable(
    periviability_technological_drift,
    'As neonatal intensive care improves, does the viability threshold move earlier? And if it does, does moral status move with it?',
    'Historical tracking of viability definitions and survival rates at each gestational age (21 weeks, 22 weeks, 23 weeks, 24 weeks) from 1970s to present; analysis of whether legal viability boundaries have shifted or remained fixed despite improved technology',
    'If technology pushes viability earlier and the moral boundary follows: the reading becomes increasingly restrictive of pre-viability abortion as periviability improves. If the boundary remains fixed at 24 weeks despite better survival at 22 weeks: the reading reveals that ''viability'' is not actually the operative criterion but a fixed rule that viability nominally justifies. This would undermine the reading''s claim to be tracking a biological fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(periviability_technological_drift, empirical, 'Technological drift in periviability boundaries and whether moral status threshold follows').

omega_variable(
    fetal_interests_before_viability,
    'Do pre-viable fetuses have morally protectable interests? And if so, why don''t those interests constrain pre-viability abortion?',
    'Analysis of what capacities matter morally (sentience, consciousness, relational identity) and whether pre-viable fetuses possess them; examination of whether the viability reading must deny pre-viable fetal interests or merely claim they are outweighed by pregnant-person autonomy',
    'If pre-viable fetuses have morally protectable interests the reading must deny them (making the reading more extreme) or claim interests exist but don''t constrain abortion (making the reading''s balancing act more explicit and challengeable). This determines whether the reading''s victim set should expand to include all fetuses or is limited to viable ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fetal_interests_before_viability, conceptual, 'Whether pre-viable fetuses have morally protectable interests').

omega_variable(
    reading_identity_under_contestation,
    'Is the viability reading one coherent position or a family of related positions that diverge when pressed on hard cases?',
    'Analysis of judicial, philosophical, and advocacy uses of ''viability'' in post-viability abortion cases: maternal health exceptions, fetal anomaly exceptions, socioeconomic hardship cases. Identification of whether viability advocates hold consistent positions across cases or adjust the boundary based on circumstances.',
    'If coherent: the reading is a stable constraint with determinate implications. If a family of positions: the reading lacks a stable identity and may collapse into sibling readings under pressure. High impact on whether this constraint is a robust kernel reading or an unstable compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_under_contestation, conceptual, 'Internal coherence of viability reading across hard cases').

omega_variable(
    this_reading_as_contingent_compromise,
    'Is the viability reading grounded in foundational commitments about personhood/moral status, or is it primarily a practical compromise designed to balance competing interests?',
    'Textual analysis of foundational claims in viability advocates'' works versus practical balancing language; historical analysis of whether the reading emerged from principled moral argument or from case-law compromise (Roe v. Wade''s trimester framework was a compromise, not a principled boundary)',
    'If grounded in foundational commitments: the reading is a stable kernel reading with genuine axioms. If primarily a compromise: the reading may be vulnerable to institutional drift or reclassification as a scaffold with a shorter sunset than expected. Affects the cs_structure.axioms and reading_relations assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_as_contingent_compromise, conceptual, 'Whether viability reading is grounded in principles or constructed as practical compromise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viability_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(viab_tr_t0, viability_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(viab_tr_t15, viability_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(viab_tr_t30, viability_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(viab_be_t0, viability_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(viab_be_t15, viability_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(viab_be_t30, viability_reading, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(viability_reading, conception_reading).
narrative_ontology:affects_constraint(viability_reading, birth_reading).

% DUAL FORMULATION NOTE:
% The personhood-boundary kernel contains three structurally distinct constraints, each with a different epsilon value reflecting different empirical claims and different institutional implications. The viability reading (ε=0.52, tangled_rope) claims personhood begins at biological viability. The conception reading (expected ε≈0.25, mountain or rope depending on framing) claims personhood is present from fertilization. The birth reading (expected ε≈0.45, tangled_rope or scaffold) claims moral status is fully realized only at birth. Each reading instantiates a different constraint structure with different beneficiary/victim profiles and different temporal trajectories. All three are linked through the kernel: they are competing interpretations of the same canonical question, not independent constraints. The viability reading influences both siblings: it establishes the post-viability restriction that conception-reading advocates push against (influence relation) and the pre-viability permission that birth-reading advocates push against (influence relation). Neither reading forecloses the other — both remain live positions in contemporary jurisprudence and philosophy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
