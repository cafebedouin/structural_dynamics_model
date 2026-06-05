% ============================================================================
% CONSTRAINT STORY: personhood_boundary__conception_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__conception_reading, []).

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
 *   constraint_id: personhood_boundary__conception_reading
 *   human_readable: Personhood Boundary: Conception Reading (Genetic Uniqueness = Full Moral Status)
 *   domain: normative_ethics/bioethics/political_philosophy
 *
 * SUMMARY:
 *   The conception reading of the personhood boundary claims that genetic
 *   uniqueness — the presence of a distinct genome at fertilization — is
 *   necessary and sufficient for full moral personhood. Under this reading,
 *   the fetus is a person from conception; abortion is homicide; maternal
 *   reproductive autonomy is subordinated to fetal personhood claims across
 *   the full term of pregnancy. This is ONE READING of a contested kernel
 *   (the personhood boundary itself). The sibling readings (viability
 *   reading: personhood begins when consciousness/viability is possible;
 *   birth reading: personhood begins at delivery) are alternative readings of
 *   the same kernel — they accept the kernel's authority but interpret it
 *   differently. The conception reading is structurally distinctive in that
 *   it maximizes the scope of fetal personhood and minimizes the scope of
 *   reproductive autonomy. The constraint exhibits a snare structure:
 *   pregnant persons face maximum suppression (legal prohibition, moral
 *   coercion, no alternatives); the constraint is actively enforced through
 *   law and social pressure; beneficiaries (institutional authority,
 *   antiabortion advocates) experience the constraint as coordination of
 *   moral doctrine. The theater ratio is low (0.35) because the conception
 *   reading does not rely on performative ritual — it makes direct claims
 *   about metaphysical fact (genetic uniqueness entails personhood). The
 *   suppression has increased over the 30-year interval as the reading has
 *   been institutionalized in law (successive restrictions on abortion
 *   access, mandatory waiting periods, fetal personhood statutes), suggesting
 *   enforcement ratchet dynamics. The extractiveness trajectory shows
 *   accumulation: the constraint began as doctrinal assertion (ε≈0.55) and
 *   has hardened into institutional policy (ε≈0.68), extracting increasing
 *   compliance from pregnant persons as legal enforcement intensifies.
 *
 * KEY AGENTS:
 *   - Pregnant Persons: Primary victim (powerless/trapped) — lose bodily autonomy, medical decision-making authority, and life trajectory control upon conception
 *   - Reproductive Autonomy (Collective Good): Primary victim (powerless/identity_locked) — the capacity for autonomous reproduction is conceptually prerequisite for personhood; the constraint denies this autonomy to those asserting their personhood through reproductive choice
 *   - Antiabortion Institutional Authority: Primary beneficiary (institutional/arbitrage) — religious and conservative institutions benefit through doctrinal consolidation, institutional authority over reproduction, and public policy alignment
 *   - Medical Professionals: Secondary actor (moderate/constrained) — experience mixed coordination (need a framework for fetal moral status) and extraction (reduced clinical autonomy, licensing constraints, conscience conflicts)
 *   - Reproductive Justice Coalition: Organized counter-actor (organized/mobile) — build alternative frameworks and policy pathways with sunset logic (medical technology dissolves biological dependency)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the conception boundary as an inevitable metaphysical truth rather than a constructed institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__conception_reading, 0.68).
domain_priors:suppression_score(personhood_boundary__conception_reading, 0.72).
domain_priors:theater_ratio(personhood_boundary__conception_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__conception_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__conception_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__conception_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__conception_reading, snare).
narrative_ontology:human_readable(personhood_boundary__conception_reading, "Personhood Boundary: Conception Reading (Genetic Uniqueness = Full Moral Status)").
narrative_ontology:topic_domain(personhood_boundary__conception_reading, "normative_ethics/bioethics/political_philosophy").

domain_priors:requires_active_enforcement(personhood_boundary__conception_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__conception_reading, '25f41963-941b-47ce-9d43-50c7fd3cf4a6').
narrative_ontology:cs_kernel_codification('25f41963-941b-47ce-9d43-50c7fd3cf4a6', formalized).
narrative_ontology:cs_authority_grounding('25f41963-941b-47ce-9d43-50c7fd3cf4a6', lineage).
narrative_ontology:cs_interpretation_layer_present('25f41963-941b-47ce-9d43-50c7fd3cf4a6').
narrative_ontology:cs_reading_relation('25f41963-941b-47ce-9d43-50c7fd3cf4a6', personhood_boundary__viability_reading, coexists_with).
narrative_ontology:cs_reading_relation('25f41963-941b-47ce-9d43-50c7fd3cf4a6', personhood_boundary__birth_reading, coexists_with).
narrative_ontology:cs_axiom('25f41963-941b-47ce-9d43-50c7fd3cf4a6', foundational, genetic_uniqueness_sufficient_personhood).
narrative_ontology:cs_axiom_status(genetic_uniqueness_sufficient_personhood, holdable).
narrative_ontology:cs_axiom_grounding('25f41963-941b-47ce-9d43-50c7fd3cf4a6', genetic_uniqueness_sufficient_personhood, deontological).
narrative_ontology:cs_axiom('25f41963-941b-47ce-9d43-50c7fd3cf4a6', foundational, fetal_interests_override_reproductive_autonomy).
narrative_ontology:cs_axiom_status(fetal_interests_override_reproductive_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('25f41963-941b-47ce-9d43-50c7fd3cf4a6', fetal_interests_override_reproductive_autonomy, deontological).
narrative_ontology:cs_reference_frame('25f41963-941b-47ce-9d43-50c7fd3cf4a6', conception_as_personhood_origin).
narrative_ontology:cs_drift_state('25f41963-941b-47ce-9d43-50c7fd3cf4a6', contemporary_secular_democratic_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('25f41963-941b-47ce-9d43-50c7fd3cf4a6', '').
narrative_ontology:cs_kernel_id(personhood_boundary__conception_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__conception_reading, fetal_moral_status_defenders).
narrative_ontology:constraint_beneficiary(personhood_boundary__conception_reading, antiabortion_institutional_authority).
narrative_ontology:constraint_victim(personhood_boundary__conception_reading, pregnant_persons).
narrative_ontology:constraint_victim(personhood_boundary__conception_reading, reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (SNARE) — Experiences maximum extraction. Once conception establishes personhood, the pregnant person's bodily autonomy, medical decision-making, and life trajectory are structurally subordinated to fetal personhood claims. Exit options are eliminated: continuing pregnancy is legally and socially mandated; medical exceptions are narrow and often contested; social/economic support for continuation is minimal. No alternatives, no exit, maximum suppression through legal prohibition and moral coercion.
constraint_indexing:constraint_classification(personhood_boundary__conception_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REPRODUCTIVE AUTONOMY AS COLLECTIVE GOOD (SNARE with identity_locked) — The capacity for autonomous reproduction is a prerequisite for personhood itself in social contract theory. When conception-based personhood restricts reproductive autonomy for pregnant persons, it creates a paradox: personhood is denied to those asserting personhood through reproductive choice. The identity lock is cognitive — the constraint prevents those it affects from exercising the autonomy that grounds their own personhood claims. This agent is collective and has no material exit, only identity dissolution.
constraint_indexing:constraint_classification(personhood_boundary__conception_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTIABORTION INSTITUTIONAL AUTHORITY (ROPE) — Religious and conservative institutional actors benefit from the conception reading through doctrinal coherence, institutional authority consolidation, and public policy alignment. They experience the constraint as coordination: enforcing a single moral framework across their constituency. The institutional actor has exit options (arbitrage) — they can shift focus, reinterpret doctrine, or concede policy domains. Net beneficiary with agency.
constraint_indexing:constraint_classification(personhood_boundary__conception_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL PROFESSIONALS (TANGLED ROPE) — Physicians face a genuine coordination problem: developing protocols for pregnancy care, prenatal testing, and maternal health requires some framework for fetal moral status. The conception reading provides that framework. However, it also creates extraction: physicians are constrained in clinical judgment, forced to weigh fetal interests against maternal health in ways that reduce their professional autonomy. They have constrained exit (can practice elsewhere, can specialize outside reproductive medicine) but face licensing and conscience-clause constraints.
constraint_indexing:constraint_classification(personhood_boundary__conception_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPRODUCTIVE JUSTICE COALITION (SCAFFOLD) — Organized advocates for reproductive autonomy see the conception reading as a temporary institutional arrangement with a sunset: as medical technology (long-acting contraception, in vitro development, artificial gestation) advances, the biological dependency of early pregnancy dissolves, and fetal moral status can be reconsidered without restricting pregnant person autonomy. This coalition has agency and exit paths (migration, institutional building, alternative frameworks). Theater is low because the coalition's strategies are direct action and policy advocacy, not performance.
constraint_indexing:constraint_classification(personhood_boundary__conception_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some boundaries of personhood are immutable: genetic uniqueness is an objective biological fact, not a construction; the fertilized ovum is a distinct organism with its own DNA; these facts compel moral consideration as a law of nature, not a contingent institutional choice. However, this perspective risks false summitry: the claim that genetic uniqueness ENTAILS personhood is a normative inference, not a biological law. The boundary is contestable, and beneficiary alignment reveals the reading as constructed.
constraint_indexing:constraint_classification(personhood_boundary__conception_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__conception_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personhood_boundary__conception_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personhood_boundary__conception_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__conception_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__conception_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, in the snare range. The conception reading grants full moral personhood to entities that have zero agency, zero consciousness, zero capacity for interests or preferences. This personhood is asserted against the explicit will and bodily autonomy of the pregnant person. The extractiveness is not moderately high (which would reflect a genuine coordination problem with some benefit to both parties); it is severely asymmetric. The pregnant person bears all costs (bodily burden, medical risk, life trajectory constraint, economic dependency); the fetus is described as a beneficiary but has no agency to claim or exercise benefits. The institutional beneficiaries (antiabortion advocates) extract policy legitimacy and moral authority. The measurement trajectory (0.55→0.68) reflects hardening: the reading began as doctrinal assertion and has become embedded in statute law, enforcement mechanisms (clinic regulations, mandatory waiting periods, fetal personhood definitions), making extraction systematic rather than merely rhetorical. Suppression (0.72): Very high. Pregnant persons face near-total suppression of alternatives: legal prohibition on abortion in most jurisdictions; social/religious coercion; economic dependency created by pregnancy; medical gatekeeping. The suppression is not mild (some exceptions for maternal life threat) but the exceptions are narrow and often unenforced. The measurement trajectory (0.60→0.72) shows suppression intensification through legal enforcement. Theater ratio (0.35): Low. The conception reading does not rely on performative ritual; it makes direct metaphysical claims (genetic uniqueness entails personhood). Unlike some constraints that maintain themselves through theater, the conception reading's institutional force comes from legal enforcement and moral doctrine. The slight increase (0.25→0.35) reflects increasing performative elements as the reading becomes contested — legislative testimony, philosophical debate, institutional rhetoric — but the core claim remains direct.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the institutional beneficiary and the pregnant person victim is maximum. The beneficiary experiences the constraint as coordination (establishing a moral framework); the victim experiences it as extraction (subordination of autonomy). The beneficiary has exit options (arbitrage: can shift focus or reinterpret doctrine); the victim has none (trapped). From the analytical perspective, the constraint risks appearing as a mountain (an immutable truth about personhood rooted in genetic facts), but the structural data reveals it as constructed: beneficiaries exist, institutional authority aligns with the reading, alternatives (viability and birth readings) are live in other jurisdictions. The mountain perspective is a false summit. The scaffold perspective (Reproductive Justice Coalition) sees a temporal boundary that the conception reading does not: as medical technology (artificial gestation, extended in vitro development) advances, the biological dependency that justifies the reading dissolves, and the constraint's legitimacy becomes contingent rather than necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to this specific constraint. Pregnant persons as victims with zero exit options occupy d≈0.95 (maximum extraction target), producing f(d)≈1.42. The reproductive autonomy collective is similarly trapped, with d≈0.95. Institutional beneficiaries with arbitrage exit options occupy d≈0.05 (full beneficiary), producing f(d)≈-0.12 (negative effective extraction — they benefit). Medical professionals with constrained exit occupy d≈0.55 (mixed), producing f(d)≈0.75. The Reproductive Justice Coalition with mobile exit options occupies d≈0.35 (moderate beneficiary capacity), producing f(d)≈0.40. The analytical observer occupies d≈0.72 (typical for the analytical perspective given universal scope and the absence of clear beneficiary/victim structure at civilizational scale), producing f(d)≈1.15. The scope modifier σ(S) for national scope is 1.0; for global scope (coalition perspective) is 1.2. This directionality spread — from d=0.05 (institutional beneficiary) to d=0.95 (pregnant person victim) — is the maximum possible and explains why the snare classification is unambiguous from the victim perspective: χ = ε × f(d) × σ(S) = 0.68 × 1.42 × 1.0 ≈ 0.97, well into snare territory (χ≥0.66). The institutional beneficiary experiences χ = 0.68 × (-0.12) × 1.0 ≈ -0.08, clearly negative (rope/coordination). The perspectival gap is structural.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via the kernel reading framework. The mandatrophy ('Is this pure extraction or genuine coordination?') is resolved by recognizing that this is ONE READING of a contested kernel. The conception reading classifies as SNARE because it grants personhood to entities with zero agency and zero consciousness while systematically extracting autonomy from those who can exercise agency. This is not a mislabeling of coordination as extraction (mandatrophy error type 1) because the constraint has no genuine coordination function — pregnant persons are not agents cooperating to solve a collective problem; they are targets of subordination. The reading also does not mislabel extraction as coordination (mandatrophy error type 2) — it is honest about the outcome: fetuses are declared persons, pregnant persons are denied autonomy. The resolution is that mandatrophy does not apply to readings of contested kernels. The question 'Is this reading coordinate or extractive?' is answered by the reading's own foundational axioms: if genetic uniqueness entails personhood, then the reading is not extractive — it is deontologically principled (extraction is illicit according to deontological axioms, but the reading is merely applying principle). If genetic uniqueness does NOT entail personhood (the genetic_uniqueness_moral_sufficiency omega), then the reading is extractive in that it imposes a personhood boundary without sufficient justification. The mandatrophy resolution locates the real uncertainty in the omega variables, not in the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_uniqueness_moral_sufficiency,
    'Does genetic uniqueness (presence of distinct DNA) logically entail full moral personhood, or is genetic uniqueness a necessary but insufficient condition for personhood?',
    'Philosophical analysis of the conceptual relationship between biological distinctness and moral status. Comparison with other genetic-uniqueness entities (somatic cell with mutation, chimeric tissue, cloned embryo) to test whether genetic uniqueness alone predicts the reading''s moral conclusions. Examination of whether the reading can coherently grant genetic uniqueness without granting personhood (e.g., in cases of genetic abnormality incompatible with consciousness).',
    'If genetic uniqueness entails personhood: conception reading is deontologically grounded and forecloses the viability and birth readings. If genetic uniqueness is insufficient: conception reading requires additional premises (ensoulment, species membership, potential for consciousness) whose status is contested, reclassifying the constraint as tangled_rope or piton depending on how those additional premises are grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genetic_uniqueness_moral_sufficiency, conceptual, 'Whether genetic uniqueness logically entails full moral personhood').

omega_variable(
    fetal_consciousness_and_moral_status,
    'Does the conception reading require consciousness/sentience for personhood, or is consciousness irrelevant to personhood under this reading?',
    'Examination of the reading''s foundational axioms and how they relate to empirical neurodevelopment. If the reading grants consciousness as relevant, review neurobiological evidence of fetal consciousness emergence (22-32 weeks). If consciousness is irrelevant, identify what grounds personhood if not consciousness — then examine whether that grounds apply equally to genetic material outside the body (gametes, induced pluripotent stem cells).',
    'If consciousness is required: the conception reading is empirically falsifiable and loses normative force if consciousness emerges later than conception. If consciousness is irrelevant: the reading must explain why consciousness-independent personhood is granted to conceived entities but not to other genetically unique entities (somatic cells, gametes, organoids) — this demand for consistency may force the reading to declare organoids/gametes as persons (reductio ad absurdum) or to admit consciousness is implicit in the grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fetal_consciousness_and_moral_status, empirical, 'Role of consciousness or sentience in the conception reading''s personhood claim').

omega_variable(
    ontological_status_of_potential,
    'Does the conception reading ground personhood in potential for consciousness and autonomy, or in actual present properties?',
    'Close reading of the axioms to determine whether ''genetic uniqueness sufficient for full moral status'' rests on potential (this entity could become a person) or on present properties (this entity IS a person now). Comparison with potential-based arguments for other entities (human organoids, genetically human but consciousness-impossible entities, brain-dead humans with intact DNA). Test whether potential-based grounding licenses granting personhood to other potential-persons (stored gametes in long-term cryopreservation, cloning templates).',
    'If grounded in potential: the reading is temporally fragile — early zygotes have less differentiated potential than blastocysts, which have less potential than fetuses. The reading must explain where on the potential gradient personhood begins and why. If grounded in present properties: the reading must identify which present properties ground personhood; if only genetic uniqueness, it triggers the genetic_uniqueness_moral_sufficiency omega; if consciousness or development-dependent properties, it loses the conception boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_status_of_potential, conceptual, 'Whether personhood under this reading grounds in potential or actual present properties').

omega_variable(
    maternal_personhood_conflict,
    'When pregnant person personhood directly conflicts with fetal personhood under this reading, which personhood takes priority and on what grounds?',
    'Examination of how the reading resolves the classical conflict cases: maternal health threat, fetal genetic abnormality incompatible with extrauterine life, maternal life-threatening condition. Review whether the reading grants fetal personhood equal or greater weight than pregnant person personhood. Identify the decision procedure the reading offers (harm principle, potentiality principle, relationship-based prioritization, etc.) and test it across hard cases.',
    'If fetal personhood is systematically prioritized: the constraint''s extractiveness for pregnant persons increases; suppression intensifies. If pregnant person personhood is sometimes prioritized: the constraint is less purely a snare; it becomes tangled_rope (mixed coordination of two personhoods with asymmetric capacity). The decision procedure''s coherence across cases determines whether the reading is principled or ad hoc.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maternal_personhood_conflict, conceptual, 'Priority rules for maternal vs fetal personhood conflict').

omega_variable(
    reading_naturalization_vs_construction,
    'Is the conception boundary a discovered natural fact (genetic uniqueness compels moral status through laws of human nature), or a constructed institutional framework (a choice to treat genetic uniqueness as sufficient for moral status)?',
    'Examination of whether the reading presents genetic uniqueness as sufficient through deontological axioms (intrinsic rights grounded in species membership) or through instrumental/consequentialist reasoning (embryonic research produces harmful outcomes). Comparison with scientific biology: does the biological fact of genetic uniqueness necessitate the moral boundary, or do scientific definitions of personhood vary independently of genetic facts (e.g., biology grants personhood to some multi-cellular organisms without individual DNA)? Review whether sibling readings make identical biological claims but reach different moral boundaries — if so, the boundary is constructed, not discovered.',
    'If discovered natural fact: the conception reading is deontologically grounded and forecloses sibling readings (forecloses relation). If constructed: the reading is a contingent institutional choice that coexists with alternatives (coexists_with relation). The FSM engine''s false_summit detector uses this omega to evaluate whether the mountain perspective is naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_naturalization_vs_construction, conceptual, 'Whether the conception boundary is discovered natural fact or constructed framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__conception_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pb_conception_tr_t0, personhood_boundary__conception_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pb_conception_tr_t15, personhood_boundary__conception_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(pb_conception_tr_t30, personhood_boundary__conception_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(pb_conception_be_t0, personhood_boundary__conception_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pb_conception_be_t15, personhood_boundary__conception_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(pb_conception_be_t30, personhood_boundary__conception_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pb_conception_su_t0, personhood_boundary__conception_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(pb_conception_su_t15, personhood_boundary__conception_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(pb_conception_su_t30, personhood_boundary__conception_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__conception_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__conception_reading, personhood_boundary__viability_reading).
narrative_ontology:affects_constraint(personhood_boundary__conception_reading, personhood_boundary__birth_reading).
narrative_ontology:affects_constraint(personhood_boundary__conception_reading, abortion_access_restriction).
narrative_ontology:affects_constraint(personhood_boundary__conception_reading, fetal_research_prohibition).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel contains three structurally distinct constraints corresponding to three different readings: conception_reading (this file, ε=0.68, Snare), viability_reading (ε≈0.45, Tangled Rope), and birth_reading (ε≈0.30, Tangled Rope or Rope). Each reading has its own beneficiary/victim structure, authority grounding, and institutional alignment. The three stories are linked not by causal dependency but by epistemic conflict: they are competing interpretations of the same kernel. The conception reading affects downstream constraints (abortion access restriction: the policy depends on treating fetuses as persons, making the conception reading's personhood claim the boundary condition). The three readings together form the constraint family; all three must be generated to represent the full structure of the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
