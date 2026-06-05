% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Personhood at Conception: Developmental Potentiality Reading
 *   domain: legal_philosophy/constitutional_law/reproductive_rights
 *
 * SUMMARY:
 *   The developmental potentiality reading of the legal personhood boundary
 *   instantiates a single coherent claim: legal personhood begins at
 *   conception (the moment of fertilization and genetic uniqueness) based on
 *   the principle that any organism bearing the complete human genome—and
 *   thus the biological potential to develop into a rights-bearing human
 *   being—possesses personhood from that moment. This reading subordinates
 *   pregnant persons' bodily autonomy to fetal rights from conception onward
 *   and grants the state enforcement authority over pregnancy outcomes. The
 *   constraint exhibits the core structure of a tangled_rope: it provides
 *   genuine coordination (clarifying when personhood begins, resolving
 *   jurisdictional ambiguity about fetal protection, enabling unified legal
 *   status across development) while simultaneously extracting authority over
 *   reproductive decisions and bodily integrity from pregnant persons. The
 *   reading is one of three structurally distinct interpretations of the same
 *   kernel (legal personhood boundary). The other readings—functional
 *   capacity (personhood requires capacity for consciousness, sentience, or
 *   rational agency, placing the boundary at viability or birth) and
 *   restrictive anthropocentric (personhood requires sentient experience or
 *   moral agency, excluding early fetuses entirely)—occupy different parties'
 *   institutional commitments and generate different victim/beneficiary
 *   structures. The developmental potentiality reading generates the widest
 *   victim set (all pregnant persons from conception onward) and the
 *   strongest state enforcement authority. Extractiveness has risen over the
 *   30-year interval (0.35 → 0.68) as enforcement mechanisms have been
 *   institutionalized and state capacity to monitor pregnancy outcomes has
 *   expanded. Suppression has similarly increased (0.45 → 0.72) as
 *   enforcement moved from legal liability to criminal prosecution to
 *   surveillance infrastructure. Theater ratio has declined (0.42 → 0.35)
 *   because the functional work of enforcement has come to dominate
 *   performative discourse—the juridical claim now executes directly as
 *   policy rather than remaining abstract.
 *
 * KEY AGENTS:
 *   - Pregnant Persons: Primary victims (powerless/trapped) — bodily autonomy subordinated to fetal rights; no exit mechanism within this reading's framework; bear full extraction cost through reproductive constraint and state surveillance
 *   - Fetal Entities: Rights beneficiaries (institutional/arbitrage per state authority) — gain legal personhood, state protection, and enforcement resources; no inherent agency, entirely mediated by state enforcement machinery and advocacy constituencies
 *   - State Enforcement Authority: Institutional beneficiary (institutional/arbitrage) — gains jurisdiction, enforcement authority, and legal clarity; experiences constraint as coordination solution; can arbitrage to alternative readings
 *   - Fetal Rights Advocacy Coalition: Organized beneficiary (organized/constrained) — Right to Life movements, religious organizations, conservative legal academics; benefits from institutional grounding of their position; bears coordination costs; constrained exit through organized opposition
 *   - Healthcare Providers: Constrained secondary victims (moderate/constrained) — coordinate medical care under legal clarity, but experience extraction through mandatory reporting, criminal liability, and constrained medical judgment; can migrate to alternative jurisdictions
 *   - The Natural Law Jurisprudential Tradition: Institutional authority claimant (institutional/arbitrage) — sustains the reading through appeal to immutable human nature and metaphysical necessity; relies on institutional assertion more than empirical defense (piton signature)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as metaphysical necessity; false-summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Personhood at Conception: Developmental Potentiality Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/reproductive_rights").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '90e45c91-38bf-4a1a-9b3e-5a826b3194b0').
narrative_ontology:cs_kernel_codification('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', formalized).
narrative_ontology:cs_authority_grounding('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', extraction).
narrative_ontology:cs_interpretation_layer_present('90e45c91-38bf-4a1a-9b3e-5a826b3194b0').
narrative_ontology:cs_reading_relation('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_reading_relation('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_axiom('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', foundational, genetic_uniqueness_sufficient_for_personhood).
narrative_ontology:cs_axiom_status(genetic_uniqueness_sufficient_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', genetic_uniqueness_sufficient_for_personhood, deontological).
narrative_ontology:cs_axiom('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', foundational, state_protective_duty_to_potential_human_life).
narrative_ontology:cs_axiom_status(state_protective_duty_to_potential_human_life, holdable).
narrative_ontology:cs_axiom_grounding('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', state_protective_duty_to_potential_human_life, deontological).
narrative_ontology:cs_reference_frame('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', natural_law_personhood_principle).
narrative_ontology:cs_drift_state('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', contemporary_post_roe_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('90e45c91-38bf-4a1a-9b3e-5a826b3194b0', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_constituency).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (SNARE) — Subordinated to fetal rights from conception onward. Exit options: none at biographical time horizon within this reading's framework. The person's bodily integrity, reproductive choice, and medical decision-making authority are subordinated to state enforcement of fetal claims. No alternative jurisdiction, no opt-out mechanism. Maximum extraction with maximum suppression.
constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE ENFORCEMENT AUTHORITY (ROPE) — Gains jurisdiction and enforcement apparatus over pregnancy outcomes, medical decisions, and bodily integrity violations. Experiences the constraint as a coordination solution: clarifying at what point legal personhood begins resolves jurisdictional ambiguity and enables state protection of fetal life. Net institutional beneficiary with low-cost enforcement (built into existing medical/criminal infrastructure). Arbitrage: can exit this reading by adopting alternative threshold.
constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FETAL RIGHTS ADVOCACY COALITION (TANGLED ROPE) — Organized constituency (Right to Life movements, religious organizations, conservative legal academics) that benefits from this reading: fetal rights are codified, enforcement authority is clarified, and the coalition gains standing to participate in state enforcement. But the coalition also bears coordination costs: must organize across multiple institutions (courts, legislatures, medical boards), faces organized opposition, and sustains high mobilization costs. Coordination function (clarifying legal status) + asymmetric extraction (concentration of decision-making power in coalition hands) + active enforcement.
constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTHCARE PROVIDERS (TANGLED ROPE) — Physicians, nurses, and clinic operators coordinate patient care under legal clarity about personhood boundary, but experience extraction: mandatory reporting, liability for failing to protect 'fetal persons,' potential criminal exposure, and constrained medical judgment. Some coordination benefit (legal clarity about obligations), but suppression is high (criminal and civil liability, professional regulation). Constrained exit: cannot ethically exit medical practice, but can migrate to jurisdictions with different readings.
constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NATURAL LAW TRADITION (PITON) — This reading claims grounding in natural law and immutable human nature (personhood begins at conception as a matter of metaphysical fact, not convention). But institutional reality shows degradation: modern natural law discourse does not carry the same epistemic force it once did; the claim is sustained more through institutional assertion (church teaching, conservative legal academia) than through demonstrated logical necessity. Theater ratio is low (0.35) because the functional work is enforcement (not performative — this reading actually executes law and policy). Piton arises from the inertial weight of institutional authority outpacing the empirical defense of the boundary claim.
constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / METAPHYSICAL NECESSITY VIEW (MOUNTAIN) — The civilizational analytical perspective risks classifying this as a Mountain: the claim that personhood begins at conception is presented as a necessary truth about the nature of human life, not a contingent institutional choice. From this view, the boundary is immutable — a matter of metaphysical fact about when a distinct human organism with its own genome begins. However, structural analysis reveals this as a false summit: the claim's force depends on institutional authority (state enforcement, jurisprudential tradition, religious doctrine), not on logical necessity. The metaphysical boundary is not naturally emergent — it is actively sustained by suppression and enforcement.
constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_personhood_boundary__developmental_potentiality_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, TR),
    TR >= 0.70.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The reading extracts reproductive autonomy and bodily integrity decision-making from pregnant persons for the 280-day gestation period plus enforcement period. The extraction is not maximal (0.99) because some agents (state authority, advocacy coalitions) experience coordination benefit alongside extraction, and enforcement is resource-intensive (not frictionless extraction). The trajectory shows institutional escalation: as legal frameworks matured from abstract principle to concrete enforcement (criminalization, surveillance infrastructure, mandatory reporting), extractiveness increased. Suppression (0.72): High and rising. Barriers to exit include constitutional/statutory law (pregnant persons cannot exit pregnancy once conception occurs without violating fetal personhood claims), medical authority constraints (decisions are mediated through state-mandated frameworks), criminal liability (for actions defined as threatening fetal personhood), and surveillance infrastructure (monitoring of pregnancy outcomes). The trajectory reflects institutionalization of enforcement—as surveillance capability expanded and criminal statutes clarified, suppression increased. Theater ratio (0.35): Low and declining. This reading's functional work is enforcement, not performance. The juridical claim executes directly as policy (prosecution, surveillance, bodily integrity intervention), not as abstract principle. As enforcement has become institutionalized, performative content has diminished. The reading does not sustain itself through theater—it sustains itself through state power and legal authority. Tangled Rope classification requires: beneficiaries (fetal rights constituency, state authority) ✓; victims (pregnant persons, healthcare providers) ✓; requires_active_enforcement (true, expanding criminal and surveillance infrastructure) ✓; χ within tangled_rope range (0.40 ≤ χ ≤ 0.90, expecting χ ≈ 0.72-0.85 from d ≈ 0.85 for trapped pregnant persons).
 *
 * PERSPECTIVAL GAP:
 *   This reading generates the maximal perspectival divergence. Pregnant persons see a snare (complete bodily subordination, no exit, maximum extraction). State authority sees a rope (coordination solution clarifying personhood boundary). Fetal rights coalitions see tangled rope (coordination with enforcement costs). Healthcare providers see tangled rope (legal clarity with constrained discretion and liability). The natural law tradition sees piton (institutional assertion of metaphysical necessity, but weakening empirical defense). The analytical observer risks seeing mountain (metaphysical fact about human nature), but structural analysis reveals false summit (institutional contingency). The perspectival gap reveals that the constraint's force does not derive from natural law but from institutional power: if the reading is merely institutional, all perspectives collapse into tangled_rope or snare, and the false-summit signature fires.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent. Pregnant persons: d ≈ 0.95 (full target of extraction, no exit, body is the jurisdiction). State authority: d ≈ 0.10 (primary beneficiary, arbitrage exit, jurisdiction expanded). Fetal rights coalition: d ≈ 0.30 (beneficiary with organized constraints; constrained exit through opposition). Healthcare providers: d ≈ 0.70 (mixed — coordinate care but face extraction through liability; constrained exit through professional obligation). The natural law tradition: d ≈ 0.15 (beneficiary, institutional arbitrage). The analytical observer: d ≈ 0.72 (observer position, high f(d), seeing structure that native positions cannot). The widest gaps are between pregnant persons (d ≈ 0.95) and state authority (d ≈ 0.10), corresponding to snare and rope classifications at 0.85 difference. This asymmetry is the reading's structural signature.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED AT TANGLED_ROPE: This reading successfully instantiates a mixed coordination-extraction hybrid. The coordination function is genuine—the reading clarifies a previously ambiguous legal boundary and enables unified state protection of fetal life. The extraction is equally genuine—pregnant persons' reproductive autonomy is subordinated to fetal rights. The reading is not a disguised snare (pure extraction hiding as coordination): the state authority truly does experience coordination benefit, and the fetal rights constituency truly does benefit from legal clarification. But it is not pure rope (coordination without extraction): the coordination benefit is asymmetrically distributed, and enforcement requires suppression of pregnant persons' exit options. The tangled_rope classification holds because the constraint simultaneously solves a coordination problem (when does personhood begin?) and creates an extraction mechanism (state authority over pregnancy). The high suppression (0.72) and high extractiveness (0.68) confirm that this is not a low-overhead coordination mechanism—the state must invest heavily in enforcement to sustain the boundary. The rising extractiveness trajectory (0.35 → 0.68) reflects that the initial legal claim had low enforcement cost, but as resistance mounted and pregnant persons discovered exit mechanisms (interstate travel, medical secrecy), state enforcement escalated. The constraint's mandatrophy is resolved by recognizing that coordination and extraction are genuinely dual aspects of the same mechanism: clarifying the personhood boundary enables state protection (coordination) and enables state control of pregnancy (extraction). Neither aspect is epiphenomenal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_vs_institutional_grounding,
    'Is the personhood boundary at conception a metaphysical fact about human nature, or an institutional definition grounded in legal authority?',
    'Comparative constitutional analysis across jurisdictions with different thresholds; empirical investigation of whether consensus changed as institutional authority changed (e.g., post-Roe v. Wade adoption of viability standard in US); analysis of whether non-institutional philosophical arguments can defend the conception boundary without appeal to legal authority or religious doctrine.',
    'If metaphysical: mountain classification holds (natural law). If institutional: false-summit signature confirms; reclassify as tangled_rope with active enforcement. Affects the normative force of the boundary and the justifiability of state enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_vs_institutional_grounding, conceptual, 'Metaphysical vs. institutional grounding of conception boundary').

omega_variable(
    pregnant_person_agency_framework,
    'Are pregnant persons'' bodily autonomy rights subordinated or merely constrained by competing fetal rights under this reading?',
    'Analysis of case law applying this reading: do pregnant persons retain any residual decision-making authority (e.g., in cases of medical conflict, harm to self, severe fetal anomaly)? Can a pregnant person ever refuse pregnancy continuation? Does the reading permit any escape clause (threat to maternal life, viability limits, medical necessity)?',
    'If complete subordination: snare classification confirmed; pregnant persons are fully trapped. If constrained but not subordinated: reclassify snare perspective to constrained exit and tangled_rope (mixed coordination/extraction). Affects assessment of suppression and exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pregnant_person_agency_framework, empirical, 'Scope of pregnant persons'' retained bodily autonomy').

omega_variable(
    enforcement_mechanism_stability,
    'Can state enforcement of fetal personhood from conception be sustained without escalating into severe bodily integrity violations (forced pregnancy continuation, criminal liability for miscarriage, surveillance of pregnancy outcomes)?',
    'Comparative study of jurisdictions that have adopted conception-boundary personhood laws: measure frequency and severity of enforcement outcomes (prosecutions, forced interventions, miscarriage investigations, forced cesarean sections); assess whether enforcement escalates over time or plateaus; identify which enforcement mechanisms are actually deployed vs. theoretically available.',
    'If escalating: suppression value may underestimate actual institutional coercion; reclassify snare perspective to higher suppression. If stable: suppression (0.72) is justified. Affects assessment of whether the constraint is sustainable or self-undermining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_stability, empirical, 'Institutional stability and escalation trajectory of fetal personhood enforcement').

omega_variable(
    biological_discontinuity_at_conception,
    'Does conception represent a discontinuous change in biological status sufficient to ground a sharp legal boundary, or is personhood development a continuous process with no privileged biological threshold?',
    'Developmental biology analysis: identify all significant biological transitions in early development (fertilization, pronucleus formation, DNA replication, cell division, implantation, genetic uniqueness, sentience emergence, viability, birth). Compare their relative biological significance. Assess whether any single transition can be philosophically defended as THE threshold for personhood without circularity.',
    'If conception is discontinuous and privileged: supports the reading''s boundary claim. If development is continuous: undermines the conception boundary as arbitrary; reclassify mountain perspective as false summit. Affects the reading''s metaphysical claim and its institutional sustainability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_discontinuity_at_conception, empirical, 'Biological status of conception vs. other developmental thresholds').

omega_variable(
    competing_personhood_readings_coexistence,
    'Can this reading coexist in a pluralistic legal framework with sibling readings (functional capacity, restrictive anthropocentric), or does the claim that personhood begins at conception logically foreclose the others?',
    'Constitutional law analysis: survey jurisdictions where multiple readings are held by different parties or where institutional shifts between readings occurred without armed conflict or constitutional collapse. Assess whether plural readings are genuinely coexistent (different parties, same framework) or whether institutional dominance of one reading functionally forecloses the others despite formal coexistence.',
    'If logically forecloses: reading_relations should include ''forecloses'' edges to siblings. If coexistent: ''coexists_with'' edges. Affects the mandatrophy analysis and the reading''s institutional vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_personhood_readings_coexistence, conceptual, 'Logical compatibility of conception-boundary reading with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(persbdy_theater_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(persbdy_theater_t15, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(persbdy_theater_t30, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(persbdy_ext_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(persbdy_ext_t15, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(persbdy_ext_t30, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(persbdy_supp_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(persbdy_supp_t15, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(persbdy_supp_t30, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, reproductive_autonomy_constraint__state_enforcement).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, abortion_access_constraint__fetal_protection_doctrine).

% DUAL FORMULATION NOTE:
% The legal personhood boundary kernel admits multiple structurally distinct readings with different ε values and enforcement mechanisms. The developmental potentiality reading instantiated here (ε ≈ 0.68) is downstream of the kernel definition ('when does personhood begin?') and upstream of concrete enforcement constraints (abortion prohibition, fetal protection doctrine, reproductive surveillance). The functional capacity reading (ε ≈ 0.40) and restrictive anthropocentric reading (ε ≈ 0.15) will be instantiated as separate stories. Each reading instantiates a different constraint because the personhood boundary determines different victim sets, enforcement mechanisms, and extraction profiles. These are not the same constraint viewed from different angles—they are structurally distinct constraints with fundamentally different ε values because the observable used to evaluate them (when does personhood begin?) is constitutive of the constraint itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__developmental_potentiality_reading, powerless, 0.95).
constraint_indexing:directionality_override(legal_personhood_boundary__developmental_potentiality_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
