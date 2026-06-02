% ============================================================================
% CONSTRAINT STORY: survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_survival_competence_reading, []).

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
 *   constraint_id: survival_competence_reading
 *   human_readable: Catastrophe Memory as Survival Competence Transmission
 *   domain: religious_studies/cultural_anthropology/pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel:
 *   catastrophe-memory transmission in religious and cultural communities.
 *   The survival-competence reading frames catastrophe-memory as a
 *   pedagogical vehicle for preserving and transmitting adaptive knowledge —
 *   the structures and practices that allow communities to survive future
 *   catastrophes and maintain identity continuity through rupture. This
 *   reading emphasizes the seder structure (Passover meal), liturgical
 *   cycles, oral history frameworks, and ritual-embedded narratives as
 *   mechanisms for teaching 'how to tell the story,' 'what survival
 *   required,' and 'what identity means when everything else is lost.' The
 *   constraint exhibits genuine tangled-rope structure: the transmission
 *   mechanisms (rituals, narratives, pedagogical labor) provide real
 *   coordination benefits (communities without structured transmission lose
 *   survival knowledge), but they also impose extraction costs (pedagogical
 *   labor burden on tradition-keepers, conformity pressure on community
 *   members, resource devotion to ritual maintenance). The theater ratio
 *   (0.35) reflects that transmission function is explicit and largely
 *   functional — the ritual serves its stated purpose of knowledge transfer —
 *   but some ceremonial performance persists. The extractiveness (0.38)
 *   reflects moderate asymmetry between the community (beneficiary of
 *   cultural continuity) and the tradition-keepers (who bear the labor
 *   burden). Sibling readings: the mourning-practice reading frames the same
 *   constraint as primarily affective (processing grief, community solidarity
 *   through shared suffering); the hybrid-pedagogical reading attempts to
 *   integrate both functions. This story generates ONLY the
 *   survival-competence reading as a pure constraint story.
 *
 * KEY AGENTS:
 *   - Transmitting Community Elders: Primary beneficiary (institutional/arbitrage) — benefit from transmission function and social status as knowledge-keepers; can arbitrage by selecting which narratives to emphasize
 *   - Cultural Continuity Institutions: Secondary beneficiary (institutional/arbitrage) — religious organizations, cultural societies, lineage-based communities benefit from structured transmission preserving group identity
 *   - Pedagogical Labor Bearers: Primary victim (moderate/constrained) — tradition-keepers, historians, ritual specialists who maintain and teach catastrophe narratives; face sustained effort and social burden; constrained exit because abandoning transmission incurs community cost
 *   - Future Generations Without Transmission: Secondary victim (powerless/trapped) — populations that would lack adaptive survival knowledge if transmission fails; cannot exit deprivation and depend entirely on successful intergenerational transfer
 *   - Modernizing Institutions: Degraded actor (institutional/arbitrage) — religious organizations that retain ritual form while reducing transmission content; maintain performative structure without pedagogical function
 *   - Revitalization Movements: Organized agents (organized/constrained) — communities deliberately rebuilding transmission pathways outside traditional institutional channels; have agency in choosing participation but face resource constraints
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (ritual structure, elder authority, narrative canonicity) as universal laws of human identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(survival_competence_reading, 0.38).
domain_priors:suppression_score(survival_competence_reading, 0.42).
domain_priors:theater_ratio(survival_competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(survival_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(survival_competence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(survival_competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(survival_competence_reading, "Catastrophe Memory as Survival Competence Transmission").
narrative_ontology:topic_domain(survival_competence_reading, "religious_studies/cultural_anthropology/pedagogy").

domain_priors:requires_active_enforcement(survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(survival_competence_reading, 'b6378281-2491-4d0d-80be-28464bd5eb1c').
narrative_ontology:cs_created_at('b6378281-2491-4d0d-80be-28464bd5eb1c', '').
narrative_ontology:cs_kernel_codification('b6378281-2491-4d0d-80be-28464bd5eb1c', fixed_text).
narrative_ontology:cs_authority_grounding('b6378281-2491-4d0d-80be-28464bd5eb1c', lineage).
narrative_ontology:cs_interpretation_layer_present('b6378281-2491-4d0d-80be-28464bd5eb1c').
narrative_ontology:cs_kernel_id(survival_competence_reading, catastrophe_memory_transmission).
narrative_ontology:cs_reading_relation('b6378281-2491-4d0d-80be-28464bd5eb1c', mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6378281-2491-4d0d-80be-28464bd5eb1c', hybrid_pedagogical_reading, influences).
narrative_ontology:cs_axiom('b6378281-2491-4d0d-80be-28464bd5eb1c', foundational, survival_knowledge_foundational_identity).
narrative_ontology:cs_axiom_status(survival_knowledge_foundational_identity, holdable).
narrative_ontology:cs_axiom_grounding('b6378281-2491-4d0d-80be-28464bd5eb1c', survival_knowledge_foundational_identity, deontological).
narrative_ontology:cs_axiom('b6378281-2491-4d0d-80be-28464bd5eb1c', foundational, pedagogical_transmission_mechanism_required).
narrative_ontology:cs_axiom_status(pedagogical_transmission_mechanism_required, holdable).
narrative_ontology:cs_axiom_grounding('b6378281-2491-4d0d-80be-28464bd5eb1c', pedagogical_transmission_mechanism_required, empirically_contingent).
narrative_ontology:cs_reference_frame('b6378281-2491-4d0d-80be-28464bd5eb1c', structured_ritual_pedagogy).
narrative_ontology:cs_drift_state('b6378281-2491-4d0d-80be-28464bd5eb1c', contemporary_secularization_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(survival_competence_reading, transmitting_community_elders).
narrative_ontology:constraint_beneficiary(survival_competence_reading, cultural_continuity_institutions).
narrative_ontology:constraint_victim(survival_competence_reading, future_generations_knowledge_deprived).
narrative_ontology:constraint_victim(survival_competence_reading, pedagogical_labor_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Cannot exit deprivation of survival knowledge; trapped by dependence on successful intergenerational transmission. If the constraint fails or is abandoned, these agents bear the full cost of lost adaptive capacity and cultural-historical continuity. No agency, no exit, maximum extraction of knowledge burden onto transmitting elders.
constraint_indexing:constraint_classification(survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PEDAGOGICAL LABOR BEARERS (TANGLED ROPE) — Community members who maintain and transmit ritual memory structures (elders, historians, ritual specialists). Face constraints: requires sustained effort, risks social isolation if not properly integrated into community lifecycle. But also genuine coordination benefit: transmission ensures cultural survival and confers social status as knowledge-keeper. Extraction exists (disproportionate time/labor burden) alongside coordination (cultural continuity function). Constrained exit — cannot abandon transmission without community cost, but some agents can and do exit into modernizing paths.
constraint_indexing:constraint_classification(survival_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CULTURAL CONTINUITY INSTITUTIONS (ROPE) — Religious and cultural organizations benefit from preservation and transmission mechanisms (e.g., synagogues, churches, temples, lineage holders). The constraint solves a genuine collective-action problem: without structured catastrophe-memory transmission (seder rituals, liturgical cycles, oral history frameworks), cultural knowledge dissolves. Institutions experience the constraint as coordination infrastructure. They benefit from the transmission function while also bearing transmission costs. Net beneficiary through arbitrage — can exit into alternative transmission models if needed, but structured ritual provides reliable coordination at low marginal cost.
constraint_indexing:constraint_classification(survival_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERNIZING INSTITUTIONS (PITON) — Religious organizations that retain ritual structures while abandoning transmission content or pedagogical function. Rituals persist through institutional inertia (seder is performed, but the Exodus narrative transmission is reduced to ceremony). Theater ratio is high because the ritual form is maintained while the adaptive-capacity function atrophies. Institutions can arbitrage: keep ceremonial status while reducing pedagogical burden. The constraint is performative rather than functional in this perspective.
constraint_indexing:constraint_classification(survival_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REVITALIZATION MOVEMENTS (SCAFFOLD) — Organized communities deliberately reconstructing catastrophe-memory transmission outside traditional institutional channels (Holocaust education programs, language revitalization initiatives, Indigenous knowledge reclamation). See the constraint as a temporary coordination failure being solved through alternative pedagogies. Theater is low because function (knowledge transmission) is explicit and measured. Suppression is reduced because participants have agency in choosing to participate. Sunset clause: as revitalization moves from recovery to institutionalization, the constraint itself may dissolve into normalized transmission pathways.
constraint_indexing:constraint_classification(survival_competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, all human groups must transmit survival knowledge across catastrophe to maintain adaptive capacity and cultural identity. This appears as an immutable constraint: identity itself is constituted through historical narrative transmission; removing the constraint removes group continuity. The constraint feels inevitable because group identity is at stake. However, the structural data reveals this as a potential false summit: the 'inevitability' may naturalize specific institutional arrangements (ritual structure, elder authority, narrative canonicity) rather than capturing an underlying universal.
constraint_indexing:constraint_classification(survival_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(survival_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(survival_competence_reading, TR),
    TR >= 0.70.

:- end_tests(survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint provides genuine coordination benefit (preserving adaptive survival knowledge across catastrophe), but the beneficiaries (elders, cultural institutions) accrue disproportionate status and social authority during transmission, creating asymmetric extraction. The core extraction mechanism is the pedagogical labor burden: the work of maintaining, refining, and transmitting narratives falls more heavily on specialists than on the general community. This is not maximal extraction (beneficiaries and victims are interdependent; communities without tradition-keepers cannot preserve knowledge), but the asymmetry is real. Suppression (0.42): Moderate. Barriers to exit include social penalty for abandoning tradition (identity loss, community exclusion), opportunity cost of time devoted to ritual participation, and conformity pressure to accept narratives as authoritative. However, suppression is not maximal: modernizing populations regularly exit with incomplete cultural loss, revitalization movements show that transmission can be restructured, and digital archiving provides alternative knowledge-preservation. Theater ratio (0.35): Moderate-low. The transmission function is largely explicit and functional — the ritual IS designed to teach survival knowledge — but some performance persists (ceremonial elements without clear pedagogical content, canonical freezing of narratives that might otherwise adapt, ritual authority that exceeds the knowledge content). Theater has remained relatively stable because pedagogical content is integral to ritual function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits significant perspectival divergence. Elders see preservation and status (Rope). Future generations see dependence on others' labor (Snare). Pedagogical labor bearers see mixed coordination and extraction (Tangled Rope). Modernizing institutions see performative ritual without pedagogical commitment (Piton). Revitalization movements see temporary coordination problem with reconstructive exit path (Scaffold). The analytical observer risks seeing universal natural law (Mountain). The gap reflects genuine structural differences in how agents experience the transmission function: the beneficiaries experience coordination and autonomy; the constraints bearers experience labor burden and limited exit; the deprived future-agents experience total dependence. The piton classification is crucial — it reveals how institutions can maintain ritual form while abandoning pedagogical function, sustaining the constraint through inertia rather than through genuine coordination benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural relationship to the transmission function. Elders and cultural institutions are beneficiaries with arbitrage options (low d ≈ 0.15-0.20, negative χ) — they experience the constraint as advantageous coordination. Pedagogical labor bearers are constrained victims (moderate d ≈ 0.60-0.65, positive χ) — they experience extraction but have partial agency through specialization. Future generations without transmission are trapped victims (high d ≈ 0.95, maximum χ) — they depend entirely on successful transmission. Modernizing institutions arbitrage by reducing transmission burden while retaining ritual status (low d despite victim function — they have chosen the institutional path). Revitalization movements are organized actors with constrained exit (moderate d ≈ 0.55-0.60) — they have chosen to rebuild transmission despite barriers. The analytical observer occupies the universal/civilizational position and risks naturalizing (d ≈ 0.72, analytical chi) — observational distance creates vulnerability to false-summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining structural clarity about coordination function vs. extraction mechanism. Coordination function: catastrophe-memory transmission solves the genuine collective-action problem of preserving survival knowledge across cultural rupture. Extraction mechanism: the pedagogical labor burden falls disproportionately on specialists, creating asymmetric status and authority. The perspectives confirm that both are real: the rope classification (beneficiaries) and the tangled-rope classification (bearers) are not competing descriptions — they are the same constraint seen from different structural positions. The piton perspective (modernizing institutions) diagnoses a failure mode: when the transmission function decays (knowledge content becomes ceremonial), the constraint persists through inertia. The snare perspective (future generations) confirms that the constraint is real and asymmetric: dependence on successful transmission is genuine. The scaffold perspective (revitalization movements) shows that the constraint can be restructured with agency and sunset logic — alternative transmission pathways reduce dependence on institutional ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_reading,
    'Is catastrophe-memory transmission a universal requirement for any human group identity, or is it a contingent institutional arrangement that specific communities have chosen?',
    'Comparative analysis of transmission failure across historical diasporas: does identity persist without structured ritual transmission? Can identity be reconstituted through non-ritual channels? Do groups that abandon catastrophe-memory rituals experience categorical identity loss or gradual cultural drift?',
    'If universal requirement: mountain classification is correct — the constraint is inherent to human continuity. If contingent: false-summit detection fires — naturalization masks an institutional choice. Reclassifies to tangled_rope or snare depending on extraction mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_reading, conceptual, 'Whether transmission is natural law or contingent institutional choice').

omega_variable(
    pedagogical_extraction_vs_coordination,
    'Does the pedagogical labor burden (on elders, tradition-keepers) represent genuine extraction costs imposed by the community, or is it a voluntary role-specialization that carries status and meaning equivalent to the labor burden?',
    'Ethnographic analysis of tradition-keeper compensation, status, and exit options. Historical comparison of transmission burden across periods of high cultural stability vs. catastrophe/diaspora. Interview data on tradition-keepers'' experience of the role as voluntary coordinating or as imposed obligation.',
    'If extraction: suppression ≥ 0.50, tangled_rope classification holds. If voluntary specialization with adequate compensation (social status, community support, specialized authority): suppression ≤ 0.35, reclassifies to rope. Affects the constraint-type of the constraint itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pedagogical_extraction_vs_coordination, empirical, 'Whether pedagogy burden is extraction or compensated role-specialization').

omega_variable(
    transmission_mechanism_substitutability,
    'Are oral ritual transmission, written canonical texts, embodied practice, and digital/archival methods functionally equivalent as transmission mechanisms, or do they preserve different types of adaptive knowledge?',
    'Comparative knowledge-loss analysis across different transmission media: which survival competencies are preserved by each? Are there knowledge types that require oral/ritual transmission to remain adaptive? Historical cases where communities switched transmission media — did identity and adaptive capacity persist?',
    'If equivalent: constraint could be satisfied through non-extractive mechanisms (low-cost digital archiving, open-source knowledge bases), reclassifying to rope. If rituals preserve uniquely important knowledge: constraint is more stringent, suppression increases, snare classification more probable for populations without access to ritual knowledge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_mechanism_substitutability, empirical, 'Functional equivalence of different transmission mechanisms').

omega_variable(
    identity_lock_in_transmission_commitment,
    'For individual transmitters and community members, is adherence to catastrophe-memory transmission mechanisms structurally constrained (material barriers, social penalty) or identity-locked (the person cannot imagine themselves as group member without engaging in transmission)?',
    'Ethnographic differentiation: interview tradition-keepers on whether they could exit transmission if social penalties were removed (identity_locked) vs. whether exit barriers are primarily social/economic (constrained). Cohort comparison across generations of declining religious participation — do exiting members report identity dissolution or identity transformation?',
    'If identity_locked: exit_options classification shifts to identity_locked for moderate/powerful agents. Changes perspectival chi calculations. Creates diagnostic signal that binding is cognitive/identity rather than purely structural. If constrained: exit_options remain constrained; binding is external and potentially removable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_in_transmission_commitment, empirical, 'Whether transmission commitment is identity-locked or externally constrained').

omega_variable(
    competing_master_narrative_foreclosure,
    'Does the survival-competence reading''s emphasis on adaptive knowledge transmission inherently foreclose the mourning-practice reading''s focus on affective processing and grief work, or can both functions coexist in the same ritual structure?',
    'Ritual structural analysis: does seder structure accommodate both pedagogical knowledge-transfer AND affective mourning-work, or does emphasizing one suppress the other? Ethnographic data on participant experience — do people report learning survival knowledge AND processing grief, or do these feel like competing framings? Historical variation in ritual emphasis — do periods of high transmission focus show lower affective integration, or do both vary independently?',
    'If foreclosure: reading_relations entry is ''forecloses'' — the survival-competence framing rules out mourning-practice within a single institutional framework. If coexistence: reading_relations entry is ''coexists_with'' — both readings remain live positions for different communities or different participants. If influence: reading_relations entry is ''influences'' — survival-competence emphasis creates structural pressure toward knowledge-verification that changes mourning-practice''s conditions but doesn''t rule it out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_master_narrative_foreclosure, conceptual, 'Whether survival-competence reading forecloses or coexists with mourning-practice reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(survival_competence_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(surv_tr_t2, survival_competence_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(surv_tr_t4, survival_competence_reading, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, survival_competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(surv_be_t2, survival_competence_reading, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(surv_be_t4, survival_competence_reading, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(survival_competence_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(survival_competence_reading, hybrid_pedagogical_reading).

% DUAL FORMULATION NOTE:
% Catastrophe-memory transmission is one contested kernel instantiated by three distinct constraint stories: this survival-competence reading (pedagogical function, ε=0.38, Tangled Rope), the mourning-practice reading (affective function, expected ε≈0.30, Tangled Rope or Rope), and the hybrid-pedagogical reading (integrated functions, expected ε≈0.45, Tangled Rope). All three are readings of the same stabilized commitment but with different ε values because they measure different aspects of the transmission mechanism. The readings are linked via kernel structure, not by ordinary network dependency. Each story gets its own perspectives, metrics, and axiom set. The network edge indicates that this reading's interpretation of pedagogical function affects how the sibling readings frame the same rituals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
