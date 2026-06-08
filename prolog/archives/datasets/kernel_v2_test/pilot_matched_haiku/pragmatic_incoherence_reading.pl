% ============================================================================
% CONSTRAINT STORY: pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatic_incoherence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: pragmatic_incoherence_reading
 *   human_readable: Pragmatic Incoherence in Kami-Buddha Ontology (Japanese Religious Practice)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   In medieval and early modern Japan, Buddhist and Shinto religious
 *   frameworks coexisted without unified ontological resolution.
 *   Practitioners navigated contradictory cosmologies, deity classifications,
 *   and ritual functions opportunistically — performing kami veneration and
 *   Buddhist ceremonies without resolving their incompatibility. This
 *   constraint describes the structural arrangement that maintained this
 *   incoherence: no institutional authority enforced coherence-seeking, and
 *   the absence of unified ontology became itself a coordination mechanism
 *   enabling institutional flexibility and ritual adaptation. The pragmatic
 *   incoherence reading instantiates one interpretation of this kernel: the
 *   incoherence is not a failure of synthesis but an active institutional
 *   choice to suppress coherence-seeking in favor of functional pragmatism.
 *   This reading coexists with the syncretic fusion reading (which claims
 *   coherence was achieved through synthesis) and the domain partition
 *   reading (which claims kami and Buddha operated in separate ontological
 *   domains). The pragmatic incoherence reading differs structurally: it
 *   asserts that no coherence was achieved, that practitioners navigated
 *   contradictions without resolution, and that institutional actors
 *   benefited from maintaining this state.
 *
 * KEY AGENTS:
 *   - Lay Practitioners: Primary victims (powerless/identity_locked) — bear cognitive cost of navigating contradictory frameworks; identity fused with ritual participation; no exit except apostasy
 *   - Village Priests: Secondary beneficiary (moderate/constrained) — coordinate ritual functions while extracting authority through ontological ambiguity; constrained by institutional expectations
 *   - Institutional Syncretism (Buddhist temples and Shinto shrines): Primary beneficiary (institutional/arbitrage) — benefit from flexibility to absorb local practices without doctrinal crisis
 *   - Doctrinal Authorities (Buddhist and Shinto): Secondary beneficiary (institutional/arbitrage) — maintain fiction of coherence through interpretive layers; preserve authority through non-engagement with contradictions
 *   - Organized Priesthood: Secondary beneficiary (organized/constrained) — extract authority and resources through doctrinal gatekeeping; maintain incoherence as institutional property
 *   - Ontological Consistency: Primary victim (abstract/trapped) — the coherence-seeking impulse is suppressed; doctrinal resolution is prevented
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as immutable feature of religious practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatic_incoherence_reading, 0.62).
domain_priors:suppression_score(pragmatic_incoherence_reading, 0.48).
domain_priors:theater_ratio(pragmatic_incoherence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(pragmatic_incoherence_reading, "Pragmatic Incoherence in Kami-Buddha Ontology (Japanese Religious Practice)").
narrative_ontology:topic_domain(pragmatic_incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatic_incoherence_reading, '66d18764-b7fe-4e64-8b49-84afbe0cb7b4').
narrative_ontology:cs_kernel_codification('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', distributed).
narrative_ontology:cs_authority_grounding('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', extraction).
narrative_ontology:cs_interpretation_layer_present('66d18764-b7fe-4e64-8b49-84afbe0cb7b4').
narrative_ontology:cs_reading_relation('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', pragmatic_incoherence_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', pragmatic_incoherence_reading__domain_partition_reading, influences).
narrative_ontology:cs_axiom('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', foundational, incoherence_maintained_not_resolved).
narrative_ontology:cs_axiom_status(incoherence_maintained_not_resolved, holdable).
narrative_ontology:cs_axiom_grounding('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', incoherence_maintained_not_resolved, empirically_contingent).
narrative_ontology:cs_axiom('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', foundational, pragmatism_prioritized_over_coherence).
narrative_ontology:cs_axiom_status(pragmatism_prioritized_over_coherence, holdable).
narrative_ontology:cs_axiom_grounding('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', pragmatism_prioritized_over_coherence, instrumental).
narrative_ontology:cs_reference_frame('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', pragmatic_framework_navigation).
narrative_ontology:cs_drift_state('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66d18764-b7fe-4e64-8b49-84afbe0cb7b4', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(pragmatic_incoherence_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, institutional_flexibility).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, ontological_consistency).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, doctrinal_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, village_priests).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, shinto_shrines).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(pragmatic_incoherence_reading, pragmatism_over_consistency).
narrative_ontology:constraint_vindicates(pragmatic_incoherence_reading, functional_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in both kami and Buddha rituals without doctrinal resolution. Bear cognitive burden of navigating contradictory frameworks. Identity fused with ritual participation and community membership. Cannot exit without abandoning spiritual identity and social standing.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, lay_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Coordinate ritual functions (life-cycle ceremonies, seasonal observances, community cohesion) across kami and Buddha frameworks. Extract authority and resources through ontological ambiguity. Benefit from flexibility to serve both functions without doctrinal resolution. Constrained by institutional expectations and career dependence on ritual authority.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, village_priests, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(pragmatic_incoherence_reading, village_priests, beneficiary).

% Maintain institutional stability through flexible absorption of local kami practices. Benefit from absence of enforced ontological coherence. Can adapt to regional variation without doctrinal crisis. Net beneficiary of pragmatic incoherence.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, buddhist_temples, beneficiary,
    institutional, generational, arbitrage, national).

% Maintain institutional stability through flexible absorption of Buddhist practices. Benefit from absence of enforced ontological coherence. Can adapt to regional variation without doctrinal crisis. Net beneficiary of pragmatic incoherence.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, shinto_shrines, beneficiary,
    institutional, generational, arbitrage, national).

% Maintain fiction of coherence through interpretive layers and doctrinal silence. Preserve authority through non-engagement with contradictions. Benefit from incoherence as institutional property that prevents doctrinal resolution and maintains interpretive monopoly.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, doctrinal_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Coordinate ritual practice across regions through organized associations. Extract authority and resources through doctrinal gatekeeping. Maintain incoherence as institutional property that prevents lay practitioners from resolving contradictions independently.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, organized_priesthood, agenda_setter,
    organized, generational, constrained, national).

% The coherence-seeking impulse and the doctrinal resolution that would satisfy it are suppressed. Ontological consistency is not achieved; the constraint prevents its achievement. Non-agent entity kept for narrative completeness.
narrative_ontology:constraint_stakeholder(pragmatic_incoherence_reading, ontological_consistency, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(pragmatic_incoherence_reading, ontological_consistency).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate ritual practice across kami and Buddha frameworks without requiring doctrinal resolution. Enable institutional flexibility to absorb local practices and adapt to regional variation. Maintain community cohesion through ritual participation without enforcing ontological coherence.
% TRANSFER_FUNCTION: Cognitive labor flows from lay practitioners (who bear the burden of navigating contradictions) to institutional actors (who benefit from flexibility and authority). Authority and resources flow from lay practitioners to priests and doctrinal authorities through ritual participation and doctrinal gatekeeping.
% ABSENT_VOICES: Coherence-seeking philosophers and theologians who would propose synthesis or partition solutions are absent from the institutional conversation. Their proposals are suppressed through non-engagement and interpretive gatekeeping. Lay practitioners who would demand coherence are constrained by identity fusion and community expectations.
% DISAPPEARANCE_RATIONALE: If pragmatic incoherence disappeared (if coherence were enforced or achieved), institutional arrangements would rearrange significantly. Doctrinal authorities would lose interpretive monopoly. Priests would lose flexibility to serve both functions. Institutional actors would face doctrinal constraints on practice. The entire coordination mechanism depends on the absence of enforced coherence.
% FOUNDING_PROBLEM: How to maintain religious practice and institutional stability in a context where kami and Buddha frameworks coexist without unified ontology. The founding problem is coordination across incoherent frameworks, not resolution of incoherence.
% FOUNDING_PROBLEM_CORROBORATION: Historical evidence: Japanese religious institutions maintained this arrangement for centuries without resolving the contradiction. Contemporary ethnographic evidence: practitioners continue to navigate frameworks pragmatically. Institutional evidence: no major coherence-seeking movement successfully enforced doctrinal resolution. The founding problem remains live because institutional actors continue to benefit from pragmatic incoherence.
narrative_ontology:disappearance_verdict(pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(pragmatic_incoherence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY PRACTITIONER (SNARE) — Identity fused with ritual participation; cannot exit without abandoning community membership and spiritual identity. Navigates contradictory frameworks without resolution, bearing the cognitive cost of incoherence. No exit path except apostasy. Maximum extraction: forced to hold incompatible commitments simultaneously.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE PRIEST (TANGLED ROPE) — Coordinates genuine ritual functions (life-cycle ceremonies, seasonal observances, community cohesion) while extracting authority and resources through ontological ambiguity. Benefits from flexibility to serve both kami and Buddha functions without doctrinal resolution. Constrained by institutional expectations and career dependence on ritual authority.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL SYNCRETISM (ROPE) — Buddhist temples and Shinto shrines benefit from the absence of enforced ontological coherence. Flexibility to absorb local practices, adapt to regional variation, and maintain institutional stability without doctrinal crisis. Net beneficiary: the incoherence is the coordination mechanism that enables institutional survival.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINAL AUTHORITY (PITON) — Buddhist and Shinto doctrinal authorities maintain the fiction of coherence through interpretive layers and silence. The coherence-maintenance apparatus is largely performative: doctrinal texts are preserved and cited, but their contradictions are absorbed through non-engagement rather than resolution. Theater ratio high: the appearance of doctrinal authority persists despite functional incoherence.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED PRIESTHOOD (TANGLED ROPE) — Organized religious institutions (Buddhist sects, Shinto associations) coordinate ritual practice across regions while extracting authority and resources through doctrinal gatekeeping. The incoherence is maintained as institutional property: resolving it would dissolve the authority structure that depends on interpretive monopoly.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, ontological incoherence may appear as an immutable feature of religious practice itself: all religious systems contain unresolved tensions, and practitioners navigate them pragmatically. This perspective risks naturalizing what is actually a contingent institutional choice to suppress coherence-seeking. Engine will flag as false summit: the incoherence is maintained through active suppression, not inherent to religion.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatic_incoherence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pragmatic_incoherence_reading, TR),
    TR >= 0.70.

:- end_tests(pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint extracts cognitive labor from lay practitioners (who bear the burden of navigating contradictions) and doctrinal authority from institutional actors (who benefit from the absence of coherence-seeking). The extraction is not maximal because genuine coordination functions exist (ritual flexibility, institutional stability, community cohesion). Suppression (0.48): Moderate. Institutional actors suppress coherence-seeking through non-engagement, interpretive gatekeeping, and doctrinal silence. But suppression is not total — coherence-seeking movements did emerge historically, and practitioners can navigate frameworks with some agency. Theater ratio (0.58): Moderate-high. Doctrinal authorities maintain the appearance of coherence through preserved texts and formal authority, but the coherence-maintenance apparatus is largely performative. The theater has increased over time as the gap between doctrine and practice widened, requiring more interpretive labor to maintain the fiction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. Lay practitioners experience maximum extraction (Snare) — they are trapped by identity fusion and forced to hold incompatible commitments. Village priests experience mixed coordination and extraction (Tangled Rope) — they benefit from flexibility while constraining practitioners. Institutional actors experience pure coordination (Rope) — the incoherence enables institutional survival and adaptation. Doctrinal authorities experience degraded coherence (Piton) — they maintain the fiction of authority through performative coherence-maintenance. The organized priesthood experiences institutional extraction (Tangled Rope) — they extract authority through doctrinal gatekeeping. The analytical observer risks seeing immutable natural law (Mountain) — ontological incoherence as inherent to religious practice — but the structural data reveals this as a false summit: the incoherence is actively maintained through institutional suppression of coherence-seeking.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the constraint. Lay practitioners are victims with identity_locked exit: high d → high χ (maximum experienced extraction). Village priests are beneficiaries with constrained exit: moderate d → moderate χ. Institutional actors are beneficiaries with arbitrage exit: low d → low/negative χ (subsidy). Doctrinal authorities are beneficiaries with arbitrage exit: low d → low χ. The organized priesthood are beneficiaries with constrained exit: moderate d → moderate χ. Ontological consistency is a victim (abstract agent) with trapped exit: high d → high χ. The engine derives d from beneficiary/victim declarations and exit modulation; the commentary reflects the structural relationships without computing the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the pragmatic incoherence reading instantiates a specific institutional choice: to suppress coherence-seeking in favor of functional pragmatism. The mandate (maintain religious practice and institutional stability) has not outlived its function — it remains live. But the mandate has been decoupled from coherence-seeking: the institutional actors have chosen pragmatism over doctrinal resolution. This is not mandatrophy in the classical sense (mandate outlived function) but rather a deliberate institutional choice to prioritize coordination over coherence. The constraint is Tangled Rope because it coordinates genuine ritual functions while extracting cognitive labor and doctrinal authority. The false summit (Mountain) is the risk that the pragmatic incoherence appears as natural law rather than institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_suppression_mechanism,
    'Is the absence of unified ontology a structural feature of Japanese religious practice, or an actively maintained institutional choice to suppress coherence-seeking?',
    'Historical analysis of coherence-seeking movements (e.g., Edo-period doctrinal synthesis attempts); examination of institutional responses to coherence proposals; comparison with religious systems that enforce ontological consistency',
    'If structural feature: constraint is closer to Mountain (immutable). If actively suppressed: constraint is Snare/Tangled Rope (extractive, maintained through enforcement). Classification hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_suppression_mechanism, empirical, 'Whether incoherence is structural or actively suppressed').

omega_variable(
    pragmatism_vs_extraction,
    'Does pragmatic navigation of contradictory frameworks represent genuine functional coordination, or does it mask extraction of cognitive labor and doctrinal authority?',
    'Ethnographic analysis of practitioner experience: do practitioners experience the incoherence as enabling (flexibility) or constraining (cognitive burden)? Comparison of ritual efficacy claims across coherent vs incoherent frameworks.',
    'If genuine coordination: constraint is Rope/Tangled Rope with real beneficiaries. If extraction masked as pragmatism: constraint is Snare with victims bearing cognitive cost. Beneficiary/victim structure depends on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pragmatism_vs_extraction, empirical, 'Whether pragmatism masks extraction of cognitive labor').

omega_variable(
    reading_contest_kernel,
    'This constraint is one reading of the kami-buddha ontology kernel. Does the pragmatic incoherence reading foreclose the syncretic fusion reading (coherence achieved through synthesis), or do both remain live positions?',
    'Historical examination of coherence-seeking movements and institutional responses; analysis of whether synthesis proposals were rejected on logical grounds (foreclosure) or institutional grounds (suppression). Comparison of contemporary practitioners who hold fusion vs incoherence readings.',
    'If foreclosure: the readings are logically incompatible. If coexistence: both readings remain live, and the constraint is the institutional choice to suppress fusion-seeking. This determines the reading_relations field in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel, conceptual, 'Whether pragmatic incoherence forecloses syncretic fusion reading').

omega_variable(
    institutional_benefit_distribution,
    'Who specifically benefits from the maintenance of incoherence? Is it distributed across all institutional actors, or concentrated in doctrinal authorities and organized priesthoods?',
    'Institutional analysis: examination of which actors gain authority, resources, or flexibility from incoherence vs coherence. Comparison of institutional positions under coherence-seeking vs incoherence-maintenance regimes.',
    'If distributed: constraint is Rope (coordination). If concentrated: constraint is Snare/Tangled Rope (extraction). Beneficiary structure and directionality depend on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_distribution, empirical, 'Distribution of institutional benefits from incoherence').

omega_variable(
    lay_practitioner_agency,
    'Do lay practitioners experience the incoherence as constraining (identity_locked) or as enabling (mobile/arbitrage)? Can they navigate frameworks opportunistically, or are they trapped by institutional expectations?',
    'Ethnographic study of practitioner choice: do practitioners select kami or Buddha frameworks based on functional need, or are they constrained by community expectations? Analysis of practitioners who attempt to exit or reframe.',
    'If constrained/trapped: lay practitioners are victims. If mobile/arbitrage: they are beneficiaries of flexibility. Exit options classification depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_practitioner_agency, empirical, 'Lay practitioner agency in framework navigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatic_incoherence_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_incoherence_tr_t0, pragmatic_incoherence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prag_incoherence_tr_t3, pragmatic_incoherence_reading, theater_ratio, 3, 0.51).
narrative_ontology:measurement(prag_incoherence_tr_t6, pragmatic_incoherence_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(prag_incoherence_tr_t9, pragmatic_incoherence_reading, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(prag_incoherence_be_t0, pragmatic_incoherence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prag_incoherence_be_t3, pragmatic_incoherence_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(prag_incoherence_be_t6, pragmatic_incoherence_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(prag_incoherence_be_t9, pragmatic_incoherence_reading, base_extractiveness, 9, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prag_incoherence_su_t0, pragmatic_incoherence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prag_incoherence_su_t3, pragmatic_incoherence_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(prag_incoherence_su_t6, pragmatic_incoherence_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(prag_incoherence_su_t9, pragmatic_incoherence_reading, suppression_requirement, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% The pragmatic incoherence reading is part of a constraint family decomposing the kami-buddha ontology kernel. Each reading has its own ε value and structural properties. The pragmatic incoherence reading (this file) has ε=0.62 (moderate-high extraction); the syncretic fusion reading would have lower ε (coherence achieved, less extraction); the domain partition reading would have different victim/beneficiary structure (no cognitive burden if domains are separate). All three readings are linked via network.affects_constraints to show the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
