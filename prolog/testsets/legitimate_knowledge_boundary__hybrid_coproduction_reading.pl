% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Legitimate Knowledge Boundary (Hybrid Co-Production Reading)
 *   domain: epistemology/STS/political_theory
 *
 * SUMMARY:
 *   The hybrid co-production reading of the legitimate knowledge boundary
 *   construes it as a constraint that requires BOTH methodological rigor AND
 *   experiential validity to be recognized as legitimate knowledge,
 *   integrated through co-production processes where credentialed and
 *   non-credentialed knowledge bearers jointly construct validation
 *   standards. This reading is one interpretation of what makes knowledge
 *   claims legitimate in contemporary STS and political epistemology. It
 *   differs from a pure expertise reading (which would privilege methodology
 *   alone) and a pure experientiality reading (which would privilege lived
 *   experience alone). The hybrid reading construes the boundary as
 *   necessarily permeable, requiring active infrastructure and governance to
 *   integrate both sources. The constraint exhibits genuine coordination
 *   (co-production solves the problem of how to honor both rigor and
 *   validity) while also exhibiting extraction (the process of integration
 *   systematically advantages institutional actors and risks deforming
 *   experiential knowledge through translation into hybrid standards). The
 *   rising extractiveness (0.32 → 0.48) and suppression_requirement (0.48 →
 *   0.62) over the interval reflect the constraint's maturation: as
 *   co-production infrastructure institutionalizes, gatekeeping relocates
 *   from explicit expertise credentials to committee membership and process
 *   control. The rising theater_ratio (0.40 → 0.58) indicates that
 *   performative inclusion (checking boxes for community engagement,
 *   establishing committees) is outpacing genuine epistemic integration.
 *
 * KEY AGENTS:
 *   - Co-Production Infrastructure: Institutional beneficiary (institutional/arbitrage) — universities, health systems, environmental agencies that establish official co-production protocols; benefits from reduced contestation and legitimacy buffering
 *   - Marginalized Knowledge Communities: Primary victims with constrained exit (powerless/trapped or organized/constrained) — communities whose experiential knowledge was previously devalued; gain institutional recognition but at cost of epistemic autonomy and translation loss
 *   - Credentialed Expertise Gatekeepers: Defenders of methodology-first approaches (powerful/mobile but trapped in practice) — experience co-production as extraction that redistributes their authority; cannot easily exit due to institutional dependencies
 *   - Experiential Knowledge Movement: Organized agents (organized/constrained) — movements for experiential validation; benefit from legitimacy in institutional contexts but lose control over meaning-making through hybrid translation
 *   - Co-Production Practitioners: Intermediary implementers (moderate/constrained) — researchers, facilitators, and designers who build co-production infrastructure; experience genuine coordination problems alongside extraction
 *   - Academic Quality Assurance System: Institutional maintenance structure (institutional/arbitrage) — peer review, IRBs, credentialing bodies; maintains theater-heavy processes while performing inclusion
 *   - Analytical Observer: Civilizational epistemology (analytical/analytical) — risks naturalizing contingent hybrid integration as inevitable feature of knowledge systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.48).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.62).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Legitimate Knowledge Boundary (Hybrid Co-Production Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/STS/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'fe81d567-0131-4c6e-8fa0-fd72b30171fa').
narrative_ontology:cs_kernel_codification('fe81d567-0131-4c6e-8fa0-fd72b30171fa', distributed).
narrative_ontology:cs_authority_grounding('fe81d567-0131-4c6e-8fa0-fd72b30171fa', extraction).
narrative_ontology:cs_reading_relation('fe81d567-0131-4c6e-8fa0-fd72b30171fa', credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe81d567-0131-4c6e-8fa0-fd72b30171fa', experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('fe81d567-0131-4c6e-8fa0-fd72b30171fa', foundational, methodological_rigor_is_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(methodological_rigor_is_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('fe81d567-0131-4c6e-8fa0-fd72b30171fa', methodological_rigor_is_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('fe81d567-0131-4c6e-8fa0-fd72b30171fa', foundational, experiential_validity_is_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(experiential_validity_is_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('fe81d567-0131-4c6e-8fa0-fd72b30171fa', experiential_validity_is_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('fe81d567-0131-4c6e-8fa0-fd72b30171fa', secondary, co_production_infrastructure_can_integrate_both).
narrative_ontology:cs_axiom_status(co_production_infrastructure_can_integrate_both, holdable).
narrative_ontology:cs_axiom_grounding('fe81d567-0131-4c6e-8fa0-fd72b30171fa', co_production_infrastructure_can_integrate_both, empirically_contingent).
narrative_ontology:cs_reference_frame('fe81d567-0131-4c6e-8fa0-fd72b30171fa', dual_validation_framework).
narrative_ontology:cs_drift_state('fe81d567-0131-4c6e-8fa0-fd72b30171fa', contemporary_institutional_coproduction_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe81d567-0131-4c6e-8fa0-fd72b30171fa', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_infrastructure).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_knowledge_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_monopoly).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_framings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED KNOWLEDGE BEARER (SNARE) — Communities whose experiential knowledge is systematically devalued by credentialed expertise gatekeepers cannot exit the epistemic regime without abandoning their own lived authority. The constraint traps them in permanent subordination: their knowledge is only valid when certified by the credentialed gate, yet credentialed validation requires them to translate their knowledge into forms that distort or erase its context-dependence. No exit; maximum extraction.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CO-PRODUCTION PRACTITIONER (TANGLED ROPE) — Practitioners building co-production infrastructure (research centers, participatory design processes, community validation protocols) experience the constraint as a genuine coordination problem with asymmetric extraction. They benefit from the legitimacy bridge (co-production enables knowledge integration) but face high costs of infrastructure maintenance, epistemic friction, and ongoing tension with both credentialed experts and pure experiential communities. Mixed: both coordination and extraction.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CO-PRODUCTION INSTITUTION (ROPE) — Organizations (universities, health systems, environmental agencies) that establish official co-production protocols benefit from reduced contestation: having a legitimized pathway for integrating experiential knowledge deflects criticism and enables resource extraction from marginalized communities framed as 'partnership.' The constraint, from this view, is pure coordination: solving the problem of how to incorporate experiential input without ceding expertise authority. Low or negative extraction experienced by the institution.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIALED EXPERTISE MONOPOLY (SNARE) — From the perspective of those defending expertise gatekeeping, co-production appears as a snare: it forces institutional incorporation of knowledge they cannot validate through their own standards, strips them of authority over legitimacy determination, and exposes them to liability for outcomes that integrate non-credentialed inputs. They experience the constraint as extraction because it redistributes epistemic authority without their consent, yet they cannot easily exit (their institutional position depends on the legitimacy they now must share). Mobile in theory; trapped in practice by institutional dependencies.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EXPERIENTIAL KNOWLEDGE MOVEMENT (TANGLED ROPE) — Organized movements for experiential validation (disability justice frameworks, indigenous knowledge sovereignty, community science networks) experience co-production as partial victory with embedded extraction. Co-production legitimizes their knowledge in institutional contexts, but doing so requires translation into hybrid validation standards that compromise epistemic autonomy. The constraint both enables their participation and constrains the integrity of their knowledge. They gain access but lose control over meaning-making.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC QUALITY ASSURANCE SYSTEM (PITON) — Peer review, institutional review boards, and credentialing bodies recognize that knowledge legitimacy increasingly requires co-production validation, yet they maintain theater-heavy review processes that bracket experiential input. 'Community engagement' boxes are checked; co-production committees exist on paper; but core epistemic authority remains gatekept by credentialed processes. The system performs inclusion while maintaining exclusion. Theater ratio is high; functional coordination is degraded.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some hybrid validation is inherent to knowledge production: all knowledge is socially embedded, and credentialed expertise is itself shaped by the communities it studies. Complete epistemic purity is impossible. The constraint appears as an immutable feature of knowledge systems themselves — inescapable tension between rigor and validity, standardization and context-dependence. However, this naturalizes what is actually a contingent institutional arrangement (the specific mechanisms of co-production gatekeeping are not laws of nature).
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimate_knowledge_boundary__hybrid_coproduction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, TR),
    TR >= 0.70.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The co-production constraint requires dual validation infrastructure that advantages institutional actors (universities, agencies, committees) who can organize and resource co-production processes. Excluded or under-resourced communities gain access to legitimacy pathways but must translate their knowledge into hybrid standards, incurring epistemic costs. The extraction is not total (coordination genuinely solves a real problem — how to integrate rigor and validity) but it is substantial (the integration process systematically relocates gatekeeping from explicit credentials to less visible committee structures). Suppression (0.62): Moderate-high. Significant barriers exist for those trying to operate outside the co-production framework: pure experientiality is dismissed as methodologically unsound; pure methodology is dismissed as contextually blind. The constraint enforces dual participation. However, suppression is not absolute — some actors can and do work outside co-production (lone researchers, traditional practitioners, pure STS theorists). Theater (0.58): Moderate-high. Co-production processes display substantial performative content: committees are established (then rarely convened); community engagement boxes are checked (with limited actual voice in decisions); inclusion statements are written (without redistribution of epistemic authority). The performative element has increased as co-production has become institutionally fashionable — many organizations adopt the form (committees, protocols) while maintaining substantive expertise gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomena (knowledge communities with different validation standards, institutional needs to incorporate multiple voices, power asymmetries in who gets to define what counts as knowledge) appear as different types from different perspectives. The excluded knowledge bearer sees a snare (trapped in mandatory translation). The co-production practitioner sees genuine coordination with extraction (tangled rope). The institutional adopter sees pure coordination (rope). The expertise defender sees extraction of authority (snare). The experiential movement sees partial victory with epistemic compromise (tangled rope). The quality assurance system sees its own degradation (piton). The analytical observer risks naturalizing the whole constraint as inevitable (mountain/false summit). No single perspective reveals the full structure — the constraint is the perspectival gap itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's position relative to the extraction flow. Marginalized knowledge communities (powerless/trapped) experience high d (0.90+) — they have minimal exit options and bear the cost of mandatory translation. Co-production practitioners (moderate/constrained) experience moderate d (0.50-0.60) — they both benefit (from legitimacy bridges) and bear costs (from infrastructure maintenance). Institutional adopters (institutional/arbitrage) experience low d (0.10-0.25) — they are beneficiaries with high exit capacity. Expertise defenders (powerful/mobile in theory) paradoxically experience high d in practice (0.70+) because their institutional position depends on expertise authority that co-production redistributes — they are structurally trapped despite nominal mobility. The empirical directionality signatures reveal the constraint's structure more accurately than nominal power levels do.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid co-production reading resolves mandatrophy by showing that tangled_rope classification is correct: the constraint genuinely coordinates (solves the problem of integrating rigor and validity) while genuinely extracting (relocates gatekeeping and deforms experiential knowledge). The temptation is to classify it as either pure rope (if one emphasizes the coordination benefit) or snare (if one emphasizes the extraction mechanism). The tangled_rope classification honors both: this reading REQUIRES both coordinated integration AND suppression mechanisms. Without the suppression (the requirement that experiential knowledge be validated through hybrid standards, the gatekeeping relocated to committees, the institutional resource concentration), the coordination function collapses into pure experientiality. Without the coordination function (the genuine problem of how to honor both rigor and contextuality), the suppression would be unmotivated and would likely be challenged successfully. The mandatrophy is resolved by recognizing that the constraint's stability depends on maintaining both functions simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_validation_sufficiency,
    'Do co-production processes actually produce more legitimate knowledge than either pure methodology or pure experiential frameworks alone, or do they reproduce the extraction logic of both simultaneously?',
    'Longitudinal tracking of knowledge claims validated through co-production vs single-framework approaches; comparison of contestation rates, adoption rates, and long-term validity; post-hoc analysis of whether hybrid validation prevented or merely delayed epistemic failure',
    'If co-production produces genuinely superior knowledge: constraint is justified tangled rope (coordination + modest extraction). If co-production merely distributes extraction across both framings: constraint is snare with false legitimacy (high-extraction gatekeeping disguised as inclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_validation_sufficiency, empirical, 'Whether hybrid validation produces legitimate knowledge superior to single-framework approaches').

omega_variable(
    translation_loss_asymmetry,
    'Does the translation from experiential knowledge into hybrid validation standards systematically distort or erase aspects of the original knowledge that credentialed methodology cannot represent? If so, is the loss distributed symmetrically between frameworks or does one framework''s knowledge survive translation intact while the other''s is structurally deformed?',
    'Discourse analysis of translated vs original experiential knowledge claims; interviews with experiential knowledge bearers about perceived epistemic loss; comparison of survival rates of context-dependent features across credentialed vs experiential vs hybrid framings',
    'If translation loss is asymmetric (credentialed framework survives intact while experiential is deformed): co-production reproduces expert-favorable extraction under the guise of inclusion. If symmetric: genuine hybrid integration. If asymmetry favors experiential frameworks: credentialed framework faces erosion pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_loss_asymmetry, empirical, 'Whether translation into hybrid standards asymmetrically deforms one framework''s knowledge').

omega_variable(
    gatekeeping_mechanism_displacement,
    'Does co-production displace the gatekeeping mechanism from credentialed expertise to co-production committee membership? In other words, does it abolish gatekeeping or merely relocate who controls the gate?',
    'Compositional analysis of co-production committees; tracking of whose voices carry decision weight; examination of whose knowledge claims are accepted vs rejected within committees; post-hoc audits of whether excluded communities experience the committee as more or less accessible than traditional credentialing',
    'If gatekeeping is displaced but not abolished: co-production is snare wearing rope costume (suppression is maintained through committee formation, not expertise credential). If gatekeeping is genuinely abolished: constraint is true tangled rope (coordination with honest extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeping_mechanism_displacement, empirical, 'Whether co-production abolishes or merely relocates epistemic gatekeeping').

omega_variable(
    kernel_frame_adequacy,
    'Is the contested kernel (''legitimate knowledge boundary'') adequately framed as the integration of methodology AND experiential validity, or does this framing itself embed a particular reading (hybrid co-production) by assuming that both frameworks must be integrated rather than that one or both might be replaced entirely?',
    'Conceptual analysis of alternative kernel framings that do not presuppose methodological-experiential integration; examination of whether the hybrid frame is neutral or whether it already privileges certain readings and forecloses others',
    'If the kernel framing is neutral: this reading coexists equally with sibling readings. If the hybrid frame presupposes co-production''s legitimacy: the kernel itself has been gerrymandered to favor this reading, which would suggest the kernel is already contested at a deeper level (contestation about what the contested question even IS).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_frame_adequacy, conceptual, 'Whether the kernel framing itself embeds the co-production reading').

omega_variable(
    reading_identity_lock,
    'For actors genuinely committed to co-production as a reading (institutional leaders, STS scholars, some marginalized knowledge communities), is this commitment identity-locked (their professional identity is constituted through co-production advocacy) or structurally mobile (they could exit to pure methodology or pure experientiality without identity collapse)?',
    'Ethnographic examination of commitment sustainability under pressure; interviews probing whether exiting co-production would feel like identity dissolution or policy disagreement; analysis of turnover rates and career trajectories of co-production advocates',
    'If identity-locked: co-production communities have internalized their own suppression mechanism — the constraint reproduces itself through identity fusion. If structurally mobile: commitment to co-production is a rational choice among alternatives, not cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_lock, empirical, 'Whether co-production advocates are identity-locked or structurally mobile').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_know_coprod_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legit_know_coprod_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(legit_know_coprod_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(legit_know_coprod_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(legit_know_coprod_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(legit_know_coprod_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(legit_know_coprod_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(legit_know_coprod_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(legit_know_coprod_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, institutional_knowledge_gatekeeping).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemic_authority_distribution).

% DUAL FORMULATION NOTE:
% The legitimate knowledge boundary is a kernel — a stabilized but contested commitment that different parties read differently. Three structurally distinct constraint stories instantiate the three live readings: (1) credentialed_expertise_reading (ε≈0.25, Mountain for methodology-privileging observers) — knows legitimacy as methodological rigor; treats experiential input as corrigible; (2) experiential_pluralism_reading (ε≈0.35, Rope for experiential-privileging observers) — knows legitimacy as lived validity; treats methodology as one perspective among many; (3) hybrid_coproduction_reading [THIS FILE] (ε≈0.48, Tangled Rope) — knows legitimacy as integrated dual validation; treats both as necessary and institutional co-production as the solution. Each story has its own classification, metrics, and perspectives. They are linked by network.affects_constraints to indicate that changes in one reading's institutional adoption affect the pressure on sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
