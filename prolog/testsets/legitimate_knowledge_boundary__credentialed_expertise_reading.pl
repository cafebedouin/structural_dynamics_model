% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Expertise Gatekeeping of Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies
 *
 * SUMMARY:
 *   This constraint establishes that legitimate knowledge—knowledge worthy of
 *   funding, policy influence, and institutional resources—derives
 *   exclusively from methodologically rigorous inquiry validated through
 *   credentialed peer review. This reading embodies the institutional
 *   position of established science: methodology as gateway, credentialing as
 *   proof of rigor, peer review as quality control. The constraint is
 *   presented as protecting truth from fraud and ensuring reliable evidence
 *   for policy. Communities excluded by the constraint (indigenous knowledge
 *   systems, experiential practitioners, self-taught experts, dissenting
 *   methodologies) read the same structure as gatekeeping that privileges
 *   particular ways of knowing, excludes superior knowledge sources, and
 *   extracts legitimacy and resources from the excluded to the credentialed.
 *   The claim/metric gap is intentional: this story claims tangled_rope
 *   (genuine coordination function + asymmetric extraction) while authoring
 *   metrics that describe substantial extraction with high suppression. The
 *   engine will measure whether the claim holds structurally.
 *
 * KEY AGENTS:
 *   - credentialed_research_institutions — institutional agenda-setters; control credentialing, peer review, funding allocation; benefit through prestige, resources, monopoly on legitimacy
 *   - peer_review_gatekeepers — individual institutional agenda-setters; control publication pathways; identity-fused with role; high suppression due to anonymity and discretionary power
 *   - disciplinary_authority_bodies — institutional agenda-setters; codify methodological standards; benefit through authority over boundaries
 *   - experiential_knowledge_practitioners — moderate-powered victims; excluded by credentialing requirement; trapped by inability to exit without losing livelihood and identity
 *   - powerless_communities — structural victims; trapped with identity-locking because credential-translation distorts their knowledge; bear costs of exclusion from policy and resource allocation
 *   - dissenting_methodologists — constrained payers; face institutional marginalization despite rigor; must either conform or accept degradation
 *   - policy_makers_and_funders — mobile beneficiaries; use credentialed expertise as administrative solution to legitimacy; could fund alternatives but institutionally pressured not to
 *   - epistemological_pluralists — analytical observers; document the constraint's mechanisms without claiming to adjudicate truth; provide evidence that credentialing is constructed not discovered
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.71).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Gatekeeping of Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '63982111-ab3a-4e5a-8b29-ae1de7e3c75f').
narrative_ontology:cs_kernel_codification('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', formalized).
narrative_ontology:cs_authority_grounding('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', extraction).
narrative_ontology:cs_interpretation_layer_present('63982111-ab3a-4e5a-8b29-ae1de7e3c75f').
narrative_ontology:cs_reading_relation('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', foundational, methodological_rigor_truth_proximity).
narrative_ontology:cs_axiom_status(methodological_rigor_truth_proximity, holdable).
narrative_ontology:cs_axiom_grounding('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', methodological_rigor_truth_proximity, empirically_contingent).
narrative_ontology:cs_axiom('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', foundational, credentialing_necessity_for_legitimacy).
narrative_ontology:cs_axiom_status(credentialing_necessity_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', credentialing_necessity_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', scientific_methodology_as_truth_filter).
narrative_ontology:cs_drift_state('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', contemporary_post_replication_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('63982111-ab3a-4e5a-8b29-ae1de7e3c75f', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_research_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeepers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, disciplinary_authority_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_practitioners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, communities_with_non_methodological_expertise).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, dissenting_methodological_approaches).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_research_traditions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end because the constraint's core function—gatekeeping legitimacy—operates to concentrate epistemic authority and associated resources (funding, prestige, policy influence) in credentialed institutions. The constraint systematically devalues and de-resources non-credentialed knowledge despite cases of superior empirical performance. Suppression measures 0.71 because the constraint's persistence requires active enforcement through peer-review gatekeeping, publication rejection, credential-requirement policies, and the threat of professional isolation. The suppression is asymmetrically applied: credentialed researchers face peer review too, but within a community that shares their framework; excluded practitioners face a peer review designed in their absence and fundamentally hostile to their epistemology. Theater ratio 0.41 reflects that peer review performs a genuine quality-control function (identifying fraud, poor methodology, unreplicable results), but an increasing share of peer-review activity defends the credentialing boundary itself rather than identifying actual errors. The accessibility_collapse grid shows how alternatives are most collapsed at the organizational level (established institutions are locked into credentialing or lose legitimacy) and least at the structural level (competing epistemological frameworks remain live in discourse even while excluded from resources). The suppression grid shows individual and organizational suppression rising over the interval (credentialing requirements tightening, peer-review barriers intensifying, alternative publication channels facing stigma) while structural-level suppression remains lower (the framework of methodological rigor as proxy for truth persists as a claim, not as enforced fact—it must still be defended, which is why resistance to the constraint exists). Temporal measurements show extractiveness accumulating as the constraint became more asymmetric over 50 years: in t0 (founding era) it was closer to genuine coordination; in t50 (contemporary) it functions increasingly as gatekeeping divorced from quality improvement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (credentialed institutions, peer-review gatekeepers, disciplinary bodies) and the victim seats (excluded communities, constrained methodologists) compute dramatically different types from the same structural data. From the agenda-setter position the constraint is coordination—a hard-won solution to fraud and pseudoscience that requires active maintenance. From the victim positions the constraint is extraction—a monopoly on legitimacy-certification that denies resources and voice to superior knowledge sources. From the policy-maker position (beneficiary but mobile) the constraint is convenient administrative outsourcing—they avoid making legitimacy judgments by delegating to credentialed experts. The engine should detect this divergence from the stakeholder roles and power/exit configurations. The claim of tangled_rope asserts both coordination and extraction coexist; the metrics support that reading. The divergence in computed type across seats validates the tangled_rope claim: one reading cannot adjudicate which seat's experience is 'true' because both are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed institutions and peer-review gatekeepers sit near d=1.0 (full target of the constraint's benefits; collectors of legitimacy, funding, and authority). Their exit options are arbitrage (they could loosen standards but would lose distinctiveness) or identity-locked (for gatekeepers, exiting the role means losing professional identity). Communities excluded by credentialing sit near d approaching 1.0 (targets of extraction—denied resources, voice, legitimacy). Their exit is trapped or identity-locked (they cannot become credentialed without translating their knowledge into a framework that distorts it; exiting their knowledge tradition means losing community and livelihood). Policy makers sit near d=0.5 (they benefit from avoiding legitimacy disputes through expert gatekeeping, but they also pay the cost of being locked into credentialed advice and unable to access non-credentialed knowledge). The asymmetry in d across seats drives the extraction metric: the constraint's persistence depends on the structural lock that keeps victims from exiting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real and urgent: early modern science required protection from quackery and politically-imposed falsehood. The mandatrophy question is whether the founding problem remains live or has been substantially solved and the constraint persists as theater/extraction. The founding_problem_status is authored as 'contested' because credentialed institutions claim the problem is still live (pseudoscience persists, credentialing is still necessary), while external witnesses (historians of science, retrospective policy analysis) attest the founding problem is substantially solved. The constraint's core coordination function—distinguishing careful inquiry from fraud—is now largely automated through infrastructure: replication studies, open data, statistical standards, and computational reproducibility provide fraud-detection without requiring credentialed gatekeeping. What persists is the gatekeeping function itself, increasingly serving to maintain institutional authority rather than to detect fraud. The theater_ratio rising from 0.25 to 0.41 over the interval reflects exactly this drift: the performative maintenance of credentialing (journal-rank obsession, credential-inflation, citation games) increasing as a proportion of peer-review activity relative to actual quality-checking. The mandatrophy is unresolved: the constraint persists despite the founding problem being substantially solved, and the mechanism of persistence is institutional (credentialed institutions benefit and can prevent alternative legitimacy systems from emerging). Declaring mandatrophy_resolved=false is appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_rigor_definition,
    'Is methodological rigor a stable epistemic standard discoverable through careful inquiry, or is it a socially constructed category that privileges certain ways of knowing and excludes others?',
    'Historical and ethnographic documentation of how methodology standards have shifted over time, who decided which standards count, and whether excluded communities produced validated knowledge using different standards.',
    'If methodological rigor is discovered, the constraint appears as protecting legitimate knowledge; if constructed, it appears as gatekeeping that privileges particular ways of knowing. This determination affects whether the constraint''s asymmetry is justified or extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_rigor_definition, conceptual, 'Whether methodological standards are discovered or constructed—shifts the entire interpretation of the constraint.').

omega_variable(
    credentialing_necessity_boundary,
    'Is institutional credentialing structurally necessary for identifying reliable knowledge, or do other validation mechanisms (community reputation, longitudinal outcome tracking, peer-of-practice review) provide equivalent or superior reliability without centralized gatekeeping?',
    'Comparative study of knowledge validation outcomes across credentialed and non-credentialed systems addressing the same domains (agricultural knowledge, medical treatments, engineering solutions). Track predictive accuracy, adaptive capacity, and harm avoidance across systems.',
    'If credentialing is necessary, the constraint is justified coordination cost; if alternative mechanisms are equivalent or superior, the constraint is pure extraction layered over a coordination function. This determines whether the tangled_rope reading survives or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_necessity_boundary, empirical, 'Whether credential gatekeeping is structurally necessary or substitutable.').

omega_variable(
    peer_review_biasing_mechanisms,
    'Does peer review systematically bias against certain knowledge sources (marginalized communities, non-Western epistemologies, results that contradict mainstream consensus) independent of methodological quality?',
    'Blind-review randomized trials submitting identical methodological quality work under different authorship/institutional affiliations; meta-analysis of publication acceptance rates by author identity and knowledge tradition; longitudinal tracking of how long it takes heterodox insights to gain publication despite eventual vindication.',
    'If systematic bias exists, suppression is higher than authored and gatekeeping functions as identity-policing; if absent, the asymmetry reflects genuine quality judgment. The magnitude of bias affects whether effective extraction is correctly estimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peer_review_biasing_mechanisms, empirical, 'Whether peer review is identity-neutral or systematically biases against marginalized knowledge sources.').

omega_variable(
    reading_plurality_constraint,
    'This constraint is ONE reading of the legitimate_knowledge_boundary kernel. Could a single framework simultaneously hold both the credentialed_expertise_reading (this constraint: legitimate knowledge requires credentialing and peer review) and the experiential_pluralism_reading (legitimate knowledge arises from lived experience with credentials as optional tools)?',
    'Formal logical analysis of whether the core axioms are contradictory or whether a meta-framework could hold both as valid under different conditions (e.g., ''methodological rigor is necessary for some questions, experiential validity for others''). Historical documentation of whether mixed systems (credentialing + community validation) have been attempted and what pressures pushed them toward one or the other.',
    'If the readings are logically incompatible (forecloses relation), the constraint represents a foundational epistemological choice with no neutral ground; if compatible (coexists or influences relation), the constraint is a choice among options, which changes how its enforced persistence appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_plurality_constraint, conceptual, 'Whether this reading logically forecloses the sibling readings or coexists with them as options.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the suppression that keeps non-credentialed knowledge excluded primarily structural (legal barriers, funding requirements, publication gatekeeping) or internalized (individuals believe their knowledge is inferior, have internalized the credentialing hierarchy, doubt their expertise)?',
    'Post-validation trajectory study: when excluded practitioners gain external recognition (awards, media attention, community investment), does suppression persist (internalized component) or dissolve (structural component only)? Qualitative interviews with practitioners about whether they have absorbed the credentialing hierarchy''s valuation.',
    'If suppression is primarily structural, removing gatekeeping would quickly allow excluded knowledge to circulate; if internalized, the constraint persists in practitioners'' self-perception even after institutional removal. This affects remediation strategy and the effective extraction experienced by victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Whether suppression is structural or internalized—affects both the measurement and potential pathways to constraint removal.').

omega_variable(
    knowledge_pluralism_coordination_viability,
    'Could a system that genuinely pluralized legitimacy (recognizing credentialed AND experiential AND indigenous AND hybrid forms of knowledge as equally valid starting points) actually coordinate research, policy, and resource allocation, or does pluralism necessarily devolve into conflict and incoherence?',
    'Study of existing pluralistic knowledge systems (medical systems that recognize credentialed and herbal medicine, agricultural systems with credentialed agronomy and farmer expertise, environmental management with scientific AND indigenous knowledge). Document coordination mechanisms, conflict resolution, and outcomes.',
    'If pluralism is viable, the constraint appears as an unnecessary monopoly that extracts rents while reducing overall knowledge capacity; if pluralism devolves into incoherence, the constraint provides genuine coordination value. This affects the classification boundary between tangled_rope and rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_pluralism_coordination_viability, empirical, 'Whether pluralized legitimacy could sustain functional coordination or necessarily fails.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(legi_tr_t35, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(legi_tr_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(legi_be_t35, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(legi_be_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(legi_su_t35, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(legi_su_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, research_resource_allocation_gatekeeping).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_consensus_authority_formation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialism_occupational_gatekeeping).

% DUAL FORMULATION NOTE:
% This constraint is part of the legitimate_knowledge_boundary kernel family. The kernel is contested: three readings produce three distinct constraints with different ε, different victim/beneficiary structures, and different types. This reading (credentialed_expertise_reading) claims tangled_rope; the experiential_pluralism_reading will claim snare or rope; the hybrid_coproduction_reading will claim scaffold or rope. All three readings are authored as separate constraint stories and linked via affects_constraints. They are NOT alternative measurements of one constraint—they are structurally distinct constraints arising from genuinely incompatible epistemological commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
