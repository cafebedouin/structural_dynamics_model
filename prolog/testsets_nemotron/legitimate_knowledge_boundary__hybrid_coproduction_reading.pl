% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-Production Epistemic Standard
 *   domain: epistemology/science_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid co-production reading of the
 *   contested kernel 'legitimate_knowledge_boundary'. It asserts that
 *   legitimate knowledge requires BOTH methodological rigor AND experiential
 *   validity, integrated through co-production processes. The constraint
 *   emerged from STS critiques of expertise, CBPR movements, and policy
 *   demands for 'translational' research. It now operates as a funding
 *   mandate, publication standard, and institutional review criterion. The
 *   claimed_type is tangled_rope: it performs genuine coordination
 *   (integrating fragmented epistemic communities) while extracting
 *   compliance costs from those who cannot or will not meet dual standards.
 *   The engine will compute per-seat classifications from the structural
 *   data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.38).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-Production Epistemic Standard").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '8307ebfc-afc3-457b-9c42-591eb55f511e').
narrative_ontology:cs_kernel_codification('8307ebfc-afc3-457b-9c42-591eb55f511e', distributed).
narrative_ontology:cs_authority_grounding('8307ebfc-afc3-457b-9c42-591eb55f511e', practice).
narrative_ontology:cs_interpretation_layer_present('8307ebfc-afc3-457b-9c42-591eb55f511e').
narrative_ontology:cs_reading_relation('8307ebfc-afc3-457b-9c42-591eb55f511e', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('8307ebfc-afc3-457b-9c42-591eb55f511e', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('8307ebfc-afc3-457b-9c42-591eb55f511e', foundational, dual_validation_required_for_legitimacy).
narrative_ontology:cs_axiom_status(dual_validation_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8307ebfc-afc3-457b-9c42-591eb55f511e', dual_validation_required_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('8307ebfc-afc3-457b-9c42-591eb55f511e', foundational, co_production_infrastructure_as_epistemic_prerequisite).
narrative_ontology:cs_axiom_status(co_production_infrastructure_as_epistemic_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('8307ebfc-afc3-457b-9c42-591eb55f511e', co_production_infrastructure_as_epistemic_prerequisite, conventional).
narrative_ontology:cs_reference_frame('8307ebfc-afc3-457b-9c42-591eb55f511e', post_positivist_epistemic_pluralism).
narrative_ontology:cs_drift_state('8307ebfc-afc3-457b-9c42-591eb55f511e', contemporary_funding_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8307ebfc-afc3-457b-9c42-591eb55f511e', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_research_centers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_based_participatory_research_practitioners).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_agencies_mandating_coproduction).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, purely_theoretical_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, purely_experiential_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, resource_constrained_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer co-production frameworks, set standards for what counts as valid integration of methods and experience, and compete for dedicated funding streams. They benefit from the institutionalization of co-production as a requirement.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_research_centers, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_research_centers, beneficiary).

% Require co-production plans in grant applications, shaping research agendas through funding criteria. They gain legitimacy and policy relevance by demonstrating inclusive knowledge practices.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_agencies_mandating_coproduction, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_agencies_mandating_coproduction, beneficiary).

% Gain formal recognition and funding access for methodologies they have long practiced. Their expertise becomes a gatekeeping credential in the new epistemic regime, but they must now meet methodological documentation standards.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_based_participatory_research_practitioners, beneficiary,
    organized, biographical, constrained, regional).

% Face new barriers: their work is deemed incomplete without experiential validation, requiring partnership building, ethics approvals, and translational effort they are not trained for. Funding success drops for non-co-produced proposals.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, purely_theoretical_researchers, payer,
    organized, biographical, constrained, global).

% Indigenous elders, patient advocates, and community historians find their knowledge now 'valid' only when framed through academic co-production protocols. They must learn methodological documentation or depend on academic partners who control the process.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, purely_experiential_knowledge_holders, payer,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, purely_experiential_knowledge_holders, excluded).

% Cannot meet the infrastructure demands of co-production (IRB compliance, data management plans, partnership administration) but are told their problems require co-produced solutions. They bear the cost of exclusion from both traditional and new epistemic channels.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, resource_constrained_communities, payer,
    powerless, immediate, trapped, local).

% Analyze the normative structure of the hybrid standard, track whether it resolves or reproduces epistemic injustice, and evaluate claims about integration versus assimilation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemology_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation between credentialed expertise and lived experience by requiring structured collaboration that validates both methodological transparency and experiential authenticity, producing knowledge that is both rigorous and relevant.
% TRANSFER_FUNCTION: Moves epistemic authority and research resources from mono-methodological silos (pure theory, pure experience) toward integrated co-production infrastructures — funding, training, review panels, publication venues — that require dual validation.
% ABSENT_VOICES: Communities that reject academic partnership entirely (sovereign Indigenous nations refusing research relations, underground medical communities avoiding institutional capture) are excluded by the very framework that claims to include them. They would object to the premise that legitimacy requires co-production at all.
% DISAPPEARANCE_RATIONALE: If the hybrid co-production requirement vanished, funding agencies would revert to discipline-specific review, CBPR practitioners would lose dedicated funding streams, purely theoretical work would regain parity, and communities would negotiate knowledge relationships on their own terms — the entire infrastructure of integration mandates would dissolve.
% FOUNDING_PROBLEM: The persistent failure of both credentialed expertise (blind to situated harm) and experiential pluralism (vulnerable to relativism and capture) to produce knowledge that is simultaneously rigorous, relevant, and just — especially in health, environment, and technology policy where both lives and liberties are at stake.
% FOUNDING_PROBLEM_CORROBORATION: CBPR networks and STS scholars attest the problem is live and worsening with AI-driven evidence synthesis. Traditional discipline leaders attest the problem is overstated — peer review self-corrects. Indigenous data sovereignty movements attest the problem is real but the co-production solution reproduces colonial extractivism. No consensus outside the benefiting institutional coalition.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).
:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the real compliance burden: researchers must build partnerships, navigate IRBs, learn translational skills; communities must document experience in academic terms. Suppression (0.38) is moderate: alternatives (pure theory, pure experience) are not banned but are structurally disadvantaged in funding and publication. Theater ratio (0.28) is rising as performative 'community engagement' boxes replace substantive power-sharing. Accessibility collapse (0.45) is partial: mono-methodological work persists but is increasingly marginal. Resistance (0.52) is significant from both excluded traditionalists and critical communities.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a rope: it solves the coordination problem of fragmented epistemic authority. From the purely theoretical researcher seat, it is a snare: a new gatekeeping layer that extracts labor without improving their work. From the experiential holder seat, it is a tangled rope: their knowledge gains recognition but only through assimilation. The engine computes this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda setters (centers, funders) sit near beneficiary end (d ~ 0.15): they shape the standard and capture its resources. CBPR practitioners are beneficiaries with constrained exit (d ~ 0.3): they gain recognition but must conform to new bureaucratic forms. Purely theoretical researchers are payers with constrained exit (d ~ 0.7): they lose parity and face new barriers. Purely experiential holders are payers/excluded with trapped exit (d ~ 0.85): their knowledge is 'validated' only on others' terms. Resource-constrained communities are victims with trapped exit (d ~ 0.9): they bear infrastructure costs without capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigor-relevance-justice gap) remains contested. If it is dead (peer review self-corrects, communities self-organize), the co-production mandate becomes a piton: performative infrastructure maintained by centers and funders who benefit from it. If live, it remains a tangled rope with genuine coordination function. The corroboration split (CBPR/STS vs disciplines vs Indigenous sovereignty) maps exactly to the seat divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_assimilation,
    'Does the hybrid standard genuinely integrate experiential knowledge on its own terms, or does it assimilate experience into methodological frameworks that erase its distinct epistemic character?',
    'Longitudinal study of co-produced knowledge outputs: track whether experiential claims survive peer review unchanged, or are translated into methodological language that strips their contextual meaning. Compare citation patterns and policy uptake.',
    'If assimilation dominates, the constraint is a snare for experiential holders (extraction disguised as inclusion). If integration holds, it is a genuine tangled rope with real coordination value. The classification shifts on this boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_vs_assimilation, conceptual, 'Whether co-production integrates or assimilates experiential knowledge').

omega_variable(
    infrastructure_cost_as_barrier,
    'Is the co-production infrastructure (IRBs, data agreements, partnership administration) a necessary coordination cost or an exclusionary barrier that concentrates epistemic authority in resource-rich institutions?',
    'Compare funding success rates and publication outcomes for co-produced proposals from well-resourced vs resource-constrained communities, controlling for topic relevance. Track administrative burden hours per stakeholder type.',
    'If infrastructure costs are exclusionary, the constraint extracts from the least powerful while claiming to empower them — snare dynamics. If costs are proportionate and supported, it is a rope with genuine coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_cost_as_barrier, empirical, 'Whether co-production infrastructure enables or excludes').

omega_variable(
    kernel_reading_relations,
    'How does this hybrid co-production reading structurally relate to the credentialed_expertise_reading and experiential_pluralism_reading of the legitimate_knowledge_boundary kernel?',
    'Map institutional adoption: when funders mandate co-production, do they foreclose single-track review (forecloses), run parallel tracks (coexists_with), or create downstream pressure on single-track legitimacy (influences)? Track policy documents and review criteria evolution.',
    'If this reading forecloses credentialed_expertise_reading in institutional frameworks, the kernel is resolving toward hybridity. If they coexist, the kernel remains contested. If this reading influences the others by raising the bar for legitimacy, the drift is toward integration without foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations among the three readings of the legitimate_knowledge_boundary kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding rules, publication gatekeeping) or internalized (researchers/communities pre-emptively conforming to co-production norms)?',
    'Compare suppression in mandated vs voluntary co-production contexts. If suppression persists in voluntary contexts, internalization is significant. Interview researchers who left academia citing ''co-production fatigue''.',
    'If internalized, effective suppression is higher than structural measures suggest — agents carry the constraint with them. This would increase the constraint''s extractiveness for identity-locked seats (CBPR practitioners, experiential holders).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in co-production mandates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t2000, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(legi_tr_t2005, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(legi_tr_t2015, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(legi_tr_t2020, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(legi_tr_t2025, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement(legi_tr_t2030, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2030, 0.31).

% Extraction over time
narrative_ontology:measurement(legi_be_t2000, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(legi_be_t2005, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(legi_be_t2015, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(legi_be_t2020, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2020, 0.39).
narrative_ontology:measurement(legi_be_t2025, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement(legi_be_t2030, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2030, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t2000, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(legi_su_t2005, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(legi_su_t2015, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2015, 0.31).
narrative_ontology:measurement(legi_su_t2020, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(legi_su_t2025, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(legi_su_t2030, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2030, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, research_funding_allocation_mechanisms).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_publication_review_standards).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_research_ethics_governance).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, evidence_based_policy_making_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legitimate_knowledge_boundary kernel. The credentialed_expertise_reading treats methodological peer review as the legitimacy gate (lower extraction, higher suppression for non-credentialed). The experiential_pluralism_reading treats community validation as primary (lower suppression, higher accessibility_collapse for experiential knowledge). This hybrid reading requires both, creating dual validation infrastructure. The three readings form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized, 0.3).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, moderate, 0.85).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
